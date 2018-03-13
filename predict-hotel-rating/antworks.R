library(data.table)
library(ggplot2)
library(ROSE)
#library(caret)
library(dplyr)
library(stringr)
library(gridExtra)
library(quanteda)
library(Matrix)
library(xgboost)
#library(caretEnsemble)

## Steps involved
# 1. Data Loading
# 2. EDA
# 3. Data Cleaning
# 4. Modeling
# 5. Prediction
# 6. Resultset creation

# variables used
path = "C:/wip/antworks/data/"

# Data Loading
train <- fread(paste0(path, "train.csv"), sep = "~", data.table = FALSE, 
               colClasses = c("integer", "character", "factor", "factor", "factor") )

test <- fread(paste0(path, "test.csv"), sep = "~", data.table = FALSE, 
              colClasses = c("integer", "character", "factor", "factor") )

print(object.size(train), units = 'Mb') # 27.3 Mb sized object
print(object.size(test), units = 'Mb') # 7.9 Mb sized object


# Quick Descriptive statistics of training data 
dim(train) # 30172 x 5
names(train) # "User_ID"      "Description"  "Browser_Used" "Device_Used"  "Is_Response"
str(train)
sum(is.na(train)) #checking for NA 

# A quick summary shows that 
# 1. 'Browser Used' needs data cleaning
# 2. More reviews are from Desktop & Mobile
# 3. There seems to be a class imbalance. Need more analysis
summary(train[, -c(1, 2)])

# Analysing 'Browser Used'
table(train$Browser_Used) # Need data cleaning
prop.table(table(train$Browser_Used)) * 100


# Analysing 'Device Used'
table(train$Device_Used)
prop.table(table(train$Device_Used)) * 100


# Analysing respose variable (Is_Response)
table(train$Is_Response)
prop.table(table(train$Is_Response)) * 100 #Clear indication of class imbalance (Good = 68.17%, Bad = 31.83%)

# Graphically checking class imbalance
ggplot(data=train, aes(x=Is_Response)) +
  geom_bar() +
  labs(x = 'Response', y = 'Count', title = 'Responses')



# Over Sampling to overcome class imbalance
train.new <- ovun.sample(Is_Response~., data=train, 
                         p=0.5, seed=1, 
                         method="over")$data
table(train.new$Is_Response)


# Combining datasets for data cleaning
test$Is_Response <- 'TEST'
combine <- rbind(train, test)



# Data Cleaning
combine <-  combine %>% mutate(Browser_Used = ifelse(as.character(Browser_Used) %in% c('Firefox', 'Mozilla'),
                                                     'Mozilla Firefox',
                                                     as.character(Browser_Used)))

combine <-  combine %>% mutate(Browser_Used = ifelse(as.character(Browser_Used) == 'Chrome',
                                                     'Google Chrome',
                                                     Browser_Used)) 

combine <-  combine %>% mutate(Browser_Used = ifelse(as.character(Browser_Used) %in% c('InternetExplorer', 'IE'),
                                                     'Internet Explorer',
                                                     Browser_Used)) 
# Combining Opera/Safari to a new category (Others) 
combine <-  combine %>% mutate(Browser_Used = ifelse(as.character(Browser_Used) %in% c('Opera', 'Safari'),
                                                     'Others',
                                                     Browser_Used)) 

combine$Browser_Used <- as.factor(combine$Browser_Used)


# Exploratory Data Analysis (EDA)

# Browser Used
# 1. Firefox is mostly widely
# 2. Interestingly IE is used more than chrome
# 3. Microsoft outperforms others combining IE + Edge

table(combine$Browser_Used)
prop.table(table(combine$Browser_Used)) * 100

combine %>% group_by(Browser_Used) %>% summarise(count = n()) %>%
  ggplot(aes (x = reorder(Browser_Used, count), y = count)) +
  geom_bar(fill = 'cyan2', position = 'dodge', stat = 'identity') +
  theme (axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = rel (1.2))) +
  labs(x = 'Browser Used', y = 'Count', title = 'Reviews per Browser')



# Device Used
# 1. Desktop and Mobile are used above 38% 
# 2. Tablet is used 22.87%
prop.table(table(combine$Device_Used)) * 100
combine %>% group_by(Device_Used) %>% summarise(count = n()) %>%
  ggplot(aes (x = reorder (Device_Used, count), y = count)) +
  geom_bar(fill = 'cyan2', position = 'dodge', stat = 'identity') +
  theme (axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = rel (1.2))) +
  labs(x = 'Device Used', y = 'Count', title = 'Reviews per Device')



# Multi variate Analysis

# Response based on the browser used
prop.table(table(combine$Is_Response, combine$Browser_Used), 1) * 100
ggplot (combine) +
  geom_bar (aes (x = Browser_Used, fill = Is_Response), position = 'stack') +
  theme (axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = rel (1.2))) +
  labs(x = 'Responses based on Browsers', y = 'Review Count', 
       title = 'Browser based Responses')

# Response based on the browser used
prop.table(table(combine$Is_Response, combine$Device_Used), 1) * 100
ggplot (combine) +
  geom_bar (aes (x = Device_Used, fill = Is_Response), position = 'stack') +
  theme (axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = rel (1.2))) +
  labs(x = 'Responses based on Device', y = 'Review Count', 
       title = 'Device based Responses')

# Browsers used in different devices
# 1. Tablet users prefer Edge and Firefox
prop.table(table(combine$Device_Used, combine$Browser_Used), 1) * 100
ggplot (combine) +
  geom_bar (aes (x = Browser_Used, fill = Device_Used), position = 'dodge') +
  theme (axis.text.x=element_text(angle=45, vjust=1, hjust=1, size = rel (1.2))) +
  labs(x = 'Browsers used in Device', y = 'Count', title = 'Browser based Reviews from various devices')





# Description

# Creating a description length column
combine$Desc_Length = nchar(combine$Description)
# combine[combine$Desc_Length %in% head(sort(combine$Desc_Length, decreasing = F), 20), 
#         c("Description", "Desc_Length", "Is_Response")]

# Creating a new column for count of words in Description
combine$Word_Count <- str_count(combine$Description, '\\S+')
#combine[combine$Word_Count %in% head(sort(combine$Word_Count, decreasing = F), 20), c("Description", "Word_Count", "Is_Response")]


p1 = combine %>% 
  ggplot(aes(x=log(Desc_Length))) +
  geom_histogram(bins=50) +
  ggtitle('Distribution of Length of Descriptions') +
  xlab('Length of Review Description') +
  theme(plot.title = element_text(size=10))

p2 = combine %>% 
  ggplot(aes(x=log(Word_Count))) +
  geom_histogram(bins=50) +
  ggtitle('Distribution of # of Tokens of Descriptions') +
  xlab('Number of Tokens') +
  theme(plot.title = element_text(size=10))

grid.arrange(p1, p2, ncol=2)

##############################################################


# create corpus object from Description column
descriptions <- corpus(char_tolower(combine$Description))

description_tokens <- tokens(
  tokens_remove(tokens(descriptions,   
                       remove_numbers = FALSE, 
                       remove_punct = TRUE,
                       remove_symbols = TRUE, 
                       remove_separators = TRUE), 
                stopwords("english")), 
  ngrams = 1:2
)

description_dtm <- dfm(
  description_tokens
)


description_dtm_trimmed <- dfm_trim(description_dtm, min_count = 600)
description_tf_matrix <- tfidf(description_dtm_trimmed)
#description_tf_matrix <- dfm_tfidf(description_dtm_trimmed)


description_tf_matrix
topfeatures(description_tf_matrix)



# Preparing data for modelling
sparse_matrix <- sparse.model.matrix(
  ~Browser_Used + 
    Device_Used + 
    Desc_Length + 
    Word_Count,
  data = combine)

## Fix for cbind dfm and sparse matrix
class(description_tf_matrix) <- class(sparse_matrix)

aaa <- cbind(
  sparse_matrix, # basic features
  description_tf_matrix  # description
)

rownames(aaa) <- NULL

sparse_train <- aaa[seq_len(nrow(train)), ]
sparse_test  <- aaa[seq(from = (nrow(train) + 1), to = nrow(aaa)), ]

dtrain <- xgb.DMatrix(as.matrix(sparse_train),label= ifelse(train$Is_Response=='Good', 1, 0))
#dtrain <- lgb.Dataset(sparse_train, label=log_prices)

nrounds <- 8000

param <- list(
  objective = "binary:logistic",
  metric = "Accuracy"
)

model <- xgb.train(
  params = param,
  data = dtrain,
  nrounds = nrounds,
  learning_rate = 1,
  subsample = 0.7,
  max_depth = 4,
  eval_freq = 50,
  verbose = -1,
  nthread = 4
)

# Predicting results
predicted <- predict(model, sparse_test)
results <- data.frame(
  User_ID = test$User_ID, #as.integer(seq_len(nrow(test)) - 1),
  Is_Response = predicted
)


results$Is_Response <- ifelse(results$Is_Response>.5, 'Good', 'Bad')

head(results)
# head(test$User_ID)
table(results$Is_Response)
prop.table(table(results$Is_Response)) * 100

output <- paste0(results$User_ID,'~', results$Is_Response)
write.csv(output, paste0(path,"xgb_results.csv"))
