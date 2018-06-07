setwd("C:/Users/jakek/Documents/intrepidinsight/scrape_median")
library("kernlab") 
library("caret") 
library("tm") 
library("dplyr") 
library("splitstackshape")
library("e1071")
library('monkeylearn')

orig<-read.csv("toclassify.csv")

# create training data.
data <- orig$V1
corpus <- VCorpus(VectorSource(data))
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
train <- as.matrix(tdm)
train <- cbind(train, orig$class)
colnames(train)[ncol(train)] <- 'y'
train <- as.data.frame(train)
train$y <- as.factor(train$y)

# Train.
fit <- train(y ~ ., data = train, method = 'bayesglm')

# Check accuracy on training.
predict(fit, newdata = train)

# load all data
load("median_scraped.RData")
data2<-results$V1
corpus2<-VCorpus(VectorSource(data2))
tdm2<-DocumentTermMatrix(corpus2, control = list(dictionary = Terms(tdm), removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
full<-as.matrix(tdm2)
final<-data.frame(results$V1,results$median_comp, predict(fit, newdata = full))

# monkeylearn
text<-"Apple Corp. is the best company."
output <- monkeylearn_extract(text,
                              extractor_id = "ex_y7BPYzNG",
                              params = list(max_keywords = 3,
                                            use_company_names = 1), key="57c24e8a78ef7aadfc0004f8f09989911f87ed99")



