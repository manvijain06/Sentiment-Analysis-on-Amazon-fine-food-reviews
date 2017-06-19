# MANVI JAIN
# FINAL TERM PROJECT
# OR 568 : APPLIED PREDICTIVE ANALYTICS

rm(list = ls())

install.packages('tm')
install.packages('caret')
install.packages('wordcloud')


library(tm)
library(wordcloud)
library(caret)
library(corrplot)
library(MASS)
library(AppliedPredictiveModeling)
library(pROC)

# -------------Set directory - Read File ------------------------------
#Loading the data

setwd("/Users/manvijain/Desktop/amazon-fine-foods")
originalData <- read.csv("reviews.csv", stringsAsFactors = FALSE)
reviews <- originalData[1:50000,] #taking sample data
hist(reviews$Score, main = "Distribution of Ratings ", xlab = "Ratings" , ylab="Percent of reviews")
hist(reviews$HelpfulnessNumerator,main = "Distribution of Helpfulness", xlab = "Helpfulness")

#-----PREPROCESSING FOR SENTIMENT ANALYSIS AND CONDUCTING ANALYSIS------------------------------#

sentimentanalysis <- function(file_name_vec){
  #Read the csv file as a data frame 
  
  fil <- file_name_vec
  #Get the text from the txt column. Store in txt
  
  txt <- fil$Text 
  summ <- fil$Summary 
  #Variable containing words to exclude 
  
  exwords <- c("<br />","\n")
  #If any word from exwords is found in txt replace with ''.
  
  txt <- gsub(x=txt,pattern = paste(exwords, collapse = "|"), replacement = "") 
  summ <- gsub(x=summ,pattern = paste(exwords, collapse = "|"), replacement = "") 
  #Convert all word in txt to lower case.
  
  txt <- tolower(txt) 
  summ <- tolower(summ) 
  
  sentence <- strsplit(txt, split=".", fixed=TRUE) 
  sentence1 <- strsplit(summ, split=".", fixed=TRUE) 
  #Split the text body by '.' 
  
  word <- strsplit(txt,split=" ", fixed=TRUE) 
  word1 <- strsplit(summ,split=" ", fixed=TRUE)
  #Spit the text body by ' ' 
  
  #   # Define function for stemming
  #   fcn_stem = function(word) wordStem(txt,language = "porter")
  #   word <- fcn_stem(word)
  
  cname <- c("words","value") 
  #Column names to be used in next line
  
  afinn <- read.delim2("AFINN-111.txt", header=FALSE, col.names = cname) 
  # Read AFINN as a data frame use cname for column names
  # Contains a list of words and values associated with their sentiment.
  
  for(i in 1:length(txt)) 
  {
    word[[i]]  <- gsub("[[:punct:]]", "", word[[i]])
    #Remove punctuations from all words 
    
    sentence[[i]] <- gsub("[[:punct:]]", " ", sentence[[i]])
    #Remove punctuations from sentences 
    
    inds <- which(afinn$words %in% word[[i]]) 
    #Get a list of indexes from the AFINN data frame for all the words in the word list 'i'.
    
    ifelse(length(inds) == 0, txt_score <- 0, txt_score<- mean(afinn$value[inds])) 
    #If no words found set 'txt_score' to 0 else get mean of all values from the index positions.
    
    fil$Text_Score[i] <- txt_score 
    #Save text score in the original dataframe 'fil' with new column 'Text_Score'
  }
  
  for(i in 1:length(summ)) 
  {
    word1[[i]]  <- gsub("[[:punct:]]", "", word1[[i]])
    sentence1[[i]] <- gsub("[[:punct:]]", " ", sentence1[[i]])
    inds1 <- which(afinn$words %in% word1[[i]])
    ifelse(length(inds1) == 0, summary_score <- 0, summary_score<- mean(afinn$value[inds1])) 
    fil$summary_Score[i] <- summary_score 
    
  }
  ret <- fil 
  return(ret) 
}


input <- reviews
review_result<- sentimentanalysis(input)

##--------------------------Plotting data based on Sentiment Analysis-----------------------------

layout(matrix(1:2,nrow =1))
hist(review_result$Text_Score, main = "Sentiment Analysis of Reviews", xlab = "Sentiment Analysis score")
hist(review_result$Text_Score, main = "Sentiment Analysis of Summary review", xlab = "Sentiment Analysis score")
boxplot(review_result$Text_Score, main ="Sentiment Analysis of Reviews")
boxplot(review_result$summary_Score, main ="Sentiment Analysis of Summary review")

layout(matrix(1:1, nrow=1))
plot(review_result$Score,review_result$Text_Score, main = "predicted ratings vs given ratings", xlab = "Given Ratings", ylab =" Ratings evaluated with sentiment analysis")
plot(review_result$Text_Score,review_result$HelpfulnessNumerator)

## ----------------creating WordCloud -----------------------------##
library(readr)
getWordFreq <- function(textVector) {
  
  stopWords <- stopwords("en")
  stopWords <- stopWords[stopWords != "not"]
  
  corpus = Corpus(VectorSource(textVector))
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, c("amazon", stopWords))  
  
  dtm = DocumentTermMatrix(corpus)
  removeSparse <- removeSparseTerms(dtm, 0.997)
  return(as.data.frame(as.matrix(removeSparse)))
}

### define a function that shows the top 100 frequent words in cloud
displayWordcloud <- function(wordFrequency) {
  wordcloud(colnames(wordFrequency), colSums(wordFrequency),
            max.words = 100, random.order = FALSE, scale = c(4, 1),
            rot.per=0.35, colors=brewer.pal(8, "Dark2"))
}

### word frequencies of the column "summary" for score = 1, 2, 3, 4, 5
cat("Calculate word frequency of reviews with score = 1 ...\n")
wordFreq1 <- getWordFreq(reviews$Text[reviews$Score ==1])

cat("Calculate word frequency of reviews with score = 2 ...\n")
wordFreq2 <- getWordFreq(reviews$Text[reviews$Score == 2])

cat("Calculate word frequency of reviews with score = 3 ...\n")
wordFreq3 <- getWordFreq(reviews$Text[reviews$Score == 3])

cat("Calculate word frequency of reviews with score = 4 ...\n")
wordFreq4 <- getWordFreq(reviews$Text[reviews$Score == 4])

cat("Calculate word frequency of reviews with score = 5 ...\n")
wordFreq5 <- getWordFreq(reviews$Text[reviews$Score == 5])

layout(matrix(1:5, nrow=1))

displayWordcloud(wordFreq1)
text(x = 0.5, y = 1.1, "score = 1", cex = 2)
displayWordcloud(wordFreq2)
text(x = 0.5, y = 1.1, "score = 2", cex = 2)
displayWordcloud(wordFreq3)
text(x = 0.5, y = 1.1, "score = 3", cex = 2)
displayWordcloud(wordFreq4)
text(x = 0.5, y = 1.1, "score = 4", cex = 2)
displayWordcloud(wordFreq5)
text(x = 0.5, y = 1.1, "score = 5", cex = 2)

#-------------Categorical Variable -> Numerical Variable---------------#

noOfWords <- function(keyword){
  return(length(unlist(strsplit(keyword,"\\S+"))))
}

for (i in 1:nrow(review_result))
  review_result$rev_length[i] <- noOfWords(review_result$Text[i])

for (i in 1:nrow(review_result))
  if (review_result$Score[i] > 3 ) review_result$Score[i] = "POSITIVE" else review_result$Score[i] = "NEGATIVE"

review_result <- review_result[,-c(2,3,4,8,9,10)]
hist(review_result$rev_length)

#----------------------cleaning the data :REMOVING NA VALUES-----------------------------#

apply(review_result, 2, function(x){ sum(is.na(x)) } )
row.has.na <- apply(review_result, 1, function(x){any(is.na(x))})
sum(row.has.na)
review_result <- review_result[!row.has.na,]

##------------------ Dividing the dataset in a 70% Training and 30% Test dataset--------------------------
set.seed(100)
partition <- createDataPartition(review_result$Id,p = 0.7,list =TRUE)[[1]]

reviewTrain <- review_result[partition,]
reviewTest  <- review_result[-partition, ]

TrainY <- reviewTrain[,c(4)]
TrainX <- reviewTrain[,-c(1,4)]

TestY <- reviewTest[,c(4)]
TestX <- reviewTest[,-c(1,4)]


## --------------------------To create the output file------------------------------------
## write.csv(output1,file ="predictedscore.csv",sep = ",",col.names = NA,qmethod = "double")


#-----------------------KNN-----------------------------------------
set.seed(100)
# Without specifying train control, the default is bootstrap 
knnModel = train(x=TrainX, y=TrainY, method="knn",
                 preProc=c("center","scale"),
                 tuneLength=10)

knnModel
plot(knnModel$results$k, knnModel$results$Accuracy, type="o",xlab="# neighbors",ylab="Accuracy", main="KNNs for Friedman Benchmark")

knnPred = predict(knnModel, TestX)
knnrocCurve = roc(response = TestY, predictor = as.numeric(knnPred))


#-----------------------------------Naive Bayes Classifier---------------------

nbaiyesFit <- train(x = TrainX, y = TrainY, method ='nb') 

nbaiyesFit$pred <- predict(nbaiyesFit,TestX)
nbaiyesCM <- confusionMatrix(data = nbaiyesFit$pred , reference = TestY)
nbaiyesCM
nbrocCurve = roc(response = TestY, predictor = as.numeric(nbaiyesFit$pred))
               

#---------------------------------------------SVM-------------------------
library(kernlab)
set.seed(202)
sigmaRangeReduced <- sigest(as.matrix(TrainX))
svmRGridReduced <- expand.grid(.sigma = sigmaRangeReduced[1], .C = 2^(seq(-4, 4)))

set.seed(476)
svmRModel <- train(TrainX,TrainY,method = "svmRadial",preProc = c("center", "scale"),tuneGrid = svmRGridReduced,fit = FALSE)
svmRModel

svmRModel$pred <- predict(svmRModel,TestX)
svmCM <- confusionMatrix(data = svmRModel$pred, reference = TestY)
svmCM

svmrocCurve = roc(response = TestY, predictor = as.numeric(svmRModel$pred))

#-------------------------------LINEAR MODELS------------------------------------------------

#----------------------------LOGISTIC REGRESSION--------------
set.seed(1056)
lRegFit <- train(x = TrainX, y = TrainY, method = "glm", trControl = trainControl(method = "repeatedcv", repeats = 5))
lRegFit
lRegFit$pred <- predict(lRegFit, TestX)
lRegCM <- confusionMatrix(lRegFit, norm = "none")
lRegCM
lrRoc <- roc(response = TestY,predictor = as.numeric(lRegFit$pred))


##--------------------------------LDA-------------------------
ldaFit <- train(x = TrainX,y = TrainY,method = "lda",preProc = c("center","scale"),metric = "ROC",trControl = trainControl(method = "repeatedcv", repeats = 5,classProbs = TRUE))
ldaFit

ldaFit$pred <- predict(ldaFit, TestX)
ldaCM <- confusionMatrix(ldaFit, norm = "none")
ldaCM
ldaRoc <- roc(response = TestY,predictor = as.numeric(ldaFit$pred))


plot(ldaRoc, legacy.axes = TRUE, main ="LDA")
plot.new()
plot(lrRoc, type = "s", col = 'red',legacy.axes = TRUE, main ="Logistic Regression")

plot(nbrocCurve,  add =FALSE, col = 'blue',legacy.axes = TRUE, main ="Naives Bayes")
plot(svmrocCurve, add = FALSE, col = 'yellow',legacy.axes = TRUE,main ="SVM")
plot(knnrocCurve, add = FALSE, col = 'green',legacy.axes = TRUE, main ="KNN")

auc(ldaRoc)
auc(lrRoc)
auc(nbrocCurve) 
auc(svmrocCurve) 
auc(knnrocCurve)   