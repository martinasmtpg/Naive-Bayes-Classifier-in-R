##=====LOAD PACKAGES=====##
library(tm)           #package for text mining such as tm_map function
library(NLP)          #required package for text mining
library(textclean)    #package for add_comma_space, replace_non_ascii, and replace_word_elongation
library(stringr)      #package for tokenizing
library(caret)        #package for splitting data
library(dplyr)        #package for manipulation data (data frame)
library(katadasaR)    #package for stemming
library(tau)          #package for tokenizing
library(parallel)     #package for parallel processing

##=====IMPORT DATA=====##
bodyshame <- read.csv("D:/MARTINA/Documents/skripsi martina/dataraw.csv" , stringsAsFactors = TRUE)
View(bodyshame)       #view dataset in new tab
glimpse(bodyshame)    #do observation of dataset

##=====MEMBUAT CORPUS=====##
bodyshameCorpus <- Corpus(VectorSource(bodyshame$commentText)) #convert data into corpus form
inspect(bodyshameCorpus[1:10])    #show 1 up to 10 teratas record of dataset

##=====PREPROCESSING=====##

#CLEANING DATASET#
#hapus url
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
hapus_URL <- tm_map(bodyshameCorpus, content_transformer(removeURL))
inspect(hapus_URL[1:10])

#hapus mention/username
remove.mention <- function(x) gsub("@\\S+", "", x)
hapus_mention <- tm_map(hapus_URL, remove.mention)
inspect(hapus_mention[1:10])

#hapus hashtag
remove.hashtag <- function(x) gsub("#\\S+", "", x)
hapus_hashtag <- tm_map(hapus_mention, remove.hashtag)
inspect(hapus_hashtag[1:10])

#menambah spasi setelah tanda koma
tambah.spasi <- tm_map(hapus_hashtag, content_transformer(add_comma_space))
inspect(tambah.spasi[1:10])

#replace non ascii
replace.nonascii <- tm_map(tambah.spasi, content_transformer(replace_non_ascii))

#hapus tanda baca
hapus_punctuation <- tm_map(replace.nonascii, content_transformer(removePunctuation))

#hapus angka
hapus_angka <- tm_map(hapus_punctuation, content_transformer(removeNumbers))

##CASE FOLDING##
casefolding <- tm_map(hapus_angka, content_transformer(tolower))

##TOKENIZING##
tokenizing <- function(x) strsplit(as.character(x), ";")
token <- tm_map(casefolding, tokenizing)
inspect(token)
token[[1]]

##NORMALIZATION##
normalisasi <- read.csv("D:/MARTINA/Documents/skripsi martina/normalisasi.csv", header=T)
old_normalisasi <- as.character(normalisasi$old)
new_normalisasi <- as.character(normalisasi$new)
normalization<-function(x) Reduce(function(x,r) gsub(normalisasi$old[r],normalisasi$new[r],x,fixed=T),seq_len(nrow(normalisasi)),x)
normalisasi_kata <- tm_map(token, normalization)

#hapus duplikat huruf
remove.duplikat <- tm_map(normalisasi_kata, content_transformer(replace_word_elongation))

##STOPWORD##
cStopwordID <- readLines("D:/MARTINA/Documents/skripsi martina/stopwords.csv")
stopword <- tm_map(remove.duplikat, removeWords, cStopwordID)

##STEMMING##
stem_text <- function(text,mc.cores=1)
{
  stem_string <- function(str)
  {
    str <- tokenize(x=str)
    str <- sapply(str,katadasaR)
    str <- paste(str,collapse = "")
    return(str)
  }
  x <- mclapply(X=text,FUN=stem_string,mc.cores=mc.cores)
  return(unlist(x))
}
stemming <- tm_map(stopword, stem_text)

#hapus whitespace
data_clean <- tm_map(stemming, stripWhitespace)

#hapus whitespace tanpa stemming
data_clean <- tm_map(stopword, stripWhitespace)

#save data clean
databersih <- data.frame(bodyshame$label,text=unlist(sapply(data_clean,'[')), stringsAsFactors=F)
write.csv(databersih,file="D:/MARTINA/Documents/skripsi martina/databersih.csv", row.names = FALSE)

#remove NAs
hapusna = read.csv("D:/MARTINA/Documents/skripsi martina/databersih.csv", na.strings = c(" ", "NA"))
sum(is.na(hapusna))
hapusna = hapusna %>% na.omit()
hapusna %>% head(5)
write.csv(hapusna,file="D:/MARTINA/Documents/skripsi martina/hapusna.csv", row.names = FALSE)

##===== CONSTRUCT DATA =====##
df <- read.csv("D:/MARTINA/Documents/skripsi martina/hapusna.csv", stringsAsFactors = FALSE)
glimpse(df)
summary(df)

#check table
table(df$bodyshame.label)

#check classes distribution
prop.table(table(df$bodyshame.label))

set.seed(1)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
glimpse(df)
df$bodyshame.label <- as.factor(df$bodyshame.label)
corpus <- Corpus(VectorSource(df$text))
corpus
inspect(corpus[1:3])

## TRANSFORM DATA INTO DOCUMENT TERM MATRIX
dtm <- DocumentTermMatrix(corpus)
inspect(dtm)

findFreqTerms(dtm, 5) #mencari kata yang mempunyai frekuensi muncul lebih dari 5 kali
tdm <- TermDocumentMatrix(corpus)
inspect(tdm)

#remove the infrequently used words
#start by removing sparse terms:
dtms <- removeSparseTerms(dtm, 0.99) #this makes a matrix that is 99% empty space, maximum
inspect(dtms)

#Explore your data
freq <- colSums(as.matrix(dtms))
length(freq)
ord <- order(freq)
m <- as.matrix(dtms) #if you prefer to export the matrix to Excel
dim(m)
write.csv(m, file = "D:/MARTINA/Documents/skripsi martina/DTM.csv")

#check out the frequency of frequencies
head(table(freq), 20) #the 20 indicates that we only want the first 20 frequencies. feel free to change that number
tail(table(freq), 20)
freq <- colSums(as.matrix(dtms))
freq

#an alternate view of term frequency:
findFreqTerms(dtm, lowfreq = 50)

#another way to do this
wf <- data.frame(word=names(freq), freq = freq)
head(wf)

#plot word frequencies
library(ggplot2)
p <- ggplot(subset(wf, freq>10), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

#wordcloud
library(wordcloud)
library(RColorBrewer)

#plot words that occur at least 20 times
#set.seed(123)
wordcloud(names(freq), freq, min.freq = 20)

#plot the 10 most frequently used words
#set.seed(142)
wordcloud(names(freq), freq, max.words = 50)

#add some color and plot words occuring at least 20 times
#set.seed(142)   
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

## SPLITTING DATA
library(e1071)  #package for classification

n <- nrow(df)
df.train <- df[1:round(.9 * n),]  #percentage split for 90:10, do to another composition 80:20, 70:30, 60:40, and 50:50
df.test  <- df[(round(.9 * n)+1):n,]

nn <- length(corpus)
corpus.train <- corpus[1:round(.9 * nn)]
corpus.test  <- corpus[(round(.9 * nn)+1):nn]

nnn <- nrow(dtm)
dtm.train <- dtm[1:round(.9 * nnn),]
dtm.test  <- dtm[(round(.9 * nnn)+1):nnn,]
dim(dtm.train)

fivefreq <- findFreqTerms(dtm.train, 10)
length((fivefreq))
fivefreq

# Use only 5 most frequent words (fivefreq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)

dtm.test.nb <- DocumentTermMatrix(corpus.test, control=list(dictionary = fivefreq))

dim(dtm.test.nb)

# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("non_shaming", "shaming"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

# Train the classifier
system.time( classifier <- naiveBayes(trainNB, df.train$bodyshame.label, laplace = 1) )

# Use the NB classifier we built to make predictions on the test set.
system.time( pred <- predict(classifier, newdata=testNB) )

# Create a truth table by tabulating the predicted class labels with the actual class labels 
library(gmodels)
table("Predictions"= pred,  "Actual" = df.test$bodyshame.label )

CrossTable(pred, df.test$bodyshame.label,
           prop.chisq = FALSE, 
           prop.t = FALSE,
           dnn = c('predicted', 'actual'))

# Prepare the confusion matrix
conf.mat <- confusionMatrix(pred, df.test$bodyshame.label)

conf.mat

conf.mat$byClass

conf.mat$overall
conf.mat$overall['Accuracy']

#see model probabilities
print(classifier)

#print data test
class.df <- as.data.frame(pred)
colnames(class.df) <- c("Predicted")
datatest_class.df <- cbind(df.test, class.df)
View(datatest_class.df)
write.csv(datatest_class.df,file="D:/MARTINA/Documents/skripsi martina/Data Testing beserta Kelas prediksi.csv", row.names = FALSE)

#cara lain print data test
lihatdata <- print(data.frame(df.test, bodyshame.label=pred))
lihatdata
View(lihatdata) #hasilnya sama kok beda sintaks aja lebih sederhana

#input sebuah uji data baru
data_new1 <- data.frame(text = "jelek")
data_new2 <- data.frame(text = "semangat vloggingnya cantik jangan menyerah")
model = naiveBayes(bodyshame.label~., data = df)
predict(model, data_new1, type = "class")
predict(model, data_new1, type = "raw")
predict(model, data_new2, type = "class")
predict(model, data_new2, type = "raw")

#load the pROC package
library(pROC)

# Create a ROC curve
ROC <- roc(df.test$bodyshame.label, predictor = factor(pred, ordered = TRUE), plot=TRUE)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)
