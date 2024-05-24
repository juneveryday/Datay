# reset
rm(list = ls())


library(slam)
library(tm)
library(SnowballC)
library(proxy)

# text file path checking
data_cname = file.path(".","Data_text")

# file checking
# print(dir(food_cname))


# Question 2
# make corpus
data_docs = Corpus(DirSource(data_cname))


# Question 3
# Start Making Token
# summary(food_docs)
juneremoveChars <- content_transformer(function(x, pattern) gsub(pattern, "", x))

## Tokenisation
data_docs <- tm_map(data_docs, removeNumbers)
data_docs <- tm_map(data_docs, removePunctuation)
data_docs <- tm_map(data_docs, content_transformer(tolower))

data_docs <- tm_map(data_docs, juneremoveChars, "’s")
data_docs <- tm_map(data_docs, juneremoveChars, "’ve")
data_docs <- tm_map(data_docs, juneremoveChars, "’d")
data_docs <- tm_map(data_docs, juneremoveChars, "’m")
data_docs <- tm_map(data_docs, juneremoveChars, "doesn’t")
data_docs <- tm_map(data_docs, juneremoveChars, "didn’t")
data_docs <- tm_map(data_docs, juneremoveChars, "isn’t")
data_docs <- tm_map(data_docs, juneremoveChars, "–")
data_docs <- tm_map(data_docs, juneremoveChars, "“")
data_docs <- tm_map(data_docs, juneremoveChars, "”")

# Filter words
# Remove stop words and white space
data_docs <- tm_map(data_docs, removeWords, stopwords("english"))
data_docs <- tm_map(data_docs, stripWhitespace)

# Stem
data_docs<- tm_map(data_docs, stemDocument, language = "english")


# Create document term matrix
data_dtm <- DocumentTermMatrix(data_docs) 

dim(data_dtm)

# Remove sparse terms
data_dtm <- removeSparseTerms(data_dtm, sparse = 0.45)

write.csv(data_dtm, "data_dtm.csv")

dim(data_dtm)

# Question 4

findFreqTerms(data_dtm,lowfreq = 10)
dtms = as.matrix(data_dtm)
distmatrix = proxy::dist(dtms, method = "cosine")
fit = hclust(distmatrix, method = "ward.D")
plot(fit, hang = -1, main = "Question 4, Clustering Dendrogram")

inspect(data_dtm)

# • Use the cosine distance between each document for clustering.
# • Identify which cluster each document belongs to.
# • Calculate the accuracy with which the clustering groups documents by topic.
# • Give a qualitative description of the quality of the clustering. (4 Marks)



# Question 5

