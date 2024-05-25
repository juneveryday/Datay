# reset
rm(list = ls())


library(slam)
library(tm)
library(SnowballC)
library(proxy)
library(igraph)

# text file path checking
data_cname = file.path(".","Data_text")

# file checking
# print(dir(food_cname))


# Question 2
# make corpus
data_docs = Corpus(DirSource(data_cname))

data_docs

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
data_docs <- tm_map(data_docs, juneremoveChars, "n't")
# data_docs <- tm_map(data_docs, juneremoveChars, "didn’t")
# data_docs <- tm_map(data_docs, juneremoveChars, "isn’t")
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
# Remove columns with 60% empty (0) cells
data_dtm <- removeSparseTerms(data_dtm, sparse = 0.5)

#write.csv(data_dtm, "data_dtm.csv")

dim(data_dtm)

# Question 4

# Cosine distance between each document for clustering.
dtms = as.matrix(data_dtm)
distmatrix = proxy::dist(dtms, method = "cosine")
fit = hclust(distmatrix, method = "ward.D")
plot(fit, hang = -1, main = "Question 4, Clustering Dendrogram")

#inspect(data_dtm)

fit <- hclust(distmatrix, method = "ward.D")

fit

# using cluster object "fit" create required number of clusters.
cutfit <- cutree(fit, k = 3)


# Calculate the accuracy with which the clustering groups documents by topic.

# Create vector of topic labels in same order as corpus
topics = c("food", "food", "food","food","food",
           "tech", "tech", "tech", "tech", "tech",
           "world", "world", "world", "world", "world")


groups = cutree(fit, k = 3)

cluster_table <- table(GroupNames = topics, Clusters = groups)

TA =  as.data.frame.matrix(table(GroupNames = topics, Clusters = groups))

TA = TA[,c(2,1,3)]

TA

TA_matrix <- as.matrix(TA)

diag_TA <- diag(TA_matrix)

accuracy <- sum(diag_TA) / sum(TA_matrix)

accuracy

# Question 5
 
# convert to binary matrix
dtmsx = as.matrix((dtms > 0) + 0)

# multiply binary matrix by its transpose
ByAbsMatrix = dtmsx %*% t(dtmsx)

# make leading diagonal zero
diag(ByAbsMatrix) = 0

#ByAbsMatrix

# Create graph object
Q5_SM_network = graph_from_adjacency_matrix(ByAbsMatrix, mode = "undirected", weighted = TRUE)

# Plot the Basic model
plot(Q5_SM_network,
     main = "Q5 Single Basic Mode Network")

# Get the weights
Q5_SM_network_weight = E(Q5_SM_network)$weight

# Create color palette function
Q5_color_picker = colorRampPalette(c("lightpink","skyblue","grey"))

# Generate edge colors based on weights
Q5_edge_colors <- Q5_color_picker(length(Q5_SM_network_weight))[as.numeric(cut(Q5_SM_network_weight, breaks = length(Q5_SM_network_weight)))]

# Plot the graph
plot(Q5_SM_network,
     edge.label = Q5_SM_network_weight,
     edge.color = Q5_edge_colors,
     edge.width = 1,
     main = "Q5 Single Final Mode Network")









