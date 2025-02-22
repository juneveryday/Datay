# reset
rm(list = ls())

# bringing library
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
set.seed("31994695")
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
data_docs <- tm_map(data_docs, juneremoveChars, "like")
data_docs <- tm_map(data_docs, juneremoveChars, "can")
data_docs <- tm_map(data_docs, juneremoveChars, "take")
data_docs <- tm_map(data_docs, juneremoveChars, "also")
data_docs <- tm_map(data_docs, juneremoveChars, "one")
data_docs <- tm_map(data_docs, juneremoveChars, "year")
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
set.seed("31994695")
data_dtm <- DocumentTermMatrix(data_docs) 

dim(data_dtm)

# Remove sparse terms
# Remove columns with 60% empty (0) cells
data_dtm <- removeSparseTerms(data_dtm, sparse = 0.5)

#write.csv(data_dtm, "data_dtm.csv")

dim(data_dtm)

# Question 4

# Cosine distance between each document for clustering.
set.seed("31994695")
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

# ByAbsMatrix

# Create graph object
set.seed("31994695")
Q5_SM_network = graph_from_adjacency_matrix(ByAbsMatrix, mode = "undirected", weighted = TRUE)

# Plot the Basic model
set.seed("31994695")
plot(Q5_SM_network,
     main = "Q5 Single Basic Mode Network")

# Get the weights
Q5_SM_network_weight = E(Q5_SM_network)$weight

# Create color palette function
color_picker = colorRampPalette(c("pink","lightblue","green"))

# Generate edge colors based on weights
Q5_edge_colors <- color_picker(length(Q5_SM_network_weight))[as.numeric(cut(Q5_SM_network_weight, breaks = length(Q5_SM_network_weight)))]


set.seed("31994695")
# Plot the graph
plot(Q5_SM_network,
     edge.label = Q5_SM_network_weight,
     edge.color = Q5_edge_colors,
     edge.width = 1,
     main = "Q5 Single Final Mode Network")


# Question 6
set.seed("31994695")
ByTokenMatrix = t(dtmsx) %*% dtmsx

# make leading diagonal zero
diag(ByTokenMatrix) = 0

# Create graph object
set.seed("31994695")
Q6_TK_network = graph_from_adjacency_matrix(ByTokenMatrix, mode = "undirected", weighted = TRUE)

# plot the basic model
set.seed("31994695")
plot(Q6_TK_network,
     main = "Q6 Single Basic Mode Network")

# Get the weights
Q6_TK_network_weight = E(Q6_TK_network)$weight

# Generate edge colors based on weights
Q6_edge_colors <- color_picker(length(Q6_TK_network_weight))[as.numeric(cut(Q6_TK_network_weight, breaks = length(Q6_TK_network_weight)))]

set.seed("31994695")

# Plot the graph
plot(Q6_TK_network,
     edge.label = Q6_TK_network_weight,
     edge.color = Q6_edge_colors,
     edge.width = 1,
     main = "Q6 Token Final Mode Network")

# Question 7
# start with documnet term matrix dtms
dtmsa = as.data.frame(dtms) # clone dtms

dtmsa$ABS = rownames(dtmsa) # add row names

dtmsb = data.frame()
for (i in 1:nrow(dtmsa)){
  for (j in 1:(ncol(dtmsa) - 1)){
    touse = cbind(dtmsa[i,j], dtmsa[i,ncol(dtmsa)],colnames(dtmsa[j]))
    dtmsb = rbind(dtmsb,touse)}} # close loops

colnames(dtmsb) = c("weight", "abs", "token")    

dtmsc = dtmsb[dtmsb$weight != 0,] #delete 0 weights

# put columns in order : abs, token, weight
dtmsc = dtmsc[,c(2,3,1)]

# create graph object and declare bipartite
g <- graph.data.frame(dtmsc, directed = FALSE)

bipartite.mapping(g)

g
set.seed("31994695")
V(g)$type <- bipartite_mapping(g)$type
V(g)$color <- ifelse(V(g)$type, "lightgreen","pink")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgrey"

# plot the basic Bipartite network plot
#plot(g)

# Get the weights
Q7_BP_network_weight = E(g)$weight

# change to numeric
Q7_BP_network_weight <- as.numeric(Q7_BP_network_weight)

set.seed("31994695")
# Generate edge colors based on weights
Q7_edge_colors <- color_picker(length(Q7_BP_network_weight))[as.numeric(cut(Q7_BP_network_weight, breaks = length(Q7_BP_network_weight)))]


# Plot the graph
set.seed("31994695")
plot(g,
     edge.label = Q7_BP_network_weight,
     edge.color = Q7_edge_colors,
     edge.width = 1,
     main = "Q7 Bipartite Final Mode Network")

