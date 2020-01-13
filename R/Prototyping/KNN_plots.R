library(dbscan)


data(iris)
iris <- as.matrix(iris[,1:4])

## Find the 4-NN distance for each observation (see ?kNN
## for different search strategies)
kNNdist(iris, k=4)

## Get a matrix with distances to the 1st, 2nd, ..., 4th NN.
kNNdist(iris, k=4, all = TRUE)

## Produce a k-NN distance plot to determine a suitable eps for
## DBSCAN (the knee is around a distance of .5)
kNNdistplot(iris, k=4)


# Correct way to do spit 
irisDT <- as.data.table(iris[,1:4])

irisDT[, id:= 1:.N]
train_sample <- sample(x = irisDT$id, size = 0.8 * length(irisDT$id))
irisDT_train <- irisDT[id %in% train_sample]
irisDT_test <- irisDT[!(id %in% train_sample)]

irisDT_train[, id:= NULL]
irisDT_test[, id:= NULL]


dist_train <- as.data.table(kNNdist(irisDT_train, k=5))
dist_test <- as.data.table(kNNdist(irisDT_test, k=5))

t11 <- density(dist_train$V1)
t22 <- density(dist_test$V1)

library(ggplot2)
ggplot() + 
  geom_density(data = dist_train, aes(x = V1), 
               fill = "#E69F00", color = "black", alpha = 0.7) + 
  geom_density(data = dist_test, aes(x = V1),
               fill = "#56B4E9", color = "black", alpha = 0.7) +
  ggtitle("Correct way to split")


# Wrong way to do spit 
iris_dist <- as.data.table(kNNdist(iris, k=5))
iris_dist[, id:= 1:.N]

train_sample_dist <- sample(x = iris_dist$id, size = 0.8 * length(iris_dist$id))
irisDT_train_dist <- iris_dist[id %in% train_sample_dist]
irisDT_test_dist <- iris_dist[!(id %in% train_sample_dist)]

irisDT_train_dist[, id:= NULL]
irisDT_test_dist[, id:= NULL]

t1 <- density(irisDT_train_dist$V1)
t2 <- density(irisDT_test_dist$V1)

library(philentropy)
philentropy::euclidean(irisDT_train_dist$V1, dist_train$V1, testNA = F)

philentropy::euclidean(irisDT_test_dist$V1, dist_test$V1, testNA = F)

ggplot() + 
  geom_density(data = irisDT_train_dist, aes(x = V1), 
               fill = "#E69F00", color = "black", alpha = 0.7) + 
  geom_density(data = irisDT_test_dist, aes(x = V1),
               fill = "#56B4E9", color = "black", alpha = 0.7)+
  ggtitle("Wrong way to split")



# Read a DAMI dataset -----------------------------------------------------

originalDT <- fread("data/derived-data/Ionosphere_withoutdupl_norm.csv")
originalDT[, .N, by = Label]
datasetname <- "Ionosphere"
K <- 5

originalDT[, id:= 1:.N]
train_sample <- sample(x = originalDT$id, size = 0.7 * length(originalDT$id))
originalDT_train <- originalDT[id %in% train_sample]
originalDT_train[, .N, by = Label]
originalDT_test <- originalDT[!(id %in% train_sample)]
originalDT_test[, .N, by = Label]

originalDT_train[, `:=` (id = NULL, Label = NULL)]
originalDT_test[, `:=` (id = NULL, Label = NULL)]

dist_train <- as.data.table(kNNdist(originalDT_train, k=K))
dist_test <- as.data.table(kNNdist(originalDT_test, k=K))

t11 <- density(dist_train$V1)
t22 <- density(dist_test$V1)

library(ggplot2)
ggplot() + 
  geom_density(data = dist_train, aes(x = V1), 
               fill = "#E69F00", color = "black", alpha = 0.7) + 
  geom_density(data = dist_test, aes(x = V1),
               fill = "#56B4E9", color = "black", alpha = 0.7) +
  ggtitle(paste0("Correct way to split. K=",K, " & dataset:", datasetname))


# Wrong way to do spit 
label <- originalDT$Label
originalDT[, `:=` (id = NULL, Label = NULL)]
original_dist <- as.data.table(kNNdist(originalDT, k=K))
original_dist[, id:= 1:.N]

train_sample_dist <- sample(x = original_dist$id, size = 0.7 * length(original_dist$id))
originalDT_train_dist <- original_dist[id %in% train_sample_dist]
originalDT_test_dist <- original_dist[!(id %in% train_sample_dist)]

originalDT_train_dist[, id:= NULL]
originalDT_test_dist[, id:= NULL]

t1 <- density(originalDT_train_dist$V1)
t2 <- density(originalDT_test_dist$V1)


ggplot() + 
  geom_density(data = originalDT_train_dist, aes(x = V1), 
               fill = "#E69F00", color = "black", alpha = 0.7) + 
  geom_density(data = originalDT_test_dist, aes(x = V1),
               fill = "#56B4E9", color = "black", alpha = 0.7)+
  ggtitle(paste0("Wrong way to split. K=",K, " & dataset:", datasetname))




library(philentropy)
philentropy::canberra(originalDT_train_dist$V1, dist_train$V1, testNA = F)

philentropy::canberra(originalDT_test_dist$V1, dist_test$V1, testNA = F)