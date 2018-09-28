#install.packages("MASS")
library(MASS)
data = survey
View(data)
table1 = table(data$Exer,data$Smoke)
View(table1)
chisq.test(table1)


ctb = cbind(table1["Freq",], table1["None",]+table1["Some",])
ctb
colnames(ctb) = c("Freq","Non_freq")

chisq.test(ctb)
#####################################################################

wer = table(data$Sex,data$Smoke)
wer
jkl = cbind(wer[,"Heavy"] + wer[,"Occas"] + wer[,"Regul"], wer[])
jkl

colnames(jkl) = c("Heavy","Never","Regular")

####################################################################
library(dplyr)


fgg = iris
levels(fgg$Species)
View(fgg)

data1 = iris[-5]
View(data1)

### Standardize the data
# It doesn't results into loss of information
data2 = scale(data1)
View(data2)
class(data2)
# It gives side effect that it converts into matrix form, not in dataframe
data2 = as.data.frame(data2)

# Creating the clusters
cluster = kmeans(data2, 3)
cluster


## FInding centroids of clusters

cluster$centers


## Clusters 1 to 10

cluster1 = kmeans(data2,1)
cluster2 = kmeans(data2,2)
cluster3 = kmeans(data2,3)
cluster4 = kmeans(data2,4)
cluster5 = kmeans(data2,5)
cluster6 = kmeans(data2,6)
cluster7 = kmeans(data2,7)
cluster8 = kmeans(data2,8)
cluster9 = kmeans(data2,9)
cluster10 = kmeans(data2,10)



y = c(cluster1$tot.withinss,cluster2$tot.withinss,cluster3$tot.withinss,cluster4$tot.withinss,cluster5$tot.withinss,cluster6$tot.withinss,cluster7$tot.withinss,cluster8$tot.withinss,cluster9$tot.withinss,cluster10$tot.withinss)

x = c(1,2,3,4,5,6,7,8,9,10)

plot(x, y, xlab = "No. of Cluster", ylab = "Total withiness", type = "b", col="red")
help(plot)
####################################################################

data1$Cluster_type = cluster$cluster
View(data1)


# using clustering we can not preddict, we can only describe supervised data 

species = as.list(iris$Species)
species
species = unlist(species)
species

help("unlist")

table(species,cluster$cluster)

plot(iris[c("Sepal.Length","Sepal.Width")], col=cluster$cluster)

#####  Descision Tree #####

install.packages("rpart")
install.packages("rpart.plot")
install.packages("party")
library(rpart)
library(rpart.plot)
library(party)

## Dividing the data into test and train data


pd = sample(2, nrow(iris), prob = c(0.8,0.2), replace = TRUE)


train = iris[pd==1, ]

test = iris[pd==2, ]

# Creating the model

model = ctree(Species~., data = train)
help("rpart")
plot(model)

# Dot is for all the columns 

#library(dplyr)
#df= train %>% filter(Petal.Length<2.5)%>%View()

## Let's preddict the model

pred = predict(model, test[,1:4]) 
pred


## Confusion Matrix
tab = table(pred, test$Species)
tab

## Let validate for the below values

val1 = 2.5
val2 = 3.5
val3 = 11.75
val4 = 0.6

val_df = cbind(val1,val2,val3,val4)

colnames(val_df) = c("Sepal.Length", "Sepal.Width","Petal.Length", "Petal.Width")

val_df = data.frame(val_df)


View(val_df)

## Preddicting the classification of the above data

val_pred = predict(model, val_df)
val_pred

