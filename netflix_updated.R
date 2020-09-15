movies<-read.table ("u.item.txt",header = FALSE,sep = "|",quote="")

movies<-read.csv("u.item.txt",header = FALSE,sep = "|")

movies<-read.csv("http://files.grouplens.org/datasets/movie
lens/ml-100k/u.item", header = FALSE, sep = "|")

colnames(movies)

str(movies)
head(movies)
colnames(movies)=c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary","Drama","Fantasy","FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller","War","Western")
str(movies)
movies$ID=NULL
movies$ReleaseDate=NULL
movies$VideoReleaseDate=NULL
movies$IMDB=NULL
movies[,c("ID","ReleaseDate","VideoReleaseDate", "IMDB")]<-NULL
movies[,c(1,3,4,5)]<-NULL
which(table(movies$Title)>1)
class(movies)
str(movies)

table(duplicated(movies$Title))

movies<-unique(movies)

which(table(movies$Title)>1)
str(movies)
library(caTools)
set.seed(50)
spl<-sample.split(movies$Title,SplitRatio = 0.8)
#table(spl)
train<-subset(movies,spl==T)
test<-subset(movies,spl==F)
# Quick Question 

table(movies$Comedy)
table(movies$Western)
table(movies$Romance,movies$Drama)
table(movies$Romance==1,movies$Drama==1)
distance<-dist(movies[2:20],method = "euclidean")
distance_train<-dist(train[2:20],method = "euclidean")
head(distance_train)
str(distance)
head(distance)
summary(distance)
clusterMovies<-hclust(d = distance,method="ward.D")
summary(clusterMovies$height)
View(movies[c(1,419),])
movies[c(2,117),]
clusterMovies$merge
summary(clusterMovies)
clusterTrain<-hclust(d=distance_train,method = "ward.D")
plot(clusterMovies)
rect.hclust(clusterMovies,k=10)
plot(clusterTrain)
clusterGroups<-cutree(clusterMovies,k=10)
hist(clusterGroups)
head(clusterGroups,100)
GroupTrain<-cutree(clusterTrain,k=10)
table(GroupTrain)
table(clusterGroups)
prop.table(table(clusterGroups,movies$Action),margin = 1)
table(movies$Action,clusterGroups==1)
tapply(movies$Action,clusterGroups,mean)
tapply(movies$Romance,clusterGroups,mean)
tapply(movies$Comedy,clusterGroups,mean)
subset(movies$Title,clusterGroups==1)
round(tapply(movies$Comedy,clusterGroups,mean),2)
colMeans(subset(movies[2:20], clusterGroups == 1))
agg<-aggregate(movies[2:20],list(clusterGroups),mean)


apply(agg,MARGIN = 1,FUN = sum)
agg_train<-aggregate(train[2:20],list(GroupTrain),mean)
agg_train<-data.frame(agg_train,row.names = 1)
agg<-data.frame(agg,row.names = 1)
trans<-data.frame(t(agg),check.names = T)
trans_train<-data.frame(t(agg_train),check.names = T)
movies$cluster<-clusterGroups
SelectMovie<-sqldf("Select cluster from movies[,2:20] group by cluster")
library(ggplot2)
qplot(clusterGroups,geom="bar")
which(movies$Title== "Men in Black (1997)")
match("Men in Black (1997)",table = movies$Title)
grep("men in black",x = movies$Title,ignore.case = T)
clusterGroups[257]
movies[257,] 
spl = split(movies[2:20], clusterGroups)
spl[[1]]
lapply(spl, colMeans)

movies$clusterGroups<-clusterGroups
# Recommendation Systems
library(sqldf)
grep(pattern = "men in black",x = movies$Title,ignore.case = T)
sqldf("select * from movies where Title like '%Men in Black%' ")
#OR 
subset(movies,Title=="Men in Black (1997)")
clusterGroups[257]
movies2<- subset(movies,clusterGroups==2)
movies2$Title
head(movies2$Title,10)
library(flexclust)
set.seed(50)
hclust.KCCA<-as.kcca(object = clusterTrain,k = 10,data = train[2:20])
summary(hclust.KCCA)
View(hclust.KCCA@centers)
pred<-predict(hclust.KCCA,newdata=test[2:20])
table(pred)
agg_test<-aggregate(test[2:20],by = list(pred),FUN = mean)
verify<-subset(test,pred==10)
# Quick Question
clusterGroups2<-cutree(clusterMovies,k=2)
colMeans(subset(movies[2:20], clusterGroups2 == 2))
colMeans(subset(movies[2:20], clusterGroups2 == 1))
agg2<-aggregate(movies[2:20],list(clusterGroups2),mean)

# Applying kmeans
set.seed(50)
KMC<-kmeans(x = train[2:20],centers = 10,iter.max = 100)
KMC$centers
View(KMC$centers)
table(KMC$cluster)
agg_KMC<-aggregate(x = train[2:20],by = list(KMC$cluster),FUN = mean)
KMC_KCCA<-as.kcca(object = KMC,data = train[2:20])
summary(KMC_KCCA)
KMC_KCCA@centers
pred_KMC<-predict(KMC_KCCA,newdata=test[2:20])

agg_KMC_test<-aggregate(x = test[,2:20],by = list(pred_KMC),FUN = mean)

table(pred_KMC)

verify<-subset(test,pred_KMC==8)
SumWithinss = sapply(2:15, function(x) {kmeans(train[2:20], centers=x, iter.max=1000)$tot.withinss})
SumWithinss
NumClusters<-seq(2,15,1)
plot(NumClusters,SumWithinss,type="b")


# Applying kmeans
set.seed(500)
k=10
KMC<-kmeans(x = movies[2:20],centers = k,iter.max = 100)
names(KMC)
KMC$cluster
table(KMC$cluster)
head(subset(movies$Title,KMC$cluster==1),10)
setdiff(subset(movies$Title,KMC$cluster==1),movies2$Title)
KMC$cluster  
KMC$centers
View(KMC$centers)
KMC$size
KMC$iter
KMC$totss
KMC$withinss
KMC$tot.withinss
KMC$betweenss
KMC$cluster[257]
movies1=subset(movies,KMC$cluster==1)
library(dplyr)
tbl_df(movies1$Title)
head(movies1$Title,10)
set.seed(50)
KMC2<-kmeans(x = movies[2:20],centers = 2,iter.max = 100)
KMC2$tot.withinss
KMC2$betweenss
KMC_train<-kmeans(x = train[,2:20],centers = 10 ,iter.max = 100)
agg_KMC<-aggregate(x = train[2:20],by = list(KMC_train$cluster),FUN = mean)
KMC_KCCA<-as.kcca(object = KMC_train,data = train[2:20])
pred_KMC<-predict(object = KMC_KCCA,newdata=test[2:20])
test_KMC<-cbind(test,pred_KMC)
verify<-subset(test,pred_KMC==8)

SumWithinss = sapply(X =2:15, FUN = function(x) {
              set.seed(50) ; 
              kmeans(movies[2:20], centers=x, iter.max=1000)$tot.withinss
              }
              )
plot(SumWithinss,type='b')
NumClusters<-seq(2,15,1)
Betweenss = sapply(2:15, function(x) {set.seed(50) ; kmeans(movies[2:20], centers=x, iter.max=1000)$betweenss})
plot(SumWithinss)
Fratio = Betweenss/SumWithinss
plot(NumClusters,SumWithinss,type="b")
plot(NumClusters,Betweenss,type="b")
plot(NumClusters,Fratio,type="b")
Withinss<-NULL
for (k in 2:20)
{
  set.seed(50) 
  Withinss[k-1]<-kmeans(movies[2:20], centers=k, iter.max=100)$tot.withinss
}
Withinss
NumClusters=seq(2,20,1)
plot(NumClusters,Withinss,type = "b")
# Plotting scree plot for hclust 
# For 1 cluster
col_var<-sapply(movies[,2:20],var)
sumofsq<-col_var*(nrow(movies)-1)
withinss<-sum(sumofsq)

# For 10 clusters 
agg_var<-aggregate(x = movies[,2:20],by = list(clusterGroups),FUN = var)
View(agg_var)
var<-apply(agg_var[,-1],1,sum)
withinss<-var*(table(clusterGroups)-1)
sum(withinss)

dist2 = dist(movies2[2:20],method = "euclidean")

dist2 = as.matrix(dist2)
apply(dist2,1,min)

library(flexclust)
set.seed(50)
kmeanspp = kcca(x = movies[,2:20],k = 10,control=list(initcent="kmeanspp",iter.max=100,verbose=1,ntry=10))
kmeanspp@totaldist
kmeanspp@clusinfo
View(kmeanspp@centers)
info(kmeanspp,"distsum")
set.seed(50)
KMC <- kmeans(movies[,2:20],centers=10)
KMC$size
KMC$centers
KMC$totss
KMC$withinss
KMC$tot.withinss
KMC$betweenss
KMC$iter


kmcpp <- as.kcca(object=KMC,data=movies[,2:20],control=list(initcent="kmeanspp",iter.max=100,verbose=1,ntry=10))



kmcpp@centers
x<- info(kmcpp,"distsum")

sum(kmcpp@clusinfo$av_dist*kmcpp@clusinfo$size)
withinss<-NULL
for (k in 2:20)
{
  set.seed(50) 
  kmeanspp = kcca(x = movies[,2:20],k = k,control=list(initcent="kmeanspp",iter.max=100,verbose=1,ntry=10))
  withinss[k-1]<-info(kmeanspp,'distsum')
  
}
withinss
numclust <- seq(2,20,1)
plot(numclust,withinss,type='b')

set.seed(50)
kmeanspp_train = kcca(x = train[,2:20],k = 10,control=list(initcent="kmeanspp",iter.max=100,verbose=1,ntry=10))
View(kmeanspp_train@centers)
pred<- predict(kmeanspp_train,newdata=test[2:20])
table(pred)
kmc_agg_test<-aggregate(test[2:20],by = list(pred),mean)
View(kmc_agg_test)


