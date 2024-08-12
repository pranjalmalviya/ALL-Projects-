df=read.csv("C:\\Users\\hp\\OneDrive\\Desktop\\machine learning\\dataset\\maleria dataset.csv")
str(df)


normalize=function(x){
  return ((x)-min(x))/(max(x)-min(x))
}

df_n=data.frame(lapply(df[2:6], normalize))
df_n

df$Label=as.factor(df$Label)

#library(fastDummies)
#df$Label=dummy_cols(df$Label)[-c(1,2)]
#logistic
train_n=df_n[1:20000,]
test_n=df_n[20000:27558,]

#splitting
train=df[1:20000,]
test=df[20000:27558,]

algo =glm(df$Label~df$area_0+df$area_1+df$area_2+df$area_3+df$area_4,data=df,family=binomial)
summary(algo)

pr=predict(algo,df,interval="predict")

t=table(pr,df$Label)
acc=sum(diag(t))/sum(t)
acc

#knn
library(class)
knn=knn(train_n,test_n,cl=train$Label,k=2)

t=table(pr,df$Label)
acc=sum(diag(t))/sum(t)
acc

#kmeans clustring
dar=df[-1]
m=kmeans(dar,2)
plot(df[c("area_1","area_2")],col=m$cluster)
plot(df[c("area_1","area_2")],col=df$Label)

das=table(df$Label,m$cluster)
das

acc= sum(diag(das))/sum(das)
acc
