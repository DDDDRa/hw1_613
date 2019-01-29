#Use the library
library(dplyr)
#Import data
df<-read.csv("~/Desktop/dat/MyData.csv",stringsAsFactors=FALSE)
M<-read.csv("~/Desktop/dat/datstu.csv",stringsAsFactors=FALSE)
M1<-read.csv("~/Desktop/dat/datsss.csv",stringsAsFactors=FALSE)
# Omite NAs
df[df=="NA"|df==" "]<-NA
Data<-na.omit(df)
Trans<-Data[,7]
Trans<-na.omit(Trans)
Trans<-as.numeric(Trans)
Trans<-na.omit(Trans)
#Calculate the quantiles
group<-quantile(Trans,prob=seq(0,1,length=11),na.rm=FALSE)
#According to the factor:group, classify the school+program in the excel from in exercise 3 into 10 gorups
#Assign each school a number from 1 to 10, which group it belongs to 
K<-matrix(data=NA,nrow=nrow(Data),ncol=4)
K[,1]<-Data[,2]
K[,2]<-Data[,3]
for (i in 1:nrow(Data)){
  if((Data[i,7]<group[2])&(Data[i,7]>=group[1])){
    K[i,3]<-1
  }
  if((Data[i,7]<group[3])&(Data[i,7]>=group[2])){
    K[i,3]<-2
  }
  if((Data[i,7]<group[4])&(Data[i,7]>=group[3])){
    K[i,3]<-3
  }
  if((Data[i,7]<group[5])&(Data[i,7]>=group[4])){
    K[i,3]<-4
  }
  if((Data[i,7]<group[6])&(Data[i,7]>=group[5])){
    K[i,3]<-5
  }
  if((Data[i,7]<group[7])&(Data[i,7]>=group[6])){
    K[i,3]<-6
  }
  if((Data[i,7]<group[8])&(Data[i,7]>=group[7])){
    K[i,3]<-7
  }
  if((Data[i,7]<group[9])&(Data[i,7]>=group[8])){
    K[i,3]<-8
  }
  if((Data[i,7]<group[10])&(Data[i,7]>=group[9])){
    K[i,3]<-9
  }
  if((Data[i,7]<group[11])&(Data[i,7]>=group[10])){
      K[i,3]<-10
    }
}

for (i in 1:nrow(Data)){
  index<-match(K[i,1],M1[,2])
  K[i,4]<-M1[index,3]
  
} 
#Take out the data about the school and program in the excel datstu.csv
dfs1<-M[,5]
dfp1<-M[,11]
dfs2<-M[,6]
dfp2<-M[,12]
dfs3<-M[,7]
dfp3<-M[,13]
dfs4<-M[,8]
dfp4<-M[,14]
dfs5<-M[,9]
dfp5<-M[,15]
dfs6<-M[,10]
dfp6<-M[,16]
df1<-data.frame(dfs1,dfp1)
df2<-data.frame(dfs2,dfp2)
df3<-data.frame(dfs3,dfp3)
df4<-data.frame(dfs4,dfp4)
df5<-data.frame(dfs5,dfp5)
df6<-data.frame(dfs6,dfp6)
colnames(df1)<-c("Schoolcode","Program")
colnames(K)<-c("School","Program","Group","Schoolcode")
#Merge the original data with the data newly constructed


X<-df1
Y<-K
MER<-merge(X,Y,by=c("Schoolcode","Program"),all.X=TRUE)
#
colnames(df2)<-c("Schoolcode","Program")
X<-df2
Y<-K
MER2<-merge(X,Y,by=c("Schoolcode","Program"),all.X=TRUE)

colnames(df3)<-c("Schoolcode","Program")
X<-df3
Y<-K
MER3<-merge(X,Y,by=c("Schoolcode","Program"),all.X=TRUE)

colnames(df4)<-c("Schoolcode","Program")
X<-df4
Y<-K
MER4<-merge(X,Y,by=c("Schoolcode","Program"),all.X=TRUE)

colnames(df5)<-c("Schoolcode","Program")
X<-df5
Y<-K
MER5<-merge(X,Y,by=c("Schoolcode","Program"),all.X=TRUE)

colnames(df6)<-c("Schoolcode","Program")
X<-df6
Y<-K
MER6<-merge(X,Y,by=c("Schoolcode","Program"),all.X=TRUE)

Data_q5<-data.frame(MER,MER2,MER3,MER4,MER5,MER6)


