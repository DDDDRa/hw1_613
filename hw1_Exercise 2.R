#Install package
install.packages("dplyr")
#Use package
library(dplyr)
# Import the data from datsss.csv and datstu.csv
M<-read.csv("~/Desktop/dat/datsss.csv",stringsAsFactors=FALSE)
M1<-read.csv("~/Desktop/dat/datstu.csv",stringsAsFactors=FALSE)
df<-data.frame(M1[1:340823,5:10])
schoolcho<-stack(df)
len=nrow(schoolcho)
df1<-M1[1:340823,11:16]
programcho<-stack(df1)
MyData<-matrix(data="NA",nrow=2044938,ncol=8)
colnames(MyData)<-c("schoolcode","program","sdistrict","slongitude","slatitude","cutoff","quality","size")
MyData[,1]<-schoolcho[,1]
MyData[,2]<-programcho[,1] 
MyData<-unique(MyData)
len=nrow(MyData)
len2=nrow(M1)
for (i in 1:len){
  index<-match(MyData[i,1],t(M[,3]))
  MyData[i,3]<-M[index,4]
  MyData[i,4]<-M[index,5]
  MyData[i,5]<-M[index,6]
}


#Calculate the cutoff, average score and size number (Answering question 4,5,6)

TransData<-matrix(data="NA",nrow=340823,ncol=5)

TransData[,1]<-c(1:len2)
TransData[,4]<-M1[,2]
for (i in 1:len2){
  if(!is.na(M1[i,18])){
    if (M1[i,18]==1){
      TransData[i,2]=M1[i,5]
      TransData[i,3]=M1[i,11]
    }
    if (M1[i,18]==2){
      TransData[i,2]=M1[i,6]
      TransData[i,3]=M1[i,12]
    }
    if (M1[i,18]==3){
      TransData[i,2]=M1[i,7]
      TransData[i,3]=M1[i,13]
    }
    if (M1[i,18]==4){
      TransData[i,2]=M1[i,8]
      TransData[i,3]=M1[i,14]
    }
    if (M1[i,18]==5){
      TransData[i,2]=M1[i,9]
      TransData[i,3]=M1[i,15]
    }
    if (M1[i,18]==6){
      TransData[i,2]=M1[i,10]
      TransData[i,3]=M1[i,16]
    }
  }
}
colnames(TransData)<-c("student","school","program","score","schoolname")
#TransData[TransData==""|TransData=="NA"]<-NA
#TransD<-na.omit(TransData,cols=TransData[,4])
#Calculate the cutoff(Answering question 4)
Trans4<-aggregate(as.numeric(TransData[,4]),by=list(TransData[,2], TransData[,3]),FUN=min)
names(Trans4)<-c("schoolcode","program","cutoff")
# Calculate the average score of the students admitted (Answering question 5)
Trans5<-aggregate(as.numeric(TransData[,4]),by=list(TransData[,2], TransData[,3]),FUN=mean)
names(Trans5)<-c("schoolcode","program","quality")
#Calculate the size of each program (Answering question 6)
Trans6<-aggregate(as.numeric(TransData[,4]),by=list(TransData[,2], TransData[,3]),FUN=length)
names(Trans6)<-c("schoolcode","program","size")
# Oeganize all the data in a excel form
#MyData<-data.frame(MyData,stringsAsFactors = FALSE)
#Trans4<-data.frame(Trans4,stringsAsFactors = FALSE)
#Trans5<-data.frame(Trans5,stringsAsFactors = FALSE)
#Trans6<-data.frame(Trans6,stringsAsFactors = FALSE)
#test<-merge(MyData,Trans4,by.MyData=c("schoolcode","program"),by.Trans4=c("schoolcode","program"),all=TRUE)
#test<-merge(test,Trans5,by.test=c("schoolcode","program"),by.Trans5=c("schoolcode","program"),all=TRUE)
#test<-merge(test,Trans6,by.test=c("schoolcode","program"),by.Trans6=c("schoolcode","program"),all=TRUE)
for(m in 1:nrow(MyData)){
  index1<-match(MyData[m,1],Trans4[,1])
  MyData[m,6]<-Trans4[index1,3]
}

for(m in 1:nrow(MyData)){
  index2<-match(MyData[m,1],Trans5[,1])
  MyData[m,7]<-Trans5[index2,3]
}

for(m in 1:nrow(MyData)){
  index3<-match(MyData[m,1],Trans6[,1])
  MyData[m,8]<-Trans6[index3,3]
}

#Transfer the school code to school name
for(i in 1:nrow(MyData)){
  indexnew<-match(MyData[i,1],M[,3])
  MyData[i,1]<-M[index,2]
} 


colnames(MyData)<-c("School","Program","District","Longitude","Latitude","Cutoff","Quality","Size")

write.csv(MyData,"~/Desktop/dat/MyData.csv")
