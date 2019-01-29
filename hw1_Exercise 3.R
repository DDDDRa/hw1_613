# Exercise 3
M<-read.csv("~/Desktop/dat/datstu.csv",stringsAsFactors=FALSE)
M1<-read.csv("~/Desktop/dat/datjss.csv",stringsAsFactors=FALSE)
M2<-read.csv("~/Desktop/dat/datsss.csv",stringsAsFactors=FALSE)

len3<-nrow(M)
NewData<-matrix(data="",nrow=len3,ncol=8)
NewData[,1]<-M[,1]
NewData[,2]<-M[,17]

for(row in 1:len3 ){
  index<-match(NewData[row,2],M1[,2])
  NewData[row,3]<-M1[index,3]
  NewData[row,4]<-M1[index,4]
}


# Construct a new matrix to shwo the admission result of every student
TransData<-matrix(data="NA",nrow=340823,ncol=3)
len2=nrow(M)
TransData[,1]<-c(1:len2)
TransData[,3]<-M[,2]
for (i in 1:len2){
  if(!is.na(M[i,18])){
    if (M[i,18]==1){
      TransData[i,2]=M[i,5]
    }
    if (M[i,18]==2){
      TransData[i,2]=M[i,6]
    }
    if (M[i,18]==3){
      TransData[i,2]=M[i,7]
    }
    if (M[i,18]==4){
      TransData[i,2]=M[i,8]
    }
    if (M[i,18]==5){
      TransData[i,2]=M[i,9]
    }
    if (M[i,18]==6){
      TransData[i,2]=M[i,10]
    }
  }
}
NewData[,5]<-TransData[,2]

for(row in 1:len3 ){
  index<-match(NewData[row,5],M2[,3])
  NewData[row,6]<-M2[index,5]
  NewData[row,7]<-M2[index,6]
}


dist=matrix(data=0,nrow=len3,ncol=1)
for(i in 1:len3){
  ssslong<-as.numeric(NewData[i,6])
  ssslat<-as.numeric(NewData[i,7])
  jsslong<-as.numeric(NewData[i,3])
  jsslat<-as.numeric(NewData[i,4])
  dist[i] = sqrt(((69.172)*(ssslong - jsslong) * cos(jsslat/57.3))^2 + (69.172 * (ssslat-jsslat))^2)
}
NewData[,8]<-dist
#Construct an excel to show the result
write.csv(NewData,"~/Desktop/dat/NewData.csv")