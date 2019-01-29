#Exercise 4
#Import the data from excel forms
M<-read.csv("~/Desktop/dat/datstu.csv",stringsAsFactors=FALSE)
M1<-read.csv("~/Desktop/dat/NewData.csv",stringsAsFactors=FALSE)
M2<-read.csv("~/Desktop/dat/MyData.csv",stringsAsFactors=FALSE)
#Delete NAs in M2
M2[M2==""|M2=="NA"]<-NA
M2<-na.omit(M2)
M3<-read.csv("~/Desktop/dat/datsss.csv",stringsAsFactors=FALSE)
#Name M1
colnames(M1)<-c("X","X","district","slongitude","slatitude","Schoolcode","slongitude","slatitude","Distance")
#Delete NAs in M1
M1<-data.frame(M1[,6],M1[,9])
M1[M1==" "|M1=="NA"]<-NA
M1<-na.omit(M1)
#Delete NAs in M
M[M==" "|M=="NA"]<-NA
M<-na.omit(M,cols="rankplace")
#Take out hte data from the excel
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
#Calculate the mean and standard variance of cutoff and quality in the 6th choice
#Constract a data.frame of school code, school name and program name
df1<-data.frame(dfs1,dfp1)
Trans1=matrix(data=NA,nrow=length(dfs1),ncol=3)
Trans1[,3]<-dfp1
Trans1[,1]<-dfs1
for (i in 1:length(dfs1)){
  index<-match(Trans1[i,1],M3[,3])
  Trans1[i,2]<-M3[index,2]
}
#merge the new data.frame and M2
colnames(Trans1)<-c("Schoolcode","School","Program")
X<-data.frame(Trans1)
Y<-data.frame(M2)
Pgn1<-merge(X,Y,by.X=c("School","program"),by.Y=c("School","program"),all.X=TRUE)

#Calculate the mean and standard variance of cutoff and quality in the 2th choice
df2<-data.frame(dfs2,dfp2)
Trans2=matrix(data=NA,nrow=length(dfs2),ncol=3)
Trans2[,3]<-dfp2
Trans2[,1]<-dfs2
for (i in 1:length(dfs2)){
  index<-match(Trans2[i,1],M3[,3])
  Trans2[i,2]<-M3[index,2]
}
colnames(Trans2)<-c("Schoolcode","School","Program")
X<-data.frame(Trans2)
Y<-data.frame(M2)
Pgn2<-merge(X,Y,by.X=c("School","program"),by.Y=c("School","program"),all.X=TRUE)

#Calculate the mean and standard variance of cutoff and quality in the 3th choice
df3<-data.frame(dfs3,dfp3)
Trans3<-matrix(data=NA,nrow=length(dfs3),ncol=3)
Trans3[,3]<-dfp3
Trans3[,1]<-dfs3
for (i in 1:length(dfs3)){
  index<-match(Trans3[i,1],M3[,3])
  Trans3[i,2]<-M3[index,2]
}
colnames(Trans3)<-c("Schoolcode","School","Program")
X<-data.frame(Trans3)
Y<-data.frame(M2)
Pgn3<-merge(X,Y,by.X=c("School","program"),by.Y=c("School","program"),all.X=TRUE)

#Calculate the mean and standard variance of cutoff and quality in the 4th choice
df4<-data.frame(dfs4,dfp4)
Trans4=matrix(data=NA,nrow=length(dfs4),ncol=3)
Trans4[,3]<-dfp4
Trans4[,1]<-dfs4
for (i in 1:length(dfs4)){
  index<-match(Trans4[i,1],M3[,3])
  Trans4[i,2]<-M3[index,2]
}
colnames(Trans4)<-c("Schoolcode","School","Program")
X<-data.frame(Trans4)
Y<-data.frame(M2)
Pgn4<-merge(X,Y,by.X=c("School","program"),by.Y=c("School","program"),all.X=TRUE)

#Calculate the mean and standard variance of cutoff and quality in the 5th choice
df5<-data.frame(dfs5,dfp5)
Trans5=matrix(data=NA,nrow=length(dfs5),ncol=3)
Trans5[,3]<-dfp5
Trans5[,1]<-dfs5
for (i in 1:length(dfs5)){
  index<-match(Trans5[i,1],M3[,3])
  Trans5[i,2]<-M3[index,2]
}
colnames(Trans5)<-c("Schoolcode","School","Program")
X<-data.frame(Trans5)
Y<-data.frame(M2)
Pgn5<-merge(X,Y,by.X=c("School","program"),by.Y=c("School","program"),all.X=TRUE)

#Calculate the mean and standard variance of cutoff and quality in the 6th choice
df6<-data.frame(dfs6,dfp6)
Trans6=matrix(data=NA,nrow=length(dfs6),ncol=3)
Trans6[,3]<-dfp6
Trans6[,1]<-dfs6
for (i in 1:length(dfs6)){
  index<-match(Trans6[i,1],M3[,3])
  Trans6[i,2]<-M3[index,2]
}
colnames(Trans6)<-c("Schoolcode","School","Program")
X<-data.frame(Trans6)
Y<-data.frame(M2)
Pgn6<-merge(X,Y,by.X=c("School","program"),by.Y=c("School","program"),all.X=TRUE)

#Export the data to excel
write.csv(Pgn1,"~/Desktop/dat/test1.csv")
write.csv(Pgn2,"~/Desktop/dat/test2.csv")
write.csv(Pgn3,"~/Desktop/dat/test3.csv")
write.csv(Pgn4,"~/Desktop/dat/test4.csv")
write.csv(Pgn5,"~/Desktop/dat/test5.csv")
write.csv(Pgn6,"~/Desktop/dat/test6.csv")
#Import the data
#Calculate the average of cutoff and quality for rank 1
K<-read.csv("~/Desktop/dat/test1.csv")
Average1cutoff<-mean(as.numeric(K[,9]))
sd1cutoff<-sd(as.numeric(K[,9]))
Average1quality<-mean(as.numeric(K[,10]))
sd1quality<-sd(as.numeric(K[,10]))
#Import the data
#Calculate the average of cutoff and quality for rank 2
K<-read.csv("~/Desktop/dat/test2.csv")
Average2cutoff<-mean(as.numeric(K[,9]))
sd2cutoff<-sd(as.numeric(K[,9]))
Average2quality<-mean(as.numeric(K[,10]))
sd2quality<-sd(as.numeric(K[,10]))
#Import the data
#Calculate the average of cutoff and quality for rank 3
K<-read.csv("~/Desktop/dat/test3.csv")
Average3cutoff<-mean(as.numeric(K[,9]))
sd3cutoff<-sd(as.numeric(K[,9]))
Average3quality<-mean(as.numeric(K[,10]))
sd3quality<-sd(as.numeric(K[,10]))
#Import the data
#Calculate the average of cutoff and quality for rank 4
K<-read.csv("~/Desktop/dat/test4.csv")
Average4cutoff<-mean(as.numeric(K[,9]))
sd4cutoff<-sd(as.numeric(K[,9]))
Average4quality<-mean(as.numeric(K[,10]))
sd4quality<-sd(as.numeric(K[,10]))
#Import the data
#Calculate the average of cutoff and quality for rank 5
K<-read.csv("~/Desktop/dat/test5.csv")
Average5cutoff<-mean(as.numeric(K[,9]))
sd5cutoff<-sd(as.numeric(K[,9]))
Average5quality<-mean(as.numeric(K[,10]))
sd5quality<-sd(as.numeric(K[,10]))
#Import the data
#Calculate the average of cutoff and quality for rank 6
K<-read.csv("~/Desktop/dat/test6.csv")
Average6cutoff<-mean(as.numeric(K[,9]))
sd6cutoff<-sd(as.numeric(K[,9]))
Average6quality<-mean(as.numeric(K[,10]))
sd6quality<-sd(as.numeric(K[,10]))

#Calculate the distance of each rank
Dist1<-matrix(data=0,nrow=length(dfs1),ncol=2)
Dist1[,1]<-dfs1
for(i in 1:length(dfs1)){
  index<-match(dfs1[i],M1[,1])
  Dist1[i,2]<-M1[index,2]
}
D1<-na.omit(Dist1[,2])
mean(D1)
sd(D1)

Dist2<-matrix(data=0,nrow=length(dfs2),ncol=2)
Dist2[,1]<-dfs2
for(i in 1:length(dfs2)){
  index<-match(dfs2[i],M1[,1])
  Dist2[i,2]<-M1[index,2]
}
D2<-na.omit(Dist2[,2])
mean(D2)
sd(D2)

Dist3<-matrix(data=0,nrow=length(dfs3),ncol=2)
Dist3[,1]<-dfs3
for(i in 1:length(dfs3)){
  index<-match(dfs3[i],M1[,1])
  Dist3[i,2]<-M1[index,2]
}
D3<-na.omit(Dist3[,2])
mean(D3)
sd(D3)

Dist4<-matrix(data=0,nrow=length(dfs4),ncol=2)
Dist4[,1]<-dfs4
for(i in 1:length(dfs4)){
  index<-match(dfs4[i],M1[,1])
  Dist4[i,2]<-M1[index,2]
}
D4<-na.omit(Dist4[,2])
mean(D4)
sd(D4)

Dist5<-matrix(data=0,nrow=length(dfs5),ncol=2)
Dist5[,1]<-dfs5
for(i in 1:length(dfs5)){
  index<-match(dfs5[i],M1[,1])
  Dist5[i,2]<-M1[index,2]
}
D5<-na.omit(Dist5[,2])
mean(D5)
sd(D5)

Dist6<-matrix(data=0,nrow=length(dfs6),ncol=2)
Dist6[,1]<-dfs6
for(i in 1:length(dfs6)){
  index<-match(dfs6[i],M1[,1])
  Dist6[i,2]<-M1[index,2]
}
D6<-na.omit(Dist6[,2])
mean(D6)
sd(D6)

#Redo the question (diﬀerentiating by student test score quantiles) 
M[M==" "|M=="NA"]<-NA
M<-na.omit(M,cols="score")
DaTa<-as.numeric(M[,2])
group<-quantile(DaTa)
pgn1<-c()
pgn2<-c()
pgn3<-c()
pgn4<-c()
for (i in 2:length(DaTa)){
  if ((DaTa[i]>group[1]) & (DaTa[i]<=group[2])){
    pgn1<-c(pgn1,i)
  }
  if ((DaTa[i]>group[2]) & (DaTa[i]<=group[3])){
    pgn2<-c(pgn2,i)
  }
  if (DaTa[i]>group[3] & DaTa[i]<=group[4]){
    pgn3<-c(pgn3,i)
  }
  if (DaTa[i]>group[4] & DaTa[i]<=group[5]){
    pgn4<-c(pgn4,i)
  }
}


gn1cutoff<-c()
gn1quality<-c()
for (j in pgn1){
  gn1cutoff<-c(gn1cutoff,Pgn1[j,8],Pgn2[j,8],Pgn3[j,8],Pgn4[j,8],Pgn5[j,8],Pgn6[j,8])
  gn1quality<-c(gn1quality,Pgn1[j,9],Pgn2[j,9],Pgn3[j,9],Pgn4[j,9],Pgn5[j,9],Pgn6[j,9])
}

write.csv(gn1cutoff,"~/Desktop/dat/tst1.csv")
write.csv(gn1quality,"~/Desktop/dat/tt1.csv")
#Calculate average and standard variance of cutoff
K<-read.csv("~/Desktop/dat/tst1.csv")
est<-as.numeric(na.omit(K[,2]))
mean(est)
sd(est)
#Calculate average and standard variance of quality
K<-read.csv("~/Desktop/dat/tt1.csv")
est<-as.numeric(na.omit(K[,2]))
mean(est)
sd(est)

gn2cutoff<-c()
gn2quality<-c()
for (j in pgn2){
  gn2cutoff<-c(gn2cutoff,Pgn1[j,8],Pgn2[j,8],Pgn3[j,8],Pgn4[j,8],Pgn5[j,8],Pgn6[j,8])
  gn2quality<-c(gn2quality,Pgn1[j,9],Pgn2[j,9],Pgn3[j,9],Pgn4[j,9],Pgn5[j,9],Pgn6[j,9])
}
write.csv(gn2cutoff,"~/Desktop/dat/tst2.csv")
write.csv(gn2quality,"~/Desktop/dat/tt2.csv")
#Calculate average and standard variance of cutoff
K<-read.csv("~/Desktop/dat/tst2.csv")
est<-as.numeric(na.omit(K[,2]))
mean(est)
sd(est)
#Calculate average and standard variance of quality
K<-read.csv("~/Desktop/dat/tt2.csv")
est<-as.numeric(na.omit(K[,2]))
mean(est)
sd(est)

gn3cutoff<-c()
gn3quality<-c()
for (j in pgn3){
  gn3cutoff<-c(gn3cutoff,Pgn1[j,8],Pgn2[j,8],Pgn3[j,8],Pgn4[j,8],Pgn5[j,8],Pgn6[j,8])
  gn3quality<-c(gn3quality,Pgn1[j,8],Pgn2[j,8],Pgn3[j,8],Pgn4[j,8],Pgn5[j,8],Pgn6[j,8])
}
write.csv(gn3cutoff,"~/Desktop/dat/tst3.csv")
write.csv(gn3quality,"~/Desktop/dat/tt3.csv")
#Calculate average and standard variance of cutoff
K<-read.csv("~/Desktop/dat/tst3.csv")
est<-as.numeric(na.omit(K[,2]))
mean(est)
sd(est)
#Calculate average and standard variance of quality
K<-read.csv("~/Desktop/dat/tt3.csv")
est<-as.numeric(na.omit(K[,2]))
mean(est)
sd(est)


gn4cutoff<-c()
gn4quality<-c()
for (j in pgn4){
  gn4cutoff<-c(gn4cutoff,Pgn1[j,8],Pgn2[j,8],Pgn3[j,8],Pgn4[j,8],Pgn5[j,8],Pgn6[j,8])
  gn4quality<-c(gn4quality,Pgn1[j,8],Pgn2[j,8],Pgn3[j,8],Pgn4[j,8],Pgn5[j,8],Pgn6[j,8])
}
write.csv(gn4cutoff,"~/Desktop/dat/tst4.csv")
write.csv(gn4quality,"~/Desktop/dat/tt4.csv")
#Calculate average and standard variance of cutoff
K<-read.csv("~/Desktop/dat/tst4.csv")
est<-as.numeric(na.omit(K[,2]))
mean(est)
sd(est)
#Calculate average and standard variance of quality
K<-read.csv("~/Desktop/dat/tt4.csv")
est<-as.numeric(na.omit(K[,2]))
mean(est)
sd(est)


