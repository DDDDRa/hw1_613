#Exercise 1

#Import data from datstu
M<-read.csv("~/Desktop/613/data/datstu.csv")

#Calculate the number of students (Answering question 1) 
numstudents=nrow(M)
print(numstudents)

#Calculate the number of schools (Answering question 2)
#The number of school that students apply to
df1<-unlist(M[,5:10])
df1uni<-unique(df1)
numschools<-length(df1uni)
print(numschools)

#The number of schools 
MM<-read.csv("~/Desktop/dat/datsss.csv")
df11uni<-unique(MM[,3])
#df1=stack(cho_school)
numscho<-length(df11uni)
print(numscho)

#Calculate the number of programs (Answering question 3)
#Import data from the datstu
df<-read.csv("~/Desktop/dat/datstu.csv",stringsAsFactors=FALSE)
#Collect the students' chocie of programs 
df<-df[1:340823,11:16]
#Set an empty vector
pgn<-vector()
for (row in 1:nrow(df)){
  choices<-df[row,]
  pgn<-union(pgn,choices)
}
# Minus one
# Print the result
print (length(pgn)-1)

#Calculate the number of choice (school program)  (Answering question 4)
df<-read.csv("~/Desktop/613/data/datstu.csv",stringsAsFactors=FALSE)

#Collect the school code in the first choice as dfs1
dfs1<-df[1:340823,5]
dfs1[dfs1==""]<-NA
#Collect the school code in the second choice as dfs2
dfs2<-df[1:340823,6]
dfs1[dfs1==""]<-NA
#Collect the school code in the third choice as dfs3
dfs3<-df[1:340823,7]
dfs1[dfs1==""]<-NA
#Collect the school code in the fourth choice as dfs4
dfs4<-df[1:340823,8]
dfs1[dfs1==""]<-NA
#Collect the school code in the fifth choice as dfs5
dfs5<-df[1:340823,9]
dfs1[dfs1==""]<-NA
#Collect the school code in the sixth choice as dfs6
dfs6<-df[1:340823,10]
dfs1[dfs1==""]<-NA
#Collect the program in the first choice as dfp1
dfp1<-df[1:340823,11]
dfp1[dfp1==""]<-NA
#Collect the program in the second choice as dfp2
dfp2<-df[1:340823,12]
dfp2[dfp2==""]<-NA
#Collect the program in the third choice as dfp3
dfp3<-df[1:340823,13]
dfp3[dfp3==""]<-NA
#Collect the program in the fourth choice as dfp4
dfp4<-df[1:340823,14]
dfp4[dfp4==""]<-NA
#Collect the program in the fifth choice as dfp5
dfp5<-df[1:340823,15]
dfp5[dfp5==""]<-NA
#Collect the program in the sixth choice as dfp6
dfp6<-df[1:340823,16]
dfp6[dfp6==""]<-NA
# List students first ,second ,third, fourth, fifth and sixth choice respectively
# Consider students' choices regarding both the school code and the specific program 
df1<-data.frame(dfs1,dfp1)
df2<-data.frame(dfs2,dfp2)
df3<-data.frame(dfs3,dfp3)
df4<-data.frame(dfs4,dfp4)
df5<-data.frame(dfs5,dfp5)
df6<-data.frame(dfs6,dfp6)

#Equate the name of each choice's list and combine these lists into a single list named as combination
names(df1)<-c("schoolcode","programchoice")
names(df2)<-c("schoolcode","programchoice")
names(df3)<-c("schoolcode","programchoice")
names(df4)<-c("schoolcode","programchoice")
names(df5)<-c("schoolcode","programchoice")
names(df6)<-c("schoolcode","programchoice")

combination<-rbind(df1,df2,df3,df4,df5,df6)
combination<-na.omit(combination)
total<-unique(combination)
numchoices=nrow(total)
print(numchoices)

#Calculate the number of missing scores (Answering question 5)
M<-read.csv("~/Desktop/613/data/datstu.csv")
score<-M[,2]
socre[score==""|score=="Na"]<-NA
s=0
#Take out the score of each student and count the students who do not have their scores
for (row in 1:length(score)){
  if(is.na(score[row])){
    s=s+1
  }
}
#Print the answer
print(s)

#Calculate the number of students who apply to the same school (Answering question 6)
#Import data from the datstu
df<-read.csv("~/Desktop/dat/datstu.csv")
#Collect the students' chocies of programs 
dfq6<-df[1:340823,5:10]
names(dfq6)<-c("schoolcode","schoolcode","schoolcode","schoolcode","schoolcode","schoolcode")
dfq6[dfq6==""]<-NA
#Set a variable to sum up the number of choices of programs
s=0
#Sum up the number of choices of programs by using for loop
len<-nrow(dfq6)
for (row in 1:len){
  numna<-sum(is.na(dfq6[row,]))
  trans<-unlist(dfq6[row,])
  l<-length(trans)-numna
  trans1<-unique(trans)
  l1<-length(trans1)
  if(numna>=1){
      l1<-length(trans1)-1
  }
  
  if(l1<l){
    s=s+1
  }
}
#Print the answer 
print (s)

#Calculate the number of students who apply to less than six choices (Answering question 7)
M=read.csv("~/Desktop/dat/datstu.csv")
#Collect the data of program choices
dfq7<-M[,5:16]
dfq7[dfq7==""]<-NA
#Set a variable to count number
s=0
for (row in 1:nrow(dfq7)){
  #If the last choice of program blank is not filled, the student does not apply to up to six programs.
  if(sum(is.na(dfq7[row,]))>=1){
    s=s+1
  }
}
print (s)

