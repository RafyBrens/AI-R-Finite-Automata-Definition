#Reading csv files
data<-read.csv("hw1_data v32.csv")
#OR
file1<-file("hw1_data v32.csv","r")
data1<-read.csv(file1)
close(file1)

#Reading txt files
inicial<-read.table("hw1_data v32.txt", header=TRUE)