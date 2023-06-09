library(mice)
library(ggplot2)
library(tidyverse)


#read dataset
df<-read.csv('G2_anthropometry.csv')
df

df$gender[df$gender == 'F']<-'Female'
df$gender[df$gender == 'cm']<-'Male'
df

#Dataset Sense
nrow(df)
ncol(df)
colnames(df)<-c('Age', 'Gender', 'Foot_length',   'Height')
df
str(df)

#Data Slicing

head(df,30)
tail(df,30)
result<-df[1:8,c(1,2,3)]
result
result2<-df[-(10:20),]
df[-(20:50),-c(1,3)]
df[-(20:50),1:3]

foot_len<-head(df["Foot_length"],3)
foot_len # data frame
class(foot_len)

#using foot_len to bint to other dataframe
name<-c('ali','omar','mona')
age<-c(9,12,14)
data1<-data.frame(name,age)
all<-cbind(data1,foot_len)
all

#filter dataset
filter1<-df[df$Gender == 'Male' & df$Age>10 , ]
filter1
filter2<-df[df$Gender == 'Male' & df$Age<10 , c(1,2)]
filter2

#Data Sorting

sort1<-df[order(df$Age),]
sort2<-df[order(-df$Age),1:2]

#Re-coding Columns
 
df$level_of_age[df$Age<10]='child'
df$level_of_age[df$Age>=10]='teenager'
df

#Dealing with Missing Data

df

df<-read.csv('G2_anthropometry.csv')
df
# Re-code gender
df$gender[df$gender == 'F']<-'Female'
df$gender[df$gender == 'cm']<-'Male'

df$level_of_age[df$age<10]='child'
df$level_of_age[df$age>=10]='teenager'
df

complete.cases(df)
df[ ! complete.cases(df), ]

df
# Remove cm from the height column
df$height <- gsub("cm", "", df$height)
# Converting variables
df$height<-as.numeric(df$height)
df$gender <- as.factor(df$gender)
df$foot_length<-as.numeric(df$foot_length)
str(df)

#using mice function
pre.imp <-mice(df,m=8,meth=c("","","pmm",""),maxit = 20)
pre.imp <- mice(df, m = 8, meth = c("", "", "pmm", ""), maxit = 20)
pre.imp$imp
dfnew<-complete(pre.imp,4)

#using median 
df[is.na(df$foot_length),]
med<-median(df[df$gender == 'Female','foot_length'],na.rm = T)
df[is.na(df$foot_length) & df$gender == 'Female', "foot_length"] <- med

med2<-median(df[df$gender == 'Male','foot_length'],na.rm = T)
df[is.na(df$foot_length) & df$gender == 'Male', "foot_length"] <- med2


#Re-coding Columns

#visualization

#histogram
Hist <- ggplot(df, aes(x = height)) +
geom_histogram(binwidth = 5, color = "black", fill = "green", alpha = .9)+
ggtitle("height Histogram")+
labs(x = "Children height", y = "Number")

#barchart

Bar<- ggplot(df, aes(x = gender , fill=level_of_age)) +
geom_bar() +
labs(y = "gender Category", title = "gender Bar Chart") +
facet_wrap(~gender)

#box blot

box<- ggplot(df, aes(x= gender , y=age , group=gender)) +
  geom_boxplot()+ geom_jitter(width = .2,aes(color=gender))

#scatter plot

Scat <- ggplot(df, aes(x = height, y = foot_length)) +
  geom_point() +
  stat_smooth(se = FALSE) +
  labs(x = "Children Foot Lenght", y = "Children Height", title = "Foot Length and Height Scatter")
  
  
  





  


















