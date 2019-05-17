#R code for Getting and cleaning the Data

# **Import the necessary packages**
library(plyr)
library(reshape2)

# 1. **Merges the training and the test sets to create one data set.**
x_train<-read.table("./getandcleandata/train/X_train.txt")
y_train<-read.table("./getandcleandata/train/y_train.txt",header = FALSE)
y_test<-read.table("./getandcleandata/test/y_test.txt",header = FALSE)
x_test<-read.table("./getandcleandata/test/X_test.txt",header = FALSE)
x_full<-rbind(x_train,x_test)
y_full<-rbind(y_train,y_test)
df<-cbind(x_full,y_full)
dim(df)
#Assigning the feature names as the column names
features<-read.delim("./getandcleandata/features.txt",header = FALSE,sep = "\t")
features<-append(features,"Labels")
str(features)
names(df)<-features

# 2.**Extracts only the measurements on the mean and standard deviation for each measurement.**
df1<-grepl("mean+|std+",names(df),ignore.case = TRUE,perl = TRUE)
df_names<-features[df1]
df_sub<-df[df1]

# 3.**Uses descriptive activity names to name the activities in the data set**
sub1<-read.delim("./getandcleandata/train/subject_train.txt",header = FALSE,sep = "\t")
sub2<-read.delim("./getandcleandata/test/subject_test.txt",header = FALSE,sep = "\t")
sub3<-rbind(sub1,sub2)
av<-read.table("./getandcleandata/activity_labels.txt",header = FALSE)
av<- factor(av$V2,levels = av$V1,labels = av$V2)
for (i in 1:nrow(df_sub)) {
     df_sub$V2[i]<-levels(av)[df_sub$V1[i]]
 }
df_sub$V1<-NULL

# 4.**Appropriately labels the data set with descriptive variable names.**
colnames(df_sub)[87]<-"Activity"
df_sub<-cbind(df_sub,sub3)
colnames(df_sub)[88]<-"Subject"
colnames(df_sub)<-sub(".+? ", "",colnames(df_sub))
colnames(df_sub)<-gsub("^t","Time",colnames(df_sub))
colnames(df_sub)<-gsub("^f","Frequency",colnames(df_sub))

# 5.**Tidy data with the average of each variable grouped by activity and each subject.**
df_melt<-melt(df_sub,id=c("Activity","Subject"))
df_cast<-dcast(df_melt,Activity+Subject~variable,mean)
write.csv(df_cast,file = "FinalTidyData.csv")
