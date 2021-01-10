library(dplyr)

feature = read.table('UCI HAR Dataset/features.txt',col.names=c('n','functions'))
head(feature)
activities = read.table('UCI HAR Dataset/activity_labels.txt',col.names=c('no','label'))
head(activities)

subject_test  = read.table('UCI HAR Dataset/test/subject_test.txt',col.names=c('subject'))
head(subject_test)
x_test = read.table('UCI HAR Dataset/test/X_test.txt',col.names=feature[,'functions'])
head(x_test)
nrow(x_test)
ncol(x_test)

y_test = read.table('UCI HAR Dataset/test/y_test.txt',col.names=c('code'))
head(y_test)
nrow(y_test)
ncol(y_test)

subject_train  = read.table('UCI HAR Dataset/train/subject_train.txt',col.names=c('subject'))
head(subject_train)
x_train = read.table('UCI HAR Dataset/train/X_train.txt',col.names=feature[,'functions'])
head(x_train)
nrow(x_train)
ncol(x_train)

y_train = read.table('UCI HAR Dataset/train/y_train.txt',col.names=c('code'))
head(y_train)
nrow(y_train)
ncol(y_train)


# Merging the Data Frame
x_data = rbind(x_train,x_test)
y_data = rbind(y_train,y_test)
subject_data = rbind(subject_train,subject_test)
final_data = cbind(subject_data,x_data,y_data)

head(final_data)
nrow(final_data)
ncol(final_data)

name_df = names(final_data)
name_df

tidy_data = select(final_data,subject,code,contains('mean'),contains('std')) # Selecting only the columns with mean and std
tidy_data

tidy_data$code = activities[tidy_data$code,'label'] # Renaming the code
colnames(tidy_data)

# Providing proper name to the columns
names(tidy_data)[2] = "activity"
names(tidy_data)<-gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data)<-gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data)<-gsub("BodyBody", "Body", names(tidy_data))
names(tidy_data)<-gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data)<-gsub("^t", "Time", names(tidy_data))
names(tidy_data)<-gsub("^f", "Frequency", names(tidy_data))
names(tidy_data)<-gsub("tBody", "TimeBody", names(tidy_data))
names(tidy_data)<-gsub("-mean()", "Mean", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("-std()", "STD", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("-freq()", "Frequency", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("angle", "Angle", names(tidy_data))
names(tidy_data)<-gsub("gravity", "Gravity", names(tidy_data))



FinalData <- tidy_data %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)


str(FinalData)
head(FinalData)
