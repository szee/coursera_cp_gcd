## Merging training and test data sets to create one data set

train_set <- read.table("train/X_train.txt", header = FALSE)
train_labels <- read.table("train/y_train.txt")
train_subject <- read.table("train/subject_train.txt")

test_set <- read.table("test/X_test.txt", header = FALSE)
test_labels <- read.table("test/y_test.txt")
test_subject <- read.table("test/subject_test.txt")

data <- rbind(train_set, test_set)
labels <- rbind(train_labels, test_labels)
subject <- rbind(train_subject, test_subject)

names(labels) = "activities"
names(subject) = "subject"

## Getting only mean and standard deviation of each measurement

feature_names <- read.table("features.txt")
feature_names[, 2] <- tolower(feature_names[, 2])

mean_names <- feature_names[grepl("-mean()", feature_names[, 2]), ]
mean_names <- mean_names[!grepl("meanfreq", mean_names[, 2]), ]


std_names <- feature_names[grepl("std()",feature_names[, 2]), ]

mean_std <- rbind(mean_names, std_names)
names(mean_std) <- c("id", "name")

rownames(mean_std) = NULL

## Naming activities and labeling dataset

data <- data[,mean_std$id]
names(data) <- mean_std$names
data <- cbind(labels, subject, data)


activity_labels<-read.table("activity_labels.txt")
names(activity_labels)=c("activities","activity")

id<-1:nrow(data)
names(id)="id"
data<-cbind(id,data)
data<-merge(activity_labels,data)
data<-data[order(data$id), ]
row.names(data)=NULL
data$id=NULL
data$activities=NULL

##Output

f<-interaction(data$activity,data$subject)
s<-split(data,f)
out<-sapply(s,function(data) colMeans(data[,3:length(data)]))
write.table(out,"output_set.txt")
