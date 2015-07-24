#Setting Work DB

parent.folder <- paste(getwd(),"/UCI HAR Dataset",sep="")
setwd(parent.folder)

#Reading data

file.name.v <- list.files(pattern="*.txt")
activity_labels <- read.table(file.name.v[1])
features <- read.table(file.name.v[2])

sub.folders1 <- list.dirs(parent.folder, recursive=TRUE)[-1]
r.scripts <- file.path(sub.folders1)

setwd(r.scripts[1])
file.name.v <- list.files(pattern="*.txt")
subject_test <- read.table(file.name.v[1])
X_test <- read.table(file.name.v[2])
y_test <- read.table(file.name.v[3])

setwd(r.scripts[3])
file.name.v <- list.files(pattern="*.txt")
subject_train <- read.table(file.name.v[1])
X_train <- read.table(file.name.v[2])
y_train <- read.table(file.name.v[3])

full_data1 <- cbind(subject_test, y_test, X_test)
full_data2 <- cbind(subject_train, y_train, X_train)

full_data <- rbind(full_data1, full_data2)
colnames(full_data) <- c("Person_ID", "Activity", as.character(features[,2]))

mean_col_ind <- grep("mean", colnames(full_data))
std_col_ind <- grep("std", colnames(full_data))
mean_std_data <- full_data[,c(1,2,mean_col_ind, std_col_ind)]

mean_std_data$Activity[ which(mean_std_data$Activity == 1) ] <- "WALKING"
mean_std_data$Activity[ which(mean_std_data$Activity == 2) ] <- "WALKING_UPSTAIRS"
mean_std_data$Activity[ which(mean_std_data$Activity == 3) ] <- "WALKING_DOWNSTAIRS"
mean_std_data$Activity[ which(mean_std_data$Activity == 4) ] <- "SITTING"
mean_std_data$Activity[ which(mean_std_data$Activity == 5) ] <- "STANDING"
mean_std_data$Activity[ which(mean_std_data$Activity == 6) ] <- "LAYING"

tidy_data_set1 <- aggregate(mean_std_data, by=list(mean_std_data$Activity, mean_std_data$PERSON_ID), FUN=mean)

tidy_data_set2 <- aggregate(mean_std_data, by=list(mean_std_data$Activity), FUN=mean)
summary(mean_std_data~Activity+PERSON_ID, data=mean_std_data, FUN=mean)

ft <- ftable(xtabs(mean_std_data~mean_std_data$Activity+mean_std_data$PERSON_ID)) 
attributes(ft)$col.vars <- list(c("mean","sd")) 


