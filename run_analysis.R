## Course 3 Project

## Step 1: Download the data files
## Get required packages and set the Directory
install.packages("data.table")
install.packages("reshape2")
library(data.table)
library(reshape2)
setwd("C:/Giri/RProgramming/Courseera/Course3")
path <- getwd()

## Download the required data for analysis
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {
  dir.create(path)
}
download.file(url, file.path(path, f))

## Unzip the file
executable <- file.path("C:", "Program Files (x86)", "7-Zip", "7z.exe")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path, 
                                                                         f), "\""))
system(cmd)

## review files unzipped
pathIn <- file.path(path, "UCIHARDataset")
list.files(pathIn, recursive = TRUE)

## Step 2: Load the Files in R for analysis
dtSubTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
names(dtSubjectTrain)
dtSubTest <- fread(file.path(pathIn, "test", "subject_test.txt"))
names(dtSubjectTest)
dtActTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
names(dtActTrain)
dtActTest <- fread(file.path(pathIn, "test", "Y_test.txt"))

fileToDataTable <- function(f) {
  df <- read.table(f)
  dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtTest <- fileToDataTable(file.path(pathIn, "test", "X_test.txt"))

## Step 3: Merging the data
dtSub <- rbind(dtSubTrain, dtSubTest)
setnames(dtSub, "V1", "subject")
names(dtSub)
dtAct <- rbind(dtActTrain, dtActTest)
setnames(dtAct, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)
names(dt)
dtSubAct <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubAct, dt)
setkey(dt, subject, activityNum)
names(dt)

## Step 4: Extract Mean and Standard Deviation
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]

## Step 5: Use descriptive names
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

## Step 6: Label with descriptive activity names
dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)
setkey(dt, subject, activityNum, activityName)

dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))
head(dt)

dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", 
            all.x = TRUE)
head(dt)

dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)
head(dt)

grepthis <- function(regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

head(dt)

r1 <- nrow(dt[, .N, by = c("feature")])
r2 <- nrow(dt[, .N, by = c("featDomain", "featAcceleration", "featInstrument", 
                           "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

## Step 7: Create a Tidy dataset
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, 
       featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]
head(dtTidy)