#Assumes the folder was unzipped into the WD
setwd("./UCI HAR Dataset")
library(data.table)
#Load data
xtrain <- read.table("train/X_train.txt")
xtest <- read.table("test/X_test.txt")
features <- read.table("features.txt")
act <- as.character(read.table("activity_labels.txt")[,2])
strain <- read.table("train/subject_train.txt")
stest <- read.table("test/subject_test.txt")
ytrain <- read.table("train/Y_train.txt")
ytest <- read.table("test/Y_test.txt")

#(1) Merge train and test data, adding the variable names given in features.txt
dat <- rbind(xtrain, xtest)
colnames(dat) <- features[,2]

#(2) Extract only columns that have mean() or std() in the column name
n_mstd <- grep(".*mean|.*std", as.character(features[,2]),
                 ignore.case = FALSE)
dat_mstd <- dat[, n_mstd]

#(3) Make descriptive names for activities
actname <- gsub("_"," ", act)
#Capitalization function given by the help page, modified
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
}
actdesc <- unname(sapply(actname, .simpleCap))

#(4) Combining data sets and labeling variables
sub <- rbind(strain, stest)
colnames(sub) <- "Subject"
Subject <- as.factor(sub[,1])
suby <- rbind(ytrain, ytest)
colnames(suby) <- "Activity"

fin <- cbind(dat, Subject)

#(5) Independent data set with the average for each variable by subject
avg <- as.data.table(fin)
setkey(avg, Subject)
avg <- avg[ , lapply(.SD, mean), by=Subject]

write.table(avg, file = "tidy.txt", row.name = FALSE)