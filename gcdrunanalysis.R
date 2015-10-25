print("Please enter the directory path where the project files are saved in the function - createtidydata.")

createtidydata <- function(d = "working directory") {

setwd(d)
  install.packages("data.table")
  library(data.table)
  
## Step 1
read.table("./X_test.txt") -> testdata  ## 2947 obs. of 561 variables
read.table("./X_train.txt") -> traindata  ## 7352 obs. of 561 variables
read.table("./y_test.txt") -> testlabel  ## 2947 obs. of 1 variable
read.table("./y_train.txt") -> trainlabel  ## 7352 obs. of 1 variable
read.table("./subject_test.txt") -> subtestlabel  ## 2947 obs. of 1 variable
read.table("./subject_train.txt") -> subtrainlabel  ## 7352 obs. of 1 variable

temptest <- cbind(subtestlabel, testlabel)  ## column binds the test subject with activity level
testdata <- cbind(temptest, testdata)  ## column binds both to the test results, 2947 obs. of 563 variables
temptrain <- cbind(subtrainlabel, trainlabel)  ## column binds the test subject with activity level
traindata <- cbind(temptrain, traindata)  ## column binds both to the test results, 7352 obs. of 563 variables

## ensure data.table package is loaded
tdata <- data.table(testdata)  ## convert to data table
trdata <- data.table(traindata)  ## convert to data table
mdata <- rbind(tdata, trdata) ## merge two data sets to one for 10299 obs. of 563 variables

## Step 4 (in place of Step 2) because it made it simpler to subset in Step 2
read.table("./UCI HAR Dataset/UCI HAR Dataset/features.txt") -> colvar  ## list of 561 features (variables)
as.character(colvar[,2]) -> coltemp  ## extract 2nd col of colvar into char vector to replace var. name of measurements in mdata (merged data sets)
col <- c("Subject", "Activity", coltemp)  ## adds 2 labels more to char vector of column names 
mdata <- setnames(mdata, old = c(1:563), new = col)  ## renames columns of merged dataset with vector of column names

## Step 2
mtdata2 <- grep("mean()|std()", col) ## select indices of columns to keep
mdatatemp <- subset(mdata, select = c(1,2,mtdata2)) ## selected only indexed columns, 10299 obs. of 81 variables

## had to finetune list of variables to exclude instances of "meanFreq" from previous subsetted data set
mdata1 <- colnames(mdatatemp)  ## assign column names to character vector and then extract indices with meanFreq
mdata1sub <- grep("meanFreq", mdata1) ## 13 instances of "meanFreq"
mdata2 <- subset(mdatatemp, select = -c(mdata1sub)) ## selected out 13 instances of "meanFreq"

## Step 3  use descriptive names for the activities
for (i in 1:nrow(mdata2)) {
  if (mdata2$Activity[i] == "1") {
    mdata2$Activity[i] = "Walking"
  }
  else if (mdata2$Activity[i] == "2") {
    mdata2$Activity[i] = "Walking_Up"
  }
  else if (mdata2$Activity[i] == "3") {
    mdata2$Activity[i] = "Walking_Down"
  }
  else if (mdata2$Activity[i] == "4") {
    mdata2$Activity[i] = "Sitting"
  }
  else if (mdata2$Activity[i] == "5") {
    mdata2$Activity[i] = "Standing"
  }
  else if (mdata2$Activity[i] == "6") {
    mdata2$Activity[i] = "Laying"
  }
}
oldnames <- as.character(colnames(mdata2))

gsub("BodyAcc", "BA", oldnames) -> oldnames
gsub("GravityAcc", "GA", oldnames) -> oldnames
gsub("BAJerk", "BAJ", oldnames) -> oldnames
gsub("BodyGyro", "BG", oldnames) -> oldnames
gsub("BGJerk", "BGJ", oldnames) -> oldnames
gsub("BAMag", "BAM", oldnames) -> oldnames
gsub("GAMag", "GAM", oldnames) -> oldnames
gsub("BAJMag", "BAJM", oldnames) -> oldnames
gsub("BGMag", "BGM", oldnames) -> oldnames
gsub("BGJMag", "BGJM", oldnames) -> oldnames
gsub("BodyBAJM", "BAJM", oldnames) -> oldnames
gsub("BodyBGM", "BGM", oldnames) -> oldnames
gsub("BodyBGJM", "BGJM", oldnames) -> oldnames

write.table(oldnames, "./oldnames.txt")
read.table("./oldnames2.txt") -> tempnames
as.character(tempnames[,1]) -> newnames
mdata2 <- setnames(mdata2, old = c(1:68), new = newnames)

## mdata2 has 10299 observations of 68 variables
write.table(mdata2, "./tidydata2.txt") ##in case I lose environment 
print("The file tidydata2.txt shows the results through step 4 and is saved in your working directory.")

## Step 5 Create Independent Tidy Data Set from resulting data set in previous steps
install.packages("dplyr")
library(dplyr)
read.table("./tidydata2.txt") -> newtidy
## alternate layout with Activity before Subject
group_by(newtidy, Activity, Subject) -> x  ##used to group 1st by subject, then by activity
summarize(x, tBA_mean_X=mean(tBA_mean_X), tBA_mean_Y=mean(tBA_mean_Y), tBA_mean_Z=mean(tBA_mean_Z), tBA_std_X=mean(tBA_std_X), tBA_std_Y=mean(tBA_std_Y), tBA_std_Z=mean(tBA_std_Z), tGA_mean_X=mean(tGA_mean_X), tGA_mean_Y=mean(tGA_mean_Y), tGA_mean_Z=mean(tGA_mean_Z), tGA_std_X=mean(tGA_std_X), tGA_std_Y=mean(tGA_std_Y), tGA_std_Z=mean(tGA_std_Z), tBAJ_mean_X=mean(tBAJ_mean_X), tBAJ_mean_Y=mean(tBAJ_mean_Y), tBAJ_mean_Z=mean(tBAJ_mean_Z), tBAJ_std_X=mean(tBAJ_std_X), tBAJ_std_Y=mean(tBAJ_std_Y), tBAJ_std_Z=mean(tBAJ_std_Z), tBG_mean_X=mean(tBG_mean_X), tBG_mean_Y=mean(tBG_mean_Y), tBG_mean_Z=mean(tBG_mean_Z), tBG_std_X=mean(tBG_std_X), tBG_std_Y=mean(tBG_std_Y), tBG_std_Z=mean(tBG_std_Z), tBGJ_mean_X=mean(tBGJ_mean_X), tBGJ_mean_Y=mean(tBGJ_mean_Y), tBGJ_mean_Z=mean(tBGJ_mean_Z), tBGJ_std_X=mean(tBGJ_std_X), tBGJ_std_Y=mean(tBGJ_std_Y), tBGJ_std_Z=mean(tBGJ_std_Z), tBAM_mean=mean(tBAM_mean), tBAM_std=mean(tBAM_std), tGAM_mean=mean(tGAM_mean), tGAM_std=mean(tGAM_std), tBAJM_mean=mean(tBAJM_mean), tBAJM_std=mean(tBAJM_std), tBGM_mean=mean(tBGM_mean), tBGM_std=mean(tBGM_std), tBGJM_mean=mean(tBGJM_mean), tBGJM_std=mean(tBGJM_std), fBA_mean_X=mean(fBA_mean_X), fBA_mean_Y=mean(fBA_mean_Y), fBA_mean_Z=mean(fBA_mean_Z), fBA_std_X=mean(fBA_std_X), fBA_std_Y=mean(fBA_std_Y), fBA_std_Z=mean(fBA_std_Z), fBAJ_mean_X=mean(fBAJ_mean_X), fBAJ_mean_Y=mean(fBAJ_mean_Y), fBAJ_mean_Z=mean(fBAJ_mean_Z), fBAJ_std_X=mean(fBAJ_std_X), fBAJ_std_Y=mean(fBAJ_std_Y), fBAJ_std_Z=mean(fBAJ_std_Z), fBG_mean_X=mean(fBG_mean_X), fBG_mean_Y=mean(fBG_mean_Y), fBG_mean_Z=mean(fBG_mean_Z), fBG_std_X=mean(fBG_std_X), fBG_std_Y=mean(fBG_std_Y), fBG_std_Z=mean(fBG_std_Z), fBAM_mean=mean(fBAM_mean), fBAM_std=mean(fBAM_std), fBAJM_mean=mean(fBAJM_mean), fBAJM_std=mean(fBAJM_std), fBGM_mean=mean(fBGM_mean), fBGM_std=mean(fBGM_std), fBGJM_mean=mean(fBGJM_mean), fBGJM_std=mean(fBGJM_std)) -> w
write.table(w, "./tidydatafinal.txt", row.name=FALSE)
print("The file tidydatafinal.txt, result of Step 5, should be in your working directory.")
}