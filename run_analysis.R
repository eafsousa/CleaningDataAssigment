library(dplyr)

#setwd("C:/Work/Repositories/Coursera/CleaningDataAssigment")





#Define a structure for containing paths for the files of test
FilesTest <- data()
FilesTest$subjects <- "./UCI HAR Dataset/test/subject_test.txt"
FilesTest$X_test <- "./UCI HAR Dataset/test/X_test.txt"
FilesTest$Y_test <- "./UCI HAR Dataset/test/y_test.txt"
FilesTest$body_acc_x <- "./UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt"
FilesTest$body_acc_y <- "./UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt"
FilesTest$body_acc_z <- "./UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt"
FilesTest$gyro_x<-"./UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt"
FilesTest$gyro_y<-"./UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt"
FilesTest$gyro_z<-"./UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt"
FilesTest$totalAcc_x<-"./UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt"
FilesTest$totalAcc_y<-"./UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt"
FilesTest$totalAcc_z<-"./UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt"





#Define a structure for containing paths for the files of train
FilesTrain <- data()
FilesTrain$subjects <- "./UCI HAR Dataset/train/subject_train.txt"
FilesTrain$X_test <- "./UCI HAR Dataset/train/X_train.txt"
FilesTrain$Y_test <- "./UCI HAR Dataset/train/y_train.txt"
FilesTrain$body_acc_x <- "./UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt"
FilesTrain$body_acc_y <- "./UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt"
FilesTrain$body_acc_z <- "./UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt"
FilesTrain$gyro_x<-"./UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt"
FilesTrain$gyro_y<-"./UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt"
FilesTrain$gyro_z<-"./UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt"
FilesTrain$totalAcc_x<-"./UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt"
FilesTrain$totalAcc_y<-"./UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt"
FilesTrain$totalAcc_z<-"./UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt"




#Create a function that loads all the data and builds a data from for a condition (test/train)

processTrial <- function (fileData,type)
{

    #Load the participants IDs file and creates a vector
    P_ID <- read.csv(fileData$subjects,header = FALSE)
    
    colnames(P_ID) <- "ParticipantID"
    
    
    
    
    ############ Features ############
    
    #Load the features values
    features <- read.csv(fileData$X_test,header = FALSE, sep = "")
  
    # Prepare the name of the features names
    featureLines <- readLines("./UCI HAR Dataset/features.txt")
    
    #Remove the number on the beggining of each line
    lastElement<- function(x){x[2]}
    aux <- sapply(strsplit(featureLines," "),lastElement)
    
    #Clear opening and closing parenthesis
    cl1 <- gsub("\\(\\)|\\)$","",aux)
    
    #Clear parenthesis on last character
    featuresnames <- gsub("[)(,-]",".",cl1)
    
    #Attribute the feature names
    colnames(features)<-featuresnames
    
    #RemoveDuplicated columns
    featuresFilt <- features[, !duplicated(featuresnames, fromLast = TRUE)] 

    #Join with the participant ID
    aux<-cbind(P_ID,featuresFilt)
    
    
    
    
    ############ Activities ############
    
    #load activities info
    Activities <- read.csv(fileData$Y_test,header = FALSE)
    
    #Reads activity labels
    ActivityLabels <- read.csv("./UCI HAR Dataset/activity_labels.txt",header = FALSE, sep = "")
    
    #Define the activity column
    aux$Activity <- sapply(Activities, function(x) as.character(ActivityLabels$V2[x]))
    
    
    
    
    ############ Measurements ############
    
    #Loads acceleration measurements for x coordinate and calculates means and stds
    body_acc_x <- read.csv(fileData$body_acc_x,header = FALSE, sep = "")
    aux$Mean.Body.Acc.x <- apply(body_acc_x,1,mean)
    aux$STD.Body.Acc.x <- apply(body_acc_x,1,sd)
    
    body_acc_y <- read.csv(fileData$body_acc_y,header = FALSE, sep = "")
    aux$Mean.Body.Acc.y <- apply(body_acc_y,1,mean)
    aux$STD.Body.Acc.y <- apply(body_acc_y,1,sd)
    
    body_acc_z <- read.csv(fileData$body_acc_z,header = FALSE, sep = "")
    aux$Mean.Body.Acc.z <- apply(body_acc_z,1,mean)
    aux$STD.Body.Acc.z <- apply(body_acc_z,1,sd)
    
    
    
    #Loads gyroscope measurements for x coordinate and calculates means and stds
    body_gyro_x <- read.csv(fileData$gyro_x,header = FALSE, sep = "")
    aux$Mean.Body.Gyro.x <- apply(body_gyro_x,1,mean)
    aux$STD.Body.Gyro.x <- apply(body_gyro_x,1,sd)
    
    body_gyro_y <- read.csv(fileData$gyro_y,header = FALSE, sep = "")
    aux$Mean.Body.Gyro.y <- apply(body_gyro_y,1,mean)
    aux$STD.Body.Gyro.y <- apply(body_gyro_y,1,sd)
    
    body_gyro_z <- read.csv(fileData$gyro_z,header = FALSE, sep = "")
    aux$Mean.Body.Gyro.z <- apply(body_gyro_z,1,mean)
    aux$STD.Body.Gyro.z <- apply(body_gyro_z,1,sd)
   
    
     
    #Loads total acceleration measurements for x coordinate and calculates means and stds
    total_acc_x <- read.csv(fileData$totalAcc_x,header = FALSE, sep = "")
    aux$Mean.Total.Acc.x <- apply(total_acc_x,1,mean)
    aux$STD.Total.Acc.x <- apply(total_acc_x,1,sd)
    
    total_acc_y <- read.csv(fileData$totalAcc_y,header = FALSE, sep = "")
    aux$Mean.Total.Acc.y <- apply(total_acc_y,1,mean)
    aux$STD.Total.Acc.y <- apply(total_acc_y,1,sd)
  
    total_acc_z <- read.csv(fileData$totalAcc_z,header = FALSE, sep = "")
    aux$Mean.Total.Acc.z <- apply(total_acc_z,1,mean)
    aux$STD.Total.Acc.z <- apply(total_acc_z,1,sd)
  
    
    ## Defines the type (test/train)
    aux$Type <- as.character(type)
  
    aux
}

#Load test data and builds a dataframe
df1 <- processTrial(FilesTest,"Test")

#Load trial data and builds a dataframe
df2 <- processTrial(FilesTrain,"Train")

#Joins the dataframes
final.df<-rbind(df1,df2)

#Write to file
write.table(final.df,file = "CompleteDataSet.txt", row.name=FALSE,sep = ",")

############ Summary tables ############

#Discard the "type" and "ParticipantID" columns
aux.df <-final.df[ , !names(final.df) %in%  c("Type","ParticipantID")]

#Group and summarizes by Activity
summ_tb<- aux.df %>% group_by(Activity) %>% summarise_each(funs(mean))

#Write the table
write.table(summ_tb,file = "CompleteDataSet_summary.txt", row.name=FALSE,sep = ",")
