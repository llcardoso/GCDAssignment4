#From the analysis of the data on the "Inertial Signals" folders, for both the training and test datasets, feature vectors were made and compiled on files called "X_train" and "X_test".
#As the authors provide the required column names for these files through the "features.txt" file, I am assuming these are the datasets to use on this exercise.

#The first step is to Store the test files in R objects. We have to store the data on the "X" files.

#Storing the train files in R objects

X_train_ <- read.delim2 ("UCI_HAR_Dataset/train/X_train.txt", sep= "",header= FALSE,stringsAsFactors = TRUE)

#Storing the test files in R objects

X_test_ <- read.delim2 ("UCI_HAR_Dataset/test/X_test.txt", sep= "",header= FALSE)

#Since each Row represents a feature vector, data should be merged by adding rows. Hence, to merge training and test data, I use rbind():

X_all_ <-rbind(X_train_,X_test_)

#To name the measurements, I am using the authors' expressions on the features.txt file.
#I am creating a vector with the names, which I can then add as column names.

feature_file <- read.delim2("UCI_HAR_Dataset/features.txt",header = FALSE)
feature_list <- as.vector(feature_file)
X_all_cn <- X_all_
feature_vector <- feature_file [,1]
names(X_all_cn) = feature_vector

#We can search the column names for the expressions "mean" and "std" to generate logical vectors, which will help us subset the data to obtain means and stds only.

meansearch <- grepl("mean", feature_vector)
stdsearch <- grepl("std", feature_vector)

feature_vector_mean  <- which(meansearch == TRUE)
feature_vector_std  <- which(stdsearch == TRUE)

#Now we have the vectors corresponding to the columns in which the means and the stds for each measurement are stated,
#for the concatenated dataset of training and test data.
#What is left for point 2 is to subset the data according to these vectors and
#joining the means and stds in a single dataset:

mean_X_all_ <- subset(X_all_cn[,feature_vector_mean])
std_X_all_ <- subset(X_all_cn[,feature_vector_std])
mean_std_X_all_ <- cbind(mean_X_all_,std_X_all_)

#As I am using the authors' feature file as column name giver, I am assuming these as appropriate column names.
#To complete points 3 and 4, now we have to label the data by activity and subject. To keep the data tidy, the best option is
#to use the labels given by the authors, convert them into an appropriate label (like the name of the activity or subject number) and add 
#this information as additional columns in the data set.

#For subject labeling, decoding the characters into numeric keys:
subject_train_labels <- readLines ("UCI_HAR_Dataset/train/subject_train.txt",encoding = "UTF-8")
subject_test_labels <- readLines ("UCI_HAR_Dataset/test/subject_test.txt",encoding = "UTF-8")
subject_all_labels <- append(subject_train_labels,subject_test_labels)

#Thankfully, the authors made it so that the subject ID has a unique key across test and training datasets.
#As such, the labels could be safely merged directly. As subject names are confidential, I will keep the numeric key as their identifier.


#Now for activity labeling:

Y_train_labels <- readLines ("UCI_HAR_Dataset/train/y_train.txt",encoding = "UTF-8")
Y_test_labels <- readLines ("UCI_HAR_Dataset/test/y_test.txt",encoding = "UTF-8")
Y_all_labels <- append(Y_train_labels,Y_test_labels)

#To be clearer, here we will substitute the numeric labels by the corresponding activity.

Y_all_labels[Y_all_labels == "1"] <- "WALKING"
Y_all_labels[Y_all_labels == "2"] <- "WALKING_UPSTAIRS"
Y_all_labels[Y_all_labels == "3"] <- "WALKING_DOWNSTAIRS"
Y_all_labels[Y_all_labels == "4"] <- "SITTING"
Y_all_labels[Y_all_labels == "5"] <- "STANDING"
Y_all_labels[Y_all_labels == "6"] <- "LAYING"

#So, now we have identifiers for both the individuals and the activity. All we need to do now is to
#merge these IDs with the data from the X files.

df_subject_all_labels <- as.data.frame(subject_all_labels)
df_Y_all_labels <- as.data.frame(Y_all_labels)

mean_std_dataset_final <- cbind(df_subject_all_labels,df_Y_all_labels,mean_std_X_all_)
colnames(mean_std_dataset_final)[1] <- "Subject_ID"
colnames(mean_std_dataset_final)[2] <- "Activity"

#With this I think we conclude steps 1 to 4 of the exercise.
#Now for step 5, we need to average the means by subject and activity.
#For this purpose, I am going to use lapply on the columns carrying "mean" or "std" (excluding the label columns I added) to turn them into numeric,
#and then I am doing a new dataframe consisting of the averages of each Activity and Subject combination.

#Loading libraries
library(data.table)

#Setting data as data table for manipulation
mean_std_DT <- mean_std_dataset_final
setDT(mean_std_DT)

#Selecting data mean or std data columns, calculating the averages on a new data frame and renaming the columns to match column names in exercise 4
numeric_cols <- grep("mean|std", names(mean_std_DT), value = TRUE)
mean_std_DT[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
averages_mean_std <- mean_std_DT[,lapply(.SD, mean, na.rm = TRUE), by = .(subject_all_labels,Y_all_labels), .SDcols = numeric_cols]
colnames(averages_mean_std) <- colnames (mean_std_DT)

#After running this code, we have a dataframe named "averages_mean_std" with the average of each mean and std columns.
#The only thing left to do is to export it as a text file for the submission.

write.table(averages_mean_std, file = "Exercise4_5.txt",row.name=FALSE) 

