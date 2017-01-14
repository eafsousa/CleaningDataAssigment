# CleaningDataAssigment


============Files description==================

The file "run_analysis.R" contains the code that processes the input datafiles and builds the Final dataset with all the variables for all measurements of all participants.

The file "CompleteDataSet.txt" is a CSV file with the resulting dataset first row of the file contains the labels of each column

The file "CompleteDataSet_summary.txt" is a csv file that contains the Averages (mean and std) for every variable of the Complete dataset grouped by Activity.

the file "CodeBook.txt" contains the description of every variable in the dataSet.

============Transformations==================
The training and test dataset were combined.

For every measure, the features were added to the final dataset, as well as the hand-defined label and group (test/train).

For every measurement the mean and standard deviation were calculated and added to the final dataset.

A second dataset containing the averages was calculated by determining averages (mean and STD) for activity type.

============Instructions==================

To regenerate the dataset please run the file "run_analysis.R". This assumes that the input datafolder "UCI HAR Dataset" is in the R working directory. this file will generate the "CompleteDataSet.txt" and the "CompleteDataSet_summary.txt".