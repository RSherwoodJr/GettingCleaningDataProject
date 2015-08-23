##This repository contains code and data necessary for the Coursera "Getting and Cleaning Data" project

run_analysis.R is commented in plain text detailing what each step is doing

In summary, it:
*reads in the data (assuming the working directory is \UCI HAR Dataset)
*creates descriptive variable names
*combines the data
*labels the data
*subsets the data to only the averages and standard deviations (and columns identifying the observation)
*calculates averages by subject and activity
*outputs Averages.txt with the average data