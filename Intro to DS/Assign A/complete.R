# Part 2
# 
# Write a function that reads a directory full of files and reports the number of 
# completely observed cases in each data file. 
# The function should return a data frame where the first column is the name of 
# the file and the second column is the number of complete cases.
# A prototype of this function follows:

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases

complete <- function(directory, id = 1:332) {
  files = list.files(directory, full.names=TRUE)
  df = data.frame()
  result = data.frame()

  for(i in id)
  {
    
    df = read.csv(files[i])
    # remove NA value from column "sulfate" & "nitrate"   
    # output nobs:
    # solution 1:
    df_wt_na = df[complete.cases(df[,2:3]),]  
    nobs = nrow(df_wt_na)  
    # solution 2:
    # nobs = sum(complete.cases(df))  
    result_line = data.frame(id = i, nobs)
    result = rbind(result, result_line)
  }
  return(result)
}

  
# Unit Test:
# source('~/DS/complete.R", echo=TRUE) 
# complete("specdata", 1) 
# complete("specdata", 30:25)