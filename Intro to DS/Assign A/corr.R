# Part 3
# 
# Write a function that takes a directory of data files and a threshold for 
# complete cases and calculates the correlation between sulfate and nitrate for monitor locations 
# where the number of completely observed cases (on all variables) is greater than the threshold.
# The function should return a vector of correlations for the monitors that meet the threshold requirement. 
# If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. 
# A prototype of this function follows:

corr <- function(directory, threshold = 0) {
  
  # get the complete table
  complete_tb <- complete(directory)
  nobs <- complete_tb$nobs
  
  # find the valid id that greater than threshold
  id_th <- complete_tb$id[nobs > threshold]
  
  # find all files
  files = list.files(directory, full.names=TRUE)
  df = data.frame()
  result = data.frame()
  
  # output corr with id
  for(i in id_th) {
    df = read.csv(files[i])
    corr = cor(df$sulfate, df$nitrate, use="complete.obs")
    result_line = data.frame(id = i, corr)
    result = rbind(result, result_line)
  }
  return(result)   
}

# Unit Test:
# source("corr.R") source("complete.R")
# cr <- corr("specdata", 150)