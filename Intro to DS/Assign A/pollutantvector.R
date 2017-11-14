# Part 4
# 
# Write a function named 'pollutantvector' that returns a vector of those pollutants (sulfate or nitrate) 
# whose values are greater than 'p', across a specified list of monitors. 
# The function 'pollutantvector' takes four arguments: 'directory', 'pollutant', 'id' and 'p'. 
# Given a vector monitor ID numbers, 'pollutantvector' reads that monitors
# ' particulate matter data from the directory specified in the 'directory' argument 
# and returns the ones more than a certain value ('p') across all of the monitors, 
# ignoring any missing values coded as NA.

pollutantvector <- function(directory, pollutant, id = 1:332, p) {
  
    files = list.files(directory, full.names=TRUE)
    df = data.frame()
    result = data.frame()  # data, value, id
    
    for(i in id)
    {
      
      df = read.csv(files[i])
      # remove NA value from column "sulfate" & "nitrate"   
      df_wt_na = df[complete.cases(df[,2:3]),]  
     
      # find the valid records
      result_line = df_wt_na[df_wt_na[[pollutant]]>p,] 

      result = rbind(result, result_line)
    }
    
    # Only include input pollutant 
    if (pollutant == 'sulfate') {
      result = result[, !names(result)%in%'nitrate']
    }
    else {
      result = result[, !names(result)%in%'sulfate']     
    }
    # covert to vector
    result_vector = result[,2]
    return(result_vector)
  }
  

# Unit Test:
# source('~/DS/pollutantvector.R') 
# pollutantvector("specdata", "sulfate", 1:35, 0.5)