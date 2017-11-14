# Part 5
#
# Write a function that prompts a user to choose an operation between six available operations:
#   1) Add, 2)Subtract, 3) Multiply, 4) Divide, 5) Factors and 6) Prime number.
# The first four operations will ask user to provide two numbers and add, subtract, multiply
# and divide them accordingly.
# The fifth operation calculates the factors of a number and sixth operation checks if a number is prime.

calculator <- function() {
  ## 1. Hind for input
  cat("******Simple R Calculator - Select operation: ******\n")
  cat("1.Add\n")
  cat("2.Subtract\n")
  cat("3.Multiply\n")
  cat("4.Divide\n")
  cat("5.Factors\n")
  cat("6.Prime\n")
  
  ## 2. check validity of input:choice
  valid_input = c(1:6)
  repeat {
    choice = readline("Enter choice [1/2/3/4/5/6] ")
    
    if (!(choice %in%  valid_input)) {
      cat("Are you kidding? Should be 1/2/3/4/5/6. Try again:\n")
    }
    else {
      break
    }
  }
  
  ## 3. calculate 1:4
  if (choice %in% c(1:4)) {
    ## 3.1. check validity of input: first_num
    repeat {
      first_num = readline("Enter first number: ")
      
      if (is.na(as.numeric(first_num))) {
        cat("Are you kidding? Should be number. Try again:\n")
      }
      else {
        break
      }
    }
    
    ## 3.2. check validity of input: second_num
    repeat {
      second_num = readline("Enter second number: ")
      
      if (is.na(as.numeric(second_num))) {
        cat("Are you kidding? Should be number. Try again:\n")
      }
      else {
        break
      }
    }
    
    # 3.3. convert to number
    first_num = as.numeric(first_num)
    second_num = as.numeric(second_num)
    
    # 3.4. output
    if (choice == 1) {
      result = paste(first_num, "+", second_num, "=", first_num + second_num)
    }
    else if (choice == 2) {
      result = paste(first_num, "-", second_num, "=", first_num - second_num)
    }
    else if (choice == 3) {
      result = paste(first_num, "*", second_num, "=", first_num * second_num)
    }
    else if (choice == 4) {
      if (second_num == 0) {
        result = "Invalid value, Divident should not Zero!"
      }
      else {
        result = paste(first_num, "/", second_num, "=", first_num / second_num)
      }
    }
    # suppressWarnings
    # suppressWarnings(result)
    return(result)
  }
  
  ## 4. calculate 5:6
  else if (choice %in% c(5:6)) {
    ## 4.1. check validity of input: num
    repeat {
      num = readline("Enter the number: ")
      
      if (is.na(as.numeric(num))) {
        cat("Are you kidding? Should be number. Try again:\n")
      }
      else {
        num = as.numeric(num)
        break
      }
    }
    
    ## 4.2. output
    if (choice == 5) {
      print(paste("The factors of", num, "are:"))
      for (i in 1:num) {
        if ((num %% i) == 0) {
          print(i)
        }
      }
    }
    
    else if (choice == 6) {
      result = ifelse(sum(num %% (1:num) == 0) > 2, "NOT Prime", "Prime")
      print(paste("The number of", num, "is:", result))
    }
  }
}


# Unit Test:
# source("calculator.R")
#
# [1] "******Simple R Calculator - Select operation: ******" [1] "1.Add"
# [1] "2.Subtract"
# [1] "3.Multiply"
# [1] "4.Divide"
# [1] "5.Factors"
# [1] "6.Prime"
#
# Enter choice [1/2/3/4/5/6]: 4
# Enter first number: 20
# Enter second number: 4
# [1] "20 / 4 = 5"
#
# Enter choice [1/2/3/4/5/6]: 5
# Enter the number: 120
# [1] "The factors of 120 are:"
# [1] 1
# [1] 2
# [1] 3
# [1] 4
# [1] 5
# [1] 6
# [1] 8
# [1] 10
# [1] 12
# [1] 15
# [1] 20
# [1] 24
# [1] 30
# [1] 40
# [1] 60
# [1] 120
