
#Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
#The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame containing the hospital in each state that has the ranking specified in num.

#set the directory to the source file location:

rankALL <- function(outcome, num) {
   # reading the data file
   data <- read.csv(file = "Datasets/outcome-of-care-measures.csv", colClasses = "character")
   # creating a vector which contains unique values of states
   unique_state  <- unique(data$State)
   # creating a data frame of unique state
   unique_state <- data.frame(unique_state)
   # creating a matrix with 54 rows and 2 col
   x <-matrix(,nrow = 54, ncol = 2)
   # giving colnames of our matrix
   colnames(x) <- c("state", "hospital name")  
  
   # validating the value of the outcome 
   if(outcome != "Heart Attack" & outcome != "Heart Failure" & outcome != "Pneumonia")
     {
      stop("INVALID OUTCOME")
     }

  # if the value of the outcome is Heart Attack 
  if(outcome == "Heart Attack")
    {
     # creating a for loop which runs for every value of state in the unique_state dataframe
     for (i in 1: 54)
      {
       state <- unique_state[i,1]
       # assigning the value of the state to the first column of matrix x
       x[i,1] <- as.matrix(unique_state[i,1])
       # subsetting data based on the value of state
       data_1 <- subset(data, data$State == state) 
       #creating a data frame which contains only two variables. First is mortality rate and second is hospital name
       data_output <- data.frame(data_1$Hospital.Name, data_1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
       # ordering a data frame based on numeric value of mortality and also based on hospital name
       output <- data_output[order(as.numeric(as.character(data_output$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),data_output$data_1.Hospital.Name),]
       output_result <- data.frame(output)
       # subsetting data frame by excluding those values which are equal to 'Not Available'
       output <- subset(output_result, output_result$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available")
       # storing the output in a dataframe named as final
       final <- data.frame(output)
       # if num parameter is  equal to worst
       if(num == "worst")
        {
         num = nrow(final)
       }
       # if num parameter is equal to best
       if(num == "best")
         {
          num = 1
       }
       # storing the particular value of hospital name based on num (rank) parameter
       e<-data.frame(final)[num,1]
       # storing the above value to the second column of matrix x
       x[i,2] <- as.matrix(data.frame(e))
       # resetting the value of num to worst
       if(num ==  nrow(final))
         {
          num <- "worst"
       }
       
       # resetting the value of num to best
       if (num ==  1)
         {
          num <- "best"
         }
      }
    
    #storing the result in the data frame
    result <- data.frame(x)
    # ordering the value in the data frame
    result[order(result$state),]
    
   }
   # if the value of the outcome is Heart Failure
   else if(outcome == "Heart Failure")
     {
     # creating a for loop which runs for every value of state in the unique_state dataframe
      for(i in 1: 54)
       {
        
        state <- unique_state[i,1]
        # assigning the value of the state to the first column of matrix x
        x[i,1] <- as.matrix(unique_state[i,1])
        # subsetting data based on the value of state
        data_1 <- subset(data, data$State == state) 
        #creating a data frame which contains only two variables. First is mortality rate and second is hospital name
        data_output <- data.frame(data_1$Hospital.Name, data_1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        # ordering a data frame based on numeric value of mortality and also based on hospital name
        output <- data_output[order(as.numeric(as.character(data_output$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),data_output$data_1.Hospital.Name),]
        output_result <- data.frame(output)
        # subsetting data frame by excluding those values which are equal to 'Not Available'
        output <- subset(output_result, output_result$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available")
        # storing the output in a dataframe named as final
        final <- data.frame(output)
        # if num parameter is  equal to worst
        if(num == "worst")
          {
           num = nrow(final)
          }
        # if num parameter is equal to best
        if(num == "best")
          {
           num = 1
          }
        # storing the particular value of hospital name based on num (rank) parameter
        e<-data.frame(final)[num,1]
        # storing the above value to the second column of matrix x
        x[i,2] <- as.matrix(data.frame(e))
        # resetting the value of num to worst
        if (num ==  nrow(final))
          {
           num <- "worst"
          }
        # resetting the value of num to best
        if (num ==  1)
          {
           num <- "best"
          }
      }
     
    #storing the result in the data frame
    result <- data.frame(x)
    # ordering the value in the data frame
    result[order(result$state),]
  }
  
  else
   {
    for(i in 1:54)
      
     {
      state <- unique_state[i,1]
      x[i,1] <- as.matrix(unique_state[i,1])
      # subsetting data based on the value of state
      data_1 <- subset(data, data$State == state) 
      #creating a data frame which contains only two variables. First is mortality rate and second is hospital name
      data_output <- data.frame(data_1$Hospital.Name, data_1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      # ordering a data frame based on numeric value of mortality and also based on hospital name
      output <- data_output[order(as.numeric(as.character(data_output$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),data_output$data_1.Hospital.Name),]
      output_result <- data.frame(output)
      # subsetting data frame by excluding those values which are equal to 'Not Available'
      output <- subset(output_result, output_result$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available")
      # storing the output in a dataframe named as final
      final <- data.frame(output)
      # if num parameter is  equal to worst
      if(num == "worst")
       {
        num = nrow(final)
      }
      # if num parameter is equal to best
      if(num == "best")
       {
        num = 1
       }
      # storing the particular value of hospital name based on num (rank) parameter
      e<-data.frame(final)[num,1]
      # storing the above value to the second column of matrix x
      x[i,2] <- as.matrix(data.frame(e))
      # resetting the value of num to worst
      if(num ==  nrow(final))
        {
         num <- "worst"
        }
      if(num ==  1)
        {
         num <- "best"
        }
      
      }
    
    #storing the result in the data frame
    result <- data.frame(x)
    # ordering the value in the data frame
    result[order(result$state),]  
  } 
    
}

# test code to check the accuracy of the model
rankALL("Heart Failure",10)

