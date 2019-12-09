# Creating a function called rankhospital that takes three arguments: the 2-character abbreviated name of a state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the ranking specified by the num argument.

#set the directory to the source file location:

rankHospital <- function(state, outcome, num) {
  
  
   # creating a csv data file 
   data <- read.csv(file = "Datasets/outcome-of-care-measures.csv", colClasses = "character")
  # subsetting a data for a particular state
   data_1 <- subset(data, data$State == state)
  # if the subsetted the data_1 has zero rows than the state is invalid as all the states have a more than one row
   if(nrow(data_1) <=  0)
     {
      stop("INVALID STATE")
     }
  # validating the outcome parameter   
   if(outcome != "Heart Attack" & outcome != "Heart Failure" & outcome != "Pneumonia")
     {
       stop("INVALID OUTCOME")
     }
   # if the outcome is heart attack 
   if(outcome == "Heart Attack")
     {
      # creating a data frame with hospital name and death mortality rate column
      data_output <- data.frame(data_1$Hospital.Name, data_1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
      # ordering the dataframe based on death mortality rate and storing in the output vector
      output <- data_output[order(as.numeric(as.character(data_output$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),data_output$data_1.Hospital.Name),]
      # creating a dataframe of output vector and storing in output_Result
      output_result <- data.frame(output)
      # subsetting values which are not equal to 'Not Available'
      output <- subset(output_result, output_result$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available")
      # storing the final output in the data frame
      final <- data.frame(output)
      # if num parameter is worst than value of num is equal to nrow
      if (num == "worst")
        {
         num = nrow(final)
        }
      # if num parameter is best than the value of num is equal to 1 
      if (num == "best")
        {
         num = 1
        }
      # storing a particular value based on rank (num)  
      x<-data.frame(final)[num,1]
      data.frame(x)
     }
  
   #if the outcome is heart failure
   else if(outcome == "Heart Failure")
     {
      # creating a data frame with hospital name and death mortality rate column
      data_output <- data.frame(data_1$Hospital.Name, data_1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
      # ordering the dataframe based on death mortality rate and storing in the output vector
      output <- data_output[order(as.numeric(as.character(data_output$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),data_output$data_1.Hospital.Name),]
      # creating a dataframe of output vector and storing in output_Result
      output_result <- data.frame(output)
      # subsetting values which are not equal to 'Not Available'
      output <- subset(output_result, output_result$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != "Not Available")
      # storing the final output in the data frame
      final <- data.frame(output)
      # if num parameter is worst than value of num is equal to nrow
      if(num == "worst")
        {
        num = nrow(final)
        }
    
      # if num parameter is best than the value of num is equal to 1 
      if(num == "best")
        {
         num = 1
        }
      # storing a particular value based on rank (num)  
    
      x<-data.frame(final)[num,1]
      data.frame(x)
     } 
  
   # if the outcome value is Pneumonia
   else
     {
      # creating a data frame with hospital name and death mortality rate column
      data_output <- data.frame(data_1$Hospital.Name, data_1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      # ordering the dataframe based on death mortality rate and storing in the output vector
      output <- data_output[order(as.numeric(as.character(data_output$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),data_output$data_1.Hospital.Name),]
      # creating a dataframe of output vector and storing in output_Result
      output_result <- data.frame(output)
      # subsetting values which are not equal to 'Not Available'
      output <- subset(output_result, output_result$data_1.Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != "Not Available")
      # storing the final output in the data frame
      final <- data.frame(output)
      # if num parameter is worst than value of num is equal to nrow
      if(num == "worst")
        {
         num = nrow(final)
        }
    
      # if num parameter is best than the value of num is equal to 1 
    
      if(num == "best")
        {
         num = 1
        }
    
      # storing a particular value based on rank (num)  
    
      x<-data.frame(final)[num,1]
      data.frame(x)
    
      }
  
 }

# test code to check the accuracy of the function
rankHospital("MD","Heart Attack", 1)

