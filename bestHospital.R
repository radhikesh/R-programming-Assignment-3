# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an outcome name. 
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state.

#set the directory to the source file location:

best <- function(state, outcome){
  # Reading the csv file from the directory
  data <- read.csv(file = "Datasets/outcome-of-care-measures.csv", colClasses = "character")
  
  # subsetting the data with the input variable state
  data_1 <- subset(data, data$State == state)
  
  # checking if the input state parameter is a valid state or not
  if (nrow(data_1) <=  0)
  {
    stop("INVALID STATE")
  }
  
  # checking if the input outcome parameter is a valid outcome or not
  if (outcome != "Heart Attack" & outcome != "Heart Failure" & outcome != "Pneumonia")
  {
    stop("INVALID OUTCOME")
  }
  
  # if the input the variable in Heart Attack 
  
  if(outcome == "Heart Attack")
  {
    # We have to identify the best hospital. The best hospital will have the minimum value of the mortality rate, so used "min" function to subset the data. 
    # The data subset is converted to numeric using as.numeric(as.character()) function
    data_output <- subset(data_1, as.numeric(as.character(data_1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) == min(as.numeric(as.character(data_1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),na.rm = TRUE))  
    # the output subset is stored in a dataframe
    output <- data.frame(data_output$Hospital.Name)
    # if two hopitals having the same value then to break the tie, the output dataframe is sorted using sort function. The sort function sort's the dataframe in alphabetical order and it returns only one value of the hospital
    sort(output)[1]
  }
  
  # if the input variable is Heart Failure  
  else if(outcome == "Heart Failure")
  {
    # We have to identify the best hospital. The best hospital will have the minimum value of the mortality rate, so used "min" function to subset the data. 
    # The data subset is converted to numeric using as.numeric(as.character()) function
    data_output <- subset(data_1, as.numeric(as.character(data_1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)) == min(as.numeric(as.character(data_1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),na.rm = TRUE))  
    # the output subset is stored in a dataframe
    output <- data.frame(data_output$Hospital.Name)
    # if two hopitals having the same value then to break the tie, the output dataframe is sorted using sort function. The sort function sort's the dataframe in alphabetical order and it returns only one value of the hospital
    sort(output)[1]
  }
  else
  {
    # We have to identify the best hospital. The best hospital will have the minimum value of the mortality rate, so used "min" function to subset the data. 
    # The data subset is converted to numeric using as.numeric(as.character()) function
    data_output <- subset(data_1, as.numeric(as.character(data_1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)) == min(as.numeric(as.character(data_1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),na.rm = TRUE))  
    # the output subset is stored in a dataframe
    output <- data.frame(data_output$Hospital.Name)
    # if two hopitals having the same value then to break the tie, the output dataframe is sorted using sort function. The sort function sort's the dataframe in alphabetical order and it returns only one value of the hospital
    sort(output)[1]
  }
}

# test code to check the accuracy of the function, returns invalid state
best("TE","Pneumonia")
# test code to check the accuracy of the function, returns invalid outcome
best("TX","pneumoniaa")
# test code to check the correctness of the function, returns the best best hospital in texas. Warnings introduced because originally read the data in as character
best("TX","Pneumonia")
