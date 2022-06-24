# 


# The user inputs a country's name or 2-letter abbreviation, 
# and start dates in the format "YYYY-MM-DD" 

newCount <- function(country, startDate){

    # find the "slug" based on the country the user selected
  slug <- getSlug(country)
  
  # save the day before so we can do our subtraction to get New cases
  # using the cumulative numbers
  # Had to actually subtract 2 to get data from the day before
  dayBefore <- as.Date(startDate) -2
  
  # Get the country data from the Live By Country And Status After Date endpoint
  output <- fromJSON(paste(
    "https://api.covid19api.com/live/country/", slug, "/status/confirmed/date/", dayBefore, "T13:13:30Z", sep = "")
  )

  # Convert the date column to date class
  output$Date <- as.Date(output$Date)
  
  # Add columns 
  # for new confirmed
  output$newConfirmed[1] <- 0
  output$newConfirmed[2:length(output$Confirmed)] <- diff(output$Confirmed[1:length(output$Confirmed)], lag = 1)
  
  # new deaths column
  output$newDeaths[1] <- 0
  output$newDeaths[2:length(output$Deaths)] <- diff(output$Deaths[1:length(output$Deaths)], lag = 1)  
  
  # new recovered
  output$newRecovered[1] <- 0
  output$newRecovered[2:length(output$Recovered)] <- diff(output$Recovered[1:length(output$Recovered)], lag = 1)    
  
  # new active cases
  output$newActive[1] <- 0
  output$newActive[2:length(output$Active)] <- diff(output$Active[1:length(output$Active)], lag = 1)  
  
  # Delete the first observation
  # It was only returned initially so we could subtract it to get the new numbers that day
  output <- output[-1]
  
  # Return the output data frame.
  return(output)
}