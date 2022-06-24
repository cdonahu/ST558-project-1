
countryCases <- function(country="all"){
  ###
  # This function returns a data frame with data on Covid cases. It can also
  # return those columns for a single country if a country's name or abbreviation is passed.
  ###
  
  # Get the country data from the summaryRoute endpoint.
  outputAPI <- fromJSON(
    "https://api.covid19api.com/summary"
  )
  
  # Select only the data.frame from the JSON output.
  output <- outputAPI$Countries
  
  # If country does not equal "all", check if it is an abbrev or country name.
  if (country != "all"){
    
    # If country is in the CountryCode column, subset output for just that row.
    if (country %in% output$CountryCode){
      output <- output %>%
        filter(CountryCode == country)
    }
    # If country is in the Country column, subset output for just that row.
    else if (country %in% output$Country){
      output <- output %>%
        filter(Country == country)
    }
    # Otherwise, throw an informative error.
    else {
      message <- paste("ERROR: Argument for country was not found in either",
                       "the Country or CountryCode columns. Try ('all') to",
                       "find the country you're looking for.")
      stop(message)
    }
  }
  # Do nothing if the country value equals "all".
  else {
    
  }
  
 # Return the output data frame.
  return(output)
}