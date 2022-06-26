COVID-19 API Vignette
================
C. Donahue
June 26, 2022

-   [Requirements](#requirements)
-   [Functions to Interact With the
    API](#functions-to-interact-with-the-api)
    -   [`countryCases`](#countrycases)
    -   [`getSlug`](#getslug)
    -   [`dailyCases`](#dailycases)
    -   [`plotCases`](#plotcases)
    -   [`casesByDate`](#casesbydate)
    -   [`typesByCountry`](#typesbycountry)
    -   [`newCount`](#newcount)
    -   [`getAllCovidData`](#getallcoviddata)
-   [Data Exploration](#data-exploration)
-   [Wrap-Up](#wrap-up)

This document is a vignette to describe how to read and summarize data
from an API. I will demonstrate using a [COVID-19
API](https://covid19api.com/), because my attempts to work with the
Pokemon API were unsuccessful. I will build a couple of functions to
interact and explore the data we can retrieve from the API.

To use a function that returns data at the country level, the user may
either enter the country’s name or its two-letter country code in the
[ISO 3166](https://www.iban.com/country-codes) format.

# Requirements

I used the following packages in the creation of the vignette, and the
user will need them to run the functions and interact with the COVID-19
API:

-   [`httr`](https://httr.r-lib.org/): I used this to access the API  
-   [`jsonlite`](https://cran.r-project.org/web/packages/jsonlite/index.html):
    I used this to convert the Pokemon data to a dataframe  
-   [`tidyverse`](https://www.tidyverse.org/): I used this set of
    packages for things like piping and plotting data visualizations  
-   [`sjmisc`](https://cran.r-project.org/web/packages/sjmisc/index.html):
    I used this to group a numeric variable into categories to make a
    histogram  
-   [`countrycode`](https://cran.r-project.org/web/packages/countrycode/countrycode.pdf):
    I used this to categorize the COVID-19 observations by continent

To get started, install and load the packages listed above:

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(sjmisc)
library(countrycode)
```

# Functions to Interact With the API

The COVID-19 API is simple because it does not require users to get a
key and authenticate. This makes accessing it less complex than some
other APIs. If you want, you can purchase different monthly subscription
levels for more than just the basic authorization to the API. The
premium subscriptions include more interesting data (population ages,
GDP, diabetes rates, handwashing facilities, etc.) and no rate limit.

## `countryCases`

I wrote a function `countryCases` for a user to interact with the
`summary` endpoint of the Coronavirus COVID19 API. It returns a data
frame with key metrics (cases, deaths, recoveries) for every country. It
accepts one argument, `country`, and the default value is “all”. The
user may enter a country’s name or its two character country code to get
only data for a specific country.

``` r
countryCases <- function(country="all"){
  ###
  # This function returns a data frame with data on Covid cases. It can also
  # return those columns for a single country if a country's name or abbreviation is passed.
  ###
  
  # Get the country data from the summaryRoute endpoint.
  outputAPI <- fromJSON(
    "https://api.covid19api.com/summary"
  )
  
  # Select only the Countries data from the JSON output.
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

  # convert the Date to a Date class
  output$Date <- as.Date(output$Date)
  
 # Return the output data frame.
  return(output)
}
```

## `getSlug`

I also wrote a helper function, `getSlug`, which finds the “slug”, or
the version of a country’s name formatted for use within a URL. It uses
the `Countries` endpoint. The user inputs a country’s name or two-letter
abbreviation, and `getSlug` returns the slug. This will help in other
functions that interact with the COVID-19 API.

``` r
# Function to get the slug for a given country name or abbreviation
getSlug <- function(country){
  
  # Get the country list from the Countries endpoint.
  output <- fromJSON("https://api.covid19api.com/countries")
  
  # If country is in the ISO2 column, subset output for just that row.
  if (country %in% output$ISO2){
    output <- output$Slug[output$ISO2 == country]
  }
  # If country is in the Country column, subset output for just that row.
  else if (country %in% output$Country){
    output <- output$Slug[output$Country == country]
  }
  # Otherwise, throw an informative error.
  else {
    message <- paste("ERROR: Argument for country was not found in either",
                     "the Country or CountryCode columns. Try ('all') to",
                     "find the country you're looking for.")
    stop(message)
  }
  # Return the slug.
  return(output)
}
```

## `dailyCases`

I wrote a function to allow the user to select a country of interest and
receive the daily confirmed case count for that country, as well as the
country’s location (latitude/longitude). The `dailyCases` function uses
the helper function `getSlug` from above to retrieve the information via
the `Day One Live` endpoint. It returns cumulative confirmed cases
starting with the first day a case was confirmed.

``` r
dailyCases <- function(country){
  
  # find the "slug" based on the country the user selected
  slug <- getSlug(country)
  
  # Get the country data from the Day One Live endpoint.
  output <- fromJSON(paste(
    "https://api.covid19api.com/dayone/country/", slug, "/status/confirmed/live", sep = "")
  )
  
  # Return the output data frame.
  return(output)
}
```

## `plotCases`

Next, I wanted to create a function to plot the cases by date, so the
user could get a visualization of the type of data they retrieve using
my `dailyCases` function. The user inputs a country, and receives a
labeled plot of cumulative confirmed cases in that country.

``` r
plotCases <- function(country){
  
  data <- dailyCases(country)
  
  # Convert date column from character into date class
  data$Date <- as.Date(data$Date)
  
  # Plot the data
  ggplot(data, aes(x = Date, y = Cases)) +
    geom_col() +
    ggtitle(paste("Cumulative Cases by Date in ", country, sep = "")) +
    xlab("Date") + ylab("Cumulative Cases")
}
```

## `casesByDate`

The `casesByDate` function interacts with the `By Country Live`
endpoint, using the user’s choice of country, start date, and end date.
It returns a data frame with daily cumulative confirmed case counts, as
well as the country’s name, abbreviation, and location.

``` r
# The user inputs a country or 2-letter abbreviation, 
# and start/end dates in the format YYYY-MM-DD 

casesByDate <- function(country, startDate, endDate){
  
  ###
  # This function returns a data frame with data on number of cumulative confirmed Covid cases between selected dates in selected country. 
  ###
  
  # find the "slug" based on the country the user selected
  slug <- getSlug(country)
  
  # Get the country data from the Day One Live endpoint.
  output <- fromJSON(paste(
    "https://api.covid19api.com/dayone/country/", slug, "/status/confirmed/live?from=", startDate, "T00:00:00Z&to=", endDate, "T00:00:00Z", sep = "")
  )
  
  # Return the output data frame.
  return(output)
}
```

## `typesByCountry`

The `typesByCountry` function interacts with the
`Live By Country and Status` endpoint. This function returns a data
frame with data on number of cumulative confirmed Covid cases, deaths,
recoveries, and active cases in the selected country over time.

``` r
typesByCountry <- function(country){
  
  # find the "slug" based on the country the user selected
  slug <- getSlug(country)
  
  # Get the country data from the Day One Live endpoint.
  output <- fromJSON(paste(
    "https://api.covid19api.com/dayone/country/", slug, "/status/confirmed", sep = "")
  )
  
  # Return the output data frame.
  return(output)
}
```

## `newCount`

After looking through the data while making the functions above, I
wanted to calculate the daily number of *new* cases, instead of just
looking at cumulative case counts for a country. To get new cases for
today, for example one just needs to subtract yesterday’s cumulative
case count from today’s cumulative case count.  
For the `newCount` function, we interact with the
`Live By Country and Status After Date` endpoint. (Some other endpoints
do have a count of new cases.) The user parameters include a country of
interest and a start date. The function will output a data frame for
that country with the number of new cases of each type (Confirmed,
Deaths, Recovered, Active), each day.

``` r
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
  
  # Need to sort by province and then date
  # but only if the selected country lists provinces (or states)
  if (!all(is.na(output$Province) | output$Province == "")) {
    output <- output[order(output$Province),]
  }
  
  # needs some work still in order to properly handle countries w/provinces
  
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
```

## `getAllCovidData`

Finally, I created a function to get all the COVID-19 data from the API
and return a dataframe to the user. This function interacts through the
`All Data` endpoint, returning 10MB of data, so should be used
infrequently.

``` r
getAllCovidData <- function(){

  # Get the data from the All Data endpoint.
  output <- fromJSON("https://api.covid19api.com/all")
  
  # Return the output data frame.
  return(output)
}
```

# Data Exploration

First let’s pull a summary of the COVID-19 data for all countries by
calling `getAllCovidData()`.

``` r
covid <- countryCases()
```

One of the most concerning aspects of COVID-19 is how deadly it is, so I
wanted to look at the number of deaths as a percentage of confirmed
cases. This variable does not exist, so I need to calculate it. I
divided the total number of deaths for each country by the total number
of confirmed cases.

``` r
# Add a column for the fatality rate. 
covid <- covid %>%
  mutate(FatalityRate = TotalDeaths / TotalConfirmed)
```

I would hypothesize that wealthier countries have a lower fatality rate
because of access to better medical care and safer living conditions in
general. Since this dataset does not give me any measure of a country’s
wealth (without a paid subscription), I will just look at a ranking of
the best and worst fatality rates to see if I notice anything.

``` r
head(covid[order(covid$FatalityRate),], 25)
```

    ##                                       ID                         Country
    ## 6   8b6ab99a-e614-4e58-8eaa-63c64459247f                      Antarctica
    ## 74  47110b35-204f-49b0-964c-862f6f66ae34   Holy See (Vatican City State)
    ## 111 aac1a54b-6b0a-49a4-96c7-5d87ef97bb43                Marshall Islands
    ## 115 678488ab-8555-478e-b73b-6825d7f4e79e Micronesia, Federated States of
    ## 21  f4feba31-e52d-41a8-b5ae-06da6f03b6e0                          Bhutan
    ## 77  7e34f8a9-4699-45be-9c1e-9d38048c8f25                         Iceland
    ## 29  1c7d93ad-aced-42f7-9967-f97c01385617                         Burundi
    ## 179 a5d30d2b-34b2-4eb4-b966-40200b3a9ec1                           Tonga
    ## 158 41777e91-4ec6-41bc-8d33-6f22a036d539                       Singapore
    ## 126 c823dc40-025d-473e-bd94-a26ffa9c8535                     New Zealand
    ## 133 d032a660-418a-4a18-9001-d12f37880b79                           Palau
    ## 10  1646a507-c638-4526-9a25-a6a762c094d9                       Australia
    ## 190 542f759f-85ae-4c14-aaf4-e88e34659a3d                         Vanuatu
    ## 92  c872e581-bee3-4e8c-8830-b3e7c12d4543                   Korea (South)
    ## 26  cb124a1f-1616-4948-a97e-41febbd693f8               Brunei Darussalam
    ## 108 053b1282-6134-412d-9509-18b4bc8690a4                        Maldives
    ## 173 219e619f-f814-4802-bf0b-25309494484e       Taiwan, Republic of China
    ## 142 5ac621d9-283b-4f95-ab7c-46087454d8cd                           Qatar
    ## 150 a172adfd-c8e5-4afe-b0f9-3459ff81bc5f                           Samoa
    ## 48  698c1444-8a72-4176-a817-5f3bfb889e36                         Denmark
    ## 45  fb4f6bc9-0e9d-4f10-bfdd-1edd16b580ff                          Cyprus
    ## 130 04288e76-1899-4102-8c4d-1252f9c2c146                          Norway
    ## 118 b56f291e-6c19-47e2-a120-d9f2e6c5ed45                        Mongolia
    ## 14  192949f4-bd64-468d-9bec-741c7e48c5f2                         Bahrain
    ## 185 c3e50e5d-3270-4609-8cf4-5cf9c2ab12cd            United Arab Emirates
    ##     CountryCode                        Slug NewConfirmed TotalConfirmed
    ## 6            AQ                  antarctica            0             11
    ## 74           VA holy-see-vatican-city-state            0             29
    ## 111          MH            marshall-islands            0             18
    ## 115          FM                  micronesia            0             38
    ## 21           BT                      bhutan            0          59674
    ## 77           IS                     iceland            0         192991
    ## 29           BI                     burundi            0          42542
    ## 179          TO                       tonga            0          12079
    ## 158          SG                   singapore            0        1403242
    ## 126          NZ                 new-zealand            0        1314155
    ## 133          PW                       palau            0           5201
    ## 10           AU                   australia        21437        8000312
    ## 190          VU                     vanuatu            0          11044
    ## 92           KR                 korea-south            0       18326019
    ## 26           BN                      brunei            0         159591
    ## 108          MV                    maldives            0         180384
    ## 173          TW                      taiwan            0        3573703
    ## 142          QA                       qatar            0         379277
    ## 150          WS                       samoa            0          14812
    ## 48           DK                     denmark            0        3207777
    ## 45           CY                      cyprus            0         504717
    ## 130          NO                      norway            0        1444043
    ## 118          MN                    mongolia            0         926282
    ## 14           BH                     bahrain            0         616588
    ## 185          AE        united-arab-emirates            0         937037
    ##     NewDeaths TotalDeaths NewRecovered TotalRecovered       Date FatalityRate
    ## 6           0           0            0              0 2022-06-26 0.0000000000
    ## 74          0           0            0              0 2022-06-26 0.0000000000
    ## 111         0           0            0              0 2022-06-26 0.0000000000
    ## 115         0           0            0              0 2022-06-26 0.0000000000
    ## 21          0          21            0              0 2022-06-26 0.0003519121
    ## 77          0         153            0              0 2022-06-26 0.0007927831
    ## 29          0          38            0              0 2022-06-26 0.0008932349
    ## 179         0          12            0              0 2022-06-26 0.0009934597
    ## 158         0        1408            0              0 2022-06-26 0.0010033907
    ## 126         0        1411            0              0 2022-06-26 0.0010736937
    ## 133         0           6            0              0 2022-06-26 0.0011536243
    ## 10         27        9682            0              0 2022-06-26 0.0012102028
    ## 190         0          14            0              0 2022-06-26 0.0012676566
    ## 92          0       24522            0              0 2022-06-26 0.0013380975
    ## 26          0         225            0              0 2022-06-26 0.0014098539
    ## 108         0         300            0              0 2022-06-26 0.0016631187
    ## 173         0        6120            0              0 2022-06-26 0.0017125094
    ## 142         0         678            0              0 2022-06-26 0.0017876117
    ## 150         0          29            0              0 2022-06-26 0.0019578720
    ## 48          0        6487            0              0 2022-06-26 0.0020222727
    ## 45          0        1072            0              0 2022-06-26 0.0021239625
    ## 130         0        3280            0              0 2022-06-26 0.0022714005
    ## 118         0        2179            0              0 2022-06-26 0.0023524154
    ## 14          0        1492            0              0 2022-06-26 0.0024197681
    ## 185         0        2311            0              0 2022-06-26 0.0024662847

What stands out, interestingly, is that a lot of these countries in the
top 25 (lowest) fatality rate list are island nations.

``` r
tail(covid[order(covid$FatalityRate),], 25)
```

    ##                                       ID                      Country
    ## 75  79efdb4e-5ebb-48ae-84fe-b85596681e99                     Honduras
    ## 79  8b989979-8d4b-4623-b1cc-c51edbc09193                    Indonesia
    ## 3   22b54635-374d-48ed-83f8-b1cb4d9f4d6f                      Algeria
    ## 35  3a1b4e16-dfa6-46b9-9734-cc1fac69d280                         Chad
    ## 73  dd27fff0-c20f-4f1d-bf75-7c8aae598aac                        Haiti
    ## 181 830dc2f5-b97f-439e-a330-431029cf7a37                      Tunisia
    ## 137 40c3e5f7-509c-49da-82f9-72394dafb269                     Paraguay
    ## 104 9089b3bf-10be-480f-9607-52e31bc29a76       Macedonia, Republic of
    ## 63  dcd14f0c-add3-4479-bacf-93266f17ca81                       Gambia
    ## 106 df32a8d1-3404-4884-b3e5-11972dc562af                       Malawi
    ## 122 6d9b26e6-2919-471e-827c-799ba2760d0a                      Myanmar
    ## 27  82632e10-9862-41a8-bda8-8b84e6601e53                     Bulgaria
    ## 128 2c3e44b9-161f-4369-b27e-c3a5d3b9e21c                        Niger
    ## 99  f7529cd5-6043-4dc3-a53b-26c0f0151640                      Liberia
    ## 52  54af5b14-4e1c-4a1f-9307-b60e30736cac                      Ecuador
    ## 23  fe0d0627-e0b1-423a-b141-e7e83d68f9e3       Bosnia and Herzegovina
    ## 1   a682cc75-a85d-4df5-9352-769ce863179f                  Afghanistan
    ## 53  78171cb8-5444-40e9-a62b-e47f49d56ed4                        Egypt
    ## 162 16d5013d-013e-474a-947a-bdabcae970c0                      Somalia
    ## 114 76c5f10e-3947-4f1f-b85c-38a835b62997                       Mexico
    ## 172 1a9c51ca-00d4-479d-9c8f-5ff07819fd56 Syrian Arab Republic (Syria)
    ## 138 3e4f0ed1-6984-4e16-9d44-e47d64077830                         Peru
    ## 167 b821c9cf-ae75-46ce-95ad-079793fd1eaa                        Sudan
    ## 193 80188572-1146-4891-9437-62b49f6295dc                        Yemen
    ## 91  0a101f00-c4fd-4c46-8355-b49c3c87fa7e                Korea (North)
    ##     CountryCode                   Slug NewConfirmed TotalConfirmed NewDeaths
    ## 75           HN               honduras            0         426490         0
    ## 79           ID              indonesia            0        6078725         0
    ## 3            DZ                algeria            0         266030         0
    ## 35           TD                   chad            0           7424         0
    ## 73           HT                  haiti            0          31301         0
    ## 181          TN                tunisia            0        1046703         0
    ## 137          PY               paraguay            0         655532         0
    ## 104          MK              macedonia            0         313360         0
    ## 63           GM                 gambia            0          12002         0
    ## 106          MW                 malawi            0          86348         0
    ## 122          MM                myanmar            0         613553         0
    ## 27           BG               bulgaria            0        1169968         0
    ## 128          NE                  niger            0           9031         0
    ## 99           LR                liberia            0           7493         0
    ## 52           EC                ecuador            0         901739         0
    ## 23           BA bosnia-and-herzegovina            0         378413         0
    ## 1            AF            afghanistan            0         182072         0
    ## 53           EG                  egypt            0         515645         0
    ## 162          SO                somalia            0          26748         0
    ## 114          MX                 mexico        33646        5956732        65
    ## 172          SY                  syria            0          55920         0
    ## 138          PE                   peru            0        3611123         0
    ## 167          SD                  sudan            0          62551         0
    ## 193          YE                  yemen            0          11824         0
    ## 91           KP            korea-north            0              1         0
    ##     TotalDeaths NewRecovered TotalRecovered       Date FatalityRate
    ## 75        10904            0              0 2022-06-26   0.02556684
    ## 79       156714            0              0 2022-06-26   0.02578074
    ## 3          6875            0              0 2022-06-26   0.02584295
    ## 35          193            0              0 2022-06-26   0.02599677
    ## 73          837            0              0 2022-06-26   0.02674036
    ## 181       28670            0              0 2022-06-26   0.02739077
    ## 137       18963            0              0 2022-06-26   0.02892765
    ## 104        9322            0              0 2022-06-26   0.02974853
    ## 63          365            0              0 2022-06-26   0.03041160
    ## 106        2645            0              0 2022-06-26   0.03063186
    ## 122       19434            0              0 2022-06-26   0.03167453
    ## 27        37246            0              0 2022-06-26   0.03183506
    ## 128         310            0              0 2022-06-26   0.03432621
    ## 99          294            0              0 2022-06-26   0.03923662
    ## 52        35705            0              0 2022-06-26   0.03959571
    ## 23        15799            0              0 2022-06-26   0.04175068
    ## 1          7717            0              0 2022-06-26   0.04238433
    ## 53        24722            0              0 2022-06-26   0.04794384
    ## 162        1361            0              0 2022-06-26   0.05088231
    ## 114      325576            0              0 2022-06-26   0.05465682
    ## 172        3150            0              0 2022-06-26   0.05633047
    ## 138      213443            0              0 2022-06-26   0.05910710
    ## 167        4951            0              0 2022-06-26   0.07915141
    ## 193        2149            0              0 2022-06-26   0.18174899
    ## 91            6            0              0 2022-06-26   6.00000000

The worst fatality rates, meanwhile, seem to include not just generally
poorer countries, but many that are also war-torn.

Next, I want to create some charts. I will start with the simple
`plotCases` function to see what looks interesting. I’m going to choose
Australia, as it’s a developed country but also an island, and then
Canada.

``` r
plotCases("Australia")
```

![](/../README_files/figure-gfm/plotCases%20Aus%20and%20Canada-1.png)<!-- -->

``` r
plotCases("Canada")
```

![](../README_files/figure-gfm/plotCases%20Aus%20and%20Canada-2.png)<!-- -->

Looks like Australia had a lot less gradual increase in case counts, at
least compared to Canada. Although both have a large climb right around
the beginning of 2022. It is important to note that the scale of the
y-axis is not consistent here, so we are really just comparing shape and
not scale.

Below I plotted the fatality rate against the total cumulative count of
confirmed COVID-19 cases. I added a regression line as well. What really
stands out, however, is that there is an outlier (North Korea, in fact)
with an incredibly high fatality rate (and another with a really high
case count, but not on the same scale).

``` r
plot1 <- ggplot(covid, aes(TotalConfirmed,
                                   FatalityRate,
                                   color=FatalityRate)) + 
  # Add a scatter plot layer and adjust the size and opaqueness of points.
  geom_point(size=4, alpha=0.75) + 
  # Add a color gradient for FatalityRate
  scale_color_gradient(low="blue", high="red") + 
  # Remove the legend because it takes up space.
  theme(legend.position="none") + 
  # Add a black regression line.
  geom_smooth(method=lm, formula=y~x, color="black") + 
  # Add labels to the axes.
  scale_x_continuous("Cumulative Confirmed Cases") + 
  scale_y_continuous("Fatality Rate") + 
  # Add a title.
  ggtitle("Fatality Rate vs. Cumulative Confirmed Cases of COVID-19")

plot1
```

![](../README_files/figure-gfm/Scatter%20plot-1.png)<!-- -->
It is tough to see much here, since the outlier in red is skewing the
y-axis. I wanted to see which country that was, and also the one skewing
the x-axis (to a lesser degree).

``` r
head(covid[order(covid$TotalConfirmed),], 5)
```

    ##                                       ID                         Country
    ## 91  0a101f00-c4fd-4c46-8355-b49c3c87fa7e                   Korea (North)
    ## 6   8b6ab99a-e614-4e58-8eaa-63c64459247f                      Antarctica
    ## 111 aac1a54b-6b0a-49a4-96c7-5d87ef97bb43                Marshall Islands
    ## 74  47110b35-204f-49b0-964c-862f6f66ae34   Holy See (Vatican City State)
    ## 115 678488ab-8555-478e-b73b-6825d7f4e79e Micronesia, Federated States of
    ##     CountryCode                        Slug NewConfirmed TotalConfirmed
    ## 91           KP                 korea-north            0              1
    ## 6            AQ                  antarctica            0             11
    ## 111          MH            marshall-islands            0             18
    ## 74           VA holy-see-vatican-city-state            0             29
    ## 115          FM                  micronesia            0             38
    ##     NewDeaths TotalDeaths NewRecovered TotalRecovered       Date FatalityRate
    ## 91          0           6            0              0 2022-06-26            6
    ## 6           0           0            0              0 2022-06-26            0
    ## 111         0           0            0              0 2022-06-26            0
    ## 74          0           0            0              0 2022-06-26            0
    ## 115         0           0            0              0 2022-06-26            0

``` r
tail(covid[order(covid$TotalConfirmed),], 5)
```

    ##                                       ID                  Country CountryCode
    ## 65  c000439b-1059-4475-bb0d-3b2e8584c556                  Germany          DE
    ## 61  784c8225-6aae-41ca-8a31-e34a818f44d0                   France          FR
    ## 25  2a6425df-5814-4531-927e-1f361c6ea046                   Brazil          BR
    ## 78  31fe6b9b-2877-4cdb-9155-3f035e4c588d                    India          IN
    ## 187 925a42ba-7a86-462f-9116-34b25825b8c8 United States of America          US
    ##              Slug NewConfirmed TotalConfirmed NewDeaths TotalDeaths NewRecovered
    ## 65        germany            1       27771112         0      140734            0
    ## 61         france            0       30714200         0      150356            0
    ## 25         brazil            0       32023166         0      670229            0
    ## 78          india        11739       43389973        25      524999            0
    ## 187 united-states        39372       86949088       144     1015933            0
    ##     TotalRecovered       Date FatalityRate
    ## 65               0 2022-06-26  0.005067640
    ## 61               0 2022-06-26  0.004895325
    ## 25               0 2022-06-26  0.020929505
    ## 78               0 2022-06-26  0.012099547
    ## 187              0 2022-06-26  0.011684228

If we rank the data by number of confirmed cases (cumulative), we can
see that the outlier in red on the scatter plot is there because North
Korea only had one confirmed case, but somehow had six deaths. And the
outlier with the incredibly high case count? That’s the United States,
with almost 87 million as of late June 2022. I plotted the same thing
once more, but left out North Korea.

``` r
ggplot(covid[covid$Country != "Korea (North)",], aes(TotalConfirmed,
                                   FatalityRate,
                                   color=FatalityRate)) + 
  # Add a scatter plot layer and adjust the size and opaqueness of points.
  geom_point(size=4, alpha=0.75) + 
  # Add a color gradient for FatalityRate
  scale_color_gradient(low="blue", high="red") + 
  # Remove the legend because it takes up space.
  theme(legend.position="none") + 
  # Add a black regression line.
  geom_smooth(method=lm, formula=y~x, color="black") + 
  # Add labels to the axes.
  scale_x_continuous("Cumulative Confirmed Cases") + 
  scale_y_continuous("Fatality Rate") + 
  # Add a title.
  ggtitle("Fatality Rate vs. Cumulative Confirmed Cases of COVID-19")
```

![](../README_files/figure-gfm/scatter%20plot%20no%20nK-1.png)<!-- -->

Now the scatter plot looks more reasonable, and it’s easy to see the
legitimate outliers (Yemen for Fatality Rate and USA for Cases). And
more generally, that countries with higher case counts tend to have
not-so-high fatality rates.

I wanted to see how the fatality rate data was shaped or distributed,
and whether it was gather around a certain rate, or close to zero. I
plotted FatalityRate in a histogram to get an idea. For this, I dropped
the North Korea observation, which did not seem to be documented the
same way other countries were and was an extreme outlier, causing
problems in my visualization.

``` r
ggplot(covid[covid$Country != "Korea (North)",], aes(x=FatalityRate)) +
  geom_histogram(color = "orange", fill = "pink") +
  ggtitle("Histogram of Countries by Fatality Rate") +
  ylab("Count of Countries") +
  xlab("Deaths per Confirmed Case")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../README_files/figure-gfm/Histogram-1.png)<!-- -->

Then I decided to bring in the package `countrycode` so that I could
categorize the observations by continent.

``` r
library(countrycode)

covid$Continent <- countrycode::countrycode(sourcevar = covid[,"Country"],
                                            origin = "country.name",
                                            destination = "continent")
```

    ## Warning in countrycode_convert(sourcevar = sourcevar, origin = origin, destination = dest, : Some values were not matched unambiguously: Antarctica, Republic of Kosovo

``` r
covid$Continent[covid$Country == "Antarctica"] <- "Antarctica"
covid$Continent[covid$Country == "Republic of Kosovo"] <- "Europe" 
```

Antarctica and Kosovo were the only two observations I had to manually
set a continent for. I looked at a contingency table of the new column
using \`table(covid$Continent) to see how many observations each
continent had.

``` r
table(covid$Continent)
```

    ## 
    ##     Africa   Americas Antarctica       Asia     Europe    Oceania 
    ##         54         35          1         48         45         12

Once I had the continents set, I looked at how COVID-19 fatality rates
compared across continents. I looked at means and medians for the total
number of deaths, cases (cumulative), and new cases grouped by
continent.

``` r
covid %>%
  group_by(Continent) %>%
  summarise(n = n(),
            meanDeaths = mean(TotalDeaths),
            medianDeaths = median(TotalDeaths),
            meanCases = mean(TotalConfirmed),
            medianCases = median(TotalConfirmed),
            meanNewCases = mean(NewConfirmed),
            medianNewCases = median(NewConfirmed))
```

    ## # A tibble: 6 × 8
    ##   Continent      n meanDeaths medianDeaths meanCases medianCases meanNewCases
    ##   <chr>      <int>      <dbl>        <dbl>     <dbl>       <dbl>        <dbl>
    ## 1 Africa        54      4720.        992     222659.      60990.           0 
    ## 2 Americas      35     78758.       5728    4602151.     525716         2473.
    ## 3 Antarctica     1         0           0         11          11            0 
    ## 4 Asia          48     30087.       8912.   3251606.     859711          676.
    ## 5 Europe        45     41219       13822    4517730.    1444043         1340.
    ## 6 Oceania       12      1071.         21.5   791050.      13446.        1786.
    ## # … with 1 more variable: medianNewCases <dbl>

Excluding Antarctica and Oceania due to the small numbers of countries
in each continent, these summaries show that countries in Europe
generally have more (reported) COVID-19 deaths than countries in Africa,
the Americas, and Asia. I thought that was a little surprising.

Then I looked at the Fatality Rate compared across continents, using a
box and whiskers plot. I again dropped the North Korea observation for
this plot.

``` r
ggplot(covid[covid$Country != "Korea (North)",], aes(Continent, FatalityRate)) +
  geom_boxplot(color = "blue", outlier.color = "red") +
  ggtitle("Boxplot of COVID-19 Fatality Rate by Continent") +
  ylab("Deaths per Confirmed Case")
```

![](../README_files/figure-gfm/Boxplot-1.png)<!-- -->

The boxplot was interesting, and could probably be moreso if I had split
the “Americas” continent category up by North and South America. You can
see that the fatality rates in Oceania are significantly lower than
those in Africa and the Americas, because the boxes to not overlap
vertically. But I also noticed that the fatality rate for Europe was
lower than Africa and the Americas. So the higher death toll in Europe
noted in the summary above could be due to the higher case count there,
not because the disease was more deadly.

A lot of American tourists are starting to travel overseas this summer
after taking a couple years off. Let’s look at data for some common
destinations. I will use `newCount()` to get data sets for Austria,
Switzerland, and Greece from the beginning of May 2022.

``` r
austria <- newCount("Austria", "2022-05-01")
switzerland <- newCount("Switzerland", "2022-05-01")
greece <- newCount("Greece", "2022-05-01")
```

Let’s plot the daily count of new cases for these countries. First, I am
going to join the data sets by the date column, though.

``` r
vacation <- rbind(austria, switzerland, greece)

ggplot(vacation, aes(fill=Country, y= newConfirmed, x=Date)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Summer 2022 New COVID-19 Cases by Country") +
  ylab("New Cases")
```

![](../README_files/figure-gfm/bar%20chart-1.png)<!-- -->

This plot is not as compelling as I had hoped because it appears that
Switzerland is only reporting new cases every week or so, and Austria is
not doing so on the weekends. These varying reporting timelines mean
visualizing the daily data does not necessarily give the whole picture
or make for great comparison. Still, from this chart, it does seem that
from the beginning of May until the end of June, new case counts are
generally increasing in these three summer travel destinations.

# Wrap-Up

To summarize everything I did in this vignette, I built functions to
interact with some of the COVID-19 API’s endpoints, retrieved some of
the data, and explored it using tables, numerical summaries, and data
visualization. I found some interesting things, like that some island
nations had more success keeping COVID cases at bay for longer and
keeping the COVID mortality rate low, while people living in poorer or
war-torn countries had a harder time with surviving the pandemic.

I hope this vignette helps my readers more successfully interact with
APIs. It has been good practice for me.
