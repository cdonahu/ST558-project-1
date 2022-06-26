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
    ## 6   646ec970-8bab-4812-97f2-f6ff0af52c67                      Antarctica
    ## 74  1790f2b7-3579-44fa-bf6e-b63fd63b7657   Holy See (Vatican City State)
    ## 111 b4266b34-7cd6-4df9-bc89-7d21b29af81e                Marshall Islands
    ## 115 a89fab86-b42b-4893-8fd2-5fc2025de216 Micronesia, Federated States of
    ## 21  b0fab17f-9798-4cb5-9a52-d757de551e53                          Bhutan
    ## 77  68dc1984-2b18-49ee-9d9f-cb6a2910e117                         Iceland
    ## 29  f417e6ee-e552-49c5-a9d8-c3136b4dcbfe                         Burundi
    ## 179 1696707c-dd6b-4a1f-8e4b-25731afd3079                           Tonga
    ## 158 1737509f-c00a-46ff-a8cd-f0dfca6880af                       Singapore
    ## 126 efe994ba-e8dc-43fa-8372-5964b29790ea                     New Zealand
    ## 133 efd18531-8e24-4a51-bee8-98ba42c30036                           Palau
    ## 10  c0903c2f-e90f-488f-929d-1ed4bdb0bf53                       Australia
    ## 190 062221fc-fdf8-4093-95aa-1e9e04ce831d                         Vanuatu
    ## 92  6a3c4c55-8984-4744-9ab3-0b4327c3adba                   Korea (South)
    ## 26  1f6cadbc-1cce-472b-a57e-11b75f44b0c6               Brunei Darussalam
    ## 108 b43735e2-c165-411f-a7af-910aaddc4944                        Maldives
    ## 173 78fac0b4-f86e-4109-a3f3-21231e8938e2       Taiwan, Republic of China
    ## 142 6986e816-d032-48d9-abab-e04350bc0af8                           Qatar
    ## 150 6fccb57b-02e8-4fc1-9363-0aecf84e97ac                           Samoa
    ## 48  bbf04e14-f1c6-43e5-bd51-a67ed5699a32                         Denmark
    ## 45  98109b14-3222-4d2d-86ac-d9695f7884a1                          Cyprus
    ## 130 b64e5f00-167d-4bb4-85dc-4e3fd32956f5                          Norway
    ## 118 a6d12e19-b1e1-4a57-89f8-c705dbc8e192                        Mongolia
    ## 14  abe89703-21f5-4f22-9f10-fb499c6dc22c                         Bahrain
    ## 185 44c6277b-e934-411e-8610-e58fd9c7f130            United Arab Emirates
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
    ## 75  95495490-47f9-4c39-afd5-e582c230b6f7                     Honduras
    ## 79  f0bf7d7c-2fc3-4e22-82b2-50f4247e03d3                    Indonesia
    ## 3   3454fadd-0e90-4511-81d3-ed16eedf34b8                      Algeria
    ## 35  8f38c7ec-ec6b-4d2a-84fe-08491fe0f620                         Chad
    ## 73  84924446-b172-4c07-ad91-69f71c8a695a                        Haiti
    ## 181 f3d07864-0f28-47dc-b936-400c09f336d7                      Tunisia
    ## 137 68cd6ff0-ace3-43db-9f77-61beab5e8a4d                     Paraguay
    ## 104 65433199-26df-4963-b099-f562fdf5976c       Macedonia, Republic of
    ## 63  cdfefb3b-f6e4-44c2-a5cc-999c445ce184                       Gambia
    ## 106 4ae19970-addb-451e-939f-45495c6c8212                       Malawi
    ## 122 d2bbba15-a12e-48e3-8f97-98aee3159e4e                      Myanmar
    ## 27  40e75d62-ea19-4e51-8a94-985e0bf4128b                     Bulgaria
    ## 128 9562d4a2-df71-4cfc-a49b-b518a4c5f71f                        Niger
    ## 99  796faef2-dc88-461e-8cd8-521155847c27                      Liberia
    ## 52  6f491327-1483-441a-a3a6-9611395998e0                      Ecuador
    ## 23  a59f17d1-211c-41d7-b656-8166e48506be       Bosnia and Herzegovina
    ## 1   fe439931-1d7f-4415-8544-775c5fb1057c                  Afghanistan
    ## 53  de607d41-5d1d-49f1-bdc9-a397dfa01f28                        Egypt
    ## 162 b8fb24ac-3b48-4c14-aa32-824f61689b8e                      Somalia
    ## 114 5253d61e-5c00-492a-9bb3-3fb9a82ab7a9                       Mexico
    ## 172 baad1677-4f31-49f9-875b-f1e7409c47c8 Syrian Arab Republic (Syria)
    ## 138 33da3075-9679-4cc8-859e-291426bda577                         Peru
    ## 167 e001aeee-0ce0-4451-8659-78594eaa1235                        Sudan
    ## 193 50db70dd-4bed-485e-8f2f-0ca52b704abe                        Yemen
    ## 91  12891229-5b09-42bd-850f-db39d5af2a2b                Korea (North)
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

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/plotCases%20Aus%20and%20Canada-1.png)<!-- -->

``` r
plotCases("Canada")
```

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/plotCases%20Aus%20and%20Canada-2.png)<!-- -->

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

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/Scatter%20plot-1.png)<!-- -->
It is tough to see much here, since the outlier in red is skewing the
y-axis. I wanted to see which country that was, and also the one skewing
the x-axis (to a lesser degree).

``` r
head(covid[order(covid$TotalConfirmed),], 5)
```

    ##                                       ID                         Country
    ## 91  12891229-5b09-42bd-850f-db39d5af2a2b                   Korea (North)
    ## 6   646ec970-8bab-4812-97f2-f6ff0af52c67                      Antarctica
    ## 111 b4266b34-7cd6-4df9-bc89-7d21b29af81e                Marshall Islands
    ## 74  1790f2b7-3579-44fa-bf6e-b63fd63b7657   Holy See (Vatican City State)
    ## 115 a89fab86-b42b-4893-8fd2-5fc2025de216 Micronesia, Federated States of
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
    ## 65  b8b4ff74-d213-4070-8dbf-bfedc12cf278                  Germany          DE
    ## 61  35ba7b8c-cf52-4bd0-bc8e-d2afb3eb072e                   France          FR
    ## 25  cce2870a-119e-49c1-802d-eb1cfc54adec                   Brazil          BR
    ## 78  f28b00b6-b2e9-4103-9dc9-726047c17f11                    India          IN
    ## 187 543a7052-c492-4c6b-a4e8-3e52fcdc2334 United States of America          US
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

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/scatter%20plot%20no%20nK-1.png)<!-- -->

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

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/Histogram-1.png)<!-- -->

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

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/Boxplot-1.png)<!-- -->

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

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/bar%20chart-1.png)<!-- -->

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
