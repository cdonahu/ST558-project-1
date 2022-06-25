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
    ## 6   fab4a32e-c3ac-4f76-9d6a-ed36545d96f6                      Antarctica
    ## 74  ad201080-746e-45d6-b982-63dae82d4041   Holy See (Vatican City State)
    ## 111 55ccbae4-befe-45ac-81b1-8f560d4aad29                Marshall Islands
    ## 115 85ac342a-f938-491f-80b8-6e8e64dbcc6b Micronesia, Federated States of
    ## 21  228c5b7b-8a9f-4f83-8fd2-f7f9e66da400                          Bhutan
    ## 77  9ad55824-a060-4889-bc32-6fe43d314498                         Iceland
    ## 29  4286a5a5-f58c-4126-95da-035d921655fd                         Burundi
    ## 179 8ade5617-44bd-4011-8175-007272b908aa                           Tonga
    ## 158 0cf1ffca-4a5a-48c5-84be-106f28fbb5e2                       Singapore
    ## 126 188a2e96-45e3-483e-b3fe-713abba69a51                     New Zealand
    ## 133 8765358a-2506-4dc8-8bd2-bc8bae16d0e3                           Palau
    ## 10  8f5a94d6-4b07-4f9c-9139-fb72cd57ba45                       Australia
    ## 190 a78fc17d-5a5b-4d63-9438-7716e06e95d9                         Vanuatu
    ## 92  62590406-3e02-4dd4-8f01-6ff99893bccc                   Korea (South)
    ## 26  70b49dbc-ec75-4f33-9859-eb6adccfbea7               Brunei Darussalam
    ## 108 8305c63b-0b8f-42bb-a15f-13c71de8627b                        Maldives
    ## 173 5398c4ff-735e-47e9-8be8-c102d607ecec       Taiwan, Republic of China
    ## 142 7cce51f9-035d-4525-90be-d3bf9a4dc2f0                           Qatar
    ## 150 a9a0026c-2266-47e4-ba2e-a82dd3e4c6bf                           Samoa
    ## 48  987f89ed-6a29-4129-bbb3-eef81aa6b4a9                         Denmark
    ## 45  265fdaaf-a5dd-4079-bf39-f0041846e4a8                          Cyprus
    ## 130 5848cca7-7b29-47f0-81fd-22dda1891108                          Norway
    ## 118 3aea266e-644f-4cf8-90f3-4b62d7d30109                        Mongolia
    ## 14  a2e88334-4eb2-40cd-a1cd-fcd7e4f7f37d                         Bahrain
    ## 185 0ef33aee-a8d2-4bef-ba34-8439f656b1a9            United Arab Emirates
    ##     CountryCode                        Slug NewConfirmed TotalConfirmed
    ## 6            AQ                  antarctica            0             11
    ## 74           VA holy-see-vatican-city-state            0             29
    ## 111          MH            marshall-islands            0             18
    ## 115          FM                  micronesia            0             38
    ## 21           BT                      bhutan            0          59674
    ## 77           IS                     iceland            0         192991
    ## 29           BI                     burundi            0          42542
    ## 179          TO                       tonga            0          12079
    ## 158          SG                   singapore            0        1397074
    ## 126          NZ                 new-zealand            0        1309547
    ## 133          PW                       palau            0           5201
    ## 10           AU                   australia        33149        7978875
    ## 190          VU                     vanuatu            0          11044
    ## 92           KR                 korea-south            0       18319773
    ## 26           BN                      brunei            0         159591
    ## 108          MV                    maldives            0         180384
    ## 173          TW                      taiwan            0        3533335
    ## 142          QA                       qatar            0         378818
    ## 150          WS                       samoa            0          14812
    ## 48           DK                     denmark            0        3207777
    ## 45           CY                      cyprus            0         504717
    ## 130          NO                      norway            0        1443637
    ## 118          MN                    mongolia            0         926282
    ## 14           BH                     bahrain            0         615125
    ## 185          AE        united-arab-emirates            0         935345
    ##     NewDeaths TotalDeaths NewRecovered TotalRecovered       Date FatalityRate
    ## 6           0           0            0              0 2022-06-25 0.0000000000
    ## 74          0           0            0              0 2022-06-25 0.0000000000
    ## 111         0           0            0              0 2022-06-25 0.0000000000
    ## 115         0           0            0              0 2022-06-25 0.0000000000
    ## 21          0          21            0              0 2022-06-25 0.0003519121
    ## 77          0         153            0              0 2022-06-25 0.0007927831
    ## 29          0          38            0              0 2022-06-25 0.0008932349
    ## 179         0          12            0              0 2022-06-25 0.0009934597
    ## 158         0        1408            0              0 2022-06-25 0.0010078206
    ## 126         0        1405            0              0 2022-06-25 0.0010728901
    ## 133         0           6            0              0 2022-06-25 0.0011536243
    ## 10         52        9655            0              0 2022-06-25 0.0012100703
    ## 190         0          14            0              0 2022-06-25 0.0012676566
    ## 92          0       24516            0              0 2022-06-25 0.0013382262
    ## 26          0         225            0              0 2022-06-25 0.0014098539
    ## 108         0         300            0              0 2022-06-25 0.0016631187
    ## 173         0        5969            0              0 2022-06-25 0.0016893388
    ## 142         0         678            0              0 2022-06-25 0.0017897777
    ## 150         0          29            0              0 2022-06-25 0.0019578720
    ## 48          0        6487            0              0 2022-06-25 0.0020222727
    ## 45          0        1072            0              0 2022-06-25 0.0021239625
    ## 130         0        3280            0              0 2022-06-25 0.0022720393
    ## 118         0        2179            0              0 2022-06-25 0.0023524154
    ## 14          0        1492            0              0 2022-06-25 0.0024255233
    ## 185         0        2310            0              0 2022-06-25 0.0024696770

What stands out, interestingly, is that a lot of these countries in the
top 25 (lowest) fatality rate list are island nations.

``` r
tail(covid[order(covid$FatalityRate),], 25)
```

    ##                                       ID                      Country
    ## 75  7fcb9cc2-c4e0-4b0b-9392-34062571dbeb                     Honduras
    ## 79  5716d4f6-1dfd-4698-b55e-66d8df9678a1                    Indonesia
    ## 3   c89ef663-78b3-40b5-9547-3d47c8ec061c                      Algeria
    ## 35  26cb5eb0-6de6-4e01-9103-17565715d8b6                         Chad
    ## 73  4cf7592c-1052-4352-97a4-783750d236f9                        Haiti
    ## 181 05606d37-17c7-4baa-9939-bb82311d4e83                      Tunisia
    ## 137 8bfdf4ff-0add-4018-998a-4dd2ebe7f0d0                     Paraguay
    ## 104 b790bc2a-7e33-4c15-b44c-0ccc4ba8d4ca       Macedonia, Republic of
    ## 63  9a730dfd-47f9-47b9-bf2d-741bec855c4d                       Gambia
    ## 106 7184c5fa-0269-41b1-a886-acb3def83cfd                       Malawi
    ## 122 5bcdd3aa-1715-4579-abfd-7dafc9ade82f                      Myanmar
    ## 27  c5894d27-f7f9-4cf3-8c77-7336ce79dfae                     Bulgaria
    ## 128 2048e307-7769-4a3e-959e-88745c70a59a                        Niger
    ## 99  0d4fe7a0-3ca2-45c0-9a01-7ce512ae5d13                      Liberia
    ## 52  c98f4bde-68df-42c9-b24f-f5eb91711c2c                      Ecuador
    ## 23  4dce8d35-b6e1-4ccd-bf91-332502b92195       Bosnia and Herzegovina
    ## 1   35ef5171-8952-4501-9399-0550abbcbb05                  Afghanistan
    ## 53  a956d116-3891-4618-a6bf-adef6dc696ae                        Egypt
    ## 162 83201a47-9c25-492b-9c49-e3f2b8d0eb3b                      Somalia
    ## 114 2f4f5caa-ce3c-4c13-bbc9-17c00bf76e6f                       Mexico
    ## 172 daa3785a-e816-4de0-bba7-f7a7a07ebfb0 Syrian Arab Republic (Syria)
    ## 138 12190cd7-6dde-4e5c-b35c-2f84d2020506                         Peru
    ## 167 f42adc40-955f-450b-b336-e7cf121ab5a1                        Sudan
    ## 193 bd554589-3c35-4aa4-9d6d-ddf2a2715c63                        Yemen
    ## 91  da42d401-61c3-4f10-b6fe-07d0a4a17122                Korea (North)
    ##     CountryCode                   Slug NewConfirmed TotalConfirmed NewDeaths
    ## 75           HN               honduras            0         426490         0
    ## 79           ID              indonesia            0        6076894         0
    ## 3            DZ                algeria            0         266025         0
    ## 35           TD                   chad            0           7424         0
    ## 73           HT                  haiti            0          31248         0
    ## 181          TN                tunisia            0        1046703         0
    ## 137          PY               paraguay            0         655532         0
    ## 104          MK              macedonia            0         313360         0
    ## 63           GM                 gambia            0          12002         0
    ## 106          MW                 malawi            0          86348         0
    ## 122          MM                myanmar            0         613538         0
    ## 27           BG               bulgaria            0        1169728         0
    ## 128          NE                  niger            0           9031         0
    ## 99           LR                liberia            0           7493         0
    ## 52           EC                ecuador            0         898667         0
    ## 23           BA bosnia-and-herzegovina            0         378413         0
    ## 1            AF            afghanistan            0         182033         0
    ## 53           EG                  egypt            0         515645         0
    ## 162          SO                somalia            0          26748         0
    ## 114          MX                 mexico        16133        5923086        24
    ## 172          SY                  syria            0          55920         0
    ## 138          PE                   peru            0        3608565         0
    ## 167          SD                  sudan            0          62551         0
    ## 193          YE                  yemen            0          11824         0
    ## 91           KP            korea-north            0              1         0
    ##     TotalDeaths NewRecovered TotalRecovered       Date FatalityRate
    ## 75        10904            0              0 2022-06-25   0.02556684
    ## 79       156711            0              0 2022-06-25   0.02578801
    ## 3          6875            0              0 2022-06-25   0.02584344
    ## 35          193            0              0 2022-06-25   0.02599677
    ## 73          837            0              0 2022-06-25   0.02678571
    ## 181       28670            0              0 2022-06-25   0.02739077
    ## 137       18963            0              0 2022-06-25   0.02892765
    ## 104        9322            0              0 2022-06-25   0.02974853
    ## 63          365            0              0 2022-06-25   0.03041160
    ## 106        2645            0              0 2022-06-25   0.03063186
    ## 122       19434            0              0 2022-06-25   0.03167530
    ## 27        37246            0              0 2022-06-25   0.03184159
    ## 128         310            0              0 2022-06-25   0.03432621
    ## 99          294            0              0 2022-06-25   0.03923662
    ## 52        35695            0              0 2022-06-25   0.03971994
    ## 23        15799            0              0 2022-06-25   0.04175068
    ## 1          7717            0              0 2022-06-25   0.04239341
    ## 53        24722            0              0 2022-06-25   0.04794384
    ## 162        1361            0              0 2022-06-25   0.05088231
    ## 114      325511            0              0 2022-06-25   0.05495632
    ## 172        3150            0              0 2022-06-25   0.05633047
    ## 138      213432            0              0 2022-06-25   0.05914595
    ## 167        4951            0              0 2022-06-25   0.07915141
    ## 193        2149            0              0 2022-06-25   0.18174899
    ## 91            6            0              0 2022-06-25   6.00000000

The worst fatality rates, meanwhile, seem to include not just generally
poorer countries, but many that are also war-torn.

Next, I want to create some charts. I will start with the simple
`plotCases` function to see what looks interesting. I’m going to choose
Australia, as it’s a developed country but also an island, and then
Canada.

``` r
plotCases("Australia")
```

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

``` r
plotCases("Canada")
```

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/unnamed-chunk-62-2.png)<!-- -->

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
# Create a scatter plot of win pct vs. shots per game.
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

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->
It is tough to see much here, since the outlier in red is skewing the
y-axis. I wanted to see which country that was, and also the one skewing
the x-axis (to a lesser degree).

``` r
head(covid[order(covid$TotalConfirmed),], 5)
```

    ##                                       ID                         Country
    ## 91  da42d401-61c3-4f10-b6fe-07d0a4a17122                   Korea (North)
    ## 6   fab4a32e-c3ac-4f76-9d6a-ed36545d96f6                      Antarctica
    ## 111 55ccbae4-befe-45ac-81b1-8f560d4aad29                Marshall Islands
    ## 74  ad201080-746e-45d6-b982-63dae82d4041   Holy See (Vatican City State)
    ## 115 85ac342a-f938-491f-80b8-6e8e64dbcc6b Micronesia, Federated States of
    ##     CountryCode                        Slug NewConfirmed TotalConfirmed
    ## 91           KP                 korea-north            0              1
    ## 6            AQ                  antarctica            0             11
    ## 111          MH            marshall-islands            0             18
    ## 74           VA holy-see-vatican-city-state            0             29
    ## 115          FM                  micronesia            0             38
    ##     NewDeaths TotalDeaths NewRecovered TotalRecovered       Date FatalityRate
    ## 91          0           6            0              0 2022-06-25            6
    ## 6           0           0            0              0 2022-06-25            0
    ## 111         0           0            0              0 2022-06-25            0
    ## 74          0           0            0              0 2022-06-25            0
    ## 115         0           0            0              0 2022-06-25            0

``` r
tail(covid[order(covid$TotalConfirmed),], 5)
```

    ##                                       ID                  Country CountryCode
    ## 65  43aba47a-2b6d-453e-aab5-0678d5d197cc                  Germany          DE
    ## 61  38621799-00eb-4e9a-9a13-e27de8eb0776                   France          FR
    ## 25  d9f816e3-d69a-4fe5-9217-a67d4e7b2341                   Brazil          BR
    ## 78  c43d0d7d-785c-4076-a70a-6ef238b0972e                    India          IN
    ## 187 963f2cac-5704-4105-b8e5-69883304c62c United States of America          US
    ##              Slug NewConfirmed TotalConfirmed NewDeaths TotalDeaths NewRecovered
    ## 65        germany        89336       27771111        84      140734            0
    ## 61         france            0       30714200         0      150356            0
    ## 25         brazil        60384       32023166       334      670229            0
    ## 78          india        15940       43378234        20      524974            0
    ## 187 united-states       152095       86909716       447     1015789            0
    ##     TotalRecovered       Date FatalityRate
    ## 65               0 2022-06-25  0.005067640
    ## 61               0 2022-06-25  0.004895325
    ## 25               0 2022-06-25  0.020929505
    ## 78               0 2022-06-25  0.012102245
    ## 187              0 2022-06-25  0.011687865

If we rank the data by number of confirmed cases (cumulative), we can
see that the outlier in red on the scatter plot is there because North
Korea only had one confirmed case, but somehow had six deaths. And the
outlier with the incredibly high case count? That’s the United States,
with almost 87 million as of late June 2022. I plotted the same thing
once more, but left out North Korea.

``` r
# Create a scatter plot of win pct vs. shots per game.
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

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

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
    ## 1 Africa        54      4720.        992     222555.      60976            0 
    ## 2 Americas      35     78750.       5726    4599341.     525539         6915.
    ## 3 Antarctica     1         0           0         11          11            0 
    ## 4 Asia          48     30082.       8912.   3249358.     859694          766.
    ## 5 Europe        45     41216.      13822    4513956.    1443637         5304.
    ## 6 Oceania       12      1068.         21.5   788880.      13446.        2762.
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

![](/Users/claudialdonahue/Documents/00%20MOR/ST%20558%20Data%20Science%20for%20Statisticians/Project%201/ST558-project-1/README_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

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
