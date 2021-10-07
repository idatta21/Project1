COVID19 API
================
Ipsita Datta
9/28/2021

     This document is a vignette to show how to retrieve data from Covid19API.To demonstrate, I'll be interacting with the Covid19 API. I am going to build a few functions to interact with some of the endpoints and explore some of the data I can retrieve.

– The most difficult part of the logic and programming for me is to make
a requirement or what I should analysis and how I am going to implement
that.

– In,future, First I will sketch a project outline like what are the
things is going to discuss or what are the data I should fetch from API.

# Requirements

To use the functions for interacting with the API, I used the following
packages:

-   [`tidyverse`](https://www.tidyverse.org/): useful features for data
    manipulation and visualization
-   [`jsonlite`](https://cran.r-project.org/web/packages/jsonlite/): API
    interaction
-   [`httr`](https://cran.r-project.org/web/packages/httr/index.html):
    For using function to interact with API

# API Interaction Functions

Here are the functions to interact with Covid19 API.

## `Country`

I wrote this function to interact with the `Country` endpoint of the
Covid19 API.It returns all countries and associated provinces. The
country\_slug variable is used for country specific data

``` r
Country <-function(CountryName ="all"){
    #Returns all the available countries and provinces, as well as the country slug for per country requests.
  outputAPI<-GET("https://api.covid19api.com/countries")
  output<- outputAPI$content %>% rawToChar() %>% fromJSON()
  if ( CountryName != "all"){
         #if country name is in country
         if (CountryName %in% output$Country){
         # subset output for just that row.
         output<-output %>% filter(Country == CountryName)
         }
         # if country code in country slug
         else if (CountryName %in% output$Slug ){
          output<-output %>% filter(Slug == CountryName) 
         }
         # if country name is in countrycode/ISO2
         else if (CountryName %in% output$ISO2 ){
          output<-output %>% filter(CountryCode == CountryName) 
         }
  }
    # Return the output data.frame.
  
  return(output)
}
# for ex: Country() or Country("united-states") 
```

## `C_summary`

This function returns current cases for all the country and can also
return for single country if name passed as argument.

``` r
C_summary<-function(CountryName ="all"){
         ## Get all the number of cases from the summary endpoint.
         outputAPI<-fromJSON("https://api.covid19api.com/summary")
         output<-outputAPI$Countries
         if ( CountryName != "all"){
         #if country name is in country
         if (CountryName %in% output$Country){
         # subset output for just that row.
         output<-output %>% filter(Country == CountryName)
         }
         # if country name is in countrycode
         else if (CountryName %in% output$CountryCode ){
          output<-output %>% filter(CountryCode == CountryName) 
         }
         # if country code in country slug
         else if (CountryName %in% output$Slug ){
          output<-output %>% filter(Slug == CountryName) 
         }
          # Otherwise, throw an informative error.
          else {
          message <- paste("ERROR: Argument for country was not found in either","the Country or CountryCode or Slug columns. Try Countries('all') to","find the CountryName you're looking for.")
      stop(message)
          }
}
          # Do nothing if the Country value equals "all".
else {
    
     }
        # Return the output data.frame.
        return(output)
}

#for ex: C_summary() or C_summary("united-states")
```

## `USAdetail`

Returns all live cases for All provinces of USA. It can be use for a
particular province with particular case type of USA too.These records
are pulled every 10 minutes and are ungrouped. Country must be the slug
from /countries or /summary. Cases must be one of: Deaths,Confirmed
,Active,Recovered

``` r
USAdetail<-function(ProvinceName ="all",type  ){
    baseurl<-"https://api.covid19api.com/live/country/united-states"
     output<-fromJSON(baseurl)
    # If Province does not equal "all", check if it is a Province .
        if (ProvinceName != "all" ){
           # If Province is in the Province column, subset output for just that row.
             if (ProvinceName %in% output$Province){
                 output <- output %>%
                           filter(ProvinceName == Province) %>%
                   select(all_of(type),Province)
            }
            # Otherwise, warn the user and return the entire dataframe.
            else {
            message <- paste("WARNING: Argument for Province was not found in either",
                       "the Province  columns. Returning all",
                       "or type should be Deaths,Confirmed ,Active,Recovered.")
           warning(message)
            }
          return(output)
         }
        # Do nothing if the province value equals "all".
        else {# Return the output data.frame.
              return(output)
              }
}
#for ex: USAdetail() or USAdetail("North Carolina","Active")
```

## `CountryDatabycase`

Get List Of Cases Per Country By Case Type From The First Recorded
Case.Returns all cases by case type for a country from the first
recorded case. Country must be the country\_slug from /countries. Cases
must be one of: confirmed, recovered, deaths or Active.

``` r
CountryDatabycase<-function(Country ="all",Status){
if (Country != "all"){
baseurl<-"https://api.covid19api.com/dayone/country/"
path1<-paste0(baseurl,Country)
endpoints<-"/status/"
fullurl<-paste0(path1,endpoints,Status)
outputAPI <-fromJSON(fullurl)
return(outputAPI)
}
  else{message <- paste("ERROR: Argument for country was not found in  Country ")
      stop(message)
  }
}
#for ex: CountryDatabycase("south-africa","deaths")
```

## `Allcasesbycountry`

Returns all cases by case type for a country from the first recorded
case. Country must be the Slug from /countries or /summary. Cases must
be one of: confirmed, recovered, deaths or Active

``` r
Allcasesbycountry<-function(Country ="all" ,type){
if (Country != "all"){
baseurl<-"https://api.covid19api.com/dayone/country/"
fullurl<-paste0(baseurl,Country)
outputAPI<-fromJSON(fullurl)
output<- outputAPI %>% select(Country,type)
return(output)
}
else{message <- paste("ERROR: Argument for country was not found in  Country ")
      stop(message)
}
}
#For ex: Allcasesbycountry("Switzerland","Confirmed")
```

## `LiveCount`

Returns all cases by case type for a country from the first recorded
case with the latest record being the live count. Country must be the
Slug from /countries or /summary. Cases must be one of: confirmed,
recovered, deaths

``` r
Livecount<-function(Country="all",type){
if (Country !="all"){
baseurl<-"https://api.covid19api.com/dayone/country/"
path1<-paste0(baseurl,Country)
fullurl<-paste0(path1,"/status/",type,"/live")
outputAPI<-fromJSON(fullurl)
return(outputAPI)
}
  else{message <- paste("ERROR: Argument for country was not found in  Country ")
      stop(message) }
}
#Livecount("iran","confirmed")
```

# Data Exploration

Now that we can interact with a few of the endpoints of the Covid19 API,
let’s get some data from them.<br>

First, let’s pull A function returns current cases for all the country
along with global cases

``` r
# Get the current cases for all of the country .
currentData<- C_summary()
```

Second, let’s pull a function returns cases by type for USA provinces.

``` r
#Get all the cases of unites-states.
currentData2<-USAdetail("Utah","Deaths")
currentData3<-USAdetail("Utah","Recovered")
Data<-cbind(currentData2,currentData3)
```

Now two variable of interest for me are detahrate and newconfirmed rate
per countries.

``` r
# Add a column for the death per 10000
currentData1 <- currentData %>%
  mutate(deathRate = round((NewDeaths*10000 )/ TotalDeaths,0),NewconfirmRate=round((NewConfirmed*10000 )/ TotalConfirmed,0)) %>% select (Country,deathRate,NewconfirmRate)
df<-as.data.frame(currentData1)

# Create a Bar Plot of deathRate per Country( last 10 country from the dataset).
plot1<-  ggplot(tail(df,30),aes(x=Country,y=deathRate,fill=deathRate)) 
 plot1+ geom_col() + 
  #  remove the legend.
  theme(axis.text.x=element_text(angle=90), legend.position="none") +
  # Set the axes labels.
  scale_x_discrete("Country") + 
  scale_y_continuous("DeathRate") +
  # Add a title.
  ggtitle("Last 10 country death rate from Current data") 
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# Create a scatter Plot of death rate vs NewconfirmRate of Countries
plot2<-df %>% ggplot(aes(x=NewconfirmRate,y=deathRate)) 
plot2+geom_point() + 
  theme(axis.text.x=element_text(angle=90), legend.position="none") +
  scale_x_continuous("NewconfirmRate") + 
  scale_y_continuous("deathRate") +
  ggtitle(" death rate vs NewconfirmRate of countries ")
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Make a box plot of comfirmrate deathrate  of  countries.

a<-data.frame(group ="deathrate",value =df$deathRate)
b<-data.frame(group ="comfirmrate",value=df$NewconfirmRate)
plotData<-rbind(a,b)
g<-ggplot(plotData,aes(x=group,y=value,fill=group)) 
  g+geom_boxplot() + coord_cartesian(ylim=c(0,225))+
  ggtitle("comfirmrate vs deathrate accorss  countries ") 
```

    ## Warning: Removed 7 rows containing non-finite values
    ## (stat_boxplot).

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#Create a histogram of Newconfirmrate of  countries from data set.
g<-ggplot(df,aes(x=NewconfirmRate))
g+geom_histogram(binwidth = 30,color="blue",fill="red")+
  ggtitle("histogram of Newconfirmrate of  countries ")
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> \# numerical
summaries for all the countries

``` r
# It shows the numerical summeries on TotalConfirmed for all the countries
currentData %>% summarise(avg=round(mean(TotalConfirmed),0),sd=round(sd(TotalConfirmed),0),median=round(median(TotalConfirmed),0),IQR=round(IQR(TotalConfirmed),0))
```

    ##       avg      sd median    IQR
    ## 1 1227497 4429359 162816 616545
