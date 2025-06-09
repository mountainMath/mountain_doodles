---
title: Rental Data
author: Jens von Bergmann
date: '2017-09-12'
slug: rental-data
categories:
  - cancensus
  - cmhc
tags: []
draft: true
description: "Trying to Make Sense of What we Have"
#featured: 'vancouver_fsr.png'
#images: ["https://doodles.mountainmath.ca/images/vancouver_fsr.png"]
#featuredalt: ""
#featuredpath: "/images"
linktitle: ''
type: "post"
aliases:
  - /blog/2017/09/12/rental-data/
---




Rental data is an important ingredient to understanding the housing market. Unfortunately we, or maybe just I, don't have good data on this. And the little data we have we, or maybe just I, don't understand well. So I spent some time to make sense of what we got. Here is what I learned.

## Rental Data
Ideally we would like to know what people pay for rent, where they live, how much money they make, when they first moved in and signed the contract, the term of their contract, how many bedrooms and what size they live in, what kind of building they live in, their income, ...

Sadly we don't know these things, at least not at the individual tenant level. But we have various sources that tell differnt parts of the story.

### Census
The census comes closest in allowing us to cross-tabulate most of these variables, but it is only administred once every five years (and is only a ~1/4 subsample). Data for housing from the 2016 census won't get released until October 25. 

### CMHC
CMHC has several surveys that can help us figure out what happens in between the censuses. They keep track of the size and makeup of the rental market, for example tracking purpose built rental construction and the purpose built rental stock, subsidized rentals and "secondary market" rentals. They also keep track of vacancy rates and rents in the purpose-built and secondary markets. Their data comes with a consisent methodology, but varying timelines, geographic resoltion, somtimes information on building type and number of bedrooms, and varying quality of the data. CMHC rental data measures "stock" rents, so average or median rents. 

### Rental Listing Services
Rental listing services aren't strictly speaking a data source, but they can become one if one keeps track of the listings that are posted. This can be done manually or one can try to automate the "scraping" of these listings. These data sources exist, but the legality of "scraping" is not entirely clear, which means people are generally not willing to share data obtained this way and casues friction in the effort to understand the rental market.

Data obtained this way measures "turnover" rents, so the amount people pay at the time they sign the lease. 

## Comparing Sources
Each of these sources comes with it's own advantages and limitations. To understand these, let's check how these different sources relate.


## Part 1: Rental Stock

We divide the rental stock into 4 main categories:

1. Purpose Built Rental (Primary Market)
2. Private Rental (Secondary Market)
3. Subsidized Rental
4. Vacant Rental

Using CMHC and census data we can get a good estimate how large each of these segments are.


```r
library(cancensus)
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(cmhc)

rental_data <- function(dataset,level,name){
regions=list_census_regions(dataset,use_cache = TRUE,quiet=TRUE) %>% 
  filter(level==!!level,name==!!name) %>% as_census_region_list
vectors=c("v_CA11N_2252","v_CA11N_2253","v_CA11N_2254","v_CA11N_2255","v_CA11N_2289")
census_data <- get_census(dataset=dataset, regions=regions, vectors=vectors, level='Regions', labels="short") %>%
  mutate(tenure=v_CA11N_2252, owner=v_CA11N_2253, renter=v_CA11N_2254, band=v_CA11N_2255, subsidized=round(v_CA11N_2289/100*renter))

cmhc_stock_params=cmhc_timeseries_params(table_id = cmhc_table_list["Rms Rental Universe Time Series"], region = cmhc_region_params(name,level))               
cmhc_vacancy_params=cmhc_timeseries_params(table_id = cmhc_table_list["Rms Vacancy Rate Time Seris"], region = cmhc_region_params(name,level)) 
cmhc_secondary_vacancy_params=cmhc_timeseries_params(table_id = cmhc_table_list["Srms Vacancy Rate Time Seris"], region = cmhc_region_params(name,"CMA"))  # can't get CSD level vacancy rates

cmhc_stock_data <- get_cmhc(cmhc_stock_params)  %>% rename(Date=X1) %>% select(-X2) 
cmhc_primary_vacancy_data <- get_cmhc(cmhc_vacancy_params) %>% rename(Date=X1)
cmhc_secondary_vacancy_data <- get_cmhc(cmhc_secondary_vacancy_params) %>% rename(Date=X1)

pb_stock=filter(cmhc_stock_data,grepl("^2011.*$",Date))$Total
primary_vacancy=filter(cmhc_primary_vacancy_data,grepl("^2011.*$",Date))$Total/100
secondary_vacancy=filter(cmhc_secondary_vacancy_data,grepl("^2011.*$",Date))$Total/100
all=census_data$renter
primary_stock=round(pb_stock / (1-primary_vacancy))
secondary_stock=round((all-primary_stock-census_data$subsidized)/(1-secondary_vacancy))
subsidized_stock=round(census_data$subsidized / (1-primary_vacancy))
vacant_stock=round((census_data$subsidized+primary_stock)*primary_vacancy+secondary_stock*secondary_vacancy)
df <- tibble(type=c("Purpose Built Rental","Subsized Rental","Private Investor Rental"),count=c(primary_stock,subsidized_stock,secondary_stock))
return (df)
}

df <- rental_data("CA11","CSD","Vancouver")
```

```
## Warning in list_census_regions(dataset, use_cache = TRUE, quiet = TRUE):
## Cached regions list may be out of date. Set `use_cache = FALSE` to update
## it.
```

```
## Reading vectors data from local cache.
```

```
## Warning in strptime(x, fmt, tz = "GMT"): unknown timezone 'default/America/
## Vancouver'
```

```r
total=sum(df$count)
df$ratio=df$count/total
```



## Part 2: Stock Rents

## Part 3: Turnover Rents

## Part 4: Vacancy Rates and Stock Rents

## Part 5: Stock Rents vs Turnover Rents

We start with CMHC and census data, both of these are freely available and both measure "stock" rents. We have convenient access to census data using our [`cancensus` package](https://github.com/mountainMath/cancensus) and CMHC data using our [`cmhc` package](https://github.com/mountainMath/cmhc).


```r
#devtools::install_github("mountainmath/cancensus")
library(cancensus)
#devtools::install_github("mountainmath/cmhc")
library(cmhc)
```



Let's read in the historical average rent data from CMHC.

```r
prep <- function(data) {
  variables=c("Bachelor","1 Bedroom","2 Bedroom","3 Bedroom +","Total","Single","Semi / Row / Duplex","Other- Primarily Accessory Suites")
  data$Date=as.integer(sub("\\s.+$","",data$X1))
  return(data %>%  select(c("Date","name",intersect(names(data), variables)))) 
}

get_median_rent_data <- function(places,geography_type){
  
  average_primary_rents <- do.call(rbind,lapply(places, function(place_name){
    get_cmhc(cmhc_timeseries_params(table_id = "2.2.11", region=cmhc_region_params(geography = place_name, type=geography_type))) %>%
      mutate(name=place_name)
  })) %>% prep
  average_condo_rents <- do.call(rbind,lapply(places, function(place_name){
    get_cmhc(cmhc_timeseries_params(table_id = "4.4.2", region=cmhc_region_params(geography = place_name, type=geography_type))) %>%
      mutate(name=place_name)
    })) %>% prep
  average_other_secondary_rents <- do.call(rbind,lapply(places, function(place_name){
    get_cmhc(cmhc_timeseries_params(table_id = "4.6.2", region=cmhc_region_params(geography = place_name, type=geography_type))) %>%
    mutate(name=place_name)
    })) %>% prep

plot_data <- do.call(rbind,list(
  average_primary_rents %>% rename(`Total Primary` = Total) %>% melt(id=c("Date","name")), 
  average_condo_rents %>% 
    rename(`Bachelor Condo`=`Bachelor`,
           `1 Bedroom Condo`=`1 Bedroom`,
           `2 Bedroom Condo`=`2 Bedroom`,
           `3 Bedroom + Condo`=`3 Bedroom +`,
           `Total Condo` = Total) %>% 
    melt(id=c("Date","name")), 
  average_other_secondary_rents %>% rename(`Total Other` = Total) %>% melt(id=c("Date","name"))
  ))
return(plot_data)
}
```

And plot the time series.

```r
places=c("Vancouver","Toronto","Calgary","Victoria")
geography_type="CMA"

plot_data <- get_median_rent_data(places,geography_type)
ggplot(plot_data %>% arrange(Date) %>% filter(name=="Vancouver"), 
       aes(x=Date, y=value, colour=variable)) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(labels=currency_format) +
  labs(title="Vancouver Stock Median Rents", x="Year", y="Monthly Rent")
```

<img src="index_files/figure-html/unnamed-chunk-5-1.png" width="960" />


Looking at 1 bedroom rents across the cities we get

```r
ggplot(plot_data %>% filter(variable %in% c("1 Bedroom","1 Bedroom Condo")) %>% 
       mutate(type=paste0(name," ",variable)) %>%
       arrange(Date,type), 
       aes(x=Date, y=value, colour=type)) + 
  geom_line() +
  geom_point() +
  scale_color_brewer("Metro / Unit Type",palette = 'Paired') +
  scale_y_continuous(labels=currency_format) +
  labs(title="1 Bedroom CMA Stock Median Rents", x="Year", y="Monthly Rent")
```

<img src="index_files/figure-html/unnamed-chunk-6-1.png" width="960" />

With a similar patter for 2 bedroom rent

```r
ggplot(plot_data %>% filter(variable %in% c("2 Bedroom","2 Bedroom Condo")) %>% 
       mutate(type=paste0(name," ",variable)) %>%
       arrange(Date,type), 
       aes(x=Date, y=value, colour=type)) + 
  geom_line() +
  geom_point() +
  scale_color_brewer("Metro / Unit Type",palette = 'Paired') +
  scale_y_continuous(labels=currency_format) +
  labs(title="2 Bedroom CMA Stock Median Rents", x="Year", y="Monthly Rent")
```

<img src="index_files/figure-html/unnamed-chunk-7-1.png" width="960" />

We can narrow it down to just the cities (census subdivisions).



