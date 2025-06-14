---
title: TongFen
author: Jens von Bergmann
date: '2020-11-10'
slug: tongfen
categories:
  - tongfen
  - CensusMapper
  - cancensus
tags: []
description: "Tongfen is now on CRAN, time for a short overview of what tongfen is and how it aids research on longitudinal spatial data on different yet congruent geographies."
featured: ''
image: "https://raw.githubusercontent.com/mountainMath/tongfen/master/images/tongfen-sticker.png"
featuredalt: ""
featuredpath: ""
linktitle: ''
type: "post"
aliases:
  - /blog/2020/11/10/tongfen/
---







The [tongfen R package](https://mountainmath.github.io/tongfen/index.html) is now [on CRAN](https://cran.r-project.org/web/packages/tongfen/index.html), so it's time for an overview post. Tongfen has changed a bit since it's inception and is now a lot more flexible but slightly more abstract to use.

## What is tongfen?
Tongfen, 通分 in Chinese, generally denotes the process of bringing two fractions onto the least common denominator. This is akin to the problem of making data on different but congruent geographies comparable by finding a least common geography. We will describe the process in detail later, intuitively imagine this as a puzzle game where two different tilings are to be matched up by joining individual areas in each of the tilings until both match. One way to achieve this is to join all areas in each of the two tilings, so it's always possible to do this. This is analogous to mutiplying the denominators in two fractions to find a common denominator. To be useful, the matching should to be done in a way that minimizes the number of joined individual areas, resulting in as fine a geography as possible, the *least common geography*.

In practice this matching may be done up to a pre-specified tolerance to allow for slight boundary changes that are simply due to improvements of accuracy in delineating geographic regions as is commonly found in e.g. census data.


## Why tongfen?
Tongfen is our answer to the problem of needing to compare data on different yet congruent geographies. The most common setting for this is census data across several censuses, where census geographies change. Another setting is polling district data, where polling districts get adjusted over time. These geography changes typically happen via a sequence of fairly localized combine and split operations to get from one geography to another.

There are essentially three different approaches on how to deal with the problem of harmonizing data on congruent geographies:

### Custom tabulation
If possible, for example when working with census data, we can request a custom tabulation of the data on the geography we desire. In some cases this isn't possible, for example when working with polling district level voting data. But when possible, this is often the best method to harmonize the data. 

However, in some cases it won't lead to satisfactory results. Disclosure cutoffs are coarser for custom tabulations, which may result in suppressed data when working with small areas. With the recognition that traditional privacy measures like random rounding are insufficient to protect user privacy, and e.g. the US census switching over to differential privacy as the release mechanism, custom tabulations don't just cost time and money, but also eat into the researcher's allotted privacy budget. This makes alternative methods more attractive. 

There have been some efforts to create publicly available custom tabulation that span several censuses, for example one for [Metro Vancouver and Toronto on 2016 dissemination areas back to 1971](https://dataverse.scholarsportal.info/dataset.xhtml?persistentId=doi:10.5683/SP2/QNO5JG) in Canada and the [LTDB-DP project for US census data on 2010 census tracts](https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTDB.htm) using differential privacy.

### Area weighted (dasymetric) interpolations
These methods try to harmonize the data by computing geographic intersections of the two geometries and estimating values of interest from one geometry to another proportionally to the relative overlap. This is often refined by folding in finer level data, for example block level population counts or by clipping out uninhabited areas. Such an approach was taken e.g. by the [Canadian Longitudinal Census Tract Database](https://onlinelibrary.wiley.com/doi/10.1111/cag.12467) or the US [Neighbourhood Change Database (NCDB/Geolytics)](https://geolytics.com/products/normalized-data/neighborhood-change-database), [LTDB](https://s4.ad.brown.edu/Projects/Diversity/Researcher/Bridging.htm), or [NHGIS](https://www.nhgis.org/about). 

The main advantage of this method is that it is very simple to implement and places no restrictions on the (target) geography to estimate the data on. The tongfen package implements this using the `tongfen_estimate` method, with the option for dasymetric refinements via the `proportional_reaggregate` method.

The main disadvantage of these methods is that they are only estimates and come with [sizeable errors](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5472362/). And the errors are generally correlated with changes in population density or change in other variables that are important to how the geographies are broken down. Additionally, we have not seen a good way to estimate these errors (something we am interested in and might spend some time thinking about in the future). This makes this approach **ill-suited for research**, although that does not seem to stop a lot of academics from doing exactly that. This is particularly true for the NCDB/Geolytics data because the their methods are opaque and [their errors are especially large](https://www.tandfonline.com/doi/full/10.1080/24694452.2016.1187060).

### Tongfen
If there is some flexibility in the geography on which the harmonized data should be presented and the geographies in question are sufficiently congruent (as is often the case with census geographies or polling districts) then tongfen can provide exact (at the same precision as the original data) counts on a common geography that is (slightly) coarser than the original geographies. 

The advantage over custom tabulations is that it is immediately available and does not cost money or privacy budget. The disadvantage is a loss of geographic granularity, the degree of which is dependent on how congruent the original geographies are. 

The advantage over area weighted interpolation is that the counts are exact and not estimates, and they can thus be used for research purposes. However, some quantities, like medians, can't be aggregated up this way and can only be estimated. The disadvantage is that tongfen requires flexibility on the target geography and only provides useful results when the geographies are sufficiently congruent.

Another advantage of tongfen is that it is not precomputed but carried out live as needed. This makes the process reproducible, auditable, and transparent, and allows the user to fine-tune the process according to their needs.

## How does tongfen work?
Abstractly tongfen works in three steps:
1. Build metadata that makes it possible to aggregate variables of interest. For variables that are additive, like population counts, this is simple and no extra information is needed. For other variables, like average income, we also need a count of the "base population" that the average is based on so it can be properly aggregated. The metadata assembles all required variables and information on how they relate so we can properly aggregate the information when needed.
2. Build a correspondence between the geographies of interest. This is similar to the information provided by e.g. Statistics Canada or the US Census Bureau in their *correspondence files*. Tongfen implements several methods to do this, one that's completely agnostic of external information and just operates on the geometries, another that assumes that geographies with identical UID correspond to one another and only matches the remaining geographies, and lastly one that uses external correspondence data.
3. Aggregate up geometries according to the correspondence and data according to the correspondence and metadata.

We will showcase a couple of simple examples on how this process works.

### Tongfen using polling district data
The [tongfen R package](https://mountainmath.github.io/tongfen/index.html) implements several methods to find the least common geography and aggregate up variables. As an example, consider the Canadian federal electoral poll districts from the 2015 and 2019 elections for Vancouver.


<img src="index_files/figure-html/vote-results-1.png" width="960" />

To compare these and compute vote shifts at a granular level we need to get the data on a common geography.

<img src="index_files/figure-html/tongfen-comparison-1.png" width="960" />

On the left we show the 2015 boundaries, in the middle the 2019 boundaries and on the right the least common geography based on the former two. This example nicely illustrates the strength and weaknesses of tongfen. On the east side the tongfen process results in a fairly granular tiling, but on the west side the tiling ends up very coarse to the point where it's almost identical to the entire federal election district of Vancouver-Quadra.

To understand how tongfen works in more detail we zoom into the area marked by the red square.

<img src="index_files/figure-html/tongfen-cutout-1.png" width="960" />

Here we can see our "puzzle game" to find a common tiling at work. Some areas remain unchanged between the years and can simply be adopted without modifications. But other areas don't match and the puzzle game starts how to best join adjacent areas to build a least common tiling.

On close inspection we also notice some insignificant boundary changes where boundaries have been modified over time to better align with features like roads. The tongfen algorithm requires to specify a tolerance up to which boundary changes are consider to be inconsequential. In this particular example the tolerance was set to be 30 metres. This is informed by the observation that polling district boundaries follow the street grid, and a change by less than 30 metres allows for realignments with streets but will detect cases where the boundaries follow a different street.

The end process shows the strength and limitations of tongfen. On the west side, where geographies changed a lot, tongfen results in a tiling that is close to the union of all areas. On the east side tongfen is able to maintain a fine geographies.

One advantage of not pre-computing the matching but doing it live as needed is that we can refine the computations by incorporating local data. To get a finer tiling we can look at Vancouver land use data and cut the electoral polling districts down to only those areas where people actually live. This can help to further weed out inconsequential boundary changes

<img src="index_files/figure-html/landuse-mask-1.png" width="768" />

Masking the polling district boundaries with the land use area helps us filter out inconsequential boundary changes. 

<img src="index_files/figure-html/dasymetric-tongfen-1.png" width="960" />

The TongFen geography is based on the 2019 geographies, with the correspondence built from the geographies that have been intersected with the residential land use. This yields slightly more detail on the west side where geographies had previously combined but the boundary changes were inconsequential where they covered unpopulated areas.

<img src="index_files/figure-html/ca-vote-shift-1.png" width="960" />

This only considers votes at the polling station and ignores advance and mail-in ballots. There is a [tongfen package vignette](https://mountainmath.github.io/tongfen/articles/polling_districts.html) that also looks into changes in advance voting patterns, a detailed analysis goes beyond the scope of this overview post.

## Canadian census data
The tongfen functionality can be easily extended to facilitate working with particular data sources. Integration with Canadian census data is most advanced, leveraging our [cancensus R package](https://mountainmath.github.io/cancensus/index.html) that taps into the [CensusMapper API](https://censusmapper.ca/api) and rich metadata for census variables from CensusMapper.

<img src="index_files/figure-html/ca-census-education-1.png" width="960" />




## T1FF taxfiler data
Thanks to a CMHC project CensusMapper now also has census tract level T1FF taxfiler data available. T1FF taxfiler data is an [extremely rich annual dataset](https://doodles.mountainmath.ca/blog/2020/04/23/census-tract-level-t1ff-tax-data/) with detailed demographic and income variables, making it very suited for longitudinal analysis at fine geographies.

We look at the share of people in low income (LICO-AT) in the City of Toronto between 2005 (the first year our CRA tax data has low income counts) and 2018. The first step is to collect the low income variables, a quick check on the [CensusMapper API UI](https://censusmapper.ca/api) reveals that the relevant internal CensusMapper vectors are `v_TX2018_551` for the 2018 percentage of population in low income, with the year string swapped out for earlier years. CensusMapper variable coding is not always consistent through time, but it is for the tax data.

As a first step we construct the necessary metadata needed for tongfen. The percentage itself is not enough information to aggregate up the data, we also need to know the base of the percentage -- population in this case. CensusMapper metadata knows what variable that is and the `meta_for_ca_census_vectors` convenience function takes care of assembling any extra variables we need for tongfen. The metadata specifies the variables that are to be considered, and also specifies rules on how they are to be aggregated.

Here is the metadata just for the variables for the 2018 year.


|variable     |label        |dataset |type     |aggregation             |units              |rule     |parent       |geo_dataset | year|
|:------------|:------------|:-------|:--------|:-----------------------|:------------------|:--------|:------------|:-----------|----:|
|v_TX2018_551 |lico_2018    |TX2018  |Original |Average of v_TX2018_546 |Percentage (0-100) |Average  |v_TX2018_546 |CA16        | 2018|
|v_TX2018_546 |v_TX2018_546 |TX2018  |Extra    |Additive                |NA                 |Additive |NA           |CA16        | 2018|

Armed with the metadata we need to select the region we are interested in, the City of Toronto in this case, and call the `get_tongfen_ca_census` wrapper function that downloads the data, harmonizes the geographies, aggregates up the data and hands back the resulting data on a uniform geography.



<img src="index_files/figure-html/toronto-lico-all-years-1.png" width="960" />

For this plot of low income shares over the years we would not have to go though the troubles of harmonizing the geographies, we could have just plotted each dataset on it's native geography. This would have resulted in a slightly higher geographic resolution for each of the maps. The key advantage of having the data on a unified geography is that we can now easily look at change over time, for example by mapping the percentage point change between the initial and final years.

<img src="index_files/figure-html/toronto-lico-1.png" width="960" />


## US census tract level data
The tongfen package also comes with basic support for US census tract level cross-census comparisons. This piggy-backs off of the [tidycensus package]() to ingest census data. As an example we look at the change in household size in the Bay Area. The first step is to build the metadata, which we do by hand.


<img src="index_files/figure-html/us-census-households-1.png" width="960" />

The census tracts used here are slightly coarser than the original 2000 and 2010 census tracts, but the resulting data is exact, in the sense in which the 2000 and 2010 census data is exact, and not estimates or noised via differential privacy.

## Upshot
TongFen is not new, people have been assembling regions before to make them comparable. In particular, Statistics Canada as well as the US Census Bureau provide *correspondence files* that specify how geographic regions relate across censuses.  But we have not seen implementations that automate this process and provide the flexibility to run this on arbitrary geographies in a reproducible and transparent way.

Tongfen is designed so it is easy to build dataset-specific wrappers to facilitate working with specific datasets.
Currently tongfen has wrappers to work with Canadian and US census data, with the Canadian version being fairly well-developed. We welcome contributions that wrap other datasets or expand the usability or coverage of tongfen on existing datasets.

As usual, the code for this post is [available on GitHub](https://github.com/mountainMath/doodles/blob/master/content/posts/2020-11-10-tongfen.Rmarkdown) in case others find it useful. The examples here are similar to the [vignettes in the tongfen package](https://mountainmath.github.io/tongfen/index.html).


<details><summary>Reproducibility receipt</summary>

```
## [1] "2020-11-11 07:24:37 PST"
```

```
## Local:    master /Users/jens/Google Drive/R/mountaindoodles
## Remote:   master @ origin (https://github.com/mountainMath/doodles.git)
## Head:     [b174751] 2020-11-11: tongfen post
```

```
## R version 4.0.2 (2020-06-22)
## Platform: x86_64-apple-darwin17.0 (64-bit)
## Running under: macOS Catalina 10.15.7
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] mountainmathHelpers_0.1.2 sf_0.9-6                 
##  [3] tongfen_0.3               cancensus_0.3.3          
##  [5] forcats_0.5.0             stringr_1.4.0            
##  [7] dplyr_1.0.2               purrr_0.3.4              
##  [9] readr_1.4.0               tidyr_1.1.2              
## [11] tibble_3.0.4              ggplot2_3.3.2            
## [13] tidyverse_1.3.0          
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.1.0   xfun_0.18          haven_2.3.1        colorspace_1.4-1  
##  [5] vctrs_0.3.4        generics_0.1.0     htmltools_0.5.0    yaml_2.2.1        
##  [9] blob_1.2.1         rlang_0.4.8        e1071_1.7-4        pillar_1.4.6      
## [13] glue_1.4.2         withr_2.3.0        DBI_1.1.0          dbplyr_1.4.4      
## [17] modelr_0.1.8       readxl_1.3.1       lifecycle_0.2.0    munsell_0.5.0     
## [21] blogdown_0.19      gtable_0.3.0       cellranger_1.1.0   rvest_0.3.6       
## [25] evaluate_0.14      knitr_1.30         class_7.3-17       fansi_0.4.1       
## [29] broom_0.7.0        Rcpp_1.0.5         KernSmooth_2.23-17 classInt_0.4-3    
## [33] scales_1.1.1       backports_1.1.10   jsonlite_1.7.1     fs_1.4.1          
## [37] hms_0.5.3          digest_0.6.27      stringi_1.5.3      bookdown_0.19     
## [41] grid_4.0.2         cli_2.1.0          tools_4.0.2        magrittr_1.5      
## [45] crayon_1.3.4       pkgconfig_2.0.3    ellipsis_0.3.1     xml2_1.3.2        
## [49] reprex_0.3.0       lubridate_1.7.9    assertthat_0.2.1   rmarkdown_2.3     
## [53] httr_1.4.2         rstudioapi_0.11    R6_2.5.0           git2r_0.27.1      
## [57] units_0.6-7        compiler_4.0.2
```
</details>

