---
title: "Population timelines update"
author: 
  - name: Jens von Bergmann
    affiliation: MountainMath
date: '2024-04-23'
date-modified: '2024-07-26'
slug: population-timeline-updates
categories:
  - Vancouver
  - Toronto
  - density
  - tongfen
description: 'A long overdue update on population timelines for Vancouver and Toronto, adding in 2021 data for a 50 year fine geography population change timeline 1971 through 2021.'
image: "index_files/figure-html/toronto_pop_change_1971_2021-1.png"
bibliography: ../../common_literature.bib 
code-tools:
  toggle: true
fig.width: 8
execute:
  cache: true
  message: false
  warning: false
---





Several years ago we did a post on long population timelines in Vancouver and Toronto. [@census-custom-timelines-2019] The underlying data came from a semi-custom tabulation of 1971 through 2011 census profile data on a uniform 2016 geography [@statcan_census_1971-2011], which then could easily be joined with 2016 census data to map long timelines at fine geographies. For Vancouver we further refined this by mixing in [Metro Vancouver Land Use data](https://open-data-portal-metrovancouver.hub.arcgis.com/datasets/28de9170a9434974bffc24c119261310_1/explore?location=49.263389%2C-123.037293%2C15.72) to cut out underpopulated areas and make the map visually more appealing.

Since then we got new data from the 2021 census, and the Metro Vancouver land use data updated too. So we are overdue to update the post. One complication is that the 2021 data comes on 2021 geographies, but that problem is fairly easy to overcome using our TongFen package that automates the process to make geographic data comparable across census (and other) geographies. [@tongfen] And since in this case we are only looking at population data we can go a step further and use 2021 census blocks to match against 2016 dissemination areas.

# Toronto

To keep things easy we start with Toronto. We express the change in terms of the change in population density, concretely the change in people per hectare, between 1971 and 2021. The change of population density gives a nice way to normalize the data without getting into small denominator problems that come up when e.g. looking at the percentage change in population.


::: {.cell crop='true'}

```{.r .cell-code}
breaks <- c(-Inf,-100,-50,-25,-10,-5,5,10,25,50,100,Inf)
labels <- c("Loss of over 100", "Loss of 50 to 100",
            "Loss of 25 to 50", "Loss of 10 to 25", "Loss of 5 to 10", "About the same",
            "Gain of 5  to 10", "Gain of 10 to 25", "Gain of 25 to 50", "Gain of 50 to 100",
            "Gain over 100")
colors <- RColorBrewer::brewer.pal(length(labels),"PiYG")
toronto_city <- get_census("CA16",regions=list(CSD="3520005"),geo_format = 'sf',quiet = TRUE)
toronto_data <- get_census("CA16CT",regions=list(CMA="35535"),
                   vectors=c("1971"="v_CA1971x16_1","2016"="v_CA16_1"),
                   level="DA",geo_format='sf',quiet = TRUE) %>%
  mutate(area=`Shape Area`*100) %>%
  mutate(change=`2016`-coalesce(`1971`,0)) %>%
  mutate(change_h=change/area) %>%
  mutate(change_d=cut(change_h,breaks=breaks,labels=labels))

toronto_2021 <- get_census("2021",regions=list(CMA="35535"),level="DB",quiet = TRUE)
toronto_2016 <- get_census("2016",regions=list(CMA="35535"),level="DB",quiet = TRUE)

db_correspondence<-tongfen:::get_single_correspondence_ca_census_for("2021","DB") |>
  inner_join(toronto_2021 |> filter(Population>0) |> select(DBUID2021=GeoUID),by="DBUID2021") |>
  inner_join(toronto_2016 |> select(DBUID2016=GeoUID,DAUID2016=DA_UID),by="DBUID2016") |>
  select(DBUID2021,DAUID2016) |>
  tongfen:::get_tongfen_correspondence()

combined_data <- toronto_data |>
  left_join(db_correspondence |> select(DAUID2016,TongfenID) |> unique(),by=c("GeoUID"="DAUID2016")) |>
  group_by(TongfenID) |>
  summarize(across(c(`1971`,`2016`,Population,area),\(x)sum(x,na.rm=TRUE)),
            .groups="drop") |>
  full_join(toronto_2021 |> 
              inner_join(db_correspondence |> 
                           select(DBUID2021,TongfenID) |> 
                           unique(),
                         by=c("GeoUID"="DBUID2021")) |> 
              summarize(`2021`=sum(Population,na.rm=TRUE),.by=TongfenID),
            by="TongfenID") |>
  #mutate(area=st_area(geometry) |> as.numeric()) |>
  mutate(change=`2021`-coalesce(`1971`,0)) |>
  mutate(change_h=change/area) |>
  mutate(change_d=cut(change_h,breaks=breaks,labels=labels))

bbox=sf::st_bbox(toronto_city %>% st_transform(st_crs(combined_data))) 

combined_data |>
  filter(`2021`>10|`1971`>10) |>
  ggplot() +
  geom_sf(data=get_census("2021",regions=list(CMA="35535"),geo_format="sf",quiet = TRUE),
          fill="darkgrey") +
  geom_sf(linewidth=0.01,aes(fill=change_d)) +
  geom_water(tile_size_px=2048) +
  geom_roads(transform=\(d) d|> filter(kind %in% c("highway","major_road")),tile_size_px=2048) +
  scale_fill_manual(values=colors,na.value="grey") +
  labs(title="Toronto change in population density 1971-2021",
       caption=caption,
       fill="Change in ppl/ha") +
   coord_sf(datum=NA, xlim=c(bbox$xmin,bbox$xmax), ylim=c(bbox$ymin,bbox$ymax))
```

::: {.cell-output-display}
![Toronto change in population density 1971-2021](index_files/figure-html/toronto_pop_change_1971_2021-1.png){width=672}
:::
:::


The map looks quite similar to the [1971 to 2016 map from the previous post](https://doodles.mountainmath.ca/posts/2019-06-15-census-custom-timelines/index.html), which should not surprise since it only adds another 5 years of change on top of the previous 45 year time span. Just out of interest we quickly map the 2016 to 2021 change on the same colour scale.


::: {.cell crop='true'}

```{.r .cell-code}
combined_data |>
  mutate(change=`2021`-coalesce(`2016`,0)) |>
  mutate(change_h=change/area) |>
  mutate(change_d=cut(change_h,breaks=breaks,labels=labels)) |>
  filter(`2021`>10|`2016`>10) |>
  ggplot() +
  geom_sf(data=get_census("2021",regions=list(CMA="35535"),geo_format="sf",quiet = TRUE),
          fill="darkgrey") +
  geom_sf(linewidth=0.01,aes(fill=change_d)) +
  geom_water(tile_size_px=1024) +
  geom_roads(transform=\(d) d|> filter(kind %in% c("highway","major_road")),tile_size_px=1024) +
  scale_fill_manual(values=colors,na.value="grey") +
  labs(title="Toronto change in population density 2016-2021",
       caption=caption,
       fill="Change in ppl/ha") +
   coord_sf(datum=NA, xlim=c(bbox$xmin,bbox$xmax), ylim=c(bbox$ymin,bbox$ymax)) 
```

::: {.cell-output-display}
![Toronto change in population density 2016-2021](index_files/figure-html/toronto_pop_change_2016_2021-1.png){width=672}
:::
:::


# Vancouver

Next we turn to Vancouver. The population decline that's visible in Toronto's centrally located low-density neighbourhoods is muted in Vancouver, likely due secondary suites and laneway houses becoming legalized earlier in the central parts of the region.


::: {.cell}

```{.r .cell-code}
data_2021 <- get_census("2021",regions=list(CMA="59933"),level="DB",quiet = TRUE)
data_2016 <- get_census("2016",regions=list(CMA="59933"),level="DB",quiet = TRUE)

db_correspondence<-tongfen:::get_single_correspondence_ca_census_for("2021","DB") |>
  inner_join(data_2016 |> select(DBUID2016=GeoUID,DAUID2016=DA_UID),by="DBUID2016") |>
  full_join(data_2021 |> filter(Population>0) |> select(DBUID2021=GeoUID),by="DBUID2021") |>
  select(DBUID2021,DAUID2016) |>
  tongfen:::get_tongfen_correspondence()

tongfen_geos <- get_census("2016", regions=list(CMA="59933"), 
                           level="DA", geo_format="sf",quiet = TRUE) |>
  left_join(db_correspondence |>
              select(DAUID2016,TongfenID) |> unique(),
            by=c("GeoUID"="DAUID2016")) |>
  group_by(TongfenID) |>
  summarize(area_old=sum(`Shape Area`,na.rm=TRUE)*100,.groups="drop") 


years=c(1971,seq(1981,2011,5))
old_vectors <- years %>% map(function(y)paste0("v_CA",y,"x16_1")) %>% unlist %>% set_names(years)
pop_data <- get_census("CA16CT",regions=list(CMA="59933"),
                   vectors=c(old_vectors,c("2016"="v_CA16_1")),level="DA",quiet = TRUE) %>%
  select_at(c("GeoUID",years %>% as.character,"2016")) %>%
  mutate(GeoUID=recode(GeoUID,"59150062"="59153402")) |>
  group_by(GeoUID) %>%
  summarise_all(sum,na.rm=TRUE) 

combined_data <- pop_data |>
  left_join(db_correspondence |> select(DAUID2016,TongfenID) |> unique(),by=c("GeoUID"="DAUID2016")) |>
  group_by(TongfenID) |>
  summarize(across(matches("^\\d{4}$"),\(x)sum(x,na.rm=TRUE)),.groups="drop") |>
  full_join(data_2021 |> 
              inner_join(db_correspondence |> 
                           select(DBUID2021,TongfenID) |> 
                           unique(),
                         by=c("GeoUID"="DBUID2021")) |> 
              summarize(`2021`=sum(Population,na.rm=TRUE),.by=TongfenID),
            by="TongfenID")
```
:::

::: {.cell crop='true'}

```{.r .cell-code}
tongfen_geos %>%
  mutate(area=st_area(.) |> as.numeric()) |>
  mutate(area=area/10000) |>
  left_join(combined_data,by="TongfenID") |>
  mutate(change=`2021`-coalesce(`1971`,0)) |>
  mutate(change_h=change/area) |>
  mutate(change_d=cut(change_h,breaks=breaks,labels=labels)) |>
  filter(`2021`>10|`1971`>10) |>
  ggplot() +
  geom_sf(data=get_census("2021",regions=list(CMA="59933"),geo_format="sf",quiet = TRUE),
          fill="darkgrey") +
  geom_sf(linewidth=0.01,aes(fill=change_d)) +
  geom_water(tile_size_px=1024) +
  geom_roads(transform=\(d) d|> filter(kind %in% c("highway","major_road")),tile_size_px=1024) +
  scale_fill_manual(values=colors,na.value="grey") +
  labs(title="Vancouver change in population density 1971-2021",
       caption=caption,
       fill="Change in ppl/ha") +
  coord_bbox(metro_van_bbox('tight')) 
```

::: {.cell-output-display}
![Vancouver change in population density 1971-2021](index_files/figure-html/vancouver_pop_change_1971_2021-1.png){width=672}
:::
:::


# Finer geographies

For Vancouver we again want to cut down the census geographies by land use, removing parks and industrial areas and other parts where nobody lives. This is a bit of a challenge as some people still live in e.g. agricultural areas, and getting too aggressive might lead to distortions.

This time we will take a slightly different route than what we did in the previous post and leave in agricultural areas next to areas with explicit residential land use. On the other hand, we will divide by the resulting cut down areas for density estimates as opposed to using the area of the broader census geographies for normalization, which makes things more spiky. So there are some different design choices than the previous version.


::: {.cell}

```{.r .cell-code}
lu_data <- get_metro_vancouver_land_use_data()
residential_land_uses <- lu_data |> 
  filter(grepl("^Residential|Mixed Residential|Agricu|Rooming Houses",Descriptio)) |> 
  pull(LU_Code) |> 
  unique()

cut_to_land_use <- function(data,land_uses){
  crs_orig <- st_crs(data)
  lu <- land_uses |> st_make_valid() |> st_union() |> st_collection_extract("POLYGON") |> st_make_valid()
  d1 <- data |>
    st_transform(st_crs(lu)) |>
    st_make_valid() |>
    st_intersection(lu) |>
    st_collection_extract("POLYGON") |>
    st_make_valid() %>%
    filter(!st_is_empty(.)) 
  d2 <- data |>
    filter(!(TongfenID %in% unique(d1$TongfenID))) |>
    st_transform(st_crs(lu))
  bind_rows(d1,d2) |> st_transform(crs_orig) |>
  st_make_valid() 
}

tongfen_geos_lu <- tongfen_geos |> 
  cut_to_land_use(lu_data |> filter(LU_Code %in% residential_land_uses))%>%
  mutate(area=st_area(.) |> as.numeric()) |>
  mutate(area=area/10000) 

geo_data <- tongfen_geos_lu |>
  mutate(area=round(area,2)) |>
  rmapshaper::ms_simplify(keep = 0.75,keep_shapes = TRUE) %>%
  left_join(combined_data,by="TongfenID") 


success <- sf_to_s3_gzip(geo_data %>% select(c(area,matches("^\\d{4}$"))),
                         s3_bucket = "mountainmath",
                         s3_path = "yvr_timeline/yvr_pop_timeline_2021.geojson.gz")
```
:::


To better deal with the spikiness of the data we again turn to a separate interactive 3D visualization for the population data, showing both population density as well as change in population density.

<a href="/html/yvr_pop_timeline_2021.html" target="_blank"><img src="images/van_pop_timeline_2021.gif"/></a>

<a class="btn btn-primary" href="/html/yvr_pop_timeline_2021.html" target="_blank">Explore interactive population change map</a>

# Conclusion

Unfortunately we only have the historic census data back to 1971 for the Vancouver and Toronto areas, there would be a lot of value in getting electronic census data at comparable geographies for all of Canada, or at least the major metropolitan areas. Using TongFen we can emulate that at a loss of geographic granularity, this is easy to do going back to 2001 and can be extended to 1996 which based on geographic matching with enumeration areas. StatCan now does make census data available in electronic form back to 1981, but 1976 and 1971 data is still not available. And census geographies in electronic form for the old censuses is also problematic.

The semi-custom tabulation we have been using solves a lot of these issues, at this point it would probably be a lot more valuable if instead of continuing to release older census data on their original census geographies StatCan were to release the older census data on geographies based on the more modern system of geographies introduced with the 2001 census, or even better, just using 2021 geographies.

As usual, the code for this post is [available on GitHub](https://github.com/mountainMath/mountain_doodles/blob/main/posts/2024-04-23-population-timelines-update/index.qmd) for anyone to reproduce or adapt for their own purposes.


# Update 2024-04-24
A quick update to address two questions that came up.

Some people have asked about the reason for the population drop in some central areas in Vancouver and Toronto. The answer is fairly simple, over that time period, areas that did not add housing tended to lose population due to demographic shifts resulting in smaller average household sizes. In particular, families tend to have fewer children than they used to, and the share of homes occupied by childless seniors or widowed individuals has increased. There is nothing wrong with that, but we should allow neighbourhoods to add housing to compensate for that and grow beyond that to allow more people to live in job and amenity rich neighbourhoods.

One caveat that bears repeating is that geocoding issues, which can occur especially in older censuses, might cause some spikes. If there is a spike visible in only one year, that's probably a geocoding issue.

The other question was whether the [Toronto population change data will become available on CensusMapper](https://x.com/beheshtialex/status/1783288488206757943) like the [1971 to 2016 population change map](https://censusmapper.ca/maps/1658?index=0#12/43.6942/-79.4231). While possible in principle, extending the map to 2021 requires creating a new common geography based on 2016 and 2021 census geographies as described above and uploading it to CensusMapper together the the corresponding updated census variables. We have done that in the past for the 2011 and 2016 census geographies [@comparing-censuses.2017], which makes for great interactive maps comparing census profiles variables across these two censuses like the [net migration map](https://censusmapper.ca/maps/731?index=0#10/49.2620/-123.1142) that we quite like. While it would be valuable to do this for the 2016 and 2021 censuses for all of Canada and all levels of geography, it requires time which we right now can't justify spending on this. 

What's relatively easy though is to simply make a one-off map of the population change for Toronto copying the work we did for the interactive one-off map for Vancouver above.


::: {.cell}

```{.r .cell-code}
data_2021 <- get_census("2021",regions=list(CMA="35535"),level="DB",quiet = TRUE)
data_2016 <- get_census("2016",regions=list(CMA="35535"),level="DB",quiet = TRUE)

db_correspondence<-tongfen:::get_single_correspondence_ca_census_for("2021","DB") |>
  inner_join(data_2016 |> select(DBUID2016=GeoUID,DAUID2016=DA_UID),by="DBUID2016") |>
  full_join(data_2021 |> filter(Population>0) |> select(DBUID2021=GeoUID),by="DBUID2021") |>
  select(DBUID2021,DAUID2016) |>
  tongfen:::get_tongfen_correspondence()

tongfen_geos <- get_census("2016", regions=list(CMA="35535"), 
                           level="DA", geo_format="sf",quiet = TRUE) |>
  left_join(db_correspondence |>
              select(DAUID2016,TongfenID) |> unique(),by=c("GeoUID"="DAUID2016")) |>
  group_by(TongfenID) |>
  summarize(area=sum(`Shape Area`,na.rm=TRUE)*100,.groups="drop") 


years=c(1971,seq(1981,2011,5))
old_vectors <- years %>% map(function(y)paste0("v_CA",y,"x16_1")) %>% unlist %>% set_names(years)
pop_data <- get_census("CA16CT",regions=list(CMA="35535"),
                   vectors=c(old_vectors,c("2016"="v_CA16_1")),level="DA",quiet = TRUE) %>%
  select_at(c("GeoUID",years %>% as.character,"2016")) %>%
  group_by(GeoUID) %>%
  summarise_all(sum,na.rm=TRUE) 

combined_data <- pop_data |>
  left_join(db_correspondence |> select(DAUID2016,TongfenID) |> unique(),by=c("GeoUID"="DAUID2016")) |>
  group_by(TongfenID) |>
  summarize(across(matches("^\\d{4}$"),\(x)sum(x,na.rm=TRUE)),.groups="drop") |>
  full_join(data_2021 |> 
              inner_join(db_correspondence |> 
                           select(DBUID2021,TongfenID) |> 
                           unique(),
                         by=c("GeoUID"="DBUID2021")) |> 
              summarize(`2021`=sum(Population,na.rm=TRUE),.by=TongfenID),
            by="TongfenID")

geo_data <- tongfen_geos |>
  mutate(area=round(area,2)) |>
  rmapshaper::ms_simplify(keep = 0.75,keep_shapes = TRUE) %>%
  left_join(combined_data,by="TongfenID") 

success <- geo_data |>
  select(matches("\\d{4}"),area) |>
  sf_to_s3_gzip(s3_bucket = "mountainmath",
                s3_path = "yvr_timeline/yyz_pop_timeline_2021.geojson.gz")
```
:::


This map is not as snazzy as the Vancouver one as it is not cut down by land use and the densities shown may get dragged down by parks, industrial areas, and other non-residential land uses, but it should still be useful.

<a href="/html/yyz_pop_timeline_2021.html" target="_blank"><img src="images/yyz_pop_timeline_2021.gif"/></a>

<a class="btn btn-primary" href="/html/yyz_pop_timeline_2021.html" target="_blank">Explore the interactive population change map for Toronto</a>

# Update 2024-07-26

We have further refined the data in the interactive maps for Vancouver and Toronto to ameliorate geocoding errors in the historical census data timlines. For details please see our [new post describing the process to detect and ameliorate geocoding errors](/posts/2024-07-26-geocoding-errors-in-aggregate-data/). For comparison purposes the interactive maps using straight-up data from Statistics Canada before cleaning for (potential) geocoding errors is still available for [Vancouver](/html/yvr_pop_timeline_2021.html?cleaned=false) and [Toronto](/html/yyz_pop_timeline_2021.html?cleaned=false).

<details>

<summary>Reproducibility receipt</summary>


::: {.cell}

```{.r .cell-code}
## datetime
Sys.time()
```

::: {.cell-output .cell-output-stdout}

```
[1] "2024-07-26 09:41:16 PDT"
```


:::

```{.r .cell-code}
## repository
git2r::repository()
```

::: {.cell-output .cell-output-stdout}

```
Local:    main /Users/jens/R/mountain_doodles
Remote:   main @ origin (https://github.com/mountainMath/mountain_doodles.git)
Head:     [cfd8e12] 2024-07-25: updated image in md
```


:::

```{.r .cell-code}
## Session info
sessionInfo()
```

::: {.cell-output .cell-output-stdout}

```
R version 4.4.0 (2024-04-24)
Platform: aarch64-apple-darwin20
Running under: macOS Sonoma 14.5

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Vancouver
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] mountainmathHelpers_0.1.4 sf_1.0-16                
 [3] tongfen_0.3.6             lubridate_1.9.3          
 [5] forcats_1.0.0             stringr_1.5.1            
 [7] dplyr_1.1.4               purrr_1.0.2              
 [9] readr_2.1.5               tidyr_1.3.1              
[11] tibble_3.2.1              ggplot2_3.5.1            
[13] tidyverse_2.0.0           cancensus_0.5.8          

loaded via a namespace (and not attached):
 [1] gtable_0.3.5       xfun_0.44          htmlwidgets_1.6.4  lattice_0.22-6    
 [5] tzdb_0.4.0         vctrs_0.6.5        tools_4.4.0        generics_0.1.3    
 [9] curl_5.2.1         parallel_4.4.0     proxy_0.4-27       fansi_1.0.6       
[13] rmapzen_0.5.1      pkgconfig_2.0.3    KernSmooth_2.23-22 RColorBrewer_1.1-3
[17] assertthat_0.2.1   lifecycle_1.0.4    git2r_0.33.0       farver_2.1.2      
[21] compiler_4.4.0     tinytex_0.51       munsell_0.5.1      codetools_0.2-20  
[25] jqr_1.3.3          htmltools_0.5.8.1  class_7.3-22       lazyeval_0.2.2    
[29] yaml_2.3.8         pillar_1.9.0       crayon_1.5.2       classInt_0.4-10   
[33] magick_2.8.3       wk_0.9.1           tidyselect_1.2.1   digest_0.6.35     
[37] stringi_1.8.4      fastmap_1.2.0      grid_4.4.0         colorspace_2.1-0  
[41] cli_3.6.3          magrittr_2.0.3     crul_1.4.2         utf8_1.2.4        
[45] e1071_1.7-14       withr_3.0.0        scales_1.3.0       sp_2.1-4          
[49] bit64_4.0.5        timechange_0.3.0   rmarkdown_2.27     httr_1.4.7        
[53] bit_4.0.5          hms_1.1.3          evaluate_0.23      knitr_1.47        
[57] V8_4.4.2           geojson_0.3.5      s2_1.1.6           rlang_1.1.4       
[61] Rcpp_1.0.12        httpcode_0.3.0     glue_1.7.0         DBI_1.2.3         
[65] geojsonsf_2.0.3    rstudioapi_0.16.0  vroom_1.6.5        jsonlite_1.8.8    
[69] R6_2.5.1           geojsonio_0.11.3   units_0.8-5       
```


:::
:::


</details>
