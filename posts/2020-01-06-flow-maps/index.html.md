---
title: Flow Maps
author: Jens von Bergmann
date: '2020-01-06'
slug: flow-maps
categories:
  - cancensus
  - CensusMapper
  - Transportation
tags: []
description: "Fun with flow maps."
featured: ''
image: "images/commute_flow_map.png"
featuredalt: ""
featuredpath: ""
linktitle: ''
type: "post"
math: true
blackfriday:
  fractions: false
  hrefTargetBlank: true
aliases:
  - /blog/2020/01/06/flow-maps/
---





Just came across this [excellent flow map tool](https://flowmap.blue) that takes a google sheet and turns it into an interactive flow map. It's super-easy to use, here is a quick demo.

We are using the commuting flow data between census subdivisions from the 2016 census. First we load the required libraries


```r
library(tidyverse)
library(cancensus)
#remotes::install_github("mountainmath/statcanXtabs")
library(statcanXtabs)
library(sf)
library(googlesheets4)
```

Next we create the google sheet for our flow map. It comes with three sheets, one defining overall properties, one defining the locations and one for the flows.

### Properties

```r
my_properties <- c(
  "title"="Canadian animated commuter flow",
  "description"="Canadian animated commuter flow",
  "source.name"="Statistics Canada Census 2016",
  "source.url"="https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/Rp-eng.cfm?LANG=E&APATH=3&DETAIL=0&DIM=0&FL=A&FREE=0&GC=0&GID=0&GK=0&GRP=1&PID=111332&PRID=10&PTYPE=109445&S=0&SHOWALL=0&SUB=0&Temporal=2017&THEME=125&VID=0&VNAMEE=&VNAMEF=",
  "createdBy.name"="Jens von Bergmann",
  "createdBy.email"="jens@mountainmath.ca",
  "createdBy.url"="https://doodles.mountainmath.ca/posts/2020-01-06-flow_maps"   ,
  "mapbox.mapStyle"=NA,
  "colors.scheme"="Default",
  "colors.darkMode"="yes",
  "animate.flows"="yes",
  "clustering"="yes"
)

properties <- tibble(property=names(my_properties)) %>%
  mutate(value=my_properties[property])
```

### Locations
For the locations data we use the centroids of the census CSDs, where we prettify the names and fine-tune some special locations like Electoral A in Vancouver, which we hide behind the `clean_location_data()` function call.


```r
locations <- get_census("CA16",regions=list(C="01"),level="CSD",geo_format = "sf") %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  cbind(st_coordinates(.)) %>%
  st_set_geometry(NULL) %>%
  clean_location_data() %>%
  select(id=GeoUID,name=Name,lat=Y,lon=X)
```

### Flows
For the flows we take the commute flow cross tabulation at the CSD level.


```r
flows <- get_sqlite_xtab("98-400-X2016325","https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/CompDataDownload.cfm?LANG=E&PID=111332&OFT=CSV") %>%
  select(origin=`GEO_CODE (POR)`,dest=`GEO_CODE (POW)`,count=`Dim: Sex (3): Member ID: [1]: Total - Sex`) %>%
  collect()
```

### Putting it together
The only thing left to do is to upload the data to google sheets.


```r
#my_new_sheet <- sheets_create(name="Animated commuter flow",sheets=c("properties","locations","flows"))
my_new_sheet <- sheets_get("13Q-xsfL59XXPw7-3ue_9G9xab9FtYiL3S9vOZp-yJyY")
write_sheet(properties,my_new_sheet,"properties")
write_sheet(locations,my_new_sheet,"locations")
write_sheet(flows,my_new_sheet,"flows")
```

That's it. We can read off the sheet id and embed our flow map.

<iframe width="800" height="600" src="https://flowmap.blue/13Q-xsfL59XXPw7-3ue_9G9xab9FtYiL3S9vOZp-yJyY/embed" frameborder="0" allowfullscreen></iframe>

It clusters locations by default, so at low zoom levels it only shows high-level flows. Zoom in to see finer commute flows for your region of interest.

