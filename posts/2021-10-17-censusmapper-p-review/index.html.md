---
title: CensusMapper (p)review
author: Jens von Bergmann
date: '2021-10-17'
slug: censusmapper-p-review
categories:
  - CensusMapper
tags: []
description: 'CensusMapper is six years old now. A review, and a preview where things could be heading.'
featured: 'images/censusmapper_experimental.png'
image: "images/censusmapper_experimental.png"
featuredalt: ""
featuredpath: ""
linktitle: ''
type: "post"
aliases:
  - /blog/2021/10/17/censusmapper-p-review/
---








We started CensusMapper in 2015, and it's hard to believe that it's already six years old. And with the first release of the 2021 census just a few months away it's a good time for a review. And a preview of what could possibly come in the future.

### TL;DR
CensusMapper has outgrown it's status as a personal side project and is now widely used by [researchers](https://scholar.google.ca/scholar?hl=en&as_sdt=0%2C5&as_ylo=2017&q=%22Cancensus%22&btnG=), [government officials and staff](https://princegeorge.ca/City%20Hall/Documents/Mayor%20and%20Council/Committees%20and%20Boards/Poverty%20Profile%20-%20Final%20-%20Nov%2019%20Update.pdf), non-profit organizations, and the general public. It has grown beyond a server for web maps to a server for census data, and has served nearly 4 billion fields of census data to date. After six years it's time for a comprehensive overhaul, both of the overall UI and UX, the functionality that's available to the general public, and the underlying mapping technology. And it's going to be grand. But there is a catch. To outgrow it's side project status, CensusMapper will need to either lock up features and monetize them, or find a different way to fund it's operation that allows it to remain open and freely accessible to the public. This kind of project simply isn't maintainable without basic funding in place to support it. 

## How it started
CensusMapper started as a side project that got a bit out of control. Sometime in spring 2015 I became aware how difficult it is to access and use Canadian census data. In Canada we have a census every five years, and it's an extremely rich resource. Yet even high-level numbers have been quite inaccessible outside a group of people with desktop GIS skills. Let alone being able to explore spatial variation of data. And even for GIS specialists the process of downloading and working with the data was cumbersome and time-consuming. 

We felt that census data should be easily accessible to everyone. Not just high-level stats but also being able to explore fine-grained spatial variation. Based on this belief we developed CensusMapper, a web-based interactive map that covers all of Canada, all census variables, at (almost) all aggregation levels from all of Canada down to Dissemination Areas, the finest geography general census profile data is released on, and Dissemination Blocks, an even finer geography that only has population, household and dwelling counts.

### CensusMapper web maps
Initially CensusMapper came with the 2011 census and NHS data, adding the 2006 census data in 2016, and the 2016 census in 2017 bit by bit as it came out. Later we added the 2001 and 1996 censuses to allow for longer timelines and context when viewing census data through time.

Initially map-making was locked off, but we [opened up the ability to make custom maps to everyone shortly after](https://doodles.mountainmath.ca/blog/2015/09/28/census-mapper/). Over time we added a number of features, but most of the deeper map-making features remained locked off, mostly because we did not have the time to make them user-friendly enough to be used easily.

### A flexible mapping tool
Under the hood the CensusMapper queries raw census data from the server and draws maps live in the user's browser. To do this the map-maker has to specify a function based on census variables, which may be arbitrarily complex. The CensusMapper front-end will with query those variables for the current map view from the CensusMapper back-end, evaluate the function, apply the colour scale and draw the map. This means that CensusMapper is extremely flexible and a great tool for exploratory analysis. Maps are automatically Canada-wide and work at all geographic aggregation levels.

### CensusMapper APIs
<img src="https://raw.githubusercontent.com/mountainMath/cancensus/master/images/cancensus-sticker.png" style="float:right;margin-left:10px;width:200px;max-width:50%;">
Maps are fun, but maps aren't analysis. In 2017 I was working increasingly with census data, and I was pulling data out of the CensusMapper database as I needed it. This worked in principle, but in practice there were problems.

* It wasn't reproducible, I did not keep good track of how I pulled data out of the database
* It wasn't transparent, it required privileged access to the database and others could not check or reproduce my workflows
* It wasn't adaptable. Pulling data out of the database manually made it hard to easily adapt previous work for related projects

The solution to this was to [build an API](https://doodles.mountainmath.ca/blog/2017/09/27/reproducibility/) to have an organized and transparent way to pull data out of the CensusMapper database that others could use too. And to build an [R package](https://mountainmath.github.io/cancensus/index.html) that wraps the API and enables people to easily share that uses and analyses Canadian census data fetching the data dynamically as needed.

## How it's going
CensusMapper is now six years old, and six years is a lot of time in the world of web technology, in particular web maps. Modern web maps are a lot more flexible in how to display contextual data like roads and place names, have continuous zoom, [allow 3D extrusion](https://doodles.mountainmath.ca/html/yvr_pop_timeline), and are much faster and performant. Additionally, CensusMapper was always more of a personal side-project spun out of control than a product developed for a broader set of users. The UX is clunky and unintuitive at times, the design is incoherent and wanting.

It's pretty clear what the solution to this should be. A couple of things need updating:

1. Front end map engine
2. Overhaul of general UI/UX
3. Better and more user-friendly custom map creation and expansion of advanced mapping to the general public
4. Expansion of custom map overlays to general public
5. Enabling geographic filters for general users
6. Non-map based summary information for cities
7. Better widgets and interactivity
8. Moar data
9. Expansion of the data API and python bindings (next to existing R bindings)
10. Performance boost
11. Better map projection

### Map engine
There are lots of modern map engines to choose from. What we will want is something that enables continuous zoom, ways to fine-tune the styling of contextual information like roads, place names and land use overlays, all in a snappy WebGL based rendering engine. There are several ways to do this, Mapbox is probably the best know one. This is for example what we used for the [Metro Vancouver Zoning Map](https://mountainmath.ca/zoning_map).

While Mapbox is great for rendering traditional map data, it has some limitations for rendering data. Enter Deck.gl, the Uber mapping engine optimized for data that plays well with Mapbox. This gives a lot more flexibility in how to render and interact with the data, we have used this when [mapping all of Nova Scotia residental properties and the effect of the CAP](https://mountainmath.ca/ns_cap_map), the Nova Scotia version of California's property tax reduction Prop 13.

With Deck.gl we can dynamically interweave census data layers with other layers, which allows individual styling of maps, and selective overlaying of contextual information.

For CensusMapper we need to adapt this to deal with tiled data, as well as work with the CensusMapper paradigm of strict separation of geographic and census variable data that is key to the high performance and low resource consumption architecture. 

In principle that's not too much work, we did a [quick test to see what this would look like](https://censusmapper.ca/census_experimental). It's still has some issues, especially on mobile, but it gives a general idea of the direction we think CensusMapper maps should be heading.

<video width="100%" autoplay="autoplay" loop="loop">
<source src="https://mountainmath.s3.ca-central-1.amazonaws.com/movies/CensusMapper_demo.mp4" type="video/mp4">
</video>

### UI/UX overhaul
The overall CensusMapper UI was cooked up with minimal planning, and grew out of our personal mapping needs as they arose. IT needs a major cleanup and decluttering and a more seamless transition between mobile and desktop experiences. Some UI and visualization elements are clunky. We simply never spent the time to polish things up and come up with a clear and consistent design. There is no tutorial or basic walk-through that can help interested new users to find their way around CensusMapper and gently guide them toward how to create their own maps.

### Advanced mapping
Right now CensusMapper enables anyone to map single census variables, either directly or as a percentage (if appropriate). This is great and an easy way to explore basic census variables. But it barely opens the door 
Advanced mapping opens up the possibility to map more complicated relationships. For example, our very first map on CensusMapper, and part of the motivation for it's design choices, was a [map of the "Hidden Mortgage"](https://doodles.mountainmath.ca/blog/2015/08/25/the-hidden-mortgage/), a map that estimates the transportation costs to work people in each region are paying by considering mode of transportation to work, commute time, and income.

On the map-creating side of things it needs a clearer and more purposeful user interaction that also enables the user to transition to more advanced mapping functionality. The advanced map-making module is extremely powerful, allowing for arbitrary functions to compute mappable values from any selection of census variables. Opening this up to general users needs thoughtful UI and guidance.

The payoff for enabling advanced map-making capabilities for general users is considerable, it puts the full power of CensusMapper maps into the hands of all Canadians. 

### Map overlays
Adding contextual information can be very useful, allowing custom overlays is a regularly requested feature.
Currently CensusMapper has the ability to add overlays, but it's clunky and locked up. Here is an example map that [maps proportions of the population in low, middle and high adjusted family income groups and overlays City of Vancouver low-density zoning areas](https://censusmapper.ca/maps/841?customData=10#12/49.2576/-123.1178). This should be expanded to allow users to more easily upload or link custom datasets and style them.

### Geographic filters
One of the great features of CensusMapper is that any map created is automatically Canada-wide. But this can also be a disadvantage, sometimes maps are made with very particular geographies in mind only. And might not make much sense in other areas. And example would be a map showing location quotients, so ratios relative to some higher order geography. Take the map of [age group location quotients for Metro Vancouver](https://censusmapper.ca/maps/1509) that looks at the share of the population in different age groups relative to the Metro Vancouver age distribution. This map does not make sense outside of Metro Vancouver, so it is filtered to only show information for within Metro Vancouver. While this functionality is already available in CensusMapper, it is not documented and currently locked to advanced map users. This is mostly because it's only really needed in combination with advanced mapping of census variables, but also because it hasn't been implemented in an intuitive and user-friendly way.

### Non-map based information
While CensusMapper is mostly about maps, there is also value in simple overview census profiles for municipalities and other census regions that give a high-level visual overview of key statistics like income and age distributions and other features. The [EveryCityCanda bot by Dmitry Shkolnik and myself implemented something like this](https://twitter.com/EveryCityCanada), this could be expanded on and implemented in CensusMapper to give an easy and interactive overview of key metrics. 

### Better widgets and interactivity
One of the strength of CensusMapper is the way it can add widgets with data interactions linked across widgets and the map. By default CensusMapper displays a histogram of the values in the current map view, and hovering over map areas will indicate the position of the area in the histogram. And vice versa, hovering over the histogram will highlight corresponding map areas.

CensusMapper also allows for the embedding of scatter plots into the map story side bar, and all widgets, including the histogram and the map data, interact with one another. An example of this can be seen in the active transportation map linked above, or the [Local Affordability map](https://censusmapper.ca/maps/897). These kind of widgets can greatly enhance the understanding of the data and storytelling, making this more user-friendly and enabling adding of these widgets for all users would help others create compelling data stories.

### Moar data
We will definitely add the 2021 Census data as it rolls out, but there are lots of other datasets that would greatly enhance the usability of CensusMapper, both for mapping as well as a data server. We have already integrated several cross tabulations, for example one on [document type by structural type](https://doodles.mountainmath.ca/blog/2020/01/26/unoccupied-dwellings-data/) to get a better reading of the overall housing stock. We have custom tabulations of [T1FF tax filer data at the census tract level for all census years 2000 through 2018](https://doodles.mountainmath.ca/blog/2020/04/23/census-tract-level-t1ff-tax-data/). 

### API expansion
Next to our traditional API services we have added an [enpoint that indentifies census regions intersecting custom geographies](https://mountainmath.github.io/cancensus/articles/intersecting_geometries.html). This is very useful when running analysis relating to custom geographies with a-priory unspecified spatial extent. Traditionally we would first have to identify and download data that covers our custom geographies and then run an analysis, with the new endpoint we can automate the identification of relevant geographies. There are other use cases like this, and the API can be tailored or expanded to better serve such needs.

At the same time, the most convenient way to access census data is via the `cancensus` R package. It would be useful to provide similar bindings to python, the other important statistical language with a mature ecosystem for geospatial analysis. While most people in the data world are bilingual, people have preferences and giving easier access to census data in python would be a lot more convenient for those preferring to work exclusively in python. Cleaning up the API web interface for data download for those looking to do this manually would also be useful, the current UI is quite intuitive.

### Performance boost
The CensusMapper server runs on a $20/month cloud instance. This suffices for most uses, the map server starts to slow down noticeably at around 1000 simultaneous users. Which rarely happens. But it does necessitates some compromises, e.g. the throttling of API calls. We regularly get request for extended API quotas which we usually grant, but this is starting to put some strain on the server and could benefit from a performance boost. At the same time, we could invest some time to improve server logic to better deal with large simultaneous data requests to ensure smooth performance of the other server functions like web mapping.

### Better map projections
Currently CensusMapper, like many web maps, uses the Mercator projection. This is particularly problematic for Countries as far away from the equator as Canada. Right now this is largely dictated by the availability of tile data for contextual map data like roads, place labels, land use and buildings. Having this available for other map projections would be useful for other Canadian mapping applications, and is in principle not hard to do. But it requires setting up a tile server for differently projected map data, and could be enhanced by dynamic re-projection of data to e.g. retain that North is pointing up (on average) in the visible map area for intuitive orientation.


## CensusMapper 2.0
So when will all this be ready? Here we run into the fundamental problem when turning a side project into an actual product. It requires time and effort. Not like we haven't put any time into CensusMapper in the past, we definitely have. But there are limits to how far we can justify running this off the side of our desk. We are at the juncture where we have to either lock up parts of CensusMapper and monetize it, or get other funding to keep this project open and accessible to everyone. Or, given our past history a fairly likely variant, keep CensusMapper largely as it is, while maybe every now and then doing some work around the edges.

The key portion to make CensusMapper more useful, and that justifies other upgrades like a better mapping engine, is the UI/UX overhaul. It makes little sense to just swap out and spiff up the map and leave the rest as is. Without increases in usability there is not much value in doing this.

Will CensusMapper 2.0 become reality? We don't know. But at least we have a roadmap.




