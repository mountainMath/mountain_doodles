---
title: Frontage
author: Jens von Bergmann
date: '2018-03-28'
slug: frontage
categories:
  - Assessment Data
  - Vancouver
  - zoning
  - density
tags: []
description: 'Frontages for commercial zoned properties in Vancouver.'
image: "index_files/figure-html/frontage-1.png"
featured: 'frontage-1.png'
featuredalt: ""
featuredpath: "/posts/2018-03-28-frontage_files/figure-html"
linktitle: ''
type: "post"
aliases:
  - /blog/2018/03/28/frontage/
---






Over the past years several people have asked me questions about street frontage of city properties. When I needed similar data for a work project, and Scot Hein asked me a question about frontages of commercial properties for his [Urbanarium debate](https://urbanarium.org/city-debate-11-build-missing-middle-housing-without-lot-assembly), I decided to finally pull the numbers. The answer to that question is not as straight forward as it might seem, mostly because properties aren't necessarily square. There are a couple of algorithm that can solve this problem, but in this case we can keep things reasonably simple as the City of Vancouver has property frontages listed on [VanMap](http://vancouver.ca/your-government/vanmap.aspx) and make the data available on their [Open Data Portal](http://data.vancouver.ca/datacatalogue/propertyInformation.htm).

Scot was particularly interested in the frontages in the commercial zones along the arterials. The ingredients we need to answer that question are:

1. The property parcel polygons.
2. The lot lines and dimensions.
3. The zoning data for each lot.
4. The streets data.

All of these can be found on the City of Vancouver open data portal. I'll spare you the details on how I chose to match things up, if you need to know you can just download the [R notebook that made this post](https://github.com/mountainMath/doodles/blob/master/content/posts/2018-03-28-frontage.Rmarkdown) and look at the code.

As an aside, at this years open data day event I was reminded that there can be high entry barriers to using CoV open data. At this point I have a highly processed database that I usually use for this kind of questions, but with a narrow and clearly defined question at hand I thought it might be a useful exercise to build the data from scratch so that others have the benefit of being able to fully reproduce and adapt the analysis.

There will be essentially no commentary in this post, Scot will have done a did a better job adding context than I can. Sadly I am out of the country and will have to watch the recording once it gets posted.

















# Commercial zoned properties by frontage
To understand how many properties there are with frontages smaller and larger than 150ft, and what the finer distribution of frontages is, we graph them by frequency and cumulative area.

<img src="index_files/figure-html/unnamed-chunk-5-1.png" width="864" />

## Building Age
Building age is another important variable in this, as we generally expect higher re-development pressure on older buildings. 

<img src="index_files/figure-html/unnamed-chunk-6-1.png" width="864" />

We can view the properties by number or cumulative area.

<img src="index_files/figure-html/unnamed-chunk-7-1.png" width="864" />

## Relative Building Value
The relative building value may is a more direct measure of development pressure, [as we have explained in detail previously](https://mountainmath.ca/teardowns).

<img src="index_files/figure-html/unnamed-chunk-8-1.png" width="864" />



<img src="index_files/figure-html/unnamed-chunk-9-1.png" width="864" />

# Geographic distribution
Another important dimension is the geographic distribution of these lots.










<img src="index_files/figure-html/frontage-1.png" width="768" />

While there is some bunching when it comes to narrow and wide lots, there is quite a bit of dispersion and we can find both, narrow and wide lots, in all areas.

<img src="index_files/figure-html/unnamed-chunk-13-1.png" width="768" />




Similar things can be said about the teardown pressure facing these lots. There are some areas visible where building values have somewhat kept up with land values, but there are plenty of sites facing high teardown pressure in all neighbourhoods.


Again, for those wishing to reproduce the results or adapt it for their own purposes, the code is [available in GitHub](https://github.com/mountainMath/doodles/blob/master/content/posts/2018-03-28-frontage.Rmarkdown).
