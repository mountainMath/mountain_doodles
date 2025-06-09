---
title: Land Use Data
date: 2016-01-31
categories:
  - Vancouver
  - land use
slug: land-use
author: Jens von Bergmann
tags: []
description: 'Metro Vancouver Land Use Data'
image: "images/land use.png"
featuredalt: ""
featuredpath: "/images"
linktitle: ''
type: "post"
aliases:
  - /blog/2016/01/31/land-use/
---




Metro Vancouver has a [fairly good land use dataset](http://www.metrovancouver.org/data) that I imported quite some time
ago so that [I could take a good look at it](https://mountainmath.ca/land_use/map). But then I forgot about it until
[@HealthyCityMaps](https://twitter.com/HealthyCityMaps) [reminded me](https://twitter.com/HealthyCityMaps/status/692832946714050564) just
at the time when the discussions about [assessmened values](https://mountainmath.ca/map/assessment) in Vancouver were flaring up again.

So finally I decided to mark up the City of Vancouver assessment dataset with the land use from the Metro Vancouver land
use dataset. And I used the occasion to update my color schemes from two years ago and gave it a dark background that
works better with the [colorbrewer](http://colorbrewer2.org) palettes.

The result of the land use and assessment data mashup is a much better understanding what kind of properties we have right now in Vancouver.
<!-- more -->
## Land Use for Assessment Data
There is a strong relationship between land use and zoning. But that relationship is far from perfect, zoning is a funny
beast. For example, zoning laws prevent low-rise apartments to be built in areas zoned for single family housing. But
they permit single family housing in areas zoned for low-rise apartments. Or in areas zoned as commercial. The land
use dataset allows to tag every property based on what it is actually used for.

Of course the land use dataset is not perfect and has some issues, but overall it gives a far superior view on how land
is used when compared to soley relying on zoning. But putting the two datasets together one can even further filter down
to tease out actual land use.

## The Forgotten Single Family Houses
Single family houses have received a lot of attention lately, not least because of the
[incredible gains in land value during the last year](http://doodles.mountainmath.ca/blog/2016/01/24/work-vs-twiddling-thumbs/).

But how to narrow down the assessed properties to only single family housing? The easiest way to do this is to use zoning
as a proxy, so [only use RS zoned properties](https://mountainmath.ca/map/assessment?filter=rs). But there are a number
of problems with that.

[<img  src="images/rs_not_sfh.png" style="width:50%;float:right;">](https://mountainmath.ca/map/assessment?filter=rs_not_sfh&layer=14) 
Firstly, only filtering by zone will still include lots of unwanted properties like schools and parks. I used to filter out
parks using the City of Vancouver parks dataset, but it actually does not capture all parks. And then I filtered by area
to get rid of large properties that typically house schools or other institutions. But that keeps smaller institutions in
and throws larger single family houses out. Using the land use data we can better focus in on all the residential properties
in the single family housing zone. But even then, there are [1,255 residential properties in 'RS' zoned areas that are
not single family houses](https://mountainmath.ca/map/assessment?filter=rs_not_sfh&layer=14).

[<img  src="images/sfh_not_rs.png" style="width:50%;float:left;margin-right:10px;">](https://mountainmath.ca/map/assessment?filter=sfh_not_rs&layer=6)
Secondly, and probably more importantly, it misses
[the 15% (12,111 total) of the single family stock that is situated in other zones](https://mountainmath.ca/map/assessment?filter=sfh_not_rs&layer=6).
'Upzoning' a neighbourhood
only means that other forms of housing are also permitted, but people can still build single family homes there if they
would like. 
[Zoom into the map](https://mountainmath.ca/map/assessment?filter=sfh_not_rs&layer=6) and choose different ways to colour
the properties if you want to learn more about where they are.

## All Single Family Houses
[<img  src="images/sfh.png" style="width:50%;float:right;">](https://mountainmath.ca/map/assessment?filter=sfh&layer=9) 
When adding in the land use data we can filter by actual land use. That immediately takes care of parks and schools and other institutions,
and at the same time it allows to find all "single family or duplex" properties. That gets us closer to what we are looking
for, but it also includes duplexes. But those are easy to detect in the assessment dataset, they show up with two separate
tax bills. So combining the two we get the actual [map of all single family homes](https://mountainmath.ca/map/assessment?filter=sfh&layer=9).
   
Are these really *all* single family homes? Not quite. There are some issues with the land use dataset that mis-classify
a couple of properties. In particular some RS lots without a house are classified as 'Undeveloped', and some are classfied as
'Single Detached & Duplex', probably depending on what was there around the time the data was collected. The correct classification really
depends on what one is interested in, and will likely be outdated fast.

Moreover, the property assessment dataset of the City of Vancouver is to correctly link a couple of hundred properties
to their tax data. This won't be an issue when analysing the tax data, but it does cause a problem when mapping the properties
as they won't have any assessment or zoning data associated with them.

And there are issues on the fringes. Some properties in Southlands are used as single family homes on large lots, rather
than the limited agricultural that it is zoned at and labeled as in the land use dataset. 

But overall, adding in the land
use information gives a huge improvement on narrowing down single family homes. We should remember that
"single family homes" are better described as "single owner properties that can't be stratified",
as they may include secondary suites and laneway houses. 
   
## Historical Data
One shortcoming of the land use dataset is that there are no historic versions available. It would be very nice to be able
to see how land use changed over time, but that dataset won't do the trick. The City of Vancouver assessment dataset
comes with historic data all the way up to 2006, and can be used to do some fun 
[analysis](http://doodles.mountainmath.ca/blog/2016/01/18/redevelopment/). But the historic dataset is incomlete in that
zoning information is only available from 2014 forward and in that historic property tax data can't always be linked to
physical properties if the property has been re-developed. But overall putting the datasets together allows to refine
the analysis previously made. For example re-running the [thumb twiddling rates](http://doodles.mountainmath.ca/blog/2016/01/24/work-vs-twiddling-thumbs/)
using only Single Family Homes, we get pretty much the same medians, but the averages are higher. We could also refine
our [teardown analysis](http://doodles.mountainmath.ca/blog/2016/01/18/redevelopment/) to include only single family homes
and better filter out institutional properties, although some issues with the lack of historic data remain.
 
 

