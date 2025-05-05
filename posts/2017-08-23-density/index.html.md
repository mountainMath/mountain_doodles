---
title: Density
author: Jens von Bergmann
date: 2017-08-23
categories:
  - newsfail
  - CensusMapper
  - cancensus
  - density
slug: density
tags: []
description: 'Thinking about density.'
image: "images/densest_CMAs.png"
featured: 'densest_CMAs.png'
featuredalt: ""
featuredpath: "date"
linktitle: ''
type: "post"
aliases:
  - /blog/2017/08/23/density/
---





Density in Vancouver has been one of the recurring themese on this blog, and there are many
different ways to come at it. We have [looked at density in terms of land use](http://doodles.mountainmath.ca/blog/2016/02/29/land-use/)
to understand how much land is devoted to what purpose in Metro Vancouver and it's municipalities. We have looked
at density in terms of [tax density](http://doodles.mountainmath.ca/blog/2016/03/02/property-taxes-and-land-use/) to
understand how property tax revenue depends on land use and zoning. We have looked at
[density in terms of built floor space ratio](http://doodles.mountainmath.ca/blog/2016/05/20/density/).


And of course we have looked at [population density through CensusMapper](https://censusmapper.ca/maps/591),
and this time we want to do a quick variation on that theme.

<!-- more -->

# Population Density
Recently I have heard renewed chatter about how dense Vancouver is or is not. Before diving deepr into this it is
important to distinguish two types of density that often get mixed up.

## Gross Density
Gross (population) density simply looks
at the total population divided by (land) area. Ignoring census undercounts, we can simply look at the 2016 census numbers
to compute these.

## Net Density
*Net density* takes residential land (instead of all land)
as it's base. One measure of net density is the *floor space ratio* (FSR), which we have
[approximated and mapped in the past](https://mountainmath.ca/map/assessment?zoom=13&lat=49.25&lng=-123.1182&layer=16&mapBase=2)
and that also includes commercial space next to residential living space. More details on this are in
[this older blog post]((http://doodles.mountainmath.ca/blog/2016/05/20/density/)).

Net density is typically what people refer to in the countext of building developments. Sadly it's hard to
get a hold of good data sources that would allow for meaningful comparisons across the country. The
provincial assessment authorities have that data, but in Canada this data only shared after significant financial
commitments.

# How Dense is Vancouver?
How does Vancouver stack up against the rest of Canada? That, as always, depends on the details.
In particular, on whether we are talking about Metro Vancouver or the City of Vancouver, and on
what good comparables are. With the new [cancensus R package](https://github.com/mountainMath/cancensus)
it's straight forward to start hacking away
at this question. Let's start simple by looking at the 10 census metropolitan
areas and 10 census subdivisions (cities) with the largest gross density. And to keep things
somewhat in check, let's only look at census subdivisions with at least 50,000 people. (If we
drop the population restriction we get the cities of Westmount, Côte-Saint-Luc, Hampstead,
and White Rock gate-crash our top 10 list.)

<img  src="images/densest_CMAs.png" style="width:49%;"><img  src="images/densest_CSDs.png" style="width:49%;">

We see that the City of Vancouver takes the top spot among the cities, Metro Vancouver comes in
behind Toronto, Red Deer and Montréal. Great for a game of trivia, but it's hard to learn
much of significance from this.

The reason is that we don't know how these densities come about. Metro Vancouver contains the North Shore mountains,
a very large swath of land where nobody lives. Yet this counts to our area (or at least the horizontal projection of it).
Toronto has the green belt. How does one compare these things?

# Distribution of Density
A good first step is to look at the distribution of density within the cities. Again *cancensus* makes it easy
to map dissemination block level density. We change gears a tiny pit and simply focus Canada's 9 largest (by population) cities.

<img  src="images/density_map.png" style="width:99%;margin:5px auto;">

The large grey areas, with fewer than 1 person per hectare, jump out immediately. As the colour gradient suggest, there
are some denser areas around the centres, but the exact extent of these are hard to grasp. A better way to get a grip on
the proportion is to abandon the geographic coordinates and show the data as a tree map.

<img  src="images/density_area.png" style="width:99%;margin:5px auto;">

Now we much more clearly how the density in the different cities is made up, the proportion of low and high density areas.
The grey areas, with less than 1 person per hectare, are the parks, industrial and commercial land base of the city. Areas with
fewer than 25 people per hectare are densities usually found in broad suburban sprawl. In Vancouver, Shaughnessey, Southlands, or the Drummond Drive
area of West Point Grey are examples.

We can see that only Toronto, Montreal and Vancouver have significant portions of land beyond the 100 people per hectare density, with
Montréal devoting a larger portion of it's land to that density than any other of the cities.
In Vancouver the 50 to 100 people per hectare density dominates, althoguh the cutoffs are quite arbitrary to this should not be
over-interpreted, whereas in Toronto the  1-50 and 50-100 areas are larger.

Instead of asking how much area is devoted to what density, we can also ask what share of the population lives in what density.

<img  src="images/density_population.png" style="width:99%;margin:5px auto;">

This visualizes the density as felt by the population that lives in it. Comparing Montréal and Vancouver we see that about half of Montréal's citizens
live in 50 ro 150 people per hectare density, whereas in Vancouver half live in 25 to 100 people per hectare density. To make up for that, Vancouver has
a much higher portion of the population living in over 300 people per hectare density than Montréal.

# Next Steps
There are several obvious ways to expand on this. One would be to take better comparables, so instead of comparing cities one could compare
areas with similar population centred around census metropolitan area centres. Comparing Vancouver to Toronto as cities is tricky, Toronto has
a comparable population to the whole of Metro Vancouver. So a better comparison could be to take the old City of Toronto boundaries and compare
these against Toronto. Or throw Burnaby, Richmond, New Westminster and North Vancouver into the mix with the City of Vancouver.

With the [cancensus R package](https://github.com/mountainMath/cancensus) this is quite easy to do. And totally reproducible,
as I have [uploaded the R notebook I used to create all the included images to github](https://github.com/mountainMath/density-explorations).

Feel free to clone or fork it and adapt it for your own purposes.
