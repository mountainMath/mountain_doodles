---
title: Land Use, Roads (and Parking)
date: 2016-02-29
categories:
  - Vancouver
  - land use
  - Transportation
slug: land-use
author: Jens von Bergmann
tags: []
description: 'How do we use land in Metro Vancouver?'
featured: 'land_use_van.png'
image: "images/land_use_van.png"
featuredalt: ""
featuredpath: "/images"
linktitle: ''
type: "post"
aliases:
  - /blog/2016/02/29/land-use/
---



The other day at the [SFU's City Conversations](https://www.sfu.ca/publicsquare/upcoming-events/city-conversations/2016/Feb-18-2016.html)
someone asked a question about space dedicated to roads, and how that could be unlocked
to aid housing. He mentioned what percentage of space is currently dedicated to roads. I forgot the number,
but I thought to myself that I should look that up for all Metro Vancouver communities. So here we go.
<!-- more -->

Actually, it would be interesting to compare how land is allocated to all kinds of land uses across Metro Vancouver, not just roads.
And with their excellent
[land use dataset](http://doodles.mountainmath.ca/blog/2016/01/31/land-use/) it's easy enough to do, justifying spending
two hours on this. [Looking at the land use map](https://mountainmath.ca/land_use/map)
one can see that there are some issues with using the dataset for 
that purpose, for example the roads within
Stanly Park are missing, as parking lots in Vancouver parks. So this slightly overestimates the green space. But when
added up this won't do much to change the overall area of each land use.

## Land Use Breakdown
<link rel="stylesheet" href="/css/custom.css">
Let's start with a simple chart summing up the land use in each of Metro Vancouver's Municipalities. Just select the one
you are interested in from the dropdown.
<div id="land_use_breakdown" class="land_use"></div>

City of Vancouver stands out as the municipality with the largest proportion of area dedicated to roads right of way.
The right of way includes nature strips, sidewalks, and of course on-street parking. 


## Built Area Land Use
For some of the municipalities large "Natural Areas" and "Undeveloped Area" make it difficult to discern the makeup of
the built up areas. So here comes the same graph with some of the large space-cosuming categories taken out.
<div id="land_use_breakdown2" class="land_use"></div>

What's remarkable is that City of Vancouver still is the clear winner in terms of space taken up by roads right of way. 


## Roads right of way
It might be interesting to compute more precisely how the roads right of way is used. But that requires a lot more work.
The City of Vancouver has a dataset with road widths, which would help separate area taken by roads from area taken by
sidewalks and nature strips. One could use google maps to estimate to estimate the amount of space taking up by on-street
parking by sampling a couple of roads and scaling this according to road area. Too much work for me right now, maybe one day I will
have a good enough reason to dedicate some time to this.

## Parking
Parking in particular is a land use that is artificially inflated. We pay lots of money to buy private property to live
on, and then continue to pay property taxes for that privilege. But we pay nothing to store our vehicles on public roads.

At current land values in Vancouver it just does not make any sense to socialize the cost of parking. A 12m&sup2; on-street
parking space at a low-balled $3000/m&sup2; value is worth $36,000. If we were to price parking at value, rather than
socializing the cost we could have a discussion about using that space more effectively. Killing the parking subsidy
would have a substantial impact on demand (and car ownership), freeing up space to be re-allocated for other uses. For
example for housing by increasing lot sizes and making multi-unit structures more feasible, especally on corner lots.

<hr>
## Update
Saw a [great little data visualization fly by today](https://twitter.com/d3visualization/status/705421356222029824) that
is just the missing link in the above visualizations. A simple way to visualize where all the municipalities stand in
relation to one another. Thought I would throw that in real quick.

<div>
<div id="radviz" class="radviz"></div>
<div class="radviz-list-container">
<div class="muni"></div>
<div class="list"></div>
</div>
<div style="clear:both;"></div>
</div>

<script src="//d3js.org/d3.v3.min.js" charset="utf-8"></script>
<script src="/lib/jquery.min.js" charset="utf-8"></script>
<script src="/js/land_use_breakdown.js">
