---
title: Trick-or-Treat 2016
author: Jens von Bergmann
date: 2016-10-21
categories:
  - CensusMapper
slug: trick-or-treat-2016
tags: []
description: 'Halloween Mapping Fun.'
image: "images/tot_density.png"
featured: 'tot_density.png'
featuredalt: ""
featuredpath: "/images"
linktitle: ''
type: "post"
aliases:
  - /blog/2016/10/21/trick-or-treat-2016/
---




A year ago, as were were just getting CensusMapper up and running, we put out three Halloween-themed census maps. Those
maps almost broke our servers when they went viral. At least as viral as census data goes. They were viewed by over
150,000 Canadians over the course of three days. And many of those came back to view the maps more than once.

Lots of things have happened at CensusMapper since last year, and we heeded the call and put some of CensusMapper's prowess
to use to make some important improvements for this Halloween.

<!-- more -->
We are stuck in a weired place this year. We just completed the
[most successful census in the Canadian history](https://twitter.com/NavdeepSBains/status/770281404848566273) this May,
but the results are still undergoing quality control and will only get released starting this coming spring. The 2011
census, that last year's maps are based on, is now 5 years old. Some children that were of prime trick-or-treating age
(which we take as 5 to 14 years old) in 2011 are now in College. And some that weren't even borne in 2011 will be out
knocking on doors this Halloween.

As people move around, the composition of neighbourhoods does not change all that much over time. Young kids grow up to take
the place of the older kids in the neighbourhood. That was our take last year, as we were still mapping 2011 data to
give people an idea how many kids would show up on their door or where to expect the most foot traffic.

## The maps
Last year we made three maps, the
[Trick-or-Treat Density map](https://censusmapper.ca/maps/137) that visualized the expected trick-or-treat foot traffic,
the [Trick-or-Treat Onslaught Map](https://censusmapper.ca/maps/136) that visualized how many kids to expect at your
door and the [Haunted House Map](https://censusmapper.ca/maps/138)
that's mapping homes occupied by ghosts and other "unusual residents".

This year we improved on those maps by building a dynamic estimator that estimates conditions on the ground this Halloween. We
have:

<table>
<tr>
<td style="width:25%;padding:3%">
<a href="https://censusmapper.ca/maps/529" target="_blank"><img  src="images/tot_density.png" style="width:100%"></a>
</td>
<td style="width:25%;padding:3%">
<a href="https://censusmapper.ca/maps/528" target="_blank"><img  src="images/tot_onslaught.png" style="width:100%"></a>
</td>
<td style="width:25%;padding:3%">
<a href="https://censusmapper.ca/maps/138" target="_blank"><img  src="images/tot_haunted_houses.png" style="width:100%"></a>
</td>
<tr> 
</table>

* <a class='btn btn-success' href="https://censusmapper.ca/maps/529" target="_blank">Trick-or-Treat Density Estimator</a>
that estimates the number of children of prime trick-or-treat age per area, so the expected trick-or-treat foot traffic
in each area. Use this map if you plan on taking your kids out trick-or-treating and want to make sure you find the area
in your neighbourhood with the most kids out on the street.

* <a class='btn btn-success' href="https://censusmapper.ca/maps/528" target="_blank">Trick-or-Treat Onslaught Estimator</a>
that estimates the number of children of prime trick-or-treat age per doorbell. Use this map if you are guarding the door
and want to know how much candy you should have ready.

* <a class='btn btn-success' href="https://censusmapper.ca/maps/528" target="_blank">Haunted Houses Map</a> that shows
the houses that were occupied only by ghosts or *unusual residents* in May 2011. As ghosts and *unusual residents* tend
to be restless we were not comfortable making estimates where they may be now,
even with all of CensusMapper's power behind it. So we only have the 2011 map to give some inspiration where the best
chances might be to run into ghosts or cross paths with *unusual residents*.


## How did we estimate things for 2016 Halloween?
An skillful estimator strikes a balance between simplicity and effectiveness. We decided to make use of the
following data:

* The number of children in the age brackets 0-4, 5-9 and 10-14 in each area in 2011.
* The number of children in the age brackets 0-4, 5-9 and 10-14 in each area in 2006.
* The percentage of the population in each area in 2011 that did not move between 2006 and 2011.
 
Based on this, we split the estimate of the number of children in each area in 2016 into two groups. Children that
already lived in the are in 2011 and still live there in 2016, and children that moved into the area between 2011 and 2016.
We estimate the proportion by using the proportion of people that did not move between 2006 and 2011 as a proxy.

Then estimating the number of children that did not move is easy, we simply age the children we saw in 2011 forward by
five years and weight them by the proportion of non-movers.

To estimate the number of children moving into the area take into account at the trends from 2006 to 2011 in each area
and extrapolate from there to 2016. And weight the number by the proportion of the population that did move and also
allow for some population growth according to the 2006 to 2011 trend.
 
I will spare you the exact formula. It is possible that refining the model and adding in more variables could make some
improvements to the estimates, but overcomplicating the model also leads to risks of exaggeratig biases. We feel that
our simplistic model strikes a good balance between simplicity and effectiveness. It will be interesting to see how we
did when 2016 data comes in.

## The Super Nerdy Details
For the data nerds still reading, the CensusMapper servers actually don't do any of that work. They just server straight-up
census data. Plus the instructions of how to use the data to make the map. All the computations and drawings are then
done locally on each user's browser. That's what keeps our servers
lean, responsive, and most importantly maintains maximum flexibility. With the same process we can easily map any function
built from census variables. Canada-wide and across all the entire geographical census hierarchy, from all of Canada down
to Dissemination Areas and, for some data, even Dissemination Blocks. Bwetween the 2006 and 2011 census
variables and all the different geographic regions we have about 1 billion fields in our database that we can map
dynamically. Still a far cry from "big data", but enough to require some careful architecture choices.
 
## Census Mapping for Everyone 
Census data can be very complex and mixing various variables is powerful yet prone to produce meaningless maps due to
misinterpretation of the census variables. Since last Halloween and now we are proud to have been able to open up some
of CensusMapper's capabilities to the general public. Everyone can now make Canada-wide interactive maps based on single
2011 census variables via a couple of mouse clicks and share the freely. You can read more about this
[in a previous blog post](http://doodles.mountainmath.ca/blog/2016/05/04/census-mapping-for-everyone/) or
[jump right in and make your own map](https://censusmapper.ca/maps/new).

## FAQ
Here are some of the questions we got most last year:

### Are the maps only for Vancouver?
No. All CensusMapper maps are fully interactive and by default Canada-wide. Use the search function or geo-location
button to switch to any place in Canada that tickles your interest. Pan and zoom around to explore. You can always share
the current view of the map with your friends by grabbing the URL in your browser address bar or using the build in share buttons. 
 
### What's the difference between the Trick-or-Treat Density and the Trick-or-Treat Onslaught map?
The maps are related, both map the number of children of prime trick-or-treat age, but they are normalized differently.
The Density map considers the number of children per area. This estimates how many you will see out on the street in the area.
The Onslaught map considers the number of children per door, so you can estimate how many will come knocking.
The difference can be seen in high density areas like Yaletown in Vancouver. There are a lot of kids living in Yaletown
in a fairly small area, so the expected foot traffic is high. But these kids distribute over the units in the highrises
there and on a per household (or per door) basis, the number of kids per household does not stand out. 

### How accurate are the predictions?
Accuracy of predictions vary. Some parents will drive their kids halfway across town to find the best area for
trick-or-treating. Some areas change too fast for our estimator to catch on. And some neighbourhoods have traditions and
local areas where kids congregate that we don't have data for. So these maps are best use in conjunction with local data.

### Won't your maps lead to more people trick-or-treating outside of their neighbourhood?
While our maps can be used to facilitate driving kids across town to trick-or-treat, we encourage everyone to also
use the map to find areas near
where they live, and where the kids can walk to. We believe an important part of the Halloween experience is for everyone
to get to know their own neighbourhood better, see things in a new light and experience the
joys of running into friends and acquaintances, and making new friends, along the way. 

### I am running out of candy / I got way too much candy!
The Onslaught map just estimates the average number of children that knock on doors in each area. Some doors will see a
lot more kids than others. Take the Yaletown example from above. Most
of the action happens on the street. So while on average people can only expect a moderate number of trick-or-treaters
at their door, the ones at ground level will be *very* busy, while the ones higher up will likely just see one or two
neighbour kids from the same floor.
