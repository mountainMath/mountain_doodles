---
title: Bike Routing
author: Jens von Bergmann
date: 2015-12-14
categories:
  - OSM
  - Mapzen
  - bikes
slug: routing
tags: []
description: "Tweaking bike routing"
featured: 'bike-routes2.png'
image: "images/bike-routes2.png"
featuredalt: ""
featuredpath: "/images"
linktitle: ''
type: "post"
aliases:
  - /blog/2015/12/14/routing/
---



Routing is a hard problem. Routing for drivers is pretty good at this point, mostly because we have been very good at
designing for cars and creating predicable infrastructure. Routing for bikes is a whole other story, data quality is
poor and the physical infrastructure is, at least in North America, not strongly predictive of cycling comfort/safety.
And cycling comfort/safety is the top priority for the
[vast majority of (potential) cyclists](http://usa.streetsblog.org/2015/03/13/the-first-ever-nationwide-survey-of-interested-but-concerned-bikers-is-here/).

And it's the ones that don't cycle frequently, often out of concern for safety, that would benefit most from effective
bike routing.

Read on or [go directly to the routing demo](/bike_routing.html).
<!-- more -->
Google does a decent job directing a relatively experienced cyclist from A to B, but it has a hard time to learn about
places where cyclists can go but cars can't. And it won't be able to answer my fundamental question: *Can I bring my 6
year old along?*. And Apple doesn't even try and offer bike routing.

## Routing test
So what's really needed apart from [better bike maps](http://doodles.mountainmath.ca/blog/2015/12/13/how-to-make-a-bike-map/)
is better bike routing. So building on yesterday's post, I decided to take a quick look at routing. Time to try out 
[Mapzen's routing engine](https://mapzen.com/projects/valhalla) which, as expected, was really easy to set up:

Feel free to drag the endpoints to test your favorite routes.

<iframe src="/html/bike_routing.html" width="100%" height="550"></iframe>
[Full screen view](/html/bike_routing.html)

Initial testing seems to indicate that this works reasonably well. And while the engine allows for some customization
on rider needs, right now there is no way to get the "dad's routing" that I would like to have. 

Part of the problem is of course that I still don't have enough information in OSM to even make a "dad's map"
[as I lamented earlier](http://doodles.mountainmath.ca/blog/2015/12/13/how-to-make-a-bike-map/). But at least OSM gets
me half-way there by giving me a finer control over distinguishing infrastructure that I may deem as generally more
suitable so that I can fade selected bike infrastructure out by checking the appropriate boxes in the map.

## Route costing options
The bike routing options in Mapzen's routing engine allow for some level of control on wheter gravel should be avoided
(great feature for the lycra cowed but useless for dads), whether hill should be avoided (helpful) and whether to avoid
roads without bike infrastructure. But when I cycle with my 6 year old a bike lane between parked cars and 50 traffic
is as good as no bike infrastructure at all. And there is currently no way to cost different types of bike
infrastructure, so they can't be used as a proxy for cycling comfort.
 
## Where to go from here
Next steps are to [look deeper into Mapzen's routing engine](https://github.com/valhalla/thor) and see how hard it would
be to hack some of these more advanced costing options into their routing engine and open up a feature request.

Wrapping up the three-night trials in bike mapping is the
[post on data](http://doodles.mountainmath.ca/blog/2015/12/15/bike-data/).




 
