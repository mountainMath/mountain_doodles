---
title: Bike Data
author: Jens von Bergmann
date: 2015-12-15
categories:
  - OSM
  - bikes
slug: bike-data
tags: []
description: "The bike data I want"
featured: 'bike-routes2.png'
image: "images/bike-routes2.png"
featuredalt: ""
featuredpath: "/images"
linktitle: ''
type: "post"
aliases:
  - /blog/2015/12/15/bike-data/
---




Maps live and die with the quality
of the underlying data. So I decided to dive a little deeper down the OSM bike data rabbit hole. Task number one was
to expand display data for a wider region. My primitive workflow to pull data out of OSM only allows for extracting a quarter
of a degree at a time. For playing around with all of Vancouver's data I again turned to Mapzen for their
[metro extracts](https://mapzen.com/data/metro-extracts) as a convenient shortcut for OSM data. For the case of Vancouver
the word "metro" is a bit of an exaggeration, it just covers the City of Vancouver. But good enough for some more testing.

## More data
<!-- more -->
More data means a a larger variety of [bike infrastructure types](http://wiki.openstreetmap.org/wiki/Key:cycleway).
At first glance tagging seems to be reasonably accurate, but on closer inspection one quickly spots lots of issues. 
First off, the [edits Alex made earlier](https://twitter.com/alexwarrior/status/675820327331479552) are <strike>not
showing up, that's because the metro extracts are only done weekly. Just a matter of time.</strike> showing up now after
updating with new OSM data.

And then there are lots of little issues. Off street paths
don't connect to roads (or anything for that matter), making the useless for routing. Some changes in the bike network
are tagged differently, take Point Grey Road as an example. Some paths are labeled to be ok for bikes, but bikes aren't
allowed there. Some tags seem off.

And I don't seem to have captured all relevant bike infrastructure, will have to spend some time one of these days to check
through all the different taggings in OSM to make sure I pull out all the correct ones.

So to make more sense out of the network I made a new map, added some more categories for optionally fading them out when
displayed. And on hover I display the tags, just for interest.

## Improving data
This gets us to task number two. The folks at Mapzen gave me some friendly pointers to allow easy editing of features.
So I pasted a couple of lines of code in so if you hold down the shift key and
click on a feature, it will take you automatically to OSM to edit the feature. You may have to sign in the first time
you do this. If you hold the shift key while clicking anywhere else it also takes you to the OSM editor for that spot,
in case you want to add something there. It takes a lot of the pain out of editing bike infrastructure, I just fixed a
whole bunch of things in short time.

<iframe src="/html/bike_map2.html" width="100%" height="550"></iframe>

Go for it and fix some problems that you see, either by shift-clicking on the embedded map or by
[taking the map full-screen first](/html/bike_map2.html). Remember that some of the issues may be fixed already, the bike map will not reflect
updates until a week later or so.

## Where to go from here?
It looks like making a decent bike map with OSM data is feasible. The hard work will be to collaboratively do all the
OSM edits required to get the data into good shape.
 
One problem is that edits won't show up on the bike map for up to a week, that's the frequency at which the metro extracts
are updated. And then I have to update the file for displaying the bike data. A minor inconvenience, in theory there
are ways to seed this up if one wants to be serious about this. But then again, once the OSM bike data is fixed up in a
given region, it won't need updating very frequently. 

I guess I will have to mull this over and decide how deep down this rabbit hole I would like to go. Looking back at my
[rant on what's wrong with most bike maps](http://doodles.mountainmath.ca/blog/2015/12/13/how-to-make-a-bike-map/) I am
asking myself how much the current map has accomplished.

1. The accuracy of the infrastructure mapped is in the hands of the OSM community. With easy access to the OSM editing
functionality from the map (desktop only) it's in the hands of the people that know best: cyclists.
2. Still coming up blank when it comes to showing comfort level. Will have to think more how to best do this, it might
have to go into a separate database. Which is doable, but requires work to get it right and is probably a little too
involved to do it off the side.
3. Click and zoom is not an issue, but now I am in the opposite corner where it might be nice sometimes to actually have
a glossy big paper map. And it's really easy to save the map as an image. Some tinkering should be able to produce a nice
high-resolution one covering a large area.
4. The map already adapts to user preferences quite well, different types of infrastructure will fade out if deselected.
5. Not on the original list, but routing should be part of this. Routing works reasonably well to get this off the ground,
but work is still required to make it work properly with user preferences. This may well require running a custom router,
again a little more than I bargained for.

So is this really worth the trouble to make yet another bike map? I am not sure. It's always easy to through up a quick
proof of concept, but to do it really well requires quite a bit more work. For now I could just build one focused on
Vancouver and see how it goes. And it would be great if the comfort level rating could somehow be automated.
   
<img  src="images/cycletrack.PNG" style="width:50%;float:right">Which brings to another one of pet projects that
I never took beyond the testing realm. My CycleTrack App that records
all bike trips in the background (as long as you carry your phone with you). No pressing of "start" or "stop" buttons,
the app notices when you are moving and will take gyroscope and accelerometer readings to figure out if you are cycling,
running, walking, driving or taking a train. Then it stores your cycling trips and computes aggregate data. It avoids
using GPS so not to drain your battery too much, on a typical day it will consume about 3% of battery power. The downside
is that the accuracy and frequency of the location updates is not as high, so things get a little messy. But not too bad. 

How does this fit into the bike map project? Simple. If one can collect regular cycling data from normal people
cycling (not just
the lycra crowd that presses start cycles in circles and presses stop again and recharges their phone while taking a shower),
once can infer a lot about comfort levels just by looking at the data. And to collect data from regular "citizen cyclist"
one cannot expect them to press "start" and "stop" to delineate their bike trips, and one cannot have an app that will
require them to recharge their phone after every trip.

But then the project gets even bigger...
