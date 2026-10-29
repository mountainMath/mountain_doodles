---
title: "Dot-density drawn from distributions"
author:
  - name: Jens von Bergmann
    affiliation: MountainMath
date: '2026-10-28'
slug: dot-density-drawn-from-distributions
description: 'Working with addresses and ecological inference'
image: 'images/viz-min-dots.png'
categories: 
  - geeky
  - census
bibliography: ../../common_literature.bib 
code-tools:
  toggle: true
fig-width: 8
execute:
  cache: false
  message: false
  warning: false
---


We have been longtime fans of dot-density maps, [@bartholomew-s-dot-density.2021] especially multi-category ones. [@dot-density.2017] For this post we are thinking of (multi-category) dot-density maps based on census data, for example visualizing population density by drawing dots where (we think) people live, or colouring dots by people's characteristics, like income or visible minority status.

Their main advantage is that they tend to be a lot easier to interpret than choropleth maps, but this comes at a cost. They suggest a level of precision that the data simply does not warrant. This provides an interesting challenge: How to leverage the intuitiveness of dot-density maps without misleading readers.

We have grappled with this over the years, the old CensusMapper had an implementation of dot-density maps, and we have a messy R package to automate some of the common issues when drawing static dot-density map. But all of these past approaches have left us somewhat unsatisfied.

Now we have built a new prototype that we feel a lot better about. We leverage advances in technology, advances in data and data processing capabilities, and some new ideas. And we want to share this approach to get some feedback before integrating it into the new CensusMapper.

If you are interested to hear about how the map was done, continue reading.


[Just show me the map already.](https://mountainmath.ca/dots/?spec=visible_minority&layout=synthetic#10.2/49.247/-122.9841){.btn .btn-primary}


# Philosophy

To start we need to be honest about what dot-density maps are. Dot-density maps are so intuitive to read because drawing a dot on a map is a very strong symbol that is easily associated with an individual person in a particular location. For example, when drawing a dot for each person coloured by visible minority status, we immediately grasp densities of different visible minority groups by observing changes in colour patterns across regions. Associating a non-visible minority person with a grey dot, a person listing Chinese as their visible minority status with a red dot, is very legible and easy to interpret.

The problem is that interpretation is fundamentally incorrect. When working with census data we generally only know something about how many people of a given visible minority group live in a particular region, but we know nothing about where in that region they live. When zoomed out so that the regions are small on the map that does not matter much, but when zooming in the intuitiveness, ease of interpretation and power of the dot as symbol for a person with particular traits becomes a liability. This is the tension dot-density maps need to navigate.

In our prototype we address this tension by postulating a simple principle. **A draw of the map is a draw from a distribution.** The distribution is a distribution of traits in locations in each particular region. And at fine zoom levels dot-density maps should visualize distributions. We do this by regularly re-drawing the map, each time re-drawing from the distribution of likely dot placements and associated traits. And we also do this after every zoom or pan interaction. Space is the dimension where we draw a particular instantiation from the distribution, time is the dimension through which we express the probabilistic nature of the distribution.

This might seem a bit annoying and uncomfortable at first. At high zoom levels a user might inspect their particular block and see people and their traits on their street. But after a while, right now set to an 8 second interval, the map re-draws and the image of the traits changes. This is deliberate, the 8 seconds are a heuristic that allows users to understand an instantiation of the distribution in broad terms, but does not give enough time to associate particular traits to a particular address. The former is something we want, the latter we want to avoid.

It might still feel a little spooky when people see dots drawn on their own or their neighbours' homes. The random 8-second re-draws aren't what preserve privacy, they are just a reminder that dots should not be taken literally. What preserves privacy is StatCan disclosure control that only releases aggregate data, none of our modelling, including the ecological inference model we describe below, can undo that. 


The central remaining question is the exact nature of the distribution we are drawing from. A priori we know nothing about this, and it seems like drawing from a uniform distribution is the best we can do. But we think we can do better in two main ways. We do have information about where people live: at residential addresses. And we have information about how people likely distribute into households and how traits distribute within and across them. As a simple example, children under 15 are very rarely found in households that don't have adults in them too. 

So instead of drawing from a random distribution of where we place dots and how we associate traits to dots, we place dots more deliberately where people likely live, and associate traits in ways that reflect better how these traits may likely distribute. Again, thinking of these choices as distributions we draw from, except these aren't uniform distributions but informed by strong external priors. This makes each draw of the map more legible, while communicating the probabilistic nature via the time dimension.

# The making of the dot-density prototype

The philosophy that *a draw of the map is a draw from a distribution* is not really saying much new, lots of people have grappled with dot-density maps and had similar thoughts and implemented some of them into their maps. We have implemented this in some way in different maps we made. But recently a couple of things came together for us that gave this new clarity. We want to walk through these pieces one by one.

## Dot placement

Dot placement is crucial to making maps intuitive. Based on the data we are interested in, census data, we take a staged approach to dot placement.

### Dwellings

The census enumerates where people live, dwelling units should form the basis for dot placements.

Census data gives us aggregate data for each region, dot-density needs to translate that into positions of people, households, or dwelling units. A simple approach is to randomly distribute these in each region, and that's something we have done in the past. One challenge that comes with this is that it places dots into regions where we know nobody lives, like parks or industrial areas, which is confusing to viewers and disorienting for e.g. large parks in urban areas. In the revamped CensusMapper we are overlaying large parks and industrial areas on choropleth maps to add that context. Similarly we can avoid placing dots into those regions.

But can we do better? Yes, we now have a range of much more precise data where people live. StatCan publishes the **National Address Register** (NAR) which, while far from perfect, is slowly converging to a useful base of residential (and other) addresses. Additionally, this allows us to (somewhat imperfectly) identify apartment buildings where many residential addresses stack on top of each other. Similarly, it also carries information on secondary suites and laneway houses. However, there are gaps and issues, some regions have poor coverage, geocoding is missing from some addresses, and there are issues around the timing, earlier versions of the NAR, like the 2022 version that aligns closer with the 2021 census, had much larger gaps and problems.

Some NAR problems, like systematic shift in geocoding in the City of Toronto, are fairly easy to deal with once detected by applying a uniform correction.[^1] Others, like missing rural Quebec addresses, can be backfilled with data from **Adresses Québec**.

[^1]: Ideally this problem will get fixed upstream by StatCan, and we need to ensure that our data pipelines pick up on that if and when it does.

The next problem comes with census geocoding not always aligning with the NAR, especially in Toronto there are noticeable mismatches between census counts and NAR addresses that can't be explained away with differences in timing. Here we apply a simple heuristic that re-assigns data from neighbouring **Dissemination Blocks (DB)** if they show clear patterns of geocoding issues across a common boundary, similar to how we implemented [geocoding anomaly detection and correction](https://mountainmath.github.io/tongfen/articles/tongfen_anomalies.html) in the TongFen package. [@vonbergmann2020]

But this won't cover all cases. Sometimes the NAR will have too many addresses for the (private) dwelling count in the census, and sometimes it's vice versa. Timing and collective dwellings can explain some of that, but not all. Here we need to make judgement calls and place dots, or fail to place dots, using other means. We use **NRCan's CanVec** hydro layer to identify water where nobody lives. The **GHSL EMC-BUILT (Copernicus/JRC)** layer from 2020 classifies the globe by built-up area and residential use on a 10m grid that can help further rule out areas where nobody lives, and pinpoint where people live. Taken together we get a fairly good idea where dwelling units are located.

For parts that are still left over we distribute the dwellings uniformly in the respective region with water cut out.

The entire processing pipeline might seem complex, but most regions are just fine using the NAR and we focus the imputation on the other parts. Some of this work we have already done when building the [cangeocode package](https://mountainmath.github.io/cangeocode/) and some with previous work of cutting water layers out from census regions for better visual representation in the old CensusMapper, and some during extensive prior work with various GHSL datasets. [@city-density-patterns.2019; @density-timelines.2019; @satellites-sprawl-and-city-six-packs.2021] Basically, all these pieces were already in place and just needed to be slotted appropriately into a data processing pipeline to build a Canada-wide distribution of dot placements, for the vast majority corresponding to locations of actual dwelling units, and consistent with census counts.

One important note is how we treat apartment buildings, seen in our address-based dwelling location data as several dwelling units with the same geographic location and high multiplicity. We code them in the data just like that, a single location with multiplicity, and when drawing them on a map we fan out the dwellings in a Vogel spiral. This is visually appealing and makes apartment buildings easily recognizable.^[In past visualizations we have [randomly drawn them in a circle around the location](https://mountainmath.ca/ns_cap), which works too but feels visually busy.]

That enables us to draw maps based on dwellings.

{{< video images/dwelling-top.mp4 >}}

### Households {#sec-households}

A household is an occupied dwelling unit. [@what-s-a-household.2017] When mapping household based data we are faced with the task of deciding which dwelling units are *occupied by usual residents*, and which ones aren't. We don't have any reasonable way to know which dwelling unit holds a household and which one doesn't, so we draw from a uniform distribution to decide within each DB. This is our first level random draw, and we re-draw from the distribution for every draw of the map.

### People

People live in households, but how many people live in a particular household? We resolve this at the DB level and the dwelling locations within. A simple approach to this is to assign a person to each household and then randomly assigning the leftover population, respecting the constraint given by the total number of people. But that would end up making households look unnaturally uniform. While we don't have information on particular household sizes for each dwelling unit, selecting them fairly uniformly does a poor job of visually conveying how people distribute over households. 

One refinement is to better model the actual distribution of household sizes we see in the census. For this we can also use information on the multiplicity of addresses at each location, e.g. distinguish how we treat a highrise tower which shows up with 100 addresses pinned to a single location, vs a single detached house with one address per location, and so forth. This matchup is not perfect, for example bare land strata can show up as a single location with multiplicity for each single detached house in the strata, but it gives good enough information in the vast majority of regions that it's worth exploiting it. Glossing over details, in each DB we take stock of dwelling locations and their multiplicity, select households among the dwelling units as in [the households section](#sec-households), take hard-coded observed household size distributions from the census for all of Canada for different dwelling types, and scale these so that the totals match up in each DB. Then we scale the means of the household size distribution so that it matches the leftover population and assign the leftover population by drawing from a Weibull distribution with those scaled household sizes and designed to capture the spread seen in overall Canada, rounded systematically to ensure totals reconcile.

That might sound complicated, but is simple to implement and works well in practice.

What's still left to decide is how to visualize the people. One simple solution is to draw Vogel spirals with people around each location. That works well for multiplicity one locations like single-detached homes, but blurs the distinction between households and people within households. And loses information about which dwelling units are households in higher multiplicity locations. Another option is to assemble households in clusters arranged in ring form within the household positions in the Vogel spiral. That gets visually busy again, but allows us to show likely household structures within apartment buildings.

We aren't entirely convinced which of these options is better, so we left the ability to select *dots* or *clusters* in the prototype map.

### Traits

When showing traits, like structural type of dwelling or tenure for households, or visible minority status or income group for people, with each category getting a different colour for our multi-category dot-density map, we need to decide how to assign colours. These variables are only available at the **Dissemination Area (DA)** level and we distribute them for all dots within a given DA at a time. The conservative approach is to distribute them uniformly across all dots in each DA.

While this is simple and does not require any additional assumptions, it's somewhat unsatisfactory. With households we already decided that showing realistic distributions of household sizes is useful in giving viewers a more accurate impression on what the actual distribution of household sizes might look like. Similarly, we can do better when showing traits. People in households are indistinguishable, but households are already distinguished by household size. And locations are distinguished by multiplicity. So the problem we are facing when we want to show more realistic distributions of traits is an **ecological inference problem** that assigns aggregate distribution of traits to how these distribute over people in households. This gets complicated fast, and warrants its own section.

## Ecological inference model

There are a number of ways how one might want to go about building an ecological inference model, it's important to keep the purpose of the model in mind. For this use case, the purpose is to show realistic possible distributions of the given traits within and across households. Whether this is useful depends crucially on whether the distributions we are showing reflect how people actually distribute over housing. Our ecological inference model needs to be realistic enough to be useful.

At the same time, the model evaluation needs to be fast enough so it can work in an interactive visualization, where we draw from the distribution on every map interaction and every 8 seconds without map interaction, without feeling too laggy.

To accomplish this we decided to build one relying on the 2021 Census hierarchical **Public Use Microdata File (PUMF)** as a strong prior. In practice, what this model does for a given DA is to re-weight the PUMF based on a *base selection* of traits^[For the current model the traits used in this step are household size and type, tenure, dwelling type, age × gender, visible minority status (yes/no), immigration, mobility, household income + log households, log persons per household, log density + province and CMA indicators.] of the DA, creating a distribution of households whose expected value matches that of those traits. We then draw households from that distribution to create synthetic households, with an accounting step at the end that might make some changes to ensure that the totals for the traits we are showing on the map match the DA counts.^[We do make some exceptions in some cases, recognizing the census counts are (randomly) rounded and small counts are shrunk to zero.]

When we show traits that aren't part of this *base selection* of traits but are in some way encoded in the PUMF we add on an adjustment to the re-weighting process that dynamically tilts the resulting distribution toward the expectation values seen of the traits we want to show.

If we show traits that aren't captured in the PUMF we still use the synthetic households based on the *base selection* of traits to model households, but then distribute the traits randomly among the persons in each household. In some cases we might use auxiliary variables that strongly correlate with these traits to help distribute them.

The end result is a much more realistic representation of how people, and their traits, distribute over households in a given DA.

{{< video images/viz-minority-dots.mp4 >}}


### How does the EI model work?

We only give a rough outline. The core question is how to re-weight the PUMF to get a realistic distribution of households for a given DA. For the *base selection* of traits we train a small neural net to do this. The main reason for this choice is that this is fast to evaluate, so it won't be a problem doing this in the browser, simple to train, and fairly robust and able to deal with DA level data suppression and other quirks. 

The neural net we use is a small multi-layer perceptron with around 100 input and output units and two hidden layers of 256 units each, plus a linear skip connection, about 125 thousand parameters in total. It turns the *base selection* of traits, expressed as shares, into *tilts* that are used to re-weight the PUMF. We then ship the weights to the browser and evaluate the neural net in plain javascript.


When mapping traits outside of the *base selection* we add an exponential tilting of the model's distribution that satisfies the shares of the traits in question in the given dissemination area. This will incur some extra compute time, roughly doubling the time needed to draw a DA on the first draw, but can be cached and re-used for subsequent re-draws. In more technical terms, the traits outside of the *base selection* impose the additional constraint on the PUMF distribution that it satisfy the counts in the DA in question, we re-interpret this as a Lagrange multiplier problem for minimum-KL re-weighting of the PUMF distribution.


Here we glossed over some details, how to deal with suppressed data or small categories that systematically round to zero. We also skipped details on how the neural net is implemented, and variations on how to tile the PUMF distribution for traits outside of the *base selection*. The prototype map implements a range of options for the latter that aren't transparent to the user and mostly for testing purposes. Consolidating this into a useful overall spec that can handle any type of variables one might want to map is still outstanding, but required before carrying this over to CensusMapper. 


## The front end

Now that we have a basic idea how the dot-density map should work we need to implement that in a performant way. In hindsight, the technology is remarkably simple. At least when standing on the shoulders of several pieces of technology that others built. All it takes is stringing them together in the right way.

For the mapping engine, we use MapLibre as the base with deck.gl to drive the data layer. This follows what we are doing in (the revamped) CensusMapper. 

The data we need to ship is the location of the dots, their multiplicity, and geographic identifiers to link it to census data. And then of course the census data to link to the dots. It's similar to CensusMapper, but instead of polygon regions we are sending dots.

One big difference from CensusMapper is that we are not tiling the data layer. The main reason is that DAs can span more than one tile, and when drawing a dot-density map for a DA in one tile, we need to know what's going on in other tiles that overlap the same DA. And that gets complicated very fast.

Instead we ship dots wholesale, not for the entire country because that's quite a heavy load, but one census division at a time. There are about 300 census divisions, and shipping the dots ranges from a couple of kilobytes for northern census divisions to 2 megabytes for Montréal. To know which census division we need data for we also ship census division geographies that the map checks against its current bounds, which is cheap. We then store those in **Origin Private File System (OPFS)**, so they only need to get fetched once. When zooming out it's ok to only grab a sub-sample, we implement that as 1:10 nation-wide sub-sample. When drawing dots we can, and do, downsample further if it makes sense. 

That covers the dots, to get the census data we additionally do targeted range requests to the full census profile data parquet files, targeted at the variables we need for a given map. We could use a proper database setup here, but that's not going to save us much download bandwidth and adds complexity that really isn't needed for this. Parquet is amazingly efficient at this, especially if tuned for this purpose. We make an exception for the *base selection* vectors and ship these in a separate parquet file for efficiency.

All these data downloads are cached in OPFS, if we need more variables we grab just the ones we don't have and store them in a separate parquet file. If too many parquet files accumulate we consolidate them in the browser OPFS. If too much data accumulates over time we evict data that we haven't used for the longest.

For managing that data and fetching data for the locations in the map window we use DuckDB-WASM, a fast implementation of DuckDB in the browser. It has some rough edges, but works well for this.

At high zoom levels, starting with zoom level 16, we then add the ecological inference model to sort people into households and traits to people. That's a fairly pedestrian affair that gets settled in javascript. This requires fetching the model weights, which only needs to happen once, and the *base selection* census data which happens a census division at a time, depending on which parts of Canada people are viewing.


# Upshot

We are quite satisfied with the result, it feels like this is a useful way to show multi-category data. It's very intuitive and easy to interpret, responds reasonably well to user interactions, and regularly re-drawing from the distribution does a decent job at communicating the probabilistic nature of the map and prevents people from associating traits too closely to any particular home or location while still giving a realistic view of how traits like incomes, religion, age, or visible minority category likely distribute in a given area.

We are still going to make changes before porting this to CensusMapper, feedback is welcome. Check out the [live version](https://mountainmath.ca/dots/), play with the available map options and at high zoom level check out the difference between the three layouts, the [*dots*](https://mountainmath.ca/dots/?layout=dots&spec=visible_minority#16.44/49.217405/-123.102414), [*household clusters*](https://mountainmath.ca/dots/?layout=clusters&spec=visible_minority#16.44/49.217405/-123.102414) and [*synthetic households*](https://mountainmath.ca/dots/?layout=synthetic&spec=visible_minority#16.44/49.217405/-123.102414). That last one is the ecological inference model. You can add "pretty=true" to the url parameters to get rid of the debug information and the select if you just want to share a pretty version of a specific map.

And as a throwback to the past, we have added a [trick-or-treat density map](https://mountainmath.ca/dots/?pretty=true&spec=trick_or_treat&layout=synthetic#3.28/51.63/-98.12).



