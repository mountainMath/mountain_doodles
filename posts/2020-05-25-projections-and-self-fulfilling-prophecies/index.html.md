---
title: Projections and self-fulfilling prophecies
author: 
  - name: Jens von Bergmann
    affiliation: "MountainMath"
  - name: Nathan Lauster
    affiliation: "UBC, Department of Sociology"
date: '2020-05-25'
slug: projections-and-self-fulfilling-prophecies
categories:
  - cancensus
  - cmhc
  - Vancouver
tags: []
description: "Housing and population growth are endogenous in high-demand areas. Which gives cities the tools to exclude people, but should they? Deciding how to grow is a values question, not a technocratic one."
featured: ''
image: "index_files/figure-html/cov-vs-metro-pop-growth-1.png"
featuredalt: ""
featuredpath: ""
linktitle: ''
type: "post"
aliases:
  - /blog/2020/05/25/projections-and-self-fulfilling-prophecies/
---




<p style="text-align:center;"><i>(Joint with Nathan Lauster and cross-posted at <a href="https://homefreesociology.com/2020/05/25/projections-and-self-fulfilling-prophecies/" target="_blank">HomeFreeSociology</a>)</i></p>





When people want to live in your city, how many should you let in? On the one hand, this is a moral question. Do you have an obligation to people who don't already live here? On the other hand, it's a moot question. At least in Canada, cities don't have the power to control migration. 

BUT WAIT! Cities DO have power over how many new dwellings to allow. This actually changes our moral question a bit. Cities can't keep people out, but because they have power over dwellings, municipalities can control how many people get to remain in. As a result, if you don't allow any new dwellings when people want to live in your city then rich people will generally outbid poor people for the housing that's left.

It may be the case that municipal politicians are fine with rich folk replacing the poor folk in their cities while their own housing rapidly appreciates in price. Why let any new housing get built? "No thanks, we're full!" But they can't always SAY this. Especially in cities full of renters that generally support progressive and inclusive values.

So what to do? Two paths are readily available. One: transform the moral question ("isn't it terrible that developers make money off building housing?") Two: turn the moral question into a narrow technocratic one instead. Let's explore this latter option a bit more, because it's really interesting and sits well within our wheelhouse (mathematician and demographer). 

Here in the City of Vancouver, a new motion was just launched, titled [Recalibrating the Vancouver Housing Strategy](https://council.vancouver.ca/20200527/documents/pspc4.pdf) (RVHS). There are some good initiatives in this motion, but the main thrust and motivation is to turn the moral question of how many people get to remain in Vancouver into the narrow technocratic question of how do we forecast population growth? As any demographer can tell you, this can be tricky, especially when it comes to forecasting for municipalities. But there's a naive kind of work-around some people use when they don't follow demographic techniques and concerns very closely and don't want to think too hard about the question at hand. They simply turn the population forecast into a projection forward from how a city grew in the past. 

This is a neat trick! Especially if you're in a city that's limited new dwellings in the past and thereby kept its population growth to a minimum and you want to keep it that way. "The evidence suggests we haven't been growing very fast, so we shouldn't add much more housing." With a little bit of hand-waving, the number of dwellings allowed by the city is reimagined as something that can be tailored to meet the forecast rather than the central determinative factor of the forecast.

Is this the kind of thing that could happen in Vancouver? Before we get into the motion, let's just quickly look at Vancouver's recent past. We know prices and rents rose rapidly through 2016 (and beyond), which is pretty good evidence that we didn't add enough housing for the people who wanted to live here all by itself. But how did the City of Vancouver grow relative to the rest of the region? It grew more slowly. ("No thanks! We're full!") Did we lose poor people and replace them with rich people as a result? Yap, this is [exactly what has happend in the City of Vancouver, which has lost lower and middle income people, and gained high-income people, at a faster pace than the surrounding Metro area.](https://doodles.mountainmath.ca/blog/2017/09/26/evolution-of-the-income-distribution/)

![](/posts/2017-09-26-evolution-of-the-income-distribution/index_files/figure-html/2005-2015_rel_change-1.png)

## The Motion
Now let's get back to that RVHS motion, starting with [part A](https://council.vancouver.ca/20200527/documents/pspc4.pdf#page=2):

>    THAT Council direct staff to revisit the Housing Vancouver Strategy targets to align with historical and projected population growth based on census data.

This is a vague statement. There are, of course, many ways to "align" something (Dungeons and Dragons fans may be immediately reminded of the nine different alignments readily found therein). There are also many ways to project population growth. These often rely upon multiple sources of data. Birth rates, death rates, age structure, labour market statistics, and net migration rates serve as typical baseline sources of information for demographers, and are usually gathered from all manner of data (e.g. vital statistics, surveys, policy-based immigration projections, etc.) rather than simply historical census data. So how is the author of this particular motion imagining more specific alignments and projections? The answer can probably be found in the *WHEREAS* [sections 4 and 5](https://council.vancouver.ca/20200527/documents/pspc4.pdf):

>    Population growth has been consistent at approximately 1% per annum over the past 20 years according to Statistics Canada census data. Based on this historical trend, a similar growth rate for the coming decade would amount to a population increase of around 66,000. In the City of Vancouver, the average household size is 2.2 individuals per dwelling unit (or “home”);
    
>    The target of 72,000 new homes across Vancouver in the next 10 years multiplied by 2.2 would mean a population increase of 158,400 - more than twice the historical rate. A projected historical rate of population growth would imply instead a need for roughly 30,000 new housing units over the coming decade;

We've left the refined techniques of demography behind here, as well as the determinative forces of births, deaths, and moves. Indeed, people pretty much disappear and their dwellings get only scare-quotes as *homes*. But let's follow the math we do get and try and understand what projecting past trends means in terms of numbers (leaving aside if we agree that things went splendid and we should just keep going the same way). Let's try and reproduce the estimation of new housing units assuming we hold the 20 year trends in the two mentioned metrics, population and household size, constant.

The 1% annual growth rate roughly checks out, although there have been variations. 



<img src="index_files/figure-html/cov-vs-metro-pop-growth-1.png" width="768" />





And population in the City has grown consistently at a lower rate than overall Metro Vancouver population. In fact, if the City of Vancouver had grown at the same rate as Metro Vancouver over those 20 years, Vancouver would have had roughly 60,000 more people within city limits in 2016. But maybe people would just rather live farther out in the surrounding suburbs? Again, there are variations, but overall that is not what the price and rent data tell us.


<img src="index_files/figure-html/unnamed-chunk-3-1.png" width="768" />

People want to live in Vancouver. But they often settle for living farther out, based on the specifics of what they want and can afford. The competition for the limited number of dwellings in Vancouver drives up prices here relative to surrounding municipalities. 

So what to make of the close relationship between population growth and dwelling units added? It's a real relationship.

<img src="index_files/figure-html/unnamed-chunk-4-1.png" width="768" />


The motion, as presented, seems to suggest that this close relationship is evidence that we're projecting population growth really well, thereby allowing almost perfectly enough new housing to meet population needs. Is this what we're doing? Well, no. In fact, the amount of new housing allowed sets a cap on population growth that can only be exceeded by increasing household size (which in many cases cities have also made illegal)^[For example the City of of Vancouver only allows at most one kitchen per dwelling unit and limits the number of unrelated individuals sharing a dwelling to 3 (+ 2 boarders or lodgers) to restrict sharing of homes.] or decreasing the number of empty dwellings.

There is broad support for decreasing the number of empty dwellings, and both the City of Vancouver and the Province of British Columbia have put in place taxes on vacant properties and their owners to do just that. Have they succeeded? Quite possibly! But compared to other municipalities, Vancouver's vacancies (as recorded in the Census) [looked relatively normal](https://homefreesociology.com/2019/08/19/running-on-empties/) prior to the new taxes, despite persistent rumours of some [mythical oversupply](https://doodles.mountainmath.ca/blog/2020/01/27/mythical-oversupply/). After the new taxes, [administrative](https://vancouver.ca/files/cov/empty-homes-tax-annual-report.pdf#page=5) [data](https://news.gov.bc.ca/files/SVT_Consultation_All.pdf#page=84) reveals there aren't many taxable units left vacant at all (~1%).

What about household size? The motion suggests imposing a constant for Vancouver, expecting 2.2 people per household. But household size is not staying constant. It's [falling all across Canada](https://www150.statcan.gc.ca/n1/pub/11-630-x/11-630-x2015008-eng.htm), due to a combination of forces (aging of the population, declining childbearing, changes in partnership, the rise of people living alone). We also know that as people get richer, they tend to occupy more space. And, as pointed out above, Vancouver's been getting richer.  


<img src="index_files/figure-html/unnamed-chunk-5-1.png" width="768" />








As we see, household size in the City of Vancouver has continuously declined over the years, a trend that has significant impact on the relationship between housing and population growth. Sticking with the bad assumption that past population growth should be predictive of future housing needs, we can see that we're still going to need more housing per person than in the past. Projecting these trends forward, lazily anchored at the 2016 census data, gives an increase in population in private households of about 67,000 and a corresponding increase in 41,000 households (aka occupied dwelling units). And that is not yet accounting for the increase in population in non-private households that Vancouver has experienced, like retirement homes or similar institutional housing.

So if the RVHS motion points us toward a bad way to do population projections, then how should one do it? There are lots of models to look at, but given that people want to live in Vancouver, a key ingredient in any model should be how much housing will be allowed. Conditional on allowing a given amount of housing, we can attempt to forecast how many people will come. But this moves us back from narrow technical questions (which we're more than happy to continue exploring in depth!) toward the central moral question at hand. How many people are we comfortable allowing to live in Vancouver? Because if we allow more housing, more people will come. And if we allow more housing, we'll also allow more of those currently at risk of feeling unwanted in Vancouver to stay.


That begs the question: **What would be the problem with allowing more housing?** The last *WHEREAS* of the RVHS motion holds an answer to that.

>    A revised and more accurate understanding of demographic needs and demand will assist in properly planning for the post COVID-19 reality. Setting excessively high targets will pressure the City of Vancouver to grant significant amounts of density at a low price, in an attempt to induce housing construction approaching the HVS targets. This will cost the City of Vancouver potential revenue, and will mean that the City abandons its commitment to having growth pay for itself.

In short, housing might get cheaper. Which incidentally is quite in line the goals of the Vancouver Housing Strategy. 

But there are a couple things here that need a bit more unpacking. First, from the title throughout the motion and showing up here again are mentions of planning for a "post COVID-19 reality." To put it bluntly, this is odd. These parts of the motion caution us against assuming what comes next will reflect what came before. But, as discussed above, this is exactly the assumption the rest of the motion says we should make, resting as it does upon a very selective reading of Vancouver's recent population growth. Weird contradiction. But then again, pretty much the same language [has been employed way before COVID-19 was on anyone's radar](https://vancouversun.com/opinion/op-ed/elizabeth-murphy-vancouver-growth-targets-dont-add-up/), suggesting that COVID-19 has just been tacked on for extra effect.

Second, the notion that "growth pay for itself" sounds quite reasonable, but it's not clear what that means in practice. In Vancouver, new housing projects pay a variety of municipal fees, DCLs, CACs and additional engineering fees upfront, and annual property taxes thereafter. How much of the overall cost of living in the city should be charged upfront, and how much should be charged over the lifetime of the housing as property taxes? That's a political question that Vancouver should have a discussion on. 

Charging high entry fees keeps prices high, not just of new housing but of all housing. It **encourages treating housing as an investment**, with low holding costs (property taxes) and high barriers to increasing housing even as population pressures keep prices and rents rising.

Charging a lower entry tax and collecting a higher portion as property taxes later can lower the entry point to housing and spreads the costs out over the lifetime of the dwelling unit. This **treats housing as a place to live**, lowering the barriers to new housing construction and asking people to pay for city services and amenities over their time living in the city.

## The (sort of) good parts of the motion
Let's end with a few bright notes. There are some good parts to the motion! We like data and Part B asks:

>    THAT Council direct staff to provide annual historical data since 2000 on the number of units approved through rezoning, the breakdown of housing types that have been approved, housing starts and net housing completions, and estimated zoned capacity for the City of Vancouver.

This part of the motion is asking for better data, but it needs refinement. As it is right now it is hard to see what it will accomplish. 

**Number of units approved through rezoning** is hard to interpret unless it is accompanied by more detail on how many of these units actually got built. Take the approved first version of the Oakridge development for example. A massive number of units got approved, yet the project died when drilling found an aquifer that precluded the project from going forward as approved. Several years later, a different proposal got approved, for the data on approvals to be useful we need to know what happened to those units.

Monthly data on **housing starts** is already easily available, asking the data be reproduced adds zero value and amounts to a waste of staff time.

**Net housing completions** is an important number, but very hard to do in Vancouver, given our high reliance on informal housing. It is still worthwhile to try and approximate this, but the motion should be clearer what part staff should focus on beyond the data on completions, demolitions and secondary suite estimates that we already have.

**Estimates of zoned capacity** is a great stat to get clarity on. Some vague estimate has been making the rounds for a while after surfacing in a consultant report, with next to no detail how it was derived. Having an estimate with a clear methodology would be a great addition to inform Vancouver housing policies.


Part B is a good and simple ask:

>    THAT Council direct staff to clarify whether the Vancouver Housing Strategy targets refer to net housing completions or gross housing completions.

Part E is mostly redundant:

>    THAT Council direct staff to provide detailed inventory data through the Open Data Portal4 of housing starts, development projects anticipated in the pipeline (including form and type of units), and existing zoned capacity (disaggregated by local area) to inform this work.
    
The open data portal already has detailed information on housing units in the pipeline. The information could be improved, but this ask is useless unless it specified how. As mentioned before, detailed information on housing starts is already easily available as open data, monthly stats by structural type and intended market, down to the census tract level. It is less helpful than the other parts above and risks directing staff resources away from other project just to replicate what's already out there.

## Bottom line
There's no way around it. How many dwellings to allow in a city is ultimately a moral question rather than a technocratic one. Given the overwhelming evidence that people want to live in places like Vancouver, population forecasts necessarily reflect first and foremost how many new dwellings we're willing to allow. In technical terms, it's silly to imagine we're meeting the needs of population growth when we're in fact setting a hard cap on population growth. In moral terms, we come back to the central question: Are we planning for kicking poor people out? Or are we open to inviting more people in?  


As usual, the code underlying the stats and graphs is [available on GitHub](https://github.com/mountainMath/doodles/blob/master/content/posts/2020-05-25-projections-and-self-fulfilling-prophecies.Rmarkdown
) for anyone to reproduce or appropriate for their own use. And if you want to read (much) more about how to know if you have enough housing, check our [simple metrics post](https://homefreesociology.com/2019/06/12/simple-metrics-for-deciding-if-you-have-enough-housing/). 



