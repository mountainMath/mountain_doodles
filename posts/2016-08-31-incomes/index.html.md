---
title: Quick Guide to Income Data
author: Jens von Bergmann
date: 2016-08-31
categories:
  - Vancouver
slug: incomes
image: "images/yvr_income.png"
tags: []
description: 'Progressing past medians.'
featured: ''
featuredalt: ""
featuredpath: ""
linktitle: ''
type: "post"
aliases:
  - /blog/2016/08/31/incomes/
---




The news media has an extraordinarily difficult time with income data. And that's only partially the media's fault.
In general, income data is a funny beast. So I decided to give a quick overview over income data that's available through
Statistics Canada. To make things a little more concrete I focused on Vancouver as an example, but everything applies
equally well to any other area in Canada.

Most journalists I talked to care about accurate and relevant statements. If that's you, you will need to keep reading
and dive down the slightly wonkish income rabbit hole.

For those that don't really care, I made a
*quotable random income generator*. Just hit the "generate" button copy & paste the result and link this post as source.
(Use at own risk!)

<!-- more -->

<style>
.income-generator div div {
  display:none;
}
.income-generator.active div div {
  display:inline;
}
.income-generator {
  padding:10px;
  broder-radius:2px;
  border: 1px solid grey;
  background: lightgrey;
}
</style>

<div class='income-generator'>
<div class='income-choices'>
<div class="geo"></div>
<div class="med_av"></div>
<div class="tax"></div>
<div class="hh"></div>
<div> income is </div>
<div class="income"></div>
</div>
<a class="btn btn-default" style="background:orange;">Click to Generate Random Income Numbers</a>
</div>
The income numbers don't fit the story? No problem, just play again until you get something you like!

## Geographic Regions
The first thing to do when quoting income numbers of any sort is to decide what region to use. Seems straight forward
enough, but in reality [hardly ever done](https://twitter.com/vb_jens/status/744407354867736576). In the Vancouver
context, it simply comes down to deciding if one wants to
report on income for the City of Vancouver or Metro Vancouver. Both are frequently referred to as just "Vancouver". Which
is just fine as long as it is clear from the context which region we are talking about. And more importantly, whatever
the income is compared to (e.g. housing price data) are taken for the same region.

## Base Demographics
Next we have to choose a base demographic we are interested in, choose which income distribution is relevant for our context.
In some cases this choice is obvious. If we are writing a story on lone parents in the City of Vancouver, we would want
to look at the income distribution of lone parent households. If we are looking at the development of wages, we should
consider individual incomes. For a story on consumer spending we may want to look at after-tax income. 
 
We may also be interested in specific type of income, for example only in investment income. Or the net capital gains
and losses. Or pension benefits. Or self-employment income. 
 
## Statistic
Often we would like to distill everything down to a single number. Typically we would choose the average or median
income from the distribution we are interested in, but we may also choose other statistics, for example the top quintile.
Which one we choose really depends on the story. As a rule of thumb, choose median incomes if you are interested in the
'typical' person or household. Choose average income if you are interested in the questions relating to the total amount
of money earned in a given area. Income quantiles are great to focus in on certain subgroups of the population.
 
## Availability
Generally we also have to consider the availability of information. The census has very detailed income data in their
standard release that allow us to be very specific about what kind of incomes we are interested in. And if that's not
enough we can make a special request to StatsCan to narrow the incomes down to a group we are interested in, for example incomes
of renter households as was done in the [rental housing index](http://rentalhousingindex.ca).

Annual income data is available from CANSIM (Statistics Canada's socioeconomic database), but the geography is coarser
than in the census. Generally we can only
expect to get annual data for the whole country, provinces, census metropolitan areas, but not census
subdivisions (municipalities) or finer geographies.

## Sources and Comparability
All income data eventually come down to CRA taxfiler data. But the devil is in the details. For the 2006 and 2011
censuses, there was a mixture between self-reporting and cross-referencing of individual level income data with CRA data. 
And even self-reported data would again undergo quality control and get somewhat adjusted according to CRA data.

But there are important differences in the data, both on what kind of income is included and also what people are included.

The details are endless, many of them won't matter. Here are a couple to look out for:

* CANSIM is a comprehensive sample, census data is roughly a 1 in 5 subsample which may lead to sampling bias especially
 in small geographies.
* CENSUS data is sensitive to non-return bias. Not just no-return bias introduced by the NHS, but also by census undercounts
which, for example, were in the 10% range for the 25 to 29 year old demographic in both the 2006 and 2011 censuses. 
* CANSIM and the Census have different denominators for reporting average or median incomes. CANSIM reports on all
T1 taxfilers, as well as various family types. Some data can be broken out into age groups at
the 15 and 65 year cutoffs, 
whereas Census reports only on people (15 or older) (or families or households) in "private households" with income greater
than zero. 
* the definition of "private household" changed between 2006 and 2011 censuses.
* 2011 and 2006 censuses differ slightly in what is included as income, for example
[2006 included RRSP withdrawls](http://www12.statcan.gc.ca/census-recensement/2006/ref/dict/pop123-eng.cfm),
[2011 didn't](https://www12.statcan.gc.ca/nhs-enm/2011/ref/dict/pop123-eng.cfm) (both include pensions and RRIFs). 
2011 includes net capital gains and losses as a percentage of total income in a separate category, 2006 did not
include any information on capital gains. 
  
So even when comparing CANSIM income data to census income data for the appropriate year we should expect some differences.
Or comparing 2006 to 2011 income, even if hypothetically an area had identical tax returns, we would still see differences.
Generally speaking, CANSIM average and median income estimates are lower than census estimates as it includes a larger pool
of people that are likely to have lower income.

## Where to get the goods?
<img  src="images/yvr_income.png" style="width:50%;float:right;margin-left:10px;">
The latest census income numbers are available via [Statistics Canada](https://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/prof/index.cfm).
I am biased and prefer the [CensusMapper](https://censusmapper.ca/maps/467) interface. For 2011 data, choose any
2011 CensusMapper map, select the geographic
region you are interested in and press the "more" button on the popup to bring up the "census wheel". Then drill into the
"income" section, the last NHS variable in the widget.

CANSIM has annual data based on taxfiler information for coarser geographies. So you won't be able to get data for the City of Vancouver
or neighbourhoods within the city like with census data, but you can get data for Metro Vancouver. And you can get it for
more recent years. Things get complex very fast there are
[213 CANSIM tables with income data](http://www5.statcan.gc.ca/cansim/a33?themeID=3868&spMode=tables&chunkSize=213)

At CensusMapper we are still firmly planning integrate CANSIM data into the CensusMapper interface, but their API refresh got pushed back
to sometime 2018 and we can't justify the extra resources to integrate CANSIM data temporarily using the old API just
to switch it over a year later.

## Too Abstract?
Want to see how this works on an example? Just read the
[next post on using income data for housing affordability](http://doodles.mountainmath.ca/blog/2016/09/14/measuring-housing-affordability/).



<script src="/lib/jquery.min.js" charset="utf-8"></script>
<script>
var geoData=['City of Vancouver','Metro Vancouver'];
var med_avData=['median','average'];
var taxData=['pre-tax','after-tax'];
var hhData=['individual','household','family','family with children'];
$('.income-generator .btn').on('click',function(e){
     $('.income-generator').addClass('active');
     $('.income-choices .geo').text(geoData[Math.round(Math.random())]);
     $('.income-choices .med_av').text(med_avData[Math.round(Math.random())]);
     $('.income-choices .tax').text(taxData[Math.round(Math.random())]);
     $('.income-choices .hh').text(hhData[Math.round(3.9999*Math.random()-0.5)]);
     $('.income-choices .income').text('$' + Math.round(80*Math.random()+40)+',000');
});
//$('.income-generator .btn').click();
</script>
