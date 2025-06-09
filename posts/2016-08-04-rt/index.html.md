---
title: What's up with RT?
date: 2016-08-04
categories:
  - Vancouver
  - zoning
slug: rt
author: Jens von Bergmann
tags: []
description: 'Scraping Mobi data.'
featured: 'rt_teardowns.png'
image: "images/rt_teardowns.png"
featuredalt: ""
featuredpath: "/images"
linktitle: ''
type: "post"
aliases:
  - /blog/2016/08/04/rt/
---



RT is Vancouver's zoning for duplexes. Over time, various areas have been zoned to allow duplexes. Examples are
Kits Point, much of Point Grey Road reaching up to Broadway, much of Granview-Woodlands, parts of Mount Pleasant and many
other areas.

Recently I have had some interesting conversations [on Twitter regarding RM-6](https://twitter.com/fixbced/status/748196970783637504)
and over BBQ dinner about RT-7. Then the 
[Granview-Woodland plan](http://vancouver.ca/home-property-development/grandview-woodland-community-plan.aspx)
passed by council, and it contains a curious provision of reducing the outright FSR for the
RT-zoned properties from 0.6 to 0.5.

All of which got me thinking. What is RT supposed to accomplish, how does the diverse RT-zoning rules influence development
and how is RT overall performing?

<!-- more -->

To make things complicated many RT zoning rules contain provisions to allow for higher FSR, and more than two units,
under the "heritage preservation" program. Most people like the multi-plex developments that come out of this process,
but the road there is quite adurous. Permitting takes a long time and is marred by uncertainty. Those projects are
done by small developers, that are ill-equipped to deal with risky and drawn-out rezoning processes.

These would be great projects carried out by small developers that lead to gentle and affordable density,
as this kind of re-development does not require land-assembly and does not lead
to crazy land-value lifts that require CACs to claw back some of that value rise
[as we explained before](http://doodles.mountainmath.ca/blog/2016/04/01/on-dirt-and-houses/).

We have some ideas what a refreshed RT (and RS) should look like, and there are
[some efforts to push for changes](https://pricetags.wordpress.com/2016/07/29/open-letter-on-the-downzoning-of-grandview-woodland-rt/).
We leave the details of this to the experts and take a little field expedition to
see how the different flavours of RT (and RM) perform in data.  

## Why RT?
One central question to ask is: What is the purpose of RT (or any) zoning? The initial idea was to allow higher density
than RS, with two stratified properties on one lot. But since the creation of RT the RS zoning has undergone significant
changes and allows, in many cases, for higher FSR and higher unit count, although not stratified. In much of RS three
units are allowed on each lot, the main house, a secondary suite and a laneway house.

<a href="https://mountainmath.ca/map/assessment?filter=[zone_RT_RS_FSD,residential]&zoom=13&lat=49.245&lng=-123.1166&layer=5&mapBase=2" target="_blank"><img  src="images/rs_rt_land.png" style="width:50%;float:left;margin-right:10px;"></a> 
So how does RT perform in the wild? There seems to be little effect RT vs RS zoning has on land values. The 
[boundaries between RS and RT](http://mountainmath.ca/map/assessment?filter=[zone_RT_RS_FSD,residential]&zoom=13&lat=49.245&lng=-123.1166&layer=6&mapBase=2)
can't be discerned from a
[land value map of all residential properties in RS, RT (and First Shaugnessey) zones](https://mountainmath.ca/map/assessment?filter=[zone_RT_RS_FSD,residential]&zoom=13&lat=49.245&lng=-123.1166&layer=5&mapBase=2).

How good are RT lots at realizing "zone capacity", that is how many of the historically SFH get turned over into duplexes,
or in some cases multi-plexes (through the heritage presercation program)?
That depends as the following graph shows.
<div style="margin:10px 50px;padding:5px;border: 1px solid black;border-radius:5px;">
<div id="graph_sfh" style="height:200px;max-width:640px;" data-url="/data/zone_sfh.json"></div>
</div>

<a href="https://mountainmath.ca/map/assessment?filter=[zone_RT]&zoom=13&lat=49.2581&lng=-123.1166&layer=20&mapBase=2" target="_blank"><img  src="images/types_rt.png" style="width:50%;float:left;margin-right:10px;"></a> 
We can also study this by mapping out
[all residential lots in RT zones](https://mountainmath.ca/map/assessment?filter=[zone_RT]&zoom=13&lat=49.2581&lng=-123.1166&layer=20&mapBase=2),
and color them depending if they are single family lots, duplexes or multi-plexes.

Some RT areas are essentially indistinguishable from RS areas, they are almost entirely comprised of single family lots. 
A good example is the
[RT-4, most of which is in Grandview-Woodlands](https://mountainmath.ca/map/assessment?filter=[zone_RT-4,residential]&zoom=14&lat=49.2733&lng=-123.0646&layer=20&mapBase=2).
And being in RT rathern than RS they often don't even get the benefits of a laneway house. Coach houses, which are legal
in many RT areas and could be stratified, come with such onerous side setbacks that they are very hard to build on
regular lots.

There is a lot of untapped "zoned capacity". Similarly, some of the
newer RM zones show little appetite for realizing "zoned capacity", possibly due to the added difficulty of having to
assemble lots to do so.

One can argue about whether this is good or bad. Or one can look at the variation between the different types of RT and RM
zones to see what causes the difference.

## Downzoning
To understand how downzoning works we want to consider two different types on analysis. One is to look at what happened
after RS zone was changed to reduce site coverage in the 1980 in response to concerns about 'monster homes'. We ran
an analysis using LIDAR data to understand the effect on the physical form of SFH and found that while site coverage
indeed decreased sharply, the [bulk of the building remained unchanged](http://doodles.mountainmath.ca/blog/2016/03/05/physical-sfh-form-over-time/).
and the rule change seems to have done little in easing concerns about 'monster homes', it simply changed some of the
parameters determining what 'monster homes' look like.

Another way to look at the matter is to compare how re-development happened in difference zones. This is very hard, since
there are many factors other than zoning that determine if a building gets re-developed. To get some idea we compiled two
numbers for each of the RS, RT and RM zones (ignoring some of the delicacies in the zoning code and lumping together all
sub-zones with the same leading number. So for example we lumped RT-4, RT-4A, RT-4N and RT-4AN into one category we simply
label "RT-4".
           
First we check how many residential properties got re-developed since 2000.             
<div style="margin:10px 50px;padding:5px;border: 1px solid black;border-radius:5px;">
<div id="graph_redevelopment" style="height:200px;max-width:640px;" data-url="/data/zone_redevelopment.json"></div>
</div>

Next we take a look what percentage of the existing residential stock is in immediate danger of being torn down, using the
[methodology developed earlier](http://doodles.mountainmath.ca/blog/2016/01/18/redevelopment/).
<div style="margin:10px 50px;padding:5px;border: 1px solid black;border-radius:5px;">
<div id="graph_teardown" style="height:200px;max-width:640px;" data-url="/data/zone_teardowns.json"></div>
</div>


While there is some correspondence between re-development activity and ratio of teardowns, there are a number of notable
exceptions. RT-1 and RM-9 stand out, but these are 
[oddball cases with just a few properties](https://mountainmath.ca/map/assessment?filter=[zoneE_RM-9_RT-1]&zoom=14&lat=49.2146&lng=-123.1282&layer=5&mapBase=2).

<a href="https://mountainmath.ca/map/assessment?filter=[zone_RT,years_2000]&zoom=13&lat=49.2581&lng=-123.1166&layer=20&mapBase=2" target="_blank"><img  src="images/new_types_rt.png" style="width:50%;float:right;margin-left:10px;"></a> 
We can also map just [residential properties in RT that were re-developed since 2000](https://mountainmath.ca/map/assessment?filter=[zone_RT,years_2000]&zoom=13&lat=49.2581&lng=-123.1166&layer=20&mapBase=2)
and colour them by SFH, Duplex and Multi-plex status. We can clearly see how some RT areas, like RT-7, don't see much
re-development, others, like RT-6 see mostly multi-plexes being developed. Some zones, like RT-10, 11, 12 are too new
to be judged on development happening since 2000. For example, zooming in on 
[developments in RT-10 since 2009 paints a very different picture](http://mountainmath.ca/map/assessment?filter=[zone_RT-10,residential,years_2009]&zoom=15&lat=49.2485&lng=-123.0734&layer=20&mapBase=2).

There is lots of stuff to explore, but for today I want to look at RT-7 and RT-6 in more detail.


### RT-7
<a href="https://mountainmath.ca/map/assessment?filter=[zone_RT-7,residential]&zoom=15&lat=49.2617&lng=-123.1654&layer=20&mapBase=2" target="_blank"><img  src="images/rt-7.png" style="width:50%;float:left;margin-right:10px;"></a> 
A much more interesting case is RT-7, with 35% of properties teardowns but only 4.1% of properties re-developed since 2000.
[Looking at the map](https://mountainmath.ca/map/assessment?filter=[zone_RT-7,residential]&zoom=15&lat=49.2617&lng=-123.1654&layer=4&mapBase=2)
we see that RT-7 is comprised of two pockets on the west side, and the teardown candidates stand out in red and orange.
Filtering further to see what
[has been re-developed since 2000](https://mountainmath.ca/map/assessment?filter=[zone_RT-7,residential,years_2000]&zoom=15&lat=49.2617&lng=-123.1654&layer=20&mapBase=2)
we see that, considering the size of the two pockets, re-development favours the eastern pocket near 16th and Arbutus. We
can clearly see that that the eastern pocket has an overall more valuable building stock, although it is not clear if that
is due to higher rates of re-development or for other reasons, for example the presence of some slightly larger lots. We
also note that re-development does produce duplex units and even some multi-plex.
 
So what is going on here? Looking at the [RT-7 zoning](http://former.vancouver.ca/commsvcs/BYLAWS/zoning/rt-7.pdf) we
see that the area has been downzoned to 0.4 FSR as was rencently pointed out to me, which can conditionally be upzoned
to 0.6 FSR. Additionally, there are caps on the number of units per hectar to further restrict density.

### RT-6
<a href="https://mountainmath.ca/map/assessment?filter=[zone_RT-6,residential]&zoom=16&lat=49.2606&lng=-123.1097&layer=20&mapBase=2" target="_blank"><img  src="images/rt-6.png" style="width:50%;float:right;margin-left:10px;"></a> 
Another interesting example is RT-6. RT-6 has seen modest levels of re-develpment and has a [relatively healthy building
stock](https://mountainmath.ca/map/assessment?filter=[zone_RT-6,residential]&zoom=16&lat=49.2606&lng=-123.1097&layer=4&mapBase=2).
What's even more interesting is that it contains
[many multi-plexes](https://mountainmath.ca/map/assessment?filter=[zone_RT-6,residential]&zoom=16&lat=49.2597&lng=-123.1101&layer=20&mapBase=2).
And more importantly, [developments since 2000 have mostly produced multi-plexes](https://mountainmath.ca/map/assessment?filter=[zone_RT-6,residential,years_2000]&zoom=16&lat=49.2597&lng=-123.1101&layer=20&mapBase=2).

Prime examples of the *gentle density* that we keep hearing about. I would say that something is working very well here. 

So what's the difference? In contrast to RT-7 [RT-6 zoning](http://former.vancouver.ca/commsvcs/BYLAWS/zoning/rt-6.pdf) allows for 0.6 FSR
outright, conditionally increased to 0.75. But there are other important differences too. The RT-7 lots are typically
smaller with many at about 340m&sup2; compared to the 580m&sup2; typical RT-6 lots.
(Although RT-7 also contains some 580m&sup2; lots).


## Conclusion
Does downzoning work? It depends what the goal is. Looking at the RT-7 example, downzoning has slowed re-development compared
to other areas in the city, but it also lead to a deterioration in building stock in RT-7. This is a stop-gap measure,
eventually that lower value building stock will get re-developed.

The combination of larger lots and the heritage retention program in the RT-6 zoning seems to work in producing
gentle density, except that the permitting process takes too long and it is not immediately clear what purpose the
heritage preservation has that allows stripping the building down to the studs and possibly moving it to the front and
then building an infill in the back. There must be a better way to deal with concerns behind
heritage preservation (not much is "preserved" in this process) and at the same time cut down on the time it
takes to jump through all the permit hoops involved.

It is clear that RS and RT (and RM) zoning would hugely profit from a clearer vision what these zonings should accomplish
and by using data to benchmark how these targets are met.

<script src="//d3js.org/d3.v3.min.js" charset="utf-8"></script>
<script src="/lib/jquery.min.js" charset="utf-8"></script>
<script>

function bar_graph(div,shiftAxis,domainFormatter,rangeFormatter,domainLabelFormatter,rangeLabelFormatter){
    if (!domainFormatter) domainFormatter=d3.format("d");
    if (!rangeLabelFormatter) rangeLabelFormatter=rangeFormatter;
    if (!rangeFormatter)
     rangeFormatter = function (y) {
        return y;
     };
     if (!domainLabelFormatter) domainLabelFormatter=domainFormatter;

var margin = {top: 20, right: 20, bottom: 40, left: 70},
    width = parseInt(div.style("width")) - margin.left - margin.right,
    height = parseInt(div.style("height")) - margin.top - margin.bottom;

var x = d3.scale.ordinal()
    .rangeRoundBands([0, width], .1);

var y = d3.scale.linear()
    .range([height, 0]);


var xAxis = d3.svg.axis()
    .scale(x)
    .tickFormat(domainFormatter)
    .orient("bottom");


var yAxis = d3.svg.axis()
    .scale(y)
    .orient("left")
    .tickFormat(rangeFormatter)
    .ticks(5, rangeFormatter);

var svg = div.append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var data_url=div[0][0].dataset.url;

d3.json(data_url, function(error, json) {
  if (error) throw error;
  var graphData=json[0];
  var data=graphData.data;
  
  var container=d3.select(div.node().parentNode);
  container.selectAll('.legend.no-margin').remove();
  var legend=container.append('div').attr('class',"legend no-margin");
  legend.append('p').html('<i style="background:'+graphData.color + '"></i>' + graphData.label +  '<span style="float:right;margin-right:10px;" id="' + graphData.class+'_value"></span>');
  
  x.domain(data.map(function(d) { return d.date }));
  y.domain([0, d3.max(data, function(d) { return d.count; })]);
  
  var domainTickValues=[];
  var skip=Math.round(40/x.rangeBand());
  if (skip<=0) skip=1;
  for (var i=0;i<x.domain().length;i++) {
    if (i % skip==0) domainTickValues.push(x.domain()[i]);
  }
  if (x.domain().length % 5 !=0) domainTickValues.push(x.domain()[x.domain().length-1]);
  xAxis.tickValues(domainTickValues);

  var xShift=shiftAxis ?  x.rangeBand()/2.0 * 1.1 : 0;
  
  svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(" + xShift + "," + height + ")")
      .call(xAxis);

  svg.append("g")
      .attr("class", "y axis")
      .call(yAxis);
//    .append("text")
//      .attr("transform", "rotate(-90)")
//      .attr("y", 6)
//      .attr("dy", ".71em")
//      .style("text-anchor", "end")
//      .text("Probability");

  svg.selectAll(".bar")
      .data(data)
    .enter().append("rect")
      .attr("class", graphData.class + " bar")
      .style("fill", graphData.color)
      .attr("x", function(d) { return x(d.date); })
      .attr("width", x.rangeBand())
      .attr("y", function(d) { return y(d.count); })
      .attr("height", function(d) { return height - y(d.count); })
      .on('mouseover',function(d){
         d3.select('#'+this.classList[0]+'_value').text(domainLabelFormatter(d.date) + ': ' + rangeLabelFormatter(d.count)) 
      }).on('click',function(d){
       d3.select('#'+this.classList[0]+'_value').text(domainLabelFormatter(d.date) + ': ' + rangeLabelFormatter(d.count)) 
      }).on('touch',function(d){
         d3.select('#'+this.classList[0]+'_value').text(domainLabelFormatter(d.date) + ': ' + rangeLabelFormatter(d.count)) 
      }).on('mouseout',function(){d3.select('#'+this.classList[0]+'_value').text('')});

      
});

}



var percentageFormatter=d3.format(".1%");
var textFormatter=function(d){return d};
var teardownLabelFormatter=function(d){return percentageFormatter(d) + " teardowns"};
var sfhLabelFormatter=function(d){return percentageFormatter(d) + " SFH"};
var redeveopmentLabelFormatter=function(d){return percentageFormatter(d) + " built since 2000"};
bar_graph(d3.select("#graph_redevelopment"),false,textFormatter,percentageFormatter,textFormatter,redeveopmentLabelFormatter);
bar_graph(d3.select("#graph_teardown"),false,textFormatter,percentageFormatter,textFormatter,teardownLabelFormatter);
bar_graph(d3.select("#graph_sfh"),false,textFormatter,percentageFormatter,textFormatter,sfhLabelFormatter);
</script>
