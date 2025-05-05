---
title: On Condos
date: 2016-02-10 11:01:47 -0800
categories:
  - Vancouver
  - Assessment Data
  - geeky
slug: on-condos
author: Jens von Bergmann
tags: []
description: 'What can open data say about condos?'
featured: 'strata.png'
image: "images/strata.png"
featuredalt: ""
featuredpath: "/images"
linktitle: ''
type: "post"
aliases:
  - /blog/2016/02/10/on-condos/
---





Lots has been said about the upper end of owned dwellings. The movement of the "million dollar line", the emergence of
the "two million dollar line" and "multi-million dollar lines". Most of that discussion is focused on
<a href="http://mountainmath.ca/map/assessment?filter=sfh" target="_blank">single detached homes</a> or on proxies for "single detached" like
<a href="http://mountainmath.ca/map/assessment?filter=rs" target="_blank">RS zoned properties</a>.

But all of these maps have a clear bias toward the more expensive homes. Everyone knows by now where the most expensive properties
are. But where are the more affordable ones? 

<!-- more -->
Before continuing and getting disappointed at the end, we want to highlight the huge limitations when looking for affordable homes.
There is no freely available data on the
floor area or number of bedrooms for each dwelling unit. Next to the price and location (which we have), these are the most
important features of a dwelling. And this limits the usefulness of all that follows. 

Why continue? It gives a glimpse of what kind of analysis could be done if the information would be *freely* available.

## The Elephant in the Room
In principle all this information is available, [BCAssessment](https://twitter.com/bcassessment) has all this data. And they even make it
[available on their (quite nice) eValue website](http://evaluebc.bcassessment.ca/Default.aspx#). But their terms of use
prevent us and others from using this information. Most of the information used in the analysis here originates from BCAssessment,
but it comes via the City of Vancouver that has made the data available through their open data portal. 
 
As I understand it, the main reason why this data is not freely available is that BCAssessment is charged to recover their
own operating cost. So they hold onto their data and try to sell it for cost recovery. In the process of which they harm
the ability of municipalities to plan properly and the public to get a clear idea where things are at and have a fact-based
discussion on how to move forward. Which creates large amounts of friction and may lead to social and economic damages far
exceeding any revenue collected by BCAssessment.

Personally, I don't believe that holding back data is a smart way to run a government. You might want to 
<a href="http://twitter.com/home/?status=@bcassessment Please add %23openData license to information you publish on eValue! %23giveUsData" target="_blank">ask BCAssessment to #giveUsData</a>. 


For now, let's put the data that we do have to the best use and see what we can tease out.

## Locating the Most Affordable Dwellings
<a href="https://mountainmath.ca/map/special/101" target="_blank"><img  src="images/strata.png" style="width:50%;float:left;margin-right:10px;"></a> 
Stating the obvious, the most affordable homes aren't "single detached", they are condos. And condos have been largely
absent from the affordability debate, although they make up the majority of owned dwelling units in the City of Vancouver.

Let's start off with a <a href="http://mountainmath.ca/map/special/101" target="_blank">map of the roughly 4,500 stratified residential or mixed use properties in Vancouver housing a total of about 10,2000 strata units</a>.
The exact numbers are hard to pin down. (<a href="http://twitter.com/home/?status=@bcassessment Please add %23openData license to information you publish on eValue! %23giveUsData" target="_blank">#giveUsData</a>)

The distribution of stratified units by the total number of strata units
per building gives an idea of the types of strata units that are out there. 
<div style="margin:10px 50px;padding:5px;border: 1px solid black;border-radius:5px;">
<div id="graph_strata_by_number" style="height:200px;max-width:640px;" data-url="/data/strata_by_number.json"></div>
<div class="legend no-margin">
  <p><i style="background:steelblue"></i> Number of strata units by size of strata <span style="float:right;margin-right:10px;" id="unit_counts_value"></span></p>
</div>
</div>
The horizontal axis is not linear, and the bin sizes are not equal, which makes the graph a little difficult to read. We chose bins at
2, 4, 8, 10, 16, 20 at the low end, then of width 10 up to 200, from there width 20 up to 300 and then 400, 500 and 700 at
the top end. Lazy me apologizes for the poor graphic.

We immediately notice the 4,152 strata units in stratas with exactly 2 units usually referred to as "duplexes". 
There are a lot of strata units are in stratas of size between 20 and 50, but
otherwise the units are fairly well distributed over different building sizes. At the high end there are fewer buildings,
but each with lots of units. So there our graph becomes a little jerky and heavily depends on the cutoffs we choose.

To get a basic idea on how much these units cost we plot the number of strata units by price bracket.
<div style="margin:10px 50px;padding:5px;border: 1px solid black;border-radius:5px;">
<div id="graph_strata_by_price" style="height:200px;max-width:640px;" data-url="/data/strata_by_price.json"></div>
<div class="legend no-margin">
  <p><i style="background:steelblue"></i> Number of strata units by price <span style="float:right;margin-right:10px;" id="affordable_value"></span></p>
</div>
</div>

The distribution looks largely as one would expect. It peaks between the $400k and $500k mark, with a median value of
$482,000. But there are a couple of things that jump out. Firstly, there are suspiciously many condos
below $100k, more than half of which (445 to be precise), are less than $50k. There are places where one could by
a condo for that price, but not in Vancouver. These are stratified parking spaces or other amenity spaces.

Next let's focus in on the 8,313 duplex and multi-plex units with 8 or fewer units (in 2,995 buildings) and plot these
separately.
<div style="margin:10px 50px;padding:5px;border: 1px solid black;border-radius:5px;">
<div id="graph_multiplex_by_price" style="height:200px;max-width:640px;" data-url="/data/multiplex.json"></div>
<div class="legend no-margin">
  <p><i style="background:steelblue"></i> Number of multiplex units by price <span style="float:right;margin-right:10px;" id="multiplex_value"></span></p>
</div>
</div>
We see that the price distribution for multiplex units peaks at a higher price between $800k and $900k, but the overall
numbers are quite small when compared to all strata units.
 

## Affordable Strata Units
Let's try to understand where the "affordable" housing stock is, which we take to be units below $500k, or roughly the
bottom half of the distribution. We would like to map the properties containing housing
units for each of our price brackets, but this gets tricky since the dataset does not hold
information which units are parking spaces and which are commercial. 

We need to take care of the problem with needing to distinguish parking spaces from housing units. And from commercial units.
(<a href="http://twitter.com/home/?status=@bcassessment Please add %23openData license to information you publish on eValue! %23giveUsData" target="_blank">#giveUsData</a>) So we are left with
using proxies, so let's set a cutoff price so that most units below are parking stalls and most units above are (residential)
units.

Generally, parking spaces should not cost more than $50,000 which is roughly the cost to build underground
parking. Around $40,000 if it's only one level, getting up to $75,000 (or sometimes even more) when having to go down deeper or water becomes an issue. The
fact that some spaces go for significantly less is the result of mandatory parking minimums. Some parking spaces may be worth
more than $100,000 as some people are willing to pay a premium for a convenient spot. That may mean a spot next to the
elevator by the exit ramp, or simply a parking spot in a specific building that is under supplied and trekking across the
street to a spot somewhere else is inconvenient enough. Or some stratified parking spaces might consist of several
parking spots.

## Location of Affordable Condos
<a href="https://mountainmath.ca/map/special/110" target="_blank"><img  src="images/strata_500k.png" style="width:50%;float:left;margin-right:10px;"></a> 
Since it is impossible to pick out the housing units out of all the strata units, all we can do is map all strata units, 
understanding that
those below $50k are most likely parking spaces, those between $50k and $100k could be parking or housing (or commercial) and the
majority of units above $100k are housing.

And we can't actually map the individual units, only the buildings that house the units. Here are maps of the buildings
housing the units in each of the lower brackets.

* <a href="https://mountainmath.ca/map/special/41" target="_blank">< $100k (715 units in 17 buildings)</a>
* <a href="https://mountainmath.ca/map/special/42" target="_blank">$100k -- $200k (2,415 units in 198 buildings)</a>
* <a href="https://mountainmath.ca/map/special/43" target="_blank">$200k -- $300k (10,270 units in 644 buildings)</a>
* <a href="https://mountainmath.ca/map/special/44" target="_blank">$300k -- $400k (19,128 units in 1,112 buildings)</a>
* <a href="https://mountainmath.ca/map/special/45" target="_blank">$400k -- $500k (20,538 units in 1,219 buildings)</a>

Clickin into a particular building and hitting the "more" button will pull up (a slightly cleaned) tax roster where you
can get more information on the units. And if you are really interested in finding out more about a particular one
you can always [head on over to BCAssessment's eValue website](http://evaluebc.bcassessment.ca/Default.aspx#) to look up
more of those details that BCAssessment keeps in public view but locked off from systematic public scrutiny.
(<a href="http://twitter.com/home/?status=@bcassessment Please add %23openData license to information you publish on eValue! %23giveUsData" target="_blank">#giveUsData</a>)

## Makeup of Condos
The built quality of strata units is generally higher, dollar for dollar, than that of single family homes. This should not
come as a surprise as land is very expensive and stata units tend to use land more efficiently. There are only 27 residential
or mixed use strata buildings that classify as *teardowns*, 
[with a teardown coefficient below 5%](http://doodles.mountainmath.ca/blog/2016/01/18/redevelopment/).
And 25 of these are duplexes, one is a 3-plex and one an 8-plex. There are no condos buildings with more than 8 units that
fit our most restrictive definition of *teardown*. 

Next we explore what kind of buildings the affordable units are in by graphing the number of units in several price ranges 
per size of the strata building it is in.
<div style="margin:10px 50px;padding:5px;border: 1px solid black;border-radius:5px;">
<div id="graph_affordable_strata_by_number" style="height:200px;max-width:640px;" data-url="/data/affordable_strata_by_number.json"></div>
<div class="legend no-margin">
</div>
</div>

It looks like buildings with 20 to 60 units are quite good at producing affordable units. Partially that's due to their
abundance, but graphing the percentage of units in each price bracket confirms that these buildings tend to produce a
nice mixture of low and high value condos. But larger condo buildings can also achieve this, although the actual performance
of what has been built is mixed. Other factors, for example building age, are likely also at play here.
<div style="margin:10px 50px;padding:5px;border: 1px solid black;border-radius:5px;">
<div id="graph_percentage_affordable_strata_by_number" style="height:200px;max-width:640px;" data-url="/data/percentage_affordable_strata_by_number.json"></div>
<div class="legend no-margin">
</div>
</div>


To see what the distribution of strata units by age is, we use the same price cutoffs and sort the strata units into age
brackets. Since we are mostly interested in the more affordable units we sort out units in buildings with 4 or fewer units.
These units might skew some of the age brackets.
<div style="margin:10px 50px;padding:5px;border: 1px solid black;border-radius:5px;">
<div id="graph_affordable_strata_by_age" style="height:200px;max-width:640px;" data-url="/data/affordable_strata_by_age.json"></div>
<div class="legend no-margin">
</div>
</div>

To highlight the proportional makeup we again and graph this again as percentages.
<div style="margin:10px 50px;padding:5px;border: 1px solid black;border-radius:5px;">
<div id="graph_percentage_affordable_strata_by_age" style="height:200px;max-width:640px;" data-url="/data/percentage_affordable_strata_by_age.json"></div>
<div class="legend no-margin">
</div>
</div>

We see how generally the percentage of more affordable units increases with the age of the building. Newer units generally
tend to be more expensive at first and becoming more affordable over time as the building ages.


## Taking Stock
If you read this far you probably agree that this exercise was mostly a waste of time. Hopefully you are earger to see this
analysis split up by number of bedrooms and floor area. If you are all riled up at the lack of data and BCAssessment
not giving out this information with a clean open data license you might want to drop them a line and 
<a href="http://twitter.com/home/?status=@bcassessment Please add %23openData license to information you publish on eValue! %23giveUsData" target="_blank">ask for at least the information they publish eValue to be made available with an #openData license!</a> 

I am still hopeful that this might happen some day, and we can get a much better picture of the buildings in BC cities
and check how they perform in fulfilling the needs of the community. And learn from that to make informed policy choices.

<script src="//d3js.org/d3.v3.min.js" charset="utf-8"></script>
<script src="/lib/jquery.min.js" charset="utf-8"></script>
<script>

function stacked_bar_graph(div,shiftAxis,domainFormatter,rangeFormatter,domainLabelFormatter){
    if (!domainFormatter) domainFormatter=d3.format("d")
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
var legend=d3.select(div.node().parentNode).select('.legend');


d3.json(data_url, function(error, json) {
  if (error) throw error;
  var graphData=json[0];
  var data=graphData.data;
  var color = d3.scale.ordinal().domain(graphData.colors.map(function(d,i){return i}))
  .range(graphData.colors);
  var domain=data.map(function(d){return d.date;});
  x.domain(domain);

  function graphValueId(i){
      return graphData.class + '_' + i + '_value'
  }

  graphData.labels.forEach(function(text,i){
    var color=graphData.colors[i];
    var html='<i style="background:' + color + '"></i> ' + text + ' <span style="float:right;margin-right:10px;" id="' + graphValueId(i) + '"></span>'
    legend.append('p').html(html);
  });
  
  data.forEach(function(d) {
      var y0 = 0;
      d.values = color.domain().map(function(i) { return {date: d.date, y0: y0, y1: y0 += +d.count[i]}; });
      d.total = d.values[d.values.length - 1].y1;
  });
  y.domain([0, d3.max(data, function(d) { return d.total; })]);

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

    function updateTooltip(d,i){
       color.domain().forEach(function(j){
             var value=d && i==j ? (domainLabelFormatter(d.date) + ': ' +rangeFormatter(d.y1-d.y0)) : '';
             d3.select('#'+graphValueId(j)).text( value);
       });
    }

  var year=svg.selectAll(".year")
    .data(data)
        .enter().append("g")
          .attr("class", "g");
  year.selectAll(".color-bar")
      .data(function(d) { return d.values; })
    .enter().append("rect")
      .attr("class", graphData.class + " color-bar")
      .attr("fill", graphData.color)
      .attr("x", function(d) { return x(d.date); })
      .attr("width", x.rangeBand())
      .attr("y", function(d) { return y(d.y1); })
      .attr("height", function(d) { return Math.max(0, y(d.y0) - y(d.y1)); })
      .attr("fill",function(d,i) {return color(i);})
      .on('mouseover',updateTooltip)
      .on('click',updateTooltip)
      .on('touch',updateTooltip) 
      .on('mouseout',function(){updateTooltip(null,i)});

      
});

}

function bar_graph(div,shiftAxis,domainFormatter,rangeFormatter,domainLabelFormatter){
    if (!domainFormatter) domainFormatter=d3.format("d")
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
      .attr("fill", graphData.color)
      .attr("x", function(d) { return x(d.date); })
      .attr("width", x.rangeBand())
      .attr("y", function(d) { return y(d.count); })
      .attr("height", function(d) { return height - y(d.count); })
      .on('mouseover',function(d){
         d3.select('#'+this.classList[0]+'_value').text(domainLabelFormatter(d.date) + ': ' + rangeFormatter(d.count)) 
      }).on('click',function(d){
       d3.select('#'+this.classList[0]+'_value').text(domainLabelFormatter(d.date) + ': ' + rangeFormatter(d.count)) 
      }).on('touch',function(d){
         d3.select('#'+this.classList[0]+'_value').text(domainLabelFormatter(d.date) + ': ' + rangeFormatter(d.count)) 
      }).on('mouseout',function(){d3.select('#'+this.classList[0]+'_value').text('')});

      
});

}


var priceFormatter2=d3.format("$,");
    var priceFormatter = function (y) {
        return y>=1000000 ? (priceFormatter2(y/1000000) + 'm') : (priceFormatter2(y/1000) + 'k');
    };
    var brackets=[100000,200000,300000,400000,500000,600000,700000,800000,900000,1000000,2000000,10000000,20000000,40000000]

var binFormatter=function(top){
    var bottom=0;
    if (top<=1000000) bottom=top-100000;
    else if (top==2000000) bottom= 1000000;
    else if (top==10000000) bottom= 2000000;
    else if (top=20000000) bottom= 10000000;
    else bottom=20000000;
    return priceFormatter(bottom) + ' - ' + priceFormatter(top);
}
var percentageFormatter=d3.format("%");
var numberFormatter=d3.format(",");
var numberBinFormatter=function(top){
    var     bins=[0,1,2,4,8,10,16,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,300,400,500,700];
    var index=0;
    while (bins[index]<top && index<bins.length) index ++;
    bottom=bins[index-1]+1;
    return (bottom == top) ? numberFormatter(bottom) : numberFormatter(bottom) + ' - ' + numberFormatter(top);
}
var yearFormatter=d3.format();//function(d){return d;};
var yearBinFormatter=function(top){
     var     bins=[1970,1980,1990,1995,2000,2005,2010,2020];
     var index=0;
     while (bins[index]<top && index<bins.length) index ++;
     bottom=bins[index-1]+1;
     return (bottom == top) ? yearFormatter(bottom) : yearFormatter(bottom) + ' - ' + yearFormatter(top);
}
bar_graph(d3.select("#graph_strata_by_price"),true,priceFormatter,null,binFormatter);
bar_graph(d3.select("#graph_multiplex_by_price"),true,priceFormatter,null,binFormatter);
bar_graph(d3.select("#graph_strata_by_number"),true,numberFormatter,null,numberBinFormatter);
stacked_bar_graph(d3.select("#graph_affordable_strata_by_number"),true,numberFormatter,null,numberBinFormatter);
stacked_bar_graph(d3.select("#graph_percentage_affordable_strata_by_number"),true,numberFormatter,percentageFormatter,numberBinFormatter);
stacked_bar_graph(d3.select("#graph_affordable_strata_by_age"),true,yearFormatter,null,yearBinFormatter);
stacked_bar_graph(d3.select("#graph_percentage_affordable_strata_by_age"),true,yearFormatter,percentageFormatter,yearBinFormatter);
</script>
