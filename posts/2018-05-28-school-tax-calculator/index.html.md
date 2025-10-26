---
title: Extra School Tax Calculator
author: Jens von Bergmann
date: '2018-05-28'
slug: school-tax-calculator
categories:
  - Assessment Data
  - Vancouver
tags: []
description: 'Apparently 8th grade math ability is inversly proportional to property values, here is a simple calculator for those still suffering from consequences of sub-par math education.'
image: "index_files/figure-html/school_tax_map-1.png"
featured: ''
featuredalt: ""
featuredpath: ""
linktitle: ''
type: "post"
aliases:
  - /blog/2018/05/28/school-tax-calculator/
---

One thing the extra school tax debate has [brought to light](https://twitter.com/elam101/status/1001114791975337984) is how challenging 8th grade math is for people, especially those living in homes valued over $3M. Journalists also seem to have trouble putting outrageously wrong statements into perspective, so here is a simple calculator on how much extra school tax people will owe depending on their home value, years until home is sold, expected changes in the real estate market, interest rates on deferred (through government program or second mortgage) taxes.

8th grade math should really be something all of us are comfortable with. (And it really should be 6th grade math.) Another good reason to invest more into our K-12 education system. Incidentally only 35% of which is currently funded by school taxes, although that's [not the main reason why increasing school taxes, which is just the name of the provincial portion of the property taxes, is a good idea](https://docs.google.com/document/d/1KnxJlJXkp853PAiHJNqNVL3xkhphYxsFrjg4o9zZBAk/edit).

We have talked about the school taxes before, once just [running the numbers for the City of Vancouver to see who is affected](https://doodles.mountainmath.ca/blog/2018/02/28/extra-school-tax/) and once [relating the school tax to land value against](https://doodles.mountainmath.ca/blog/2018/05/01/a-modest-school-tax-proposal/). It seems we need yet another post to simplify the math down to moving a couple of sliders.

### Assumptions
<table style="border-spacing:1em 0;border-collapse:separate;margin-bottom:20px;">
<tr><td>Home Value</td><td><input type="range" id="home"></td><td id="homeValue"></td></tr>
<tr><td>Annual change in property values</td><td> <input type="range" id="change"></td><td id="changeValue"></td></tr>
<tr><td>Deferral simple interest rate</td><td> <input type="range" id="interest"></td><td id="interestValue"></td></tr>
<tr><td>Years of deferral</td><td> <input type="range" id="years"></td><td id="yearsValue"></td></tr>
</table>

### Results
<table style="border-spacing:1em 0;border-collapse:separate;margin-bottom:20px;">
<tr><td>Annual extra school tax now</td><td id="extra1"></td></tr>
<tr><td>Total Home Value in <span class="years2">10</span> years</td><td id="home_value"></td></tr>
<tr><td>Annual extra school tax <span class="years2">10</span> years</td><td id="extra2"></td></tr>
<tr><td>Total Deferred Extra School Taxes in <span class="years2">10</span> years</td><td id="deferred_taxes"></td></tr>
<tr><td>Home Value Minus Deferred Taxes in <span class="years2">10</span> years</td><td id="remainder"></td></tr>
</table>

# An example
An upfront warning, I have a really hard time to dial down the snark level when talking about prominent examples from the news. You best not read on if that's potentially offensive to you.

As an illustrative example consider the [plight of David Tha](http://www.vancourier.com/real-estate/house-rich-tax-poor-how-a-point-grey-couple-is-struggling-to-pay-school-surtax-1.23290979). He bought his home in 1987 for $370,000 and it's assessed at $6.75M now. That's an 18.2 fold increase over 31 years, or an annual appreciation of 9.8%. He says he has done nothing to earn this increase in value, other than applying "a few coats of paint" over the years. 

Tha does not want to defer his taxes as that would be "simply deferring the injustice", and he notes that the interest rate for the deferral program is below prevailing interest rates so "that puts the burden on all B.C. residents". Which is "something he’s against in principle". Tha is quite the altruist when it comes to not taking advantage of government subsidies, not so much when it comes to his housing wealth that he says he did essentially nothing to earn.

So how does Tha plan to deal with the tax? Tha's response bears quoting in full.

>    They’ve disconnected their landline and signed up for an internet-based phone service. Tha, who is 72, is trying to figure out a way to attach an antenna to their house so they can cut cable and yet still watch television. They’re eating less meat to cut down on their grocery bills and foregoing travel plans, all to try to save the money that will go towards their higher tax bill.

We have to conclude that he is very principled, but I am still not convinced that the single mom trying to make ends meet renting an East Van basement suite will be terribly sympathetic to Tha's self-imposed budget constraints.

But Tha's plight does not end here. 

>    They also don’t want to put the burden on their daughter, a nurse to whom they plan to bequeath the house. Not only will she have to pay the deferred taxes but if the house continues to increase five per cent a year in value, and the surtax remains in place, Tha calculates the taxes will be $67,000 a year in 15 years, well beyond her ability to pay.
    
The math is of course ambitious. First off, if his house had really "only" appreciated by 5% annually since he bought it it would now be worth $1.68M and he would not have to pay the extra school tax. Next, employing some math debugging skill, he seems to believe that it's ok not to compound the appreciation and is in the same camp as many homeowners in not understanding how regular property tax mill rates are calculated by assuming the mill rate is fixed as overall property values go up. And computed the total rate based on these assumptions, not just the extra school tax. Glad to know he is retired.

So let's fix this and set out calculator to $6.75M home value, 5% real estate appreciating rate, 1.2% simple interest and 15 years. We see that in 15 years Tha's daughter will be in the very uncomfortable position of inheriting a $14M home owing $390k in deferred extra school tax, a net value of over $13.5M. At that point in time the annual extra school tax amounts to $42k and she will have to make up her mind if she wants to keep deferring taxes, which she may have to finace through a second mortgage at higher interest rates (although still lower than Tha's projected rate of real estate appreciation), or sell her home for around $14M.

Of course Tha could sell right now and downsize, this way he could avoid having to skimp on meals and cut his cable TV. But...

>    And where would they move? Prices are expensive everywhere in the city, he says. Their daughter lives with them so she’d have to find a place to live, too.

Vancouver is indeed expensive, but with $6.7M in equity, almost all of which is untaxed capital gains, it is very hard to see how they could find a reasonable place to live to fit their middle class lifestyle.

# Bottom line
There probably are some cases of people hit by the extra school tax that would illicit my sympathy. Sadly I have not seen these pop up in the local media, the most vocal voices seem to mimic Tha's line of reasoning. And to be fair, Tha's math, while wrong, is quite a bit better than [what we have seen from others](http://www.news1130.com/2018/05/27/school-tax-town-hall-eby-hundreds/).

>    “He told Frank, who’s never deferred his taxes and is living his retirement very humbly, that he should defer his taxes and live comfortably so his kids can have that $7 million home,” says Yvonne Williams, who left the meeting after Eby argued homeowners can defer the tax “Well if Frank lives to 95, there’s gonna be no money left in that house and the he’ll have to surrender it to the city.”

Since Frank is retired let's assume he is 65 years old and set the calculator to $7M, no further property appreciation, a 1.2% deferral rate and 30 years. We see that he will owe $490,000 in deferred extra school tax by the time he turns 95, leaving him with a tidy $6,510,000 in net assets. We are guessing that Frank has lived in this home for a while, the accummulated deferred extra school tax will be a drop in the bucket compared to the value gains his home as seen over the past years. I find it hard to relate to how this could constitute any form of cruel hardship. And I am comfortably housed, I don't want to imagine how someone struggling to pay rent in this city is supposed to by sympathetic to this.



<script src="//d3js.org/d3.v3.min.js" charset="utf-8"></script>
<script src="/lib/jquery.min.js" charset="utf-8"></script>
<script>
var percentageFormatter=d3.format(".2%");
var currencyFormatter=d3.format("$,.3r");
var currencyFormatterFull=d3.format("$,l");
var numberFormatter=d3.format(",.0f");

var homeValue=7000000,
    changeValue=0,
    interestValue=0.012,
    yearsValue=30;
document.getElementById('home').value=(homeValue-1000000.0)/300000.0;
document.getElementById('change').value=(changeValue+0.1)*500.0;
document.getElementById('interest').value=interestValue*2000;
document.getElementById('years').value=yearsValue;

function extraTax(h){
  return(Math.max(h-3000000,0)*0.002 + Math.max(h-4000000,0)*0.002)
}

function roundSignificant(x,d){
  return(parseFloat(d3.format("."+d+"r")(x)));
}

function updateResults(){
   $('#homeValue').html(currencyFormatter(homeValue));
   $('#changeValue').html(percentageFormatter(changeValue));
   $('#interestValue').html(percentageFormatter(interestValue));
   $('#yearsValue').html(numberFormatter(yearsValue));
   $('.years2').html(numberFormatter(yearsValue));

   var value1=roundSignificant(homeValue*Math.pow(1+changeValue,yearsValue),3);
   $('#extra1').html(currencyFormatterFull(extraTax(homeValue)));
   $('#extra2').html(currencyFormatterFull(extraTax(value1)));
   $('#home_value').html(currencyFormatterFull(value1));
   
   var value2=0;
   for (i=1;i<=yearsValue;i++){
    value2+=extraTax(homeValue*Math.pow(1+changeValue,i-1))*(1+interestValue * (yearsValue-i));
   }
   value2=roundSignificant(value2,2);
   $('#deferred_taxes').html(currencyFormatterFull(value2));
   var remainder=value1-value2;
   $('#remainder').html(currencyFormatterFull(remainder));
}

$('#home').on('change input',function(){
    homeValue=d3.round((parseFloat(this.value)*300000.0+1000000.0)/100000,0)*100000;
    updateResults();
});
$('#change').on('change input',function(){
    changeValue=d3.round(parseFloat(this.value)/500.0-0.1,4);
    updateResults();
});
$('#interest').on('change input',function(){
    interestValue=d3.round(parseFloat(this.value)/2000,4);
    updateResults();
});
$('#years').on('change input',function(){
    yearsValue=d3.round(parseFloat(this.value),0);
    updateResults();
});

updateResults();
</script>
