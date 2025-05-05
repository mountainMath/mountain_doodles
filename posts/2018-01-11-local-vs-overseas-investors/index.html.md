---
title: Local vs Overseas Investors
author: Jens von Bergmann
date: '2018-01-11'
slug: local-vs-overseas-investors
categories:
  - Vancouver
  - cancensus
tags: []
description: 'Creative uses of non-resident owner data.'
image: "index_files/figure-html/local_overseas-1.png"
featured: 'local_overseas-1.png'
featuredalt: ""
featuredpath: "/posts/2018-01-11-local-vs-overseas-investors_files/figure-html"
linktitle: ''
type: "post"
aliases:
  - /blog/2018/01/11/local-vs-overseas-investors/
---





Nathan Lauster just opened up an interesting way to look at CHSP data -- by folding in the SFS. I have [played with SFS data in the past](https://twitter.com/vb_jens/status/846951173177442308) but it clearly is time to revisit this and reproduce Lauster's numbers. Let's also fold in census estimates for that to see how these numbers match up. I have nothing to add to the excellent commentary from Lauster's original post, so please [head over there for good context of these estimates](https://homefreesociology.wordpress.com/2018/01/11/if-the-problem-is-speculation-then-why-focus-on-foreigners/).






# Estimates based on total property value
<img src="index_files/figure-html/unnamed-chunk-3-1.png" width="672" />

For Metro Vancouver the data matches up quite nicely, giving us some confidence that these estimates could be useful to combine with the CHSP data. For Toronto the census estimates are outside of the 95% confidence interval of the SFS data. This will be hard to resolve without taking a deep dive into the SFS methods.

Now to the exciting part: comparing this to CHSP estimates.

<img src="index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

For Vancouver, where we have a little more confidence in the data with the SFS and Census tracking closely, we can combine the estimates to arrive at Lauster's graph. To even out some of the intercomparability issues we average the census and SFS data, hopefully increasing the usefulness of the Toronto estimate. People with more insight into the discrepancy are welcome to [grab the code](https://github.com/mountainMath/doodles/blob/master/content/posts/2018-01-11-local-vs-overseas-investors.Rmarkdown) and make the appropriate adjustments.

<img src="index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

Again, we have higher confidence in the Vancouver estimates compared to the Toronto estimates.

# Estimates based on number of properties
We can repeat the same analysis based on number of properties instead of total value, this may remove some of the problems we have been facing.

<img src="index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

We see that the numbers match better, with both estimates within the SFS confidence interval. So let's add in the CHSP data.

<img src="index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

Armed with that we again average over SFS and census numbers and use that to separate out properties that are not owner-occupied but with owners living in Canada, aka "local speculators".

<img src="index_files/figure-html/local_overseas-1.png" width="672" />

Again, these estimates probably aren't terribly precise, but probably good enough to push out some graphs. Matching different data sources is hard and I should probably put in a little more work to cross-check things.

As always the R notbook that built this post [lives on GitHub](https://github.com/mountainMath/doodles/blob/master/content/posts/2018-01-11-local-vs-overseas-investors.Rmarkdown), feel free to download and adapt it for your purposes.
