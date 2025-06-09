---
title: Migrating to quarto
authors: 
  - name: Jens von Bergmann
    affiliation: MountainMath
date: '2024-04-11'
slug: migrating-to-quarto
categories:
  - geeky
description: 'Some overdue housekeeping and a new look for the blog.'
image: "images/blog_preview.png"
code-tools:
  toggle: true
---




I started this blog 10 years ago, first using a jekyll setup. In 2017 I switched to [blogdown](https://bookdown.org/yihui/blogdown/), which linked the posts with the Rmarkdown notebook that generated the content. The setup aimed at maximum transparency, with the source living in a [public Github repository](https://github.com/mountainMath/mountaindoodles) that keeps all changes transparent, and deploys to the website via [Netlify](https://www.netlify.com) directly triggered from the GitHub repo when it changed. That means the website of the blog was always in sync with the repository, and the repository contained the entire history of the blog.

# Why switch?

This all sounds great, so why change? Pretty much for the [same reasons that other have taken this step](https://www.incidentalfindings.org/posts/2022-08-30_from-blogdown-to-quarto/). But also because [quarto](https://quarto.org) as quickly established itself as a superior notebook platform than Rmarkdown. It is designed to be language agnostic, with good support for [R](https://quarto.org/docs/computations/r.html), [Python](https://quarto.org/docs/computations/python.html), [Julia](https://quarto.org/docs/computations/julia.html), and [Observable](https://quarto.org/docs/computations/ojs.html) right out of the box. That's basically my entire toolbox these days. And in all my actual work I have already switched over to using quarto.

At the same time this gives me an opportunity to give the blog a face lift and add some features. It now has the option to switch to dark mode, some better layout and design elements, it automatically generates suggested license information and citation suggestions for each posts (which hopefully cuts down on my emails). It now also automatically generates links to the source code, which I did manually before (and sometimes got the link wrong).

# How to switch?

The process was relatively straight forward thanks to posts by others who have done this before me. I decided that I won't recreate the original link structure but instead rely on redirects to make sure links to the old blog posts still work. This will however break links to images or other elements of the blog post.

To convert the blog posts I relied on the markdown versions produced by blogdown, that then were fed to the Hugo blog engine. Quarto can also render these markdown documents to html, but they did require some editing to make sure all the links work. I [wrote a script to handle the translation](https://github.com/mountainMath/mountain_doodles/blob/main/R/migration.R), which might be useful to others who are going down the same route even if it caters to some of the idiosyncrasies of my setup.

At the same time I decided to keep the Rmarkdown files for reference, although at this point I set this up as an entirely new repo\](https://github.com/mountainMath/mountain_doodles) with all the old code and blog setup in the original [mountaindoodles](https://github.com/mountainMath/mountaindoodles) repo. The manual links to the source code still point to the old repo.

# What's next?

Other than the look and feel, quarto has the ability to much more easily add interactives by leveraging [Observable](https://observablehq.com). Quarto also has great support for code folding, for this blog we hide code by default but now show a marker that allows it expand the code if desired. This makes it easier to check in on the details of how things are done without having to navigate to the underlying notebook on GitHub. It makes it much easier to quickly grab a code chunk and modify it.

Here is a simple example that pulls in all item and rent CPI time series directly from the StatCan NDM API into the browser and plots them interactively, taken from [our interactive ObservableHQ notebook](https://observablehq.com/@mountainmath/year-over-year-change-in-rent-cpi). Live and auto-updating data is great, but this of course also means that the code will fail when StatCan servers are down.[^1]

[^1]: Of course the StatCan servers for the data used here went down on the evening this post was written ahead of the next day's release of the March CPI data...





:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="40" source-offset="0"}
vegalite({
  data: {values: transformedData},
  vconcat:[{
    width: 600,
    height: 200,
    title: "Year over year change in CPI",
    config: {
      axis: {shortTimeLabels: true}
    },
    layer: [{
    mark: {type: 'line'},
    encoding: {
      x: {field: 'refPer', type: 'temporal', timeUnit: 'yearmonthdate',title:"",
         scale: {domain: {selection: "brush"}}},
      dx: {  
        field: "Series", "type": "nominal",
        scale: {"rangeStep": 6} 
      },
      color: {field: "Series", type: 'nominal',"scale": {"scheme": "Dark2"}}, 
      y: {type: 'quantitative', field: 'pct_change',axis: {format: ".1%"},title:"", "grid": false}
    }
  },
    {
      "data": {"values": [{"guide": 0}]},
      "mark": "rule",
      "encoding": {
        "y": {"field": "guide","type": "quantitative"},
        "color": {"value": "black"}
      }
    }
  ]}, {
    width: 600,
    mark: {type: 'line'},
    title: "",
    height: 60,
    encoding: {
      x: {field: 'refPer', type: 'temporal', timeUnit: 'yearmonthdate',title:""},
      color: {field: "Series", type: 'nominal',"scale": {"scheme": "Dark2"}}, 
      y: {type: 'quantitative', field: 'pct_change',axis: {format: ".1%"},title:""}
    },
    config: {
      background: "#808080",
      axis: {shortTimeLabels: true}
    },
    selection: {
      brush: {type: "interval", encodings: ["x"]}
    }
  }]})
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-1 nodetype="expression"}
:::
::::
:::::




The lower chart is a brush to allow to zoom into time periods of interest in the main chart. The rent CPI method was substantially updated starting in January 2019, the effect of which is quite visible in the graph when zooming in, and the year over year change is fully based on the new method starting January 2020.

One fun part about observable is that the order of the code chunks does not matter. The above code junk produced the chart, but the code to pull in the data from StatCan, and the code to load required libraries is below. This flexibility makes it easier to rearrange a blog post where one does not have to worry about breaking dependencies when moving around the code.





::::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="96" source-offset="-0"}
vectors = {return {
  41690973: "All items",
  41691052: "Rent",
}}

data = d3.json("https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods",{
        method:"POST",
      body: JSON.stringify(Object.keys(vectors).map(function(v){return {"vectorId":v, "latestN":2000}})),
  headers: {"Content-type": "application/json; charset=UTF-8"}
})
```

:::::{.cell-output .cell-output-display}

::::{}

:::{#ojs-cell-2-1 nodetype="declaration"}
:::
::::
:::::

:::::{.cell-output .cell-output-display}

::::{}

:::{#ojs-cell-2-2 nodetype="declaration"}
:::
::::
:::::
::::::

::::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="109" source-offset="-0"}
vegalite = require("@observablehq/vega-lite@0.1")
d3 = require("https://d3js.org/d3.v5.min.js")

transformedData = [].concat.apply([], data.map(function(d){
  var rows = d.object.vectorDataPoint.map(function(row){
    row.Series=vectors[d.object.vectorId];

    var year=parseInt(row.refPer.slice(0,4));
    var month=parseInt(row.refPer.slice(5,7));
    //if (month==1) {
    //  year-=1;
    //  month=12;
    //} else month-=1;
    year-=1;
    row.oldPer=''+year+'-'+(month+'').padStart(2, '0')+row.refPer.slice(7,10);
    //debugger
    row.Value=row.value*Math.pow(10,row.scalarFactorCode);
    var result = d.object.vectorDataPoint.filter(function(r) {
      return r.refPer === row.oldPer;
    });
    row.oldVal = (result[0] !== undefined) ? result[0].Value : null;
    row.change=row.Value-row.oldVal;
    row.pct_change=row.change/row.oldVal;
    row.year=year;
    return row;
  });
  return rows;
})).filter(function(row){
  return(row.oldVal!=null & row.year>=1989)
})
```

:::::{.cell-output .cell-output-display}

::::{}

:::{#ojs-cell-3-1 nodetype="declaration"}
:::
::::
:::::

:::::{.cell-output .cell-output-display}

::::{}

:::{#ojs-cell-3-2 nodetype="declaration"}
:::
::::
:::::

:::::{.cell-output .cell-output-display}

::::{}

:::{#ojs-cell-3-3 nodetype="declaration"}
:::
::::
:::::
::::::




# Downsides

There are a couple of casualties from the switch. One is the comment system. I could have kept the old system, but Disqus has problems. That means that old comments are lost. (There are ways to recover disqus comments, but it means copying and re-posting them under a different account, which is less than ideal.) I decided to switch to [giscus](https://giscus.app) as a new commenting system, it integrates nicely with the GitHub repo by using the discussion feature to store comments. No data goes off to other servers. However, this means that a GitHub account is required in order to comment, which will be a barrier to many readers. On the other hand, there really weren't all that many comments on the old system, so it might not be a big loss.

As mentioned above, the links to the posts have changed, but the redirects should (hopefully) solve this.

The RSS feed link might have changed too, not sure, so RSS people might have to subscribe again.

# Caveats

There are probably some broken links and other small issues left over to mop up, if you find any, please let me know and I will fix them as I find time.

# Conclusion

Most of these changes are purely cosmetic, but it gives more flexibility to the authors while automating some tasks. And it makes it easier for readers check into the details of the analysis. We will see how well it holds up. As a safety we are keeping intermediate markdown outputs, which makes it easier to switch back to the old system or to an entirely new system 10 years down the road.

For now we are excited about the new look and feel and the new features and additional flexibility of quarto.
