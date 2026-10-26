---
title: "BC Elections 2026"
author:
  - name: Jens von Bergmann
    affiliation: MountainMath
date: '2026-10-24'
slug: bc-elections-2026
description: 'Live updates of the 2026 BC election results as the data comes in'
image: 'images/bc_elections_mid_map.png'
category: 'geeky'
bibliography: ../../common_literature.bib 
code-tools:
  toggle: true
fig-width: 8
execute:
  cache: false
  message: false
  warning: false
---



::: {.cell}

```{.r .cell-code}
library(tidyverse)
library(mountainmathHelpers)
library(sf)
```
:::


<!--
R prepares the static inputs, the district boundaries and the 2024 results. All graphs are rendered in the
browser and re-fetch their data every `refresh_seconds`: the live CBC feed (it sends CORS headers), plus the
Elections BC results that `bc_elections_2026_pull.R` uploads to S3. Ridings use the
Elections BC vote counts when they are at least as far along as CBC's, the CBC feed for everything else.
-->

This is a simple post that tracks live election results with a couple of graphs and minimal commentary, it leans on [our previous post on the 2024 BC provincial election](https://doodles.mountainmath.ca/posts/2024-10-20-bc-election-2024/).



::: {.cell}

```{.r .cell-code}
# match Elections BC affiliation names to CBC party codes
recode_affiliation <- function(affiliation) {
  case_when(affiliation %in% c("","Independent","Unaffiliated") ~ "IND",
            str_detect(affiliation,"NDP") ~ "NDP",
            str_detect(affiliation,"Conservative") ~ "CON",
            str_detect(affiliation,"Green") ~ "GRN",
            str_detect(affiliation,"One BC") ~ "ONE",
            str_detect(affiliation,"Centre") ~ "CTR",
            str_detect(affiliation,"Libertarian") ~ "LTN",
            str_detect(affiliation,"Christian Heritage") ~ "CHP",
            str_detect(affiliation,"Communist") ~ "COM",
            str_detect(affiliation,"Freedom") ~ "FP",
            str_detect(affiliation,"Common") ~ "CWP",
            TRUE ~ affiliation)
}

# 93 ridings, same 2023 boundaries as in 2024
get_district_boundaries <- function(refresh=FALSE) {
  simpleCache({
    bc_geo <- cancensus::get_statcan_geographies("2021","PR") |>
      filter(PRUID=="59") |>
      rmapshaper::ms_simplify() |>
      st_make_valid()

    read_sf("https://openmaps.gov.bc.ca/geo/pub/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=WHSE_ADMIN_BOUNDARIES.EBC_ELECTORAL_DISTS_BS11_SVW&outputFormat=json&srsName=EPSG:3005") |>
      select(ED_ID=ELECTORAL_DISTRICT_ID,ED_ABBREV=ED_ABBREVIATION,ED_NAME) %>%
      rmapshaper::ms_simplify() %>%
      st_intersection(st_transform(bc_geo,st_crs(.)) |> select()) %>%
      rmapshaper::ms_simplify() %>%
      st_make_valid() %>%
      st_cast("MULTIPOLYGON") |>
      mutate(x=st_coordinates(st_centroid(geometry))[,1],y=st_coordinates(st_centroid(geometry))[,2]) %>%
      arrange(-y) |>
      mutate(rank = row_number())
  },"bc_2026_election_polygons.rds",refresh=refresh)
}

# Metro Vancouver at higher resolution for the zoomed-in map, simplified once from the full resolution boundaries
metro_bbox <- c(xmin=1185000,ymin=440000,xmax=1295000,ymax=500000)

get_metro_boundaries <- function(refresh=FALSE) {
  simpleCache({
    eds <- read_sf("https://openmaps.gov.bc.ca/geo/pub/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=WHSE_ADMIN_BOUNDARIES.EBC_ELECTORAL_DISTS_BS11_SVW&outputFormat=json&srsName=EPSG:3005") |>
      select(ED_NAME) |>
      st_make_valid()
    crop_area <- st_bbox(metro_bbox,crs=st_crs(eds)) |> st_as_sfc() |> st_buffer(2000)
    bc_geo <- cancensus::get_statcan_geographies("2021","PR") |>
      filter(PRUID=="59") |>
      st_transform(st_crs(eds)) |>
      st_make_valid() |>
      st_crop(crop_area) |>
      select()

    eds |>
      st_crop(crop_area) |>
      st_intersection(bc_geo) |>
      st_collection_extract("POLYGON") |>
      group_by(ED_NAME) |>
      summarise(.groups="drop") |>
      rmapshaper::ms_simplify(keep=0.05, keep_shapes=TRUE) |>
      st_make_valid() |>
      st_cast("MULTIPOLYGON")
  },"bc_2026_election_polygons_metro.rds",refresh=refresh)
}

district_polygons <- get_district_boundaries() |>
  st_cast("POLYGON") |>
  mutate(area=st_area(geometry) |> as.numeric()) |>
  mutate(mainShape=area==max(area),.by=ED_ID) |>
  select(ED_NAME,x,y,rank,mainShape,geometry)

previous_results <- simpleCache({},"bc_elections_2024_results_2024-11-08.rds") |>
  filter(Party!="Total") |>
  mutate(Party=recode_affiliation(Party)) |>
  summarise(Votes=sum(Votes),.by=c(ED_NAME,Party))

# BC Albers coordinates, rounded to the metre
ojs_define(boundaries_json=geojsonsf::sf_geojson(district_polygons,digits=0),
           metro_json=geojsonsf::sf_geojson(get_metro_boundaries(),digits=0),
           metro_bbox=as.list(metro_bbox),
           previous_results=previous_results)
```
:::


<strike>The graphs are self-updating as the data comes in, the page does not need refreshing.</strike> Starting Monday Oct 26 the data only gets updated occasionally.



:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="125" source-offset="0"}
status = {
  const polls = d3.sum(ridings, d => d.pollsReported);
  const total_polls = d3.sum(ridings, d => d.totalPolls);
  const called = ridings.filter(d => d.called).length;
  const votes = d3.sum(ridings, d => d.Total);
  const enr_ridings = ridings.filter(d => d.source === "Elections BC").length;
  // show times in BC time whatever the reader's timezone
  const pacific = date => date.toLocaleString("en-CA", {timeZone: "America/Vancouver", month: "short", day: "numeric",
                                                        hour: "numeric", minute: "2-digit", timeZoneName: "short"});
  // CBC sends UTC without a timezone, like "2026-10-25 02:58:56"
  const cbc_time = pacific(new Date(live.json.data.config.electionLastUpdated.replace(" ", "T") + "Z"));
  const enr_updated = enr.length ? ` Elections BC results as of ${pacific(new Date(enr[0].updated))}, used for ${enr_ridings} ridings.` : "";
  return md`**${fmt.comma(votes)} votes counted, ${fmt.comma(polls)} of ${fmt.comma(total_polls)} polls reported, ${called} of ${ridings.length} ridings called.**
CBC data last updated ${cbc_time}${live.archived ? " (final archived copy)" : ""}, fetched at ${live.fetched.toLocaleTimeString()}, refreshing every ${refresh_seconds/60} minutes.${enr_updated}`
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-1 nodetype="declaration"}
:::
::::
:::::

## Seat count


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="145" source-offset="0"}
seat_chart = {
  const seats = d3.rollups(ridings.filter(d => d.Total > 0), v => v.length, d => d.Party, d => d.called)
    .flatMap(([Party, s]) => s.map(([called, n]) => ({Party, called, n})))
    .sort((a, b) => b.called - a.called);
  const party_order = d3.groupSort(seats, v => -d3.sum(v, d => d.n), d => d.Party);
  if (!seats.length) return md`*Waiting for results*`;
  return Plot.plot({
    title: "Seat count",
    subtitle: "Lighter colour: leading, not called yet. Dashed line: 47 seats for a majority",
    width,
    height: 60 + 40 * party_order.length,
    marginLeft: 50,
    x: {label: "Seats"},
    y: {label: null, domain: party_order},
    color: colour_scale,
    marks: [
      Plot.barX(seats, {x: "n", y: "Party", fill: "Party", fillOpacity: d => d.called ? 1 : leading_opacity, tip: true,
                        title: d => `${d.Party} ${d.called ? "elected" : "leading"}: ${d.n}`}),
      Plot.ruleX([47], {strokeDasharray: "4,4"}),
      Plot.ruleX([0])
    ]
  })
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-2 nodetype="declaration"}
:::
::::
:::::

## Static map


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="173" source-offset="0"}
bc_map = Plot.plot({
  title: "BC provincial election 2026",
  width,
  height: width * 0.8,
  projection: {type: "reflect-y", domain: boundaries},
  color: colour_scale,
  marks: [
    Plot.geo(districts, {fill: d => riding_lookup.get(d.properties.ED_NAME).Party,
                         fillOpacity: d => riding_opacity(riding_lookup.get(d.properties.ED_NAME)),
                         stroke: "white", strokeWidth: 0.5,
                         render: map_hover(districts)})
  ]
})
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-3 nodetype="declaration"}
:::
::::
:::::

## Metro Vancouver detail map


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="191" source-offset="0"}
metro_map = {
  // Metro Vancouver zoom-in where the map view is unreadable
  const {xmin, ymin, xmax, ymax} = metro_bbox;
  return Plot.plot({
    title: "Metro Vancouver",
    width,
    height: width * (ymax - ymin) / (xmax - xmin),
    projection: {type: "reflect-y",
                 domain: {type: "Polygon", coordinates: [[[xmin, ymin], [xmax, ymin], [xmax, ymax], [xmin, ymax], [xmin, ymin]]]}},
    color: colour_scale,
    marks: [
      Plot.geo(metro_districts, {fill: d => riding_lookup.get(d.properties.ED_NAME).Party,
                           fillOpacity: d => riding_opacity(riding_lookup.get(d.properties.ED_NAME)),
                           stroke: "white", strokeWidth: 0.5, clip: true,
                           render: map_hover(metro_districts)})
    ]
  })
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-4 nodetype="declaration"}
:::
::::
:::::

## Animated geography to bubble map


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="214" source-offset="0"}
// geometry only, so the animation keeps running across data updates, vote_map_update restyles it
vote_map_animation = {
  const svg = d3.create("svg")
      .attr("viewBox", [0, 0, anim_size.width, anim_size.height])
      .style("width", "100%")
      .style("height", "auto");

  svg.append("g")
    .selectAll("path")
    .data(districts)
    .join("path")
    .attr("class", "bcDistrict")
    .attr("fill", "#bbbbbb")
    .attr("d", d => district_paths.get(d))
    .attr("stroke", "white")
    .attr("stroke-width", 0.5);

  const cycle = () => {
    const paths = svg.selectAll(".bcDistrict");
    if (!paths.data().some(d => d.morph)) return; // no simulation yet
    // main shapes morph into their bubble and back, keeping the interpolator of the outbound leg for the return
    paths.filter(d => d.morph)
      .transition()
      .delay(d => d.properties.rank * 20)
      .duration(5000)
      .attrTween("d", function(d) {
        const morph = this.__morph = d.morph;
        return t => morph_path(morph(t));
      })
      .transition()
      .delay(d => 5000 + d.properties.rank * 20)
      .duration(5000)
      .attrTween("d", function() {
        const morph = this.__morph;
        return t => morph_path(morph(1 - t));
      })
      .on("end", function(d) { this.setAttribute("d", district_paths.get(d)); });
    // islands and other secondary pieces fade out instead of morphing
    paths.filter(d => !d.morph)
      .transition()
      .delay(d => d.properties.rank * 20)
      .duration(2500)
      .attr("opacity", 0)
      .transition()
      .delay(d => 7500 + d.properties.rank * 20)
      .duration(2500)
      .attr("opacity", 1);
  };
  const first = setTimeout(cycle, 2000);
  const timer = setInterval(cycle, 25000);
  invalidation.then(() => { clearTimeout(first); clearInterval(timer); });

  return svg.node();
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-5 nodetype="declaration"}
:::
::::
:::::

## Close races


:::::{.cell .hidden}

```{.js .cell-code code-fold="undefined" startFrom="274" source-offset="0"}
vote_map_update = {
  const voteMax = d3.max(ridings, d => d.Total) || 1;
  const radiusScale = d3.scaleSqrt().domain([0, voteMax]).range([1, maxRadius]);
  districts.forEach(d => {
    const r = riding_lookup.get(d.properties.ED_NAME);
    d.properties.riding = r;
    // before results come in size bubbles equally
    d.properties.radius = d.properties.mainShape ? (r.Total > 0 ? radiusScale(r.Total) : maxRadius / 2) : 0;
  });
  const main_shapes = districts.filter(d => d.properties.mainShape);
  applySimulation(main_shapes);
  // build the morphs up front, creating them when a transition starts makes the animation stutter
  main_shapes.forEach(d => {
    d.morph = flubber.toCircle(district_rings.get(d), d.x, d.y, d.properties.radius, {maxSegmentLength: 5, string: false});
  });
  d3.select(vote_map_animation)
    .selectAll(".bcDistrict")
    .attr("fill", d => fill_colour(d.properties.riding))
    .call(hover_tooltip, d => tooltip(d.properties.riding));
  return true;
}
```

::::{.cell-output .cell-output-display .hidden}

:::{#ojs-cell-6 nodetype="declaration"}
:::
::::
:::::



:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="299" source-offset="0"}
close_chart = {
  const close = ridings.filter(d => d.Total > 0 && (!d.called || d.lead_share < 0.05));
  if (!close.length) return md`*No close or uncalled races with results*`;
  return Plot.plot({
    title: "Close races and races not called yet",
    width,
    height: 50 + 22 * close.length,
    marginLeft: 200,
    marginRight: 100,
    x: {label: "Percentage point vote lead", tickFormat: pp},
    y: {label: null},
    color: colour_scale,
    marks: [
      Plot.barX(close, {x: "lead_share", y: "ED_NAME", fill: "Party", fillOpacity: riding_opacity, sort: {y: "x"},
                        title: tooltip, tip: "y"}),
      Plot.text(close, {x: "lead_share", y: "ED_NAME", text: d => `${fmt.comma(d.lead)} vote lead`, textAnchor: "start", dx: 4}),
      Plot.ruleX([0])
    ]
  })
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-7 nodetype="declaration"}
:::
::::
:::::

## Party lead in each riding


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="324" source-offset="0"}
lead_chart = {
  const rs = ridings.filter(d => d.Total > 0);
  if (!rs.length) return md`*Waiting for results*`;
  return Plot.plot({
    title: "Party lead in each riding",
    width,
    height: 50 + 14 * rs.length,
    marginLeft: 200,
    x: {label: "Percentage point lead", tickFormat: pp},
    y: {label: null},
    color: colour_scale,
    marks: [
      Plot.barX(rs, {x: "lead_share", y: "ED_NAME", fill: "Party", fillOpacity: riding_opacity, sort: {y: "x"},
                     title: tooltip, tip: "y"}),
      Plot.ruleX([0])
    ]
  })
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-8 nodetype="declaration"}
:::
::::
:::::

## Vote share of leading candidate


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="347" source-offset="0"}
winning_share_chart = {
  const rs = ridings.filter(d => d.Total > 0);
  if (!rs.length) return md`*Waiting for results*`;
  const majority = rs.filter(d => d.winner.Votes / d.Total > 0.5).length;
  return Plot.plot({
    title: "Vote share of winning candidate",
    subtitle: `${majority} of ${rs.length} leading candidates have more than 50% of the vote`,
    width,
    height: 50 + 6 * rs.length,
    x: {label: "Vote share of winning or leading candidate", tickFormat: "%", domain: [0, 1]},
    y: {axis: null},
    color: colour_scale,
    marks: [
      Plot.barX(rs, {x: d => d.winner.Votes / d.Total, y: "ED_NAME", fill: "Party", fillOpacity: riding_opacity,
                     sort: {y: "x", reverse: true}, title: tooltip, tip: "y"}),
      Plot.ruleX([0.5], {strokeDasharray: "4,4"})
    ]
  })
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-9 nodetype="declaration"}
:::
::::
:::::

## Vote share by party


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="371" source-offset="0"}
vote_distribution_chart = {
  const rs = ridings.filter(d => d.Total > 0);
  if (!rs.length) return md`*Waiting for results*`;
  const rows = rs.flatMap(r => r.results.map(c => ({ED_NAME: r.ED_NAME, Party: c.Party, Candidate: c.Candidate,
                                                    Votes: c.Votes, Share: c.Votes / r.Total})));
  const con_share = d => d3.sum(d.results.filter(c => c.Party === "CON"), c => c.Votes) / d.Total;
  return Plot.plot({
    title: "Vote share by party, ridings ordered by Conservative vote share",
    width,
    height: 50 + 6 * rs.length,
    x: {label: "Vote share", tickFormat: "%"},
    y: {axis: null, domain: d3.sort(rs, con_share).map(d => d.ED_NAME)},
    color: colour_scale,
    marks: [
      Plot.barX(rows, {x: "Share", y: "ED_NAME", fill: "Party", order: party_codes, tip: true,
                       title: d => `${d.ED_NAME}\n${d.Candidate} (${d.Party}): ${fmt.comma(d.Votes)} (${fmt.percent(d.Share)})`})
    ]
  })
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-10 nodetype="declaration"}
:::
::::
:::::





## Flipped seats 2026 vs 2024


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="393" source-offset="0"}
flip_chart = {
  // CBC's previous winner is based on the 2024 results, on the same boundaries
  const flips = ridings.filter(d => d.Total > 0 && d.Party !== d.previousParty);
  if (!flips.length) return md`*No seats flipped (yet)*`;
  const rows = d3.rollups(flips, v => v.length, d => d.previousParty, d => d.Party, d => d.called)
    .flatMap(([previousParty, ps]) => ps.flatMap(([Party, cs]) => cs.map(([called, n]) => ({previousParty, Party, called, n}))))
    .sort((a, b) => b.called - a.called);
  return Plot.plot({
    title: "Flipped seats 2026 vs 2024",
    subtitle: "By 2024 winner, coloured by 2026 winner or leader (lighter colour)",
    width,
    height: 60 + 40 * new Set(rows.map(d => d.previousParty)).size,
    marginLeft: 50,
    x: {label: "Number of seats"},
    y: {label: "2024 winner"},
    color: colour_scale,
    marks: [
      Plot.barX(rows, {x: "n", y: "previousParty", fill: "Party", fillOpacity: d => d.called ? 1 : leading_opacity, tip: true}),
      Plot.ruleX([0])
    ]
  })
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-11 nodetype="declaration"}
:::
::::
:::::

## Vote swings


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="421" source-offset="0"}
swing_chart = {
  const rs = ridings.filter(d => d.Total > 0);
  if (!rs.length) return md`*Waiting for results*`;
  const previous = d3.rollup(transpose(previous_results), v => d3.sum(v, d => d.Votes), d => d.ED_NAME, d => d.Party);
  const parties = party_codes.filter(p => !["IND", "Other", "No results"].includes(p));
  const rows = rs.flatMap(r => {
    const prev = previous.get(r.ED_NAME) || new Map();
    const prev_total = d3.sum(prev.values());
    return parties.map(Party => ({
      ED_NAME: r.ED_NAME, Party,
      Share: d3.sum(r.results.filter(c => c.Party === Party), c => c.Votes) / r.Total,
      PreviousShare: (prev.get(Party) || 0) / prev_total
    })).filter(d => d.Share > 0 || d.PreviousShare > 0);
  });
  return Plot.plot({
    title: "Party vote share by riding, 2026 vs 2024",
    width,
    height: width * 0.7,
    x: {label: "2024 vote share", tickFormat: "%", domain: [0, 0.8]},
    y: {label: "2026 vote share", tickFormat: "%", domain: [0, 0.8]},
    color: colour_scale,
    marks: [
      Plot.line([[0, 0], [0.8, 0.8]], {strokeDasharray: "4,4"}),
      Plot.dot(rows, {x: "PreviousShare", y: "Share", stroke: "Party", tip: true,
                      title: d => `${d.ED_NAME}\n${d.Party}: ${fmt.percent(d.PreviousShare)} → ${fmt.percent(d.Share)}`})
    ]
  })
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-12 nodetype="declaration"}
:::
::::
:::::

As usual, the code for this post is [available on GitHub](https://github.com/mountainMath/mountain_doodles/blob/main/posts/2026-10-24-bc-elections-2026/index.qmd) for anyone to reproduce or adapt for their own purposes.


<details>

<summary>Reproducibility receipt</summary>


::: {.cell}

```{.r .cell-code}
## datetime
Sys.time()
```

::: {.cell-output .cell-output-stdout}

```
[1] "2026-10-26 07:01:18 PDT"
```


:::

```{.r .cell-code}
## repository
git2r::repository()
```

::: {.cell-output .cell-output-stdout}

```
Local:    main /Users/jens/R/mountain_doodles
Remote:   main @ origin (https://github.com/mountainMath/mountain_doodles.git)
Head:     [64e512f] 2026-10-26: safeconduct and r2parquet
```


:::

```{.r .cell-code}
## Session info 
sessionInfo()
```

::: {.cell-output .cell-output-stdout}

```
R version 4.6.1 (2026-06-24)
Platform: aarch64-apple-darwin23
Running under: macOS Golden Gate 27.0.1

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRblas.0.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Vancouver
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] sf_1.1-3                  mountainmathHelpers_0.1.6
 [3] lubridate_1.9.5           forcats_1.0.1            
 [5] stringr_1.6.0             dplyr_1.2.1              
 [7] purrr_1.2.2               readr_2.2.0              
 [9] tidyr_1.3.2               tibble_3.3.1             
[11] ggplot2_4.0.3             tidyverse_2.0.0          

loaded via a namespace (and not attached):
 [1] generics_0.1.4     class_7.3-23       KernSmooth_2.23-26 stringi_1.8.9     
 [5] hms_1.1.4          digest_0.6.39      magrittr_2.0.5     evaluate_1.0.5    
 [9] grid_4.6.1         timechange_0.4.0   RColorBrewer_1.1-3 fastmap_1.2.0     
[13] jsonlite_2.0.0     e1071_1.7-17       DBI_1.3.0          scales_1.4.0      
[17] cli_3.6.6          rlang_1.3.0        units_1.0-1        withr_3.0.3       
[21] yaml_2.3.12        otel_0.2.0         tools_4.6.1        tzdb_0.5.0        
[25] geojsonsf_2.0.5    vctrs_0.7.3        R6_2.6.1           git2r_0.36.2      
[29] proxy_0.4-29       lifecycle_1.0.5    classInt_0.4-11    htmlwidgets_1.6.4 
[33] pkgconfig_2.0.3    pillar_1.11.1      gtable_0.3.6       Rcpp_1.1.2        
[37] glue_1.8.1         xfun_0.62          tidyselect_1.2.1   rstudioapi_0.19.0 
[41] knitr_1.52         farver_2.1.2       htmltools_0.5.9    rmarkdown_2.32    
[45] compiler_4.6.1     S7_0.2.2          
```


:::
:::


</details>


<details>
<summary>Remaining Observable code</summary>


::::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="451" source-offset="-0"}
cbc_url = "https://canopy.cbc.ca/live/elections/prov/BC2026/all/"
s3_url = "https://mountainmath.s3.ca-central-1.amazonaws.com/bc_2026_elections/"
refresh_seconds = 1800
```

:::::{.cell-output .cell-output-display}

::::{}

:::{#ojs-cell-13-1 nodetype="declaration"}
:::
::::
:::::

:::::{.cell-output .cell-output-display}

::::{}

:::{#ojs-cell-13-2 nodetype="declaration"}
:::
::::
:::::

:::::{.cell-output .cell-output-display}

::::{}

:::{#ojs-cell-13-3 nodetype="declaration"}
:::
::::
:::::
::::::


:::::{.cell .hidden}

```{.js .cell-code code-fold="undefined" startFrom="478" source-offset="0"}
// re-fetches the CBC feed, every cell depending on `live` re-runs on each update
live = {
  // CBC feed first, then the final copy uploaded to S3 by freeze_cbc() for when the CBC feed goes dark
  const sources = [{url: cbc_url, archived: false}, {url: `${s3_url}cbc_final.json`, archived: true}];
  while (true) {
    for (const {url, archived} of sources) {
      try {
        const response = await fetch(url, {cache: "no-store"});
        const json = response.ok ? await response.json() : null;
        if (json?.data?.ridings?.length) {
          yield {json, fetched: new Date(), archived};
          break;
        }
      } catch (e) {
        console.warn(`Could not load ${url}`, e);
      }
    }
    await Promises.delay(refresh_seconds * 1000);
  }
}
```

::::{.cell-output .cell-output-display .hidden}

:::{#ojs-cell-14 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="532" source-offset="0"}
party_meta = [
  ...live.json.data.parties
    .filter(p => p.e && p.e.colourLightElected)
    .sort((a, b) => a.displayOrder - b.displayOrder)
    .map(p => ({Party: p.englishCode, colour: p.e.colourLightElected})),
  {Party: "IND", colour: "#676767"},
  {Party: "Other", colour: "#aaaa11"},
  {Party: "No results", colour: "#bbbbbb"}
]
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-15 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="538" source-offset="0"}
party_codes = party_meta.map(d => d.Party)
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-16 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="562" source-offset="0"}
colour_scale = ({domain: party_codes, range: party_meta.map(d => d.colour), legend: true})
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-17 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="574" source-offset="0"}
leading_opacity = 0.4
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-18 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="578" source-offset="0"}
riding_opacity = d => d.called || d.Total === 0 ? 1 : leading_opacity
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-19 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="582" source-offset="0"}
fill_colour = d => {
  const c = party_meta.find(p => p.Party === d.Party).colour;
  return d.called || d.Total === 0 ? c : d3.interpolateRgb(c, "white")(1 - leading_opacity);
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-20 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="586" source-offset="0"}
// CBC party codes, candidates without affiliation come through as "NA"
clean_party = p => !p || p === "NA" ? "IND" : (party_codes.includes(p) ? p : "Other")
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-21 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="590" source-offset="0"}
ridings = live.json.data.ridings.map(r => {
  const cbc_results = r.parties
    .map(p => ({Candidate: `${p.firstName} ${p.lastName}`, Party: clean_party(p.partyCode), Votes: p.votes}));
  const enr_results = (enr_by_riding.get(r.englishName) || [])
    .map(c => ({Candidate: c.Candidate, Party: clean_party(c.Party), Votes: c.Votes}));
  // Elections BC is the authoritative count, use it unless CBC is further along
  const use_enr = enr_results.length > 0 && d3.sum(enr_results, d => d.Votes) >= d3.sum(cbc_results, d => d.Votes);
  const results = (use_enr ? enr_results : cbc_results).sort((a, b) => b.Votes - a.Votes);
  const Total = d3.sum(results, d => d.Votes);
  const lead = results[0].Votes - (results.length > 1 ? results[1].Votes : 0);
  return {
    source: use_enr ? "Elections BC" : "CBC",
    ED_NAME: r.englishName,
    results,
    Total,
    winner: results[0],
    Party: Total > 0 ? results[0].Party : "No results",
    lead,
    lead_share: Total > 0 ? lead / Total : 0,
    called: r.isCandidateElected === 1,
    pollsReported: r.pollsReported,
    totalPolls: r.totalPolls,
    previousParty: clean_party(r.previousElectedPartyCode)
  };
})
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-22 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="597" source-offset="0"}
// Elections BC candidate results pulled to S3, polled independently of the CBC feed
enr = {
  while (true) {
    try {
      const response = await fetch(`${s3_url}enr_results.csv?t=${Date.now()}`, {cache: "no-store"});
      yield response.ok ? d3.csvParse(await response.text(), d3.autoType) : [];
    } catch (e) {
      console.warn("Could not load Elections BC results", e);
      yield [];
    }
    await Promises.delay(refresh_seconds * 1000);
  }
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-23 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="602" source-offset="0"}
enr_by_riding = d3.group(enr, d => d.ED_NAME)
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-24 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="630" source-offset="0"}
riding_lookup = new Map(ridings.map(d => [d.ED_NAME, d]))
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-25 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="646" source-offset="0"}
// one floating tooltip for the hand-rolled hovers, shown immediately and following the pointer
tooltip_div = {
  const div = d3.select(document.body).append("div")
    .style("position", "fixed")
    .style("pointer-events", "none")
    .style("display", "none")
    .style("z-index", 1000)
    .style("max-width", "340px")
    .style("padding", "6px 8px")
    .style("background", "white")
    .style("border", "1px solid #ccc")
    .style("border-radius", "3px")
    .style("box-shadow", "0 1px 4px rgba(0,0,0,0.2)")
    .style("font", "12px sans-serif")
    .style("white-space", "pre");
  invalidation.then(() => div.remove());
  return div.node();
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-26 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="650" source-offset="0"}
hover_tooltip = (selection, text) => {
  const div = d3.select(tooltip_div);
  const move = event => {
    const {width, height} = tooltip_div.getBoundingClientRect();
    const x = event.clientX + 14 + width > window.innerWidth ? event.clientX - 14 - width : event.clientX + 14;
    const y = event.clientY + 14 + height > window.innerHeight ? event.clientY - 14 - height : event.clientY + 14;
    div.style("left", `${Math.max(0, x)}px`).style("top", `${Math.max(0, y)}px`);
  };
  selection
    .on("pointerenter", function(event, d) {
      // raise() re-inserts the node which can fire pointerenter again, so only record the original stroke once
      if (!this.__stroke) this.__stroke = [this.getAttribute("stroke"), this.getAttribute("stroke-width")];
      d3.select(this).raise().attr("stroke", "black").attr("stroke-width", 1.5);
      div.style("display", "block").text(text.call(this, d));
      move(event);
    })
    .on("pointermove", move)
    .on("pointerleave", function() {
      if (this.__stroke) d3.select(this).attr("stroke", this.__stroke[0]).attr("stroke-width", this.__stroke[1]);
      this.__stroke = null;
      div.style("display", "none");
    });
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-27 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="654" source-offset="0"}
// hover on the actual riding shapes, Plot's own tip snaps to the nearest centroid which misses on large ridings
map_hover = features => (index, scales, values, dimensions, context, next) => {
  const g = next(index, scales, values, dimensions, context);
  const paths = d3.select(g).selectAll("path");
  paths.each(function(_, j) { this.__riding = riding_lookup.get(features[index[j]].properties.ED_NAME); });
  hover_tooltip(paths, function() { return tooltip(this.__riding); });
  return g;
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-28 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="675" source-offset="0"}
tooltip = d => {
  if (d.Total === 0) return `${d.ED_NAME}\nNo results yet`;
  return [
    d.ED_NAME,
    (d.called ? `Winner: ` : `Leading: `) + `${d.winner.Candidate} (${d.winner.Party})`,
    `Margin: ${fmt.comma(d.lead)} (${fmt.percent(d.lead_share)})`,
    `${fmt.comma(d.pollsReported)} of ${fmt.comma(d.totalPolls)} polls reported, votes from ${d.source}`,
    "",
    ...d.results.map(p => `${p.Candidate} (${p.Party}): ${fmt.comma(p.Votes)} (${fmt.percent(p.Votes / d.Total)})`)
  ].join("\n");
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-29 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="701" source-offset="0"}
fmt = ({
  percent: d3.format(".1%"),
  comma: d3.format(",.0f")
})
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-30 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="712" source-offset="0"}
pp = d => `${d3.format(".0f")(d * 100)}pp`
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-31 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="726" source-offset="0"}
boundaries = JSON.parse(Array.isArray(boundaries_json) ? boundaries_json[0] : boundaries_json)
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-32 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="733" source-offset="0"}
districts = boundaries.features
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-33 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="737" source-offset="0"}
metro_districts = JSON.parse(Array.isArray(metro_json) ? metro_json[0] : metro_json).features
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-34 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="741" source-offset="0"}
// leave a margin around the map so bubbles pushed out by the collision force stay on the canvas
anim_size = ({width: 960, height: 640, margin: 20})
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-35 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="745" source-offset="0"}
projection = d3.geoIdentity().reflectY(true)
  .fitExtent([[anim_size.margin, anim_size.margin], [anim_size.width - anim_size.margin, anim_size.height - anim_size.margin]], boundaries)
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-36 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="749" source-offset="0"}
path = d3.geoPath(projection)
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-37 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="754" source-offset="0"}
district_paths = new Map(districts.map(d => [d, path(d)]))
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-38 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="759" source-offset="0"}
// screen space outer rings of the main shapes, with sub-pixel detail removed to keep the morphs light
district_rings = {
  const rdp = (pts, eps) => {
    if (pts.length < 3) return pts;
    const [a, b] = [pts[0], pts[pts.length - 1]];
    const dx = b[0] - a[0], dy = b[1] - a[1], len = Math.hypot(dx, dy) || 1e-9;
    let index = 0, dmax = 0;
    for (let i = 1; i < pts.length - 1; i++) {
      const dist = Math.abs(dy * pts[i][0] - dx * pts[i][1] + b[0] * a[1] - b[1] * a[0]) / len;
      if (dist > dmax) { dmax = dist; index = i; }
    }
    return dmax > eps ? [...rdp(pts.slice(0, index + 1), eps).slice(0, -1), ...rdp(pts.slice(index), eps)] : [a, b];
  };
  // closed ring: split at the point furthest from the start and simplify both halves
  const simplifyRing = (ring, eps) => {
    const pts = ring.slice(0, -1);
    const far = d3.maxIndex(pts, p => Math.hypot(p[0] - pts[0][0], p[1] - pts[0][1]));
    return [...rdp(pts.slice(0, far + 1), eps).slice(0, -1), ...rdp([...pts.slice(far), pts[0]], eps).slice(0, -1)];
  };
  return new Map(districts
    .filter(d => d.properties.mainShape)
    .map(d => [d, simplifyRing(d.geometry.coordinates[0].map(projection), 0.3)]));
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-39 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="763" source-offset="0"}
// flubber's own path strings carry full precision, which is most of the per-frame cost
morph_path = pts => "M" + pts.map(p => `${p[0].toFixed(1)},${p[1].toFixed(1)}`).join("L") + "Z"
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-40 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="767" source-offset="0"}
maxRadius = 15
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-41 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="793" source-offset="0"}
nodePadding = 0.3
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-42 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="798" source-offset="0"}
applySimulation = (nodes) => {
  const simulation = d3.forceSimulation(nodes)
    .force("cx", d3.forceX().x(d => anim_size.width / 2).strength(0.02))
    .force("cy", d3.forceY().y(d => anim_size.height / 2).strength(0.02))
    .force("x", d3.forceX().x(d => projection([d.properties.x, d.properties.y])[0]).strength(0.3))
    .force("y", d3.forceY().y(d => projection([d.properties.x, d.properties.y])[1]).strength(0.3))
    .force("charge", d3.forceManyBody().strength(-1))
    .force("collide", d3.forceCollide().radius(d => d.properties.radius + nodePadding).strength(1))
    .stop()

  let i = 0;
  while (simulation.alpha() > 0.01 && i < 250) {
    simulation.tick();
    // keep every bubble fully inside the canvas whatever the vote totals do to the sizes
    nodes.forEach(d => {
      const r = d.properties.radius + nodePadding;
      d.x = Math.max(r, Math.min(anim_size.width - r, d.x));
      d.y = Math.max(r, Math.min(anim_size.height - r, d.y));
    });
    i++;
  }

  return simulation.nodes();
}
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-43 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="802" source-offset="0"}
d3 = import("https://cdn.jsdelivr.net/npm/d3@7.9.0/+esm")
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-44 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="806" source-offset="0"}
Plot = import("https://cdn.jsdelivr.net/npm/@observablehq/plot@0.6.16/+esm")
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-45 nodetype="declaration"}
:::
::::
:::::


:::::{.cell}

```{.js .cell-code code-fold="undefined" startFrom="833" source-offset="0"}
flubber = require("flubber@0.4.2")
```

::::{.cell-output .cell-output-display}

:::{#ojs-cell-46 nodetype="declaration"}
:::
::::
:::::

</details>