---
title: "Introducing canivt"
author:
  - name: Jens von Bergmann
    affiliation: MountainMath
date: '2026-07-28'
slug: introducing-canivt
description: 'The canivt R package parses Beyond 20/20 IVT files into R and enables modern data workflows based on StatCan IVT data.'
image: 'https://mountainmath.github.io/canivt/logo.png'
categories: 
  - geeky
  - Open Data
bibliography: ../../common_literature.bib 
code-tools:
  toggle: true
code-fold: show
fig-width: 8
execute:
  cache: true
  message: false
  warning: false
---


Almost every organization that operates over extended periods of time accumulates legacy infrastructure, and Statistics Canada is certainly no stranger to this phenomenon. With some effort one can find workarounds for many of these, but there is one bit of data infrastructure that has been particularly infuriating: The Beyond 20/20 IVT file format.

# TL;DR
<img src="https://mountainmath.github.io/canivt/logo.png" style="float:right;max-width:30%;width:200px;margin-left:10px;"/>
The [**canivt**](https://mountainmath.github.io/canivt/) package reads Statistics Canada's Beyond 20/20 IVT files in R and converts them to CSV or parquet. Point it at a StatCan catalogue number, or the id of a cross tabulation hosted on Borealis, and it downloads the file, parses it, and hands back a connection to a parquet database.

That matters because IVT is a closed format adopted by (the census division of) Statistics Canada in the early 1990s, and a large corpus of census data is still locked in it: Older census data, where it is the only way to get the table, and custom tabulations, which for most of that period were released this way and no other. Getting at any of it has meant installing proprietary Windows-only software and exporting to CSV by hand: cumbersome, full of idiosyncratic manual choices, prone to silent data loss on low-memory computers, and fatal to any automated or reproducible workflow starting from authoritative StatCan data.


::: {.cell}

```{.r .cell-code}
library(tidyverse)
# remotes::install_github("mountainmath/canivt")
library(canivt)
```
:::




::: {.cell}

```{.r .cell-code}
data <- get_statcan_ivt("1003011")

data |>
  filter(geo_uid == "59") |>
  label_ivt_columns() |>
  collect_ivt()
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 330 × 8
   geo_label     geo_name geo_uid Single Years of Age …¹ `Sex (3)`  value symbol
   <chr>         <chr>    <chr>   <fct>                  <fct>      <dbl> <fct> 
 1 British Colu… British… 59      Total - Age Groups     Total - … 3.28e6 <NA>  
 2 British Colu… British… 59      Total - Age Groups     Male      1.63e6 <NA>  
 3 British Colu… British… 59      Total - Age Groups     Female    1.66e6 <NA>  
 4 British Colu… British… 59      0-4 years              Total - … 2.21e5 <NA>  
 5 British Colu… British… 59      0-4 years              Male      1.13e5 <NA>  
 6 British Colu… British… 59      0-4 years              Female    1.08e5 <NA>  
 7 British Colu… British… 59      Under 1                Total - … 4.41e4 <NA>  
 8 British Colu… British… 59      Under 1                Male      2.24e4 <NA>  
 9 British Colu… British… 59      Under 1                Female    2.17e4 <NA>  
10 British Colu… British… 59      1                      Total - … 4.49e4 <NA>  
# ℹ 320 more rows
# ℹ abbreviated name: ¹​`Single Years of Age (110)`
# ℹ 1 more variable: status <fct>
```


:::
:::


If the primary goal is just to extract the data, then the `ivt_write_csv` streams a local IVT file to CSV.


::: {.cell}

```{.r .cell-code}
# download StatCan IVT or point to any local IVT file
local_ivt_path <- statcan_ivt_catalogue() |>
  filter(catalogue == "1003011") |>
  statcan_ivt_download() 
out_csv_path <- file.path(tempdir(),"1003011.csv")

ivt_write_csv(local_ivt_path, out_csv_path)
```
:::



The key contribution is reverse-engineering the IVT file format itself, the package is the usable end of that.

# What is Beyond 20/20

Beyond 20/20 is a catchall for either an undocumented data format, "Beyond 20/20 IVT tables", or the proprietary Windows-only software that reads (free) or writes (paid) this data. The Beyond 20/20 software is designed to handle cross-tabulations where data can be sliced along several *dimensions* in a table-based interface.

Beyond 20/20 is a product of its time. The file format is trying to save space while not having access to modern compression. The browser gives a table view that allows for iterative filtering and pivoting. At the time data work was done in dinosaur systems like SAS and usage of these tools was low. Beyond 20/20 offered an intuitive Excel-like experience that broadened the user base of the data. The trade-off was a closed data format and loss of control over the data format, as well as the tools to extract data for analysis.

When the census division settled on IVT in the early 90s, StatCan data was itself closed up. It was only available after paying a fee, under a license that did not allow to share or redistribute it. Against that backdrop, releasing it in a closed format was not the contradiction it looks like today. What has not aged well is everything since. Statistics Canada switched to an open data model in 2012, and more than a decade later a good chunk of their data is still available only as Beyond 20/20 IVT. That is an ongoing failure of data governance, and it severely limits the usefulness of the data.

As we have [noted before](https://doodles.mountainmath.ca/canpumf) in relation to the **canpumf** package [@canpumf], at least the data is *available*. And where it matters enough, as in this case, people outside the organization can put in the work and turn it into open data by breaking down the barriers to accessing it.

# What to do about Beyond 20/20

For standard census products IVT is not always the only option. There is SDMX for data from the early 2000s onward, and before that an HTML interface where the data can be viewed one slice at a time and scraped with enough effort and patience. Both are annoying and time-consuming, but they exist. For custom tabulations they generally do not: IVT has traditionally been the only way StatCan would distribute them, with no alternative format at all.

So researchers wanting the data have to bite the bullet, find a Windows machine, fiddle with the Beyond 20/20 browser, extract what they need by hand. Or in a lot of cases, simply don't bother, and bump the research that would have needed Beyond 20/20 data to some other time. It is hard to know how much work never happens for this reason, which is its own kind of cost. Either way, fully reproducible research on Beyond 20/20 data is impossible, because the manual extraction process is highly idiosyncratic.

Using the browser is painful in ways anyone with even passing exposure to modern data practices will recognize, and extracting a dataset in bulk is surprisingly difficult. In many cases it's impossible to extract the geographic data, the geo codes, and the quality flags at the same time. And it is a manual step that breaks reproducibility: chances are high that the next person extracts the data differently. Modern data practices require a more principled and programmatic way to get at the data and the metadata. Which would be straightforward, except that the format is proprietary and undocumented. Additionally, the export of large tables can cut short due to memory constraints, leading to silent data loss at export.

# Reverse engineering Beyond 20/20 IVT tables

This is the tricky part. We did give this a try a couple of years back but gave up. Recently we had another need to look at IVT tables and decided to give it another go, this time bringing our sidekick Claude along to help out with the more repetitive and brute-force pieces of cracking the encoding.

We know that the format was formalized sometime in the very early 1990s. That takes more modern data compression and encoding algorithms out of the picture and lets us focus on fairly simple ways someone might have coded something like this around 1990. From some of the performance issues we can also conclude that the encoding is likely not terribly sophisticated. This gives us a starting point and some hope that decoding this is possible.

The other reason for why this might be doable is that we have data to validate against, either the scraped HTML for older data, or the SDMX or CSV for modern data. In essence this is attempting a *Known-Plaintext Attack*, trying to break the IVT cipher.

A firm rule kept the whole thing honest: parsing has to be driven by metadata and markers found in the file itself. The validation data is for *checking* a hypothesis, never for encoding an answer. A parser that special-cases the tables it has already seen is not a parser, and the corpus is full of vintages the next file will not resemble.

A handful of quirks account for most of the difficulty, and they are more interesting than the process of finding them.

The first is how the cells are laid out. A table nests its dimensions positionally, innermost to outermost, into fixed 256-byte pages. Exactly one dimension straddles a page boundary: as much of it as fits stays in the page, and the remainder becomes windows addressed through a directory.

Everything in that nesting is padded to powers of two. A dimension with five members will typically occupy eight slots, and one with ten members can occupy thirty-two. A format that goes to real lengths to avoid storing zeros is perfectly happy to leave two thirds of its addressing space empty, because power-of-two strides turn address arithmetic into bit shifts. Disk space was expensive in the 1990s and modern compression was not available, and IVT pays close attention to that.

Then there is byte order. Whether a given cell was written at all is recorded in a bitmap, and the bitmap's bytes are stored in swapped pairs (byte 0 and byte 1 exchanged, 2 and 3, and so on) while the values that follow are stored in file order. It's an artifact of whatever 16-bit machine the format was designed on around 1990. Read straight through, the bitmap *almost* lines up, which is the awkward part: it's off by a bit here and a byte there, never consistently enough to look like a byte-order problem rather than a subtly wrong addressing scheme.

The same 16-bit heritage shows up in the file's own pointers. The header stores the location of the page directory in two bytes, which stops working the moment a table grows past 64 kilobytes, and essentially all of them do. The high bits are simply gone. You recover the real location by adding multiples of 65,536 to the stored value until one of them lands on something that reads as a valid directory. The format outgrew its own address space and nobody widened the field.

The quirk with the most consequences is that the presence bitmap addresses members by *slot* rather than by position. Each dimension declares a fixed number of slots, and every slot is flagged as live, deleted, or never used. A deleted one still carries the member's code, just without the length prefix a live one gets. So the file records not only the categories a table has but the ones somebody removed while building it: one Business Patterns industry axis carries 929 industries across 949 used slots, with 20 deleted. This is the clearest sign that IVT was never really an export format. It is a live, editable database file that happens to get shipped. It also has teeth, because a hole is invisible if you assume members are contiguous: read them positionally and every member after the gap is silently attached to the wrong data.

Last, the hierarchy among members is stored as whitespace. Census dimensions are deeply nested, "Total income" contains "With income" contains "Under $10,000", and the format encodes that nesting as leading spaces in the display label. There is no parent field, no depth field. The tree exists only as indentation inside a string meant for a table header, which makes the structure exactly as durable as the presentation. And the indent width is not even constant across files: most tables use two spaces per level, but the census of agriculture tables use one, running Canada, province, agricultural region, census division and subdivision across zero to four spaces.

Nothing about the format is sophisticated. That is exactly what makes it hard -- there is no elegant structure to reason your way toward, just a pile of small decisions someone made thirty-five years ago and never made public.

That is a tour rather than a specification. The byte-level account -- page geometry, the header slot table, the codebook blocks, the bit-level slot records -- is written up as the [file format article](https://mountainmath.github.io/canivt/articles/ivt-format.html), which also ships with the package as `vignette("ivt-format", package = "canivt")`. Underneath it sit the working notes the parser was built from and is still checked against: a [byte-marker catalog](https://github.com/mountainMath/canivt/blob/main/inst/notes/markers.md), which the test suite verifies against the code so the documentation cannot drift from the recognizer, and a [decode history](https://github.com/mountainMath/canivt/blob/main/inst/notes/decode-history.md) recording how each vintage was cracked and why each invariant is worded the way it is. Both are in the repository and both ship inside the package, via `system.file("notes", package = "canivt")`.

# canivt

The **canivt** package provides the functionality to read and process IVT files and export the data and metadata. Metadata matters as much as the data here. The package recovers the dimension members and their hierarchy, the geographic names and standard identifiers (DGUIDs and their older equivalents), the data-quality flags, the footnotes and how they relate to the data, and the French labels alongside the English, all from the same single file.

Additionally, it provides functionality to explore and search IVT data from two repositories, the Statistics Canada repository of standard release census data and the Borealis dataverse, a crowd-sourced repository of, among other things, IVT files from (custom) census data extracts. The latter are data tables individual researchers have made available to the general public. Either because sharing their custom extracts is the right thing to do, or because Tri-Council grants now require to make them publicly accessible and Borealis is a convenient platform to do so.

Ideally these custom tabulations on Borealis are unique extracts that are not available from the official StatCan repository, but unfortunately the Borealis repository is heavily polluted by researchers simply posting standard Statistics Canada products. This practice creates no additional value and pollutes the Borealis repository, making data discovery unnecessarily difficult. And it complicates data provenance, where people might cite the researchers who uploaded the data instead of referring directly to the standard StatCan catalogue number when citing the data source.

There are many people who have pulled custom tabulations and never made them publicly available, if you have IVT files that could be of value to others, please consider adding them to the Borealis dataverse, whether they were funded via Tri-Council grants or not.

The **canivt** package is fairly mature at this point. Every release runs against an extensive test suite. Virtually every part of the IVT files has been decoded with only very short sections without a clear meaning assigned to them, the most compact summary is in the [coverage tracker](https://github.com/mountainMath/canivt/blob/main/inst/notes/coverage.md).

The package documentation has a [getting-started guide](https://mountainmath.github.io/canivt/articles/getting-started.html), an article on searching and pulling tables off [Borealis](https://mountainmath.github.io/canivt/articles/borealis.html), and one on [onboarding an IVT that came from neither index](https://mountainmath.github.io/canivt/articles/custom-files.html), e.g. a custom order someone emailed you, or a file you downloaded by hand.

That said, it may well still choke on some IVT files or emit a loud warning. If it does, please [open an issue](https://github.com/mountainMath/canivt/issues) with a link to the offending IVT and a brief description of the failure.


# Echoes of Beyond 20/20 IVT

An interesting by-product of this decoding exercise has been understanding the echoes of the format in modern release data. The New Dissemination Model (NDM) is the standard way Statistics Canada releases data today, and it is well thought out and broadly aligned with modern practices. But the census division was quite late to it, disseminating via the NDM only from the 2021 census, and without re-releasing older data on the new standard.

And importantly, the census data only gets disseminated through an amended NDM that degraded the NDM standard in several key ways. Census data arrives in semi-wide form, aligning with standard IVT display and export settings instead of modem practices of distributing data in tidy long-form. To make things worse, the meaning of variables can't be derived from column names alone. Some column names are duplicated, and the meaning of these has to be inferred by column order. This is a big red flag, a major failure of data governance. And really surprising, given how well-structured and thought out the core NDM standard is. And there is absolutely no reason why the data could not be distributed in tidy long form, which would avoid this. Whoever forced this degradation of the NDM standard and though it a good idea to disseminate data this way needs to give their head a shake.

The other important way in which Beyond 20/20 IVT files shine through is how zeros are handled. IVT skips encoding all zero values, but reveals their presence later in a sequence of bitmaps that mark the position of missing/present data in the dimensions grid and then further distinguishes zeros from truly missing data points and, in modern versions, the reason for which data is missing (e.g. suppressed, not applicable, ...). In principle these IVT internals should be completely opaque to data disseminated via the NDM, and in the bulk CSV download it is indeed handled in such a way with zeros filled in and missing data markers as appropriate. But something goes wrong with the NDM endpoint for pinpointed data query by table and coordinate. This can probably be best seen when looking at an example.

Table 98-10-0040 crosses structural type of dwelling with household size, for Canada, the provinces and territories, and every census metropolitan area and agglomeration. It is published both as an IVT file and through the NDM, it is small enough to walk coordinate by coordinate in full, and it contains all three of the things a cell can be when it is not a number: a zero, a suppressed value, and a not-applicable.


::: {.cell}

```{.r .cell-code}
tab   <- ivt_read_table("98100040")                     # the published table
store <- ivt_read_table("98100040", complete = FALSE)   # only what the file stores

c(cells_in_the_table = prod(tab$metadata$dimension_counts),
  cells_in_the_file  = nrow(store$cells))
```

::: {.cell-output .cell-output-stdout}

```
cells_in_the_table  cells_in_the_file 
             12528              11592 
```


:::
:::


Nine hundred and thirty-six cells never made it into the file. Putting them back is the reader's job, and it is not the one-liner it looks like, because the reader also has to know *why* each one is absent. **canivt** does that by default: `$cells` is the full grid, an absent cell that is a zero written out as `0`, and an absent cell that is anything else written as `NA` carrying the symbol the file gives it. Here is one slice of it: The tall apartment buildings of Corner Brook, Newfoundland.


::: {.cell}

```{.r .cell-code}
tab |>
  collect_ivt() |>
  filter(geo_uid == "2021S0504015",                 # Corner Brook CA
         grepl("five or more storeys", structural)) |>
  select(household, value, symbol)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 8 × 3
  household                               value symbol
  <fct>                                   <dbl> <fct> 
1 Total - Household size                      5 <NA>  
2 1 person                                    0 <NA>  
3 2 persons                                   5 <NA>  
4 3 persons                                   0 <NA>  
5 4 persons                                   0 <NA>  
6 5 or more persons                           0 <NA>  
7 Number of persons in private households     5 <NA>  
8 Average household size                     NA X     
```


:::
:::


We can see the same structure in the CSV download, here accessed via the **cansim** package.


::: {.cell}

```{.r .cell-code}
library(cansim)

get_cansim("98-10-0040") |>
  filter(startsWith(COORDINATE, "3.7.")) |>
  select(COORDINATE,`Household size (8)`, VALUE, Symbol)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 8 × 4
  COORDINATE `Household size (8)`                    VALUE Symbol
  <chr>      <fct>                                   <dbl> <chr> 
1 3.7.1      Total - Household size                      5 <NA>  
2 3.7.2      1 person                                    0 <NA>  
3 3.7.3      2 persons                                   5 <NA>  
4 3.7.4      3 persons                                   0 <NA>  
5 3.7.5      4 persons                                   0 <NA>  
6 3.7.6      5 or more persons                           0 <NA>  
7 3.7.7      Number of persons in private households     5 <NA>  
8 3.7.8      Average household size                     NA x     
```


:::
:::


A handful of households, all of them two-person statistically rounded to `5` or `0` and then, in the last row, no number at all. Technically the average household size is 2 based on the result of the statistical rounding, disclosing the actual average household size would reveal finer information on pre-rounded data and is suppressed.

Now ask the NDM for the same eight cells by table and coordinate. The eight cells above are coordinates `3.7.1` through `3.7.8`. We also add a ninth non-existent coordinate `3.7.9` as an additional point of reference. Instead of using the **cansim** package for this it's instructional to manually pull the exact responses.


::: {.cell}

```{.r .cell-code}
library(httr2)

ndm_coordinate <- function(pid, coordinate) {
  coordinate <- paste(c(strsplit(coordinate, ".", fixed = TRUE)[[1]],
                        rep("0", 10))[1:10], collapse = ".")
  r <- request("https://www150.statcan.gc.ca/t1/wds/rest/getDataFromCubePidCoordAndLatestNPeriods") |>
    req_body_json(list(list(productId = pid, coordinate = coordinate, latestN = 1))) |>
    req_perform() |>
    resp_body_json()
  point <- r[[1]]$object$vectorDataPoint
  tibble(coordinate  = coordinate |> gsub("(\\.0)+$","", x=_),
         status      = r[[1]]$status,
         value       = if (length(point)) point[[1]]$value %||% NA_real_ else NA_real_,
         status_code = if (length(point)) point[[1]]$statusCode else NA_integer_,
         suppressed  = if (length(point)) point[[1]]$securityLevelCode == 1 else NA)
}

map_dfr(1:9, \(h) ndm_coordinate(98100040, paste("3.7", h, sep = ".")))
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 9 × 5
  coordinate status  value status_code suppressed
  <chr>      <chr>   <dbl>       <int> <lgl>     
1 3.7.1      SUCCESS     5           0 FALSE     
2 3.7.2      FAILED     NA          NA NA        
3 3.7.3      SUCCESS     5           0 FALSE     
4 3.7.4      FAILED     NA          NA NA        
5 3.7.5      FAILED     NA          NA NA        
6 3.7.6      FAILED     NA          NA NA        
7 3.7.7      SUCCESS     5           0 FALSE     
8 3.7.8      SUCCESS    NA           0 TRUE      
9 3.7.9      FAILED     NA          NA NA        
```


:::
:::


Of the legitimate coordinates, three of the eight come back as values. Four fail, their true value is zero, but the API marks them as a failed request. And `3.7.8`, `Average household size`, is not zero, and it is not absent because it is zero, it is *suppressed*, withheld for confidentiality. And the NDM says so plainly: the request succeeds, the value is null, and `securityLevelCode` marks it. The API has a full vocabulary for this. (There is also the richer vocabulary of status codes that is designed to give more detail on the suppression, but it is not used by the API.)

The last one, `3.7.9`, which asks for a household size this table does not have, fails in exactly the same way as the other four. Nothing in the response distinguishes "this value is zero" from "you asked for something that does not exist", so a client walking a table by coordinate cannot tell a real zero from its own bug. Other API endpoints that can be called by table and coordinate, like `getSeriesInfoFromCubePidCoord`, can't help distinguish this either. We could distinguish these cases by querying overall table dimensions to detect that coordinate `3.7.9` falls outside of the declared table dimension and member grid and infer zero values for all "failed" API queries that fall within the grid. But this is extremely brittle and breaks the very point of having an API as a reliable and principled way to query data.

In the **cansim** package we have noticed this issue a couple of years ago and emit a loud warning, dropping data points that are marked as "failed" from the table but keeping track of them in a sidecar for inspection if desired. 


::: {.cell}

```{.r .cell-code}
cansim_pull <-  get_cansim_data_for_table_coord_periods(
  list("98100040"=paste("3.7", seq(1,9), sep = "."))) |>
  select(COORDINATE,`Household size (8)`, VALUE)
```
:::


The failed coordinates census does retain this contextual information and the **cansim** package leaves it up to the user to reconcile this. 


::: {.cell}

```{.r .cell-code}
attr(cansim_pull,"failed_coordinates_census")
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 5 × 2
  cansimTableNumber COORDINATE         
  <chr>             <chr>              
1 98100040          3.7.2.0.0.0.0.0.0.0
2 98100040          3.7.4.0.0.0.0.0.0.0
3 98100040          3.7.5.0.0.0.0.0.0.0
4 98100040          3.7.6.0.0.0.0.0.0.0
5 98100040          3.7.9.0.0.0.0.0.0.0
```


:::
:::


At the time we were lacking the context of the connection to the IVT file format.^[I reported this issue to StatCan at the time, and the answer I got was that this behaviour was "requested by the Census team". The underlying mechanism is different from the IVT files though, the underlying database has the data and it serves it when it's requested as CSV. But the census division requested the API use a ‘Default replacement symbol’ of 0 instead of the usual ‘Not available’, and this is what drops zeros from the API response.]

The NDM whole table download recovers the full data, and the **canivt** package does the same and adds the zeros back into the data file, as well as markers for suppressed data or data missing for other reasons. But the table and coordinate NDM endpoints mirrors the narrow IVT data handling that drops zeros, and does not use the auxiliary information available in the IVT, as well as in the whole table download of the NDM, to add the zeros back in.

So the principled stance exists inside the NDM; it is just not the one the coordinate API takes. Which leaves the awkward situation that the *same table*, from the *same agency*, answers the question differently depending on which door you come in through, and the door that answers badly is the one you use when you want just a (small) sub-selection of numbers rather than a whole download.

What makes the coordinate API genuinely awkward is that it is not even consistent across the census cubes. Whether a coordinate query can answer with a zero is settled per table, by whether the zeros were written out when the cube was loaded, and the NDM's own metadata gives that away: it reports the number of data points stored, which you can compare against the size of the full cross-tabulation.


::: {.cell}

```{.r .cell-code}
ndm_cube <- function(pid) {
  o <- request("https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata") |>
    req_body_json(list(list(productId = pid))) |>
    req_perform() |>
    resp_body_json()
  o <- o[[1]]$object
  tibble(pid            = pid,
         cells_in_table = prod(vapply(o$dimension, \(d) length(d$member), numeric(1))),
         stored_in_cube = o$nbDatapointsCube)
}

whole_numbers <- function(pid) {
  v <- ivt_read_table(pid, complete = FALSE)$cells$value   # the values the file stores
  mean(abs(v - round(v)) < 1e-9)
}

map_dfr(c(98100040, 98100044, 98100045, 98100167, 98100563, 98100096), \(p)
  ndm_cube(p) |> mutate(zeros_written = stored_in_cube == cells_in_table,
                        whole_numbers = round(whole_numbers(p), 2)))
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 5
       pid cells_in_table stored_in_cube zeros_written whole_numbers
     <dbl>          <dbl>          <int> <lgl>                 <dbl>
1 98100040          12528          11651 FALSE                  0.88
2 98100044            448            399 FALSE                  1   
3 98100045          13440           9218 FALSE                  1   
4 98100167          24633          24633 TRUE                   0.13
5 98100563           7038           7038 TRUE                   0.11
6 98100096          54680          54680 TRUE                   0.22
```


:::
:::


Across all 525 census cubes, 69 write their zeros and 456 do not, and this split does not seem to depend on file size, complexity or date of the product.

What it does track is what kind of number the table holds, and that is visible in the IVT file itself and shown in the last column above. Tables of counts, the rounded whole numbers that come out of a census tabulation, arrive sparse. Tables of rates, averages, percentages and quality indicators arrive complete. Across the forty-odd census tables we measured, the fraction of whole numbers in the IVT separates the two behaviours cleanly with nothing landing in between, and it held on the tables we picked out after forming the rule. Which makes sense, in a backwards sort of way. A count table inherits the convention that an absent cell means zero, because that is how the IVT wrote it; a table of averages has no such convention, since 0.0% is a computed number like any other, so every cell gets written out. The zeros do not go missing because anyone decided they should. They go missing when the table is the kind of table whose zeros were never stored in the first place. (There is a second, smaller group of complete cubes: tables whose IVT has almost no empty cells to begin with, which arrive whole for want of anything to drop.)

None of it looks arbitrary [once you have seen the bytes](https://mountainmath.github.io/canivt/articles/ivt-format.html). An IVT table stores only its non-zero cells. Each page carries a fixed-size bitmap saying which cells were written, and the values follow in bitmap order. A zero is simply a bit that was never set, costing nothing. That is a thoroughly sensible design for 1990, when the alternative was writing eight bytes of zero for every empty cell in a sparse cross-tabulation.

What it is not is lossy. After the values, the same page carries the status block -- a second bitmap on some vintages, a packed array of reason codes on others -- which is where Corner Brook's suppressed average was sitting the whole time. The sparseness and the meaning were separated, not traded off against each other. So the thing inherited by a REST API three decades later is not the storage constraint, which would at least have been an explicable inheritance. It is the *habit* of treating an absent cell as a zero, carried forward past a format that itself never required it. And carried forward selectively, since the CSV writer for the very same cubes does not do it.

That is the part worth insisting on. Sparse storage is a fine thing for a file to do. It is not a fine thing for an interface to do without saying so. The IVT gets this right by writing down, next to the values, which absences are zeros and which are something else. The bulk CSV gets it right by publishing every cell and naming the exception. The coordinate API is the one place in the chain where the storage layout shows through to the caller -- and the effect is that whether you can trust a zero to be reported depends on which table you happen to be asking about. **canivt** picks the CSV's side: a completed grid by default, symbols where the file gives them, and the sparse store still there under `complete = FALSE` if that is what you actually want.

# Upshot

Being able to read IVT files programmatically has been on our list for a long time. Hopefully others will find this useful too. Older census data and custom tabulations provided by StatCan are *available data*, the **canivt** package turns them into **open data** by making the data and metadata readable by open statistical software. Statistics Canada data quality is generally very high, and the work that has gone into the New Dissemination Model shows that some effort has been paid to *data governance*. That makes it all the more astounding that Statistics Canada still has such large and lasting gaps in data governance in other parts of their data dissemination system. And quite possibly also in their non-public data, it's hard to believe that these are just issues with external data, and the degradation of the NDM for census data releases point to much deeper underlying issues.

For not the package lives on GitHub only, we will spend some more time testing and collecting feedback before submitting to CRAN.

<details>

<summary>Reproducibility receipt</summary>


::: {.cell}

```{.r .cell-code}
## datetime
Sys.time()
```

::: {.cell-output .cell-output-stdout}

```
[1] "2026-07-28 22:20:37 PDT"
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
Head:     [b9a6796] 2026-07-29: canivt post
```


:::

```{.r .cell-code}
## Session info 
sessionInfo()
```

::: {.cell-output .cell-output-stdout}

```
R version 4.6.0 (2026-04-24)
Platform: aarch64-apple-darwin23
Running under: macOS Tahoe 26.5.2

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRblas.0.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Vancouver
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
 [1] vctrs_0.7.3               cli_3.6.6                
 [3] knitr_1.51                rlang_1.2.0              
 [5] xfun_0.59                 otel_0.2.0               
 [7] generics_0.1.4            S7_0.2.2                 
 [9] jsonlite_2.0.0            glue_1.8.1               
[11] git2r_0.36.2              htmltools_0.5.9          
[13] mountainmathHelpers_0.1.4 scales_1.4.0             
[15] rmarkdown_2.31            grid_4.6.0               
[17] evaluate_1.0.5            tibble_3.3.1             
[19] fastmap_1.2.0             yaml_2.3.12              
[21] lifecycle_1.0.5           compiler_4.6.0           
[23] dplyr_1.2.1               RColorBrewer_1.1-3       
[25] htmlwidgets_1.6.4         pkgconfig_2.0.3          
[27] rstudioapi_0.19.0         digest_0.6.39            
[29] farver_2.1.2              R6_2.6.1                 
[31] tidyselect_1.2.1          pillar_1.11.1            
[33] magrittr_2.0.5            tools_4.6.0              
[35] gtable_0.3.6              ggplot2_4.0.3            
```


:::
:::


</details>

### References

::: {#refs}
:::
