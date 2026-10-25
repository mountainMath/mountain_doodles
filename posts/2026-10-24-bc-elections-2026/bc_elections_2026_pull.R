# Pulls BC 2026 election results and uploads them to S3 for the live graphs in bc_elections_2026.qmd
#   source("bc_elections_2026_pull.R"); pull_results()         # once
#   source("bc_elections_2026_pull.R"); pull_loop(minutes = 5) # until interrupted
#
# Uploads (public, gzipped) to s3://mountainmath/bc_2026_elections/enr_results.csv the candidate votes
# from the Elections BC unofficial results (header only until posted). The CBC results are read live by the page.

library(tidyverse)
library(mountainmathHelpers)

enr_container <- "https://electionsbcenr.blob.core.windows.net/electionsbcenr"
s3_bucket <- "mountainmath"
s3_dir <- "bc_2026_elections/"

# match Elections BC affiliation names to CBC party codes
recode_affiliation <- function(affiliation) {
  case_when(affiliation %in% c("","Independent","Unaffiliated") ~ "IND",
            str_detect(affiliation,"NDP") ~ "NDP",
            str_detect(affiliation,"Conservative") ~ "CON",
            str_detect(affiliation,"Green") ~ "GRN",
            str_detect(affiliation,"One ?BC") ~ "ONE",
            str_detect(affiliation,"Centre") ~ "CTR",
            str_detect(affiliation,"Libertarian") ~ "LTN",
            str_detect(affiliation,"Christian Heritage") ~ "CHP",
            str_detect(affiliation,"Communist") ~ "COM",
            str_detect(affiliation,"Freedom") ~ "FP",
            affiliation=="CWP" | str_detect(affiliation,"CanWest") ~ "CWP",
            TRUE ~ affiliation)
}

upload_csv <- function(data, name) {
  path <- file.path(tempdir(), name)
  write_csv(data, path, na = "")
  file_to_s3_gzip(path, s3_bucket, paste0(s3_dir, name))
  unlink(path)
}

# Elections BC ----

get_enr_file <- function(election_date="2026-10-24") {
  listing <- xml2::read_xml(paste0(enr_container,"?restype=container&comp=list&prefix=Results_"))
  blobs <- xml2::xml_find_all(listing,".//Blob")
  files <- tibble(name=xml2::xml_find_first(blobs,"./Name") |> xml2::xml_text(),
                  modified=xml2::xml_find_first(blobs,".//Last-Modified") |> xml2::xml_text()) |>
    filter(str_detect(name,paste0("^Results_\\d+_GE-",election_date,"_Candidate\\.html$")))
  if (nrow(files)==0) return(NULL)
  files |>
    arrange(name) |>
    slice_tail(n=1) |>
    mutate(url=paste0(enr_container,"/",name),
           modified=as.POSIXct(modified,format="%a, %d %b %Y %H:%M:%S GMT",tz="UTC"))
}

get_enr_results <- function(file=get_enr_file()) {
  empty <- tibble(ED_NAME=character(),Candidate=character(),Party=character(),Votes=numeric(),updated=character())
  if (is.null(file)) {
    message("No Elections BC results file posted yet")
    return(empty)
  }
  page <- rvest::read_html(file$url)
  # the page header carries the time of the count, "As of October 24, 2026 20:01 PST" (labelled PST even during
  # daylight time), fall back to the blob upload time, the blob gets rewritten every few minutes regardless
  as_of <- rvest::html_text(page) |>
    str_match("As of ([A-Za-z]+ \\d{1,2}, \\d{4} \\d{1,2}:\\d{2})") |>
    _[,2] |>
    as.POSIXct(format="%B %d, %Y %H:%M",tz="America/Vancouver")
  if (is.na(as_of)) as_of <- file$modified
  # the vote column has been called both "Total Valid Votes" and "Total Votes"
  page |>
    rvest::html_nodes("table#tblReport") |>
    rvest::html_table() |>
    first() |>
    select(ED_NAME=`Electoral District`,Candidate=`Candidate's Ballot Name`,Affiliation,Votes=matches("^Total.*Votes$")) |>
    # ballot box status rows span all columns, so the district and candidate cells hold the same text
    filter(ED_NAME!=Candidate) |>
    mutate(ED_NAME = na_if(ED_NAME,"")) |>
    fill(ED_NAME,.direction = "down") |>
    # per-district totals sit in the candidate column
    filter(Candidate!="", !grepl("^Total|^Rejected Ballots",Candidate), Affiliation!="Total") |>
    # some names come through with their HTML entities still encoded, like C&#244;t&#233;
    mutate(across(c(ED_NAME,Candidate),\(x) str_replace_all(x,"&#(\\d+);",\(m) intToUtf8(as.integer(str_extract(m,"\\d+")),multiple=TRUE)))) |>
    mutate(Votes=gsub(",","",Votes) |> as.numeric(),
           Party=recode_affiliation(Affiliation),
           updated=format(as_of,"%Y-%m-%dT%H:%M:%SZ",tz="UTC")) |>
    select(names(empty))
}

# Pull ----

pull_results <- function() {
  enr <- get_enr_results()
  if (nrow(enr)>0) {
    ridings <- n_distinct(enr$ED_NAME)
    if (ridings!=93) warning("Elections BC results have ", ridings, " ridings")
    message("Uploaded Elections BC results, ", format(sum(enr$Votes),big.mark=","), " votes")
  }
  upload_csv(enr, "enr_results.csv")
  invisible(enr)
}

pull_loop <- function(minutes=5) {
  repeat {
    message(format(Sys.time(),"%H:%M:%S"), " pulling")
    tryCatch(pull_results(), error=\(e) warning("Elections BC pull failed: ", conditionMessage(e)))
    Sys.sleep(minutes*60)
  }
}
