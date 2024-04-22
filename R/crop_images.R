# crop images to remove extraneous white space

library(readr)
library(dplyr)
library(tidyr)
library(stringr)


crop_list <- list(
  "what-if-recent-apartment-buildings-in-vancouver-were-20-taller"=c("supply-demand-1.png"),
  "first-time-buyer-lorenz-curves-revisited"=c("calgary-ftb-1.png",
                                              "ftb-updated-1.png",
                                              "prop-tax-effect-1.png",
                                              "ftb-pt-1.png",
                                              #"ftb-synthetic-1.png",
                                              "ftb-approx-1.png"),
  "a-brief-history-of-vancouver-planning-development-regimes"=c("point_grey_hood-1.png"),
  "nanaimo-station"=c("nanaimo_zoning-1.png",
                      "unnamed-chunk-2-1.png",
                      "unnamed-chunk-4-1.png",
                      "unnamed-chunk-5-1.png",
                      "unnamed-chunk-7-1.png",
                      "unnamed-chunk-9-1.png",
                      "unnamed-chunk-10-1.png",
                      "unnamed-chunk-13-1.png",
                      "unnamed-chunk-14-1.png"),
  "25-years-of-structural-change"=c("yvr_structural_dots-1.png"),
  "ins-and-outs-of-cmhc-data"=c("data-quality-geo-mismatch-1.png"),
  "on-broadway"=c("combind_census_data-1.png",
                  "broadway_zoning_history-1.png"),
  "children-are-good-actually"=c("unnamed-chunk-1-1.png",
                                 "children_change-1.png",
                                 "unnamed-chunk-2-1.png",
                                 "unnamed-chunk-3-1.png",
                                 "unnamed-chunk-8-1.png",
                                 "unnamed-chunk-10-1.png",
                                 "unnamed-chunk-12-1.png",
                                 "unnamed-chunk-15-1.png")
)

all_posts <- dir(here::here("posts"))

for (post in names(crop_list)) {
  path <- all_posts[grepl(post, all_posts)]
  if (length(path)!=1) {
    warning("Skipping ",post, " not found or too many matches ",path)
    next
  } 
  images <- crop_list[[post]]
  for (image in images) {
    image_base <- here::here("posts",path,"index_files","figure-html")
    image_path <- dir(image_base,pattern=image,full.names=TRUE)
    if (length(image_path)!=1) {
      warning("Skipping image ",image," in ",post, " not found or too many matches ",image_path)
      next
    }
    knitr::plot_crop(image_path)
  }
}


image_path <- here::here("posts",
                         "2024-04-11-what-if-recent-apartment-buildings-in-vancouver-were-20-taller",
                         "index_files","figure-html","supply-demand-1.png")




knitr::plot_crop(image_path)
