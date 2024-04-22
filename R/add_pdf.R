# add metadata to generate PDF for old posts

pdf_posts <- c(
  "what-if-recent-apartment-buildings-in-vancouver-were-20-taller"="We estimate that planning decisions preventing apartment buildings built in the past 5 years in Metro Vancouver from being on average 20% taller are resulting in an annual redistribution of income from renters to existing landlords on the order of half a billion dollars across the region via higher rents.",
  "first-time-buyer-lorenz-curves-revisited"="This article expands on our [previous work](https://doodles.mountainmath.ca/posts/2020-09-28-first-time-buyer-lorenz-curves/index.html) on Lorenz curves and derived GINI index by adding in 2021 data, and adding the discretization into two metrics, the aggregate unaffordability that approximates the GINI index, and the high-income skew that captures the degree to which lower income earners do worse than higher income earners. We also included property taxes, as rates differ across metro areas and across time, and this makes a significant difference. Property tax rates were taken from the central municipalities, this could use refinement in future iterations.

This could be expanded on by using higher frequency data like home transactions and the Canadian Income Survey to allow closer monitoring of first time buyer affordability.",
  "housing-outcomes"="Most commonly used metrics use existing households as the base of analysis, but households are a consequence of housing pressures. This kind of misspecification is a form of collider or selection bias that, especially in tight housing markets, misleads researchers toward faulty conclusions and policy recommendations. It blinds researchers to the struggles of people who are unhappy about their current household living arrangement, like young adults struggling to move out of their parent’s place or out of a bad roommate setup, as well as people who have left their desired region and moved away, or failed to move in, because of the lack of housing options.

This post explains the problem with analysis based solely on households in more detail, and explains why this will lead to incorrect diagnoses of our housing problems and misguided policy recommendations.",
  "housing-targets"="This post lays out a framework for setting housing targets at the municipal level based on the assumption that we believe housing inherently provides value, and estimates targets for select Metro Vancouver municipalities to illustrate how this works. It emphasizes that any housing target methodology is based on underlying values that should be made explicit.",
  "metro-vancouver-planning-regimes"="This post traces the role regional planning plays when it comes to coordinating planning for housing and population growth, especially in a heavily balkanized environment like Metro Vancouver (containing 21 municipalities, an electoral area, a First Nation Treaty Land, and, while not explicitly represented in Metro Vancouver planning processes, 16 First Nation reserves). We draw attention to a regime change in planning. Starting with plans from the 1970s, the GVRD moved away from coordinating for growth and toward gate management. This laid a strong foundation for housing scarcity, and Metro Vancouver has to this day continued to work toward perpetuating and deepening housing scarcity in the region through its projection and coordination work. While it is unclear how important the Metro Vancouver Regional Growth Strategies have been in coordinating municipal limits on housing development, they have at a minimum given cover to municipalities for perpetuating and deepening housing scarcity and spatial misallocation, shifting growth away from where people want to live.",
  "investing-in-definitions-and-framing"="With last week’s CHSP release of data on the investment status of residential properties and the framing of the accompanying article there has been a lot of rather uninformed and misleading news coverage.

The misleading reporting, combined with sometimes plainly wrong statements by people quoted in the news coverage, on one hand highlights the poor understanding of housing in the public discourse. On the other hand it highlights the importance of providing careful framing with data releases. In general it is good practice to accompany a data release with a brief analysis to provide framing and context. Analysts close to the raw data will have a much better understanding of what the data is measuring and can properly frame the data. Unfortunately, StatCan fell short of doing so, and the overview analysis provided by StatCan itself contains a number of problems. Given the public attention this has gotten it’s probably worthwhile to take a look at what the data does and does not say, and to correct some of the misinterpretations that have been circulating.",
  "a-brief-history-of-vancouver-planning-development-regimes"="This post focuses on the history of planning for housing in the City of Vancouver and divides time up into three rough regimes, which we call the Permissive Regime, the Zoning for Growth Regime, and the Spot Discretionary Regime. We explain the basic features of each of these planning regimes, and how they have affected how housing has been built -- or has been prevented from getting built in the city."
)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

all_posts <- dir(here::here("posts"))

for (post in names(pdf_posts)) {
  path <- all_posts[grepl(post, all_posts)]
  if (length(path)!=1) {
    warning("Skipping ",post, " not found or too many matches ",path)
    next
  } 
  post_path <- file.path("posts",path,"index.markdown")
  pp<-read_lines(post_path)
  end_of_yaml <- which(pp=="---")[2]
  
  format_index <- which(grepl("^format\\:",pp[1:end_of_yaml]))
  blog_pfd_index <- which(grepl("^\\s*blog-pdf\\:",pp[1:end_of_yaml]))
  
  if (length(blog_pfd_index)>0) {
    message("Skipping ",post, " already has pdf metadata")
    next
  } 
  
  if (length(format_index)>0) {
    warning("Alreadu have format data for ",post, " but did not find PDF, skipping")
    next
  }
  
  
  slug <- pp[which(grepl("^slug\\: ",pp[1:(end_of_yaml-1)]))] |>
    gsub("^slug\\:\\s+","",x=_) 
  
  pdf_abstract <- pp[which(grepl("^pdf_abstract\\: ",pp[1:(end_of_yaml-1)]))] |>
    gsub("^pdf_abstract\\:\\s+","",x=_)
  
  if (length(pdf_abstract)==0) {
    text <- pdf_posts[[post]] |> 
      strsplit("\n")  |> 
      unlist()  %>%
      paste0("  ",.) 
    pdf_abstract <- c("pdf_abstract: |", text)
  } else {
    pdf_abstract <- c()
  }
  
  pp <- c(pp[1:(end_of_yaml-1)],
          pdf_abstract,
          "format:",
          "  html: default",
          "  blog-pdf:",
          paste0("    output-file: '",slug,"'"),
          pp[end_of_yaml:length(pp)])

  
  write_lines(pp,post_path)
  
  
}