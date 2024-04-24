library(stringr)
library(readr)

noisy <- FALSE

paths <- dir(here::here("_site/posts"),full.names=TRUE,pattern="^\\d{4}-\\d{2}-\\d{2}-")

for (path in paths) {
  index_path <- file.path(path,"index.html")
  if (!file.exists(index_path)) {
    if (noisy) warning("Skipping ",path, " no index.html")
    next
  }
  
  pp <- read_lines(index_path)
  
  base <- basename(path)
  
  citation_start <- which(grepl("id=\"quarto-citation\">",pp))
  
  citation_key_line <- which(grepl("<code class=\"sourceCode bibtex\">\\@misc\\{[^,]+,",pp))
  
  if (length(citation_key_line)==1) {
    citation_key <- str_match(pp[citation_key_line],"\\@misc\\{([^,]+),")[,2]
    if (!is.na(citation_key)) {
      new_key <- paste0(gsub("\\d{4}-\\d{2}-\\d{2}-","",base),".",substr(base,1,4))
      pp[citation_key_line] <- gsub(citation_key,new_key,pp[citation_key_line])
    } else {
      if (noisy) warning("No bibtext citation key found for ",base)
    }
  } else {
    if (noisy) warning("No bibtext citation key found for ",base)
  }
  
  
  citation_line <- which(pp==paste0("  url = {https://doodles.mountainmath.ca/posts/",base,"},"))
  
  if (length(citation_line)==1) {
    pp[citation_line] <- paste0("  url = {https://doodles.mountainmath.ca/posts/",base,"/}")
  } else {
    if (noisy) warning("No bibtext citation found for ",base)
  }

  author_line <- which(grepl("^  author = \\{.+\\},$",pp)&grepl("von Bergmann, Jens",pp))
  
  if (length(author_line)==1) {
    pp[author_line] <- gsub("von Bergmann, Jens","{von Bergmann}, Jens",pp[author_line])
  } else {
    if (noisy) warning("No bibtext author found for ",base)
  }
  
  old_string <- paste0("<a href=\"https://doodles.mountainmath.ca/posts/",base,
                       "\">https://doodles.mountainmath.ca/posts/",base,"</a>")
  citation_link_line <- which(grepl(old_string,pp))
  if (length(citation_link_line)==1) {
    new_string <-   old_string <- paste0("<a href=\"https://doodles.mountainmath.ca/posts/",base,
                                         "/\">https://doodles.mountainmath.ca/posts/",base,"/</a>")
    
    pp[citation_link_line] <- sub(pp[citation_link_line],old_string,new_string)
  } else {
    if (noisy) warning("No citation link found for ",base)
  }
  
  author_link_line <- which(grepl("Bergmann, Jens von",pp))
  if (length(author_link_line)==1) {
    pp[author_link_line] <- gsub("Bergmann, Jens von","von Bergmann, Jens",pp[author_link_line])
  } else {
    if (noisy) warning("No author link found for ",base)
  }
  
  write_lines(pp,index_path)
}