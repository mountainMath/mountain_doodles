new_post <- function(title,
                     author=c("Jens von Bergmann"),
                     affiliation=c("MountainMath"),
                     date=Sys.Date(),
                     categories=c()) {
  slug=title |>
    tolower() |> 
    gsub(" ","-",x=_) |> 
    gsub("[^a-z0-9-]","",x=_)
  path <- here::here("posts",paste0(date,"-",slug))
  
  text <- dplyr::tibble(values="---")
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("title: \"",title,"\"")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("author:")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("  - name: ",author)))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("    affiliation: ",affiliation)))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("date: '",date,"'")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("slug: ",slug)))
  if (length(categories)>0) {
    text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("categories:")))
    text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("  - ",categories)))
  }
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("description: ''")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("image: ''")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("bibliography: ../../common_literature.bib ")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("code-tools:")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("  toggle: true")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("fig-width: 8")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("execute:")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("  cache: true")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("  message: false")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values=paste0("  warning: false")))
  text <- dplyr::bind_rows(text,dplyr::tibble(values="---"))
  
  
  dir.create(path)
  readr::write_lines(text$values,file.path(path,"index.qmd"))
  usethis::edit_file(file.path(path,"index.qmd"))
}


  