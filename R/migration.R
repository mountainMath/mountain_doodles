# migration scrpts

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

old_blog_path <- "~/R/mountaindoodles"

old_posts_path <- file.path(old_blog_path, "/content/posts")
new_posts_path <- here::here("posts")

old_posts <- list.files(old_posts_path)

old_posts <- old_posts[!grepl("\\.Rmarkdown$",old_posts)]

old_markdown_posts <- old_posts[grepl("\\.markdown$", old_posts)]

new_posts <- old_posts[dir.exists(file.path(old_posts_path,old_posts))]

html_image_grep_string <- "<img\\s.*?src=(?:'|\")([^'\">]+)(?:'|\").*?\\/?>"
markdown_image_grep_string <- "!\\[[^\\]]*\\]\\(([^\\)]+)\\)"
shortcode_grep_string <- "\\{\\{\\s*<\\s*([^>]+)\\s*>\\s*\\}\\}"

#user_lookup <- c("958850995211051008"="SteveSaretsky")

#tweet_html <- c("958850995211051008"='<blockquote class="twitter-tweet" data-conversation="none"><p lang="en" dir="ltr">Unfortunately roughly 22% of BC’s economy is dependent on Real Estate. <a href="https://t.co/DmuAnpTnHT">pic.twitter.com/DmuAnpTnHT</a></p>&mdash; Steve Saretsky (@SteveSaretsky) <a href="https://twitter.com/SteveSaretsky/status/958850995211051008?ref_src=twsrc%5Etfw">January 31, 2018</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')

regenerate <- TRUE

fixup_categories <- function(pp){
  end_of_yaml <- which(pp=="---")[2]
  categories_index <- which(grepl("^categories:",pp[1:end_of_yaml]))
  not_categories_list <- which(!grepl("^.*- ",pp[1:end_of_yaml]))
  categories_list <- seq(categories_index+1,not_categories_list[not_categories_list>categories_index][1])
  cansim_index <- intersect(categories_list,which(grepl("cansim",pp[1:end_of_yaml],ignore.case=TRUE)))
  if (length(cansim_index)==1) {
    pp[cansim_index] <- gsub("cansim","cansim",pp[cansim_index],ignore.case = TRUE)
  }
  pp
}

fixup_latex <- function(pp){
  slug <- pp[which(grepl("^slug:",pp))][1] |> gsub("^slug: *","",x=_) |> gsub('"','',x=_)
  latex_starts <- which(grepl("\\`\\\\\\(",pp))
  latex_ends <- which(grepl("\\\\\\)\\`",pp))
  if (length(latex_starts)>0) {
    if (length(latex_starts)!=length(latex_ends)) {
      warning("Mismatched latex starts and ends in ",slug)
    } else {
      pp[latex_starts] <- gsub("\\`\\\\\\(","$",pp[latex_starts])
      pp[latex_ends] <- gsub("\\\\\\)\\`","$",pp[latex_ends])
    }
  }
}

fixup_author_info <- function(pp) {
  add_affiliation <- function(pp,position,affiliation) {
    indent_level <- str_extract(pp[position],"^\\s+- ") |> gsub("-"," ",x=_)
    c(pp[1:position], paste0(indent_level,"affiliation: \"",affiliation,"\""),  pp[(position+1):length(pp)])
  }
  
  end_of_yaml <- which(pp=="---")[2]
  authors_index <- which(grepl("^authors:", pp[1:end_of_yaml]))
  if (length(authors_index)==1) {
    pp[authors_index] <- gsub("^authors:","author:",pp[authors_index])
  }
  authors_index <- which(grepl("^author:", pp[1:end_of_yaml]))
  author_block_end <- which(grepl("^[^ ]+",pp[1:end_of_yaml]))
  if (length(author_block_end)==0) {
    return(pp)
  }
  author_block_end <- author_block_end[author_block_end>authors_index] |> first()
  if (length(author_block_end)==0) {
    return(pp)
  }
  if (length(author_block_end)>0 && author_block_end>authors_index+1) {
    author_block_end <- author_block_end - 1
    author_block_start <- authors_index+1
    #pp[author_block_start:author_block_end]
    if (sum(grepl("^\\s+-",pp[author_block_start:author_block_end]))==author_block_end-author_block_start+1) {
      # sanity check to make sure we don't already have extensive metadata
      for (i in author_block_end:author_block_start) {
        if (!grepl("name\\:",pp[i])) {
          indent_level <- str_extract(pp[i],"^\\s+- ")
          pp[i] <- gsub(" - "," - name: ",pp[i])
          if (grepl("Jens von Bergmann",pp[i])) {
            pp <- add_affiliation(pp,i,"MountainMath")
          } else if (grepl("HsingChi von Bergmann",pp[i])) {
            pp <- add_affiliation(pp,i,"UBC, Department of Dentistry")
          } else if (grepl("Lauster",pp[i])) {
            pp <- add_affiliation(pp,i,"UBC, Department of Sociology")
          }
        }
      }
    }
  }
  pp
}

fixup_key_image <- function(pp){
  end_of_yaml <- which(pp=="---")[2]
  slug_index <- which(grepl("^slug:", pp[1:end_of_yaml]))
  images_index <- which(grepl("^images:", pp[1:end_of_yaml]))
  if (length(images_index)==1) {
    images_line <- pp[images_index]
    images_path <- gsub("^images: *\\[|\\] *$","",images_line) |> 
      gsub('"','',x=_)
  } else {
    images_index <- which(grepl("^featured:", pp[1:end_of_yaml]))
    image_path_index <- which(grepl("^featuredpath:", pp[1:end_of_yaml]))
    images_path <- gsub("^featured: *'|'$","",pp[images_index]) |> 
      gsub('"','',x=_)
    images_path_path <- gsub("^featuredpath: *","",pp[image_path_index]) |> 
      gsub('"','',x=_)
    images_path <- file.path(images_path_path,images_path)
  }
  if (length(images_path)==1 && grepl("^https://doodles.mountainmath.ca/",images_path)) {
    images_path <- gsub("^https://doodles.mountainmath.ca","",images_path)
  }
  images_path <- gsub("^/","",images_path)
  if (length(images_path)==1 && grepl("^images/",images_path)) {
    old_image_path <- file.path(old_blog_path,"static",images_path)
    if (file.exists(old_image_path)) {
      if (!dir.exists(file.path(new_post_path,"images"))) {
        dir.create(file.path(new_post_path,"images"))
      }
      file.copy(old_image_path, file.path(new_post_path,"images",gsub("images/","",images_path)))
      pp[[images_index]] <- paste0('image: "',images_path,'"')
    } else {
      stop("Image not found: ", images_path)
    }
  } else if (length(images_path)==1 && grepl("/index_files/",images_path)) {
    images_path <- str_extract(images_path,"figure-html/.*$|figure_html/.*$")
    pp[[images_index]] <- paste0('image: "index_files/',images_path,'"')
  } else if (length(images_path)==1 && grepl("/figure-html/|/figure_html/",images_path)) {
    images_path <- str_extract(images_path,"figure-html/.*$|figure_html/.*$")
    pp[[images_index]] <- paste0('image: "index_files/',images_path,'"')
  } else if (length(images_path)==1 && grepl("https\\://",images_path)){
    pp[[images_index]] <- paste0('image: "index_files/',images_path,'"')
  } else if (grepl("jane-jacobs",pp[slug_index])) {
    pp <- c(pp[1:slug_index], 
            "image: \"images/Downtown.png\"", 
            pp[(slug_index+1):length(pp)])
  } else if (pp[slug_index]=="slug: incomes") {
    pp <- c(pp[1:slug_index], 
            "image: \"images/yvr_income.png\"", 
            pp[(slug_index+1):length(pp)])
  } else if (pp[slug_index]=="slug: character-retention") {
    pp <- c(pp[1:slug_index], 
            "image: \"images/4-plex.png\"", 
            pp[(slug_index+1):length(pp)])
  } else {
    message("no key image found for: ", post)
  }
  pp
}

fixup_metadata <- function(pp){
  pp |>
    fixup_author_info() |>
    fixup_key_image() |>
    fixup_categories()
}

get_new_link <- function(link,bp){
  # print(link)
  if (length(link)==0) {return(link)}
  
  if (grepl("^https://doodles.mountainmath.ca/images/",link)) {
    link <- gsub("^https://doodles.mountainmath.ca","",link)
  }
  ssl <- basename(bp)
  ss <- gsub("\\d{4}-\\d{2}-\\d{2}-","",ssl)
  if (grepl("^images/",link)) {
    # relative link, everything should be fine if image is already there
    if (file.exists(file.path(bp,link))) {
      new_link <- link
   } else {
      stop("Image not found: ",link)
    }
  } else if (grepl("^/images/",link)) {
    # local images file
    old_image_path <- file.path(old_blog_path,"static",link)
    if (!file.exists(old_image_path) && grepl("%20",link)) {
      link <- gsub("%20"," ",link)
      old_image_path <- file.path(old_blog_path,"static",link)
      if (!file.exists(old_image_path)) {
        stop("Image not found: ",link)
      }
    }
    image_base_path <- file.path(bp,"images")
    if (!dir.exists(image_base_path)) {
      dir.create(image_base_path)
    }
    new_image_path <- file.path(image_base_path,basename(link))
    # if (grepl("/","new_image_path")) {
    #   stop("invalid image path: ",new_image_path)
    # }
    if (!file.exists(new_image_path)) {
      file.copy(old_image_path,new_image_path)
    }
    new_link <- file.path("images",basename(link))
  } else if (grepl(paste0(ss,"_files"),link)) {
    # internal link, make sure it fits new structure
    new_link <- paste0("index_files/",gsub(paste0("^.*",ss,"_files/"),"",link)) 
  } else if (grepl("^https\\://doodles.mountainmath.ca/posts/",link)) {
    # link to other post
    new_link <- gsub("_files/","/index_files/",link) |> 
      gsub("^https\\://doodles.mountainmath.ca","",x=_) # make links relative
  } else if (grepl("^https\\://doodles.mountainmath.ca/blog/",link)) {
    # link to other post
    date_string=str_match(link,"/blog/(\\d{4}/\\d{2}/\\d{2})/")[2]
    new_link <- gsub("/blog/\\d{4}/\\d{2}/\\d{2}/",paste0("/posts/",gsub("/","-",date_string),"-"),link) |> 
      gsub("^https\\://doodles.mountainmath.ca","",x=_) # make links relative
  } else if (grepl("^https\\://doodles.mountainmath.ca/",link)) {
    stop("don't know what to do with this: ",link)
  } else if (grepl("^https\\://",link)) {
    # external link, just keep it. 
    new_link <- link
  } else if (grepl("^index_files/figure-html",link)) {
    # already good 
    new_link <- link
  } else {
    stop("don't know what to do with this: ",link)
  }
  new_link <- gsub("index_files/index_files","index_files",new_link)
  new_link
}

for (post in old_markdown_posts) {
  base_path <- gsub("\\.markdown$","",post)
  new_post_path <- file.path(new_posts_path, base_path)
  if (dir.exists(new_post_path)) {
    if (regenerate) {
      message("Post already exists, regenerating: ", new_post_path)
      unlink(new_post_path, recursive=TRUE)
      unlink(here::here("_site","posts",base_path), recursive=TRUE)
    } else {
      message("Post already exists, skipping: ", new_post_path)
      next
    }
  } else {
    message("Processing post: ", post)
  }
  dir.create(new_post_path)
  static_files_path <- dir(file.path(old_blog_path,"static","posts"),full.names = TRUE)
  static_files_path <- static_files_path[grepl(base_path, static_files_path)]
  if (length(static_files_path) > 0) {
    file.copy(static_files_path, new_post_path,recursive=TRUE)
    file.rename(file.path(new_post_path,paste0(base_path,"_files")), file.path(new_post_path,"index_files"))
  }
  new_post <- file.path(new_posts_path, base_path, "index.markdown")
  pp <- read_lines(file.path(old_posts_path, post))
  end_of_yaml <- which(pp=="---")[2]
  slug <- gsub("\\d{4}-\\d{2}-\\d{2}-","",base_path)
  date <- str_extract(base_path,"\\d{4}-\\d{2}-\\d{2}")
  pp <- c(pp[1:(end_of_yaml-1)], "aliases:", paste0("  - /blog/", gsub("-","/",date), "/",slug,"/"), pp[end_of_yaml:length(pp)])
  
  pp <- fixup_metadata(pp)
  
  
  pp <- pp |> str_replace_all("\\{\\{< blogdown/postref >\\}\\}","")
  
  short_codes <- str_match_all(pp,shortcode_grep_string) |>
    lapply(as.data.frame) |> 
    bind_rows() |> 
    setNames(c("full","shortcode")) |>
    as_tibble() |>
    mutate(new_shortcode = case_when(
      #grepl("tweet",shortcode) ~ paste0("tweet user=",user_lookup[str_extract(shortcode,"\\d+\\s*$") |> gsub("\\s","",x=_)]," id=",str_extract(shortcode,"\\d+\\s*$")),
      TRUE ~ shortcode
    ))

  unhandled_short_codes <- short_codes |> filter(!grepl("img-post|tweet|figure|youtube",shortcode))
  if (nrow(unhandled_short_codes)>0) print(unhandled_short_codes$full)
  
  img_post_codes <- short_codes |>
    filter(grepl("img-post",shortcode)) |>
    mutate(path=str_match(shortcode,"path=\"([^\"]+)\" ")[,2],
           file=str_match(shortcode,"file=\"([^\"]+)\" ")[,2],
           alt=str_match(shortcode,"alt=\"([^\"]+)\" ")[,2],
           type=str_match(shortcode,"type=\"([^\"]+)\" ")[,2]) |>
    mutate(image_path=file.path(path,file)) |>
    rowwise() |>
    mutate(new_image_path=get_new_link(image_path,new_post_path)) |>
    ungroup() |>
    mutate(new_shortcode = case_when(
      type=="left" ~ paste0("<img src=\"",new_image_path,"\" alt=\"",alt,"\" style=\"width:45%;margin:0 5% 4px 0;float:left;\" />"),
      type=="right" ~ paste0("<img src=\"",new_image_path,"\" alt=\"",alt,"\" style=\"width:45%;margin:0 0 4px 5%;float:right;\" />"),
      TRUE ~ "xxxx"
    ))

  if (nrow(img_post_codes |> filter(new_shortcode=="xxxx"))>0) {
    stop("unknown image type in shortcodes")
  }
  if (nrow(img_post_codes)>0) for (i in 1:nrow(img_post_codes)) {
    pp <- str_replace_all(pp,img_post_codes$full[i] |>
                            gsub("\\{","\\\\{",x=_) |>
                            gsub("\\}","\\\\}",x=_),
                          img_post_codes$new_shortcode[i])
  }
  

    
  figure_codes <- short_codes |>
    filter(grepl("\\<\\s*figure",shortcode)) |>
    mutate(image_path=str_match(shortcode,"src=\"([^\"]+)\"")[,2],
           link=str_match(shortcode,"link=\"([^\"]+)\"")[,2],
           title=str_match(shortcode,"title=\"([^\"]+)\"")[,2],
           class=str_match(shortcode,"class=\"([^\"]+)\"")[,2])  |>
    #mutate(class=strsplit(class," ") |> lapply(\(x)paste0(".",x) |> paste0(collapse=" ")) |> unlist()) |>
    mutate(new_shortcode = case_when(
      is.na(link) ~ paste0("![](",image_path,")"),
      TRUE ~ paste0("[![](",image_path,")](",link,")"),
    )) |>
    mutate(new_shortcode = case_when(
      is.na(class) ~ paste0("<figure> ",new_shortcode," <figcaption>",title,"</figcaption></figure>"),
      TRUE ~ paste0("<figure class=\"",class,"\"> ",new_shortcode," <figcaption>",title,"</figcaption></figure>"),
      ))
  
  if (nrow(figure_codes)>0) for (i in 1:nrow(figure_codes)) {
    pp <- str_replace_all(pp,figure_codes$full[i] |>
                            gsub("\\{","\\\\{",x=_) |>
                            gsub("\\}","\\\\}",x=_),
                          figure_codes$new_shortcode[i])
  }

  html_image_links <- str_match_all(pp,html_image_grep_string) |> 
    lapply(as.data.frame) |> 
    bind_rows() |> 
    setNames(c("full","link")) |>
    as_tibble()
  
  markdown_image_links <- str_match_all(pp,markdown_image_grep_string) |>
    lapply(as.data.frame) |> 
    bind_rows() |> 
    setNames(c("full","link")) |>
    as_tibble()
  
  
  
  link_table <- bind_rows(html_image_links,markdown_image_links) |> 
    select(link) |>
    distinct()
  if (nrow(link_table)>0) {
    link_table <- link_table |>
      rowwise() |>
      mutate(new_link = get_new_link(link,new_post_path)) |>
      filter(new_link!=link)
  }
  
  #pp <- pp |> str_replace_all("\\{\\{< blogdown/postref >\\}\\}","")
  
  
  if (nrow(link_table)>0) for (i in 1:nrow(link_table)) {
    pp <- pp |> str_replace_all(link_table$link[i],link_table$new_link[i])
  }
  
  if (post=="2017-10-23-trick-or-treat-2017.markdown") {
    pp <- pp |> gsub("display:inline-block;","display:inline-block!important;",x=_)
    inline_style <- which(pp==" <style>")
    pp <- c(pp[1:(inline_style-1)]," ",pp[inline_style:length(pp)])
  }
  
  write_lines(pp, new_post)
  
  # copy Rmarkdown file if available for the record
  rm <- gsub("\\.markdown",".Rmarkdown",post)
  if (file.exists(file.path(old_posts_path, rm))) {
    file.copy(file.path(old_posts_path, rm), file.path(new_post_path, "index.Rmarkdown"))
  }
}

for (post in new_posts) {
  base_path <- gsub("\\.markdown","",post)
  new_post_path <- file.path(new_posts_path, base_path)
  if (dir.exists(new_post_path)) {
    if (regenerate) {
      message("Post already exists, regenerating: ", new_post_path)
      unlink(new_post_path, recursive=TRUE)
      unlink(here::here("_site","posts",base_path), recursive=TRUE)
    } else {
      message("Post already exists, skipping: ", new_post_path)
      next
    }
  } else {
    message("Processing post: ", post)
  }

  old_post_base_path <- file.path(old_posts_path, post)
  
  file.copy(old_post_base_path, new_posts_path,recursive=TRUE)
  #file.copy(file.path(old_post_base_path,"index.markdown"), file.path(new_post_path, "index.markdown"))
  
  if (file.exists(file.path(new_post_path, "index"))) {
    unlink(file.path(new_post_path, "index"))
  }
  if (dir.exists(file.path(new_post_path, "index_cache"))) {
    unlink(file.path(new_post_path, "index_cache"), recursive=TRUE)
  }
  
  new_post <- file.path(new_posts_path, base_path, "index.markdown")
  pp <- read_lines(new_post)
  end_of_yaml <- which(pp=="---")[2]
  slug <- gsub("\\d{4}-\\d{2}-\\d{2}-","",base_path)
  date <- str_extract(base_path,"\\d{4}-\\d{2}-\\d{2}")
  pp <- c(pp[1:(end_of_yaml-1)], "aliases:", paste0("  - /blog/", gsub("-","/",date), "/",slug,"/"), pp[end_of_yaml:length(pp)])
  
  pp <- fixup_metadata(pp)

  
 
  
  pp <- pp |> str_replace_all("\\{\\{< blogdown/postref >\\}\\}","")
  
  short_codes <- str_match_all(pp,shortcode_grep_string) |>
    lapply(as.data.frame) |> 
    bind_rows() |> 
    setNames(c("full","shortcode")) |>
    as_tibble()
  
  unhandled_short_codes <- short_codes |> filter(!grepl("img-post|tweet|figure|youtube",shortcode))
  if (nrow(unhandled_short_codes)>0) print(unhandled_short_codes$full)
  
  
  html_image_links <- str_match_all(pp,html_image_grep_string) |> 
    lapply(as.data.frame) |> 
    bind_rows() |> 
    setNames(c("full","link")) |>
    as_tibble()
  
  markdown_image_links <- str_match_all(pp,markdown_image_grep_string) |>
    lapply(as.data.frame) |> 
    bind_rows() |> 
    setNames(c("full","link")) |>
    as_tibble()
  
  

  link_table <- bind_rows(html_image_links,markdown_image_links) |> 
    select(link) |>
    distinct() |>
    rowwise() |>
    mutate(new_link = get_new_link(link,new_post_path)) |>
    filter(new_link!=link)

  #pp <- pp |> str_replace_all("\\{\\{< blogdown/postref >\\}\\}","")
  
    
  if (nrow(link_table)>0) for (i in 1:nrow(link_table)) {
    pp <- pp |> str_replace_all(link_table$link[i],link_table$new_link[i])
  }
  
  # safety
  
  
  write_lines(pp, new_post)
}


