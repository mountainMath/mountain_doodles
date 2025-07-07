# old census data for AEHH and HH
# a lot of the data comes from here: https://mdl.library.utoronto.ca/collections/numeric-data/census-canada/1941


hmr_1981_canada <- simpleCache(get_phm_timeline(),"hmr_data_all.rds",path=here::here("data"),refresh=FALSE) |>
  summarize(Count=sum(Count),.by=c(AGEGRP,PRIHM,Year)) |>
  mutate(hmr=Count/sum(Count),.by=c(AGEGRP,Year)) |>
  filter(Year=="1981",PRIHM)  |>
  select(AGEGRP,base_hmr=hmr)

hmr_1981_canada_15_19 <-  hmr_1981_canada |>
  mutate(w=case_when(AGEGRP=="15 to 17 years"~3,
                     AGEGRP=="18 to 19 years"~2,
                     TRUE ~ 1)) |>
  mutate(AGEGRP=recode(AGEGRP,"18 to 19 years"="15 to 19 years",
                       "15 to 17 years"="15 to 19 years")) |>
  summarise(base_hmr=weighted.mean(base_hmr,w),.by=AGEGRP)

get_1971_hh <- function(refresh=FALSE) {
  simpleCache({
    tmp <- tempfile()
    download.file("https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1971/statistics/ust/b1hhdb01_spss.zip",tmp,mode="wb")
    exdir=file.path(tempdir(),"hh1971")
    unzip(tmp,exdir=exdir)
    unlink(tmp)
    # obtained from https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/ALQYLB
    d<- foreign::read.spss(file.path(exdir,"b1hhdb01.sav")) |>
      as_tibble() |>
      summarize(Households=sum(V1),.by=c(PROV,CMACA)) |>
      rename(CMA=CMACA)
    d}, "hh1971.Rda",refresh=refresh)
}

get_1971_age <- function(refresh=FALSE){ 
  simpleCache({
    tmp <- tempfile()
    download.file("https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1971/statistics/ust/b1demb01_spss.zip",tmp,mode="wb")
    exdir <- file.path(tempdir(),"B1DEMB01")
    unzip(tmp,exdir=exdir)
    unlink(tmp)
    
    d<-foreign::read.spss(file.path(exdir,"b1demb01.sav")) |> #"~/Downloads/B1DEMB01/B1DEMB01.sav") |>
      as_tibble() |>
      summarize(across(matches("V\\d+"),sum),.by=c(PROV,CMACA)) |>
      pivot_longer(matches("V\\d+")) |>
      mutate(age=as.integer(str_extract(name,"\\d+"))) |>
      mutate(Age=case_when(age<4 ~ -1,
                           age<105 ~ age-4,
                           TRUE ~ age-105 ),
             Sex=case_when(age<105 ~ "Male",
                           TRUE ~ "Female")) |>
      filter(Age>=0,Age<=100) |>
      mutate(AGEGRP=case_when(Age<5 ~ "0 to 4 years",
                              Age<10 ~ "5 to 9 years",
                              Age<15 ~ "10 to 14 years",
                              Age<20 ~ "15 to 19 years",
                              Age<25 ~ "20 to 24 years",
                              Age<30 ~ "25 to 29 years",
                              Age<35 ~ "30 to 34 years",
                              Age<40 ~ "35 to 39 years",
                              Age<45 ~ "40 to 44 years",
                              Age<50 ~ "45 to 49 years",
                              Age<55 ~ "50 to 54 years",
                              Age<60 ~ "55 to 59 years",
                              Age<65 ~ "60 to 64 years",
                              Age<70 ~ "65 to 69 years",
                              Age<75 ~ "70 to 74 years",
                              Age<80 ~ "75 to 79 years",
                              Age<85 ~ "80 to 84 years",
                              TRUE ~ "85 years and over")) |>
      summarize(value=sum(value),.by=c(PROV,CMACA,AGEGRP)) |>
      rename(CMA=CMACA)
    
    d}, "age1971.Rda",refresh=refresh)
}

get_1976_age <- function(refresh=FALSE){ 
  simpleCache({
    tmp <- tempfile()
    download.file("http://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1976/statistics/bst/eadema10_spss.zip",tmp,mode="wb")
    exdir <- file.path(tempdir(),"eadema10")
    unzip(tmp,exdir=exdir)
    unlink(tmp)
    
    d<-foreign::read.spss(file.path(exdir,"cnd_eadema10.sav")) |> #"~/Downloads/B1DEMB01/B1DEMB01.sav") |>
      as_tibble() |>
      filter(RECTYPE=="ENUM AREA") |>
      summarize(across(matches("V\\d+"),sum),.by=c(PROV,CMACA)) |>
      pivot_longer(matches("V\\d+")) |>
      filter(name %in% paste0("V",seq(4,67,3))) |>
      mutate(AGEGRP=recode(name,"V4"="0 to 4 years",
                           "V7"="0 to 4 years",
                           "V10"="5 to 9 years",
                           "V13"="10 to 14 years",
                           "V16"="15 to 19 years",
                           "V19"="20 to 24 years",
                           "V22"="25 to 29 years",
                           "V25"="30 to 34 years",
                           "V28"="35 to 39 years",
                           "V31"="40 to 44 years",
                           "V34"="45 to 49 years",
                           "V37"="50 to 54 years",
                           "V40"="55 to 59 years",
                           "V43"="60 to 64 years",
                           "V46"="65 to 69 years",
                           "V49"="70 to 74 years",
                           "V52"="75 to 79 years",
                           "V55"="80 to 84 years",
                           "V58"="85 years and over",
                           "V61"="85 years and over",
                           "V64"="85 years and over",
                           "V67"="85 years and over"
      )) |>
      summarize(value=sum(value),.by=c(PROV,CMACA,AGEGRP)) |>
      rename(CMA=CMACA)
    
    d}, "age1976.Rda",refresh=refresh)
}


get_1976_hh <- function(refresh=FALSE) {
  simpleCache({
    tmp <- tempfile()
    download.file("http://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1976/statistics/bst/eadhha10_spss.zip",tmp,mode="wb")
    exdir=file.path(tempdir(),"hh1976")
    unzip(tmp,exdir=exdir)
    unlink(tmp)
    # obtained from https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/ALQYLB
    d<- foreign::read.spss(file.path(exdir,"eadhha10.sav")) |>
      as_tibble() |>
      filter(RECTYPE=="ENUM AREA") |>
      summarize(Households=sum(V1),.by=c(PROV,CMACA)) |>
      rename(CMA=CMACA)
    d}, "hh1976.Rda",refresh=refresh)
}

get_1961_age <- function(refresh=FALSE){ 
  simpleCache({
    tmp=tempfile()
    download.file("https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1961/population_file_male_spss.zip",tmp,mode="wb")
    exdirm=file.path(tempdir(),"pop1961m")
    unzip(tmp,exdir=exdirm)
    unlink(tmp)
    download.file("https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1961/population_file_female_spss.zip",tmp,mode="wb")
    exdirf=file.path(tempdir(),"pop1961f")
    unzip(tmp,exdir=exdirf)
    unlink(tmp)
    d <- bind_rows(
      foreign::read.spss(file.path(exdirm,"pop61-m.sav")),
      foreign::read.spss(file.path(exdirf,"pop61-f.sav"))
    ) |>
      pivot_longer(matches("^AGE\\d+"),names_to="Age") |>
      mutate(AGEGRP=recode(Age,
                           "AGE1"="0 to 4 years",
                           "AGE2"="5 to 9 years",
                           "AGE3"="10 to 14 years",
                           "AGE4"="15 to 19 years",
                           "AGE5"="20 to 24 years",
                           "AGE6"="25 to 29 years",
                           "AGE7"="30 to 34 years",
                           "AGE8"="35 to 39 years",
                           "AGE9"="40 to 44 years",
                           "AGE10"="45 to 49 years",
                           "AGE11"="50 to 54 years",
                           "AGE12"="55 to 59 years",
                           "AGE13"="60 to 64 years",
                           "AGE14"="65 to 69 years",
                           "AGE15"="70 to 74 years",
                           "AGE16"="75 to 79 years",
                           "AGE17"="80 to 84 years",
                           "AGE18"="85 years and over",
                           "AGE19"="85 years and over",
                           "AGE20"="85 years and over"
      )) |>
      summarize(value=sum(value),.by=c(PROV,MAREA,AGEGRP)) |>
      rename(CMA=MAREA)
    d}, "age1961.Rda",refresh=refresh)
}

get_1966_age_weird <- function(refresh=FALSE){
  simpleCache({
    tmp=tempfile()
    download.file("https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1966/population_z50_spss.zip",tmp,mode="wb")
    exdir=file.path(tempdir(),"pop1966")
    unzip(tmp,exdir=exdir)
    unlink(tmp)
    d <- foreign::read.spss(file.path(exdir,"pop66a.sav")) |>
      as_tibble() |>
      pivot_longer(matches("^AGE\\d+_\\d+|^AGE95$"),names_to="Age") |>
      mutate(AGEGRP=recode(Age,
                           "AGE0_4"="0 to 4 years",
                           "AGE5_9"="5 to 9 years",
                           "AGE10_14"="10 to 14 years",
                           "AGE15_19"="15 to 19 years",
                           "AGE20_24"="20 to 24 years",
                           "AGE25_29"="25 to 29 years",
                           "AGE30_34"="30 to 34 years",
                           "AGE35_39"="35 to 39 years",
                           "AGE40_44"="40 to 44 years",
                           "AGE45_49"="45 to 49 years",
                           "AGE50_54"="50 to 54 years",
                           "AGE55_59"="55 to 59 years",
                           "AGE60_64"="60 to 64 years",
                           "AGE65_69"="65 to 69 years",
                           "AGE70_74"="70 to 74 years",
                           "AGE75_79"="75 to 79 years",
                           "AGE80_84"="80 to 84 years",
                           "AGE85_89"="85 years and over",
                           "AGE90_94"="85 years and over",
                           "AGE95"="85 years and over"
      )) |>
      summarize(value=sum(value),.by=c(PROV,METUA,AGEGRP)) |>
      rename(CMA=METUA)
    d}, "age1966.Rda",refresh=refresh)
}

get_1966_age_ottawa <- function(){
  tribble(~CMA,~`15 to 19 years`,~`20 to 24 years`,
          ~`25 to 29 years`,~`30 to 34 years`,~`35 to 39 years`,~`40 to 44 years`,~`45 to 49 years`,
          ~`50 to 54 years`,~`55 to 59 years`,~`60 to 64 years`,~`65 to 69 years`,~`70 to 74 years`,
          ~`75 to 79 years`,~`80 to 84 years`,~`85 years and over`,
          "Ottawa – Gatineau",45646,38175,32808,32020,32753,33880,28994,24649,18434,13995,11476,
          9049,6145,3422,1554+470+106) |>
    pivot_longer(-CMA,names_to = "AGEGRP")
  
}

get_1966_age <- function(refresh=FALSE){
  simpleCache({
    tmp=tempfile()
    download.file("https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1966/population_z10_spss.zip",tmp,mode="wb")
    exdir=file.path(tempdir(),"pop1966")
    unzip(tmp,exdir=exdir)
    unlink(tmp)
    d <- foreign::read.spss(file.path(exdir,"pop66s.sav")) |>
      as_tibble() |>
      pivot_longer(matches("AGE\\d+"),names_pattern="^(.+AGE)(\\d+)$",names_to=c("Type","Age")) |>
      mutate(Age=as.integer(Age)) |>
      mutate(AGEGRP=case_when(Age<5 ~ "0 to 4 years",
                              Age<10 ~ "5 to 9 years",
                              Age<15 ~ "10 to 14 years",
                              Age<20 ~ "15 to 19 years",
                              Age<25 ~ "20 to 24 years",
                              Age<30 ~ "25 to 29 years",
                              Age<35 ~ "30 to 34 years",
                              Age<40 ~ "35 to 39 years",
                              Age<45 ~ "40 to 44 years",
                              Age<50 ~ "45 to 49 years",
                              Age<55 ~ "50 to 54 years",
                              Age<60 ~ "55 to 59 years",
                              Age<65 ~ "60 to 64 years",
                              Age<70 ~ "65 to 69 years",
                              Age<75 ~ "70 to 74 years",
                              Age<80 ~ "75 to 79 years",
                              Age<85 ~ "80 to 84 years",
                              TRUE ~ "85 years and over")) |>
      summarize(value=sum(value),.by=c(PROV,METUA,AGEGRP)) |>
      rename(CMA=METUA)
    d}, "age1966.Rda",refresh=refresh)
}


get_1961_hh <- function(refresh=FALSE){
  simpleCache({
    tmp <- tempfile()
    download.file("https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1961/households_spss.zip",tmp,mode="wb")
    exdir=file.path(tempdir(),"hh1961")
    unzip(tmp,exdir=exdir)
    unlink(tmp)
    d<-foreign::read.spss(file.path(exdir,"hh61.sav")) |>
      as_tibble() |>
      summarize(Households=sum(THH),.by=c(PROV,MAREA)) |>
      rename(CMA=MAREA)
    d}, "hh1961.Rda",refresh=refresh)
}

get_1966_hh_st <- function(refresh=FALSE){
  simpleCache({
    tmp <- tempfile()
    download.file("https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1966/family_household_spss.zip",tmp,mode="wb")
    exdir=file.path(tempdir(),"hh1966")
    unzip(tmp,exdir=exdir)
    unlink(tmp)
    d<-foreign::read.spss(file.path(exdir,"famh66.sav")) |>
      as_tibble() |>
      summarize(Households=sum(NHHOWN+NHHTNT),
                #HH = sum(HH0F+HH1F+HH2F+HH3F+HH4F),
                #HH2 = sum(HH0FP+HH2FP+HH3FP+HH4FP+HH5FP+HH6FP),
                .by=c(PROV,METUA)) |>
      rename(CMA=METUA)
    d}, "hh1966.Rda",refresh=refresh)
}

get_1966_hh <- function(){
  # https://publications.gc.ca/collections/collection_2017/statcan/CS93-602-1966.pdf
  # Table 6, 4
  tribble(~CMA,~Households,
          "Calgary",94941,
          "Edmonton",110224,
          "Halifax",47692,
          "Hamilton",123352,
          "Montréal",668901,
          "Ottawa – Gatineau",130256,
          "Québec",97221,
          "Toronto",586581,
          "Vancouver",271956,
          "Winnipeg",143710,
          "Canada",5180473)
}

get_1951_hh <- function() {
  # https://archive.org/details/1951981951FV31953engfra/page/n87/mode/2up
  # table 11
  tribble(~CMA,~Households,
          "Calgary",40235,
          "Edmonton",46395,
          "Halifax",29640,
          "Hamilton",68640,
          "Montréal",334705,
          "Ottawa – Gatineau",66265,
          "Québec",54930,
          "Toronto",273200,
          "Vancouver",153975,
          "Winnipeg",95955,
          "Canada",3409295)
}

get_1941_hh <- function() {
  # https://archive.org/details/1941981941FV91949engfra/page/n7/mode/2up
  # table 32
  tribble(~CMA,~Households,
          "Calgary",21753,
          "Edmonton",23082,
          "Halifax",17910,
          "Hamilton",41779,
          "Montréal",249560,
          "Ottawa – Gatineau",44588,
          "Québec",34405,
          "Toronto",207665,
          "Vancouver",92782,
          "Winnipeg",65353,
          "Canada",2575644)
}

canada_age_1951 <- tribble(~CMA,~`15 to 19 years`,~`20 to 24 years`,
                           ~`25 to 29 years`,~`30 to 34 years`,~`35 to 39 years`,~`40 to 44 years`,~`45 to 49 years`,
                           ~`50 to 54 years`,~`55 to 59 years`,~`60 to 64 years`,~`65 to 69 years`,~`70 to 74 years`,
                           ~`75 to 79 years`,~`80 to 84 years`,~`85 years and over`,
                           "Canada",1057972,1088641,1131215,1042734,999133,868567,744679,662656,570690,506152,433497,
                           315072,188391,96791,39599+10742+2181) |>
  pivot_longer(-CMA,names_to = "AGEGRP")


recode_short_age_groups <- function(data) {
  data |>
    mutate(AGEGRP=recode(AGEGRP,"25 to 29 years"="25 to 34 years","30 to 34 years"="25 to 34 years",
                         "35 to 39 years"="35 to 44 years","40 to 44 years"="35 to 44 years",
                         "45 to 49 years"="45 to 54 years","50 to 54 years"="45 to 54 years",
                         "55 to 59 years"="55 to 64 years","60 to 64 years"="55 to 64 years",
                         "70 to 74 years"="70 years and over","75 to 79 years"="70 years and over",
                         "80 to 84 years"="70 years and over","85 years and over"="70 years and over"))
}

hmr_1981_canada_short_1951 <- hmr_1981_canada_15_19 |>
  left_join(canada_age_1951 |> select(-CMA),by="AGEGRP") |>
  recode_short_age_groups() |>
  summarise(base_hmr=weighted.mean(base_hmr,value),.by=AGEGRP)



get_1951_age <- function() {
  # https://archive.org/details/1951981951FV11953engfra/page/n335/mode/2up
  # tables 21 and 25
  tribble(~CMA,~`15 to 19 years`,~`20 to 24 years`,~`25 to 34 years`,~`35 to 44 years`,~`45 to 54 years`,
          ~`55 to 64 years`,~`65 to 69 years`,~`70 years and over`,
          "Calgary",8807,12424,25218,19564,13975,12293,5520,7062,
          "Edmonton",11896,16693,32637,24067,15757,12528,4983,6633,
          "Halifax",10109,12956,24632,18930,12367,8697,2955,4685,
          "Hamilton",16144,21021,45340,37859,29959,22837,9064,12854,
          "Montréal",99074,120668,242924,210600,161452,109477,37270,51047,
          "Ottawa – Gatineau",19289,23131,49110,40348,28665,21884,7918,11784,
          "Québec",22837,24895,45570,38004,26742,17638,6094,9232,
          "Toronto",69372,92474,197601,173215,141220,106721,41188,57047,
          "Vancouver",28347,35859,87223,81281,58904,52902,26225,36634,
          "Winnipeg",22790,28512,60868,52245,38495,33935,13957,17679,
          "Canada",1057972,1088641,1131215+1042734,999133+868567,744679+662656,570690+506152,433497,
          315072+188391+96791+39599+10742+2181) |>
    pivot_longer(-CMA,names_to = "AGEGRP")
}

canada_age_1956 <- tribble(~CMA,~`15 to 19 years`,~`20 to 24 years`,
                           ~`25 to 29 years`,~`30 to 34 years`,~`35 to 39 years`,~`40 to 44 years`,~`45 to 49 years`,
                           ~`50 to 54 years`,~`55 to 59 years`,~`60 to 64 years`,~`65 to 69 years`,~`70 to 74 years`,
                           ~`75 to 79 years`,~`80 to 84 years`,~`85 years and over`,
                           "Canada",1162301,1129110,1198137,1216285,1114385,1025399,878815,733050,629244,524917,464113,
                           370708,227498,117096,48358+13386+2779) |>
  pivot_longer(-CMA,names_to = "AGEGRP")

get_1956_age <- function() {
  # https://archive.org/details/195692501935011958engfra/page/n291/mode/2up
  # tables 20, 16
  tribble(~CMA,~`15 to 19 years`,~`20 to 24 years`,~`25 to 34 years`,~`35 to 44 years`,~`45 to 54 years`,
          ~`55 to 64 years`,~`65 to 69 years`,~`70 years and over`,
          "Calgary",11152,16457,36864,28595,18619,12985,6124,9983,
          "Edmonton",15955,21733,46445,34846,22444,14321,6066,9375,
          "Halifax",11955,16209,27290,22733,15181,10149,3397,5633,
          "Hamilton",18488,21669,56186,47666,35782,25976,10176,16649,
          "Montréal",104143,124629,282816,241109,185543,121702,42077,59799,
          "Ottawa – Gatineau",22829,25351,55908,50199,33967,24314,9011,14385,
          "Québec",24003,25087,50358,42801,31655,20228,6988,10630,
          "Toronto",71073,99884,250066,207468,161872,115643,43856,70209,
          "Vancouver",36494,38811,100019,101727,74615,56166,28426,51831,
          "Winnipeg",24861,30076,65038,59603,44173,33224,14984,23625,
          "Canada",1162301,1129110,1198137+1216285,1114385+1025399,878815+733050,629244+524917,464113, 
          370708 + 227498 + 117096 + 48358+13386+2779) |>
    pivot_longer(-CMA,names_to = "AGEGRP")
}


get_1956_hh <- function() {
  # https://archive.org/details/195692501935011958engfra/page/n427/mode/2up
  # table 36
  tribble(~CMA,~Households,
          "Calgary",57375,
          "Edmonton",63581,
          "Halifax",37171,
          "Hamilton",86990,
          "Montréal",407966,
          "Ottawa – Gatineau",83859,
          "Québec",64825,
          "Toronto",341076,
          "Vancouver",192004,
          "Winnipeg",107841,
          "Canada",3923646)
}



hmr_1981_canada_short_1956 <- hmr_1981_canada_15_19 |>
  left_join(canada_age_1956 |> select(-CMA),by="AGEGRP") |>
  recode_short_age_groups() |>
  summarise(base_hmr=weighted.mean(base_hmr,value),.by=AGEGRP)

canada_age_1941 <- tribble(~CMA,~`15 to 19 years`,~`20 to 24 years`,
                           ~`25 to 29 years`,~`30 to 34 years`,~`35 to 39 years`,~`40 to 44 years`,~`45 to 49 years`,
                           ~`50 to 54 years`,~`55 to 59 years`,~`60 to 64 years`,~`65 to 69 years`,~`70 to 74 years`,
                           ~`75 to 79 years`,~`80 to 84 years`,~`85 years and over`,
                           "Canada",1120035,1032426,966990,834846,759554,676545,635146,591704,506892,407155,307724,
                           217101,135695,71514,27636+8145) |>
  pivot_longer(-CMA,names_to = "AGEGRP")


hmr_1981_canada_short_1941 <- hmr_1981_canada_15_19 |>
  left_join(canada_age_1941 |> select(-CMA),by="AGEGRP") |>
  recode_short_age_groups() |>
  summarise(base_hmr=weighted.mean(base_hmr,value),.by=AGEGRP)


get_1941_age <- function() {
  # https://archive.org/details/1941981941fv31946engfra/page/n5/mode/2up
  # tables 8
  tribble(~CMA,~`15 to 19 years`,~`20 to 24 years`,~`25 to 34 years`,~`35 to 44 years`,~`45 to 54 years`,
          ~`55 to 64 years`,~`65 to 69 years`,~`70 years and over`,
          "Montréal",107370,104116,201228,167658,129929,83775,25838,34119,
          "Toronto",75604,78038,155277,137675,121591,85517,25801,36404,
          "Vancouver",28103,30695,59838,46839,47847,43618,13358,13316,
          "Winnipeg",27927,29460,52405,38881,38294,28958,7664,9969,
          "Calgary",7323,8908,15811,11969,11475,9228,2531,3149,
          "Edmonton",8826,10057,16555,11992,11340,8382,2173,2856,
          # "Halifax",6371,8067,13488,9382,7439,4580,1563,2465,
          # "Hamilton",15142,15227,27657,24055,21438,15346,4666,6804,
          "Canada",1120035,1032426,966990+834846,759554+676545,635146+591704,506892+407155,307724,
          217101+135695+71514+27636+8145) |>
    pivot_longer(-CMA,names_to = "AGEGRP")
}


get_1931_age <- function() {
  tribble(~CMA,~`15 to 19 years`,~`20 to 24 years`,
          ~`25 to 29 years`,~`30 to 34 years`,~`35 to 39 years`,~`40 to 44 years`,~`45 to 49 years`,
          ~`50 to 54 years`,~`55 to 59 years`,~`60 to 64 years`,~`65 to 69 years`,~`70 to 74 years`,
          ~`75 to 79 years`,~`80 to 84 years`,~`85 years and over`,
          "Canada",10.02,8.78,7.58,6.83,6.63,6.23,5.64,4.71,3.54,2.84,2.23,1.65,0.95,0.47,0.18+0.05+0.01) |>
    pivot_longer(-CMA,names_to = "AGEGRP") |>
    mutate(value=value/100*10376786)
}

get_1931_hh <- function() {
  tibble(CMA="Canada",Households=2275171)
}

get_canada_age_data <- function() {
  # background https://publications.gc.ca/collections/collection_2016/statcan/CS91-512-1973.pdf
  get_cansim_connection("17-10-0029") |> 
    filter(GEO=="Canada",
           Sex=="Both sexes") |> 
    collect_and_normalize() |>
    mutate(`Age group`=gsub("-"," to ",`Age group`)) |>
    filter(`Age group` %in% c(paste0(seq(0,85,5) %>% paste0(.," to ",.+4," years")),"90 years and over")) |>
    mutate(`Age group`=recode(`Age group`,"85 to 89 years" ="85 years and over",
                              "90 years and over"="85 years and over")) |>
    summarise(value=sum(val_norm),.by=c(GEO,`Age group`,REF_DATE)) |>
    rename(CMA=GEO,Year=REF_DATE,AGEGRP=`Age group`)
}

get_canada_household_data <- function() {
  get_cansim_connection("17-10-0075") |>
    filter(GEO=="Canada",
           Estimates=="Total number of households",
           `Type of area`=="Total urban and rural areas") |>
    collect_and_normalize() |>
    rename(CMA=GEO,Year=REF_DATE) |>
    select(CMA,Year,Households=val_norm)
}

get_canada_age_data_wrong_age_grouping <- function() {
  simpleCache({
  url="https://www12.statcan.gc.ca/census-recensement/2006/dp-pd/tbt/OpenDataDownload.cfm?PID=88977"
  tmp=tempfile()
  download.file(url,tmp,mode="wb")
  exdir <- file.path(tempdir(),"age2006")
  unzip(tmp,exdir=exdir)
  s<-rsdmx::readSDMX(file.path(exdir,"Structure_97-551-XCB2006005.xml"),isURL = FALSE) 
  
  cls <- slot(s,"codelists") 
  codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))
  
  age_codes <- as.data.frame(slot(s, "codelists"), codelistId = "CL_AGE")
  sex_codes <- as.data.frame(slot(s, "codelists"), codelistId = "CL_SEX")
  geo_codes <- as.data.frame(slot(s, "codelists"), codelistId = "CL_GEO")
  year_codes <- as.data.frame(slot(s, "codelists"), codelistId = "CL_DIM")
  
  
  d<-rsdmx::readSDMX(file.path(exdir,"Generic_97-551-XCB2006005.xml"),isURL = FALSE) |>
    as_tibble() |>
    left_join(geo_codes |> select(GEO=id,GEO.label=label.default),by="GEO") |>
    left_join(sex_codes |> select(Sex=id,Sex.label=label.en),by="Sex") |>
    left_join(age_codes |> select(Age=id,Age.label=label.en),by="Age") |>
    left_join(year_codes |> select(DIM0=id,Year=label.en),by="DIM0")
  
  d |>
    filter(Sex.label=="Total - Sex") |>
    filter(Age.label != "Total - Age groups",Age.label!="Median age") |>
    select(CMA=GEO.label,Year,AGEGRP=Age.label,value=obsValue) |>
    mutate(AGEGRP=gsub("^ +","",AGEGRP)) |>
    mutate(AGEGRP=recode(AGEGRP,"Under 5 years"="0 to 4 years")) 
  }, "age_1921_2006.Rda",path=here::here("data"),refresh=FALSE)
}

compute_aehh <- function(data,hmr) {
  data |>
    full_join(hmr,by="AGEGRP") |>
    mutate(base_hmr=coalesce(base_hmr,0)) |>
    summarize(AEHH=sum(value*base_hmr),.by=CMA)
}

recode_cmas <- function(data,cma_recodes) {
  data |>
    mutate(CMA=as.character(CMA)) |>
    mutate(CMA=recode(CMA,!!!cma_recodes))
}

cmas_1966 <- c("7"="Toronto",
               "5"="Montréal",
               "16"="Vancouver",
               "14"="Calgary",
               "15"="Edmonton",
               "6"="Hamilton",
               "8"="Ottawa – Gatineau",
               "4"="Québec",
               "2"="Halifax",
               "13"="Winnipeg")

# https://mdl.library.utoronto.ca/sites/default/public/mdldata/open/canada/national/statcan/census/1971/statistics/ust/doc/b1demb01.pdf
cmas_1971 <- c("21" = "Toronto",
               "8" = "Montréal",
               "23" = "Vancouver",
               "4" = "Halifax",
               "11" = "Québec",
               "5" = "Hamilton",
               "7" = "London",
               "10" = "Ottawa – Gatineau",
               "26" = "Winnipeg",
               "3" = "Edmonton",
               "1" = "Calgary")

cmas_1976 <- c("535" = "Toronto",
               "462" = "Montréal",
               "933" = "Vancouver",
               "205" = "Halifax",
               "421" = "Québec",
               "537" = "Hamilton",
               "555" = "London",
               "505" = "Ottawa – Gatineau",
               "602" = "Winnipeg",
               "835" = "Edmonton",
               "825" = "Calgary")

cmas_1961 <- c("TORONTO"="Toronto",
               "MONTREAL"="Montréal",
               "VANCOUVER"="Vancouver",
               "HAMILTON"="Hamilton",
               "EDMONTON"="Edmonton",
               "CALGARY"="Calgary",
               "WINNIPEG"="Winnipeg",
               "QUEBEC" = "Québec",
               "OTTAWA"="Ottawa – Gatineau")
cmas_1951 <- c()


get_aeh_hh_data <- function(age_data,hh_data,cma_recodes,hmr=hmr_1981_canada) {
  hh_data <- hh_data |> 
    mutate(CMA=as.character(CMA)) |>
    recode_cmas(cma_recodes)
  if (!("Canada" %in% na.omit(unique(hh_data$CMA)))) {
    hh_data <- hh_data %>%
      bind_rows((.) |> 
                summarize(Households=sum(Households)) |> 
                mutate(CMA="Canada"))
  }
  
  age_data   |>
    mutate(CMA=as.character(CMA)) %>%
    bind_rows((.) |> summarise(value=sum(value),.by=AGEGRP) |> mutate(CMA="Canada")) |>
    summarise(value=sum(value),.by=c(AGEGRP,CMA)) |>
    compute_aehh(hmr) |>
    recode_cmas(cma_recodes) |>
    left_join(hh_data |> summarize(Households=sum(Households),.by=CMA), by="CMA") |>
    select(CMA,Households,AEHH) |>
    summarise(across(matches("Households|AEHH"),sum),.by=CMA)
}


get_canada_aehh_timeline <- function(hmr) {
  get_canada_age_data() |>
    left_join(hmr,by="AGEGRP") |>
    mutate(base_hmr=coalesce(base_hmr,0)) |>
    summarize(AEHH=sum(value*base_hmr),.by=c(CMA,Year))
}

pop_priv_ratios <- tibble(
  Year=c("1931","1941","1951","1956","1961","1966","1971"),
  pop=c(10362833,11489263,14009429,16080791,18238247,20014880,21568310),
  pop_priv=c(10015779,11489263,13572465,15447656,17612145,19405615,21033625)
) |>
  mutate(pop_priv_fact=pop_priv/pop)

get_1941_1976_aehh_hh <- function(){
  bind_rows(
    get_aeh_hh_data(get_1976_age(),get_1976_hh(),cmas_1976)  |> mutate(Year="1976"),
    get_aeh_hh_data(get_1971_age(),get_1971_hh(),cmas_1971)  |> mutate(Year="1971"),
    get_aeh_hh_data(get_1966_age(),get_1966_hh(),cmas_1966)  |> mutate(Year="1966") |> 
      left_join(get_1966_age_ottawa() |> compute_aehh(hmr_1981_canada_15_19),by="CMA") |>
      mutate(AEHH=coalesce(AEHH.y,AEHH.x)) |>
      select(-AEHH.x,-AEHH.y),
    get_aeh_hh_data(get_1961_age(),get_1961_hh(),cmas_1961)  |> mutate(Year="1961"),
    get_1956_age() |> compute_aehh(hmr_1981_canada_short_1956) |> left_join(get_1956_hh(),by="CMA")  |> mutate(Year="1956"),
    get_1951_age() |> compute_aehh(hmr_1981_canada_short_1951) |> left_join(get_1951_hh(),by="CMA")  |> mutate(Year="1951"),
    get_1941_age() |> compute_aehh(hmr_1981_canada_short_1941) |> left_join(get_1941_hh(),by="CMA")  |> mutate(Year="1941"),
  ) |>
    #filter(!(CMA=="Canada" & Year %in% c("1941","1951","1961","1971"))) |>
    bind_rows(
      get_canada_aehh_timeline(hmr_1981_canada) |> left_join(get_canada_household_data(),by=c("CMA","Year")) |> 
        filter(!is.na(Households)) |>
        filter(!(Year %in% c("1941","1951","1961","1971")))
    ) |>
    left_join(pop_priv_ratios |> select(Year,pop_priv_fact),by="Year") |>
    mutate(pop_priv_fact=coalesce(pop_priv_fact,1)) #|>
    #mutate(AEHH=AEHH*pop_priv_fact)
}

