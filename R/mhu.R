library(tidyverse)
library(cansim)
library(canpumf)
library(cmhc)
library(cancensus)

get_mhu_counts <- function() {
  all_years <- c("2011","2016","2021")
  year_colours <- setNames(sanzo::trios$c142 |> rev(),all_years)
  
  
  mhu_cats_2021 <- c("Married spouse or common-law partner without children",
                     "Married spouse or common-law partner with children",
                     "Parent in a one-parent family","Person living alone")
  
  mhu_hh_cats_2021 <- c("One-census-family household without additional persons: Couple without children",
                        "One-census-family household without additional persons: Couple with children",
                        "One-census-family household without additional persons: one-parent family",
                        "Non-census-family household: one-person household")
  fam_grep_string <- "common-law|married|spouse|partner|Comlaw|Commlaw|husband|wife"
  
  mhu4_cats_2021 <- c("Married spouse or common-law partner with children")
  
  
  pumf_2021 <- get_pumf("Census","2021") |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(SEX=fct_recode(GENDER,"Female"="Woman+","Male"="Man+")) |>
    mutate(hh_lower=case_when(HHINC=="Under $2,000" ~ 0,
                              TRUE ~ as.numeric(gsub("\\$|,","",str_extract(as.character(HHINC),"^\\$[\\d,]+")))),
           hh_upper=case_when(HHINC=="$250,000 and over" ~ 350000,
                              TRUE~as.numeric(gsub("\\$|,","",str_extract(as.character(HHINC),"\\$[\\d,]+$"))))) %>%
    mutate(hh_income=(hh_lower+hh_upper)/2) |>
    mutate(max_rent=hh_income*0.3) |>
    mutate(max_rent2=hh_income*0.5) |>
    mutate(gap=pmax(SHELCO*12-max_rent,0)) |>
    mutate(gap2=pmax(SHELCO*12-max_rent2,0)) |>
    mutate(TENUR=fct_recode(TENUR,"Renter"="Renter or Dwelling provided by the local government, First Nation or Indian band")) |>
    mutate(CMA=fct_recode(CMA,"Other"="Other census metropolitan areas, census agglomerations and other geographies")) |>
    mutate(Year="2021")
  
  pumf_2016 <- get_pumf("Census","2016") |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(hh_lower=case_when(HHINC=="Under $2,000" ~ 0,
                              TRUE ~ as.numeric(gsub("\\$|,","",str_extract(as.character(HHINC),"^\\$[\\d,]+")))),
           hh_upper=case_when(HHINC=="$250,000 and over" ~ 350000,
                              TRUE~as.numeric(gsub("\\$|,","",str_extract(as.character(HHINC),"\\$[\\d,]+$"))))) %>%
    mutate(across(c(TOTINC,SHELCO),as.numeric)) %>%
    mutate(TOTINC=ifelse(TOTINC>=88888888,NA,TOTINC)) |>
    mutate(hh_income=(hh_lower+hh_upper)/2) |>
    mutate(max_rent=hh_income*0.3) |>
    mutate(max_rent2=hh_income*0.5) |>
    mutate(gap=pmax(SHELCO*12-max_rent,0)) |>
    mutate(gap2=pmax(SHELCO*12-max_rent2,0)) |>
    mutate(TENUR=fct_recode(TENUR,"Renter"="Rented or Band housing","Owner"="Owned by a member of the household")) |>
    mutate(CMA=fct_recode(CMA,"Other"="Other census metropolitan areas, census agglomerations and other geographies",
                          "Greater Sudbury – Thunder Bay"="Greater Sudbury / Grand Sudbury – Thunder Bay",
                          "Kelowna – Abbotsford-Mission"="Kelowna – Abbotsford"))  |>
    mutate(HHTYPE=fct_recode(HHTYPE,
                             "One-census-family household without additional persons: one-parent family"=
                               "One-census-family household without additional persons: Lone parent family",
                             "One-census-family household with additional persons: one-parent family"=
                               "One-census-family household with additional persons: Lone parent family",
                             "Non-census-family household: one-person household"=
                               "Non-census-family households One person household",
                             "Non-census-family household: two-or-more-person non-census-family household"=
                               "Non-census-family household: Two or more person non-census-family household")) |>
    mutate(CFSTAT=fct_recode(CFSTAT,
                             "Parent in a one-parent family"="Lone parent",
                             "Child of a parent in a one-parent family"="Child of a lone parent",
                             "Person not in a census family living with non-relatives only"=
                               "Person living with non-relatives only")) |>
    mutate(Year="2016")
  
  pumf_2011 <- get_pumf("Census","2011") |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(hh_lower=case_when(HHINC=="Under $2,000" ~ 0,
                              TRUE ~ as.numeric(gsub("\\$|,","",str_extract(as.character(HHINC),"^\\$[\\d,]+")))),
           hh_upper=case_when(HHINC=="$250,000 and over" ~ 350000,
                              TRUE~as.numeric(gsub("\\$|,","",str_extract(as.character(HHINC),"\\$[\\d,]+$"))))) %>%
    mutate(TENUR=fct_recode(TENUR,"Renter"="Rented or Band housing","Owner"="Owned by a member of the household")) |>
    mutate(across(c(TOTINC,GROSRT,OMP),as.numeric)) %>%
    mutate(TOTINC=ifelse(TOTINC>=88888888,NA,TOTINC),
           GROSRT=ifelse(GROSRT>=8888,NA,GROSRT),
           OMP=ifelse(OMP>=8888,NA,OMP)) |>
    mutate(SHELCO=coalesce(GROSRT,OMP)) |>
    mutate(hh_income=(hh_lower+hh_upper)/2) |>
    mutate(max_rent=hh_income*0.3) |>
    mutate(max_rent2=hh_income*0.5) |>
    mutate(gap=pmax(SHELCO*12-max_rent,0)) |>
    mutate(gap2=pmax(SHELCO*12-max_rent2,0)) |>
    mutate(CMA=fct_recode(CMA,"Other"="Other census metropolitan areas, census agglomerations and other geographies",
                          "Greater Sudbury – Thunder Bay"="Greater Sudbury / Grand Sudbury – Thunder Bay",
                          "Kelowna – Abbotsford-Mission"="Kelowna – Abbotsford"))  |>
    mutate(HHTYPE=fct_recode(HHTYPE,
                             "One-census-family household without additional persons: Couple without children"= 
                               "One-family only households: Married couples or common-law partners without children",
                             "One-census-family household without additional persons: Couple with children"=
                               "One-family only households: Married couples or common-law partners with children",
                             "One-census-family household with additional persons: Couple without children"=
                               "One-family households with persons not in a census family: Married couples or common-law partners without children",
                             "One-census-family household with additional persons: Couple with children"=
                               "One-family households with persons not in a census family: Married couples or common-law partners with children",
                             "One-census-family household without additional persons: one-parent family"=
                               "One-family only households: Lone parents",
                             "One-census-family household with additional persons: one-parent family"=
                               "One-family households with persons not in a census family: Lone parents",
                             "Multiple-census-family household"=
                               "Multiple-family households",
                             "Non-census-family household: one-person household"=
                               "Non-family households: One person only",
                             "Non-census-family household: two-or-more-person non-census-family household"=
                               "Non-family households: Two or more persons")) |>
    mutate(CFSTAT=fct_recode(CFSTAT,
                             "Parent in a one-parent family"="Lone parent",
                             "Child of a parent in a one-parent family"="Child of a lone parent",
                             "Person not in a census family living with non-relatives only"=
                               "Person living with non-relatives only")) |> 
    mutate(Year="2011")
  
  
  pumf_selects <- c("CMA","AGEGRP","PRIHM","TENUR","HCORENEED_IND","SHELCO",
                    "CFSTAT","HHTYPE","HHSIZE","CFSIZE","PR",
                    "BEDRM","MOB1","MOB5","TOTINC","EFDECILE","SUBSIDY","ATTSCH",
                    "hh_income","max_rent","max_rent2",
                    "gap","gap2","Year","WEIGHT")
  
  pumf_data <- bind_rows(pumf_2011 |> select(any_of(pumf_selects)),
                         pumf_2016 |> select(any_of(pumf_selects)),
                         pumf_2021 |>  select(any_of(pumf_selects))) |>
    mutate(Year=factor(Year)) |>
    mutate(EFDECILE=fct_recode(EFDECILE,
                               !!!setNames(pumf_2021$EFDECILE |> levels(),
                                           pumf_2016$EFDECILE |> levels())),
           SUBSIDY=fct_recode(SUBSIDY,
                              !!!setNames(c("Not subsidized", "Subsidized", "Not available", "Not applicable"),
                                          c("No, not a subsidized dwelling", "Yes, a subsidized dwelling", "Not available", "Not applicable" ))))
  
  
  
  
  mhu_data <- pumf_data |>
    mutate(MHU_CFSTAT=CFSTAT %in% mhu_cats_2021 | 
             (CFSTAT %in% c("Child of a couple","Child of a parent in a one-parent family") & 
                AGEGRP %in% c("0 to 4 years","5 to 6 years", "7 to 9 years", "10 to 11 years","12 to 14 years",
                              "15 to 17 years","18 to 19 years" ,"20 to 24 years" ) )) |>
    mutate(MHU_HHTYPE=HHTYPE %in% mhu_hh_cats_2021) |>
    mutate(MHU_hhcf=MHU_CFSTAT&MHU_HHTYPE) |>
    mutate(MHU4=CFSTAT %in% mhu4_cats_2021) |>
    mutate(GROSRT=SHELCO) |>
    mutate(HHCLASS="Private household") |>
    mutate(HHSIZE=fct_recode(HHSIZE,NULL = "Not available")) |>
    mutate(CFSIZE=fct_recode(CFSIZE,"1 person"="Person not in a census family",NULL = "Not available")) |>
    mutate(HHSIZE_n=as.numeric(substr(HHSIZE,1,2) |> gsub(" |\\+","",x=_))) |>
    mutate(CFSIZE_n=as.numeric(substr(CFSIZE,1,2) |> gsub(" |\\+","",x=_))) |>
    #mutate(CFSIZE_n=ifelse(CFSIZE=="Non-family person",1,CFSIZE_n)) |>
    mutate(MHU_size=case_when(is.na(CFSIZE_n)|is.na(HHSIZE_n) ~ NA,
                              CFSIZE_n==HHSIZE_n ~ TRUE,
                              CFSIZE_n>=HHSIZE_n ~ NA,
                              CFSIZE_n<HHSIZE_n ~ FALSE,
                              TRUE ~ NA)) |>
    mutate(MHU_size=MHU_size & (!grepl("Child of a",CFSTAT) |
                                  AGEGRP %in% c("0 to 4 years","5 to 6 years", "7 to 9 years", "10 to 11 years","12 to 14 years",
                                                "15 to 17 years","18 to 19 years" ))) |>
    mutate(MHU=MHU_size&MHU_hhcf) |>
    mutate(MHU=coalesce(MHU,MHU_hhcf)) |>
    mutate(MHU1=MHU&(!is.na(CFSIZE_n) & CFSIZE_n==1 | 
                       is.na(CFSIZE_n) & CFSTAT %in% c("Person living alone","NCFPL Living alone"))) |>
    mutate(MHU2=MHU & (grepl("lone parent|lone-parent|Lonepar|one-parent",CFSTAT,ignore.case = TRUE) |
                         HHTYPE=="One-family only households: Lone parents")) |>
    mutate(MHU3=MHU&(!is.na(CFSIZE) & CFSIZE_n==2 & grepl(fam_grep_string,CFSTAT,ignore.case = TRUE) |
                       is.na(CFSIZE_n) & HHTYPE %in% c("One-census-family household without additional persons: Couple without children",
                                                       "One-family only households: Married couples or common-law partners without children"))) |>
    mutate(MHU4=MHU&(!is.na(CFSIZE) & CFSIZE_n>2 & grepl(fam_grep_string,CFSTAT,ignore.case = TRUE) |
                       is.na(CFSIZE_n) & HHTYPE %in% c("Fam-Oneh-Cou-NowM-wNMSD-woAP",
                                                       "One-census-family household without additional persons: Couple with children",
                                                       "One-family only households: Married couples or common-law partners with children"))) |>
    mutate(MHU_cat=case_when(
      MHU1 ~ "MHU1",
      MHU2 ~ "MHU2",
      MHU3 ~ "MHU3",
      MHU4 ~ "MHU4",
      TRUE ~ "Non-MHU",
    )) |>
    mutate(MHU_cat=factor(MHU_cat,levels=c("MHU1","MHU2","MHU3","MHU4","Non-MHU"))) |>
    mutate(school=case_when(is.na(ATTSCH) | ATTSCH == "Not available" ~ "Not available",
                            ATTSCH %in% c("Not applicable","Not applicable (< 15 years)") ~ "Not applicable",
                            ATTSCH %in% c("Did not attend in past 9 mths.",
                                          "NotAttendPast8Months",
                                          "Did not attend school",
                                          "Do not attend",
                                          "Not attending school") ~ "Not attending school", 
                            TRUE ~ "Attending school")) |>
    mutate(NMHU_cat = case_when(
      MHU ~ "MHU",
      HHTYPE == "Non-census-family household: two-or-more-person non-census-family household" ~ "Living with roommates",
      HHTYPE == "Multiple-census-family household" ~ "Living in multifamily household",
      CFSTAT =="Person not in a census family but living with other relatives" ~ "Living with relatives",
      CFSTAT %in% c("Child of a couple","Child of a parent in a one-parent family") & 
        AGEGRP %in% c("25 to 29 years",    "30 to 34 years",    "35 to 39 years") ~ "Young adult living with parents",
      CFSTAT %in% c("Child of a couple","Child of a parent in a one-parent family") & 
        AGEGRP %in% c("20 to 24 years") ~ "20 to 24 year old living with parents",
      CFSTAT %in% c("Child of a couple","Child of a parent in a one-parent family") & 
        AGEGRP %in% c("40 to 44 years" ,   "45 to 49 years",    "50 to 54 years") ~ "Older adult living with parents",
      CFSTAT %in% c("Child of a couple","Child of a parent in a one-parent family") & 
        AGEGRP %in% c("55 to 59 years",    "60 to 64 years","65 to 69 years",    "70 to 74 years",
                      "75 to 79 years",    "80 to 84 years",    "85 years and over") ~ "55+ adult living with parents",
      HHTYPE %in% c("One-census-family household with additional persons: Couple without children",
                    "One-census-family household with additional persons: Couple with children",
                    "One-census-family household with additional persons: one-parent family")  ~ "Living in complex household",
      Year=="2006" & HHTYPE %in% c("One family, common-law couple",
                                   "One family, lone parent family",
                                   "One family, married couple") ~ "Living in complex household",
      TRUE ~ "Other"
    )) |>
    mutate(NMHU_cat=factor(NMHU_cat,
                           levels=c("20 to 24 year old living with parents","Young adult living with parents",
                                    "Older adult living with parents","55+ adult living with parents",
                                    "Living in multifamily household",
                                    "Living with roommates","Living with relatives",
                                    "Living in complex household","Other","MHU"))) |>
    mutate(NMHU_cat=fct_recode(NMHU_cat,"Adult living with parents"="20 to 24 year old living with parents",
                               "Adult living with parents"="Young adult living with parents",
                               "Adult living with parents"="Older adult living with parents",
                               "Adult living with parents"="55+ adult living with parents")) |>
    mutate(MHU_full=case_when(
      MHU ~ MHU_cat,
      TRUE ~ NMHU_cat
    )) |>
    mutate(MHU_full=factor(MHU_full,setdiff(c(levels(MHU_cat),levels(NMHU_cat)),c("MHU","Non-MHU")))) |>
    mutate(Maintainer=case_when(PRIHM %in% c("Yes","Primary household maintainer","PrimaryHhldMaintainr",
                                             "Person is primary maintainer")~ "Maintainer",
                                PRIHM %in% c("No","Not primary hhld. maintainer","NoPrimryHhldMaintain",
                                             "Not primary household maintainer","Person is not primary maintainer")~ "Not a maintainer",
                                PRIHM=="Not applicable"~ "Not applicable",
                                TRUE ~ "Other")) 
  
  nmhu_colours <- setNames(MetBrewer::met.brewer("Egypt",5),
                           c("Adult living with parents",
                             "Living in multifamily household",
                             "Living with roommates",
                             "Living with relatives",
                             "Living in complex household" ))
  
  
  school_cats <- c(
    "Yes, attended trade school, college, CEGEP or other non-university institutions only",
    "Yes, attended trade school, college, CEGEP or other non-university institutions only",
    "Yes, attended university only",
    "Multiple responses",
    "Technical or trade school, community college or CEGEP",
    "University",
    "College, CEGEP, trade school or other non-university institution"
  )
  
  child_years <- c("0 to 4 years","5 to 6 years","7 to 9 years","10 to 11 years","12 to 14 years","15 to 17 years","18 to 19 years")
  
  mhu_counts <- mhu_data |> 
    mutate(Region=case_when(CMA %in% c("Toronto","Montréal","Vancouver","Calgary","Edmonton","Ottawa – Gatineau") ~ CMA,
                            PR %in% c("Ontario","Quebec","British Columbia","Alberta") ~ paste0("Rest of ",PR),
                            TRUE ~ PR)) |>
    filter(AGEGRP!="Not available") |>
    filter(!(AGEGRP %in% child_years)) |>
    filter(Maintainer!="Not applicable") |>
    filter(!((AGEGRP %in% c("18 to 19 years")) & 
               CFSTAT %in% c("Child of a couple","Child of a parent in a one-parent family"))) |>
    mutate(Households=case_when(Maintainer == "Maintainer" ~ WEIGHT,
                                TRUE~0)) |>
    mutate(discount_20_24 = AGEGRP=="20 to 24 years" & 
             MHU_full %in% c("20 to 24 year old living with parents","Adult living with parents") &
             !is.na(MHU_full) & 
             ATTSCH %in% school_cats) |>
    mutate(MHU_weight=case_when(CFSTAT %in% c("Married spouse or common-law partner without children",
                                              "Married spouse or common-law partner with children") ~ 1/2,
                                discount_20_24~1/2,
                                TRUE ~ 1)) |>
    mutate(status=MHU_full) |>
    mutate(status=discount_20_24) |>
    mutate(status=factor(status)) |>
    summarize(MHU_households=sum(MHU_weight*WEIGHT),
              Households=sum(Households),.by=c(Region,status,Year)) %>%
    bind_rows(summarize(.,MHU_households=sum(MHU_households),
                        Households=sum(Households),.by=c(status,Year)) |> mutate(Region="Canada")) |>
    mutate(ratio=MHU_households/sum(Households),.by=c(Region,Year)) %>%
    mutate(Region=factor(Region,levels=filter(.,Year=="2021") |>
                           summarize(ratio=sum(ratio),.by=c(Year,Region)) |> arrange(ratio) |> pull(Region))) |>
    #mutate(ratio=ifelse(status==TRUE,ratio,ratio-1)) |>
    summarise(ratio=sum(ratio)-1,
              across(c(MHU_households,Households),sum),
              .by=c(Region,Year))
}


get_mhu_pumf_combind <- function(refresh=FALSE) {
  simpleCache(get_harmonized_pumf(seq(2001,2021,5)),
              key="mhu_pumf_combined_2001_2021.Rda",path=here::here("data"),
              refresh=refresh) |>
    filter(HHTYPE!="Not applicable") |>
    filter(HHCLASS=="Private household") |>
    mutate(Region=case_when(CMA %in% c("Toronto","Montréal","Vancouver","Calgary","Edmonton","Ottawa – Gatineau") ~ CMA,
                            PR %in% c("Ontario","Quebec","British Columbia","Alberta") ~ paste0("Rest of ",PR),
                            TRUE ~ PR))
}

get_mhu_vs_households <- function(base="CMA",refresh=FALSE) {
  stopifnot(base %in% c("CMA","Region"))
  simpleCache({
    base <- as.name(base)
    
    mhu_pumf_combined <- get_mhu_pumf_combind()
    
    couple_status <- c(
      "Family person: Male spouse", "Family person: Male common-law partner", 
      "Family person: Female spouse", 
      "Family person: Female common-law partner",
      "Male, married spouse (Husband)", "Female, married spouse (Wife)",
      "Male – Common-law partner", "Female – Common-law partner",
      "Married spouse or common-law partner without children" ,
      "Married spouse or common-law partner with children"
    )
    
    living_with_parents <-c("Family person: Child in a now-married couple family",
                            "Family person: Child in a common-law couple family",
                            "Family person: Child in a male lone-parent family",
                            "Family person: Child in a female lone-parent family",
                            "Child of married couple",
                            "Child of common-law couple",
                            "Child in lone-parent family with male parent",
                            "Child in lone-parent family with female parent",
                            "Child of a couple",
                            "Child of a lone parent",
                            "Child of a parent in a one-parent family")
    
    fam_indiv <- mhu_pumf_combined |>
      mutate(fam_weight=ifelse((CFSTAT %in% couple_status) | (AGEGRP=="20 to 24 years" & (CFSTAT %in% living_with_parents)),1/2,1)) |>
      summarise(MHU=sum(WEIGHT*fam_weight),.by=c(Year,!!base))
    
    households <- mhu_pumf_combined |>
      filter(Maintainer=="Maintainer") |>
      summarize(Households=sum(WEIGHT),.by=c(Year,!!base)) 
   
    inner_join(households,fam_indiv,by=c("Year",as.character(base)))
    
  },paste0("mhu_vs_households_",base,".rds"),path=here::here("data"),refresh=refresh)
}

get_rent_prihm_2001_2021 <- function(base="CMA") {
  stopifnot(base %in% c("CMA","Region"))
  base <- as.name(base)
  mhu_pumf_combined <- get_mhu_pumf_combind()
  
  cpi <- simpleCache(cansim::get_cansim_vector("v41690973") |>
                       filter(strftime(Date,"%m")=="05") |>
                       mutate(Year=strftime(Date,"%Y")) |>
                       select(Year,CPI=val_norm) |>
                       filter(Year<=2021) |>
                       mutate(CPI=CPI/last(CPI,order_by=Year)),
                     key="cpi_series.Rda",path=here::here("data"))
  
  prihm <- mhu_pumf_combined |>
    filter(Maintainer!="Not applicable") |>
    filter(!(AGEGRP %in% c("Not available","0 to 4 years" ,"5 to 9 years", "10 to 14 years", "15 to 17 years"))) |>
    summarize(Count=sum(WEIGHT),.by=c(!!base,AGEGRP,Year,Maintainer)) |>
    mutate(Share=Count/sum(Count),.by=c(!!base,AGEGRP,Year)) |>
    mutate(Total=sum(Count),.by=c(AGEGRP,Year)) |>
    filter(Maintainer=="Maintainer") |>
    summarize(HHMR_age=weighted.mean(Share,Total),
              HHMR_crude=weighted.mean(Share,Count/Share),.by=c(!!base,Year))
  
  incomes <- mhu_pumf_combined |>
    filter(AGEGRP %in% c("25 to 29 years" ,"30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years", "50 to 54 years")) |>
    filter(!is.na(TOTINC)) |>
    summarize(mean_working_age_income=weighted.mean(TOTINC,WEIGHT),
              median_working_age_income=spatstat.univar::weighted.quantile(TOTINC,WEIGHT,probs=0.5),
              .by=c(!!base,Year))
  
  rents <- mhu_pumf_combined |>
    filter(MOB1M=="Mover") |>
    filter(TENUR =="Renter",Maintainer=="Maintainer") |>
    filter(GROSRT<10000) |>
    summarize(Rent=spatstat.univar::weighted.quantile(GROSRT,WEIGHT,probs=0.5),
              Rentm=weighted.mean(GROSRT,WEIGHT),
              Rent_q1=spatstat.univar::weighted.quantile(GROSRT,WEIGHT,probs=0.25),
              .by=c(!!base,Year))
  
  rents_2brm <- mhu_pumf_combined |>
    filter(!is.na(BEDRM),BEDRM=="2 bedrooms") |> 
    filter(MOB1M=="Mover") |>
    filter(TENUR =="Renter",Maintainer=="Maintainer") |>
    filter(GROSRT<10000) |>
    summarize(Rent2b=spatstat.univar::weighted.quantile(GROSRT,WEIGHT,probs=0.5),
              Rent2bm=weighted.mean(GROSRT,WEIGHT),
              Rent2b_q1=spatstat.univar::weighted.quantile(GROSRT,WEIGHT,probs=0.25),
              .by=c(!!base,Year)) 
  
  cpi <- simpleCache(cansim::get_cansim_vector("v41690973") |>
                       filter(strftime(Date,"%m")=="05") |>
                       mutate(Year=strftime(Date,"%Y")) |>
                       select(Year,CPI=val_norm) |>
                       filter(Year<=2021) |>
                       mutate(CPI=CPI/last(CPI,order_by=Year)),
                     key="cpi_series.Rda",path=here::here("data"))
  
  base <- as.character(base)
  prihm |>
    left_join(rents,by=c("Year",base)) |>
    left_join(rents_2brm,by=c("Year",base)) |>
    left_join(incomes,by=c("Year",base)) |>
    left_join(cpi,by=c("Year")) 
}

bracket_median <- function(lower_bounds,weights,quantile=0.5){
  d<-tibble(l=lower_bounds,w=weights) |> 
    summarize(n=sum(w),.by=l) |>
    arrange(l) |>
    mutate(s=n/sum(n),c=cumsum(s))
  
  index=which(d$c>=quantile)[1]
  if (index==1) {
    x0 <- 0
  } else {
    x0 <- d$c[index-1]
  }
  x1=d$c[index]
  y0 <- d$l[index]
  if (index<nrow(d)) {
    y1 <- d$l[index+1]
  } else {
    y1 <- d$l[index]*1.3
  }
  
  result <- y0+(y1-y0)*(quantile-x0)/(x1-x0)
  
  result
}

