# import and harmonize census data
supported_pumf_years <- as.character(seq(1981,2021,by=5))


classify_non_mhu <- function(d) {
  # categories to distinguish:
  # * living with parents
  # * living with relatives
  # * roommates
  # * other complex households
  
  CFSTAT_parents <- c(
    "Child-male lone parent family", 
    "Child-female lone parent fam", 
    "Child - two parent family", 
    "Child in husband-wife family", 
    "Child w female lone parent", 
    "Child w male lone-parent", 
    "Family person: Child in a now-married couple family", 
    "Family person: Child in a female lone-parent family", 
    "Family person: Child in a male lone-parent family", 
    "Family person: Child in a common-law couple family", 
    "Child of common-law couple", "Child of married couple", 
    "Child in lone-parent family with female parent", 
    "Child in lone-parent family with male parent", 
    "Child of a couple", 
    "Child of a lone parent", 
    "Child of a parent in a one-parent family",
    "CF: Never-mar. S/D in H/W fam.", 
    "CF: Never-mar. S/D in FLP fam.", 
    "CF: Never-mar. S/D in MLP fam.",
    "CF: Never-mar. S/D in CL fam.", 
    "SonDaughMalFemSpouse", 
    "SonDaughMalFemCommlw", 
    "SonDaughMalLonepFaml", 
    "SonDaughFemLonepFaml"
  )
  
  CFSTAT_roommates <- c(
    "Non-census fam+non rels only", 
    "Non-cens fam person w non-rtvs", 
    "NCFP: Living w non-rel. only",
    "NonFamLivNonRelatves",
    "Non-family person: Living with non-relatives only",
    "Person living with non-relatives only", 
    "Person not in a census family living with non-relatives only"
  )
  
  CFSTAT_relatives <- c(
    "Non-census family + relatives", 
    "Non-cens fam person w relativs", 
    "NCFP: Living with relatives", 
    "NonFamLivWithRelatve", 
    "Non-family person: Living with relatives",
    "Person not in a Census family but living with other relatives",
    "Person not in a census family but living with other relatives"
  )
  
  CFSTAT_complex <- c(
    "Married spouse or common-law partner without children", 
    "Married spouse or common-law partner with children", 
    "Husband", 
    "Wife", 
    "Female lone parent",
    "Male lone parent", 
    "Husband/male common-law partnr", 
    "Wife/female common-law partner", 
    "CF: Husband", 
    "CF: Wife", 
    "CF: Female lone parent", 
    "CF: Male lone parent", 
    "CF: Male common-law partner", 
    "CF: Female common-law partner", 
    "FamlyPersnMaleSpouse", 
    "FamlyPersFemlCommlaw",
    "FamlyPersFemlLonepar", 
    "FamPersMaleLoneparnt",
    "FamlyPersnMaleComlaw",
    "FamlyPersFemleSpouse",
    "Family person: Male lone parent",
    "Family person: Female spouse",
    "Family person: Male spouse",
    "Family person: Female lone parent",
    "Family person: Female common-law partner",
    "Family person: Male common-law partner", 
    "Female parent in lone-parent family", 
    "Male, married spouse (Husband)", 
    "Female, married spouse (Wife)", 
    "Male parent in lone-parent family", 
    "Female – Common-law partner",
    "Male – Common-law partner",
    "Lone parent",
    "Parent in a one-parent family"
  )
  
  CFSTAT_leftover <- c(
    "Not applicable", 
    "Person living alone"
    )
  
  HHTYPE_multifamily <- c(
    "Fam-Multiple family hhld",
    "More than one family",
    "Multiple family",
    "Multiple family household",
    "Multiple-census-family household",                                                                                   
    "Multiple-family households",                                                                                          
    "MultipleFamilyHshold",
    "Secondary 1 parent",                                                                                                  
    "Secondary 2 parent + kids",                                                                                           
    "Secondary hw fam with children",                                                                                      
    "Secondary lone parent family",
    "Secondary 2 parent + no kids",
    "Scndry hw fam without children",
    "Family households: Multiple-family households",
    "Secondary 2 parent + no kids"
  )
  
  HHTYPE_complex <- c(
    "Fam-Oneh-Cou-CLC-wNMSD-wAP",
    "Fam-Oneh-Cou-NowM-wNMSD-wAP",
    "Fam-Oneh-LoneParent Fam-wAP",
    "FamHldCouplMarSdPers",
    "FamHldCouplCmlSdPers",
    "Family households: One-family households: All couples: Common-law couples: With children: With additional persons",
    "Family households: One-family households: All couples: Married couples: With children: With additional persons",
    "Family households: One-family households: Lone-parent families: With additional persons",
    "One-census-family household with additional persons: Couple with children",                                           
    "One-census-family household with additional persons: Lone parent family",                                             
    "One-census-family household with additional persons: one-parent family", 
    "One-family households with persons not in a census family: Lone parents",
    "One-family households with persons not in a census family: Married couples or common-law partners with children",
    "Prim hw fam w chldn & add persns",
    "Prim loneparnt fam w add prsns",
    "Primary 1 parent + others",
    "Primary 2 par + kids + others",
    "Primary 2 par + others/no kids",
    "Prim hw fam wo chldrn w add pr",
    "Fam-Oneh-Cou-NowM-woNMSD-wAP",
    "Fam-Oneh-Cou-CLC-woNMSD-wAP",
    "FamHldCouplCmlWsdPrs",
    "FamHhldLonepaPersons",
    "FamHldCouplMarWsdPrs",
    "Family households: One-family households: All couples: Married couples: Without children: With additional persons",
    "Family households: One-family households: All couples: Common-law couples: Without children: With additional persons",
    "One-family households with persons not in a census family: Married couples or common-law partners without children",
    "One-census-family household with additional persons: Couple without children"
  )
  
  
  HHTYPE_roommates <- c(
    "Non-family - 2+ persons",
    "2+ person non-family household",
    "NonFamily hhld-2 or more pers.",
    "NonFamHhldTwoMorePrs",
    "Non-family households: Two or more persons",
    "Two or more persons not in census families",
    "Non-census-family household: Two or more person non-census-family household",
    "Non-census-family household: two-or-more-person non-census-family household"
  )
  
  HHTYPE_leftover <- c(
    "Primary 1 parent + no others",
    "Primary 2 par + kids/no others",
    "Prim hw fam w chldn wo add prs",
    "Prim loneparnt fam wo add prsn",
    "Fam-Oneh-Cou-NowM-wNMSD-woAP",
    "Fam-Oneh-LoneParent Fam-woAP",
    "Fam-Oneh-Cou-CLC-wNMSD-woAP",
    "FamHldCouplMarSdWprs",
    "FamHhldLoneparWperns",
    "FamHldCouplCmlSdWprs",
    "Family households: One-family households: All couples: Married couples: With children: Without additional persons",
    "Family households: One-family households: Lone-parent families: Without additional persons",
    "Family households: One-family households: All couples: Common-law couples: With children: Without additional persons",
    "One family, common-law couple",
    "One family, married couple",
    "One family, lone parent family",
    "One-family only households: Married couples or common-law partners with children",
    "One-family only households: Lone parents",
    "One-census-family household without additional persons: Couple with children",
    "One-census-family household without additional persons: Lone parent family",
    "One-census-family household without additional persons: one-parent family"
  )
  
  HHTYPE_NA <- c(
    "Not applicable",
    "Not available")
  
  d<-d |>
    mutate(NMHU_cat = case_when(
      MHU ~ "MHU",
      HHTYPE %in% HHTYPE_roommates ~ "Living with roommates",
      HHTYPE %in% HHTYPE_multifamily ~ "Living in multifamily household",
      HHTYPE %in% HHTYPE_complex ~ "Living in complex household",
      CFSTAT %in% CFSTAT_relatives ~ "Living with relatives",
      CFSTAT %in% CFSTAT_parents ~ "Living with parents",
      Year=="2006" & HHTYPE %in% c("One family, common-law couple",
                                    "One family, lone parent family",
                                    "One family, married couple") ~ "Living in complex household",
      TRUE ~ "Other"
    )) |>
    mutate(NMHU_cat=factor(NMHU_cat,
                           levels=c("Living with parents","Living in multifamily household",
                                    "Living with roommates","Living with relatives",
                                    "Living in complex household","Other","MHU"))) |>
    mutate(MHU_full=case_when(
      MHU ~ MHU_cat,
      TRUE ~ NMHU_cat
    )) %>%
    mutate(MHU_full=factor(MHU_full,setdiff(c(levels(MHU_cat),levels(NMHU_cat)),c("MHU","Non-MHU")))) |>
    mutate(Maintainer=case_when(PRIHM %in% c("Yes","Primary household maintainer","PrimaryHhldMaintainr",
                                             "Person is primary maintainer")~ "Maintainer",
                                PRIHM %in% c("No","Not primary hhld. maintainer","NoPrimryHhldMaintain",
                                             "Not primary household maintainer","Person is not primary maintainer")~ "Not a maintainer",
                                PRIHM=="Not applicable"~ "Not applicable",
                                TRUE ~ "Other")) 
  d
}

get_5_year_age_groups <- function(AGEGRP) {
  age_levels <- c("0 to 4 years",
                  "5 to 9 years",
                  "10 to 14 years",
                  "15 to 17 years",
                  "18 to 19 years",
                  "20 to 24 years",
                  "25 to 29 years",
                  "30 to 34 years",
                  "35 to 39 years",
                  "40 to 44 years",
                  "45 to 49 years",
                  "50 to 54 years",
                  "55 to 59 years",
                  "60 to 64 years",
                  "65 to 69 years",
                  "70 to 74 years",
                  "75 to 79 years",
                  "80 to 84 years",
                  "85 years and over",
                  "Not available")
  
  case_when(AGEGRP %in% c("0","1","2","3","4","Less than 1 year old") ~ "0 to 4 years",
                          AGEGRP %in% c("5","6","7","8","9",
                                        "5 to 6 years","7 to 9 years")~"5 to 9 years",
                          AGEGRP %in% c("10","11","12","13","14",
                                        "10 to 11 years","12 to 14 years")~"10 to 14 years",
                          AGEGRP %in% c("15","16","17")~"15 to 17 years",
                          AGEGRP %in% c("18","19")~"18 to 19 years",
                          AGEGRP %in% c("20","21","22","23","24")~"20 to 24 years",
                          AGEGRP %in% c("25","26","27","28","29")~"25 to 29 years",
                          AGEGRP %in% c("30","31","32","33","34")~"30 to 34 years",
                          AGEGRP %in% c("35","36","37","38","39")~"35 to 39 years",
                          AGEGRP %in% c("40","41","42","43","44")~"40 to 44 years",
                          AGEGRP %in% c("45","46","47","48","49")~"45 to 49 years",
                          AGEGRP %in% c("50","51","52","53","54")~"50 to 54 years",
                          AGEGRP %in% c("55","56","57","58","59")~"55 to 59 years",
                          AGEGRP %in% c("60","61","62","63","64")~"60 to 64 years",
                          AGEGRP %in% c("65","66","67","68","69")~"65 to 69 years",
                          AGEGRP %in% c("70","71","72","73","74")~"70 to 74 years",
                          AGEGRP %in% c("75","76","77","78","79")~"75 to 79 years",
                          AGEGRP %in% c("80","81","82","83","84")~"80 to 84 years",
                          AGEGRP %in% c("85","86","87","88","89","85 years of age or over")~"85 years and over",
                          TRUE ~ AGEGRP) |>
    factor(levels=age_levels)
}

get_harmonized_pumf <- function(years,refresh=FALSE){
  supported_years <- as.character(seq(1981,2021,by=5))
  if (length(setdiff(years,supported_years))>0) {
    stop("Year not supported: ",setdiff(years,supported_years))
  }
  selects <- c("CMA","AGEGRP","VALUE","GROSRT","GROSRT_lower","TOTINC","MOB1","MOB5","TENUR","HHTYPE","CFSTAT","WEIGHT",
               "MHU_hhcf","MHU_CFSTAT","MHU_HHTYPE","Year","CFSIZE","HHSIZE","HHCLASS","SEX","CFINC","SUBSIDY",
               "GENSTAT","VISMIN","ETHDER","MARSTH","ATTSCH","ABOID","MTNFR","DTYPE","PRIHM","BEDRM","LFACT")
  
  
  hhsize_recodes <- c("1 person"="One person",
                      "2 persons"="Two persons",
                      "3 persons"="Three persons",
                      "4 persons"="Four persons",
                      "5 persons"="Five persons",
                      "6 persons"="Six persons",
                      "7 or more persons"="Seven or more persons",
                      "7 or more persons"="7 persons or more",
                      "8 or more persons"="Eight+ persons",
                      "7 persons"="Seven persons",
                      "8 persons"="Eight persons",
                      "9 persons"="Nine persons",
                      "10 or more persons"="Ten+ persons")
  cfsize_recodes <- c(#"1 person"="One person",
    "2 persons"="Two persons",
    "3 persons"="Three persons",
    "4 persons"="Four persons",
    "5 persons"="Five persons",
    "6 persons"="Six persons",
    "7 or more persons"="Seven or more persons",
    "2 persons"="CF:  Two persons",
    "3 persons"="CF:  Three persons",
    "4 persons"="CF:  Four persons",
    "5 persons"="CF:  Five persons",
    "6 persons"="CF:  Six persons",
    "7 or more persons"="CF:  Seven or more persons",
    "2 persons"="NonFamlyCenusTwoPers",
    "3 persons"="NonFamlyCenusThrPers",
    "4 persons"="NonFamlyCenusFourPer",
    "5 persons"="NonFamlyCenusFivePer",
    "6 persons"="NonFamlyCenusSixPers",
    "7 or more persons"="NonFamlyCenusSvnPers",
    "2 persons"="2 persons in census family",
    "3 persons"="3 persons in census family",
    "4 persons"="4 persons in census family",
    "5 persons"="5 persons in census family",
    "6 persons"="6 persons in census family",
    "7 persons"="7 persons in census family",
    "8 or more persons"="8+ persons in census family",
    "2 persons"="Persons in census family: Two persons",
    "3 persons"="Persons in census family: Three persons",
    "4 persons"="Persons in census family: Four persons",
    "5 persons"="Persons in census family: Five persons",
    "6 persons"="Persons in census family: Six persons",
    "7 or more persons"="Persons in census family: Seven or more persons",
    "7 or more persons"="7 persons or more",
    "Non-family person"="Person not living in a census family",
    "Non-family person"="Person not in a census family",
    "Non-family person"="Non-family persons",
    "7 persons"="Seven persons",
    "8 persons"="Eight persons",
    "9 persons"="Nine persons",
    "10 or more persons"="Ten+ persons"
    #"8 or more persons"="Eight+ persons",
    #"7 persons"="Seven persons"
  )
  
  tenur_recodes <- c(
    "Owned"="Owner",                                                                           
    "Rented"="Renter",                                                                          
    "Owned (with or without mort.)"="Owner",                                                   
    "Rented-cash/other or band hsg."="Renter",                                                  
    "OwndWithWoutMortgage"="Owner",                                                            
    "Rented Band Housing"="Renter",                                                             
    "Owned - with or without mortgage"="Owner",                                                
    "Rented - for cash, other or Band housing"="Renter",                                        
    "Rented (for cash, other) or Band housing"="Renter",                                        
    "Owned (with or without mortgage)" ="Owner",                                               
    "Owned by a member of the household"="Owner",
    "Rented or Band housing"="Renter",
    "Renter or Dwelling provided by the local government, First Nation or Indian band"="Renter"
  )
  
  cfstat_mhu1 <- c("Not applicable",
                   "Husband",
                   "Child-male lone parent family",
                   "Living alone",
                   "Wife",
                   "Child-female lone parent fam",
                   "Non-census fam+non rels only",
                   "Child - two parent family",
                   "Female lone parent",
                   "Non-census family + relatives",
                   "Male lone parent",
                   "Child in husband-wife family",
                   "Non-cens fam person w relativs",
                   "Wife/female common-law partner",
                   "Non-cens fam person w non-rtvs",
                   "Husband/male common-law partnr",
                   "Child w female lone parent",
                   "Non-cen fam prsn living alone",
                   "Child w male lone-parent",
                   "CF: Wife",
                   "CF: Never-mar. S/D in H/W fam.",
                   "CF: Husband",
                   "NCFP: Living with relatives",
                   "NCFPL Living alone",
                   "CF: Female lone parent",
                   "CF: Never-mar. S/D in FLP fam.",
                   "CF: Never-mar. S/D in MLP fam.",
                   "CF: Male common-law partner",
                   "CF: Female common-law partner",
                   "CF: Male lone parent",
                   "NCFP: Living w non-rel. only",
                   "CF: Never-mar. S/D in CL fam.",
                   "FamlyPersnMaleSpouse",
                   "FamlyPersFemleSpouse",
                   "SonDaughMalFemSpouse",
                   "NonFamLivWithRelatve",
                   "FamlyPersnMaleComlaw",
                   "FamlyPersFemlLonepar",
                   "SonDaughFemLonepFaml",
                   "NonFamilyLivingAlone",
                   "FamlyPersFemlCommlaw",
                   "NonFamLivNonRelatves",
                   "SonDaughMalFemCommlw",
                   "SonDaughMalLonepFaml",
                   "FamPersMaleLoneparnt",
                   "Family person: Female spouse",
                   "Family person: Female lone parent",
                   "Family person: Male spouse",
                   "Family person: Child in a now-married couple family",
                   "Non-family person: Living alone",
                   "Family person: Male lone parent",
                   "Non-family person: Living with relatives",
                   "Family person: Female common-law partner",
                   "Family person: Child in a female lone-parent family",
                   "Family person: Child in a male lone-parent family",
                   "Family person: Male common-law partner",
                   "Family person: Child in a common-law couple family",
                   "Non-family person: Living with non-relatives only",
                   "Male, married spouse (Husband)",
                   "Child of common-law couple",
                   "Person not in a Census family but living with other relatives",
                   "Female, married spouse (Wife)",
                   "Female parent in lone-parent family",
                   "Child of married couple",
                   "Person living alone",
                   "Female – Common-law partner",
                   "Person living with non-relatives only",
                   "Child in lone-parent family with female parent",
                   "Child in lone-parent family with male parent",
                   "Male – Common-law partner",
                   "Male parent in lone-parent family",
                   "Person not in a census family but living with other relatives",
                   "Lone parent",
                   "Child of a couple",
                   "Married spouse or common-law partner with children",
                   "Married spouse or common-law partner without children",
                   "Child of a lone parent",
                   "Parent in a one-parent family",
                   "Person not in a census family living with non-relatives only",
                   "Child of a parent in a one-parent family")
  
  fam_grep_string <- "common-law|married|spouse|partner|Comlaw|Commlaw|husband|wife"
  
  pumf_data <- map_df(years,\(year)do.call(paste0("get_pumf_",year),list(refresh=refresh)) |> select(any_of(selects))) |>
    mutate(AGEGRP=get_5_year_age_groups(AGEGRP)) |>
    mutate(CMA=harmonize_cma(CMA)) |>
    mutate(HHSIZE=fct_recode(HHSIZE,!!!hhsize_recodes)) |>
    mutate(HHSIZE=fct_recode(HHSIZE,NULL = "Not applicable",NULL = "Not Applicable",
                             NULL = "Not available",NULL = "Not Available")) |>
    mutate(CFSIZE=fct_recode(CFSIZE,!!!cfsize_recodes)) |>
    mutate(CFSIZE=fct_recode(CFSIZE,"1 person"="Non-family person")) |>
    mutate(CFSIZE=fct_recode(CFSIZE,NULL = "Not applicable",NULL = "Not available",NULL = "Not Available")) |>
    mutate(HHSIZE_n=as.numeric(substr(HHSIZE,1,2) |> gsub(" |\\+","",x=_))) |>
    mutate(CFSIZE_n=as.numeric(substr(CFSIZE,1,2) |> gsub(" |\\+","",x=_))) |>
    #mutate(CFSIZE_n=ifelse(CFSIZE=="Non-family person",1,CFSIZE_n)) |>
    mutate(MHU_size=case_when(is.na(CFSIZE_n)|is.na(HHSIZE_n) ~ NA,
                              CFSIZE_n==HHSIZE_n ~ TRUE,
                              CFSIZE_n>=HHSIZE_n ~ NA,
                              CFSIZE_n<HHSIZE_n ~ FALSE,
                              TRUE ~ NA)) |>
    mutate(MHU_size=MHU_size & !grepl("Child|S\\/D|SonDaugh",CFSTAT)) |>
    mutate(MHU=MHU_size&MHU_hhcf) |>
    mutate(MHU=coalesce(MHU,MHU_hhcf)) |>
    mutate(HHCLASS=fct_recode(HHCLASS,"Private household"="Private Household","Private household"="Private")) |>
    mutate(TENUR=recode(TENUR,!!!tenur_recodes)) |>
    mutate(TENUR=factor(TENUR,levels=c("Owner","Renter","Not applicable","Not available"))) |>
    mutate(MOB5M=case_when(is.na(MOB5) ~ NA_character_,
                           MOB5 %in% c("Not applicable","Not available") ~ "Not applicable",
                           MOB5 %in% c("Same dwelling","Non-movers")~"Non-movers",
                           TRUE~"Mover")) |>
    mutate(MOB5CD=case_when(is.na(MOB5) ~ NA_character_,
                           MOB5 %in% c("Not applicable","Not available") ~ "Not applicable",
                           MOB5 %in% c("Same dwelling","Non-movers") | 
                             grepl("same csd|same cd|samecd|samecsd|same census division|Non-mig",MOB5,ignore.case=TRUE)~"Same CD",
                           TRUE~"Different CD")) |>
    mutate(MOB1M=case_when(is.na(MOB1) ~ NA_character_,
                           MOB1 %in% c("Not applicable","Not available") ~ "Not applicable",
                           MOB1 %in% c("Same dwelling","Non-movers")~"Non-movers",
                         TRUE~"Mover")) |>
    mutate(CFINC_cleaned =CFINC|> gsub("\\$|,","",x=_) |> gsub("^ +","",x=_) |> gsub("k|K","000",x=_)) |> 
    mutate(CFINC_lower=CFINC_cleaned |> gsub(" .+|-.+|\\+.+","",x=_)) |>
    mutate(CFINC_lower = case_when(
      is.na(CFINC) | (CFINC %in% c("Not available","Not applicable")) ~ NA_character_,
      CFINC=="F-all-Alt.M:>120K/OtM:120<150" ~ "120000",
      CFINC=="No income" ~ "0",
      CFINC %in% c("Negative income","Loss") ~ "-1000",
      grepl("Under",CFINC) ~ "1",
      TRUE ~ CFINC_lower)
    ) |>
    mutate(CFINC_upper=CFINC_cleaned |> str_extract_all("\\d+") |> lapply(last) |> unlist()) |>
    mutate(CFINC_lower=as.numeric(CFINC_lower)) |>
    mutate(CFINC_upper=case_when(
      is.na(CFINC) | (CFINC %in% c("Not available","Not applicable")) ~ NA_character_,
      CFINC %in% c("Negative income","Loss","No income") ~ "0",
      CFINC=="F-all-Alt.M:>120K/OtM:120<150" ~ "150000",
      grepl("or more|and over",CFINC) ~ as.character(1.3*CFINC_lower),
      TRUE ~ CFINC_upper
    )) |>
    mutate(CFINC_upper = ifelse(substr(CFINC_upper,nchar(CFINC_upper),nchar(CFINC_upper))=="9",
                                as.numeric(CFINC_upper)+1,as.numeric(CFINC_upper))) |>
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
    mutate(GEN=case_when(
      GENSTAT %in% c("1G","2+G") ~ GENSTAT,
      GENSTAT %in% c("1st generation","First generation","First generation, respondent born outside Canada") ~ "1G",
      GENSTAT %in% c("2nd generation: Both parents born outside Canada",
                     "2nd generation, both parents born outside Canada",
                     "Second generation, respondent born in Canada, both parents born outside Canada") ~ "2GA",
      GENSTAT %in% c("2nd generation: One parent born outside Canada",
                     "2nd generation, one parent born outside Canada",
                     "Second generation, respondent born in Canada, one parent born outside Canada",
                     "Second generation, respondent born in Canada, one parent born outside Canada and one parent born in Canada") ~ "2GB",
      GENSTAT %in% c("3rd generation and over","3rd generation, respondent born in Canada, both parents born in Canada",
                     "Third generation or more, respondent born in Canada, both parents born in Canada") ~ "3G",
      TRUE ~ "Other")) |>
    mutate(Couple = ifelse(MARSTH %in% c("Now married","Marrd(InclCommn-law)",
                                         "Now married or living in common-law",
                                         "Married - including common-law",
                                         "Legally married (and not separated)",
                                         "Living common law",
                                         "Married"),"Couple","Single")) |>
    mutate(VISMIN=case_when(VISMIN %in% c("Not a vis. minority","Non-member of visible minority",
                                          "Not a visible minority","Not available") ~ "Not a visible minority",
                            VISMIN %in% c("Other vis. minority") ~ "Other visible minority",
                            VISMIN %in% c("Member of visible minority","Visible minority",
                                          "Multiple visible minorities","Visible minority, n.i.e.",
                                          "Multiple visible minority") ~ "Visible minority",
                            TRUE ~ VISMIN)) |>
    mutate(VM=case_when(!is.na(ABOID) & !(ABOID %in% c("Non-Indigenous identity",
                                                       "Non-Aboriginal identity",
                                                       "Non-Aboriginal identity population",
                                                       "Non-Aboriginal population",
                                                       "Non-AborignlPopltion",
                                                       "Non-aboriginal origins")) ~ "Indigenous",
                        VISMIN %in% c("Chinese", "African/Caribbean/Haitian", "Visible minority", "Black",
                                      "Latin American", "Southeast Asian", "Korean", "Filipino",
                                      "South Asian", "Arab",
                                      "West Asian", "Japanese") ~ "Visible minority",
                        TRUE ~ VISMIN)) |>
    mutate(VM=case_when(VM=="Not a visible minority" & grepl("^True",MTNFR) & (GEN %in% c("2GB","2+G","3G")) ~ "Francophone",
                        VM %in% c("Not a visible minority","Other single responses (1981)","Not applicable") ~ "Not a visible minority",
                        TRUE ~ VM)) |>
    mutate(IMM=ifelse(GEN=="1G","Immigrant","Non-Immigrant") |> factor(levels =c("Non-Immigrant","Immigrant"))) |>
    mutate(LFSTAT=case_when(grepl("Unemp",LFACT)~"Unemployed",grepl("Employed",LFACT)~"Employed",TRUE~"Not in labour force")) |>
    mutate(LFSTAT=factor(LFSTAT,levels=c("Employed","Unemployed","Not in labour force"))) 
  
  all_cmas <- count(pumf_data,CMA) |> arrange(-n) |> pull(CMA)
  all_cmas <- c(setdiff(all_cmas,"Other"),"Other")
  
  child_ages <- c("0 to 4 years","5 to 9 years","10 to 14 years","15 to 17 years","18 to 19 years")
  
  
  pumf_data |>
    mutate(CMA=factor(CMA,levels=all_cmas)) |>
    classify_non_mhu() |>
    mutate(MHU_full=case_when(!(AGEGRP %in% child_ages) ~ MHU_full,
                              HHTYPE %in% c("One-census-family household without additional persons: Couple with children",
                                            "One-family only households: Married couples or common-law partners with children") ~ factor("MHU4"),
                              HHTYPE %in% c("One-census-family household without additional persons: one-parent family",
                                            "One-census-family household without additional persons: Lone parent family",
                                            "One-family only households: Lone parents") ~ factor("MHU2"),
                              TRUE ~ MHU_full)) |>
    mutate(MHU_cat=case_when(MHU_full %in% c("MHU1","MHU2","MHU3","MHU4") ~ MHU_full, TRUE ~ MHU_cat)) |>
    mutate(MHU=case_when(MHU_full %in% c("MHU1","MHU2","MHU3","MHU4") ~ TRUE, TRUE ~ MHU)) 
}


harmonize_cma <- function(cma) {
  cma |>
    gsub(" - \\d{3}","",x=_) |>
    recode("Other census metropolitan areas, Census Agglomerations and other geographies"="Other",
           "Not Applicable"="Other",
           "Not applicable"="Other",
           "Sudbury(580)+Thunder Bay(595)"="Greater Sudbury – Thunder Bay",
           "Sudbury/Thunder Bay"="Greater Sudbury – Thunder Bay",
           "Greater Sudbury / Grand Sudbury – Thunder Bay"="Greater Sudbury – Thunder Bay",
           "Greater Sudbury/Grand Sudbury – Thunder Bay"="Greater Sudbury – Thunder Bay",
           "Greater Sudbury/Grand Sudbury – Thunder Bay"="Greater Sudbury – Thunder Bay",
           "Sudbury and Thunder Bay"="Greater Sudbury – Thunder Bay",
           "Ottawa Hull"="Ottawa – Gatineau",
           "Ottawa-Hull"="Ottawa – Gatineau",
           "St. Catharines - Niagara"="St. Catharines – Niagara",
           "St. Catharines-Niagara"="St. Catharines – Niagara",
           "St.Catharines/Niagara"="St. Catharines – Niagara",
           "Sherbrooke(433)+Trois-Riv(442)"="Sherbrooke – Trois-Rivières",
           "Sherbrooke/Trois-Riv"="Sherbrooke – Trois-Rivières",
           "Sherbrooke and Trois-Rivières"="Sherbrooke – Trois-Rivières",
           "Regina(705) + Saskatoon(725)"="Regina – Saskatoon",
           "Regina & Saskatoon"="Regina – Saskatoon",
           "Montreal"="Montréal",
           "Kelowna – Abbotsford"="Kelowna – Abbotsford-Mission",
           "Quebec"="Québec",
           "Regina/Saskatoon"="Regina – Saskatoon",
           "Regina and Saskatoon"="Regina – Saskatoon"
    )
}

get_pumf_2021 <- function(refresh=FALSE) {
  mhu_cats_2021 <- c("Married spouse or common-law partner without children",
                     "Married spouse or common-law partner with children",
                     "Parent in a one-parent family","Person living alone")
  
  mhu_hh_cats_2021 <- c("One-census-family household without additional persons: Couple without children",
                        "One-census-family household without additional persons: Couple with children",
                        "One-census-family household without additional persons: one-parent family",
                        "Non-census-family household: one-person household")
  
  mhu4_cats_2021 <- c("Married spouse or common-law partner with children")
  
  pumf_2021 <- get_pumf("Census",pumf_version="2021",refresh_layout = refresh) |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(CMA=fct_recode(CMA,"Other" = "Other census metropolitan areas, census agglomerations and other geographies")) |>
    mutate(Year="2021") |>
    mutate(across(c(TOTINC,VALUE),\(x)ifelse(x>=88888888,NA,x)))  |>
    mutate(MHU_CFSTAT=CFSTAT %in% mhu_cats_2021) |>
    mutate(MHU_HHTYPE=HHTYPE %in% mhu_hh_cats_2021) |>
    mutate(MHU_hhcf=MHU_CFSTAT&MHU_HHTYPE) |>
    mutate(MHU4=CFSTAT %in% mhu4_cats_2021) |>
    mutate(GROSRT=SHELCO) |>
    mutate(HHCLASS="Private household") |>
    mutate(SEX=fct_recode(GENDER,"Female"="Woman+","Male"="Man+")) 
  
  pumf_2021
}

get_pumf_2016 <- function(refresh=FALSE) {
  mhu_cats_2016 <- c("Married spouse or common-law partner without children",
                     "Married spouse or common-law partner with children",
                     "Lone parent","Person living alone")
  
  mhu_hh_cats_2016 <- c("One-census-family household without additional persons: Couple without children",
                        "One-census-family household without additional persons: Couple with children",
                        "One-census-family household without additional persons: Lone parent family",
                        "Non-census-family households One person household")
  
  mhu4_cats_2016 <- c("Married spouse or common-law partner with children")
  
  pumf_2016 <- get_pumf("Census",pumf_version="2016",refresh_layout = refresh) |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(CMA=fct_recode(CMA,"Other" = "Other census metropolitan areas, census agglomerations and other geographies")) |>
    mutate(Year="2016") |>
    mutate(across(c(CQPPB,CHDBN,CAPGN,TOTINC,VALUE,SHELCO),as.numeric)) |>
    mutate(across(c(VALUE),\(x)ifelse(x>=88888888,NA,x)))  |>
    mutate(across(c(CQPPB,CHDBN,CAPGN,TOTINC),\(x)ifelse(x>=8888888,NA,x)))  |>
    #mutate(MHU=CFSTAT %in% mhu_cats_2016) |>
    mutate(MHU_CFSTAT=CFSTAT %in% mhu_cats_2016) |>
    mutate(MHU_HHTYPE=HHTYPE %in% mhu_hh_cats_2016) |>
    mutate(MHU_hhcf=MHU_CFSTAT&MHU_HHTYPE) |>
    mutate(MHU4=CFSTAT %in% mhu4_cats_2016) |>
    mutate(GROSRT=SHELCO) |>
    mutate(HHCLASS="Private household")
  
  pumf_2016
}

get_pumf_2011 <- function(refresh=FALSE) {
  mhu_cats_2011 <- c("Married spouse or common-law partner without children",
                     "Married spouse or common-law partner with children",
                     "Person living alone",
                     "Lone parent")
  mhu_hh_cats_2011 <- c("One-family only households: Married couples or common-law partners without children",
                        "One-family only households: Married couples or common-law partners with children",
                        "One-family only households: Lone parents",
                        "Non-family households: One person only")
  
  mhu4_cats_2011 <- c("Married spouse or common-law partner with children")
  
  pumf_2011 <- get_pumf("Census",pumf_version="2011",refresh_layout = refresh) |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(CMA=fct_recode(CMA,"Other" = "Other census metropolitan areas, census agglomerations and other geographies")) |>
    mutate(Year="2011") |>
    mutate(across(c(CQPPB,CHDBN,CAPGN,TOTINC,VALUE,GROSRT),as.numeric)) |>
    mutate(across(c(CQPPB,CHDBN,CAPGN,TOTINC,VALUE),\(x)ifelse(x>=8888888,NA,x)))  |>
    mutate(across(c(GROSRT),\(x)ifelse(x>=9999,NA,x))) |>
    mutate(MHU_CFSTAT=CFSTAT %in% mhu_cats_2011) |>
    mutate(MHU_HHTYPE=HHTYPE %in% mhu_hh_cats_2011) |>
    mutate(MHU_hhcf=MHU_CFSTAT&MHU_HHTYPE) |>
    mutate(HHCLASS="Private household")
  
  pumf_2011
}

get_pumf_2006 <- function(refresh=FALSE) {
  mhu_cats_2006 <- c("Male, married spouse (Husband)",
                     "Female, married spouse (Wife)",
                     "Male – Common-law partner",
                     "Female – Common-law partner",
                     "Male parent in lone-parent family",
                     "Female parent in lone-parent family",
                     "Person living alone")
  mhu_hh_cats_2006 <- c("One family, married couple",
                        "One family, common-law couple",
                        "One family, lone parent family",
                        "Person living alone")
  
  
  pumf_2006 <- get_pumf("Census",pumf_version="2006",refresh_layout = refresh) |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(Year="2006") |>
    mutate(across(c(CQPPB,CHDBN,TOTINC,VALUE,GROSRT),as.numeric)) |>
    mutate(across(c(CQPPB,CHDBN,TOTINC,VALUE),\(x)ifelse(x>=8888888,NA,x)))  |>
    mutate(across(c(GROSRT),\(x)ifelse(x>=9999,NA,x)))  |>
    mutate(MHU_CFSTAT=CFSTAT %in% mhu_cats_2006) |>
    mutate(MHU_HHTYPE=HHTYPE %in% mhu_hh_cats_2006) |>
    mutate(MHU_hhcf=MHU_CFSTAT&MHU_HHTYPE)
  
  pumf_2006
}

get_pumf_2001 <- function(refresh=FALSE) {
  mhu_cats_2001 <- c("Family person: Male spouse",
                     "Family person: Male common-law partner",
                     "Family person: Male lone parent",
                     "Family person: Female spouse",
                     "Family person: Female common-law partner",
                     "Family person: Female lone parent",
                     "Non-family person: Living alone")
  
  mhu_hh_cats_2001 <- c("Family households: One-family households: All couples: Married couples: Without children: Without additional persons",
                        "Family households: One-family households: All couples: Married couples: With children: Without additional persons",
                        "Family households: One-family households: All couples: Common-law couples: Without children: Without additional persons",
                        "Family households: One-family households: All couples: Common-law couples: With children: Without additional persons",
                        "Family households: One-family households: Lone-parent families: Without additional persons",
                        "Non-family households: One person only")
  
  # pumf_2001_fam <- get_pumf("Census",pumf_version = "2001 (families)") |>
  #   label_pumf_data(rename_columns = FALSE) |>
  #   mutate(Year="2001")
  # pumf_2001_hh <- get_pumf("Census",pumf_version = "2001 (households)") |>
  #   label_pumf_data(rename_columns = FALSE) |>
  #   mutate(Year="2001") 
  
  pumf_2001 <- get_pumf("Census",pumf_version="2001",refresh_layout = refresh) |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(Year="2001") |>
    mutate(across(c(VALUEP,GROSRTP,TOTINCP),~ifelse(.x=="Not applicable",NA,.x))) |>
    mutate(across(c(VALUEP,GROSRTP,TOTINCP),as.numeric)) |>
    mutate(across(c(TOTINCP),\(x)ifelse(x>=9999999,NA,x)))|>
    mutate(across(c(VALUEP),\(x)ifelse(x>=999999,NA,x))) |>
    mutate(across(c(GROSRTP),\(x)ifelse(x>=9999,NA,x))) |>
    mutate(MHU_CFSTAT=CFSTATP %in% mhu_cats_2001) |>
    mutate(MHU_HHTYPE=HTYPEP %in% mhu_hh_cats_2001) |>
    mutate(MHU_hhcf=MHU_CFSTAT&MHU_HHTYPE) |>
    mutate(MTNFR=ifelse(grepl("French",MTNP),"True: French","False: Not French")) |>
    rename(CMA=CMAP,AGEGRP=AGEP,VALUE=VALUEP,GROSRT=GROSRTP,TOTINC=TOTINCP,
           MOB1=MOB1P,MOB5=MOB5P,SEX=SEXP,EFINC=CFINCP,LFACT=LFACTP,
           TENUR=TENURP,HHTYPE=HTYPEP,CFSTAT=CFSTATP,HHSIZE=UNITSP,CFSIZE=CFSIZEP,
           CFINC=CFINCP,VISMIN=VISMINP,GENSTAT=GENSTPOB,ETHDER=ETHNICR,ATTSCH=SCHATTP,
           MARSTH=MARSTHP, ABOID=ABSRP,PRIHM=PRMAINP,
           HHCLASS=HHCLASSP,WEIGHT=WEIGHTP)
  
  pumf_2001
}

get_pumf_1996 <- function(refresh=FALSE) {
  mhu_cats_1996 <- c("FamlyPersnMaleSpouse",
                     "FamlyPersnMaleComlaw",
                     "FamPersMaleLoneparnt",
                     "FamlyPersFemleSpouse",
                     "FamlyPersFemlCommlaw",
                     "FamlyPersFemlLonepar",
                     "NonFamilyLivingAlone")
  
  mhu_hh_cats_1996 <- c("FamHldCouplMarWsdWpr",
                        "FamHldCouplMarSdWprs",
                        "FamHldCouplCmlWsdWpr",
                        "FamHldCouplCmlSdWprs",
                        "FamHhldLoneparWperns",
                        "NonFamHholdOnePerson")
  
  pumf_1996 <- get_pumf("Census",pumf_version="1996",refresh_layout = refresh) |>
    mutate(GENSTAT=case_when(POBP %in% c("98")~"Not available",
                             POBP %in% as.character(seq(33,37)) ~ "2+G",
                             as.integer(POBP) %in% seq(1,10) ~ "2+G",
                             TRUE ~ "1G")) |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(Year="1996") |>
    mutate(across(c(TOTINCP),~ifelse(.x=="Not available",NA,.x))) |>
    mutate(across(c(VALUEP,GROSRTP,TOTINCP),as.numeric)) |>
    mutate(across(c(TOTINCP),\(x)ifelse(x>=9999999,NA,x)))|>
    mutate(across(c(VALUEP),\(x)ifelse(x>=999999,NA,x))) |>
    mutate(across(c(GROSRTP),\(x)ifelse(x>=9999,NA,x))) |>
    mutate(MHU_CFSTAT=CFSTATP %in% mhu_cats_1996) |> 
    mutate(MHU_HHTYPE=HTYPEP %in% mhu_hh_cats_1996) |>
    mutate(MHU_hhcf=MHU_CFSTAT&MHU_HHTYPE) |>
    mutate(MTNFR=ifelse(grepl("French",MTNP),"True: French","False: Not French")) |>
    rename(CMA=CMAPUMFP,AGEGRP=AGEP,VALUE=VALUEP,GROSRT=GROSRTP,TOTINC=TOTINCP,
           MOB1=MOB1P,MOB5=MOB5P,TENUR=TENURP,HHTYPE=HTYPEP,CFSTAT=CFSTATP,HHSIZE=UNITSP,
           CFSIZE=CFSIZEP,HHCLASS=HHCLASSP,SEX=SEXP,CFINC=CFINCP,
           VISMIN=VISMINP,MARSTHP,ATTSCH=SCHATTP,MARSTH=MARSTHP,
           ABOID=ABSRP,PRIHM=PRMAINP,LFACT=LFACTP,
           WEIGHT=WEIGHTP)
  
  pumf_1996
}

get_pumf_1991 <- function(refresh=FALSE) {
  mhu_cats_1991 <- c("CF: Husband",
                     "CF: Male common-law partner",
                     "CF: Male lone parent",
                     "CF: Wife",
                     "CF: Female common-law partner",
                     "CF: Female lone parent",
                     "NCFPL Living alone")
  
  mhu_hh_cats_1991 <- c("Fam-Oneh-Cou-NowM-wo NMSD-woAP",
                        "Fam-Oneh-Cou-NowM-wNMSD-woAP",
                        "Fam-Oneh-Cou-CLC-woNMSD-woAP",
                        "Fam-Oneh-Cou-CLC-wNMSD-woAP",
                        "Fam-Oneh-LoneParent Fam-woAP",
                        "NonFamily hhld-one person only")
  
  
  pumf_1991 <- get_pumf("Census",pumf_version="1991",refresh_layout = refresh) |>
    mutate(GENSTAT=case_when(POBP %in% c("98")~"Not available",
                             as.integer(POBP) %in% seq(1,10) ~ "2+G",
                             POBP %in% as.character(seq(29,33)) ~ "2+G",
                             TRUE ~ "1G")) |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(Year="1991") |>
    mutate(across(c(VALUEP,GROSRTP,TOTINCP),~ifelse(.x =="Not applicable",NA,.x))) |>
    mutate(across(c(VALUEP,GROSRTP,TOTINCP),as.numeric)) |>
    mutate(across(c(TOTINCP),\(x)ifelse(x>=9999999,NA,x)))|>
    mutate(across(c(VALUEP),\(x)ifelse(x>=999999,NA,x))) |>
    mutate(across(c(GROSRTP),\(x)ifelse(x>=9999,NA,x))) |>
    mutate(MHU_CFSTAT=CFSTATP %in% mhu_cats_1991) |> 
    mutate(MHU_HHTYPE=HTYPEP %in% mhu_hh_cats_1991) |>
    mutate(MHU_hhcf=MHU_CFSTAT&MHU_HHTYPE) |>
    mutate(MTNFR=ifelse(grepl("French",MTNP),"True: French","False: Not French")) |>
    rename(CMA=CMAPUMFP,AGEGRP=AGEP,VALUE=VALUEP,GROSRT=GROSRTP,TOTINC=TOTINCP,
           MOB5=MOB5P,SEX=SEXP,LFACT=LFACTP,
           MOB1=MOB1P,TENUR=TENURP,HHTYPE=HTYPEP,CFSTAT=CFSTATP,HHSIZE=UNITSP,
           CFSIZE=CFSIZEP,HHCLASS=HHCLASSP,CFINC=CFINCP,ATTSCH=SCHATTP,MARSTH=MARSTHP,
           ABOID=ABETHNCP,VISMIN=VISMINP,PRIHM=PRMAINP,
           WEIGHT=WEIGHTP)
  pumf_1991
}

get_pumf_1986 <- function(refresh=FALSE){
  mhu_cats_1986 <- c("Wife/female common-law partner",
                     "Husband/male common-law partnr",
                     "Female lone parent",
                     "Male lone parent",
                     "Non-cen fam prsn living alone")
  mhu_hh_cats_1986 <- c("Prim hw fam w chldn wo add prs",
                        "Prim loneparnt fam wo add prsn",
                        "Prim hw fam wo chldrn/add pers",
                        "One person non-family hhld")
  value_levels_1986 <- c("Not applicable"=  NA,
                         "Under $20,000"=  10000,
                         "$20,000-$34,999"=  27500,
                         "$35,000-$49,999"=  42500,
                         "$50,000-$64,999"=  57500,
                         "$65,000-$79,999"=  72500,
                         "$80,000-$99,999"=  90000,
                         "$100,000-$149,999"=  125000,
                         "$150,000 and over"=  200000)
  rent_levels_1986 <- c("Not applicable"=  NA,
                        "Less than $100"=50,
                        "$100 - $199"=150,
                        "$200 - $299"=250,
                        "$300 - $399"=350,
                        "$400 - $499"=450,
                        "$500 - $599"=550,
                        "$600 - $799"=700,
                        "$800 - $999"=900,
                        "$1,000 and over"=1200)
  rent_levels_1986_lower <- c("Not applicable"=  NA,
                        "Less than $100"=0,
                        "$100 - $199"=100,
                        "$200 - $299"=200,
                        "$300 - $399"=300,
                        "$400 - $499"=400,
                        "$500 - $599"=500,
                        "$600 - $799"=600,
                        "$800 - $999"=800,
                        "$1,000 and over"=1000)
  
  
  pumf_1986 <- get_pumf("Census",pumf_version="1986",refresh_layout = refresh) |>
    mutate(GENSTAT=case_when(BIRTPLAC %in% c("0")~"Not applicable",
                             as.integer(BIRTPLAC) %in% seq(1,11) ~ "2+G",
                             TRUE ~ "1G")) |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(Year="1986") |>
    mutate(VALUEP=recode(VALUEP,!!!value_levels_1986)) |>
    mutate(GROSRT_lower=recode(GROSRTP,!!!rent_levels_1986_lower)) |>
    mutate(GROSRTP=recode(GROSRTP,!!!rent_levels_1986)) |>
    mutate(across(c(TOTINCP),~case_when(.x=="Zero income"~ "0",
                                        .x=="-$50,000 or less"~ "-75000",
                                         .x=="$140,000 or more"~ "160000",
                                        TRUE ~ .x))) |>
    mutate(across(c(TOTINCP),as.numeric)) |>
    mutate(across(c(TOTINCP),\(x)ifelse(x>=9999999,NA,x)))|>
    mutate(MHU_CFSTAT=CFSTAT %in% mhu_cats_1986) |> 
    mutate(MHU_HHTYPE=HTYPE %in% mhu_hh_cats_1986) |>
    mutate(MHU_hhcf=MHU_CFSTAT&MHU_HHTYPE) |>
    mutate(WEIGHT=50) |>
    mutate(MTNFR=ifelse(grepl("French",MOTHERTG),"True: French","False: Not French")) |>
    rename(CMA=CMAPUST,AGEGRP=AGEP,VALUE=VALUEP,GROSRT=GROSRTP,TOTINC=TOTINCP,MOB5=MOB5P,
           MARSTH=MARSTP,ABOID=ABETHNIC,PRIHM=HMAINP,
           TENUR=TENURP,HHTYPE=HTYPE,HHSIZE=UNITSP,SEX=SEXP,CFINC=CFINCP) 
    # mutate(VISMIN=case_when(ETHNICOR %in% c("0")~"Not applicable",
    #                         ETHNICOR %in% c("Chinese","South Asian","Black, African Black & Caribbn","Filipino",
    #                                         "East/South East Asian origins","West Asian & Arab origins")~ETHNICOR,
    #                         ETHNICOR %in% c("South Asian")~"Other single responses (1981)",
    #                         TRUE ~ "Not a visible minority")) |>
    
    
  pumf_1986
}

get_pumf_1981 <- function(refresh=FALSE){
  mhu_cats_1981 <- c("WIFE",
                     "HUSBAND",
                     "FEMALE LONE PARENT",
                     "MALE LONE PARENT",
                     "LIVING ALONE")
  mhu_hh_cats_1981 <- c("PRIMARY 2 PAR + KIDS/NO OTHERS",
                        "PRIMARY 1 PARENT + NO OTHERS",
                        "PRIMARY 2 PAR + NO KIDS/OTHERS",
                        "NON-FAMILY - 1 PERSON")
  value_levels_1981 <- c("NOT APPLICABLE"=  NA,
                         "UNDER $20000"=  10000,
                         "$20000-34999"=  27500,
                         "$35000-49999"=  42500,
                         "$50000-64999"=  57500,
                         "$65000-79999"=  72500,
                         "$80000-99999"=  90000,
                         "$100000-149999"=  125000,
                         "$150000 AND OVER"=  200000)
  rent_levels_1981 <- c("NOT APPLICABLE"=  NA,
                        "$99 AND LESS"=50,
                        "$100-199"=150,
                        "$200-299"=250,
                        "$300-399"=350,
                        "$400-499"=450,
                        "$500-599"=550,
                        "$600-799"=700,
                        "$800-999"=900,
                        "$1000 AND OVER"=1200)
  rent_levels_1981_lower <- c("NOT APPLICABLE"=  NA,
                              "$99 AND LESS"=0,
                              "$100-199"=100,
                              "$200-299"=200,
                              "$300-399"=300,
                              "$400-499"=400,
                              "$500-599"=500,
                              "$600-799"=600,
                              "$800-999"=800,
                              "$1000 AND OVER"=1000)
  
  
  pumf_1981 <- get_pumf("Census",pumf_version="1981",refresh_layout = refresh) |>
    mutate(GENSTAT=case_when(BIRTPLAC %in% c("0","00")~"Not applicable",
                             as.integer(BIRTPLAC) %in% seq(1,12) ~ "2+G",
                             TRUE ~ "1G")) |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(Year="1981") |>
    mutate(VALUE=recode(VALUE,!!!value_levels_1981)) |>
    mutate(GROSRT_lower=recode(GROSRT,!!!rent_levels_1981_lower)) |>
    mutate(GROSRT=recode(GROSRT,!!!rent_levels_1981)) |>
    mutate(TOTINC=case_when(TOTINC=="ZERO"~ "0",
                            TRUE ~ TOTINC)) |>
    mutate(across(c(TOTINC),as.numeric)) |>
    mutate(across(c(TOTINC),\(x)ifelse(x>=9999999,NA,x)))|>
    mutate(MHU_CFSTAT=CFSTAT %in% mhu_cats_1981) |> 
    mutate(MHU_HHTYPE=HTYPE %in% mhu_hh_cats_1981) |>
    mutate(MHU_hhcf=MHU_CFSTAT&MHU_HHTYPE) |>
    mutate(WEIGHT=50) |>
    mutate(MTNFR=ifelse(grepl("FRENCH",MOTHERTG),"True: French","False: Not French")) |>
    rename(MARSTH=MARST,HHTYPE=HTYPE,ATTSCH=ATEND) |>
    mutate(ETHNICOR=str_to_title_f(ETHNICOR)) |>
    mutate(VISMIN=case_when(ETHNICOR %in% c("Not Applicable")~"Not applicable",
                            ETHNICOR %in% c("Chinese","African/Caribbean/Haitian")~ETHNICOR,
                            ETHNICOR %in% c("Other Multiple Responses")~"Other single responses (1981)",
                            TRUE ~ "Not a visible minority")) |>
    mutate(age=as.integer(AGE)) |>
    mutate(AGEGRP=case_when(age <5~"0 to 4 years",
                            age<10~"5 to 9 years",
                            age<15~"10 to 14 years",
                            age<18~"15 to 17 years",
                            age<20~"18 to 19 years",
                            age<25~"20 to 24 years", 
                            age<30~"25 to 29 years", 
                            age<35~"30 to 34 years", 
                            age<40~"35 to 39 years",
                            age<45~"40 to 44 years", 
                            age<50~"45 to 49 years", 
                            age<55~"50 to 54 years", 
                            age<60~"55 to 59 years",
                            age<65~"60 to 64 years", 
                            age<70~"65 to 69 years", 
                            age<75~"70 to 74 years", 
                            age<80~"75 to 79 years",
                            age<85~"80 to 84 years", 
                            age<90~"85 years and over",
                            TRUE ~ "Not available")) |>
    select(-age,-AGE) |>
    rename(PRIHM=HMAIN) |>
    mutate(across(c(CMA),str_to_title_f),
           across(c(HHSIZE,CFSIZE,TENUR,HHCLASS,ATTSCH,MARSTH,CFSTAT,HHTYPE,CFINC,SEX,MOB5,DTYPE,PRIHM,LFACT),
                  first_only_to_upper_f)) 
  
  pumf_1981
}

str_to_title_f<- function(x) {
  xx<-str_to_title(x)
  if (is.factor(x)) {
    xl=levels(x) |> str_to_title()
    xx<-factor(xx,levels=xl)
  }
  xx
}

first_only_to_upper_f <- function(x) {
  xx<-tolower(x)
  substr(xx, 1, 1) <- toupper(substr(xx, 1, 1))
  if (is.factor(x)) {
    xl=levels(x) |> tolower()
    substr(xl, 1, 1) <- toupper(substr(xl, 1, 1))
    xx<-factor(xx,levels=xl)
  }
  xx
}



get_pumf_1976 <- function(refresh=FALSE) {
  hhrel_recodes <- c("0"=  "Not applicable",
                     "1"=  "Primary 2 par + no kids/others",
                     "2"=  "Primary 2 par + others/no kids",
                     "3"=  "Primary 2 par + kids/no others",
                     "4"=  "Primary 2 par + kids + others",
                     "5"=  "Primary 1 parent + no others",
                     "6"=  "Primary 1 parent + others",
                     "7"=  "Secondary 2 parent + no kids",
                     "8"=  "Secondary 2 parent + kids",
                     "9"=  "Secondary 1 parent",
                     "10"=  "Multiple family",
                     "11"=  "Non-family - 1 person",
                     "12"=  "Non-family - 2+ persons")
  
  d_h<-get_pumf("Census","1976 (households)",refresh_layout =refresh) |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(CMA=recode(CMA,"0"="Not applicable",
                      "462"="Montréal",
                      "535"="Toronto",
                      "933"="Vancouver"))
  
  d_i<-get_pumf("Census","1976 (individuals)",refresh_layout =refresh) |>
    label_pumf_data(rename_columns = FALSE) |>
    mutate(CMA=recode(CMA,"NOT APPLICABLE"="Otehr",
                      "MONTREAL CMA"="Montréal",
                      "TORONTO CMA"="Toronto",
                      "VANCOUVER CMA"="Vancouver"))
  d_h
}


get_pumf_1971 <- function(refresh=FALSE,type="individuals") {
  hhrel_recodes <- c("0"=  "Not applicable",
                     "1"=  "Primary 2 par + no kids/others",
                     "2"=  "Primary 2 par + others/no kids",
                     "3"=  "Primary 2 par + kids/no others",
                     "4"=  "Primary 2 par + kids + others",
                     "5"=  "Primary 1 parent + no others",
                     "6"=  "Primary 1 parent + others",
                     "7"=  "Secondary 2 parent + no kids",
                     "8"=  "Secondary 2 parent + kids",
                     "9"=  "Secondary 1 parent",
                     "10"=  "Multiple family",
                     "11"=  "Non-family - 1 person",
                     "12"=  "Non-family - 2+ persons")
  
  all_ages <- seq(0,85,5)
  all_age_lables <- paste0(all_ages," to ",all_ages+4," years") |> gsub("85 to 89 years","85 years and over",x=_)
  
  if (type=="individuals") {
    d<-get_pumf("Census","1971",refresh_layout =refresh) |>
      label_pumf_data(rename_columns = FALSE) |>
      rename(CMA=CMACODE,ATTSCH=ATTEND,HHCLASS=HHLDCLAS) |>
      mutate(across(c(CMA),str_to_title_f),
             across(c(HHCLASS,LFCODE1,ATTSCH,SEX,MOVES,HHLDREL),
                    first_only_to_upper_f)) |>
      mutate(PRIHM=case_when(grepl("Head",HHLDREL)~"Primary household maintianer",
                             TRUE ~ "Not primary household maintainer")) |>
      mutate(TOTINC=as.numeric(na_if(INCTOTAL,"NO INCOME/NOT APLICABLE"))) |>
      mutate(age=as.numeric(AGE)) |>
      mutate(AGEGRP=cut(age,c(all_ages,Inf),
                        labels=all_age_lables,
                        right = FALSE)) |>
      mutate(CMA=fct_recode(CMA,"Montréal"="Montreal"))
  } else if (type=="households") {
    d<-get_pumf("Census","1971 (households)",refresh_layout =refresh) |>
      label_pumf_data(rename_columns = FALSE) |>
      rename(CMA=CMACODE,HHSIZE=PERSHH,CFSIZE=PERSFAM,NCFSIZE=PERSNFAM,TENUR=TENURE,VALUE=DWLVALUE,GROSRT=GRSRENT,
             AGE=AGEHD,MARSTH=MARHD,SEX=SEXHD) |>
      mutate(across(c(CMA),str_to_title_f),
             across(c(HHTYPE,TENUR,CFSIZE,NCFSIZE,HHSIZE,SEX,BEDROOMS,LODGERS,MARSTH),
                    first_only_to_upper_f))
  } else if (type=="families") {
    d<-get_pumf("Census","1971 (families)",refresh_layout =refresh) |>
      label_pumf_data(rename_columns = FALSE) |>
      rename(CMA=CMACODE,CFSTAT=FAMSTAT,AGE=AGEHD,MARSTH=MARHD,SEX=SEXHD,CFINC=INCFAM,TENUR=TENURE,DTYPE=TYPEDWL) |>
      mutate(across(c(CMA),str_to_title_f),
             across(c(HHCLASS,HCFSTAT,FAMTYPE,FAMPERS,AGE,SEX,MARSTH,TENUR,DTYPE),
                    first_only_to_upper_f))
  }
  d
}


read_old_census_rents <- function() {
  d<-read_csv(here::here("data/Census_Housing_1931-1971_CSV.csv")) |>
    mutate(Year=as.character(Year))
  
  names(d) <- d |> 
    names() |>
    gsub("Under_","Under ",x=_) |>
    gsub("under_","Under ",x=_) |> 
    gsub("_Rent_$","_Rent_Total ",x=_) |>
    gsub("_or_more"," or more",x=_) |>
    gsub("Values_","Value_",x=_) 
  
  d |> select(Year,Geography,Place,matches("Rent")) |>
    pivot_longer(cols=matches("^(\\d{4})_(Rent)_(.+)$"),names_to=c("Year2","Metric","Range"),
                 values_to="value",names_pattern=c("(\\d{4})_(Rent)_(.+)")) |>
    filter(Year==Year2) |>
    select(-Year2)|>
    filter(!is.na(value)) |>
    mutate(Value_lower=ifelse(grepl("^\\$",Range), str_extract(Range,"^\\$\\d+") |> gsub("\\$","",x=_) |> as.integer(),0)) |>
    mutate(Place=recode(Place,Ottawa="Ottawa – Gatineau",
                      "Montreal"="Montréal","Quebec"="Québec"))
}

read_old_census_housing_data <- function() {
  d<-read_csv(here::here("data/Census_Housing_1931-1971_CSV.csv")) |>
    mutate(Year=as.character(Year))
  
  names(d) <- d |> 
    names() |>
    gsub("Under_","Under ",x=_) |>
    gsub("under_","Under ",x=_) |> 
    gsub("_Rent_$","_Rent_Total ",x=_) |>
    gsub("_or_more"," or more",x=_) |>
    gsub("Values_","Value_",x=_) 
  
  d_rent <- d |> select(Year,Geography,Place,matches("Rent")) |>
    pivot_longer(cols=matches("^(\\d{4})_(Rent)_(.+)$"),names_to=c("Year2","Metric","Range"),
                 values_to="value",names_pattern=c("(\\d{4})_(Rent)_(.+)")) |>
    filter(Year==Year2) |>
    select(-Year2)|>
    filter(!is.na(value)) |>
    mutate(Value_lower=ifelse(grepl("^\\$",Range), str_extract(Range,"^\\$\\d+") |> gsub("\\$","",x=_) |> as.integer(),0))
  
  
  if (FALSE) {
    d_rent_cleaned <- d_rent |> 
      filter(!grepl("TOTAL|Median|Average|No_Cash|Total|unspec|Not_Specified",Range),
             Range!="$0") |>
      arrange(Year,Place,Value_lower) |>
      summarize(Rent=bracket_median(Value_lower,value),
                Rent_q1=bracket_median(Value_lower,value,quantile=0.25),
                .by=c(Place,Geography,Year)) |>
      left_join(cpi,by="Year") |>
      rename(CMA=Place) |> 
      mutate(`Real rent`=Rent/CPI,
             `Real Q1 rent`=Rent_q1/CPI) 
      
 
    d_rent_cleaned |> 
      bind_rows(cma_rents) |> 
      filter(CMA %in% focus_cmas) |> 
      ggplot(aes(x=Year,y=`Real rent`,colour=CMA,group=CMA)) + 
      geom_point(shape=21) +
      geom_line() + 
      scale_y_continuous(labels=scales::dollar,trans="log",breaks=seq(100,2000,100)) + 
      scale_colour_manual(values=cma_colours)
  }
 
  d_value <- d |> select(Year,Geography,Place,matches("Value_")) |>
    pivot_longer(cols=matches("^(\\d{4})_(Value)_(.+)$"),names_to=c("Year2","Metric","Range"),
                 values_to="value",names_pattern=c("(\\d{4})_(Value)_(.+)")) |>
    filter(Year==Year2) |>
    select(-Year2) |>
    mutate(value=coalesce(value,0)) |>
    mutate(Value_lower=ifelse(grepl("^\\$",Range), str_extract(Range,"^\\$\\d+") |> gsub("\\$","",x=_) |> as.integer(),0))
  
  d_pop <- d |> select(Year,Geography,Place,matches("Pop")) |>
    pivot_longer(cols=matches("^(\\d{4})_(Pop)_(.+)$"),names_to=c("Year2","Metric","Range"),
                 values_to="value",names_pattern=c("(\\d{4})_(.+)_(.+)")) |>
    filter(Year==Year2) |>
    select(-Year2)
  
  
  d_households <-  d |> select(Year,Geography,Place,matches("Households")) |>
    pivot_longer(cols=matches("^(\\d{4})_(Households)_(.+)$"),names_to=c("Year2","Metric","Range"),
                 values_to="value",names_pattern=c("(\\d{4})_(Households)_(.+)")) |>
    filter(Year==Year2) |>
    select(-Year2)

  d_heads <-  d |> select(Year,Geography,Place,matches("Heads")) |>
    pivot_longer(cols=matches("^(\\d{4})_(Heads)_(.+)$"),names_to=c("Year2","Metric","Range"),
                 values_to="value",names_pattern=c("(\\d{4})_(Heads)_(.+)")) |>
    filter(Year==Year2) |>
    select(-Year2)
  
  
  bind_rows(d_rent,d_value,d_pop,d_households,d_heads) 
}




get_bedroom_data <- function(){
  pumf_2006 <- get_pumf("Census","2006 (hierarchical)") |> 
    label_pumf_data(rename_columns = FALSE) |>
    mutate(Year="2006")
  brm_2006 <- pumf_2006 |>
    filter(!is.na(BROOMH),!duplicated(HH_ID)) |>
    summarize(Count=sum(WEIGHT),.by=c(Year,CMA,BROOMH)) |>
    mutate(Share=Count/sum(Count),.by=c(Year,CMA)) |>
    rename(BEDRM=BROOMH) 
  
  pumf_2001 <- get_pumf("Census","2001 (households)") |> 
    label_pumf_data(rename_columns = FALSE) |>
    mutate(Year="2001") |>
    mutate(WEIGHTH=as.numeric(WEIGHTH))
  brm_2001 <- pumf_2001 |>
    filter(!is.na(BROOMH)) |>
    summarize(Count=sum(WEIGHTH),.by=c(Year,CMAH,BROOMH)) |>
    mutate(Share=Count/sum(Count),.by=c(Year,CMAH)) |>
    rename(BEDRM=BROOMH,CMA=CMAH) 
  
  pumf_1996 <- get_pumf("Census","1996 (households)") |> 
    label_pumf_data(rename_columns = FALSE) |>
    mutate(Year="1996") |>
    mutate(WEIGHTH=as.numeric(WEIGHTH))
  brm_1996 <- pumf_1996 |>
    filter(!is.na(BROOMH)) |>
    summarize(Count=sum(WEIGHTH),.by=c(Year,CMAPUMFH,BROOMH)) |>
    mutate(Share=Count/sum(Count),.by=c(Year,CMAPUMFH)) |>
    rename(BEDRM=BROOMH,CMA=CMAPUMFH) 
  
  pumf_1991 <- get_pumf("Census","1991 (households)") |> 
    label_pumf_data(rename_columns = FALSE) |>
    mutate(Year="1991") |>
    mutate(WEIGHTH=as.numeric(WEIGHTH))
  brm_1991 <- pumf_1991 |>
    filter(!is.na(BROOMH)) |>
    summarize(Count=sum(WEIGHTH),.by=c(Year,CMAPUMFH,BROOMH)) |>
    mutate(Share=Count/sum(Count),.by=c(Year,CMAPUMFH)) |>
    rename(BEDRM=BROOMH,CMA=CMAPUMFH) 
  
  bind_rows(brm_2006,brm_2001,brm_1996,brm_1991) |>
    mutate(CMA=harmonize_cma(CMA))
}


recode_dtype <- function(dtype){
  dtype=case_when(grepl("private dwellings|Total",dtype) ~ "Total",
                       grepl("Single",dtype) ~ "Single",
                       grepl("Semi",dtype) ~ "Semi",
                       grepl("duplex",dtype) ~ "Duplex",
                       grepl("fewer|less",dtype) ~ "<5 storey apartment",
                       grepl("more",dtype) ~ "5+ storey apartment",
                       grepl("Row",dtype) ~ "Row",
                       grepl("attached house",dtype) ~ "Other single-attached",
                       grepl("Movable",dtype) ~ "Movable",
                       TRUE ~ "Other") 
}

get_dwelling_data <- function(dataset,region){
  vars <- find_census_vectors("Structural type",dataset) %>%
    mutate(l=recode_dtype(label)) %>%
    filter(l!="Other")
  
  stopifnot(nrow(vars)==9)
  
  dw_types <- c("Single","Duplex","Semi","Row","<5 storey apartment","5+ storey apartment",
                "Other single-attached","Movable")
  get_census(dataset,regions=region,vectors=setNames(vars$vector,vars$l),quiet = TRUE) %>%
    pivot_longer(any_of(dw_types),names_to = "DT") %>%
    mutate(DT=factor(DT,levels=dw_types))
}


remove_trailing_notes <- function(data){
  note <- which(data$Characteristic=="Note")
  data |>
    slice(-seq(note,nrow(data)))
}

read_census_data <- function(url,quiet=FALSE,encoding = "Windows-1252") {
  top <- suppressWarnings(read_csv(url,n_max=5,col_names=c("Characteristic","Value"),
                                   col_types = cols(Characteristic="c",Value="d"), locale = locale(encoding = encoding)))
  start <- which(grepl("^Geography = ",top$Characteristic))
  geography=top$Characteristic[start] |> gsub("^Geography = ","",x=_)
  if (!quiet) message(paste0("Reading data for ",geography))
  suppressWarnings(read_csv(url,skip=start,
                            col_names=c("Characteristic","Value"),
                            col_types = cols(Characteristic="c",Value="d"))) |>
    remove_trailing_notes()
}



get_geo_table <- function(year,level="CSD"){
  year=as.character(year)
  path <- file.path(tempdir(),paste0("census_geo_table_",year,"_",level,".Rda"))
  if (!file.exists(path)) {
    if (level=="CSD") {
      url_geo_table <-list(`1981`="https://www12.statcan.gc.ca/English/census81/data/tables/Geo-index-eng.cfm?TABID=5&LANG=E&APATH=3&DETAIL=1&DIM=0&FL=A&FREE=1&GC=0&GID=1375808&GK=0&GRP=1&PID=113751&PRID=0&PTYPE=113743&S=0&SHOWALL=No&SUB=0&Temporal=1986&THEME=134&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0",
                           `1986` = "https://www12.statcan.gc.ca/English/census86/data/tables/Geo-index-eng.cfm?TABID=5&LANG=E&APATH=3&DETAIL=1&DIM=0&FL=A&FREE=1&GC=0&GID=1362262&GK=0&GRP=1&PID=113685&PRID=0&PTYPE=113679&S=0&SHOWALL=No&SUB=0&Temporal=1986&THEME=133&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0",
                           `1991` = "https://www12.statcan.gc.ca/English/census91/data/profiles/Geo-index-eng.cfm?TABID=5&LANG=E&APATH=3&DETAIL=1&DIM=0&FL=A&FREE=1&GC=0&GID=3478&GK=0&GRP=1&PID=30&PRID=0&PTYPE=3&S=0&SHOWALL=No&SUB=0&Temporal=1991&THEME=113&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0")
    } else if (level=="CMA") {
      url_geo_table <-list(`1981`="https://www12.statcan.gc.ca/English/census81/data/tables/Geo-index-eng.cfm?TABID=5&LANG=E&APATH=3&DETAIL=1&DIM=0&FL=A&FREE=1&GC=0&GID=0&GK=0&GRP=1&PID=113749&PRID=0&PTYPE=113743&S=0&SHOWALL=No&SUB=0&Temporal=1986&THEME=134&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0",
                           `1986` = "https://www12.statcan.gc.ca/English/census86/data/tables/Geo-index-eng.cfm?TABID=5&LANG=E&APATH=3&DETAIL=1&DIM=0&FL=A&FREE=1&GC=0&GID=0&GK=0&GRP=1&PID=113686&PRID=0&PTYPE=113679&S=0&SHOWALL=No&SUB=0&Temporal=1986&THEME=133&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0",
                           `1991` = "https://www12.statcan.gc.ca/English/census91/data/profiles/Geo-index-eng.cfm?TABID=5&LANG=E&APATH=3&DETAIL=1&DIM=0&FL=A&FREE=1&GC=0&GID=0&GK=0&GRP=1&PID=234&PRID=0&PTYPE=3&S=0&SHOWALL=No&SUB=0&Temporal=1991&THEME=113&VID=0&VNAMEE=&VNAMEF=&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0")
    }
    
    url <- url_geo_table[[year]]
    ls <- rvest::read_html(url) |>
      rvest::html_element("body div#wb-main div#wb-main-container div#wb-main-column div.span-8 ol") |>
      rvest::html_elements("li a")
    
    t<-ls |> purrr::map_df(function(l){
      list(id=rvest::html_attr(l,"id"), 
           title=rvest::html_attr(l,"title"),
           name=rvest::html_text(l),
           href=rvest::html_attr(l,"href"))
    })
    if (grepl("\\(SCA\\d+\\)",t$title[1])) {
      t <- t |>
        filter(!grepl(" - \\d+",title)) |>
        mutate(GeoUID=gsub("\\(SCA","(",title)) |>
        mutate(GeoUID=str_extract(GeoUID,"\\(\\d+.*\\d*\\)") |> gsub("\\(|\\)","",x=_)) |>
        mutate(GeoUID=ifelse(nchar(GeoUID)==5,paste0(substr(GeoUID,4,5),substr(GeoUID,1,3)),GeoUID)) |>
        mutate(GeoUID=gsub("^00","",GeoUID))
    } else {
      t <- t |>
        mutate(GeoUID=str_extract(title,"\\(\\d+.*\\d*\\)") |> gsub("\\(|\\)","",x=_))
    }
    saveRDS(t,path)
  } else {
    t<-readRDS(path)
  }
  t
}


get_profile_data_for <- function(geo_uid,year,level="CSD",encoding = "Windows-1252",refresh=FALSE) {
  year=as.character(year)
  if (level=="CSD") {
    census_pid_a <- list(`1981`="113751",
                         `1986`="113684",
                         `1991`="29")
    census_pid_b <- list(`1981`="113752",
                         `1986`="113685",
                         `1991`="30")
  } else if (level=="CMA") {
    census_pid_a <- list(`1981`="113749",
                         `1986`="113686",
                         `1991`="234")
    census_pid_b <- list(`1981`="113750",
                         `1986`="113687",
                         `1991`="233")
  }
  if (inherits(geo_uid,"character")) {
    gs <- get_geo_table(year,level=level) |>
      filter(GeoUID==geo_uid)
  } else {
    gs <- geo_uid
  }
  stopifnot(nrow(gs)==1)
  
  path <- file.path(tempdir(),paste0("census_data_",year,"_",gs$GeoUID,".Rda"))
  if (refresh||!file.exists(path)) {
    url_base <- paste0("https://www12.statcan.gc.ca/English/census",substr(year,3,4),"/data/tables")
    url <- file.path(url_base,gs$href) |> paste0("#tab4")
    
    gid <- rvest::read_html(url) |>
      rvest::html_element("div#current-view ul li a") |> rvest::html_attr("href") |>
      str_extract(string=_,"GID=\\d+") |>
      gsub("GID=","",x=_)
    
    
    
    url_a <- paste0("https://www12.statcan.gc.ca/English/census",substr(year,3,4),"/data/tables/File.cfm?S=0&LANG=E&A=R&PID=",
                    census_pid_a[[year]],"&GID=",gid,"&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0&OFT=CSV")
    url_b <- paste0("https://www12.statcan.gc.ca/English/census",substr(year,3,4),"/data/tables/File.cfm?S=0&LANG=E&A=R&PID=",
                    census_pid_b[[year]],"&GID=",gid,"&D1=0&D2=0&D3=0&D4=0&D5=0&D6=0&OFT=CSV")
    
    d<-bind_rows(read_census_data(url_a,quiet = TRUE,encoding = encoding),
                 read_census_data(url_b,quiet = TRUE,encoding = encoding)) |>
      mutate(GeoUID=gs$GeoUID,Name=gs$name,Year=year)
    saveRDS(d,path)
  } else {
    d<-readRDS(path)
  }
  d
}

get_dtype_data <- function(){
  regions <- list_census_regions("2021") |>
    filter(level=="CMA") |>
    mutate(name=harmonize_cma(name)) |>
    filter(name %in% focus_cmas) |>
    arrange(-pop) 
  
  d<-seq(1996,2021,5) |>
    as.character() |>
    map_df(\(y)get_dwelling_data(y,as_census_region_list(regions)) |> mutate(Year=y)) |>
    mutate(CMA=`Region Name`) |>
    select(CMA,Year,DTYPE=DT,Value=value)
  
  g_1991 <- get_geo_table("1991","CMA") |>
    mutate(name=harmonize_cma(name)) |>
    filter(name %in% focus_cmas)
  
  d_1991 <- g_1991$GeoUID |>
    map_df(\(g)get_profile_data_for(g,"1991",level="CMA")) 
  
  index_0 <- which(grepl("Single-detached",d_1991$Characteristic))
  index_1 <- which(grepl("Movable dwelling",d_1991$Characteristic))
  
  chars_1991 <- d_1991$Characteristic[index_0[1]:index_1[1]]
  
  d_1991 <- d_1991 |>
    filter(Characteristic %in% chars_1991) |>
    mutate(DTYPE=recode_dtype(Characteristic),
           CMA=harmonize_cma(Name)) |>
    select(CMA,DTYPE,Year,Value)
    

  g_1986 <- get_geo_table("1986","CMA") |>
    mutate(name=harmonize_cma(name)) |>
    filter(name %in% focus_cmas)
  
  d_1986 <- g_1986$GeoUID |>
    map_df(\(g)get_profile_data_for(g,"1986",level="CMA")) 
  
  index_0 <- which(grepl("Single-detached",d_1986$Characteristic))
  index_1 <- which(grepl("All other types",d_1986$Characteristic))
  
  chars_1986 <- d_1986$Characteristic[index_0[1]:index_1[1]]
  
  d_1986 <- d_1986 |>
    filter(Characteristic %in% chars_1986) |>
    mutate(DTYPE=recode_dtype(Characteristic),
           CMA=harmonize_cma(Name)) |>
    select(CMA,DTYPE,Year,Value)
  
  g_1981 <- get_geo_table("1981","CMA") |>
    mutate(name=harmonize_cma(name)) |>
    filter(name %in% focus_cmas)
  
  d_1981 <- g_1981$GeoUID |>
    map_df(\(g)get_profile_data_for(g,"1981",level="CMA")) 
  d_1981$Characteristic <- iconv(d_1981$Characteristic, from = "ISO-8859-1", to = "UTF-8")
  
  index_0 <- which(grepl("Single detached",d_1981$Characteristic))
  index_1 <- which(grepl("Duplex",d_1981$Characteristic))
  index_2 <- which(grepl("Other multiple dwellings",d_1981$Characteristic))
  
  chars_1981 <- d_1981$Characteristic[setdiff(index_0[1]:index_1[1],index_2[1])]
  
  d_1981 <- d_1981 |>
    filter(Characteristic %in% chars_1981) |>
    mutate(DTYPE=recode_dtype(Characteristic),
           CMA=harmonize_cma(Name)) |>
    select(CMA,DTYPE,Year,Value)
  
  
  
   
  bind_rows(d_1981,d_1986,d_1991,d) 
}



