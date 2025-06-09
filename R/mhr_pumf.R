get_phm_timeline <- function() {
  
  classify_age <- function(a){
    aa=a |>
      str_extract("\\d+") |>
      as.integer() |> 
      pretty_cut(breaks=c(0,5,10,15,18,seq(20,85,5),Inf),
                 right=FALSE) |>
      str_extract("\\d+") |> 
      as.integer()
    
    case_when(is.na(aa) ~ "Not available",
              aa<5 ~ "0 to 4 years",
              aa==15 ~ "15 to 17 years",
              aa==18 ~ "18 to 19 years",
              aa>=85 ~ "85 years and over",
              TRUE ~ paste0(aa," to ",aa+4," years"))
  }
  
  harmonize_pr <- function(pr) {
    provinces <- c("Newfoundland and Labrador",
                   "Prince Edward Island",
                   "Nova Scotia",
                   "New Brunswick",
                   "Quebec",
                   "Ontario",
                   "Manitoba",
                   "Saskatchewan",
                   "Alberta",
                   "British Columbia",
                   "Northern Canada")
    recode(pr,!!!setNames(provinces,toupper(provinces))) |>
      recode("NEWFOUNDLAND"="Newfoundland and Labrador",
             "Newfoundland"="Newfoundland and Labrador",
             "PEI/YUKON/NWT"="Prince Edward Island, Yukon Territory, Northwest Territories and Nunavut",
             "Northern Canada"="Yukon Territory, Northwest Territories and Nunavut",
             "Yukon/Northwest Territories"="Yukon Territory, Northwest Territories and Nunavut",
             "Yukon and Northwest"="Yukon Territory, Northwest Territories and Nunavut",
             "Yukon & Northwest Territories"="Yukon Territory, Northwest Territories and Nunavut")
  }
  
  harmonize_cma <- function(cma) {
    cma |>
      gsub(" - \\d{3}","",x=_) |>
      recode("Other census metropolitan areas, Census Agglomerations and other geographies"="Other",
             "Other census metropolitan areas, census agglomerations and other geographies"="Other",
             "Not Applicable"="Other",
             "Not applicable"="Other",
             "NOT APPLICABLE"="Other",
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
             "Regina and Saskatoon"="Regina – Saskatoon",
             "QUEBEC"="Québec",
             "ST. CATHARINES-NIAGARA"="St. Catharines – Niagara",
             "TORONTO"="Toronto",
             "VANCOUVER"="Vancouver",
             "MONTREAL"="Montréal",
             "WINNIPEG"="Winnipeg",
             "KITCHENER"="Kitchener",
             "LONDON"="London",
             "HALIFAX"="Halifax",
             "OTTAWA-HULL"="Ottawa – Gatineau",
             "TORONTO CMA"="Toronto",
             "VANCOUVER CMA"="Vancouver",
             "MONTREAL CMA"="Montréal",
             "HAMILTON" = "Hamilton",
             "EDMONTON" = "Edmonton",
             "CALGARY" = "Calgary"
      )
  }
  
  data <- seq(1971,2021,5) |>
    as.character() |>
    map_df(\(year) {
      # print(year)
      d<-get_pumf("Census",year) |>
        label_pumf_data(rename_columns = FALSE) 
      
      if (year=="1971") {
        d_cma <- d |>
          rename(CMA=CMACODE) |>
          rename(PR=GEOCODE) |>
          filter(HHLDCLAS=="PRIVATE HOUSEHOLD") |>
          mutate(PRIHM= HHLDREL=="HEAD OF HOUSEHOLD") |>
          mutate(AGEGRP=classify_age(AGE)) |>
          summarize(Count=n()*50,.by=c(CMA,PR,AGEGRP,PRIHM))
        
        d_pr<-get_pumf("Census","1971 (individuals) PR") |>
          label_pumf_data(rename_columns = FALSE) |>
          rename(CMA=CMACODE) |>
          rename(PR=GEOCODE) |>
          filter(HHLDCLAS=="PRIVATE HOUSEHOLD") |>
          mutate(PRIHM= HHLDREL=="HEAD OF HOUSEHOLD") |>
          mutate(AGEGRP=classify_age(AGE)) |>
          summarize(Count=n()*50,.by=c(CMA,PR,AGEGRP,PRIHM)) 
        
       d <- bind_rows(d_cma |> mutate(Count=-Count),d_pr) |>
         mutate(CMA="Other") |>
         summarize(Count=sum(Count),.by=c(CMA,PR,AGEGRP,PRIHM)) |>
         bind_rows(d_cma)
        
      } else if (year=="1976") {
        d <- d |>
          filter(HHLDCLAS=="PRIVATE HOUSEHOLD") |>
          rename(PR=PROV) |>
          mutate(PRIHM= HHDLREL=="HEAD OF HOUSEHOLD") |>
          mutate(AGEGRP=classify_age(AGE)) |>
          summarize(Count=n()*50,.by=c(CMA,PR,AGEGRP,PRIHM))
      }  else if (year=="1981") {
        d <- d |>
          filter(HHCLASS=="PRIVATE") |>
          filter(HMAIN!="NOT APPLICABLE") |>
          rename(PR=PROV) |>
          mutate(PRIHM= HMAIN=="YES") |>
          mutate(AGEGRP=classify_age(AGE)) |>
          summarize(Count=n()*50,.by=c(CMA,PR,AGEGRP,PRIHM))
      }  else if (year=="1986") {
        d <- d |>
          rename(CMA=CMAPUST) |>
          filter(HHCLASS=="Private household") |>
          filter(HMAINP!="Not applicable") |>
          rename(PR=PROV) |>
          mutate(PRIHM= HMAINP=="Yes") |>
          mutate(AGEGRP=classify_age(AGEP)) |>
          summarize(Count=n()*50,.by=c(CMA,PR,AGEGRP,PRIHM))
      }  else if (year=="1991") {
        d <- d |>
          rename(CMA=CMAPUMFP) |>
          filter(HHCLASSP=="Private household") |>
          filter(PRMAINP!="Not applicable") |>
          rename(PR=PROVP) |>
          mutate(PRIHM= PRMAINP=="Primary household maintainer") |>
          mutate(AGEGRP=classify_age(AGEP)) |>
          summarize(Count=sum(WEIGHTP),.by=c(CMA,PR,AGEGRP,PRIHM))
      }  else if (year=="1996") {
        d <- d |>
          rename(CMA=CMAPUMFP) |>
          filter(HHCLASSP=="Private Household") |>
          filter(PRMAINP!="Not applicable") |>
          rename(PR=PROVP) |>
          mutate(PRIHM= PRMAINP=="PrimaryHhldMaintainr") |>
          mutate(AGEGRP=classify_age(AGEP)) |>
          summarize(Count=sum(WEIGHTP),.by=c(CMA,PR,AGEGRP,PRIHM))
      }  else if (year=="2001") {
        d <- d |>
          rename(PR=PROVP) |>
          rename(CMA=CMAP) |>
          filter(HHCLASSP=="Private household") |>
          filter(PRMAINP!="Not applicable") |>
          mutate(PRIHM= PRMAINP=="Primary household maintainer") |>
          mutate(AGEGRP=classify_age(AGEP)) |>
          summarize(Count=sum(WEIGHTP),.by=c(CMA,PR,AGEGRP,PRIHM))
      }  else if (year=="2006") {
        d <- d |>
          filter(HHCLASS=="Private household") |>
          filter(PRIHM!="Not applicable") |>
          mutate(PRIHM= PRIHM=="Person is primary maintainer") |>
          summarize(Count=sum(WEIGHT),.by=c(CMA,PR,AGEGRP,PRIHM))
      } else {
        d <- d |>
          filter(PRIHM!="Not applicable") |>
          mutate(PRIHM= PRIHM=="Person is primary maintainer") |>
          summarize(Count=sum(WEIGHT),.by=c(CMA,PR,AGEGRP,PRIHM))
      }
      d |>
        mutate(Year=year)
    }) |>
    mutate(CMA=harmonize_cma(CMA)) |>
    mutate(PR=harmonize_pr(PR)) 
  
  data |>
    mutate(AGEGRP=recode(AGEGRP,
                         "5 to 6 years"="5 to 9 years",
                         "7 to 9 years"="5 to 9 years",
                         "10 to 11 years"="10 to 14 years",
                         "12 to 14 years"="10 to 14 years",
                         "85 years and over"="85 years ond over")) |>
    summarize(Count=sum(Count),.by=c(CMA,PR,AGEGRP,PRIHM,Year)) 
}
