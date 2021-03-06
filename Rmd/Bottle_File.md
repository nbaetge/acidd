Bottle File
================
Nicholas Baetge
2/6/2020

# Intro

This document shows how the shared [ACIDD bottle
file](https://docs.google.com/spreadsheets/d/17l6UIFdWFT3ib3kXXG61SPfuYnKMAT5Tjv-nafnL0lI/edit#gid=103152813)
was processed prior to analysis.

  - Profiles are first collapsed, such that there are one set of values
    for each depth sampled.
  - Depths of 0 m, MLD (estimated from high resolution (\< 1 m bins) CTD
    data) are added to each profile. Data are interpolated for those
    depths (0 - 5 m are assumed to be the
same).

# Import Data

``` r
google.df <- read_sheet("https://docs.google.com/spreadsheets/d/17l6UIFdWFT3ib3kXXG61SPfuYnKMAT5Tjv-nafnL0lI/edit#gid=103152813", sheet = "Bottle File", skip = 0) 

saveRDS(google.df, "~/GITHUB/acidd/Input/bottlefile.rds")

google.df.headers <- read_sheet("https://docs.google.com/spreadsheets/d/17l6UIFdWFT3ib3kXXG61SPfuYnKMAT5Tjv-nafnL0lI/edit#gid=103152813", sheet = "Bottle File Headers", skip = 0) 

saveRDS(google.df.headers, "~/GITHUB/acidd/Input/bottlefile_headers.rds")
```

# Tidy Data

``` r
#Parse out time
ctd.df <- google.df %>% 
  mutate(Time_Stamp = gsub("T", "_", Time_Stamp),
         Time_Stamp = gsub(":", "-", Time_Stamp),
         Date = as.Date(Time_Stamp),
         datetime = ymd_hm(Time_Stamp, tz = "UTC"),
         decimaldate = decimal_date(datetime)) %>% 
  select(Cruise, Station, Type, Date, datetime, decimaldate, Latitude:DCM, Leg, everything()) %>% 
  filter(`Bottle Trip` != "No") %>% 
  #we'll omit the MLD and DCM columns as those were eyeballed by the CTD operators
  select(-c(Time_Stamp, `Bottle Trip`, MLD, DCM)) %>% 
  rename(Bact_DNA_ID = `16S_ID`) %>% 
  #BactAbund is in E5 cells per ml so we will convert to cells/l
  mutate(BactAbund = BactAbund * 10^8,
         BactAbund_sd = BactAbund_sd * 10^8) %>% 
  #The cruise was structured such that cast 1 of every station was a large volume cast (except for the transit stations). Since hydropgraphic samples weren't taken from these casts, we'll omit them from further analysis 
  mutate(omit = ifelse(!Leg == "T" & SCN == 1, T, F)) %>% 
  filter(!omit == T) %>% 
  select(-omit) %>% 
  #we'll also remove Station 18 as that was a failed station
  filter(!Station == 18)
  

#subset DNA_ID for later merge
dna <- ctd.df %>% 
  select(Cruise, Station, CruiseCN, Target_Z, Bact_DNA_ID) %>% 
  drop_na(Bact_DNA_ID)

#import MLD data
mld.df <- read_rds("~/GITHUB/acidd/Output/mld.rds")
```

# Collapse Profiles

``` r
#Split data frame into a list based on station
cast.list <- split(ctd.df, ctd.df$Station)

#create a function to collapse the entire cast list 
#the end product: unique column values for the same depth but sampled from different niskins are combined into a single row. 
#what is happening here is:
#for every depth that has more than one niskin associated with it, a mean is reported for each variable for that depth (disregarding NA values)
#this works for this dataset as there are never two measurements of a variable taken from the same depth of the same cast, but from different bottles
collapse.func <- function(casper){
  meta.df <- casper %>% 
    select(Cruise:SCN, Target_Z, -Niskin, -Bact_DNA_ID) %>% 
    unique(.)
  data.df <- casper %>% 
    select(-c(Cruise:SCN, Niskin), -contains("QF"), -Bact_DNA_ID) %>% 
    aggregate(., by = list(.$Target_Z), mean, na.rm = T)
  collapsedcast.df <- meta.df %>% 
    left_join(., data.df, by = "Target_Z")
}

#run the cruise list through the function 
collapsed.list <- lapply(cast.list, collapse.func)

#convert the list into a dataframe and add mlds
collapsed.df <- data.frame(rbindlist(collapsed.list)) %>% 
  select(-Group.1) %>% 
  left_join(., mld.df) %>% 
  select(Cruise:SCN, mld, everything()) %>% 
  mutate_at(vars(mld), round) %>%
  mutate_at(vars(Leg), as.character)
```

# Interpolate Data for 0 m and MLD

``` r
#split the df by Station
add_mld.list <- split(collapsed.df, collapsed.df$Station)

#create a function to add an empty row to each cast, then add the max MLD to the Target Z column 
add.func <- function(rick){
 
  rick[nrow(rick) + 1,] <- NA
  rick$Target_Z[is.na(rick$Target_Z)] <- max(rick$mld, na.rm = T)
   schwifty <- rick %>% 
     fill(., Cruise:mld, .direction = c("updown")) %>%
     arrange(Station, Target_Z)
  
   schwifty[nrow(schwifty) + 1,] <- NA
   schwifty$Target_Z[is.na(schwifty$Target_Z)] <- 0
   summer <- schwifty %>% 
     fill(., Cruise:mld, .direction = c("updown")) %>%
     arrange(Station, Target_Z)
}

#apply function to list 
added_mld.list <- lapply(add_mld.list, add.func)


#save the list as a data frame 
added_mld.df <- plyr::ldply(added_mld.list, data.frame) %>% 
  group_by(Station) %>% 
  distinct(., Target_Z, .keep_all = T) %>% 
  select(-.id) %>% 
  ungroup() 


#split the data frame into lists of stations
to_interpolate.list <- split(added_mld.df, added_mld.df$Station)

#create a function that will linearly interpolate each VOI according to the depth intervals of the casts 
interpolate.func <- function(copper) {
to_interpolate.df <- copper %>% 
  select(Target_Z:ncol(.)) %>% 
  zoo(., order.by = .$Target_Z) 
interp_Temp <- as.numeric(na.approx(to_interpolate.df$Temperature, na.rm = F))
interp_Sal <- as.numeric(na.approx(to_interpolate.df$Sal11, na.rm = F))
interp_POC <- as.numeric(na.approx(to_interpolate.df$POC, na.rm = F))
interp_PON <- as.numeric(na.approx(to_interpolate.df$PON, na.rm = F))
#interp_DOC <- as.numeric(na.approx(to_interpolate.df$DOC, na.rm = F))
#interp_TDN <- as.numeric(na.approx(to_interpolate.df$TDN, na.rm = F))
interp_NH4 <- as.numeric(na.approx(to_interpolate.df$NH4, na.rm = F))
interp_PO4 <- as.numeric(na.approx(to_interpolate.df$PO4, na.rm = F))
interp_SiO4 <- as.numeric(na.approx(to_interpolate.df$SiO4, na.rm = F))
interp_NO2_NO3 <- as.numeric(na.approx(to_interpolate.df$NO2_NO3, na.rm = F))
interp_NO2 <- as.numeric(na.approx(to_interpolate.df$NO2, na.rm = F))
interp_BactAbund <- as.numeric(na.approx(to_interpolate.df$BactAbund, na.rm = F))
Target_Z <- to_interpolate.df$Target_Z
interpolations.df <- data.frame(Target_Z, interp_Temp, interp_Sal, 
                                interp_POC, interp_PON, 
                                #interp_DOC,interp_TDN, 
                                interp_NH4,interp_PO4, interp_SiO4,
                                interp_NO2_NO3, interp_NO2,
                                interp_BactAbund)
}

#apply function to list 
interpolations.list <- lapply(to_interpolate.list, interpolate.func)

#save the list as a data frame 
interpolations.df <- plyr::ldply(interpolations.list, data.frame) %>% 
  rename(., Station = .id) %>% 
  group_by(Station) %>% 
  fill(interp_Temp:interp_BactAbund, .direction = "up") %>% 
  ungroup()

#combine the interpolated and non-interpolated data frames
interpolations.df$Station <- as.numeric(interpolations.df$Station)
interpolated.df <- left_join(collapsed.df, interpolations.df) 

#add DNA IDs back to dataframe

processed.df <- left_join(interpolated.df, dna)
```

# Save Dataset

``` r
saveRDS(processed.df,"~/GITHUB/acidd/Output/processed_bf.rds")
```
