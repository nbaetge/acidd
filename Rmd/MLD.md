MLD
================
Nicholas Baetge
7/26/2020

# Intro

This document highlights how MLDs were calculated from the the
high-resolution CTD data collected on ACIDD

  - MLDs were calculated using the N<sup>2</sup> buoyancy frequency and
    are defined as the depth below 5 m at which N<sup>2</sup> \>
    |stdev(N<sup>2</sup>)|

# Import Data

Data for each cast is in a separate sheet of the “CTD\_Summary” excel
file.

``` r
ctd.data <- readxl::excel_sheets("Input/CTD_Summary.xlsx") 
ctd.data_headers <-  readxl::read_xlsx(path = "Input/CTD_Summary_Headers.xlsx")

ctd.data %>%   
  purrr::map(function(sheet){ # iterate through each sheet name
  readxl::read_xlsx(path = "Input/CTD_Summary.xlsx", sheet = sheet, skip = 351)
}) -> ctd.data_list # Assign to a list

names(ctd.data_list) <- ctd.data

#save the list
saveRDS(ctd.data_list, "~/GITHUB/acidd/Input/CTD_Summary_List.rds")
```

# Estimate MLDs

``` r
mld.func <- function(casper){
  colnames(casper) <- ctd.data_headers$Variable
  good_boy <- casper %>% 
    select(prDM, `sigma-t00`) %>% 
    arrange(prDM) %>% 
    filter(prDM > 1) %>% 
    mutate(N2 = swN2(pressure = prDM, sigmaTheta = `sigma-t00`)) %>% 
    #include only depths below 5 m and where  N2 is > abs(stdev(N2))
    filter(prDM > 5 & N2 > abs(sd(N2))) %>%  
    filter(prDM == min(prDM)) %>% #report the shallowest depth at which the above condition is met
    rename(MLD = prDM) %>% 
    select(MLD) %>%
    distinct()
}

#run the cruise list through the function 
mld.list <- lapply(ctd.data_list, mld.func)
mld_headers <- names(mld.list) 

#convert the list into a dataframe 
mld.df <- as_tibble(mld.list) %>% 
  t() %>%
  as_tibble() %>%
  mutate(id = mld_headers) %>% 
  rename(mld = V1) %>% 
  separate(id, c("Station", "SCN"), sep = "C") %>% 
  mutate(Station = gsub("S", "", Station)) %>% 
  mutate_at(vars(Station, SCN), as.numeric)
```

    ## Warning: The `x` argument of `as_tibble.matrix()` must have column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

# Save the Dataframe

``` r
saveRDS(mld.df, "~/GITHUB/acidd/Output/mld.rds")
```
