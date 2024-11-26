## makefile01: make the sure start file and join with data on running variable (k)
## see notebook on ssni data

# notes reading in the data 

library(tidyverse)

# input/ output -----------------------------------------------------------

## Sure Start data

## The raw file from ni gov is in worksheet 1 and i reformatted 
# looks at the nimd
nimdm2010_df <- 
  readxl::read_xls('data/NIMDM 2010 soa.xls', sheet = 2)

# output data -------------------------------------------------------------

nimdm2010_df <-
  nimdm2010_df %>%
  mutate(
    k_nimdm2010 = `Multiple Deprivation Measure score` - quantile(`Multiple Deprivation Measure score`, c(0.75)),
    k_nimdm2010 = k_nimdm2010/sd(k_nimdm2010)
  ) %>%
  transmute(
    soa_code = `SOA CODE`,
    soa_name = `SOA NAME`,
    mdm_score_2010 = `Multiple Deprivation Measure score`,
    k_nimdm2010
  )

## output
nimdm2010_df %>%
  write_csv('outputs/cleaned nimdm 2010 scores (soa).csv')




  


