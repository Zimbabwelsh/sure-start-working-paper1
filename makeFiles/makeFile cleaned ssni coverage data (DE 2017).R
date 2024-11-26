## This is to clean and QA the NI coverage data 
# notes reading in the data 

library(tidyverse)
library(rJava)
library(xlsx)

# input/ output -----------------------------------------------------------

## Sure Start data

## The raw file from ni gov is in worksheet 1 and i reformatted 
ssni_df <-
  'cleaned data/Development of Sure Start Coverage Table.XLSX' %>%
  read.xlsx(2)

nimdm2010_df <- 
  readxl::read_xls('data/NIMDM 2010 soa.xls', sheet = 2)

## we get the code data from the nimdm
# output data -------------------------------------------------------------

ss_2017_df <-
  nimdm2010_df %>%
  left_join(
    ssni_df, by = c(`SOA NAME` = 'soa_nimd')
  )

ss_2017_df
ss_2017_df
nimdm2010_df <-
  nimdm2010_df %>%
  transmute(
    soa_code = `SOA CODE`,
    soa_name = `SOA NAME`,
    mdm_score_2010 = `Multiple Deprivation Measure score`,
    k_nimdm2010 = `Multiple Deprivation Measure score` - quantile(`Multiple Deprivation Measure score`, c(0.8))
  )


# output data -------------------------------------------------------------

## Join the name to get the code
## replace na phase (unjoined) with phase = 5
ss_2017_df <-
  ss_2017_df %>%
  replace_na(
    list(Phase.included = 5)
  )


ss_2017_df <-
  ss_2017_df %>%
  transmute(
#    soa_name = `SOA NAME`,
    soa_code = `SOA CODE`,
    phase_included_de = Phase.included, ## This data has issues
    in_ss_2017 = Phase.included < 4,
    in_ss_2021 = Phase.included < 5 
  )

## ss_2017_df$in_ss_2021 %>% summary ## check all is there 
# ss_2017_df$phase_included_de %>% table


### output
ss_2017_df %>% 
  write_csv('outputs/cleaned ssni coverage (2017).csv')






