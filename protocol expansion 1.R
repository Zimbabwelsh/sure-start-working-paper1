## Protocol note: first expansion based on wards 


library(tidyverse)
library(purrr)

### Clear key global objects---------------------------------------

rm(root_out_folder, out_folder)

### assign new global objects -------------------------------------------------

analysis_nm <- 'expansion 1'

## Create folders

root_out_folder <- paste0('outputs/', Sys.Date()) 
out_folder <- root_out_folder %>% file.path(analysis_nm)

dir.create(root_out_folder)
dir.create(out_folder)



##---------------------------------------

nimdm2005_ward <- 
  read_csv('outputs/cleaned nimdm 2005 scores (wards).csv')
sure_start_2008 <- 
  'manual checks/national archive to nimdm wards (checked).csv' %>% read_csv()



# QA ----------------------------------------------------------------------

sure_start_2008 %>%
  filter(
    !(bestCode %in% nimdm2005_ward$ward_code)
  )
## all there 
## Distribution





## join the data  -----------------------------------------------------------
analysis_df <-
  nimdm2005_ward %>%
  mutate(
    k = k_nimdm2005,
    treated = ward_code %in% sure_start_2008$bestCode
  )


# source scripts ----------------------------------------------------------

## Run RDD checks 

## basic checks and checks the ITT on treated
source('protocol notes/source RDD checks.R')

## Can check sample size here 
k_histPlot
treated_plot_df







# outcomes ----------------------------------------------------------------

indicators_2017_df <- 
#  'outputs/indicators 2017.csv' %>% 
  'outputs/indicators 2017 (scaled).csv' %>% 
  read_csv(na = c('*','NA'))

indicators_2017_df %>% names

soa_ward_lookup_df <-
  'outputs/soa (2001) to ward (1992) lookup.csv' %>%
  read_csv

## All SOAs are wards or sub-division of ward exception for some wards in moyles
## since the outcome is at soa level we'll use that

## get rid of duplicated and use first SOA
analysis_df <-
  soa_ward_lookup_df %>%
  left_join(analysis_df)
  
analysis_df <-
  analysis_df %>%
  group_by(soa_code) %>%
  summarise(
    k = k %>% max
  )

## join indicators

analysis_df <-
  analysis_df %>%
  left_join(indicators_2017_df)


## RDD needed ----------------------------------------------------------

names(analysis_df)
source('protocol notes/source RDD estimates.R')
rdd_df


# Quick glance at sig results?
#   # A tibble: 6 × 8
#   outcome           Bandwidth Observations Estimate Std..Error z.value Pr...z.. bw_type  
# <chr>                 <dbl>        <dbl>    <dbl>      <dbl>   <dbl>    <dbl> <chr>    
# 1 abs_prop_prim         2.01           860  -0.0035     0.0019   -1.83   0.0679 LATE     
# 2 abs_prop_prim         4.02           890  -0.0031     0.0018   -1.76   0.079  Double-BW
# 3 noHE_prop             1.91           826  -0.0331     0.0176   -1.88   0.0603 LATE     
# 4 noHE_prop             0.954          335  -0.0477     0.0238   -2.00   0.0452 Half-BW  
# 5 noHE_prop             3.81           880  -0.0311     0.0146   -2.13   0.0332 Double-BW
# 6 phy_benefit_ratio     3.48           890  -7.56       3.67     -2.06   0.0397 Double-BW

## What was the treatment status by 2017?? 


# RDD by non-cut-offs -----------------------------------------------------
bank_this <- analysis_df

analysis_nm = 'expansion 1 (non-cut-off below)'


## below
analysis_df <-
  bank_this %>%
  filter(k < 0) %>%
  mutate(
    k = k - median(k)
  )

analysis_df
source('protocol notes/source RDD estimates.R')
## nothing

analysis_nm = 'expansion 1 (non-cut-off above)'


## below
analysis_df <-
  bank_this %>%
  filter(k > 0) %>%
  mutate(
    k = k - median(k)
  )

analysis_df
source('protocol notes/source RDD estimates.R')
## nothing





# quick fix: hack to get ITT using wards ----------------------------------
analysis_df <-
  nimdm2005_ward %>%
  mutate(
    k = k_nimdm2005,
    treated = ward_code %in% sure_start_2008$bestCode
  )

analysis_nm = 'expansion 1 (wards only)'

source('protocol notes/source RDD estimates.R')
rdd_df
