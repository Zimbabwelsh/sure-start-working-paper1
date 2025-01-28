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
  'data/national archive to nimdm wards (checked).csv' %>% read_csv()

hansard_2006 <-
  'data/sure start coverage (hansard 2006) (checked).csv' %>%
  read_csv

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

## omit ward in the hansard list -------------------------------------
hansard_2006$hansard_2006_coverage %>% 
  hist()
hansard_2006$hansard_2006_coverage %>% quantile()

hansard_2006 %>% arrange(hansard_2006_coverage) ## those under 10% are clearly outreach

legacy_df <- 
  hansard_2006 #%>%
  # filter(hansard_2006_coverage > 10) # 105 legacy wards

analysis_df <-
  analysis_df %>%
  filter(
    !(ward_code %in% legacy_df$bestCode)
  )

# source scripts ----------------------------------------------------------

## Run RDD checks 

## basic checks and checks the ITT on treated
source('source RDD checks.R')

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
    k = k %>% max,
    treated = treated %>% mean
  )

## join indicators

analysis_df <-
  analysis_df %>%
  left_join(indicators_2017_df)


## RDD needed ----------------------------------------------------------

names(analysis_df)
source('source RDD estimates.R')
rdd_df


# Quick glance at sig results?                                                                
#   # A tibble: 8 × 8
#   outcome     Bandwidth Observations Estimate Std..Error z.value Pr...z.. bw_type  
# <chr>           <dbl>        <dbl>    <dbl>      <dbl>   <dbl>    <dbl> <chr>    
#   1 k            3837.             708    0         0      -Inf      0      LATE     
# 2 k            1919.             708    0         0      -Inf      0      Half-BW  
# 3 k            7674.             708    0         0      -Inf      0      Double-BW
# 4 treated         1.36           432    0.462     0.108     4.28   0      LATE     
# 5 treated         2.73           705    0.574     0.0797    7.20   0      Double-BW
# 6 noHE_prop       0.877          220   -0.478     0.271    -1.76   0.0778 Half-BW  
# 7 noHE_prop       3.51           702   -0.296     0.158    -1.87   0.0617 Double-BW
# 8 loQual_prop     2.13           699   -0.210     0.119    -1.76   0.0789 Double-BW
# There were 27 warnings (use warnings() to see them)

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
source('source RDD estimates.R')
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
source('source RDD estimates.R')
## nothing





# quick fix: hack to get ITT using wards ----------------------------------
analysis_df <-
  nimdm2005_ward %>%
  mutate(
    k = k_nimdm2005,
    treated = ward_code %in% sure_start_2008$bestCode
  )

analysis_nm = 'expansion 1 (wards only)'

source('source RDD estimates.R')
rdd_df
