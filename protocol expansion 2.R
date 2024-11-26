### Goal: we want to check nimdm2010 coverage 

### notebook

# input/ output -----------------------------------------------------------
## load in the sure start clean data 
library(tidyverse)

### Clear key global objects---------------------------------------

rm(root_out_folder, out_folder)

### assign new global objects -------------------------------------------------

analysis_nm <- 'expansion 2'

## Create folders

root_out_folder <- paste0('outputs/', Sys.Date()) 
out_folder <- root_out_folder %>% file.path(analysis_nm)

dir.create(root_out_folder)
dir.create(out_folder)


# data --------------------------------------------------------------------
sure_start_2008_df <- 
  'data/national archive to nimdm wards (checked).csv' %>% read_csv()

sure_start_2017_df <-
  read_csv('outputs/cleaned ssni coverage (2017).csv')


nimdm2005_df <- 
  'outputs/cleaned nimdm 2005 scores (wards).csv' %>%
  read_csv()

nimdm2010_df <- 
  'outputs/cleaned nimdm 2010 scores (soa).csv' %>%
  read_csv()

soa_ward_lookup_df <-
  'outputs/soa (2001) to ward (1992) lookup.csv' %>%
  read_csv

# Create the eligible sample ----------------------------------------------


## wards to soa 

## simplest is to get list of wards to exclude 
nimdm2005_df_soa <-
  soa_ward_lookup_df %>%
  left_join(nimdm2005_df)

exclude_nimdm2005_df <-
  nimdm2005_df_soa %>%
  filter(k_nimdm2005 >= 0)

## Convert 2009 list to soa
sure_start_2008_df_soa <-
  soa_ward_lookup_df %>%
  left_join(sure_start_2008_df, by= c(ward_code = 'bestCode'))

## due to ward mergers some soa are composed of 2 wards -- need to habve 1 value per soa
sure_start_2008_df_soa <-
  sure_start_2008_df_soa %>%
  group_by(soa_code) %>%
  summarise(
    in_ss_2009 = sum(!is.na(inputted_ward)) > 0
  )


## Create the data by add cols
## check data
nimdm2010_df$mdm_score_2010 %>% quantile(0.8) ## 33.382
nimdm2010_df$mdm_score_2010 %>% quantile(0.75) ## 29.225
nimdm2010_df$mdm_score_2010 %>% quantile(0.70) ## 26.021


## duplicate but not sure why??


analysis_df <-
  nimdm2010_df %>%
  left_join(sure_start_2008_df_soa) %>%
  left_join(sure_start_2017_df)


# join to outcomes --------------------------------------------------------
indicators_2017_df <- 
  'outputs/indicators 2017 (scaled).csv' %>% 
#  'outputs/indicators 2017.csv' %>% 
  read_csv(na = c('*','NA'))


analysis_df <- 
  analysis_df %>%
  left_join(indicators_2017_df)


# eligible sample ---------------------------------------------------------
analysis_df <-
  analysis_df %>%
  filter(
    !(soa_code %in% exclude_nimdm2005_df$soa_code)
  )

analysis_df <-
  analysis_df %>%
  mutate(
    k = k_nimdm2010,
    treated = in_ss_2017
  )


analysis_df

## basic checks and checks the ITT on treated -------------------------------
source('source RDD checks.R')

## Can check sample size here 
treated_plot_df


## RDD needed ----------------------------------------------------------

names(analysis_df)
source('source RDD estimates.R')
rdd_df %>% filter(Bandwidth %>% between(-5, 5)) # get rid of spurious 


# Quick glance at sig results?
# outcome        Bandwidth Observations Estimate Std..Error z.value Pr...z.. bw_type  
# <chr>              <dbl>        <dbl>    <dbl>      <dbl>   <dbl>    <dbl> <chr>    
# 10 in_ss_2021         1.15           435   0.325      0.101     3.23   0.0013 LATE     
# 11 in_ss_2021         0.574          191   0.294      0.144     2.05   0.0405 Half-BW  
# 12 in_ss_2021         2.29           694   0.392      0.0748    5.23   0      Double-BW
# 13 no5gcsr_prop       1.01           369   0.0635     0.0303    2.09   0.0364 LATE     
# 14 no5gcsr_prop       0.506          167   0.0829     0.0378    2.19   0.0283 Half-BW  
# 15 noHE_prop          1.25           472   0.048      0.0192    2.50   0.0124 LATE     
# 16 noHE_prop          0.623          201   0.0536     0.0237    2.27   0.0234 Half-BW  
# 17 noHE_prop          2.49           686   0.0388     0.0166    2.34   0.0193 Double-BW
# 21 treated            1.39           542   0.488      0.093     5.25   0      LATE     
# 22 treated            0.697          218   0.456      0.133     3.43   0.0006 Half-BW  
# 23 treated            2.79           694   0.474      0.0777    6.10   0      Double-BW

## Affects no GCSE and no HE-- otherwise strong effect on treated 

# RDD by non-cut-offs -----------------------------------------------------
bank_this <- analysis_df

analysis_nm = 'expansion 2 (non-cut-off below)'


## below
analysis_df <-
  bank_this %>%
  filter(k < 0) %>%
  mutate(
    k = k - median(k)
  )

analysis_df
source('source RDD estimates.R')
## Effect on child central extracts 
# 9 child_denExtract_ratio     0.866          600   -0.375     0.133    -2.83   0.0047 LATE     
# 10 child_denExtract_ratio     0.433          367   -0.312     0.180    -1.74   0.0819 Half-BW  
# 11 child_denExtract_ratio     1.73           631   -0.368     0.123    -2.98   0.0028 Double-BW

analysis_nm = 'expansion 2 (non-cut-off above)'


## above
analysis_df <-
  bank_this %>%
  filter(k > 0) %>%
  mutate(
    k = k - median(k)
  )

analysis_df
source('source RDD estimates.R')
## effects 
# 6 neet_prop               0.313           53    1.29      0.471     2.73   0.0062 LATE     
# 9 noHE_prop               0.553           56    0.993     0.397     2.50   0.0124 LATE     
# 12 loQual_prop             0.216           41    0.387     0.162     2.39   0.0167 LATE     
# 14 phy_benefit_ratio       0.264           52    0.427     0.125     3.43   0.0006 LATE     
# 17 multiPrescrib_ratio     0.398           55    0.588     0.337     1.75   0.0808 LATE     
### very few obs above this cut off 




