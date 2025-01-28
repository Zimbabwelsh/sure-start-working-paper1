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

hansard_2006 <-
  'data/sure start coverage (hansard 2006) (checked).csv' %>%
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

##ward in the hansard list into SOA -------------------------------------
hansard_2006 %>% arrange(hansard_2006_coverage) ## those under 10% are clearly outreach

legacy_df <- 
  hansard_2006 #%>%
#  filter(hansard_2006_coverage > 10) # 105 legacy wards

exclude_hansard_soa_df <-
  nimdm2005_df_soa %>%
  filter(ward_code %in% legacy_df$bestCode)
  



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
  # exclude 2006 legacy wards
  filter(
    !(soa_code %in% exclude_hansard_soa_df$soa_code)
  ) %>%
## Exclude 20% most deprived NIMDM
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
#   # A tibble: 26 × 8
#   outcome           Bandwidth Observations Estimate Std..Error z.value Pr...z.. bw_type  
# <chr>                 <dbl>        <dbl>    <dbl>      <dbl>   <dbl>    <dbl> <chr>    
# 5 in_ss_2017            0.942          272    0.545      0.129    4.22   0      LATE     
# 6 in_ss_2017            0.471          119    0.437      0.185    2.36   0.0181 Half-BW  
# 7 in_ss_2017            1.88           613    0.644      0.105    6.13   0      Double-BW
# 8 in_ss_2021            0.814          218    0.307      0.138    2.23   0.0257 LATE     
# 9 in_ss_2021            1.63           587    0.464      0.104    4.47   0      Double-BW
# 10 abs_prop_prim         1.24           399    0.259      0.149    1.73   0.0829 LATE     
# 11 abs_prop_prim         2.48           613    0.264      0.140    1.89   0.0581 Double-BW
# 12 no5gcsr_prop          0.621          162    0.551      0.241    2.28   0.0225 LATE     
# 13 no5gcsr_prop          0.310           77    0.598      0.308    1.94   0.0521 Half-BW  
# 14 no5gcsr_prop          1.24           400    0.442      0.196    2.25   0.0243 Double-BW
# 15 noHE_prop             0.954          278    0.552      0.181    3.05   0.0023 LATE     
# 16 noHE_prop             0.477          119    0.530      0.235    2.26   0.0241 Half-BW  
# 17 noHE_prop             1.91           609    0.487      0.153    3.19   0.0014 Double-BW
# 21 phy_benefit_ratio     0.645          165    0.207      0.112    1.85   0.0638 LATE     
# 22 phy_benefit_ratio     1.29           420    0.165      0.093    1.77   0.0766 Double-BW


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

# 8 no5gcsr_prop               0.788          521   -0.321     0.109    -2.95   0.0032 LATE    
# 9 no5gcsr_prop               0.394          321   -0.391     0.146    -2.68   0.0074 Half-BW 
# 10 no5gcsr_prop               1.58           569   -0.306     0.0987   -3.10   0.002  Double-…
# 11 sen_prop_postprim          0.867          537   -0.214     0.122    -1.75   0.0797 LATE    
# 12 sen_prop_postprim          1.73           569   -0.196     0.114    -1.72   0.0846 Double-…
# 13 abs_prop_postprim          0.571          456   -0.261     0.110    -2.36   0.0181 LATE    
# 14 abs_prop_postprim          0.286          237   -0.303     0.149    -2.03   0.0422 Half-BW 
# 15 abs_prop_postprim          1.14           569   -0.324     0.0931   -3.48   0.0005 Double-…
# 17 child_denExtract_ratio     0.655          492   -0.466     0.159    -2.93   0.0034 LATE    
# 18 child_denExtract_ratio     0.327          264   -0.620     0.240    -2.58   0.0099 Half-BW 
# 19 child_denExtract_ratio     1.31           569   -0.399     0.133    -3.01   0.0026 Double-…
# 20 multiPrescrib_ratio        0.381          306    0.273     0.129     2.12   0.0341 LATE    
# 21 multiPrescrib_ratio        0.190          157    0.517     0.170     3.04   0.0024 Half-BW 
# 22 health_long_ratio          0.164          136    0.232     0.140     1.66   0.0965 Half-BW 


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
# 5 noHE_prop            0.230            32   -0.700      0.420   -1.67   0.0954 LATE 



