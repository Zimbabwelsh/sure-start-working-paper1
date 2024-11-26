## note: expansion 3 and nimd 2017

library(tidyverse)
library(rdd)

### Clear key global objects---------------------------------------

rm(root_out_folder, out_folder)

### assign new global objects -------------------------------------------------

analysis_nm <- 'expansion 3'

## Create folders

root_out_folder <- paste0('outputs/', Sys.Date()) 
out_folder <- root_out_folder %>% file.path(analysis_nm)

dir.create(root_out_folder)
dir.create(out_folder)




## data input --------------------------
sure_start_2017_df <-
  read_csv('outputs/cleaned ssni coverage (2017).csv')

## nimdm 2010 and 2017

nimdm2010_df <- 
  read_csv('outputs/cleaned nimdm 2010 scores (soa).csv')

nimdm2017_df <- 
  read_csv('outputs/cleaned nimdm 2017 scores (soa).csv')

## Check Sure Start 2017 
sure_start_2017_df <-
  sure_start_2017_df %>%
#  mutate(SOA2001 = 'soa_code') %>%
  left_join(
    nimdm2010_df
  ) %>%
  left_join(
    nimdm2017_df
  )

sure_start_2017_df <-
  sure_start_2017_df %>%
  mutate(
    k_nimdm2017 = (mdm_score_2017 - 28.8)/ sd(mdm_score_2017)
  )

## Check for discontinuity (DE data) ----------------------------------
sure_start_2017_df %>% 
  group_by(phase_included_de) %>%
  summarise(
    min = mdm_rank_2017 %>% min(),
    max = mdm_rank_2017 %>% max(),
    min_score = mdm_score_2017 %>% min(),
    max_score = mdm_score_2017 %>% max()
    
  )
## perfect discontinuity -- cut off is 28.8
check_nimdm_2017 <- 
  sure_start_2017_df %>%
  mutate(Phase = phase_included_de %>% as.character) %>%
  filter(Phase %in% 4:5) %>%
  ggplot(
    aes(x = Phase, 
        y = 
          k_nimdm2017,
        # mdm_rank_2017, 
        colour = Phase)
  ) +
  geom_point(position = 'jitter') 

# Graphs of ITT -----------------------------------------------------------
analysis_df$k_nimdm2010 %>% quantile
itt_df <-
  sure_start_2017_df %>%
  mutate(Phase = phase_included_de %>% as.character) %>%
  filter(Phase %in% 4:5) %>%
  #  filter(!in_ss_2009) %>% # full eliminate wards on 2008
  mutate(
    k_nimdm2017_groups = k_nimdm2017 %>% cut(seq(-4, 4, 0.25))
  )

break_graph_df <-
  itt_df %>%
  group_by(k_nimdm2017_groups) %>%
  summarise(
    prop_ss_2017 = mean(in_ss_2017),
    prop_ss_2021 = mean(in_ss_2021), 
    n = n()
  )

break_graph_df
break_graph_df %>% 
  ggplot(aes(x = k_nimdm2017_groups, y = prop_ss_2021)) +
  geom_point() +
  ylab('Proportion in Sure Start (2021)') +
  xlab('NIMDM2010 scores banded (k = 0 at cut-off, higher = more deprived)') +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0.2)) +
  ggtitle('Proportion of SOAs in Sure Start (2021) by NIMDM 2010 score (k)')







## optional interactive plot 
library(plotly)
check_nimdm_2017 %>% plotly::ggplotly()

## Without the DE data --------- 
sure_start_2017_df

## nimdm 2005 data
nimdm2005_df <- 
  'outputs/cleaned nimdm 2005 scores (wards).csv' %>%
  read_csv()

soa_ward_lookup_df <-
  'outputs/soa (2001) to ward (1992) lookup.csv' %>%
  read_csv

## simplest is to get list of wards to exclude 
nimdm2005_df_soa <-
  soa_ward_lookup_df %>%
  left_join(nimdm2005_df)

exclude_nimdm2005_df <-
  nimdm2005_df_soa %>%
  filter(k_nimdm2005 >= 0)

check_nimdm_2017_no_de <- 
  sure_start_2017_df %>%
#  filter(Phase %in% 4:5) %>%
  filter(!(soa_code %in% exclude_nimdm2005_df$soa_code)) %>% ## over 25% NIMDM
  filter(k_nimdm2010 < 0) %>% ## over 25% NIMDM
  ggplot(
    aes(y = in_ss_2021, 
        x = 
          mdm_score_2017 - 28.8,
        # mdm_rank_2017, 
        colour = in_ss_2021)
  ) +
  geom_point(position = 'jitter') 






## optional interactive plot 
library(plotly)
check_nimdm_2017_no_de %>% plotly::ggplotly()


## Check numbers 
bw <- 
  0.5 * (sure_start_2017_df$mdm_score_2017 %>% sd() )## SD is 14.8

sure_start_2017_df %>%
  filter(
    mdm_score_2017 %>% between(29 - bw, 29 + bw)
    ) %>%
  group_by(
    mdm_score_2017 > 29, in_ss_2021
    ) %>%
  summarise(
    n = n()
  )

## 100% of areas with a score higher than 29 is in sure start (81 areas)
66/ (66+75)  ## 46.8% are in Sure Start
## leads to a fuzzy RDD



# indicators --------------------------------------------------------------
library(rddtools)

indicators_2017_df <- 
  'outputs/indicators 2017 (scaled).csv' %>%
#  'outputs/indicators 2017.csv' %>% 
  read_csv(na = c('*','NA'))

## All SOAs are wards or sub-division of ward exception for some wards in moyles
## since the outcome is at soa level we'll use that
## join indicators

analysis_df <- 
  sure_start_2017_df %>%
  mutate(Phase = phase_included_de %>% as.character) %>%
  filter(Phase %in% 4:5) 

analysis_df <-
  analysis_df %>%
  left_join(indicators_2017_df) 

analysis_df <-
  analysis_df %>%
  mutate(
    k = k_nimdm2017,
    treated = in_ss_2021
  )



## basic checks and checks the ITT on treated -------------------------------
source('protocol notes/source RDD checks.R')

## Can check sample size here 
treated_plot_df
k_histPlot

## RDD needed ----------------------------------------------------------

names(analysis_df)
source('protocol notes/source RDD estimates.R')
rdd_df

# Quick glance at sig results?                                                                                           
#   # A tibble: 14 × 8
#   outcome        Bandwidth Observations Estimate Std..Error z.value Pr...z.. bw_type  
# <chr>              <dbl>        <dbl>    <dbl>      <dbl>   <dbl>    <dbl> <chr>    
# 9 abs_prop_prim      1.06           267   0.0077     0.0042    1.84   0.0662 LATE     
# 10 abs_prop_prim      2.11           549   0.0068     0.0038    1.81   0.07   Double-BW
# 11 loQual_prop        2.07           549  -0.0449     0.0204   -2.20   0.0281 Double-BW

## higher levels of absenteeism post-primary in those regions -- otherwise nothing 
## We know it's a prefect discontinuity
## Also get 267 obs including all the treatment


# RDD by non-cut-offs -----------------------------------------------------
bank_this <- analysis_df

analysis_nm = 'expansion 3 (non-cut-off below)'


## below
analysis_df <-
  bank_this %>%
  filter(k < 0) %>%
  mutate(
    k = k - median(k)
  )

analysis_df
source('protocol notes/source RDD estimates.R')
## Nothing at the standard p values
# 9 sen_prop_postprim     0.789          483   -0.247      0.134   -1.83   0.0666 LATE   


## no point doing above as there's only 22 data points