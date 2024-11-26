## outcomes data 

# input/ output -----------------------------------------------------------
## load in the sure start clean data 
library(tidyverse)

### check vs 2017 education indicators -----------------------------------------
## most this pertains to 2014/15 to 2015/16 .. so exposure isn't great at this point
nimdm2017_df_school <- 
  readxl::read_xls('data/NIMDM17_SOAresults.xls', sheet = 6, na = '*')

## fixed names

names(nimdm2017_df_school) <- names(nimdm2017_df_school) %>% gsub('\n', ' ', x = .)

names(nimdm2017_df_school)
school_2017_df <-
  nimdm2017_df_school %>%
  rename(soa_code = SOA2001) %>%
  select(soa_code, 
         `Proportion of pupils attending Special Schools or attending primary school with Special Education Needs Stages 3-5 (%)`:`Proportions of working age adults (25-64) with no or low levels of qualification  (%)`)

## QA and fix

school_2017_df <-
  school_2017_df %>%
  mutate(
    `Proportion of 18-21 year olds who have not enrolled in Higher Education Courses at Higher or Further Education establishments (%)` =
      `Proportion of 18-21 year olds who have not enrolled in Higher Education Courses at Higher or Further Education establishments (%)` %>%
      as.numeric()
  )


### check vs 2017 indicators health--------------------------------------
nimdm2017_df_health <- 
    readxl::read_xls('data/NIMDM17_SOAresults.xls', sheet = 5, na = '*')

# fix names
names(nimdm2017_df_health) <- names(nimdm2017_df_health) %>% gsub('\n', ' ', x = .)
names(nimdm2017_df_health) 

health_2017_df <-
  nimdm2017_df_health %>%
  rename(
    soa_code = SOA2001
  ) %>%
  select(soa_code,
          `Standardized preventable death ratio (excluding Suicides)`:`Standardised ratio of people with a long-term health problem or disability (Excluding Mental Health problems) (NI = 100)`
         )

## join and write out
nimdm2017_indicators <- 
  school_2017_df %>%
  left_join(health_2017_df)


nimdm2017_indicators %>% str



# rename to neaten --------------------------------------------------------
nimdm2017_indicators %>% names()

nimdm2017_indicators <-
  nimdm2017_indicators %>%
  transmute(
    soa_code = `soa_code`,
    sen_prop_prim = `Proportion of pupils attending Special Schools or attending primary school with Special Education Needs Stages 3-5 (%)`,
    abs_prop_prim = `Absenteeism at Primary Schools  (% of possible attendance)`,
    no5gcsr_prop = `Proportion of school leavers not achieving five or more GCSEs at A*-C (and equivalent) incl. English and maths (%)`,
    neet_prop = `Proportion of those leaving school aged 16, 17 and 18 not entering Education, Employment or Training (%)`,
    noHE_prop = `Proportion of 18-21 year olds who have not enrolled in Higher Education Courses at Higher or Further Education establishments (%)`,
    sen_prop_postprim = `Proportion of pupils attending Special Schools or who are attending post-primary schools with Special Education Needs Stages 3-5 (%)`,
    abs_prop_postprim = `Absenteeism at  post-primary schools (% of possible attendance)`,
    loQual_prop = `Proportions of working age adults (25-64) with no or low levels of qualification  (%)`,
    death_ratio =  `Standardized preventable death ratio (excluding Suicides)`,
    phy_benefit_ratio =  `Standardised physical health-related benefit ratio`,
    cancer_ratio =  `Standardized ratio of people registered as having cancer (excluding non-melanoma skin cancers) (NI = 100)`,
    emergency_ratio =  `Standardized emergency admission ratio (NI = 100)`,
    lo_birthweight_prop =  `Proportion of Singleton Births with Low Birth Weight  (%)`,
    child_denExtract_ratio =  `Standardized ratio of Children’s Dental Extractions (NI = 100)`,
    multiPrescrib_ratio =  `Standardised ratio of people on multiple prescriptions on a regular basis (NI = 100)`,
    health_long_ratio =  `Standardised ratio of people with a long-term health problem or disability (Excluding Mental Health problems) (NI = 100)`            
    
  )


# scale outcomes ----------------------------------------------------------
nimdm2017_indicators_scaled <-
  nimdm2017_indicators %>%
  mutate_if(is.numeric, .f = function(x) scale(x)[,1]) 

## output
nimdm2017_indicators %>% 
  write_csv('outputs/indicators 2017.csv')

nimdm2017_indicators_scaled %>% 
  write_csv('outputs/indicators 2017 (scaled).csv')
