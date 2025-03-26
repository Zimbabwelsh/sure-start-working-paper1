## makeFile: Get nimdm2005 (wards) scores 
## rationale: Prior to 2009/10 DE actually tried to provide provision to top 20% of 
## deprived wards 
## After that they provided to top 20% SOA 

library(tidyverse)
#library(readODS)
## data input

nimdm2005_df <-
  'data/nimdm-2005-ward-level.csv' %>% read_csv()

### Create the deprivation score -------------------------------------------
### 29/5 To get mdm scores we need to transform the domain ranks then add them 
## find in annex I in from https://webarchive.nationalarchives.gov.uk/ukgwa/20100410180038/http:/www.communities.gov.uk/documents/communities/pdf/131209.pdf#page=44

mdm_transform <- function(thisRank, maxRank = NULL){
  if(is.null(maxRank)){maxRank = max(thisRank)}
  
  ## reverse this rank so that the least deprived is 1 
  r = ((-1*thisRank) %>% rank()) / maxRank 
  domain_x = -23*log(1 - r*( 1 - exp(-100/23) ) )
  return(domain_x)
}

mdmScore_df <-
  nimdm2005_df %>%
  transform(
    `Ward Code` = `Ward Code`,
    Ward = Ward,
    mdm_rank_2005 = `Multiple Deprivation Measure Rank`,
    inc_score = `Income Domain Rank` %>% mdm_transform(),
    emp_score = `Employment Domain Rank (18-59/64 years)` %>% mdm_transform(),
    access_score = `Proximity to Services Domain Rank` %>% mdm_transform(),
    health_score = `Health Deprivation and Disability Domain Rank` %>% mdm_transform(),
    edu_score = `Education Skills and Training Domain Rank` %>% mdm_transform(),
    crime_score = `Crime and Disorder Domain Rank` %>% mdm_transform(),
    living_score = `Living Environment Domain Rank` %>% mdm_transform()
  )

mdmScore_df <-
  mdmScore_df %>%
  mutate(
    mdm_score_2005 = 
      0.25*inc_score + 0.25*emp_score + 0.1*access_score + 0.15*health_score + 0.15*edu_score + 0.05*crime_score + 0.05*living_score
  )

## check
mdmScore_df %>%
  select(mdm_rank_2005, mdm_score_2005) %>%
  cor(method = 'spearman')
## all good


# create the running variable (80th percentile = 0) -----------------------
## higher = more deprived 

mdmScore_df <-
  mdmScore_df %>%
  mutate(
    k_nimdm2005 = mdm_score_2005 - quantile(mdm_score_2005, 0.8),
    k_nimdm2005 = k_nimdm2005 / sd(k_nimdm2005) # scale
  )

# mdmScore_df$mdm_score_2005 %>% quantile(seq(0,1,.1)) # 80% = 32.396

### output 
mdmScore_df %>% 
  rename(
    ward_name = Ward,
    ward_code = `Ward Code`
    ) %>%
  select(ward_code, ward_name, mdm_rank_2005, mdm_score_2005, k_nimdm2005) %>%
  write_csv('outputs/cleaned nimdm 2005 scores (wards).csv')

