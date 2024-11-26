## makeFile: cleaned nimdm 2017 scores (soa) 

library(tidyverse)

## SOA ranks atm
nimdm2017_df_mdm <- 
  readxl::read_xls('data/NIMDM17_SOAresults.xls', sheet = 2)

# mdm scores  -------------------------------------------------------------

## clean the names of line breaks

names(nimdm2017_df_mdm) <-
  names(nimdm2017_df_mdm) %>% gsub(pattern = '\n', replacement = '', x = .)


### 29/5 To get mdm scores we need to transform the domain ranks then add them 
## find in annex I in from https://webarchive.nationalarchives.gov.uk/ukgwa/20100410180038/http:/www.communities.gov.uk/documents/communities/pdf/131209.pdf#page=44

mdm_transform <- function(thisRank, maxRank = NULL){
  if(is.null(maxRank)){maxRank = max(thisRank)}
  
  ## reverse this rank so that the least deprived is 1 
  r = ((-1*thisRank) %>% rank()) / maxRank 
  domain_x = -23*log(1 - r*( 1 - exp(-100/23) ) )
  return(domain_x)
}

## Valdity check: check that the weights corresponds to the weights in the worked 
## example in technical report Annex D
## https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/NIMDM%202017_Technical%20Report.pdf
mdm_transform(1:5) # check that the 

### get table of raw scores 
mdmScore_df <-
  nimdm2017_df_mdm %>%
  transform(
    soa_code = SOA2001,
    soa_name = SOA2001_name,
    mdm_rank_2017 = `Multiple Deprivation Measure Rank (where 1 is most deprived)`,
    inc_score = `Income Domain Rank (where 1 is most deprived)` %>% mdm_transform(),
    emp_score = `Employment Domain Rank (where 1 is most deprived)` %>% mdm_transform(),
    access_score = `Access to Services Domain Rank (where 1 is most deprived)` %>% mdm_transform(),
    health_score = `Health Deprivation and Disability Domain Rank (where 1 is most deprived)` %>% mdm_transform(),
    edu_score = `Education, Skills and Training Domain Rank (where 1 is most deprived)` %>% mdm_transform(),
    crime_score = `Crime and Disorder Domain Rank (where 1 is most deprived)` %>% mdm_transform(),
    living_score = `Living Environment Domain Rank (where 1 is most deprived)` %>% mdm_transform()
  )

mdmScore_df <-
  mdmScore_df %>%
  mutate(
    mdm_score_2017 = 
      0.25*inc_score + 0.25*emp_score + 0.1*access_score + 0.15*health_score + 0.15*edu_score + 0.05*crime_score + 0.05*living_score
  )
mdmScore_df
## if this is done right -- should be perfect cor
cor(mdmScore_df$mdm_score_2017, mdmScore_df$mdm_rank_2017, method = 'spearman')

## write out
mdmScore_df %>% 
  select(soa_code, mdm_rank_2017, mdm_score_2017) %>%
  write_csv('outputs/cleaned nimdm 2017 scores (soa).csv')
