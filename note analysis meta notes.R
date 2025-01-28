## misc stats 


## Check SD etc of estimators 

list.files('outputs')

root_out_folder <- 'outputs/2025-01-27'

exp1_tab <- read_csv(root_out_folder %>% file.path('expansion 1/RDD tables.csv'))

exp1_tab %>% 
  filter(Estimate != 0) %>%
  group_by(bw_type) %>%
  summarise(
    se_mean = Std..Error %>% mean,
    n_mean = Observations %>% mean,
    bw_mean = Bandwidth %>% mean
  )
  
exp2_tab <- read_csv(root_out_folder %>% file.path('expansion 2/RDD tables.csv'))

exp2_tab %>% 
  filter(Estimate != 0) %>%
  group_by(bw_type) %>%
  summarise(
    se_mean = Std..Error %>% mean,
    n_mean = Observations %>% mean,
    bw_mean = Bandwidth %>% mean
  )
