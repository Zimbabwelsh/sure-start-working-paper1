## source ITT estimates 

## Looks for 
## [analysis_df] = a global object
## - k = the assignement variable (scaled)
## - outcome variables are indicated 
## [analysis_nm] = string denoting the name of the variable 
###
rdd_est <-
  function(formula, data = analysis_df, scaled = F){

    ## We have to manually sort out the bandwidth due to a bug in rdd (uses old algo)
    temp_df <- model.frame(formula,analysis_df)
    
    this_bw = 
      rddtools::rdd_data(
        x = temp_df$k,
        y = temp_df[, 1],
        cutpoint = 0
      ) %>%
      rddtools::rdd_bw_ik() ## we need to implement the 2012 bandwidth algo
    
      ## do the estimate and format
    rdd_res <- rdd::RDestimate(formula, bw = this_bw, data = analysis_df) %>% summary
    
    coef_tab <- rdd_res$coefficients %>% data.frame()
    coef_tab <- coef_tab %>% mutate(bw_type = rownames(coef_tab))
    rownames(coef_tab) <- NULL 
      
      return(
        coef_tab
        )
    
  }

safely_rdd_est <-
  rdd_est %>% safely()

## Do the RDD
rdd_list <- 
  names(analysis_df) %>% paste0(' ~ k') %>% map(as.formula) %>% map(safely_rdd_est)

rdd_list <- (rdd_list %>% transpose)$result


## formate and round numbers Round numbers
names(rdd_list) <- names(analysis_df)
rdd_df <- bind_rows(rdd_list, .id = 'outcome')

rdd_df <-
  rdd_df %>% map_df(
    .f = function(x)
      if (x %>% is.numeric) {
        return(x %>% round(4))
      } else
        (return(x)
        ))

### Save and output 

rdd_df %>% write_csv(
  out_folder %>%
    file.path('RDD tables.csv')
  )

## Print very quick glance at results
message('Quick glance at sig results?')
rdd_df %>% filter(Pr...z.. < 0.1) %>% print(n = 50)
