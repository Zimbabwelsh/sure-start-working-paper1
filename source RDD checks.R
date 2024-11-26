## source-rdd -------------------------------------
## Looks for 
## [analysis_df] = a global object
## - k = the assignement variable (scaled)
## - outcome variables are indicated 
## [analysis_nm] = string denoting the name of the variable
## [root_out_folder] -- root folder for anbalysis results (dated) 
## [out_folder] -- folder for this particular output

## saves results to outputs/[date]/[analysis_nm]/

library(tidyverse)
## Set up folder structure

# Graphs of ITT -----------------------------------------------------------

## univariate distribution of the assignment variable

k_histPlot <- 
  analysis_df %>%
  ggplot(aes(x = k)) +
  geom_histogram(fill = "white", colour = "black") +
  ggtitle('Histogram of assignment variable K')

## Cut the variable
analysis_df <-
  analysis_df %>%
  mutate(
    k_groups = k %>% cut(seq(-5, 5, 0.25)) # cut give great or equal to k
  )

treated_plot_df <-
  analysis_df %>%
  group_by(k_groups) %>%
  summarise(
    treated = mean(treated),
    n = n()
  )


treated_plot <-
  treated_plot_df %>% 
  ggplot(aes(x = k_groups, y = treated)) +
  geom_point() +
  ylab('Proportion treated') +
  xlab('Assignment scores banded (k = 0 at cut-off, higher = more deprived)') +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=0.2)) +
  ggtitle('Proportion treated by assignment score (k)') 



### McCrary test -------------------------------------------

plotMCTest <-
  function(x, cutpoint = 0){
    pval = x %>% rdd::DCdensity(cutpoint = cutpoint, plot = F) %>% round(3)
    
    (x %>% rdd::DCdensity(cutpoint = cutpoint)) + 
      title(
        paste0('McCrary test, cut-off = ', cutpoint , '; p < ', pval)
      )
  }

safely_plotMCTest <- 
  plotMCTest %>% safely()


## cutoff points ranges to try
# we have to output inside the pdf



## Paste outputs in a PDF -----------------------------------------------

pdf(file = out_folder %>% file.path('RDD checks.pdf'))

## univariate 
k_histPlot %>% print

## treated plot
treated_plot %>% print

# mcCrary
seq(-0.5, 0.5, 0.25) %>% map(safely_plotMCTest, x = analysis_df$k)



dev.off()


