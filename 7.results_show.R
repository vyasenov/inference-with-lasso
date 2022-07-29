source('0.data_prep.R')
source('1.naive.R')
source('2.single_split.R')
source('3.multi_split.R')
source('4.bias_correct.R')
source('5.posi.R')
source('6.eposi.R')

##############
############## FORMAT RESULTS
##############

result1
result2
result3
result4
result5
result6

results <- full_join(result1,result2,by='term')
results <- full_join(results,result3,by='term')
results <- full_join(results,result4,by='term')
results <- full_join(results,result5,by='term')
results <- full_join(results,result6,by='term')

results <- results %>% 
  mutate_if(is.numeric, round, 3) 
results

##############
############## PRINT RESULTS
##############

# show significant vars at 5%
results %>%
  mutate(across(p_naive:p_eposi, ~ ifelse(. < .05,1,0))) 

# show number of significant vars at 5% by method
results %>%
  mutate(across(p_naive:p_eposi, ~ ifelse(. < .05,1,0))) %>%
  mutate(across(p_naive:p_eposi, ~ ifelse(is.na(.),0,.))) %>%
  summarize(across(p_naive:p_eposi, sum))

# show significant vars at 1%
results %>%
  mutate(across(p_naive:p_eposi, ~ ifelse(. < .01,1,0))) 

# show number of significant vars at 1% by method
results %>%
  mutate(across(p_naive:p_eposi, ~ ifelse(. < .01,1,0))) %>%
  mutate(across(p_naive:p_eposi, ~ ifelse(is.na(.),0,.))) %>%
  summarize(across(p_naive:p_eposi, sum))
