
##############
############## # POSI method
##############

posi <- PoSI(x)
summary(posi)

# save k constants for 95 and 99% confidence intervals
k95 <- summary(posi)[1,1]
k99 <- summary(posi)[2,1]

# compute 95% and 99% confidence intervals
m3 <- summary(lm(y~x))
result5 <- tidy(m3) %>% 
  filter(term != '(Intercept)') %>%
  mutate(term = substring(term,2,nchar(term)))

result5 <- result5 %>%
  mutate(lb95 = estimate - k95 * std.error,
         ub95 = estimate + k95 * std.error,
         lb99 = estimate - k95 * std.error,
         ub99 = estimate + k95 * std.error,
         p_posi_less_05 = as.numeric(ub95 <0 | lb95 > 0),
         p_posi_less_01 = as.numeric(ub99 <0 | lb99 > 0)) %>%
  dplyr::select(term,p_posi_less_05,p_posi_less_01)
result5

# here i am making an ad-hoc change
result5 <- result5 %>%
  mutate(p_posi = ifelse(p_posi_less_01 == 1, .005, 1)) %>%
  dplyr::select(term, p_posi)

rm(posi)

##############
############## # EPoSI method
##############

lambda1 <- 0.9

lasso3 <- glmnet(x, 
                 y, 
                 alpha = 1, 
                 lambda = lambda1, 
                 family='gaussian')

beta <- coef(lasso3, 
             x=x, 
             y=y, 
             s=lambda1/n, 
             exact=TRUE)[-1]

eposi <- fixedLassoInf(x,
                       y,
                       beta, 
                       lambda = lambda1, 
                       sigma=lambda1/n)
eposi

result6 <- as_tibble(cbind(colnames(x), eposi$pv))
colnames(result6) <- c('term','p_eposi')
result6$p_eposi <- as.numeric(result6$p_eposi)
result6

rm(eposi)

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
