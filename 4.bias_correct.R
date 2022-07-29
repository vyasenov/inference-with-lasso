
##############
############## # bias-corrected method
##############

bclasso <- lasso.proj(x,
                      y,
                      family = 'gaussian')
result4 <- as_tibble(cbind(names(bclasso$pval.corr), bclasso$pval.corr))
colnames(result4) <- c('term', 'p_biascorr')
result4 <- result4 %>% 
  mutate(p_biascorr = as.numeric(p_biascorr))
result4

rm(bclasso)