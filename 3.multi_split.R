
##############
############## # multi split method
##############

msplit <- multi.split(x,
                      y,
                      ci = F,
                      fraction=0.5, 
                      model.selector = lasso.cv)
result3 <- as_tibble(cbind(names(msplit$pval.corr), msplit$pval.corr))
colnames(result3) <- c('term', 'p_msplit')
result3 <- result3 %>% 
  mutate(p_msplit = as.numeric(p_msplit))
result3

rm(msplit)

