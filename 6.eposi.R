
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