
##############
############## # naive method
##############

#### train lasso model
lasso_cv1 <- cv.glmnet(x, 
                       y, 
                       alpha = 1, 
                       family='gaussian')

lambda1 <- lasso_cv1$lambda.min

lasso1 <- glmnet(x, 
                 y, 
                 alpha = 1, 
                 lambda = lambda1)  # lambda = cv_model$lambda.1se
coef(lasso1)

#### extract selected variables
w <- as.data.frame(cbind(rownames(lasso1$beta), as.numeric(lasso1$beta)))
x1 <- x[, colnames(x) %in% w$V1[w$V2!=0]]

#### run OLS on selected variables
m1 <- lm(y~x1)
result1 <- tidy(m1) %>% 
  dplyr::select(term, p.value) %>%
  filter(term != '(Intercept)') %>%
  mutate(term = substring(term,3,nchar(term)))
colnames(result1) <- c('term', 'p_naive')
result1

#### clean up
rm(lasso_cv1,lasso1,w,x1,m1)
ls()
