
##############
############## # single split method
##############

# split data in train and test
data$u <- runif(nrow(data))

y_train <- y[data$u < .5]
y_test <- y[data$u >= .5]

x_train <- x[data$u < .5, ]
x_test <- x[data$u >= .5, ]

length(y)
length(y_train)
length(y_test)
dim(x)
dim(x_train)
dim(x_test)

#### train lasso model
lasso_cv2 <- cv.glmnet(x_train, 
                       y_train, 
                       alpha = 1, 
                       family='gaussian')

lambda2 <- lasso_cv2$lambda.min

lasso2 <- glmnet(x_train, 
                 y_train, 
                 alpha = 1, 
                 lambda = lambda2)  # lambda = cv_model$lambda.1se
coef(lasso2)

#### extract selected variables
w <- as.data.frame(cbind(rownames(lasso2$beta), as.numeric(lasso2$beta)))
x1 <- x_test[, colnames(x) %in% w$V1[w$V2!=0]]

#### run OLS on selected variables
m2 <- lm(y_test~x1)
result2 <- tidy(m2) %>% 
  dplyr::select(term, p.value) %>%
  filter(term != '(Intercept)') %>%
  mutate(term = substring(term,3,nchar(term)))
colnames(result2) <- c('term', 'p_ssplit')
result2

#### clean up
rm(lasso_cv2,lasso2,w,x1,lambda2,x_test,x_train,y_test,y_train,m2)
ls()
