setwd("~/Desktop/optimization/hw/")
load('project/project3/data.Rdata')


p = ncol(X)
n = nrow(X)
real.pred = X%*%beta_real
beta_real

#MIQP
library(gurobi)
k = 8
M = 0.5
success = FALSE
while (success == FALSE){
  X.bin = matrix(0,nrow = n, ncol = p) 
  X.adj = cbind(X, X.bin)
  model = list()
  model$obj = -t(X.adj) %*% y
  model$Q = (t(X.adj) %*% X.adj)*0.5
  model$A = rbind(c(rep(0,p),rep(1,p)),cbind(diag(p),diag(p)*M),cbind(diag(p), diag(p)*(-M)))
  model$rhs = c(k,rep(0,2*p))
  model$modelsense = "min"
  model$sense = c('<=', rep('>=',p), rep('<=',p))
  model$vtype = c(rep('C',p), rep('B',p))
  result = gurobi(model)
  miqp.beta = result$x[1:64]
  if (max(miqp.beta) == M){
    M = M * 2
  }
  else{
    success = TRUE
  }
}

print(miqp.beta)
print(M)

#prediction from MIQP
miqp.pred = X%*%miqp.beta
error.miqp = sum((miqp.pred - real.pred)^2)/sum((real.pred)^2)
error.miqp

#lasso
library(glmnet)
grid = 10^seq(-2,2,length = 100)
lasso.mod = glmnet(X, y, alpha = 1, lambda = grid, intercept = FALSE)
cv.lasso = cv.glmnet(X, y, alpha = 1, lambda = grid, intercept = FALSE)
bestlam.lasso = cv.lasso$lambda.1se
lasso.beta = predict(lasso.mod, type = 'coefficients', s = bestlam.lasso)
lasso.beta
lasso.pred = predict(lasso.mod, s = bestlam.lasso, newx = X)

error.lasso = sum((lasso.pred - real.pred)^2)/sum((real.pred)^2)
error.lasso

