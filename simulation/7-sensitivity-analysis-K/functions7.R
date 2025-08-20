library(glmnet) 
library(MASS)
library(expm)

### Parameter generating function
generate_para_data = function(n, p, q, r, rho=0.2, signal, seed)
{
  ## FUNCTION: generate the initial factors, loadings, coefficients with uniform signal 
  ##
  ## INPUTS: n - the sample size of the data
  ##         q - the size of test questions
  ##         p - the dimension of covariates
  ##         r - the dimension of factors (known and fixed)
  ##         signal - the nonzero entry in coefficient
  ##         seed - random seed
  ##
  ## OUTPUTS: U - factors
  ##          Gamma - loadings
  ##          Beta - coefficients
  
  set.seed(seed)
  subg <- q/r
  subgamma <- matrix(0, nrow=subg, ncol=r)
  Gamma <- NULL
  for(k in 1:r){
    subgamma[,k] <- runif(subg, 0.5, 1.5) 
    Gamma <- rbind(Gamma, subgamma)
    subgamma <- matrix(0, nrow=subg, ncol=r)
  }
  
  gdim <- nrow(Gamma)
  if(gdim < q){
    temp <- matrix(runif(gdim*r, 0.5, 1.5), nrow=gdim, ncol=r)
    Gamma <- rbind(Gamma, temp)
  }
  
  mean <- rep(0, p+r)
  var <- matrix(0, nrow=p+r, ncol=p+r)
  for (i in 1:(p+r)){
    for (j in 1:(p+r)){
      var[i,j] = rho^{abs(i-j)} 
    }
  }
  XUi <- rmvnorm(n, mean=mean, sigma=var)
  X <- XUi[, 1:p]
  U <- XUi[,(p+1):(p+r)]

  for(k in 1:p)
  {
    for(i in 1:n)
      if(abs(X[i,k])>2)
        X[i,k]=sign(X[i,k])*2;
   }
  
  X <- cbind(rep(1,n), X) 
  
  if(r >1){
    for(k in 1:r){
      for(i in 1:n)
        if(abs(U[i,k])>2)
          U[i,k]=sign(U[i,k])*2;
      U[,k] = U[,k] - mean(U[,k])
    }
  }
  
  
  GTG <- t(Gamma) %*% Gamma/q
  UTU <- t(U) %*% U/n
  
  U_true <- U %*% sqrtm(solve(UTU)%*%GTG)
  Gamma_true <- Gamma
  
  ## sparse beta
  subp <- 5
  subbeta <- NULL
  for(c in 1:p){
    if(c*subp <= q){
      temp_matrix <- matrix(0, nrow=subp, ncol=p)
      temp_matrix[,c] <- signal
      subbeta <- rbind(subbeta, temp_matrix)
    }
    else{
      break;
    }
  }
  Beta <- rbind(subbeta, matrix(0, q-nrow(subbeta), p))
  
  Beta_intercept <-  c(rep(0,q))  
  Beta <- cbind(Beta_intercept, Beta)
  
  
 
  return(list("X"=X, "U"=U_true, "Gamma"=Gamma_true, "Beta"=Beta, "var"=var))

}

### Response generating function 
generate_response = function(Gamma_true, U_true, Beta, X, seed)
{
  set.seed(seed)
  Y <- matrix(NA, n, q)
  for(i in 1:n){
    for(j in 1:q){
      temp_ij <- Gamma_true[j,] %*% U_true[i, ] + Beta[j, ] %*% X[i,] 
      prob_ij <- exp(temp_ij)/(1+exp(temp_ij)) 
      Y[i,j] <- rbinom(1, size=1, prob=prob_ij)
    }
  }
  
  return(list("Y" = Y))
}



### Data generating function
generate_data = function(n, p, q, r, U, Gamma, Beta, seed)
{
  ## FUNCTION: generate covariates and response data
  ##
  ## INPUTS: n - the sample size of the data
  ##         q - the size of test questions
  ##         p - the dimension of covariates
  ##         r - the dimension of factors (known and fixed)
  ##         U - true factor
  ##         Gamma - true loading
  ##         Beta - true coefficient
  ##         signal - the nonzero entry in coefficient
  ##         seed - random seed
  ##
  ## OUTPUTS: X - covariates
  ##          Y - response
  
  set.seed(seed)
  X <- matrix(rnorm(n*p, 0, 1), nrow = n, ncol = p)
  for(k in 1:p)
  {
    X[,k]=X[,k]-mean(X[,k]);
    for(i in 1:n)
      if(abs(X[i,k])>2)
        X[i,k]=sign(X[i,k])*2;
  }
  
  X <- cbind(rep(1,n), X) 
  
  Y <- matrix(NA, n, q)
  for(i in 1:n){
    for(j in 1:q){
      temp_ij <- Gamma[j,] %*% U[i, ] + Beta[j, ] %*% X[i,]
      prob_ij <- exp(temp_ij)/(1+exp(temp_ij)) 
      Y[i,j] <- rbinom(1, size=1, prob=prob_ij)
    }
  }
  
  return(list("X"=X, "Y"=Y))
}

### point and interval estimation function

negative_log_likelihood=function(Y, X, U, Gamma, Beta){
  ## FUNCTION: compute neg loglikelihood function
  ##
  ## INPUTS: Y - response
  ##         X - covariates
  ##         U - estimated factors
  ##         Gamma - estimated loadings
  ##         Beta - estimated coefficients
  ##
  ## OUTPUT: neglog - neg loglikelihood value
  
  temp <- Gamma %*% t(U) + Beta %*% t(X)
  phi <- t(exp(temp)/(1+exp(temp)))
  neglog <- - sum(apply(Y*log(phi) + (1-Y)*log(1-phi), 2, sum), na.rm = TRUE)
  
  return(list("neglog"=neglog))
}




normalization=function(Y, X, r, U1, Gamma1){
  if(r > 1){
    ## Fix the rotation of Gamma and U
    M_u <- t(U1) %*% U1/n
    M_gamma <- t(Gamma1) %*% Gamma1 /q
    svd1 <- svd(sqrtm(M_u) %*% M_gamma %*% sqrtm(M_u))
    #Ghat <- sqrtm(solve(M_u)) %*% sqrtm(sqrtm(sqrtm(M_u) %*% M_gamma %*% sqrtm(M_u)))
    Ghat <- solve(sqrtm(M_u)) %*% svd1$u %*% diag(sqrt(sqrt(svd1$d)))
    
    ## finalize estimators
    Gamma1 <- Gamma1 %*% t(solve(Ghat))
    U1 <- U1 %*% Ghat 
  }

  if(r==1){
    ## Fix the rotation of Gamma and U
    M_u <- t(U1) %*% U1/n
    M_gamma <- t(Gamma1) %*% Gamma1 /q
    svd1 <- svd((M_u)^(1/2) * M_gamma * (M_u)^(1/2))
    Ghat <- (M_u)^(-1/2) %*% svd1$u %*% (svd1$d)^(1/4)
    
    #Ghat <- M_u^(-1/2) *(M_u^(1/2) * M_gamma * M_u^(1/2))^(1/4)
    
    ## finalize estimators
    Gamma1 <- Gamma1 %*% t(solve(Ghat))
    U1 <- U1 %*% Ghat 
  }
  
  
  return(list("Gamma1"=Gamma1, "U1"=U1))
  
  
}

# Define the objective function to be minimized
objective_function <- function(par, beta, gamma, p, r, q) {
  # Reshape the vector par into a r x p matrix A
  A <- matrix(par, nrow = r, ncol = p)
  
  # Calculate the sum of absolute differences
  sum_loss <- sum(apply(beta[,2:(p+1)] - gamma %*% A, 2, function(x) sum(abs(x))))
  
  return(sum_loss)
}

optimize_A=function(p, r, q, Beta1, Gamma1){
  # Initial guess for A as a vector (you can provide your own initial guess)
  A_initial_vector <- runif(r * p)
  
  # Perform optimization
  result <- optim(
    par = A_initial_vector,
    fn = objective_function,
    beta = Beta1,
    gamma = Gamma1,
    p = p,
    r = r,
    q = q,
    method = "BFGS"  # You can choose a different optimization method if needed
  )
  
  # Extract the optimized A matrix
  A_optimized <- matrix(result$par, nrow = r, ncol = p, byrow=F)
  
  # Print the optimized A matrix
  return(list("A"=A_optimized))
  
}


AM_estimation=function(Y, X, r, tol=0.001, tau=5, ite=1, maxiter=20, seed)
{
  ## FUNCTION: Obtain M estimators for all parameters (note: we do not fix rotation)
  ##
  ## INPUTS: Y - response
  ##         X - covariates
  ##         Gamma0 - initial loading matrix
  ##         Beta0 - initial coefficient matrix
  ##         U0 - initial factors
  ##         tol - maximum error bound for neg log
  ##         ite - number of iterations to obtain the smallest neglog
  ##
  ## OUTPUTS: 
  
  
  n <- dim(Y)[1]
  q <- dim(Y)[2]
  p <- dim(X)[2]-1
  
  Gamma_L1 <- NULL
  Beta_L1 <- NULL
  U_L1 <- NULL
  neglog_list <- NULL
  
  for (t in 1:ite){
    Gamma1 <- NULL
    Beta1 <- NULL
    U1 <- NULL
    
    set.seed(seed + 100*t)
    
    Beta0 <- matrix(0, q, p+1)
    Gamma0 <- matrix(abs(rnorm(q*r, 0, 1)), nrow=q, ncol=r)  
    U0 <- matrix(rnorm(n*r, 0, 1), nrow=n, ncol=r)
    
    ## centralize U 
    for(k in 1:r){
      U0[,k] = U0[,k] - mean(U0[,k])
    }
    
    norm0 <- normalization(Y, X, r, U0, Gamma0)
    U0 <- norm0$U1
    Gamma0 <- norm0$Gamma1
    
    
    for (i in 1:n){
      fit_i <- glm(Y[i, ] ~ Gamma0 + offset(Beta0 %*% X[i, ])-1, family=binomial()) #, intercept=FALSE)
      coef_i <- coef(fit_i)
      # truncate the U1
      coef_i[is.na(coef_i)] <- tau
      coef_i <- pmin(coef_i, tau)  
      coef_i <- pmax(coef_i, -tau)
      U1 <- rbind(U1, coef_i)
    }
    
    for (j in 1:q){
      fit_j <- glm(Y[,j] ~ U1 + X -1,  family=binomial()) 
      coef_j <- coef(fit_j)
      coef_j[is.na(coef_j)] <- tau
      coef_j <- pmin(coef_j, tau)  
      coef_j <- pmax(coef_j, -tau)
      
      gamma_j <- coef_j[1:r]
      beta_j <- coef_j[-(1:r)]
      
      Gamma1 <- rbind(Gamma1, gamma_j)
      Beta1 <- rbind(Beta1, beta_j)
    }
    
    neg_temp <- negative_log_likelihood(Y, X, U1, Gamma1, Beta1)
    neg1 <- neg_temp$neglog
    
    
    
    iter <- 0 
    while (max(norm(U1-U0, "2")/n, norm(Beta1-Beta0, "2")/q, norm(Gamma1-Gamma0, "2")/q) > tol){
      #for (s in 1:1){
      Gamma0 <- Gamma1
      Beta0 <- Beta1
      U0 <- U1
      neg0 <- neg1
      
      Gamma1 <- NULL
      Beta1 <- NULL
      U1 <- NULL
      
      Beta_comp <- NULL
      
      for (i in 1:n){
        fit_i <- glm(Y[i, ] ~ Gamma0 + offset(Beta0 %*% X[i, ])-1, family=binomial()) #, intercept=FALSE)
        coef_i <- coef(fit_i)
        # truncate the U1
        coef_i[is.na(coef_i)] <- tau
        coef_i <- pmin(coef_i, tau)  
        coef_i <- pmax(coef_i, -tau)
        U1 <- rbind(U1, coef_i)
      }
      
      for (j in 1:q){
        fit_j <- glm(Y[,j] ~ U1 + X -1,  family=binomial()) 
        coef_j <- coef(fit_j)
        coef_j[is.na(coef_j)] <- tau
        coef_j <- pmin(coef_j, tau)  
        coef_j <- pmax(coef_j, -tau)
        
        gamma_j <- coef_j[1:r]
        beta_j <- coef_j[-(1:r)]
        
        Gamma1 <- rbind(Gamma1, gamma_j)
        Beta1 <- rbind(Beta1, beta_j)
      }
      
      
      neg_temp <- negative_log_likelihood(Y, X, U1, Gamma1, Beta1)
      neg1 <- neg_temp$neglog
      cat("iter", iter, "\n")
      cat("neg1", neg1, "\n")
      cat("norm", max(norm(U1-U0, "2")/n, norm(Beta1-Beta0, "2")/q, norm(Gamma1-Gamma0, "2")/q), "\n")
      iter <- iter + 1    
      
      if (iter >= maxiter)
        break;
    }
    
    L1_opt <- optimize_A(p, r, q, Beta1, Gamma1)
    A_opt <- L1_opt$A
    Beta1 <- cbind(Beta1[,1], Beta1[,2:(p+1)] - Gamma1 %*%A_opt)
    U1 <- U1 + X[,2:(p+1)] %*% t(A_opt)

    ## centralize U
    for(k in 1:r){
      U1[,k] = U1[,k] - mean(U1[,k])
    }

    norm0 <- normalization(Y, X, r, U1, Gamma1)
    U1 <- norm0$U1
    Gamma1 <- norm0$Gamma1
    Gamma1 <- abs(Gamma1)
    
    Gamma0 <- Gamma1
    Beta0 <- Beta1
    U0 <- U1
    neg0 <- neg1
    
    Gamma1 <- NULL
    Beta1 <- NULL
    U1 <- NULL
    
    Beta_comp <- NULL
    
    for (i in 1:n){
      fit_i <- glm(Y[i, ] ~ Gamma0 + offset(Beta0 %*% X[i, ])-1, family=binomial()) #, intercept=FALSE)
      coef_i <- coef(fit_i)
      # truncate the U1
      coef_i[is.na(coef_i)] <- tau
      coef_i <- pmin(coef_i, tau)  
      coef_i <- pmax(coef_i, -tau)
      U1 <- rbind(U1, coef_i)
    }
    
    for (j in 1:q){
      fit_j <- glm(Y[,j] ~ U1 + X -1,  family=binomial()) 
      coef_j <- coef(fit_j)
      coef_j[is.na(coef_j)] <- tau
      coef_j <- pmin(coef_j, tau)  
      coef_j <- pmax(coef_j, -tau)
      
      gamma_j <- coef_j[1:r]
      beta_j <- coef_j[-(1:r)]
      
      Gamma1 <- rbind(Gamma1, gamma_j)
      Beta1 <- rbind(Beta1, beta_j)
    }
    
    L1_opt <- optimize_A(p, r, q, Beta1, Gamma1)
    A_opt <- L1_opt$A
    Beta1 <- cbind(Beta1[,1], Beta1[,2:(p+1)] - Gamma1 %*%A_opt)
    U1 <- U1 + X[,2:(p+1)] %*% t(A_opt)
    
    ## centralize U
    for(k in 1:r){
      U1[,k] = U1[,k] - mean(U1[,k])
    }
    
    norm0 <- normalization(Y, X, r, U1, Gamma1)
    U1 <- norm0$U1
    Gamma1 <- norm0$Gamma1
    Gamma1 <- abs(Gamma1)
    
    neg_temp <- negative_log_likelihood(Y, X, U1, Gamma1, Beta1)
    neg1 <- neg_temp$neglog
    
    Gamma_L1 <- rbind(Gamma_L1, Gamma1)
    Beta_L1 <- rbind(Beta_L1, Beta1)
    U_L1 <- rbind(U_L1, U1)
    neglog_list <- c(neglog_list, neg1)
  }
  idx_opt <- which.min(neglog_list)
  Gam_opt <- Gamma_L1[((idx_opt-1)*q+1):((idx_opt-1)*q+q), ]
  Beta_opt <- Beta_L1[((idx_opt-1)*q+1):((idx_opt-1)*q+q), ]
  U_opt <- U_L1[((idx_opt-1)*n+1):((idx_opt-1)*n+n), ]
  
  return(list("Gamma1"=Gam_opt, "Beta1"=Beta_opt, "U1"=U_opt, "A_opt"=A_opt))
}

AM_estimation_bcmk=function(Y, X, r, tol=0.001, tau=5, ite=1, maxiter=20, seed)
{
  ## FUNCTION: Obtain M estimators from GLM
  ##
  ## INPUTS: Y - response
  ##         X - covariates
  ##         Gamma0 - initial loading matrix
  ##         Beta0 - initial coefficient matrix
  ##         U0 - initial factors
  ##         tol - maximum error bound for neg log
  ##         ite - number of iterations to obtain the smallest neglog
  ##
  ## OUTPUTS: 
  
  n <- dim(Y)[1]
  q <- dim(Y)[2]
  p <- dim(X)[2]-1
  
  for (j in 1:q){
    index_Nj <- which(!is.na(Y[,j]))
    fit_j <- glm(Y[index_Nj,j] ~ X[index_Nj,] -1,  family=binomial()) 
    coef_j <- coef(fit_j)
    coef_j[is.na(coef_j)] <- tau
    coef_j <- pmin(coef_j, tau)  
    coef_j <- pmax(coef_j, -tau)
      
    beta_j <- coef_j
    Beta1 <- rbind(Beta1, beta_j)
  }
  
  return(list("Beta1"=Beta1))
}
