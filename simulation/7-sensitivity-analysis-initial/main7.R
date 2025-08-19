library(glmnet) 
library(MASS)
library(mvtnorm)
library("usethis")
#library(devtools)
library("psych")
#library("ltm")
#library("irtoys")
library("mirt")

source("functions7.R")

args <- commandArgs(TRUE)
n <- as.numeric(args[[1]])        # X_i dimension 
p <- as.numeric(args[[2]])        # sample size  
q <- as.numeric(args[[3]])
r <- as.numeric(args[[4]])
nrep <- as.numeric(args[[5]])
signal <- as.numeric(args[[6]])
rho <- as.numeric(args[[7]])
ite <- as.numeric(args[[8]]) # 3 5 8

## Set parameter dimensions
#n <- 300
#p <- 30
#q <- 100
#r <- 2
#signal <- 1
#nrep <- 20
#rho <- 0.2
initial_seed <- 100
set.seed(initial_seed)

Gamma_est <- NULL
Beta_est <- NULL
U_est <- NULL

Gamma_stat_full <- NULL
Beta_stat_full <- NULL
U_stat_full <- NULL

Beta_test_full <- NULL
Beta_pvalue_full <- NULL
Beta_ind_pvalue_full <- NULL

size_ind_full <- NULL
power_ind_full <- NULL

para <- generate_para_data(n=n, p=p, q=q, r=r, rho=rho, signal=signal, seed=initial_seed)
U_true <- para$U
Gamma_true <- para$Gamma
Beta_true <- para$Beta
X <- para$X

for (rep in 1:100){
  rep_seed <- 500+nrep+rep
  set.seed(rep_seed)

  simu_data <- generate_response(Gamma_true, U_true, Beta_true, X, seed=rep_seed)
  Y <- simu_data$Y
  
  # Estimation and normalization
  am_est <- AM_estimation(Y, X, r, tol=0.0001, tau=3, ite=ite, maxiter=20, seed=rep_seed)
  
  Gamma_final <- am_est$Gamma1
  Beta_final <- am_est$Beta1
  U_final <- am_est$U1
  
  Gamma_final <- as.matrix(Gamma_final)
  U_final <- as.matrix(U_final)
  Beta_final <- as.matrix(Beta_final)
  
  
  Gamma_est <- rbind(Gamma_est, Gamma_final)
  Beta_est <- rbind(Beta_est, Beta_final)
  U_est <- rbind(U_est, U_final)
  
  # Generate test statistics
  U_stat <- NULL
  Gamma_stat <- NULL
  Beta_stat <- NULL
  
  Beta_test <- NULL
  Beta_pvalue <- NULL
  Beta_ind_pvalue <- NULL
  for(i in 1:n){
    sigma_ui1 <- 0
    sigma_ui2 <- 0
    for(j in 1:q){
      wij <- Gamma_final[j,] %*% U_final[i,] + Beta_final[j,] %*% X[i,]
      lij2 <- - exp(wij)/(1+exp(wij))^2
      sigma_ui1 <- sigma_ui1 + lij2[1]*Gamma_final[j,] %*% t(Gamma_final[j,])
      lij1 <- Y[i,j] - exp(wij)/(1+exp(wij))
      sigma_ui2 <- sigma_ui2 + lij1[1]^2 * Gamma_final[j,] %*% t(Gamma_final[j,])
    }
    sigma_ui <- q* solve(sigma_ui1) %*% sigma_ui2 %*% solve(sigma_ui1)
    
    test_U <- rep(0, r)
    U_stat <- c(U_stat, sqrt(q)* sqrtm(solve(sigma_ui)) %*%(U_final[i,] - test_U))
    
  }
  
  U_stat_full <- rbind(U_stat_full, U_stat)
  
  A_hat <- am_est$A_opt
  U_hat <- U_final - X[,2:(p+1)] %*% t(A_hat)
  Beta_hat <- Beta_final[, 2:(p+1)] + Gamma_final %*% A_hat 
  Beta_hat <- cbind(Beta_final[,1], Beta_hat)
  norm0 <- normalization(Y, X, r, U_hat, Gamma_final)
  U1 <- norm0$U1
  Gamma1 <- norm0$Gamma1
  A_hat <- -A_hat
  
  for(j in 1:q){
    sigma_gj1 <- 0
    sigma_gj2 <- 0
    
    sigma_bj1 <- matrix(0, nrow=p+1, ncol=p+1)
    sigma_bj2 <- matrix(0, nrow=p+1, ncol=p+1)
    
    sigma_gamma_beta_j1 <- matrix(0, nrow=r, ncol=p+1)
    
    for(i in 1:n){
      wij <- Gamma1[j,] %*% U1[i,] + Beta_hat[j,] %*% X[i,]
      lij2 <- - exp(wij)/((1+exp(wij))^2)
      sigma_gj1 <- sigma_gj1 + lij2[1]*U1[i,] %*% t(U1[i,])
      lij1 <- Y[i,j] - exp(wij)/(1+exp(wij))
      sigma_gj2 <- sigma_gj2 + lij1[1]^2 * U1[i,] %*% t(U1[i,])
      
      xxt <- X[i,] %*% t(X[i,])
      sigma_bj1 <- sigma_bj1 + lij2[1] * xxt
      sigma_bj2 <- sigma_bj2 + lij1[1]^2 * xxt
      
      sigma_gamma_beta_j1 <- sigma_gamma_beta_j1 + lij1[1]^2 *U1[i, ] %*% t(X[i,])
    }
    sigma_gj <- solve(sigma_gj1/n) %*% (sigma_gj2/n) %*% solve(sigma_gj1/n)

    test_Gamma <- rep(0, r)
    Gamma_stat <- c(Gamma_stat, sqrt(n)*sqrtm(solve(sigma_gj)) %*%(Gamma_final[j,] - test_Gamma))
    
    rho_gamma_beta <- solve(sigma_gj1/n) %*% (sigma_gamma_beta_j1[,2:(p+1)]/n) %*% solve(sigma_bj1[2:(p+1), 2:(p+1)]/n)
    sigma_bj <- solve(sigma_bj1[2:(p+1), 2:(p+1)]/n) %*% (sigma_bj2[2:(p+1), 2:(p+1)]/n) %*% solve(sigma_bj1[2:(p+1), 2:(p+1)]/n)
    sigma_Agamma_j <-  t(A_hat) %*% sigma_gj %*% A_hat
    sigma_Agamma_beta <- t(A_hat) %*% rho_gamma_beta + t(rho_gamma_beta) %*% A_hat
    
    sigma_beta <- sigma_bj+ sigma_Agamma_j + sigma_Agamma_beta
  
    test_beta <- rep(0, p)
    Beta_stat <- rbind(Beta_stat, sqrt(n)*sqrtm(solve(sigma_beta)) %*% (Beta_final[j,2:(p+1)]-test_beta))
    
    # Test statistics
    # individual version
    beta_j_pvalue <- NULL
    for(s in 1:p){
      beta_jk_stat <- sqrt(n)*diag(sigma_beta)[s]^(-1/2)*(Beta_final[j,s+1] - 0) 
      beta_jk_pvalue <- 2*pnorm(abs(beta_jk_stat), lower.tail=F)
      beta_j_pvalue <- c(beta_j_pvalue, beta_jk_pvalue)
    }
    Beta_ind_pvalue <- rbind(Beta_ind_pvalue, beta_j_pvalue)
    

  }
  
  Gamma_stat_full <- rbind(Gamma_stat_full, Gamma_stat)
  Beta_stat_full <- rbind(Beta_stat_full, Beta_stat)
  Beta_test_full <- rbind(Beta_test_full, Beta_test)
  Beta_pvalue_full <- rbind(Beta_pvalue_full, Beta_pvalue)
  Beta_ind_pvalue_full <- rbind(Beta_ind_pvalue_full, Beta_ind_pvalue)
  
}

write.table(Beta_ind_pvalue_full,sep=",",  col.names=FALSE, row.names=FALSE, file=
              paste0('Beta11_ind_pvalue_full_n',n,'_p_',p,'_q_',q,'_r_',r,'_signal_',signal,'_nrep_',nrep,'_rho_',rho,'_ite_',ite,'.csv'))

write.table(Beta_est,sep=",",  col.names=FALSE, row.names=FALSE, file=
              paste0('Beta_est11_n',n,'_p_',p,'_q_',q,'_r_',r,'_signal_',signal,'_nrep_',nrep,'_rho_',rho,'_ite_',ite,'.csv'))

write.table(Gamma_est,sep=",",  col.names=FALSE, row.names=FALSE, file=
              paste0('Gamma_est11_n',n,'_p_',p,'_q_',q,'_r_',r,'_signal_',signal,'_nrep_',nrep,'_rho_',rho,'_ite_',ite,'.csv'))

write.table(U_est,sep=",",  col.names=FALSE, row.names=FALSE, file=
              paste0('U_est11_n',n,'_p_',p,'_q_',q,'_r_',r,'_signal_',signal,'_nrep_',nrep,'_rho_',rho,'_ite_',ite,'.csv'))
