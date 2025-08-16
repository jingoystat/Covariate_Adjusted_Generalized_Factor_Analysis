library(haven)
library(Rcpp)
library(reshape2)
library(ggplot2)
library(dplyr)
library(mirt)
library(tidyverse)

source("functions_missing.R")

##To read in the sampling weights
df_weights=read_sas('cy07_msu_stu_qqq.sas7bdat') 

##To read in the cognitive data file
df=read_sas('cy07_msu_stu_cog.sas7bdat')

##Select variables
sampling_weights=df_weights$W_FSTUWT
gender=df_weights$ST004D01T
stratum=df_weights$STRATUM
bookid=df_weights$BOOKID
grade=df_weights$ST001D01T
birth_month=df_weights$ST003D02T
mom_level1_3=df_weights$ST005Q01TA
mom_level6=df_weights$ST006Q01TA
mom_level5A=df_weights$ST006Q02TA
mom_level5B=df_weights$ST006Q03TA
mom_level4=df_weights$ST006Q04TA
dad_level1_3=df_weights$ST007Q01TA
dad_level6=df_weights$ST008Q01TA
dad_level5A=df_weights$ST008Q02TA
dad_level5B=df_weights$ST008Q03TA
dad_level4=df_weights$ST008Q04TA
covariates1=df_weights[,c(29:44)] # In your home ...
covariates2=df_weights[,c(104:119)] # did teacher ask you to ...
covariates3=df_weights[,c(253:256)] # contact with people from other countries
covariates4=df_weights[,c(490:498)] # additional instruction

#Double-check the individual indices matches each other
student_id_questionnaire=df_weights$CNTSTUID
student_id_cog=df$CNTSTUID
all(student_id_questionnaire==student_id_cog) #False

#Re-arrange the sampling weights according to the student id in cog file
ind_weights=match(student_id_cog, student_id_questionnaire)
all(student_id_questionnaire[ind_weights]==student_id_cog) #true
sampling_weights=sampling_weights[ind_weights]
gender=gender[ind_weights]
stratum=stratum[ind_weights]
bookid=bookid[ind_weights]
grade=grade[ind_weights]
birth_month=birth_month[ind_weights]
mom_level1_3=mom_level1_3[ind_weights]
mom_level6=mom_level6[ind_weights]
mom_level5A=mom_level5A[ind_weights]
mom_level5B=mom_level5B[ind_weights]
mom_level4=mom_level4[ind_weights]
dad_level1_3=dad_level1_3[ind_weights]
dad_level6=dad_level6[ind_weights]
dad_level5A=dad_level5A[ind_weights]
dad_level5B=dad_level5B[ind_weights]
dad_level4=dad_level4[ind_weights]
covariates1=covariates1[ind_weights,]
covariates2=covariates2[ind_weights,]
covariates3=covariates3[ind_weights,]
covariates4=covariates4[ind_weights,]
#country=country[ind_weights]

#Join the sampling weights to the response data file
df=cbind.data.frame(df, sampling_weights, gender, stratum,
                    bookid, grade, birth_month, mom_level1_3, mom_level6, mom_level5A,
                    mom_level5B, mom_level4, dad_level1_3, dad_level6, dad_level5A, dad_level5B,
                    dad_level4, covariates1, covariates2, covariates3, covariates4)

item_math=c(
  'CM033Q01S', 
  'CM474Q01S', 
  'CM155Q01S',
  'CM155Q04S',
  'CM411Q01S',
  'CM411Q02S',
  'CM803Q01S',
  'CM442Q02S',
  'CM034Q01S',
  'CM305Q01S',
  'CM496Q01S',
  'CM496Q02S',
  'CM423Q01S',
  'CM192Q01S',
  'CM603Q01S',
  'CM571Q01S',
  'CM564Q01S',
  'CM564Q02S',
  'CM447Q01S',
  'CM273Q01S',
  'CM408Q01S',
  'CM420Q01S',
  'CM446Q01S',
  'CM559Q01S',
  'CM828Q03S',
  'CM464Q01S',
  'CM800Q01S',
  'CM982Q01S',
  'CM982Q02S',
  'CM982Q03S',
  'CM982Q04S',
  'CM992Q01S',
  'CM992Q02S',
  'CM915Q01S',
  'CM915Q02S',
  'CM906Q01S',
  'CM909Q01S',
  'CM909Q02S',
  'CM909Q03S',
  'CM949Q01S',
  'CM949Q02S',
  'CM00GQ01S',
  'CM955Q03S',
  'CM998Q04S',
  'CM905Q01S',
  'CM919Q01S',
  'CM919Q02S',
  'CM954Q01S',
  'CM954Q04S',
  'CM943Q01S',
  'CM943Q02S',
  'CM953Q03S',
  'CM948Q01S',
  'CM948Q02S',
  'CM948Q03S',
  'CM936Q01S',
  'CM961Q03S',
  'CM939Q01S',
  'CM939Q02S',
  'CM967Q01S',
  'CM967Q03S'
)

item_reading=c(
  'CR424Q02S',
  'CR424Q03S',
  'CR424Q07S',
  'CR220Q01S',
  'CR220Q02S',
  'CR220Q04S',
  'CR220Q05S',
  'CR220Q06S',
  'CR067Q01S',
  'DR067Q04C',
  'DR067Q05C',
  'CR456Q01S',
  'DR456Q02C',
  'DR456Q06C',
  'DR420Q02C',
  'DR420Q10C',
  'DR420Q06C',
  'DR420Q09C',
  'DR455Q02C',
  'DR455Q03C',
  'CR455Q04S',
  'CR455Q05S',
  'CR055Q01S',
  'DR055Q02C',
  'DR055Q03C',
  'DR055Q05C',
  'CR111Q01S',
  'DR111Q02BC',
  'DR111Q06C',
  'CR446Q03S',
  'DR446Q06C',
  'CR437Q01S',
  'DR437Q07C',
  'CR437Q06S',
  'CR404Q03S',
  'CR404Q06S',
  'CR404Q07S',
  'DR404Q10AC',
  'DR404Q10BC',
  'CR453Q01S',
  'DR453Q04C',
  'CR453Q05S',
  'DR453Q06C',
  'CR104Q01S',
  'CR104Q02S',
  'CR104Q05S',
  'DR466Q02C',
  'CR466Q03S',
  'CR466Q06S',
  'CR412Q01S',
  'CR412Q05S',
  'DR412Q08C',
  'CR412Q06S',
  'DR432Q01C',
  'DR432Q05C',
  'CR432Q06S',
  'DR219Q01C',
  'DR219Q01EC',
  'DR219Q02C',
  'DR460Q01C',
  'CR460Q05S',
  'CR460Q06S',
  'DR406Q01C',
  'DR406Q05C',
  'DR406Q02C',
  'CR227Q01S',
  'CR227Q02S',
  'DR227Q03C',
  'DR227Q06C',
  'DR102Q04C',
  'DR102Q05C',
  'CR102Q07S'
)

item_sci=c(
  'CS408Q01S',
  'CS408Q04S',
  'CS408Q05S',
  'CS413Q06S',
  'CS413Q04S',
  'CS413Q05S',
  'CS635Q01S',
  'CS635Q02S',
  'CS635Q04S',
  'CS604Q02S',
  'CS625Q02S',
  'CS625Q03S',
  'CS626Q01S',
  'CS626Q02S',
  'CS626Q03S',
  'CS425Q05S',
  'CS425Q02S',
  'CS438Q01S',
  'CS438Q02S',
  'CS608Q01S',
  'CS608Q02S',
  'CS608Q03S',
  'CS643Q01S',
  'CS643Q02S',
  'CS643Q04S',
  'CS610Q02S',
  'CS466Q01S',
  'CS466Q07S',
  'CS256Q01S',
  'CS326Q03S',
  'CS326Q04S',
  'CS602Q01S',
  'CS602Q02S',
  'CS602Q04S',
  'CS603Q01S',
  'CS603Q03S',
  'CS603Q04S',
  'CS603Q05S',
  'CS657Q01S',
  'CS657Q02S',
  'CS657Q03S',
  'CS527Q01S',
  'CS527Q03S',
  'CS527Q04S',
  'CS428Q01S',
  'CS428Q03S',
  'CS634Q01S',
  'CS634Q02S',
  'CS634Q04S',
  'CS629Q02S',
  'CS629Q04S',
  'CS648Q02S',
  'CS648Q03S',
  'CS498Q02S',
  'CS498Q03S',
  'CS605Q01S',
  'CS605Q02S',
  'CS605Q03S',
  'CS646Q01S',
  'CS646Q02S',
  'CS646Q03S',
  'CS620Q01S',
  'CS620Q02S',
  'CS645Q01S',
  'CS645Q03S',
  'CS478Q01S',
  'CS478Q02S',
  'CS478Q03S',
  'CS415Q07S',
  'CS415Q02S',
  'CS415Q08S',
  'CS627Q01S',
  'CS627Q03S',
  'CS627Q04S',
  'CS607Q01S',
  'CS607Q02S',
  'CS638Q01S',
  'CS638Q02S',
  'CS638Q04S',
  'CS615Q07S',
  'CS615Q01S',
  'CS615Q02S',
  'CS615Q05S'
)


#To ensure reliable analysis, we only use items with more than 10000 observations and we only consider binary scored items
#Remove items  CM961Q03S, CM939Q01S, CM939Q02S, CM967Q01S, CM948Q01S, CM948Q02S, CM948Q03S, CM936Q01S,  CM967Q03S, 
#Item CM955Q03S is with 3 categories

math_drop=c('CM961Q03S', 'CM939Q01S', 'CM939Q02S', 'CM967Q01S', 'CM948Q01S', 'CM948Q02S', 'CM948Q03S', 'CM936Q01S',  'CM967Q03S','CM955Q03S')
reading_drop=c('DR067Q04C', 'DR067Q05C', 'DR420Q10C', 'DR055Q03C', 'DR111Q02BC', 'DR111Q06C', 'CR104Q05S', 'CR227Q02S')
sci_drop=c('CS634Q02S',  'CS635Q04S',  'CS635Q01S', 'CS645Q01S')

item_math=item_math[!item_math %in% math_drop] #51
item_reading=item_reading[!item_reading %in% reading_drop] #64
item_sci=item_sci[!item_sci %in% sci_drop] #79


data_three_tests=df[, c('CNT', item_math, item_reading, item_sci)] 
dim(data_three_tests)# 606627     194


#Remove items with all NAs
ind_na_col=c()
for (i in 1:194){
  if (sum(is.na(data_three_tests[[i]]))==606627){
    ind_na_col=c(ind_na_col, i)
  }
}
ind_na_col
#There is no item with all NA's

########## TAP Clean Data ###############
TAP_data <- data_three_tests %>% filter(CNT == 'TAP')
TAP_nonull <- colSums(!is.na(TAP_data))
TAP_rank <- sort(TAP_nonull, decreasing = T)
TAP_rank

TAP_row_nonull <- rowSums(!is.na(TAP_data))
row_TAP_drop <- which(TAP_row_nonull < 10)
TAP_data <- TAP_data[-row_TAP_drop, -1]
TAP_full_data <- df %>% filter(CNT == 'TAP')
TAP_full_data <- TAP_full_data[-row_TAP_drop, ]

######### Create Y and X ###################

all_stratum <- unique(TAP_full_data$stratum)
stratum <- TAP_full_data$stratum

ind_stratum=c()
for (i in 1:length(stratum)){
  ind=which(all_stratum==stratum[i])
  ind_stratum=c(ind_stratum, ind)
}

dat_stratum=matrix(0, nrow = length(stratum), ncol = length(all_stratum))
for (i in 1: length(stratum)){
  dat_stratum[i, ind_stratum[i]]=1
}


### create feature 
junior_high <- c('TAP0101', 'TAP0102', 'TAP0103', 'TAP0104', 'TAP0105', 'TAP0106')
reg_senior_sec <- c('TAP0107', 'TAP0108', 'TAP0109', 'TAP0110', 'TAP0112')
skill_senior_sec <- c('TAP0213', 'TAP0214', 'TAP0215', 'TAP0216', 'TAP0217', 'TAP0218')
com_senior_sec <- c('TAP0219', 'TAP0220', 'TAP0221', 'TAP0222', 'TAP0223', 'TAP0224')
com_js_high <- c('TAP0325', 'TAP0326', 'TAP0327', 'TAP0328', 'TAP0329', 'TAP0330')
fiveyr_college <- c('TAP0331', 'TAP0332', 'TAP0333', 'TAP0334', 'TAP0335', 'TAP0336')

public <- c('TAP0101', 'TAP0102', 'TAP0103', 'TAP0107', 'TAP0108', 'TAP0109', 
            'TAP0213', 'TAP0214', 'TAP0215', 'TAP0219', 'TAP0220', 'TAP0221',
            'TAP0325', 'TAP0326', 'TAP0327', 'TAP0331', 'TAP0332', 'TAP0333')
private <- all_stratum[which(!all_stratum %in% public)]

urban <- c('TAP0101', 'TAP0104', 'TAP0107', 'TAP0110', 'TAP0213', 'TAP0216', 
           'TAP0219', 'TAP0222', 'TAP0325', 'TAP0328', 'TAP0331', 'TAP0334')
suburban <- c('TAP0102', 'TAP0105', 'TAP0108', 'TAP0214', 'TAP0217', 'TAP0220', 
              'TAP0223', 'TAP0326', 'TAP0329', 'TAP0332', 'TAP0335')
rural <- c('TAP0103', 'TAP0106', 'TAP0109', 'TAP0112', 'TAP0215', 'TAP0218', 
           'TAP0221', 'TAP0224', 'TAP0327', 'TAP0330', 'TAP0333', 'TAP0336')

df_main <- cbind(public_school = as.integer(stratum %in% public),
                 urb = as.integer(stratum %in% urban),
                 rural_place = as.integer(stratum %in% rural),
                 junior_h = as.integer(stratum %in% junior_high),
                 reg_ss = as.integer(stratum %in% reg_senior_sec),
                 skill = as.integer(stratum %in% skill_senior_sec),
                 com_junior_senior = as.integer(stratum %in% com_js_high),
                 five_year = as.integer(stratum %in% fiveyr_college))



Y <- as.matrix(TAP_data)
X <- cbind(df_main, TAP_full_data[, names(TAP_full_data) %in% c('gender')])
X[,9] <- X[,9]-1
n <- dim(Y)[1]
X <- cbind(rep(1,n), X)
X <- as.matrix(X)

save(Y, file="Y_gfa.RData")
save(X, file="X_gfa.RData")

female_idx <- which(X[,10]==0)
male_idx <- which(X[,10]==1)

####### TAP Estimation ########################
source("functions_missing.R")
seed <- 24
set.seed(seed)

# initial value generation
n <- dim(Y)[1]
q <- dim(Y)[2]
p <- dim(X)[2]
r <- 3


TAP_est<- AM_estimation(Y, X, r, tol=0.001, tau=5, ite=3, maxiter=20, seed)
Gamma_final <- TAP_est$Gamma1
Beta_final <- TAP_est$Beta1
U_final <- TAP_est$U1

female_idx <- which(X[,10]==0)
male_idx <- which(X[,10]==1)
hist(U_final[female_idx,], freq = F)
hist(U_final[male_idx,], freq = F)

public_idx <- which(X[,2]==1)
nonpublic_idx <- which(X[,2]==0)
hist(U_final[public_idx,], freq = F)
hist(U_final[nonpublic_idx,], freq = F)


U_test <- NULL
Gamma_test <- NULL
Beta_test <- NULL
Beta_ind_pvalue <- NULL
Beta_ind_upper <- NULL
Beta_ind_lower <- NULL


for(i in 1:n){
  sigma_ui1 <- 0
  sigma_ui2 <- 0
  index_Ji <- which(!is.na(Y[i,]))
  
  for(j in index_Ji){
    wij <- Gamma_final[j,] %*% U_final[i,] + Beta_final[j,] %*% X[i,]
    lij2 <- - exp(wij)/(1+exp(wij))^2
    sigma_ui1 <- sigma_ui1 + lij2[1]*Gamma_final[j,] %*% t(Gamma_final[j,])
    lij1 <- Y[i,j] - exp(wij)/(1+exp(wij))
    sigma_ui2 <- sigma_ui2 + lij1[1]^2 * Gamma_final[j,] %*% t(Gamma_final[j,])
  }
  sigma_ui <- q* solve(sigma_ui1) %*% sigma_ui2 %*% solve(sigma_ui1)
  U_test <- c(U_test, sqrt(q)* sqrtm(solve(sigma_ui)) %*%(U_final[i,]))
}

A_hat <- TAP_est$A_opt
U_hat <- U_final - X[,2:p] %*% t(A_hat)
Beta_hat <- Beta_final[, 2:p] + Gamma_final %*% A_hat 
Beta_hat <- cbind(Beta_final[,1], Beta_hat)
norm0 <- normalization(Y, X, r, U_hat, Gamma_final)
U1 <- norm0$U1
Gamma1 <- norm0$Gamma1
A_hat <- -A_hat


Beta_stat<- NULL
for(j in 1:q){
  sigma_gj1 <- 0
  sigma_gj2 <- 0
  
  sigma_bj1 <- matrix(0, nrow=p, ncol=p)
  sigma_bj2 <- matrix(0, nrow=p, ncol=p)
  
  sigma_gamma_beta_j1 <- matrix(0, nrow=r, ncol=p)
  
  index_Nj <- which(!is.na(Y[,j]))
  for(i in index_Nj){
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
  sigma_gj <- solve(sigma_gj1/n) * (sigma_gj2/n) * solve(sigma_gj1/n)
  Gamma_test <- c(Gamma_test, sqrt(n)*sqrtm(solve(sigma_gj)) %*%(Gamma_final[j,]))
  
  
  rho_gamma_beta <- solve(sigma_gj1/n) %*% (sigma_gamma_beta_j1[,2:p]/n) %*% solve(sigma_bj1[2:p, 2:p]/n)
  sigma_bj <- solve(sigma_bj1[2:p, 2:p]/n) %*% (sigma_bj2[2:p, 2:p]/n) %*% solve(sigma_bj1[2:p, 2:p]/n)
  sigma_Agamma_j <-  t(A_hat) %*% sigma_gj %*% A_hat
  sigma_Agamma_beta <- t(A_hat) %*% rho_gamma_beta + t(rho_gamma_beta) %*% A_hat
  
  sigma_beta <- sigma_bj+ sigma_Agamma_j + sigma_Agamma_beta
  
  Beta_test <- rbind(Beta_test, sqrt(n)*sqrtm(solve(sigma_beta)) %*% (Beta_final[j,2:p]))

  
  # Confidence intervals
  beta_j_pvalue <- NULL
  beta_j_upper <- NULL
  beta_j_lower <- NULL
  beta_j_stat <- NULL

  for(s in 1:p-1){
    beta_jk_stat <- sqrt(n)*diag(sigma_bj)[s]^(-1/2)*(Beta_final[j,s+1] - 0)
    beta_jk_pvalue <- 2*pnorm(abs(beta_jk_stat), lower.tail=F)
    beta_j_pvalue <- c(beta_j_pvalue, beta_jk_pvalue)
    
    beta_jk_upper <- Beta_final[j,s+1] + 1.96*diag(sigma_bj)[s]^(1/2)/sqrt(n)
    beta_jk_lower <- Beta_final[j,s+1] - 1.96*diag(sigma_bj)[s]^(1/2)/sqrt(n)
    
    beta_j_upper <- c(beta_j_upper, beta_jk_upper)
    beta_j_lower <- c(beta_j_lower, beta_jk_lower)
    beta_j_stat <- c(beta_j_stat, beta_jk_stat)

  }
  Beta_ind_pvalue <- rbind(Beta_ind_pvalue, beta_j_pvalue)
  Beta_ind_upper <- rbind(Beta_ind_upper, beta_j_upper)
  Beta_ind_lower <- rbind(Beta_ind_lower, beta_j_lower)
  Beta_stat <- rbind(Beta_stat, beta_j_stat)

}


############### TAP gender plot ####################################
gender_beta <- Beta_final[,10]
gender_upper <- Beta_ind_upper[,9]
gender_lower <- Beta_ind_lower[,9]
gender_pvalue <- Beta_ind_pvalue[,9]
gender_results <- cbind(gender_beta, gender_lower, gender_upper, gender_pvalue, Beta_stat[,9], Beta_stat[,9]^2)

gender_biased_item <- which(gender_pvalue<0.05/q)
gender_biased_item
colnames(Y)[gender_biased_item] 

pos <- seq(1:q)
TAP_gender <- cbind(gender_results, pos)
TAP_gender_bias <- TAP_gender[gender_biased_item,]

write.table(TAP_gender,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_gender_seed_',seed,'.csv'))
write.table(TAP_gender_bias,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_gender_bias_seed_',seed,'.csv'))


TAP_gender_plot <- ggplot(data=as.data.frame(TAP_gender)) +
  geom_point(aes(x=1:q,y=gender_beta), alpha = 0)+
  geom_point(data=as.data.frame(TAP_gender_bias), aes(x=gender_biased_item, y=gender_beta), color = "black")+
  geom_errorbar(mapping = aes(x=1:q,ymin=gender_lower, ymax=gender_upper), 
                width=1,
                color="grey",
                alpha = 0.5)+
  geom_errorbar(data=as.data.frame(TAP_gender_bias),
                mapping = aes(x=gender_biased_item, ymin=gender_lower, ymax=gender_upper), 
                width=4,
                size=0.7,
                color="black")+
  ylim(-6, 6)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1, alpha = 0.5) +
  xlab("PISA Questions for TAP")+
  ylab("Gender Effect Estimator")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 16, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))+
  geom_vline(xintercept = 51.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  geom_vline(xintercept = 105.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  annotate("text", x = 25, y = 6, label = "Math", size=7, fontface=3)+
  annotate("text", x = 80, y = 6, label = "Reading", size=7, fontface=3)+
  annotate("text", x = 150, y = 6, label = "Science", size=7, fontface=3)
TAP_gender_plot


#### public
public_beta <- Beta_final[,2]
public_upper <- Beta_ind_upper[,1]
public_lower <- Beta_ind_lower[,1]
public_pvalue <- Beta_ind_pvalue[,1]
public_results <- cbind(public_beta, public_lower, public_upper, public_pvalue, Beta_stat[,1], Beta_stat[,1]^2)

public_biased_item <- which(public_pvalue<0.05/q)
public_biased_item
colnames(Y)[public_biased_item] 

pos <- seq(1:q)
TAP_public <- cbind(public_results, pos)
TAP_public_bias <- TAP_public[public_biased_item,]

write.table(TAP_public,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_public_seed_',seed,'.csv'))
write.table(TAP_public_bias,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_public_bias_seed_',seed,'.csv'))


TAP_public_plot <- ggplot(data=as.data.frame(TAP_public)) +
  geom_point(aes(x=1:q,y=public_beta), alpha=0)+
  geom_point(data=as.data.frame(TAP_public_bias), aes(x=TAP_public_bias[,5], y=TAP_public_bias[,1]), color = "black")+
  geom_errorbar(mapping = aes(x=1:q,ymin=public_lower, ymax=public_upper), 
                width=1,
                color="grey",
                alpha=0.5)+
  geom_errorbar(data=as.data.frame(TAP_public_bias),
                mapping = aes(x=TAP_public_bias[,5], ymin=TAP_public_bias[,2], ymax=TAP_public_bias[,3]), 
                width=2,
                size=0.7,
                color="black")+
  ylim(-6, 6)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1,  alpha=0.5) +
  xlab("PISA Questions for TAP")+
  ylab("Public School Effect Estimator")+
  ggtitle("Public")+
  theme_bw()+
 theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=25),
        axis.title.x = element_text(size = 22, margin = margin(t = 12, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 12, b = 0, l = 0)),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  geom_vline(xintercept = 51.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  geom_vline(xintercept = 105.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  annotate("text", x = 25, y = 6, label = "Math", size=7, fontface=3)+
  annotate("text", x = 80, y = 6, label = "Reading", size=7, fontface=3)+
  annotate("text", x = 150, y = 6, label = "Science", size=7, fontface=3)
TAP_public_plot

#### urban
urban_beta <- Beta_final[,3]
urban_upper <- Beta_ind_upper[,2]
urban_lower <- Beta_ind_lower[,2]
urban_pvalue <- Beta_ind_pvalue[,2]
urban_results <- cbind(urban_beta, urban_lower, urban_upper, urban_pvalue, Beta_stat[,2], Beta_stat[,2]^2)

urban_biased_item <- which(urban_pvalue<0.05/q)
urban_biased_item
colnames(Y)[urban_biased_item] 

pos <- seq(1:q)
TAP_urban <- cbind(urban_results, pos)
TAP_urban_bias <- TAP_urban[urban_biased_item,]

write.table(TAP_urban,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_urban_seed_',seed,'.csv'))
write.table(TAP_urban_bias,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_urban_bias_seed_',seed,'.csv'))


TAP_urban_plot <- ggplot(data=as.data.frame(TAP_urban)) +
  geom_point(aes(x=1:q,y=urban_beta), alpha=0)+
  geom_point(data=as.data.frame(TAP_urban_bias), aes(x=TAP_urban_bias[,5], y=TAP_urban_bias[,1]), color = "black")+
  geom_errorbar(mapping = aes(x=1:q,ymin=urban_lower, ymax=urban_upper), 
                width=1,
                color="grey",
                alpha=0.5)+
  geom_errorbar(data=as.data.frame(TAP_urban_bias),
                mapping = aes(x=TAP_urban_bias[,5], ymin=TAP_urban_bias[,2], ymax=TAP_urban_bias[,3]), 
                width=4,
                size=0.7,
                color="black")+
  ylim(-6, 6)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1, alpha=0.5) +
  xlab("PISA Questions for TAP")+
  ylab("Effect Estimator")+
  ggtitle("Urban")+
  theme_bw()+
 theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=25),
        axis.title.x = element_text(size = 22, margin = margin(t = 12, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 12, b = 0, l = 0)),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  geom_vline(xintercept = 51.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  geom_vline(xintercept = 105.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  annotate("text", x = 25, y = 6, label = "Math", size=7, fontface=3)+
  annotate("text", x = 80, y = 6, label = "Reading", size=7, fontface=3)+
  annotate("text", x = 150, y = 6, label = "Science", size=7, fontface=3)
TAP_urban_plot

#### Rural
rural_beta <- Beta_final[,4]
rural_upper <- Beta_ind_upper[,3]
rural_lower <- Beta_ind_lower[,3]
rural_pvalue <- Beta_ind_pvalue[,3]
rural_results <- cbind(rural_beta, rural_lower, rural_upper, rural_pvalue, Beta_stat[,3], Beta_stat[,3]^2)

rural_biased_item <- which(rural_pvalue<0.05/q)
rural_biased_item
colnames(Y)[rural_biased_item] 

pos <- seq(1:q)
TAP_rural <- cbind(rural_results, pos)
TAP_rural_bias <- TAP_rural[rural_biased_item,]

write.table(TAP_rural,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_rural_seed_',seed,'.csv'))
write.table(TAP_rural_bias,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_rural_bias_seed_',seed,'.csv'))


TAP_rural_plot <- ggplot(data=as.data.frame(TAP_rural)) +
  geom_point(aes(x=1:q,y=rural_beta), alpha=0)+
  geom_point(data=as.data.frame(TAP_rural_bias), aes(x=rural_biased_item, y=rural_beta), color = "black")+
  geom_errorbar(mapping = aes(x=1:q,ymin=rural_lower, ymax=rural_upper), 
                width=1,
                color="grey",
                alpha=0.5)+
  geom_errorbar(data=as.data.frame(TAP_rural_bias),
                mapping = aes(x=rural_biased_item, ymin=rural_lower, ymax=rural_upper), 
                width=2,
                size=0.7,
                color="black")+
  ylim(-7, 7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1, alpha=0.5) +
  xlab("PISA Questions for TAP")+
  ylab("Effect Estimator")+
  ggtitle("Rural")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=25),
        axis.title.x = element_text(size = 22, margin = margin(t = 12, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 12, b = 0, l = 0)),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  geom_vline(xintercept = 51.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  geom_vline(xintercept = 105.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  annotate("text", x = 25, y = 7, label = "Math", size=7, fontface=3)+
  annotate("text", x = 80, y = 7, label = "Reading", size=7, fontface=3)+
  annotate("text", x = 150, y = 7, label = "Science", size=7, fontface=3)
TAP_rural_plot

#### Junior
junior_beta <- Beta_final[,5]
junior_upper <- Beta_ind_upper[,4]
junior_lower <- Beta_ind_lower[,4]
junior_pvalue <- Beta_ind_pvalue[,4]
junior_results <- cbind(junior_beta, junior_lower, junior_upper, junior_pvalue, Beta_stat[,4], Beta_stat[,4]^2)

junior_biased_item <- which(junior_pvalue<0.05/q)
junior_biased_item
colnames(Y)[junior_biased_item] 

pos <- seq(1:q)
TAP_junior <- cbind(junior_results, pos)
TAP_junior_bias <- TAP_junior[junior_biased_item,]

write.table(TAP_junior,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_junior_seed_',seed,'.csv'))
write.table(TAP_junior_bias,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_junior_bias_seed_',seed,'.csv'))


TAP_junior_plot <- ggplot(data=as.data.frame(TAP_junior)) +
  geom_point(aes(x=1:q,y=junior_beta), alpha=0)+
  geom_point(data=as.data.frame(TAP_junior_bias), aes(x=junior_biased_item, y=junior_beta), color = "black")+
  geom_errorbar(mapping = aes(x=1:q,ymin=junior_lower, ymax=junior_upper), 
                width=1,
                color="grey",
                alpha=0.5)+
  geom_errorbar(data=as.data.frame(TAP_junior_bias),
                mapping = aes(x=junior_biased_item, ymin=junior_lower, ymax=junior_upper), 
                width=2,
                size=0.7,
                color="black")+
  ylim(-7, 7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1, alpha=0.5) +
  xlab("PISA Questions for TAP")+
  ylab("Effect Estimator")+
  ggtitle("Junior")+
  theme_bw()+
 theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=25),
        axis.title.x = element_text(size = 22, margin = margin(t = 12, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 12, b = 0, l = 0)),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  geom_vline(xintercept = 51.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  geom_vline(xintercept = 105.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  annotate("text", x = 25, y = 7, label = "Math", size=7, fontface=3)+
  annotate("text", x = 80, y = 7, label = "Reading", size=7, fontface=3)+
  annotate("text", x = 150, y = 7, label = "Science", size=7, fontface=3)
TAP_junior_plot


##### regular senior secondary 
reg_ss_beta <- Beta_final[,6]
reg_ss_upper <- Beta_ind_upper[,5]
reg_ss_lower <- Beta_ind_lower[,5]
reg_ss_pvalue <- Beta_ind_pvalue[,5]
reg_ss_results <- cbind(reg_ss_beta, reg_ss_lower, reg_ss_upper, reg_ss_pvalue, Beta_stat[,5], Beta_stat[,5]^2)

reg_ss_biased_item <- which(reg_ss_pvalue<0.05/q)
reg_ss_biased_item
colnames(Y)[reg_ss_biased_item] 

pos <- seq(1:q)
TAP_reg_ss <- cbind(reg_ss_results, pos)
TAP_reg_ss_bias <- TAP_reg_ss[reg_ss_biased_item,]


write.table(TAP_reg_ss,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_reg_ss_seed_',seed,'.csv'))
write.table(TAP_reg_ss_bias,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_reg_ss_bias_seed_',seed,'.csv'))


TAP_reg_ss_plot <- ggplot(data=as.data.frame(TAP_reg_ss)) +
  geom_point(aes(x=1:q,y=reg_ss_beta), alpha=0)+
  geom_point(data=as.data.frame(TAP_reg_ss_bias), aes(x=reg_ss_biased_item, y=reg_ss_beta), color = "black")+
  geom_errorbar(mapping = aes(x=1:q,ymin=reg_ss_lower, ymax=reg_ss_upper), 
                width=1,
                color="grey",
                alpha=0.5)+
  geom_errorbar(data=as.data.frame(TAP_reg_ss_bias),
                mapping = aes(x=reg_ss_biased_item, ymin=reg_ss_lower, ymax=reg_ss_upper), 
                width=2,
                size=0.7,
                color="black")+
  ylim(-7, 7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1, alpha=0.5) +
  xlab("PISA Questions for TAP")+
  ylab("Effect Estimator")+
  ggtitle("Regular Senior Secondary")+
  theme_bw()+
 theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=25),
        axis.title.x = element_text(size = 22, margin = margin(t = 12, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 12, b = 0, l = 0)),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  geom_vline(xintercept = 51.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  geom_vline(xintercept = 105.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  annotate("text", x = 25, y = 7, label = "Math", size=7, fontface=3)+
  annotate("text", x = 80, y = 7, label = "Reading", size=7, fontface=3)+
  annotate("text", x = 150, y = 7, label = "Science", size=7, fontface=3)
TAP_reg_ss_plot

#### Skill-Based Senior Secondary
skill_beta <- Beta_final[,7]
skill_upper <- Beta_ind_upper[,6]
skill_lower <- Beta_ind_lower[,6]
skill_pvalue <- Beta_ind_pvalue[,6]
skill_results <- cbind(skill_beta, skill_lower, skill_upper, skill_pvalue, Beta_stat[,6], Beta_stat[,6]^2)

skill_biased_item <- which(skill_pvalue<0.05/q)
skill_biased_item
colnames(Y)[skill_biased_item] 

pos <- seq(1:q)
TAP_skill <- cbind(skill_results, pos)
TAP_skill_bias <- TAP_skill[skill_biased_item,]

write.table(TAP_skill,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_skill_seed_',seed,'.csv'))
write.table(TAP_skill_bias,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_skill_bias_seed_',seed,'.csv'))


TAP_skill_plot <- ggplot(data=as.data.frame(TAP_skill)) +
  geom_point(aes(x=1:q,y=skill_beta), alpha=0)+
  geom_point(data=as.data.frame(TAP_skill_bias), aes(x=skill_biased_item, y=skill_beta), color = "black")+
  geom_errorbar(mapping = aes(x=1:q,ymin=skill_lower, ymax=skill_upper), 
                width=1,
                color="grey",
                alpha=0.5)+
  geom_errorbar(data=as.data.frame(TAP_skill_bias),
                mapping = aes(x=skill_biased_item, ymin=skill_lower, ymax=skill_upper), 
                width=2,
                size=0.7,
                color="black")+
  ylim(-7, 7)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1, alpha=0.5) +
  xlab("PISA Questions for TAP")+
  ylab("Effect Estimator")+
  ggtitle("Skill-based Senior Secondary")+
  theme_bw()+
 theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=25),
        axis.title.x = element_text(size = 22, margin = margin(t = 12, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 12, b = 0, l = 0)),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  geom_vline(xintercept = 51.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  geom_vline(xintercept = 105.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  annotate("text", x = 25, y = 7, label = "Math", size=7, fontface=3)+
  annotate("text", x = 80, y = 7, label = "Reading", size=7, fontface=3)+
  annotate("text", x = 150, y = 7, label = "Science", size=7, fontface=3)
TAP_skill_plot

#### Comprehensive senior secondary
com_beta <- Beta_final[,8]
com_upper <- Beta_ind_upper[,7]
com_lower <- Beta_ind_lower[,7]
com_pvalue <- Beta_ind_pvalue[,7]
com_results <- cbind(com_beta, com_lower, com_upper, com_pvalue, Beta_stat[,7], Beta_stat[,7]^2)

com_biased_item <- which(com_pvalue<0.05/q)
com_biased_item
colnames(Y)[com_biased_item] 

pos <- seq(1:q)
TAP_com <- cbind(com_results, pos)
TAP_com_bias <- TAP_com[com_biased_item,]

write.table(TAP_com,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_com_seed_',seed,'.csv'))
write.table(TAP_com_bias,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_com_bias_seed_',seed,'.csv'))


TAP_com_plot <- ggplot(data=as.data.frame(TAP_com)) +
  geom_point(aes(x=1:q,y=com_beta), alpha=0)+
  geom_point(data=as.data.frame(TAP_com_bias), aes(x=com_biased_item, y=com_beta), color = "black")+
  geom_errorbar(mapping = aes(x=1:q,ymin=com_lower, ymax=com_upper), 
                width=1,
                alpha=0.5,
                color="grey")+
  geom_errorbar(data=as.data.frame(TAP_com_bias),
                mapping = aes(x=com_biased_item, ymin=com_lower, ymax=com_upper), 
                width=2,
                size=0.7,
                color="black")+
  ylim(-8, 8)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1, alpha=0.5) +
  xlab("PISA Questions for TAP")+
  ylab("Effect Estimator")+
  ggtitle("Comprehensive Senior Secondary")+
  theme_bw()+
 theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=25),
        axis.title.x = element_text(size = 22, margin = margin(t = 12, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 12, b = 0, l = 0)),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  geom_vline(xintercept = 51.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  geom_vline(xintercept = 105.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  annotate("text", x = 25, y = 8, label = "Math", size=7, fontface=3)+
  annotate("text", x = 80, y = 8, label = "Reading", size=7, fontface=3)+
  annotate("text", x = 150, y = 8, label = "Science", size=7, fontface=3)
TAP_com_plot


five_beta <- Beta_final[,9]
five_upper <- Beta_ind_upper[,8]
five_lower <- Beta_ind_lower[,8]
five_pvalue <- Beta_ind_pvalue[,8]
five_results <- cbind(five_beta, five_lower, five_upper, five_pvalue, Beta_stat[,8], Beta_stat[,8]^2)

five_biased_item <- which(five_pvalue<0.05/q)
five_biased_item
colnames(Y)[five_biased_item] 

pos <- seq(1:q)
TAP_five <- cbind(five_results, pos)
TAP_five_bias <- TAP_five[five_biased_item,]

write.table(TAP_five,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_five_seed_',seed,'.csv'))
write.table(TAP_five_bias,sep=",",  col.names=FALSE, row.names=FALSE, file=paste0('TAP_five_bias_seed_',seed,'.csv'))


TAP_five_plot <- ggplot(data=as.data.frame(TAP_five)) +
  geom_point(aes(x=1:q,y=five_beta), alpha=0)+
  geom_point(data=as.data.frame(TAP_five_bias), aes(x=five_biased_item, y=five_beta), color = "black")+
  geom_errorbar(mapping = aes(x=1:q,ymin=five_lower, ymax=five_upper), 
                width=1,
                alpha=0.5,
                color="grey")+
  geom_errorbar(data=as.data.frame(TAP_five_bias),
                mapping = aes(x=five_biased_item, ymin=five_lower, ymax=five_upper), 
                width=2,
                size=0.7,
                color="black")+
  ylim(-8, 8)+
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1, alpha=0.5) +
  xlab("PISA Questions for TAP")+
  ylab("Effect Estimator")+
  ggtitle("Five-year college")+
  theme_bw()+
 theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=25),
        axis.title.x = element_text(size = 22, margin = margin(t = 12, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 12, b = 0, l = 0)),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  geom_vline(xintercept = 51.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  geom_vline(xintercept = 105.5, linetype="dashed", 
             color = "black", alpha = 0.5)+
  annotate("text", x = 25, y = 8, label = "Math", size=7, fontface=3)+
  annotate("text", x = 80, y = 8, label = "Reading", size=7, fontface=3)+
  annotate("text", x = 150, y = 8, label = "Science", size=7, fontface=3)
TAP_five_plot



