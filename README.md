## Supplemantary Code to manuscript "Statistical Inference for Covariate-Adjusted and Interpretable Generalized Latent Factor Model with Application to Testing Fairness"

### Paper Abstract

Latent variable models are popularly used to measure latent embedding factors from large-scale assessment data. Beyond understanding these latent factors, the covariate effect on responses controlling for latent factors is also of great scientific interest and has wide applications, such as evaluating the fairness of educational testing, where the covariate effect reflects whether a test question is biased toward certain individual characteristics (e.g., gender and race), taking into account their latent abilities. However, the large sample sizes and high dimensional responses pose challenges to developing efficient methods and drawing valid inferences. Moreover, to accommodate the commonly encountered discrete responses, generalized latent factor models are often assumed, adding further complexity. To address these challenges, we consider a covariate-adjusted generalized factor model and develop novel and interpretable conditions to address the identifiability issue. Based on the identifiability conditions, we propose a joint maximum likelihood estimation method and establish estimation consistency and asymptotic normality results for the covariate effects. Furthermore, we derive estimation and inference results for latent factors and the factor loadings.  We illustrate the finite sample performance of the proposed method through extensive numerical studies and an educational assessment dataset from the Programme for International Student Assessment (PISA). 

### Simulation

Each folder contains the main scripts `main*.R` to run the proposed method and source file `functions*.R` for 

- `*` = 1: scripts to run the proposed method under the setting of Section 5 of the main text.
- 
