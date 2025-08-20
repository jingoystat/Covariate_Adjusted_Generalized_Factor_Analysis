## Supplemantary Code to manuscript "Statistical Inference for Covariate-Adjusted and Interpretable Generalized Latent Factor Model with Application to Testing Fairness"

### Paper Abstract

Latent variable models are popularly used to measure latent embedding factors from large-scale assessment data. Beyond understanding these latent factors, the covariate effect on responses controlling for latent factors is also of great scientific interest and has wide applications, such as evaluating the fairness of educational testing, where the covariate effect reflects whether a test question is biased toward certain individual characteristics (e.g., gender and race), taking into account their latent abilities. However, the large sample sizes and high dimensional responses pose challenges to developing efficient methods and drawing valid inferences. Moreover, to accommodate the commonly encountered discrete responses, generalized latent factor models are often assumed, adding further complexity. To address these challenges, we consider a covariate-adjusted generalized factor model and develop novel and interpretable conditions to address the identifiability issue. Based on the identifiability conditions, we propose a joint maximum likelihood estimation method and establish estimation consistency and asymptotic normality results for the covariate effects. Furthermore, we derive estimation and inference results for latent factors and the factor loadings.  We illustrate the finite sample performance of the proposed method through extensive numerical studies and an educational assessment dataset from the Programme for International Student Assessment (PISA). 

### Simulation

Each simulation folder includes a main script `main*.R` (to run the experiment) and a source file `functions*.R`. 

- Sub-directory 1 (`* = 1`): scripts for the setting in Section 5 of the main text.
- Sub-directory 2 (`* = 2`): scripts for the setting of Section F.2 of the Supplementary Materials. 
- Sub-directory 3 (`* = 3`): scripts for the setting of Section F.3 of the Supplementary Materials. 
- Sub-directory 4 (`* = 4`): scripts for the setting of Section F.4 of the Supplementary Materials. 
- Sub-directory 5 (`* = 5`): scripts for the setting of Section F.5 of the Supplementary Materials. 
- Sub-directory 6 (`* = 6`): scripts for the setting of Section F.6 of the Supplementary Materials. 
- Sub-directory 7 (`* = 7`): scripts for the setting of Section F.7 of the Supplementary Materials. 
- Sub-directory 8 (`* = 8`): scripts for the setting of Section F.8 of the Supplementary Materials.

### Application

The raw datasets (`CY07MSP_STU_QQQ.SAS7BDAT` and `CY07MSP_STU_COG.SAS7BDAT`) can be downloaded from [OECD PISA 2018 Database](https://www.oecd.org/en/data/datasets/pisa-2018-database.html#data).

The main script `real-data-TAP.R` runs the proposed method on the PISA 2018 data and produces point and interval estimation results for the gender effect and school strata covariate effects. Source code are provided in  `functions_missing.R`.

