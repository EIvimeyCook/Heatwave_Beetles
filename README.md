# Long-term evolution under heatwave conditions in the seed beetle, Callosobruchus maculatus

## Overview
Heatwaves, temporary periods of elevated temperatures, are increasing both in magnitude and in frequency and have devastating effects on many taxa. However, to date, most studies investigating the impacts of heatwaves have focused on populations that have evolved under constant conditions prior to assaying or have only investigated the short-term outcomes. Here, using the seed beetle, Callosobruchus maculatus, we investigated the long-term effects of evolution after 43 generations of fluctuating temperature with added heatwave exposure (a +2C increase peaking at 42C across 7 days, once every generation) on two important life history traits, development time and lifetime reproductive success (LRS). We find that populations that evolved under heatwave conditions eclosed at similar times but had lower LRS than those that evolved and were assayed under fluctuating conditions. When assayed at a novel constant and benign temperature of 29°C, beetles from both thermal regimes developed slower but had similar LRS. Together, this suggests that long-term heatwave exposure may incur only a very small cost to fitness, which disappears when individuals from those populations are exposed to benign control conditions. This study emphasises the potency of long-term multigenerational exposure to heatwaves in order to understand how populations respond to climate change.  

## Authors
Edward R. Ivimey-Cook, Sarah Glavan, Sophie Bricout, Claudio Piani, and Elena C. Berg

## Affiliation
School of Biological Sciences, University of East Anglia, UK
The American University of Paris, Paris, France


## Contact
📧 Edward R. Ivimey-Cook: e.ivimeycook@gmail.com Elena C. Berg: eberg@aup.edu

## Funding
The American University of Paris

## Data Files

### `Data/Analysis_dat.csv`

**Dimensions:** 22700 rows × 14 columns

**Variables:**

- `ID`: categorical | 600 unique values | NAs: 0: ID of the Individual
- `Group`: categorical | 12 unique values | NAs: 0: 12 level group
- `Treatment`: categorical | levels: F-C, F-F, H-C, H-H | NAs: 0: Treatment Group
- `Replicate`: numeric | range 1–3 | mean 2.004 | NAs: 0 | **units:** Replicate Number
- `Pairing date`: categorical | levels: 01-Feb-24, 10-Apr-24 | NAs: 0: Date of Pairing
- `Pair ID`: numeric | range 1–50 | mean 25.5 | NAs: 0 | **units:** ID of Pair
- `Day egg laid`: categorical | levels: 0, 1, 2+ | NAs: 0: Day egg was laid
- `Date egg laid`: categorical | levels: 01-Feb-24, 02-Feb-24, 03-Feb-24, 10-Apr-24, 11-Apr-24, 12-Apr-24 | NAs: 0: Date of laying
- `Date of measure`: categorical | 31 unique values | NAs: 0: Date of measuring
- `Devt time`: numeric | range 19–34 | mean 25.61 | NAs: 0 | **units:** Development /Days
- `males`: numeric | range 0–19 | mean 0.8877 | NAs: 0 | **units:** Number of Males
- `females`: numeric | range 0–18 | mean 0.869 | NAs: 0 | **units:** Number of Males
- `Total`: numeric | range 0–29 | mean 1.757 | NAs: 0 | **units:** Total Number of Individuals
- `Comments`: logical | TRUE: 0 | FALSE: 0 | NAs: 22700: Comments

### `sessionInfo.txt`

R version 4.4.2 (2024-10-31)
Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.3.2

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/London
tzcode source: internal

attached base packages:
[1] grid      stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] ragg_1.3.3           colorspace_2.1-1     ggtext_0.1.2         palmerpenguins_0.1.1
 [5] ggdist_3.3.2         ggrepel_0.9.6        gghalves_0.1.4       PupillometryR_0.0.5 
 [9] rlang_1.1.5          ggthemes_5.1.0       robustlmm_3.3-1      lmerTest_3.1-3      
[13] lme4_1.1-36          Matrix_1.7-1         janitor_2.2.1        sjPlot_2.8.17       
[17] broom_1.0.7          MuMIn_1.48.4         magrittr_2.0.3       patchwork_1.3.0     
[21] ggeffects_2.2.1      MASS_7.3-61          see_0.11.0           report_0.6.1        
[25] parameters_0.24.2    performance_0.13.0   modelbased_0.10.0    insight_1.1.0       
[29] effectsize_1.0.0     datawizard_1.0.1     correlation_0.8.7    bayestestR_0.15.2   
[33] easystats_0.7.4      emmeans_1.10.7       DHARMa_0.4.7         glmmTMB_1.1.10      
[37] hablar_0.3.2         lubridate_1.9.4      forcats_1.0.0        stringr_1.5.1       
[41] dplyr_1.1.4          purrr_1.0.4          readr_2.1.5          tidyr_1.3.1         
[45] tibble_3.2.1         ggplot2_3.5.1        tidyverse_2.0.0     

loaded via a namespace (and not attached):
 [1] Rdpack_2.6.2         snakecase_0.11.1     compiler_4.4.2       mgcv_1.9-1          
 [5] systemfonts_1.2.1    vctrs_0.6.5          pkgconfig_2.0.3      crayon_1.5.3        
 [9] backports_1.5.0      tzdb_0.4.0           nloptr_2.2.0         bit_4.6.0           
[13] xfun_0.51            sjmisc_2.8.10        parallel_4.4.2       R6_2.6.1            
[17] stringi_1.8.4        boot_1.3-31          numDeriv_2016.8-1.1  estimability_1.5.1  
[21] Rcpp_1.0.14          knitr_1.49           pacman_0.5.1         splines_4.4.2       
[25] timechange_0.3.0     tidyselect_1.2.1     rstudioapi_0.17.1    TMB_1.9.17          
[29] codetools_0.2-20     sjlabelled_1.2.0     lattice_0.22-6       withr_3.0.2         
[33] evaluate_1.0.3       xml2_1.3.7           pillar_1.10.1        renv_1.0.7          
[37] stats4_4.4.2         reformulas_0.4.0     distributional_0.5.0 generics_0.1.3      
[41] vroom_1.6.5          hms_1.1.3            munsell_0.5.1        scales_1.3.0        
[45] minqa_1.2.8          xtable_1.8-4         glue_1.8.0           fastGHQuad_1.0.1    
[49] tools_4.4.2          data.table_1.17.0    robustbase_0.99-4-1  mvtnorm_1.3-3       
[53] rbibutils_2.3        nlme_3.1-166         cli_3.6.4            textshaping_1.0.0   
[57] sjstats_0.19.0       gtable_0.3.6         DEoptimR_1.1-3-1     farver_2.1.2        
[61] lifecycle_1.0.4      gridtext_0.1.5       bit64_4.6.0-1       

## Other Files

### `renv.lock`

lockfile for renv

## Code
Scripts should be run in the following order:


1. **`Script/01-load_packages.R`**

2. **`Script/02-load_tidy_data.R`**

3. **`Script/03-model.R`**

4. **`Script/04-figures.R`**

---
*README generated with READMEBuilder on 21 April 2026.*
