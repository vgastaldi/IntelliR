#### IntelliCage data processing ####
## Author: Vinicius Daguano Gastaldi
## This script is required to start the file processing. A new script will be called depending on the operational system.
# Modified: 13.08.2024
#### Required packages ####
if (grepl("Windows", Sys.info()["sysname"])) {
  required_packages <- c("dplyr","openxlsx","ggplot2","reshape2","tcltk","multcomp","coin","dunn.test","car","tidyr","effectsize","emmeans","boot","rstatix","conover.test","shiny")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  if (packageVersion("ggplot2") < "3.4.2") {
    install.packages("ggplot2")
  }
  library(openxlsx)
  library(ggplot2)
  library(reshape2)
  library(multcomp)
  library(coin)
  library(dunn.test)
  library(car)
  library(dplyr)
  library(tcltk)
  library(shiny)
  library(tidyr)
  library(effectsize)
  library(emmeans)
  library(boot)
  library(rstatix)
  library(conover.test)
  rm(new_packages,required_packages)
} else {
  required_packages <- c("dplyr","openxlsx","ggplot2","reshape2","tcltk","multcomp","coin","dunn.test","car","tidyr","effectsize","emmeans","boot","rstatix","conover.test","shiny")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  if (packageVersion("ggplot2") < "3.4.2") {
    install.packages("ggplot2")
  }
  library(openxlsx)
  library(ggplot2)
  library(reshape2)
  library(multcomp)
  library(coin)
  library(dunn.test)
  library(car)
  library(dplyr)
  library(tcltk)
  library(shiny)
  library(tidyr)
  library(effectsize)
  library(emmeans)
  library(boot)
  library(rstatix)
  library(conover.test)
  rm(new_packages,required_packages)
}

#sessionInfo()
# R version 4.3.0 (2023-04-21)
#attached base packages:
#[1] tcltk     stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#[1] conover.test_1.1.6 rstatix_0.7.2      boot_1.3-28.1      emmeans_1.8.8      effectsize_0.8.6   tidyr_1.3.0        shiny_1.7.5        dplyr_1.1.3       
#[9] car_3.1-2          carData_3.0-5      dunn.test_1.3.5    coin_1.4-2         multcomp_1.4-25    TH.data_1.1-2      MASS_7.3-60        survival_3.5-7    
#[17] mvtnorm_1.2-3      reshape2_1.4.4     ggplot2_3.4.3      openxlsx_4.2.5.2  

#loaded via a namespace (and not attached):
#[1] gtable_0.3.4       bslib_0.5.1        bayestestR_0.13.1  insight_0.19.5     lattice_0.21-8     vctrs_0.6.3        tools_4.3.0        generics_0.1.3    
#[9] datawizard_0.9.0   stats4_4.3.0       parallel_4.3.0     sandwich_3.0-2     tibble_3.2.1       fansi_1.0.4        pkgconfig_2.0.3    Matrix_1.6-4      
#[17] lifecycle_1.0.3    compiler_4.3.0     stringr_1.5.0      munsell_0.5.0      codetools_0.2-19   httpuv_1.6.11      sass_0.4.7         htmltools_0.5.6   
#[25] jquerylib_0.1.4    pillar_1.9.0       later_1.3.1        ellipsis_0.3.2     cachem_1.0.8       abind_1.4-5        mime_0.12          tidyselect_1.2.0  
#[33] zip_2.3.0          digest_0.6.33      stringi_1.7.12     purrr_1.0.2        splines_4.3.0      fastmap_1.1.1      grid_4.3.0         colorspace_2.1-0  
#[41] cli_3.6.1          magrittr_2.0.3     utf8_1.2.3         broom_1.0.5        libcoin_1.0-9      withr_2.5.0        backports_1.4.1    scales_1.2.1      
#[49] promises_1.2.1     estimability_1.4.1 matrixStats_1.0.0  zoo_1.8-12         modeltools_0.2-23  memoise_2.0.1      coda_0.19-4        parameters_0.21.2 
#[57] rlang_1.1.1        Rcpp_1.0.11        xtable_1.8-4       glue_1.6.2         jsonlite_1.8.7     rstudioapi_0.15.0  R6_2.5.1           plyr_1.8.8 