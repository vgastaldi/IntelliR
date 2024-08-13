#### IntelliCage data processing ####
## Author: Vinicius Daguano Gastaldi
## This script is required to start the file processing. A new script will be called depending on the operational system.
# Modified: 13.08.2024
#### Required packages ####
if (grepl("Windows", Sys.info()["sysname"])) {
  required_packages <- c("dplyr","openxlsx","ggplot2","reshape2","tcltk","multcomp","coin","dunn.test","car","tidyr","effectsize","emmeans","boot","rstatix","conover.test")
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
  required_packages <- c("dplyr","openxlsx","ggplot2","reshape2","tcltk","multcomp","coin","dunn.test","car","tidyr","effectsize","emmeans","boot","rstatix","conover.test")
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


#### Defining folders and project name ####
## Shiny App built with the help of ChatGPT
if (grepl("Windows", Sys.info()["sysname"])) {
  ui_windows <- fluidPage(
    tags$head(
      tags$style(HTML("
      .form-control, .btn {
        font-size: 18px;
        height: 40px;
        width: 300px;
      }
    "))
    ),
    titlePanel("Select Folders and Project Name"),
    p(HTML("<em>Made by Vinicius Daguano Gastaldi</em><br>"),style = "font-size: 14px;"),
    p(HTML("Instructions:<br><strong>1)</strong> Remember to follow folder structure as described in the readme file.<br>
  <strong>2)</strong> Type or paste the exact folder name where your files are.<br>
  <strong>3)</strong> Select the folder where all your projects, i.e., do not enter a specific project folder. Simply select it as press OK.
  <br><strong>Do not enter the folder before pressing OK.</strong><br>
  <strong>4)</strong> Select your results folder. This should be a general folder, a specific folder will be created for your project. Simply select it as press OK.
  <br><strong>Do not enter the folder before pressing OK.</strong><br>
  <strong>5)</strong> Current version of the script can handle 2, 3, and 4 groups.
  Your group names <strong>should not contain spaces!</strong> Use underscores, _ , instead.<br>
  <strong>6)</strong> Your unblinding file should follow the instructions provided in the readme file.<br>
  <strong>7)</strong> Also check the readme file for possible color suggestions. Those have to be valid R color names.")),
    
    textInput("project", "Project name"),
    fluidRow(
      column(12,
             actionButton("project_folder", "Project folder")
      )
    ),
    fluidRow(
      column(12,
             actionButton("results_folder", "Results folder")
      )
    ),
    
    selectInput("num_groups", "Number of groups:", choices = c(2,3,4)),
    
    uiOutput("group_inputs"),
    mainPanel(
      textOutput("project_name"),
      textOutput("project_folder_path"),
      textOutput("results_folder_path")
    ),
    fluidRow(
      column(12,
             fileInput("unblinding_file", "Select unblinding file")
      )
    ),
    fluidRow(
      column(12,
             actionButton("select_file", "Select IntelliCage script")
      )
    ),
    fluidRow(
      column(12,
             actionButton("enter_project", "Click to confirm")
      )
    )
  )
  
  server_windows <- function(input, output, session) {
    project <<- NULL
    project_folder <<- NULL
    results_folder <<- NULL
    number_groups <<- NULL
    intellicage_script <<- NULL
    unblinding <<- NULL
    
    observeEvent(input$enter_project, {
      project <<- input$project
    })
    
    observeEvent(input$project_folder, {
      project_folder <<- choose.dir()
    })
    
    observeEvent(input$results_folder, {
      results_folder <<- choose.dir()
    })
    
    observeEvent(input$unblinding_file, {
      req(input$unblinding_file)
      unblinding <<- read.delim(input$unblinding_file$datapath,sep = "\t")
    })
    
    observeEvent(input$select_file, {
      intellicage_script <<- choose.files()
    })
    
    output$group_inputs <- renderUI({
      num_groups <- input$num_groups
      lapply(seq_len(num_groups), function(i) {
        fluidRow(
          column(6, textInput(paste0("group", i, "_name"), paste0("Name of group ", i))),
          column(6, textInput(paste0("group", i, "_color"), paste0("Color of group ", i)))
        )
      })
    })
    
    observe({
      num_groups <- input$num_groups
      number_groups <<- num_groups
      for (i in seq_len(num_groups)) {
        assign(paste0("group", i, "_name"), input[[paste0("group", i, "_name")]], envir = .GlobalEnv)
        assign(paste0("group", i, "_color"), input[[paste0("group", i, "_color")]], envir = .GlobalEnv)
      }
    })
    
    output$project_name <- renderText({
      project
    })
    
    output$project_folder_path <- renderText({
      project_folder
    })
    
    output$results_folder_path <- renderText({
      results_folder
    })
    
    observeEvent(c(input$enter_project, input$project_folder, input$results_folder), {
      if (!is.null(project) && nchar(project) > 0 && !is.null(project_folder) && !is.null(results_folder)) {
        stopApp()
        session$close()
      }
    })
  }
  runApp(shinyApp(ui = ui_windows, server = server_windows, options = list(launch.browser = TRUE)))
} else {
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
      .form-control, .btn {
        font-size: 18px;
        height: 40px;
        width: 300px;
      }
    "))
    ),
    titlePanel("Select Folders and Project Name"),
    p(HTML("<em>Made by Vinicius Daguano Gastaldi</em><br>"),style = "font-size: 14px;"),
    p(HTML("Instructions:<br><strong>1)</strong> Remember to follow folder structure as described in the readme file.<br>
  <strong>2)</strong> Type or paste the exact folder name where your files are.<br>
  <strong>3)</strong> Select the folder where all your projects, i.e., do not enter a specific project folder. You need to enter the directory before pressing OK.
  <br><strong>It does not work if you simply select it and press OK.</strong><br>
  <strong>4)</strong> Select your results folder. This should be a general folder, a specific folder will be created for your project. You need to enter the directory before pressing OK.
  <br><strong>It does not work if you simply select it and press OK.</strong><br>
  <strong>5)</strong> Current version of the script can handle 2, 3, and 4 groups.<br>
  Your group names <strong>should not contain spaces!</strong> Use underscores, _ , instead.<br>
  <strong>6)</strong> Your unblinding file should follow the instructions provided in the readme file.<br>
  <strong>7)</strong> Also check the readme file for possible color suggestions. Those have to be valid R color names.")),
    textInput("project", "Project name"),
    fluidRow(
      column(12,
             actionButton("project_folder", "Project folder")
      )
    ),
    fluidRow(
      column(12,
             actionButton("results_folder", "Results folder")
      )
    ),
    
    selectInput("num_groups", "Number of groups:", choices = c(2,3,4)),
    
    uiOutput("group_inputs"),
    mainPanel(
      textOutput("project_name"),
      textOutput("project_folder_path"),
      textOutput("results_folder_path")
    ),
    fluidRow(
      column(12,
             fileInput("unblinding_file", "Select unblinding file")
      )
    ),
    fluidRow(
      column(12,
             actionButton("intellicage_script_button", "Select IntelliCage script")
      )
    ),
    fluidRow(
      column(12,
             actionButton("enter_project", "Click to confirm")
      )
    )
  )
  
  server <- function(input, output, session) {

    project <<- NULL
    project_folder <<- NULL
    results_folder <<- NULL
    number_groups <<- NULL
    unblinding <<- NULL
    intellicage_script <<- NULL
    
    observeEvent(input$enter_project, {
      project <<- input$project
    })
    
    observeEvent(input$project_folder, {
      project_folder <<- tclvalue(tkchooseDirectory())
    })
    
    observeEvent(input$results_folder, {
      results_folder <<- tclvalue(tkchooseDirectory())
    })
    
    observeEvent(input$unblinding_file, {
      req(input$unblinding_file)
      unblinding <<- read.delim(input$unblinding_file$datapath,sep = "\t")
    })
    
    observeEvent(input$intellicage_script_button, {
      intellicage_script <<- file.choose()
    })
    
    output$group_inputs <- renderUI({
      num_groups <- input$num_groups
      lapply(seq_len(num_groups), function(i) {
        fluidRow(
          column(6, textInput(paste0("group", i, "_name"), paste0("Name of group ", i))),
          column(6, textInput(paste0("group", i, "_color"), paste0("Color of group ", i)))
        )
      })
    })
    
    observe({
      num_groups <- input$num_groups
      number_groups <<- num_groups
      for (i in seq_len(num_groups)) {
        assign(paste0("group", i, "_name"), input[[paste0("group", i, "_name")]], envir = .GlobalEnv)
        assign(paste0("group", i, "_color"), input[[paste0("group", i, "_color")]], envir = .GlobalEnv)
      }
    })
    
    output$project_name <- renderText({
      project
    })
    
    output$project_folder_path <- renderText({
      project_folder
    })
    
    output$results_folder_path <- renderText({
      results_folder
    })
    
    observeEvent(c(input$enter_project, input$project_folder, input$results_folder), {
      if (!is.null(project) && nchar(project) > 0 && !is.null(project_folder) && !is.null(results_folder)) {
        stopApp()
        session$close()
      }
    })
  }
  runApp(shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE)))
}
source(intellicage_script)