#This contains the overall server flow of the Application (backend)
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------


server <- shinyServer(function(input, output, session) {
  #allows for the import of bigger data frames
  options(shiny.maxRequestSize = 30000 * 1024 ^ 2)
  #options(shiny.sanitize.errors = TRUE)
 
  #helpers here (generating_diffEqs_functions)
  source("./server/helpers.R")
  source("./server/helper_createEqns.R")
  source("./server/helper_diffeqs_to_text.R")
  source("./server/helper_diffeq_laws.R")
  source("./server/helper_generate_latex_file.R")
  source("./server/helper_write_matlab_code.R") #load functions to solve differential equations
  source("./server/helper_write_R_code.R")
  
  source(file.path("server", "00_reactive_variables.R"), local = TRUE)$value
  source(file.path("server", "01_modelBuild_createVar.R"), local = TRUE)$value
  source(file.path("server", "02_modelBuild_createEqn.R"), local = TRUE)$value
  source(file.path("server", "03_modelBuild_IO.R"), local = TRUE)$value
  source(file.path("server", "04_modelBuild_params.R"), local = TRUE)$value
  source(file.path("server", "05_modelBuild_ICs.R"), local = TRUE)$value
  source(file.path("server", "06_modelBuild_diffeqn.R"), local = TRUE)$value
  source(file.path("server", "07_modelBuild_options.R"), local = TRUE)$value

  source(file.path("server", "11_run_execute.R"), local = TRUE)$value
  source(file.path("server", "12_run_post_processing.R"), local = TRUE)$value
  source(file.path("server", "13_0_run_lineplot.R"), local = TRUE)$value
  source(file.path("server", "13_1_run_lineplot_renderUI.R"), local = TRUE)$value
  source(file.path("server", "13_2_compare_model_renderUI.R"), local = TRUE)$value
  source(file.path("server", "13_3_loop_model.R"), local = TRUE)$value #controls the looping mechanism in the lineplot server
  source(file.path("server", "41_summary.R"), local = TRUE)$value
  
  source(file.path("server", "21_export.R"), local = TRUE)$value
  # #additional source pages
  source(file.path("server", "server_load_model.R"), local = TRUE)$value #controls the load function server in the sidebar
  
  
})#end of server
