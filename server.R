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
  
  source(file.path("server", "reactive_variables.R"),
         local = TRUE)$value
  source(file.path("server", "server_modelBuild_createVar.R"),
         local = TRUE)$value
  source(file.path("server", "server_modelBuild_createEqn.R"),
         local = TRUE)$value
  source(file.path("server", "server_modelBuild_IO.R"), local = TRUE)$value
  source(file.path("server", "server_modelBuild_params.R"), local = TRUE)$value
  source(file.path("server", "server_modelBuild_ICs.R"), local = TRUE)$value
  source(file.path("server", "server_modelBuild_diffeqn.R"), local = TRUE)$value
  source(file.path("server", "server_modelBuild_export.R"), local = TRUE)$value
  source(file.path("server", "server_modelBuild_options.R"), local = TRUE)$value
  
  source(file.path("server", "server_modelBuild_diffeqn.R"), local = TRUE)$value
  
  source(file.path("server", "server_run_execute.R"), local = TRUE)$value
  source(file.path("server", "server_run_post_processing.R"), local = TRUE)$value
  source(file.path("server", "server_run_lineplot_renderUI.R"),
         local = TRUE)$value
  source(file.path("server", "server_compare_model_renderUI.R"),
         local = TRUE)$value
  source(file.path("server", "server_run_lineplot.R"), local = TRUE)$value
  
  source(file.path("server", "server_loop_model.R"), local = TRUE)$value #controls the looping mechanism in the lineplot server
  
  source(file.path("server", "server_load_model.R"), local = TRUE)$value #controls the load function server in the sidebar
  
  
})#end of server