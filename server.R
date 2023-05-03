#This contains the overall server flow of the Application (backend)
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

RandomHTMLSpinner <- function() {
  # browser()
  rng <- floor(runif(1, min = 1, max = 6))
  
  spinners <- c("waiter::spin_pong()",
                "waiter::spin_heartbeat()",
                "waiter::spin_whirly()",
                "waiter::spin_pulsar()",
                "waiter::spin_ball()")
  
  return(spinners[rng])
}


waiting_screen <- tagList(eval(parse(text = RandomHTMLSpinner())),
                          br(),
                          br(),
                          h4("Loading Model..."),)

button_store <- tagList(spin_loaders(32),
                        h4("Overwriting Parameters"))

server <- shinyServer(function(input, output, session) {
  #allows for the import of bigger data frames
  options(shiny.maxRequestSize = 30000 * 1024 ^ 2)
  table.header <- reactiveValues(bg = "#3c8dbc",color = 'white')
  #options(shiny.sanitize.errors = TRUE)
  
  source(file.path("server", "helpers.R"))
  source(file.path("server", "helper_prep_ODEs_for_solver.R"))

  source(file.path("server", "helper_id_generator.R"))
  source(file.path("server", "sbml_fxns.R"))
  source(file.path("server", "rate_laws_equations.R"))
  source(file.path("server", "rate_laws_IO.R"))
  source(file.path("server", "DeriveODEs.R"))
  source(file.path("server", "write_latex_document.R"))
  source(file.path("server", "write_MATLAB.R")) 
  source(file.path("server", "write_R.R"))
  
  
  source(file.path("server", "000_init.R"), local = TRUE)$value
  source(file.path("server", "00_reactive_variables.R"), local = TRUE)$value
  source(file.path("server", "01_compartments.R"), local = TRUE)$value
  source(file.path("server", "01_species.R"), local = TRUE)$value
  source(file.path("server", "02_equations.R"), local = TRUE)$value
  source(file.path("server", "02_equations_renderUI.R"), local = TRUE)$value
  source(file.path("server", "02_equations_text_mathjax.R"),local = TRUE)$value
  source(file.path("server", "02_equations_edit.R"),local = TRUE)$value
  source(file.path("server", "03_io.R"), local = TRUE)$value
  source(file.path("server", "04_parameters.R"), local = TRUE)$value
  source(file.path("server", "05_differential_equations.R"), local = TRUE)$value
  
  source(file.path("server", "11_run_execute.R"), local = TRUE)$value
  source(file.path("server", "12_run_post_processing.R"), local = TRUE)$value
  source(file.path("server", "13_0_run_lineplot.R"), local = TRUE)$value
  #source(file.path("server", "13_compare_model.R"), local = TRUE)$value
  #source(file.path("server", "13_3_loop_model.R"), local = TRUE)$value #controls the looping mechanism in the lineplot server
  #source(file.path("server", "41_summary.R"), local = TRUE)$value
  source(file.path("server", "51_parameter_estimation.R"), local = TRUE)$value
  source(file.path("server", "61_global_options.R"), local = TRUE)$value
  
  source(file.path("server", "21_export.R"), local = TRUE)$value
  
  # #additional source pages
  source(file.path("server", "load_rds.R"), local = TRUE)$value
  source(file.path("server", "load_sbml.R"), local = TRUE)$value
  source(file.path("server", "debug.R"), local = TRUE)$value


  output$css_themes <- renderUI({
    tags$head(if (input$css_selector == "Default") {
      tags$link(rel = 'stylesheet',
                type = 'text/css',
                href = 'css/default.css')
    }  else if (input$css_selector == "RoyalBlue") {
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "css/royalBlue.css")
    } else if (input$css_selector == "Night") {
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "css/night.css")
    })
  })
  
  # This changes the colors of the generated DT table UI
  # observeEvent(input$css_selector, {
  #   if (input$css_selector == "default") {
  #     table.header$bg <- "grey"
  #     table.header$color <- "black"
  #     #fresh::use_theme("ocean.css")
  #   } else if (input$css_selector == "night") {
  #     #fresh::use_theme("night.css")
  #   } else if (input$css_selector == "test1") {
  #     #fresh::use_theme("test1.css")
  #   } else if (input$css_selector == "test2") {
  #     # fresh::use_theme("test2.css")
  #   } else if (input$css_selector == "ocean") {
  #   } else if (input$css_selector == "royalBlue") {
  #     table.header$bg <- "#3c8dbc"
  #     table.header$color <- "white"
  #   }
  # })
  
  # close info boxes
  #start with box removed on load
  #updateBox("create_var_info_box", action = "toggle")
  #updateBox("create_eqn_info_box", action = "toggle")
  
})#end of server
