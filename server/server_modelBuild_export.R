source("./server/helper_write_matlab_code.R") #load functions to solve differential equations
source("./server/generate_latex_file.R")
source("./server/helper_write_R_code.R")

############################## Export Server #################################
output$export_save_data <- downloadHandler(
  filename = function(){
    paste(input$export_model_file_name, ".rds", sep = "")
  },
  content = function(file){
    vars <- reactiveValuesToList(vars)
    eqns <- reactiveValuesToList(eqns)
    IO   <- reactiveValuesToList(IO)
    ICs  <- reactiveValuesToList(ICs)
    pars <- reactiveValuesToList(parameters)
    diff <- reactiveValuesToList(diff)
    opts <- reactiveValuesToList(options)
    rslt <- reactiveValuesToList(results)
    info <- reactiveValuesToList(info)
    dfs  <- reactiveValuesToList(data)
    logs <- reactiveValuesToList(logs)

    
    to.save <- c(vars
                 ,eqns
                 ,IO
                 ,ICs
                 ,pars
                 ,diff
                 ,opts
                 ,rslt
                 ,info
                 ,dfs
                 ,logs)

    saveRDS(to.save, file)
  }
)


output$export_data_to_matlab_script <- downloadHandler(
  filename = function(){
    paste0(input$export_code_file_name, ".m")
  },
  content = function(file){
    my_matlab_file <- create_matlab_model_function(vars$species, 
                                                   params$vars.all, 
                                                   DE$eqns, 
                                                   params$vals.all, 
                                                   eqns$additional.eqns, 
                                                   ICs$vals, 
                                                   options$time.scale.bool,
                                                   options$time.scale.value,
                                                   options$time.start,
                                                   options$time.end,
                                                   options$time.step)
    writeLines(my_matlab_file, file)
  }
)

output$export_data_to_R_script <- downloadHandler(
  filename = function(){
    "R_test_script.txt"
  },
  content = function(file){
    my.R.file <- CreateRModel(vars$species,
                              params$vars.all, 
                              params$vals.all,
                              ICs$vals,
                              eqns$additional.eqns,
                              DE$eqns,
                              options$time.scale.bool,
                              options$time.scale.value,
                              options$ode.solver.type,
                              options$time.start,
                              options$time.end,
                              options$time.step)
    writeLines(my.R.file, file)
  }
)

output$export_latex_document <- downloadHandler(
  filename = function(){"latex_test_script.txt"},
  content = function(file){
    latex.eqns <- EqnsToLatex(eqns$main)
    #latex.eqns <- "TEST"
    latex.IO <- InputOutputToLatex(IO$IO.info)
    latex.addEqns <- AdditionalEqnsToLatex(eqns$additional.eqns)
    latex.file <- GenerateLatexDocument(latex.eqns, latex.IO, latex.addEqns)
    writeLines(latex.file, file)
  }
)

parameter_df <- reactive({
  tab = data.frame(params$vars.all, params$vals.all, params$commments.all)
  colnames(tab) <- c("Parameters", "Value", "Comment")
  tab
})
#build tables for model export
#parameter tabled
observeEvent(input$export_generate_output_tables, {
  output$table_parameters <- renderDataTable({
    tab = data.frame(params$vars.all, params$vals.all, params$commments.all)
    colnames(tab) <- c("Parameters", "Value", "Comment")
    datatable(tab
              ,options=list(dom='t'
                            ,lengthMenu = list(c(-1), c("All"))
                            ))
  })
  output$table_equations <- renderDataTable({
    tab = data.frame(vars$species, DE$eqns)
    colnames(tab) <- c("Species", "Differential Equation")
    datatable(tab
              ,options=list(dom='t'
                            ,lengthMenu = list(c(-1), c("All"))
                            ))
  })
  output$table_ICs <- renderDataTable({
    tab = data.frame(vars$species, ICs$vals, ICs$comments)
    colnames(tab) <- c("Species", "Initial Condition Value", "Comment")
    datatable(tab
              ,options=list(dom='t'
                            ,lengthMenu = list(c(-1), c("All"))
                            ,columnDefs = list(list(className='dt-center'
                                                    ,targets = "_all"))))
  })
  
})

# output$export_save_data <- downloadHandler(
#   filename = function(){
#     paste("my_model", ".csv", sep = "")
#   },
#   content = function(file){
#     write.csv(eqns$main, file, row.names = FALSE)
#     
#   }
# )
