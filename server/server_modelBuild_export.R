source("./server/helper_write_matlab_code.R") #load functions to solve differential equations
source("./server/generate_latex_file.R")
source("./server/helper_write_R_code.R")

############################## Export Server #################################
output$export_save_data <- downloadHandler(
  filename = function(){
    paste(input$export_model_file_name, ".rds", sep = "")
  },
  content = function(file){
    dfs <- reactiveValuesToList(data)
    logs.to.save <- reactiveValuesToList(logs)
    model.options.to.save <- reactiveValuesToList(model.options)
    model.results.to.save <- reactiveValuesToList(model.results)
    to.save <- reactiveValuesToList(rv)
    to.save <- append(to.save, dfs)
    to.save <- append(to.save, model.options.to.save)
    to.save <- append(to.save, model.results.to.save)
    to.save <- append(to.save, logs.to.save)
    saveRDS(to.save, file)
  }
)


output$export_data_to_matlab_script <- downloadHandler(
  filename = function(){
    paste0(input$export_code_file_name, ".m")
  },
  content = function(file){
    my_matlab_file <- create_matlab_model_function(rv$vars_in_model, 
                                                   rv$parameters_in_model, 
                                                   rv$diffEQs, 
                                                   rv$parameter_values, 
                                                   rv$additional_eqns, 
                                                   rv$IC_values, 
                                                   model.options$time.scale.bool,
                                                   model.options$time.scale.value,
                                                   model.options$time.start,
                                                   model.options$time.end,
                                                   model.options$time.step)
    writeLines(my_matlab_file, file)
  }
)

output$export_data_to_R_script <- downloadHandler(
  filename = function(){
    "R_test_script.txt"
  },
  content = function(file){
    my.R.file <- CreateRModel(rv$vars_in_model,
                              rv$parameters_in_model, 
                              rv$parameter_values,
                              rv$IC_values,
                              rv$additional_eqns,
                              rv$diffEQs,
                              model.options$time.scale.bool,
                              model.options$time.scale.value,
                              model.options$ode.solver.type,
                              model.options$time.start,
                              model.options$time.end,
                              model.options$time.step)
    writeLines(my.R.file, file)
  }
)

output$export_latex_document <- downloadHandler(
  filename = function(){"latex_test_script.txt"},
  content = function(file){
    latex.eqns <- EqnsToLatex(data$eqn_info)
    #latex.eqns <- "TEST"
    latex.IO <- InputOutputToLatex(rv$inputOutputs_df)
    latex.addEqns <- AdditionalEqnsToLatex(rv$additional_eqns)
    latex.file <- GenerateLatexDocument(latex.eqns, latex.IO, latex.addEqns)
    writeLines(latex.file, file)
  }
)

parameter_df <- reactive({
  tab = data.frame(rv$parameters_in_model, rv$parameter_values, rv$parameter_descriptions)
  colnames(tab) <- c("Parameters", "Value", "Comment")
  tab
})
#build tables for model export
#parameter tabled
observeEvent(input$export_generate_output_tables, {
  output$table_parameters <- renderDataTable({
    tab = data.frame(rv$parameters_in_model, rv$parameter_values, rv$parameter_descriptions)
    colnames(tab) <- c("Parameters", "Value", "Comment")
    datatable(tab
              ,options=list(dom='t'
                            ,lengthMenu = list(c(-1), c("All"))
                            ))
  })
  output$table_equations <- renderDataTable({
    tab = data.frame(rv$vars_in_model, rv$diffEQs)
    colnames(tab) <- c("Species", "Differential Equation")
    datatable(tab
              ,options=list(dom='t'
                            ,lengthMenu = list(c(-1), c("All"))
                            ))
  })
  output$table_ICs <- renderDataTable({
    tab = data.frame(rv$vars_in_model, rv$IC_values, rv$IC_descriptions)
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
#     write.csv(data$eqn_info, file, row.names = FALSE)
#     
#   }
# )