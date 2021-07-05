source("./server/helper_write_matlab_code.R") #load functions to solve differential equations


############################## Export Server #################################
output$export_save_data <- downloadHandler(
  filename = function(){
    paste(input$export_data_rds_file_name, ".rds", sep = "")
  },
  content = function(file){
    dfs <- reactiveValuesToList(data)
    logs_to_save <- reactiveValuesToList(logs)
    to_save <- reactiveValuesToList(rv)
    to_save <- append(to_save, dfs)
    to_save <- append(to_save, logs_to_save)
    saveRDS(to_save, file)
    
  }
)


output$export_data_to_matlab_script <- downloadHandler(
  filename = function(){
    "matlab_test_script.txt"
  },
  content = function(file){
    my_matlab_file <- create_matlab_model_function(rv$vars_in_model, rv$parameters_in_model, rv$diffEQs, rv$parameter_values, rv$rate_eqns, rv$IC_values, input$execute_turnOn_time_scale_var, input$execute_time_scale_var)
    writeLines(my_matlab_file, file)
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