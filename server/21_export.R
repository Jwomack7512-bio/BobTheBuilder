############################## Export Server #################################
output$export_save_data <- downloadHandler(
  filename = function(){
    paste(input$export_model_file_name, ".rds", sep = "")
  },
  content = function(file){
    vars.temp <- reactiveValuesToList(vars)
    eqns.temp <- reactiveValuesToList(eqns)
    IO.temp   <- reactiveValuesToList(IO)
    ICs.temp  <- reactiveValuesToList(ICs)
    pars.temp <- reactiveValuesToList(params)
    diff.temp <- reactiveValuesToList(DE)
    opts.temp <- reactiveValuesToList(options)
    rslt.temp <- reactiveValuesToList(results)
    info.temp <- reactiveValuesToList(info)
    #dfs.temp  <- reactiveValuesToList(data)
    logs.temp <- reactiveValuesToList(logs)

    # to.save <- mapply(c,
    #                   vars
    #                   ,eqns
    #                   ,IO
    #                   ,ICs
    #                   ,pars
    #                   ,diff
    #                   ,opts
    #                   ,rslt
    #                   ,info
    #                   ,dfs
    #                   ,logs
    #                   ,SIMPLIFY = F)
    to.save <- c(vars.temp
                 ,eqns.temp
                 ,IO.temp
                 ,ICs.temp
                 ,pars.temp
                 ,diff.temp
                 ,opts.temp
                 ,rslt.temp
                 ,info.temp
                 ,logs.temp)

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
    
    observe({print(vars$species)})
    observe({print(vars$descriptions)})
    latex.species <- SpeciesInModel(vars$species, vars$descriptions)
    latex.eqns <- EqnsToLatex(eqns$eqn.info)
    latex.IO <- InputOutputToLatex(IO$IO.info)
    latex.addEqns <- AdditionalEqnsToLatex(eqns$additional.eqns)
    latex.paramTable <- GenerateParameterTable(params$vars.all,
                                                params$vals.all,
                                                params$comments.all)
    
    
    out <- ""
    if (input$latex_add_variables) {out <- paste0(out, latex.species)}
    if (input$latex_add_equations) {out <- paste0(out, latex.eqns)}
    if (input$latex_add_additionalEqns) {out <- paste0(out, latex.addEqns)}
    if (input$latex_add_IO) {out <- paste0(out, latex.IO)}
    if (input$latex_add_paramTable) {out <- paste0(out, latex.paramTable)}
    
    latex.file <- GenerateLatexDocument(out)
    #latex.file <- GenerateLatexDocument(latex.eqns)
    writeLines(latex.file, file)
  }
)

parameter_df <- reactive({
  tab = data.frame(params$vars.all, params$vals.all, params$comments.all)
  colnames(tab) <- c("Parameters", "Value", "Comment")
  tab
})
#build tables for model export
#parameter tabled
observeEvent(input$export_generate_output_tables, {
  output$table_parameters <- renderDataTable({
    tab = data.frame(params$vars.all, params$vals.all, params$comments.all)
    colnames(tab) <- c("Parameters", "Value", "Comment")
    datatable(tab
              ,options = list(dom = 't'
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
