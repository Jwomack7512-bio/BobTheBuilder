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
    latex.eqns <- EqnsToLatex(eqns$eqn.info, 
                              input$latex_equation_headers, 
                              input$latex_equation_description,
                              eqns$eqn.descriptions)
    latex.IO <- InputOutputToLatex(IO$IO.info)
    latex.addEqns <- AdditionalEqnsToLatex(eqns$additional.eqns)
    latex.paramTable <- GenerateParameterTable(params$vars.all,
                                                params$vals.all,
                                                params$comments.all)
    latex.diffEqs <- DifferentialEqnsInModel(vars$species, DE$eqns.in.latex)
    
    
    out <- ""
    if (input$latex_add_variables) {out <- paste0(out, latex.species)}
    if (input$latex_add_equations) {out <- paste0(out, latex.eqns)}
    if (input$latex_add_additionalEqns) {out <- paste0(out, latex.addEqns)}
    if (input$latex_add_IO) {out <- paste0(out, latex.IO)}
    if (input$latex_add_paramTable) {out <- paste0(out, latex.paramTable)}
    if (input$latex_add_diffEqns) {out <- paste0(out, latex.diffEqs)}
    
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


output$table_equations_export <- renderDT({
  tab = data.frame(vars$species, DE$eqns)
  colnames(tab) <- c("Species", "Differential Equation")
  datatable(tab
            ,rownames = FALSE
            ,class = "cell-border stripe"
            ,extensions = 'Buttons'
            ,options = list(dom = 'Bt'
                          ,lengthMenu = list(c(-1), c("All"))
                          ,buttons = list("copy"
                                          ,list(extend = "csv", filename = "Variables")
                                          ,list(extend = "excel", filename = "Variables")
                                          ,list(extend = "pdf", filename = "Variables")
                                          ,"print"
                          )
            )
            )
})

#rownames reordering does not seem to work with rownames off.  It needs that id value.
#perhaps in future I will make a select and move up and down option in a sidebar.
output$table_ICs_export <- renderDT({
  tab = ICs$ICs.table
  colnames(tab) <- c("Species", "Value", "Description")
  DT::datatable(tab
                ,rownames = FALSE
                ,editable = TRUE
                #,editable = list(target = "column", disable = list(columns = c(0,1)))
                ,class = "cell-border stripe"
                ,extensions = c('Buttons', "RowReorder", "ColReorder")
                ,options = list(autoWidth = TRUE
                                ,ordering = TRUE
                                #,rowReorder = TRUE
                                #,colReorder = TRUE
                                ,order = list(c(0 , 'asc'))
                                ,columnDefs = list(list(width = "60%", targets = 2),
                                                   list(width = "20%", targets = 0),
                                                   list(className = 'dt-center', targets = c(0,1)),
                                                   list(className = 'dt-left', targets = 2)
                                )
                                ,dom = 'Bt'
                                ,buttons = list("copy"
                                                ,list(extend = "csv", filename = "Variables")
                                                ,list(extend = "excel", filename = "Variables")
                                                ,list(extend = "pdf", filename = "Variables")
                                                ,"print"
                                )
                )
  )
})

output$table_parameters_export <- renderDT({
  DT::datatable(params$param.table
                #,editable = list(target = "column", disable = list(columns = 0))
                ,class = "cell-border stripe"
                ,extensions = c('Buttons', "RowReorder", "ColReorder")
                ,rownames = FALSE
                ,options = list(autoWidth = TRUE
                                ,pageLength = -1
                                ,ordering = TRUE
                                #,rowReorder = TRUE
                                ,colReorder = TRUE
                                #,order = list(c(0 , 'asc'))
                                ,columnDefs = list(list(width = "60%", targets = 2),
                                                   list(width = "20%", targets = 0),
                                                   list(className = 'dt-center', targets = c(0,1)),
                                                   list(className = 'dt-left', targets = 2)
                                )
                                ,dom = 'Bt'
                                ,buttons = list("copy"
                                                ,list(extend = "csv", filename = "Variables")
                                                ,list(extend = "excel", filename = "Variables")
                                                ,list(extend = "pdf", filename = "Variables")
                                                ,"print"
                                )
                )
  )
})
