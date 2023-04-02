# Page contains server for exporting data, tables, latex docs etc. 

# Export RDS -------------------------------------------------------------------
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
    id.temp   <- reactiveValuesToList(id)
    pe.temp   <- reactiveValuesToList(pe)
    unit.temp <- reactiveValuesToList(units)

    to.save <- c(vars.temp,
                 eqns.temp,
                 IO.temp,
                 ICs.temp,
                 pars.temp,
                 diff.temp,
                 opts.temp,
                 rslt.temp,
                 info.temp,
                 logs.temp,
                 id.temp,
                 pe.temp, 
                 unit.temp)

    saveRDS(to.save, file)
  }
)

# Export Matlab Code -----------------------------------------------------------
output$export_data_to_matlab_script <- downloadHandler(
  filename = function(){
    paste0(input$export_code_file_name, ".m")
  },
  content = function(file){
    my_matlab_file <- create_matlab_model_function(rv.SPECIES$species.names, 
                                                   params$par.names,
                                                   DE$de.eqns.for.solver, 
                                                   params$par.df$BaseValue, 
                                                   eqns$additional.eqns, 
                                                   rv.SPECIES$species.df$BaseValue, 
                                                   options$time.scale.bool,
                                                   options$time.scale.value,
                                                   options$time.start,
                                                   options$time.end,
                                                   options$time.step)
    writeLines(my_matlab_file, file)
  }
)

# Export to R Script -----------------------------------------------------------
output$export_data_to_R_script <- downloadHandler(
  filename = function(){
    paste0(input$export_code_file_name, ".R")
  },
  content = function(file){
    my.R.file <- CreateRModel(rv.SPECIES$species.names,
                              params$par.names, 
                              params$par.df$BaseValue,
                              rv.SPECIES$species.df$BaseValue,
                              eqns$additional.eqns,
                              DE$de.eqns.for.solver,
                              options$time.scale.bool,
                              options$time.scale.value,
                              options$ode.solver.type,
                              options$time.start,
                              options$time.end,
                              options$time.step)
    writeLines(my.R.file, file)
  }
)

# Download Latex Document
output$export_latex_document <- downloadHandler(
  filename = function(){"latex_test_script.txt"},
  content = function(file){
    add.eqn.headers <- FALSE
    add.eqn.descriptions <- FALSE
    #pull values from checkboxgroups
    if ("show_eqn_type" %in% input$latex_additional_options) {
      add.eqn.headers <- TRUE
    }
    if ("show_eqn_description" %in% input$latex_additional_options) {
      add.eqn.descriptions <- TRUE
    }
    
    #bools for pages to add for latex doc
    page.add.var <- FALSE
    page.add.eqns <- FALSE
    page.add.add.eqns <- FALSE
    page.add.IO <- FALSE
    page.add.param <- FALSE
    page.add.diffeqs <- FALSE
    
    if ("Variable" %in% input$latex_pages_to_add) {
      page.add.var <- TRUE
    }
    if ("Equations" %in% input$latex_pages_to_add) {
      page.add.eqns <- TRUE
    }
    if ("Additional Equations" %in% input$latex_pages_to_add) {
      page.add.add.eqns <- TRUE
    }
    if ("Input/Output" %in% input$latex_pages_to_add) {
      page.add.IO <- TRUE
    }
    if ("Parameter Table" %in% input$latex_pages_to_add) {
      page.add.param <- TRUE
    }
    if ("Differential Eqns" %in% input$latex_pages_to_add) {
      page.add.diffeqs <- TRUE
    }
    

    latex.species <- SpeciesInModel(rv.SPECIES$species.names, vars$descriptions)
    latex.eqns <- EqnsToLatex(eqns$eqn.main.latex,
                              add.eqn.headers,
                              add.eqn.descriptions,
                              eqns$eqn.descriptions)
    # latex.IO <- InputOutputToLatex(IO$IO.info)
    latex.addEqns <- AdditionalEqnsToLatex(eqns$additional.eqns)
    latex.paramTable <- GenerateParameterTable(names(params$par.info),
                                               params$par.df$Value,
                                                params$par.df$Description)
    latex.diffEqs <- DifferentialEqnsInModel(rv.SPECIES$species.names, DE$eqns.in.latex)
    
    
    out <- ""
    if (page.add.var) {out <- paste0(out, latex.species)}
    if (page.add.eqns) {out <- paste0(out, latex.eqns)}
    if (page.add.add.eqns) {out <- paste0(out, latex.addEqns)}
    # if (page.add.IO) {out <- paste0(out, latex.IO)}
    if (page.add.param) {out <- paste0(out, latex.paramTable)}
    if (page.add.diffeqs) {out <- paste0(out, latex.diffEqs)}

    latex.file <- GenerateLatexDocument(out)
    #latex.file <- GenerateLatexDocument(latex.eqns)
    writeLines(latex.file, file)
  }
)

# Tables To View/Print ---------------------------------------------------------
# Here we render specific model tables to view and export

## Species ---------------------------------------------------------------------
output$table_species_export <- renderDT({
  for.table <- rv.SPECIES$species.df %>%
    select("Name", "Value", "Unit", "Compartment", "Description")
  
  DT::datatable(
    for.table,
    rownames = FALSE,
    editable = TRUE,
    class = "cell-border stripe",
    extensions = c('Buttons', "RowReorder", "ColReorder"),
    options = list(
      # autoWidth = TRUE,
      # ordering = TRUE,
      # order = list(c(0 , 'asc')),
      columnDefs = list(
        list(width = "20%", targets = 0),
        list(width = "10%", targets = c(1,2,3)),
        list(width = "50%", targets = 4),
        list(className = 'dt-center', targets = c(0,1,2,3)),
        list(className = 'dt-left', targets = 4)
      ),
      dom = 'Bt',
      buttons = list("copy",
                     list(extend = "csv",   filename = "Species"),
                     list(extend = "excel", filename = "Species"),
                     list(extend = "pdf",   filename = "Species"),
                     "print"
      )
    )
  )
})
## Compartments ----------------------------------------------------------------
output$table_compartments_export <- renderDT({
  for.table <- rv.COMPARTMENTS$compartments.df %>%
    select("Name", "Volume", "Value", "Unit", "Description")
  
  DT::datatable(
    for.table,
    rownames = FALSE,
    editable = TRUE,
    class = "cell-border stripe",
    extensions = c('Buttons', "RowReorder", "ColReorder"),
    options = list(
      # autoWidth = TRUE,
      # ordering = TRUE,
      # order = list(c(0 , 'asc')),
      columnDefs = list(
        list(width = "20%", targets = 0),
        list(width = "10%", targets = c(1,2,3)),
        list(width = "50%", targets = 4),
        list(className = 'dt-center', targets = c(0,1,2,3)),
        list(className = 'dt-left', targets = 4)
      ),
      dom = 'Bt',
      buttons = list("copy",
                     list(extend = "csv",   filename = "Compartments"),
                     list(extend = "excel", filename = "Compartments"),
                     list(extend = "pdf",   filename = "Compartments"),
                     "print"
      )
    )
  )
})

## Parameters ------------------------------------------------------------------
output$table_parameters_export <- renderDT({
  for.table <- params$par.df %>% 
    select("Name", "Value", "Unit", "Description")
  DT::datatable(
    for.table,
    class = "cell-border stripe",
    extensions = c('Buttons', "RowReorder", "ColReorder"),
    rownames = FALSE,
    options = list(
      # autoWidth = TRUE,
      pageLength = -1,
      ordering = TRUE,
      colReorder = TRUE,
      columnDefs = list(
        list(width = "20%", targets = 0),
        list(width = "15%", targets = c(1,2)),
        list(width = "50%", targets = 3),
        list(className = 'dt-center', targets = c(0, 1, 2)),
        list(className = 'dt-left', targets = 3)
      ),
      dom = 'Bt',
      buttons = list("copy",
                      list(extend = "csv",   filename = "Parameters"),
                      list(extend = "excel", filename = "Parameters"),
                      list(extend = "pdf",   filename = "Parameters"),
                      "print"
      )
    )
  )
})

## Equations -------------------------------------------------------------------
output$table_equations_export <- renderDT({
  tab <- data.frame(rv.SPECIES$species.names, DE$eqns)
  colnames(tab) <- c("Species", "Differential Equation")
  datatable(
    tab,
    rownames = FALSE,
    class = "cell-border stripe",
    extensions = 'Buttons',
    options = list(
      dom = 'Bt',
      lengthMenu = list(c(-1), c("All")),
      buttons = list(
        "copy",
        list(extend = "csv",   filename = "DifferentialEquations"),
        list(extend = "excel", filename = "DifferentialEquations"),
        list(extend = "pdf",   filename = "DifferentialEquations"),
        "print"
      )
    )
  )
})
