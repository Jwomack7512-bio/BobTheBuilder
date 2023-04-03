# Page contains server for exporting data, tables, latex docs etc. 

# Export RDS -------------------------------------------------------------------
output$export_save_data <- downloadHandler(
  filename = function(){
    paste(input$export_model_file_name, ".rds", sep = "")
  },
  content = function(file){
    comp.temp <- reactiveValuesToList(rv.COMPARTMENTS)
    spec.temp <- reactiveValuesToList(rv.SPECIES)
    eqns.temp <- reactiveValuesToList(rv.REACTIONS)
    IO.temp   <- reactiveValuesToList(rv.IO)
    pars.temp <- reactiveValuesToList(rv.PARAMETERS)
    diff.temp <- reactiveValuesToList(rv.DE)
    opts.temp <- reactiveValuesToList(rv.SOLVER_OPTIONS)
    rslt.temp <- reactiveValuesToList(rv.RESULTS)
    info.temp <- reactiveValuesToList(info)
    logs.temp <- reactiveValuesToList(logs)
    id.temp   <- reactiveValuesToList(id)
    pe.temp   <- reactiveValuesToList(pe)
    unit.temp <- reactiveValuesToList(units)

    to.save <- c(comp.temp,
                 spec.temp,
                 eqns.temp,
                 IO.temp,
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
    my_matlab_file <- create_matlab_model_function(
      rv.SPECIES$species.names, 
      rv.PARAMETERS$parameters.names,
      rv.DE$de.eqns.for.solver,
      rv.PARAMETERS$parameters.df$BaseValue,
      rv.REACTIONS$additional.eqns,
      rv.SPECIES$species.df$BaseValue,
      rv.SOLVER.OPTIONS$time.scale.bool,
      rv.SOLVER.OPTIONS$time.scale.value,
      rv.SOLVER.OPTIONS$time.start,
      rv.SOLVER.OPTIONS$time.end,
      rv.SOLVER.OPTIONS$time.step)
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
                              rv.PARAMETERS$parameters.names, 
                              rv.PARAMETERS$parameters.df$BaseValue,
                              rv.SPECIES$species.df$BaseValue,
                              rv.REACTIONS$additional.eqns,
                              rv.DE$de.eqns.for.solver,
                              rv.SOLVER.OPTIONS$time.scale.bool,
                              rv.SOLVER.OPTIONS$time.scale.value,
                              rv.SOLVER.OPTIONS$ode.solver.type,
                              rv.SOLVER.OPTIONS$time.start,
                              rv.SOLVER.OPTIONS$time.end,
                              rv.SOLVER.OPTIONS$time.step)
    writeLines(my.R.file, file)
  }
)

# Download Latex Document ------------------------------------------------------
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
    latex.eqns <- EqnsToLatex(rv.REACTIONS$eqn.main.latex,
                              add.eqn.headers,
                              add.eqn.descriptions,
                              rv.REACTIONS$eqn.descriptions)
    # latex.IO <- InputOutputToLatex(rv.IO$rv.IO$InputOutput)
    latex.addEqns <- AdditionalEqnsToLatex(rv.REACTIONS$additional.eqns)
    latex.paramTable <- GenerateParameterTable(names(rv.PARAMETERS$parameters),
                                               rv.PARAMETERS$parameters.df$Value,
                                                rv.PARAMETERS$parameters.df$Description)
    latex.diffEqs <- DifferentialEqnsInModel(rv.SPECIES$species.names, rv.DE$de.eqns.in.latex)
    
    
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
  for.table <- rv.PARAMETERS$parameters.df %>% 
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
  tab <- data.frame(rv.SPECIES$species.names, rv.DE$de.eqns)
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
