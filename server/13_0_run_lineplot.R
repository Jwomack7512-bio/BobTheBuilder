######################## Line Plot Sever #####################################

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

# SUBTAB: LinePlot
# LINEPLOT000

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$execute_run_model, {
  observe({print("Updating Input for select input xvar")})
  updateSelectInput(session
                    ,"lineplot_xvar"
                    ,choices = colnames(model_output())[1])
})

#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$execute_run_model, {
  observe({print("Updating input for select input yvar")})
  updateSelectInput(session,
                    "lineplot_yvar"
                    ,choices  = colnames(ModelToUse())[2:ncol(ModelToUse())]
                    ,selected = colnames(ModelToUse())[2:ncol(ModelToUse())]
  )
})

#changes xlabel for line plot to selected input
observeEvent(input$lineplot_xvar,{
  updateTextInput(session,
                  "line_xlabel",
                  label = "X Label",
                  value = input$lineplot_factor_var)
})

#changes xlabel for line plot to selected input
observeEvent(input$lineplot_yvar,{
  updateTextInput(session,
                  "line_ylabel",
                  label = "Y Label",
                  value = 'Values')
})

# gatherLinePlotData <- function(){
#   req(input$lineplot_yvar)
#   if(input$lineplot_loop_mode){
#     selectedData <- gather(select(data.frame(loop_model_output()), input$lineplot_xvar, input$lineplot_yvar), Variable, Value, -one_of(input$lineplot_xvar))
#   }
#   else{
#     selectedData <- gather(select(data.frame(model_output()), input$lineplot_xvar, input$lineplot_yvar), Variable, Value, -one_of(input$lineplot_xvar))
#   }
# }

# # #Renders the color panel for each different stratified categorical variable (each at varied distance color levels)
output$line_color_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  lev <- sort(unique(gsub(" ", "_",gatherLinePlotData()$Variable)))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i){
    colourInput(inputId = paste0("cols_line", lev[i]),
                label=paste0("Line color: ", lev[i]),
                value=cols[i]
    )
  })
})

#This provides the dynamically allocated number of line type options for each variable in the line plots
output$line_type_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  lev <- sort(unique(gsub(" ", "_",gatherLinePlotData()$Variable)))
  
  lapply(seq_along(lev), function(i){
    selectInput(inputId = paste0("line_type", lev[i]),
                label=paste0("Line type: ", lev[i]),
                choices = c("solid" = "solid",
                            "Dashed" = "dashed",
                            "Dotted" = "dotted",
                            "Long Dash" = "longdash",
                            "Dot-Dash" = "dotdash"))
  })
})

#this function talkes multiple inputs, and factors them into one column, creating a second column of corresponding groups
#groups are stored in variable :Variable, call with gatherLinePlotData()$Variable
#data stores in cariable: Value, called same way
gatherLinePlotData <- function(){
  req(input$lineplot_yvar)
  selectedData <- gather(select(data.frame(ModelToUse()), input$lineplot_xvar, input$lineplot_yvar), Variable, Value, -one_of(input$lineplot_xvar))
}

gatherLinePlotData_compare1 <- function(){
  req(input$lineplot_yvar)
  selectedData <- gather(select(data.frame(ModelToUse()), input$lineplot_xvar, input$lineplot_yvar), Variable, Value, -one_of(input$lineplot_xvar))
  
}

theme_output_line <- function(){
  if (input$theme_output_line == 'gray') {
    theme_gray()}
  else if (input$theme_output_line == 'classic') {
    theme_classic()}
  else if (input$theme_output_line == 'void') {
    theme_void()}
  else if (input$theme_output_line == 'dark') {
    theme_dark()}
  else if (input$theme_output_line == 'bw') {
    theme_bw()}
  else if (input$theme_output_line == 'linedraw') {
    theme_linedraw()}
  else if (input$theme_output_line == 'light') {
    theme_light()}
  else if (input$theme_output_line == 'minimal') {
    theme_minimal()}
  
}

#this is the function that creates the ggplot object for the line plot
plotLineplotInput <- function(){
  #calls data function and stores it to selectedData
  selectedData <- gatherLinePlotData()
  
  #create vector of cols for lines
  cols_line <- paste0("c(", paste0("input$cols_line", unique(sort(gatherLinePlotData()$Variable)), collapse = ", "), ")")
  cols_line <- eval(parse(text = cols_line))
  
  #create vector of linetypes for lines
  type_line <-  paste0("c(", paste0("input$line_type", unique(sort(gatherLinePlotData()$Variable)), collapse = ", "), ")")
  type_line <- eval(parse(text = type_line))
  # #print(type_line)
  
  #ggplot function to print using geom_line
  g_line <- ggplot(selectedData, aes(x = selectedData[,1], y = Value, color = Variable)) +
    geom_line(aes(linetype = Variable),
              size = input$line_size_options) +
    scale_color_manual(name = input$line_legend_title,
                       values = cols_line) +
    scale_linetype_manual(name = input$line_legend_title,
                          values = type_line)
  
  if (input$line_show_dots) {g_line <- g_line + geom_point()}
  else{g_line <- g_line}
  
  g_line <- g_line +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title = input$line_title,
         x = input$line_xlabel,
         y = input$line_ylabel) +
    #hjust is used to center the title, size is used to change the text size of the title
    theme_output_line() +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          #allows user to change position of legend
          legend.position = input$line_legend_position)
  
}

plotLineplot_compare1 <- function(){
  #calls data function and stores it to selectedData
  selectedData <- gatherLinePlotData()
  
  #create vector of cols for lines
  cols_line <- paste0("c(", paste0("input$cols_line", unique(sort(gatherLinePlotData()$Variable)), collapse = ", "), ")")
  cols_line <- eval(parse(text = cols_line))
  
  #create vector of linetypes for lines
  type_line <-  paste0("c(", paste0("input$line_type", unique(sort(gatherLinePlotData()$Variable)), collapse = ", "), ")")
  type_line <- eval(parse(text = type_line))
  # #print(type_line)
  
  #ggplot function to print using geom_line
  g_line <- ggplot(selectedData, aes(x = selectedData[,1], y = Value, color = Variable)) +
    geom_line(aes(linetype = Variable),
              size = input$line_size_options) +
    scale_color_manual(name = input$line_legend_title,
                       values = cols_line) +
    scale_linetype_manual(name = input$line_legend_title,
                          values = type_line)
  
  if (input$line_show_dots) {g_line <- g_line + geom_point()}
  else{g_line <- g_line}
  
  g_line <- g_line +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title = input$line_title,
         x = input$line_xlabel,
         y = input$line_ylabel) +
    #hjust is used to center the title, size is used to change the text size of the title
    theme_output_line() +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          #allows user to change position of legend
          legend.position = input$line_legend_position)
  
}
# Ui to determine how the plots will be displayed ------------------------------
output$model_plotType <- renderUI({
  div
  (
    if (input$lineplot_choose_plot_mode == 'compare_mode') {
      fluidRow(column(width = 6,
                      jqui_resizable(plotOutput(outputId = 'LinePlot')))
               ,column(width = 6
                       ,jqui_resizable(plotOutput(outputId = 'LinePlot_to_compare')))
               ) # end fluid row
    } else if (input$lineplot_choose_plot_mode == 'loop_mode') {
      fluidRow(column(width = 12,
                      jqui_resizable(plotOutput(outputId = 'LinePlot')))
      )
    } else if (input$lineplot_choose_plot_mode == 'overlay_data_mode') {
      fluidRow(column(width = 9,
                      jqui_resizable(plotOutput(outputId = "lineplot_overlay_scatterplot")))
               ,column(width = 3,
                       box(title = NULL
                           ,status = "success"
                           ,solidHeader = FALSE
                           ,collapsible = TRUE
                           ,width = NULL,
                           "This box will have a load data option,
                           select data to scatter x,y,
                           and maybe a size/color option."
                           ,fileInput(inputId = "overlay_scatter_input", 
                                      label = "Import File")
                           ,selectInput(inputId = 'overlay_scatter_xcol',
                                       label = 'X Variable',
                                       choices = character())
                           ,selectInput(inputId = 'overlay_scatter_ycol',
                                       label = 'Y Variable',
                                       choices = character()))
                       ))
    } else if (input$lineplot_choose_plot_mode == "normal_plot") {
      fluidRow(column(width = 12,
                       jqui_resizable(plotOutput(outputId = 'LinePlot'))))
    }
  )
  
  
})

# Renderplots for all plot options ---------------------------------------------  
output$LinePlot <- renderPlot({
    print(plotLineplotInput())
})

output$LinePlot_compare1 <- renderPlot({
  print(plotLineplotInput())
})

output$LinePlot_compare2 <- renderPlot({
  print(plotLineplotInput())
})

output$LinePlot_to_compare <- renderPlot({
  print(plotLineplotInput_compare())
})

output$lineplot_overlay_scatterplot <- renderPlot({
  print(PlotLineplotOverlay())
})

output$downloadLine <- downloadHandler(
  filename = function(){
    paste(input$line_download_title, input$line_download_radiobuttons, sep="")
  },
  content = function(file){
    ggsave(file, plotLineplotInput())
  }
)

output$line_box_options <- renderUI({
  div
  (
    column(width = ifelse(input$lineplot_choose_plot_mode == "compare_mode", 6, 12),
    #____________________________________
    #Options containing Tabs
    #____________________________________
    box(
      #this is a box that holds the import data options.
      title = NULL, 
      status = "success", 
      solidHeader = FALSE, 
      collapsible = TRUE, 
      width = NULL,
      tabBox(
        title = "Options",
        width = 12,
        #____________________________________
        #Color Options
        #____________________________________
        tabPanel("Color Options",
                 fluidRow(
                   #add line color options
                   column(width = 12,
                          fluidRow(
                            column(width = 3,
                                   uiOutput("line_color_options_popdown")),
                            column(width = 3,
                                   uiOutput("line_type_options_popdown")))
                   ) #end column
                 ) #end fluidRow
        ), #end tabPanel
        #____________________________________
        #Background Options
        #____________________________________                        
        tabPanel("Background Options",
                 
                 fluidRow(
                   column(width = 5,
                          selectInput(
                            inputId = "theme_output_line",
                            label = "Background Theme", 
                            choices = c("gray"
                                        ,"bw"
                                        ,"linedraw"
                                        ,"light"
                                        ,"minimal"
                                        ,"classic"
                                        ,"void"
                                        ,"dark")
                          ),
                          fluidRow(
                            div(style="display:inline-block; text_align:right;", 
                                prettyCheckbox(inputId="line_panel_colorPicker_checkbox", 
                                               label=NULL, 
                                               value = FALSE)),
                            div(style="display:inline-block; text_align:right;", 
                                colourInput(inputId="line_panel_colorPicker", 
                                            label="Select Color", 
                                            value="grey"))))
                 )
                 
                 
        ),#end tabPanel
        #____________________________________
        #Legend Options
        #____________________________________
        tabPanel("Legend Options",
                 fluidRow(
                   column(width=5,
                          selectInput(inputId="line_legend_position",
                                      label = "Location of Legend",
                                      choices = c("Left" = "left", 
                                                  "Right" = "right", 
                                                  "Top" = "top", 
                                                  "Bottom" = "bottom", 
                                                  "No Legend" = "none"),
                                      selected = "right"),
                          textInput(inputId="line_legend_title",
                                    label = "Legend Title",
                                    value = "")))
        ) #end tabPanel
      )#End tabBox
    ) #End box
    ) #end column

  )
})

#-------------------------------------------------------------------------------

# This section covers the plotting of the comparison plot

#-------------------------------------------------------------------------------

PlotLineplotOverlay <- function(){  #---still have to add scatter plot somehow
  #calls data function and stores it to selectedData
  selectedData <- gatherLinePlotData()
  
  #create vector of cols for lines
  cols_line <- paste0("c(", paste0("input$cols_line", unique(sort(gatherLinePlotData()$Variable)), collapse = ", "), ")")
  cols_line <- eval(parse(text = cols_line))
  
  #create vector of linetypes for lines
  type_line <-  paste0("c(", paste0("input$line_type", unique(sort(gatherLinePlotData()$Variable)), collapse = ", "), ")")
  type_line <- eval(parse(text = type_line))
  #print(type_line)
  
  #ggplot function to print using geom_line
  #g_line <- ggplot(selectedData, aes(x = selectedData[,1], y = selectedData$Value, color = selectedData$Variable)) +
  g_line <- ggplot(selectedData) +
    geom_line(aes(linetype = Variable, x = selectedData[,1], y = Value, color = Variable),
              size = input$line_size_options) +
    geom_point(data = overlay_scatter_data(),
                mapping = aes(x = overlay_scatter_data()[[input$overlay_scatter_xcol]],
                              y = overlay_scatter_data()[[input$overlay_scatter_ycol]])) +
    scale_color_manual(name = input$line_legend_title,
                       values = cols_line) +
    scale_linetype_manual(values = type_line)
  
  
  
  if (input$line_show_dots) {g_line <- g_line + geom_point()}
  else{g_line <- g_line}
  
  g_line <- g_line +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title = input$line_title,
         x = input$line_xlabel,
         y = input$line_ylabel) +
    #hjust is used to center the title, size is used to change the text size of the title
    theme_output_line() +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          #allows user to change position of legend
          legend.position = input$line_legend_position)
  
}

plotLineplotInput_compare <- function(){
  #calls data function and stores it to selectedData
  if (input$compare_execute_run_model != 0)
  {
    selectedData <- gather(select(data.frame(compare_model_output()), input$lineplot_xvar, input$lineplot_yvar), Variable, Value, -one_of(input$lineplot_xvar))
  }
  else
  {
    selectedData <- gatherLinePlotData()
  }
 
  
  #create vector of cols for lines
  cols_line <- paste0("c(", paste0("input$cols_line", unique(sort(gatherLinePlotData()$Variable)), collapse = ", "), ")")
  cols_line <- eval(parse(text = cols_line))
  
  #create vector of linetypes for lines
  type_line <-  paste0("c(", paste0("input$line_type", unique(sort(gatherLinePlotData()$Variable)), collapse = ", "), ")")
  type_line <- eval(parse(text = type_line))
  # #print(type_line)
  
  #ggplot function to print using geom_line
  g_line <- ggplot(selectedData, aes(x = selectedData[,1], y = Value, color = Variable)) +
    geom_line(aes(linetype = Variable),
              size = input$line_size_options) +
    scale_color_manual(name = input$line_legend_title,
                       values = cols_line) +
    scale_linetype_manual(name = input$line_legend_title,
                          values = type_line)
  
  if (input$line_show_dots) {g_line <- g_line + geom_point()}
  else{g_line <- g_line}
  
  g_line <- g_line +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title = input$line_title_comparisonPlot,
         x = input$line_xlabel,
         y = input$line_ylabel) +
    #hjust is used to center the title, size is used to change the text size of the title
    theme_output_line() +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          #allows user to change position of legend
          legend.position = input$line_legend_position)
  
}

# Scatterplot Overlay ----------------------------------------------------------
overlay_scatter_data <- reactive({
  req(input$overlay_scatter_input)
  #fread(input$data$datapath, na.strings=c("", NA))
  if (endsWith(input$overlay_scatter_input$datapath, ".csv")) {
    read.csv(input$overlay_scatter_input$datapath)
  } else if (endsWith(input$overlay_scatter_input$datapath, ".txt")) {
    read.table(input$overlay_scatter_input$datapath,header = T)
  }else if (endsWith(input$overlay_scatter_input$datapath, ".xls")) {
    read_excel(input$overlay_scatter_input$datapath)
  } else if (endsWith(input$overlay_scatter_input$datapath, ".xlsx")) {
    read_xlsx(input$overlay_scatter_input$datapath,sheet = 1)
  }
})
observeEvent(input$overlay_scatter_input, {
  req(overlay_scatter_data())
  updateSelectInput(session
                    ,"overlay_scatter_xcol"
                    ,choices = colnames(overlay_scatter_data())
                    ,selected = colnames(overlay_scatter_data())[1])
  updateSelectInput(session
                    ,"overlay_scatter_ycol"
                    ,choices = colnames(overlay_scatter_data())
                    ,selected = colnames(overlay_scatter_data())[2])
})



