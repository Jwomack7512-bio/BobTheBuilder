
# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
# 
#  # SUBTAB: LinePlot
#  # LINEPLOT000
# 
# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
# #__________________________________________________________________________________________________________
# 
#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updateSelectInput(session, "lineplot_xvar", choices = input$checkbox)
})

#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$checkbox, {
  updatePickerInput(session,
                    "lineplot_yvar",
                    choices = input$checkbox)
})

#changes xlabel for line plot to selected input
observeEvent(input$lineplot_xvar,{
  updateTextInput(session,
                  "line_xlabel",
                  label="X Label",
                  value=input$lineplot_factor_var)
})

#changes xlabel for line plot to selected input
observeEvent(input$lineplot_yvar,{
  updateTextInput(session,
                  "line_ylabel",
                  label="Y Label",
                  value='Values')
})

gatherLinePlotData <- function(){
  req(input$lineplot_yvar)
  selectedData <- gather(select(df_to_use(), input$lineplot_xvar, input$lineplot_yvar), Variable, Value, -one_of(input$lineplot_xvar))
  
}

# #Renders the color panel for each different stratified categorical variable (each at varied distance color levels)
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



#prints datat table out to text ouptut (will probably delete)
output$test <- renderPrint({
  print(head(gatherLinePlotData()))
})

#this function talkes multiple inputs, and factors them into one column, creating a second column of corresponding groups
#groups are stored in variable :Variable, call with gatherLinePlotData()$Variable
#data stores in cariable: Value, called same way
gatherLinePlotData <- function(){
  req(input$lineplot_yvar)
  selectedData <- gather(select(df_to_use(), input$lineplot_xvar, input$lineplot_yvar), Variable, Value, -one_of(input$lineplot_xvar))
  
}

theme_output_line <- function(){
  if (input$theme_output_line=='gray'){
    theme_gray()}
  else if(input$theme_output_line=='bw'){
    theme_bw()}
  else if(input$theme_output_line=='linedraw'){
    theme_linedraw()}
  else if(input$theme_output_line=='light'){
    theme_light()}
  else if(input$v=='minimal'){
    theme_minimal()}
  else if(input$theme_output_line=='classic'){
    theme_classic()}
  else if(input$theme_output_line=='void'){
    theme_void()}
  else if(input$theme_output_line=='void'){
    theme_dark()}
}

#this is the function that creates the ggplot object for the line plot
plotLineplotInput <- function(){
  #calls data function and stores it to selectedData
  selectedData <- gatherLinePlotData()
  
  #create vector of cols for lines
  cols_line <- paste0("c(", paste0("input$cols_line", unique(sort(gatherLinePlotData()$Variable)), collapse=", "), ")")
  cols_line <-eval(parse(text=cols_line))
  
  #create vector of linetypes for lines
  type_line <-  paste0("c(", paste0("input$line_type", unique(sort(gatherLinePlotData()$Variable)), collapse=", "), ")")
  type_line <- eval(parse(text=type_line))
  #print(type_line)
  
  #ggplot function to print using geom_line
  g_line <- ggplot(selectedData, aes(x=selectedData[,1], y=Value, color=Variable)) +
    geom_line(aes(linetype=Variable),
              size=input$line_size_options) +
    scale_color_manual(name=input$line_legend_title,
                       values=cols_line) +
    scale_linetype_manual(values=type_line)
  
  if(input$line_show_dots){g_line <- g_line + geom_point()}
  else{g_line <- g_line}
  
  g_line <- g_line +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title=input$line_title,
         x=input$line_xlabel,
         y=input$line_ylabel) +
    #hjust is used to center the title, size is used to change the text size of the title
    theme_output_line() +
    theme(plot.title = element_text(hjust = 0.5, size=22),
          #allows user to change position of legend
          legend.position = input$line_legend_position)
  
}

#create LinePlot using ggplot
output$LinePlot <- renderPlot({
  print(plotLineplotInput())
})

output$downloadLine <- downloadHandler(
  filename = function(){
    paste(input$line_download_title, input$line_download_radiobuttons, sep="")
  },
  content = function(file){
    ggsave(file, plotLineplotInput())
  }
)