#This contains the overall UI flow of the Application
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

# load.lib<-c("shinydashboard", "shinydashboardPlus", "shiny","ggplot2","gridExtra","shinythemes",
#             "shinyWidgets","shinyjs","DT","tidyverse","dplyr","rhandsontable","data.table","ggpmisc",
#             "plotly","colourpicker","shinyBS","shinyjqui", "bsplus", "deSolve", "shinyFiles", "ggplot2"
#             ,"gridExtra", "shinythemes", "huxtable")
# 
# 
# install.lib<-load.lib[!load.lib %in% installed.packages()]
# for(lib in install.lib) install.packages(lib,dependencies=TRUE)
# sapply(load.lib,require,character=TRUE)

library(shinydashboard)
#library(shinydashboardPlus) #make sure this library is after shinydashboard
library(bs4Dash)
library(shiny)
library(ggplot2)
library(gridExtra)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(tidyverse)
library(dplyr)
library(rhandsontable)
library(data.table)
library(ggpmisc)
library(colourpicker)
library(shinyBS)
library(shinyjqui)
library(bsplus)
library(deSolve)
library(shinyFiles)
library(ggplot2)
library(gridExtra)
library(huxtable)
library(plotly)
library(Deriv)
#library(rapport)


#load files with UI outputs
source("./ui/01_model_varCreate_ui.R")
source("./ui/02_model_equationCreate_ui.R")
source("./ui/03_input_outputsUI.R")
source("./ui/04_model_parameters_ui.R")
source("./ui/05_model_ICs_ui.R")
source("./ui/06_model_diffEqs_ui.R")
source("./ui/07_model_options_ui.R")

source("./ui/11_run_executeUI.R")
source("./ui/12_run_post_processing.R")
source("./ui/13_run_lineplotUI.R")

source("./ui/21_export_ui.R")

source("./ui/31_documentationUI.R")


ui <- dashboardPage(header = dashboardHeader(title = "Sir BuildsAlot"),
                                        # ,enable_rightsidebar = TRUE
                                        # ,rightSidebarIcon = 'gears'),
                    sidebar = dashboardSidebar(
                                sidebarMenu(
                                  menuItem("Create Model", tabName = "TAB_MODEL_BUILD", startExpanded = TRUE
                                           ,menuSubItem("Create Variables", tabName = "TAB_VAR_CREATE", icon = icon("desktop"))
                                           ,menuSubItem("Equation Creation", tabName = "TAB_Equation_Create", icon = icon("chart-area"))
                                           ,menuSubItem("In/Out", tabName = "TAB_InOut")
                                           ,menuSubItem("Parameters", tabName = "TAB_Parameters")
                                           ,menuSubItem("Initial Conditions", tabName = "TAB_ICs")
                                           ,menuSubItem("Differential Equations", tabName = "TAB_diffEqs")
                                           ,menuSubItem("Options", tabName = "TAB_MODEL_OPTIONS")
                                  )
                                  # ,menuItem("Run Model", tabName = "TAB_RUN_MODEL"
                                  #           ,menuSubItem("Execute Model", tabName = "TAB_RUN_EXECUTE")
                                  #           ,menuSubItem("Post Processing", tabName = "TAB_run_post_processing")
                                  #           ,menuSubItem("Plot Model", tabName = "TAB_RUN_LINEPLOT"))
                                  # ,menuItem("Export", tabName = "TAB_export")
                                  # ,menuItem("Documentation", tabName = "TAB_DOCUMENTATION")


                      )#end SideBarMenu
                    ), #end dashboardSidebar
                    body = dashboardBody(
                      #tags$style(js),

                      #activates shiny javascript so that I can play with vanishing and appearing div files
                       useShinyjs()
                      ,withMathJax()
                      ,tags$script(src = "popup.js")
                      ,tags$script(src = "press_enter.js")
                      
                      ,tabItems(TAB_VAR_CREATE
                               ,TAB_Equation_Create
                               ,TAB_InOut
                               ,TAB_ICs
                               ,TAB_Parameters
                               ,TAB_diffEqs
                               ,TAB_MODEL_OPTIONS
                               ,TAB_export
                               ,TAB_RUN_EXECUTE
                               ,TAB_run_post_processing
                               ,TAB_RUN_LINEPLOT
                               ,TAB_DOCUMENTATION
                               )
                    ) #end dashboardBody

                    ,controlbar = dashboardControlbar(fileInput("load_model"
                                                                ,"Load Model"
                                                                ,placeholder = "Choose .rds File"
                                                                ,multiple = FALSE
                                                                ,accept = c(".rds")
                                                                )
                                                      ,h4("Debugging Tools")
                                                      ,actionButton(inputId = "param_view_parameters"
                                                                    ,label = "View Parameters"
                                                                    ,style = "color: #fff; background-color: green; border-color: #2e6da4")
                                                      ,hr()
                                                      ,actionButton(inputId = "param_remove_duplicate_parameters"
                                                                    ,label = "Delete Duplicate Parameters"
                                                                    ,style = "color: #fff; background-color: green; border-color: #2e6da4")
                                                      #,div(class = "p-3", skinSelector())
                                                      ,div(skinSelector())
                                                      )
                    ,footer = NULL
                    ,dark = TRUE
) #end dashboardPage

