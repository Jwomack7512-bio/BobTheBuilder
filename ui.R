#This contains the overall UI flow of the Application
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

# load.lib<-c("shinydashboard", "bs4Dash", "shiny","ggplot2","gridExtra","shinythemes",
#             "shinyWidgets","shinyjs","DT","tidyverse","dplyr","rhandsontable","data.table","ggpmisc",
#             "colourpicker","shinyBS","shinyjqui", "bsplus", "plotly", "deSolve", "waiter", "ggpubr",
#             "viridis", "Deriv", "shinycssloaders")
# 
# load.lib <- c("shinydashboard", "bs4Dash", "shiny", "ggplot2", "gridExtra","shinythemes",
#               "shinyWidgets", "shinyjs", "DT", "tidyverse", "dplyr", "rhandsontable", "data.table",
#               "ggpmisc", "colourpicker", "shinyBS", "shinyjqui", "bsplus", "deSolve", "plotly",
#               "Deriv", "viridis", "ggpubr", "shinycssloaders", "waiter", "fresh", "readxl",
#               "minpack.lm", "measurements", "qdapRegex")

# 
# install.lib<-load.lib[!load.lib %in% installed.packages()]
# for(lib in install.lib) install.packages(lib,dependencies=TRUE)
# sapply(load.lib,require,character=TRUE)

library(shinydashboard)
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
library(plotly)
library(Deriv)
library(viridis)
library(ggpubr)
library(shinycssloaders)
library(waiter)
library(fresh)
library(readxl)
library(minpack.lm)
library(measurements)
library(qdapRegex)
library(XML)
library(xml2)
library(katex)
library(reshape2)

#load files with UI outputs
source("./ui/00_home_ui.R")
source("./ui/01_create_model_ui.R")
source("./ui/11_run_execute_ui.R")
source("./ui/12_run_post_processing_ui.R")
source("./ui/13_run_lineplot_ui.R")

source("./ui/21_export_ui.R")

source("./ui/31_documentation_ui.R")
source("./ui/41_summary_ui.R")
source("./ui/contributions_ui.R")
source("./ui/51_parameter_estimination_ui.R")
source("./ui/61_global_options_ui.R")
source("./ui/debug_ui.R")

loading_screen <- tagList(
  spin_pong(), 
  h3("Loading Model...")
)

ui <- dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand(
      title = "BioModME",
      #color = "primary",
      image = "icon.svg"
    )
  ),
  sidebar = 
    dashboardSidebar(
       skin = "light",
       sidebarMenu(
        #menuItem("Home", tabName = "TAB_HOME", icon = icon("home")),
        menuItem(
          "Create Model",
          tabName = "TAB_VAR_CREATE",
          icon = icon("tasks", lib = "glyphicon")
        ),
        menuItem("Execute Model",
                 tabName = "TAB_RUN_EXECUTE",
                 icon = icon("laptop-code")),
        
        menuItem("Visualization",
                 tabName = "TAB_RUN_LINEPLOT",
                 icon = icon("images")),
        
        menuItem("Modeler's Toolbox",
                 tabName = "TAB_Toolbox",
                 icon = icon("toolbox"),
                 menuSubItem("Parameter Estimation",
                             tabName = "TAB_PARAMETER_ESTIMATION")),
        menuItem("Export", 
                 tabName = "TAB_EXPORT", 
                 icon = icon("file-export")),
        menuItem("Summary", 
                 tabName = "TAB_SUMMARY", 
                 icon = icon("list-alt")),
        menuItem("Options", 
                 tabName = "TAB_GLOBAL_OPTIONS",
                 icon = icon("tags", lib = "glyphicon")),
        menuItem("Documentation", 
                 tabName = "TAB_DOCUMENTATION",
                 icon = icon("book")),
        menuItem("Debug", 
                 tabName = "TAB_DEBUG", 
                 icon = icon("erase", lib = "glyphicon")),
       
        menuItem("Contributions", 
                 tabName = "TAB_CONTRIBUTIONS"),
       
        absolutePanel("Version 1.0.0",
                      bottom = 0,
                      left = 5,
                      fixed = TRUE)
       )#end SideBarMenu
     ), #end dashboardSidebar
  
  body = dashboardBody(
    autoWaiter(
      "eqnCreate_equationBuilder_chem",
      color = "white",
      html = spin_refresh()
    ),
    # Apply outside functionalities
    useShinyjs(),
    withMathJax(),
    useWaiter(),
    useSweetAlert(),
    #apply css
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "css/nonColorStyling.css"),
    
    # Sets mathjax context menu to show up on modals
    tags$div(HTML(
      "<script type='text/x-mathjax-config'>
        MathJax.Hub.Config({
          TeX: {
            Macros: {
            set: ['\\left\\{#1\\right\\}',1],
            }
          },
        MathMenu: {
          styles: {
            '#MathJax_About': {'z-index':1201},
            '.MathJax_Menu': {'z-index':1201}
          }
        }
        });

        MathJax.Hub.Register.StartupHook('MathMenu Ready',function () {
          MathJax.Menu.BGSTYLE['z-index'] = 1200;
        });
      </script>"
    )),

    
    # Apply js functionalites from scripts
    includeScript("www/js/popup.js"),
    includeScript("www/js/select_all.js"),
    includeScript("www/js/remove_all.js"),
    includeScript("www/js/press_enter.js"),
    
    # Functionality for changing page themes
    #uiOutput("css_themes"),
    
    # Apply tabs
    tabItems(
      TAB_HOME,
      TAB_VAR_CREATE,
      TAB_EXPORT,
      TAB_RUN_EXECUTE,
      TAB_RUN_LINEPLOT,
      TAB_SUMMARY,
      TAB_PARAMETER_ESTIMATION,
      TAB_GLOBAL_OPTIONS,
      TAB_DOCUMENTATION,
      TAB_CONTRIBUTIONS,
      TAB_DEBUG
    )
  ), #end dashboardBody
  
  # Sidebar of main page
  controlbar = dashboardControlbar(
    fileInput(
      "file_input_load_rds",
      "Load From .rds",
      placeholder = "Choose .rds File",
      multiple = FALSE,
      accept = c(".rds")
    ),
    fileInput(
      "file_input_load_sbml",
      "Load From SBML",
      placeholder = " .xml",
      multiple = FALSE,
      accept = c(".xml")
    ),
    pickerInput(
      inputId = "css_selector",
      label = "Select Skin",
      choices = c("Default",
                  "Night",
                  "RoyalBlue"),
      select = "Default"
    ),
    div(skinSelector()), 
    "$$\\require{mhchem}$$",
  )
  #,footer = NULL
  # Needed to remove light/dark switch
  , dark = NULL
) #end dashboardPage

