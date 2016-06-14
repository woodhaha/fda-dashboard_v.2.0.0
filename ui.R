library(shiny)
library(shinythemes)
library(rCharts)
library(shinydashboard)
library(jsonlite)
source("openfda.R")
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(ggthemes)

scriptHeaders <- function(inputId=NULL) {
  tagList(
    singleton(tags$head(tags$script(src = "js/gtm.js")))
  )
}

# dnames <- read.csv("diseasenames.csv",header=FALSE,row.names=1)
# names <- rownames(dnames)
drugs_dict <- read.csv("drug_dictionary.csv",skip=1,row.names=1)

####################################################
### Get the headers working
####################################################

header <-   dashboardHeader(title = "FDA Adverse Event Dashboard" , titleWidth = 350)

###########################################
## Get the sidebar
###########################################

sidebar <- dashboardSidebar(width = 250,
  #                 sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
  #                                   label = "Search Disease",  icon = shiny::icon("search")),
  sidebarMenu(
    menuItem("Drugs", tabName = "Drugs", icon=icon("dashboard"),menuSubItem("Plots", tabName ="drugS",icon = icon("search")),
             menuSubItem("indications/reactions", tabName ="dIR",icon = icon("table"))),
    
    menuItem("Pharmacological Class", tabName = "pharma", icon=icon("dashboard"),menuSubItem("Plots", tabName ="pharmS",icon = icon("search")),
             menuSubItem("indications/reactions", tabName ="pIR",icon = icon("table"))),
    menuItem("MOA Class",tabName = "moa",icon=icon("dashboard"),menuSubItem("MOA Search", tabName ="moaS",icon=icon("table"))),
    menuItem("DDI Reaction",tabName = "ddi",icon=icon("dashboard"),menuSubItem("DDI Search", tabName ="ddiS",icon=icon("table"))),
    menuItem("About",tabName = "about",icon=icon("dashboard"))
    
  )
  #imageOutput("climage",height = "150px")
)


body <- dashboardBody(
            tabItems(
              tabItem( tabName = "drugS" ,
                       fluidRow( box( status = "primary", title = "Search Drugs",
                           uiOutput("drugs")
                       )
                       ),
                       fluidRow(
                         box(
                           title = "Age distribution", status = "primary", width = 6, collapsible = T,
                           includeMarkdown("age.Rmd"),
                           plotOutput("ages"),
                           downloadButton('downloadPlot1', 'Download Plot')
                         ),
                         box( title = "Outcome Reports", status = "primary", width = 6, collapsible = T,
                              includeMarkdown("outcomes.Rmd"),
                              showOutput("outcome_plot","nvd3")
                         )),
                       
                       fluidRow(
                         box( title = "Reports Per Week", status = "primary", width = 12, collapsible = T,
                           includeMarkdown("reports_by_week.Rmd"),
                           showOutput("reports",lib="nvd3")
      
                           )                         
                       )
                ),
              tabItem( tabName = "dIR" ,
                       fluidRow(
                           box(
                               title = "Reactions Counts", status = "primary", width = 6, collapsible = T,
                               includeMarkdown("reactions.Rmd"),
                               dataTableOutput("reactions")
                           ),
                           box(
                               title = "Indications Counts", status = "primary", width = 6, collapsible = T,
                               dataTableOutput("indications")
                               )
              
              )
              ),
             tabItem(tabName = "pharmS",
                     fluidRow( box( status = "primary", title = "Search Pharmacological class",
                                    uiOutput("pharmclass")
                     )
                     ),
                     fluidRow(
                         box( title = "Counts of reports per week",status = "primary",width = 12, collapsible = T,
                              showOutput("pc_reports","nvd3")
                         )
                     ),
                     fluidRow(
                         box( title = "Count of reports per week",status = "primary",width = 12, collapsible = T,
                              dataTableOutput("pc_reports_by_week")
                         )
                     )
                     ),
             tabItem(tabName = "pIR",

                     fluidRow(
                         box( title = "Count of reactions by pharmacological class",status = "primary",width = 10, collapsible = T,
                             dataTableOutput("all_reactions")
                         )),
                    fluidRow(
                         box(title = "Count of indications by pharmacological class",status = "primary",width = 10, collapsible = T,
                             dataTableOutput("classIndication")
                         )
                     )),
             tabItem( tabName = "moaS",
                      fluidRow( box( status = "primary", title = "Search Mode Of Action",
                                     uiOutput("moa")
                      )),
                      fluidRow(
                          box( title = "Count of reactions by MOA",status = "primary",width = 12, collapsible = T,
                               dataTableOutput("moa_reactions")
                          )),
                      fluidRow(
                          box(title = "Count of indications by MOA",status = "primary",width = 12, collapsible = T,
                              dataTableOutput("moaIndication")
                          )
                      )),
             tabItem(tabName = "ddiS",
                     fluidRow( box( status = "primary", title = "Search Drugs for DDI",
                                    uiOutput("dr")
                     )),
                     fluidRow(
                       box( title = "Drug Drug Interactions",status = "primary",width = 12, collapsible = T,
                            includeMarkdown("ddi.Rmd"),
                            dataTableOutput("ddi_reactions") 
                       ))
                     
                     ),
             
             tabItem(tabName = "about",
                     includeMarkdown("about1.Rmd"),
                     #textOutput("deaths"), br(),
                     includeMarkdown("about2.Rmd"))
                      
             
            ),
            tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
                                      background-color: #041139;
                                      }
                                      
                                      /* logo when hovered */
                                      .skin-blue .main-header .logo:hover {
                                      background-color: #FFA12E;
                                      }
                                      
                                      /* navbar (rest of the header) */
                                      .skin-blue .main-header .navbar {
                                      background-color: #041139;
                                      }        
                                      
                                      /* main sidebar */
                                      .skin-blue .main-sidebar {
                                      background-color: #060606;
                                      }
                                      
                                      /* active selected tab in the sidebarmenu */
                                      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                      background-color: #FFFFF;
                                      }
                                      
                                      /* other links in the sidebarmenu */
                                      .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                      background-color: #060606;
                                      color: #FFFFF;
                                      }')))
              
  )

dashboardPage(header, sidebar, body)
