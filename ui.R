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




























# shinyUI(navbarPage(theme = "mycosmo.css",img(src = "./img/Picture1.png", height = 32, width = 50),windowTitle="Open FDA",
#                    tabPanel("Search by Drugs",
#                             br(),
#                             sidebarPanel(
#                                 uiOutput("drugs"),
#                                 width = 5
#                             ),
#                             mainPanel(
#                                 tabsetPanel(
#                                     tabPanel("By age",
#                                              h3("Reports by age"),
#                                              includeMarkdown("age.Rmd"),
#                                              plotOutput("ages"),
#                                              downloadButton('downloadPlot1', 'Download Plot'),
#                                              h4("Ages (% by group)"),
#                                              dataTableOutput("age_shares"),
#                                              hr(),
#                                              h4("Total counts of adverse events by age"),
#                                              dataTableOutput("ages_counts")
#                                              
#                                     ),
#                                     tabPanel("By outcome",
#                                              h3("Reports by outcome"),
#                                              includeMarkdown("outcomes.Rmd"),
#                                              showOutput("outcome_plot","nvd3"),
#                                              #plotOutput("outcome_plot"),
#                                              #downloadButton('downloadPlot2', 'Download Plot'),
#                                              
#                                              h4("Share(s) of outcomes"),
#                                              dataTableOutput("outcome_shares"),
#                                              hr(),
#                                              h4("Total count of outcomes"),
#                                              dataTableOutput("outcomes")
#                                     ),
#                                     tabPanel("By reaction",
#                                              h3("Count of reports by reaction"),
#                                              includeMarkdown("reactions.Rmd"),
#                                              dataTableOutput("reactions")
#                                     ),
#                                     tabPanel("By Indication",
#                                              h3("Count of reports by Indications"),
#                                              dataTableOutput("indications")
#                                     ),
#                                     tabPanel("Per week",
#                                              h3("Count of reports per week"),
#                                              includeMarkdown("reports_by_week.Rmd"),
#                                              showOutput("reports","nvd3"),
#                                              #dygraphOutput("reports"),
#                                              #plotOutput("reports"),
#                                              checkboxInput("log_scale",
#                                                            "Log y-axis (can clarify trends)",
#                                                            value = TRUE),
#                                              downloadButton('downloadPlot3', 'Download Plot'),
#                                              
#                                              hr(),
#                                              h4("Count of reports per week"),
#                                              p("Note: reverse sorted by week, recent weeks first"),
#                                              dataTableOutput("reports_by_week")
#                                     )
#                                 ),width = 8)
#                    ),tabPanel("Search by Pharmacological Class",
#                               br(),
#                               sidebarPanel(
#                                   uiOutput("pharmclass"),
#                                   width = 5
#                               ),mainPanel(
#                                   tabsetPanel(
#                                       tabPanel("By All reaction",
#                                                h3("Count of reports by reaction"),
#                                                #includeMarkdown("reactions.Rmd"),
#                                                dataTableOutput("all_reactions")                                          
#                                       ),tabPanel("By Indications",
#                                                  h3("Count of reports by Indications"),
#                                                  #includeMarkdown("reactions.Rmd"),
#                                                  dataTableOutput("classIndication")
#                                       ),tabPanel("Per week",
#                                                  h3("Count of reports per week"),
#                                                  #includeMarkdown("reports_by_week.Rmd"),
#                                                  showOutput("pc_reports","nvd3"),
#                                                  #plotOutput("pc_reports"),
#                                                  checkboxInput("log_scale",
#                                                                "Log y-axis (can clarify trends)",
#                                                                value = TRUE),
#                                                  downloadButton('downloadPlot4', 'Download Plot'),
#                                                  
#                                                  hr(),
#                                                  h4("Count of reports per week"),
#                                                  p("Note: reverse sorted by week, recent weeks first"),
#                                                  dataTableOutput("pc_reports_by_week")
#                                       )))),
#                    tabPanel("Search by MOA",
#                             br(),
#                             sidebarPanel( uiOutput("moa"),width=5
#                             ),mainPanel(tabsetPanel(
#                                 tabPanel("By reaction",
#                                          h3("Count of reports by reaction"),
#                                          #includeMarkdown("reactions.Rmd"),
#                                          dataTableOutput("moa_reactions")                                          
#                                 ),tabPanel("By Indications",
#                                            h3("Count of reports by Indications"),
#                                            #includeMarkdown("reactions.Rmd"),
#                                            dataTableOutput("moaIndication"))))),
#                    tabPanel("Reaction Stats",
#                             br(),
#                             sidebarPanel( uiOutput("dr"),width=10
#                             ),mainPanel(tabsetPanel(
#                                 tabPanel("By DDI",
#                                          h3("Reports By DDI"),
#                                          includeMarkdown("ddi.Rmd"),
#                                          #includeMarkdown("reactions.Rmd"),
#                                          dataTableOutput("ddi_reactions")                                          
#                                 ),
#                                 tabPanel("By Reaction",
#                                          h3("Reports By Reaction"),
#                                          #includeMarkdown("dR.Rmd"),
#                                          #includeMarkdown("reactions.Rmd"),
#                                          dataTableOutput("dr_reactions"))))),
#                    
#                    #         tabPanel("Drug Stats",
#                    #                  br(),
#                    #                  sidebarPanel( uiOutput("dstat"),width=10
#                    #                  ),mainPanel(tabsetPanel(
#                    #                      tabPanel("Drug Stats",
#                    #                               h3("Reports By Reactions"),
#                    #                               includeMarkdown("dstat.Rmd"),
#                    #                               #includeMarkdown("reactions.Rmd"),
#                    #                               dataTableOutput("dstat_reactions")                                          
#                    #                      )))),                                                                                            
#                    tabPanel("About", br(),
#                             includeMarkdown("about1.Rmd"),
#                             #textOutput("deaths"), br(),
#                             includeMarkdown("about2.Rmd")
#                    ),
#                    tags$head(
#                        tags$script(src="disqus.js"),
#                        tags$script("$(function() {$.fn.dataTableExt.errMode = 'none';});"),
#                        tags$link(rel = "stylesheet",
#                                  type = "text/css",
#                                  href = "base.css")
#                    )
# ))
