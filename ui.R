library(shiny)
library(shinythemes)
shinyUI(navbarPage(theme = shinytheme("cerulean"),"FDA Adverse Event Dashboard",position="static-top",
                   tabPanel("Search by Drugs",
                            br(),
                            sidebarPanel(
                                uiOutput("drugs"),
                                width = 5
                            ),
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("By age",
                                             h3("Reports by age"),
                                             includeMarkdown("age.Rmd"),
                                             plotOutput("ages"),
                                             downloadButton('downloadPlot1', 'Download Plot'),
                                             h4("Ages (% by group)"),
                                             dataTableOutput("age_shares"),
                                             hr(),
                                             h4("Total counts of adverse events by age"),
                                             dataTableOutput("ages_counts")
                                             
                                    ),
                                    tabPanel("By outcome",
                                             h3("Reports by outcome"),
                                             includeMarkdown("outcomes.Rmd"),
                                             plotOutput("outcome_plot"),
                                             downloadButton('downloadPlot2', 'Download Plot'),
                                             
                                             h4("Share(s) of outcomes"),
                                             dataTableOutput("outcome_shares"),
                                             hr(),
                                             h4("Total count of outcomes"),
                                             dataTableOutput("outcomes")
                                    ),
                                    tabPanel("By reaction",
                                             h3("Count of reports by reaction"),
                                             includeMarkdown("reactions.Rmd"),
                                             dataTableOutput("reactions")
                                    ),
                                    tabPanel("By Indication",
                                             h3("Count of reports by Indications"),
                                             dataTableOutput("indications")
                                    ),
                                    tabPanel("Per week",
                                             h3("Count of reports per week"),
                                             includeMarkdown("reports_by_week.Rmd"),
                                             plotOutput("reports"),
                                             checkboxInput("log_scale",
                                                           "Log y-axis (can clarify trends)",
                                                           value = TRUE),
                                             downloadButton('downloadPlot3', 'Download Plot'),
                                             
                                             hr(),
                                             h4("Count of reports per week"),
                                             p("Note: reverse sorted by week, recent weeks first"),
                                             dataTableOutput("reports_by_week")
                                    )
                                ),width = 8)
                   ),tabPanel("Search by Pharmacological Class",
                              br(),
                              sidebarPanel(
                                  uiOutput("pharmclass"),
                                  width = 5
                              ),mainPanel(
                                  tabsetPanel(
                                      tabPanel("By All reaction",
                                               h3("Count of reports by reaction"),
                                               #includeMarkdown("reactions.Rmd"),
                                               dataTableOutput("all_reactions")                                          
                                      ),tabPanel("By Indications",
                                                 h3("Count of reports by Indications"),
                                                 #includeMarkdown("reactions.Rmd"),
                                                 dataTableOutput("classIndication"))))),
                   tabPanel("Search by MOA",
                            br(),
                            sidebarPanel( 
                                uiOutput("moa"),
                                width=5
                            ),mainPanel(tabsetPanel(
                                tabPanel("By reaction",
                                         h3("Count of reports by reaction"),
                                         #includeMarkdown("reactions.Rmd"),
                                         dataTableOutput("moa_reactions")                                          
                                ),tabPanel("By Indications",
                                           h3("Count of reports by Indications"),
                                           #includeMarkdown("reactions.Rmd"),
                                           dataTableOutput("moaIndication"))))),
                   
                   tabPanel("About", br(),
                            includeMarkdown("about1.Rmd"),
                            #textOutput("deaths"), br(),
                            includeMarkdown("about2.Rmd")
                   )
))
