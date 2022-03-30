## Setup

library(shiny)
library(shinydashboard)
source('app_source/snowglobe.R')

options(shiny.maxRequestSize=500*1024^2)

ui <- dashboardPage(
                    skin = "purple",
                    dashboardHeader(title = "SnowGlobe"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = "HomeTab", icon = icon("home")),
                        menuItem("Prepare Search", tabName = "PrepareTab", icon = icon("dashboard")),
                        menuItem("Run Search", tabName = "RunTab", icon = icon("project-diagram")),
                        menuItem("Search Statistics", tabName = "StatisticsTab", icon = icon("chart-bar")),
                        #menuItem("Manual", tabName = "ManualTab", icon = icon("file-alt")),
                        menuItem("Settings", tabName = "SettingsTab", icon = icon("gear")),
                        menuItem("FAQ", tabName = "FAQTab", icon = icon("question-circle")),
                        menuItem("About", tabName = "AboutTab", icon = icon("info"))
                      )
                    ),
                    dashboardBody(
                      #testing
                      # tags$head(includeHTML(("google_analytics.html"))),
                      # CSS
                      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "snowglobe.css")),
                      # Navigation prompt
                      tags$head(tags$script(HTML("window.onbeforeunload = function() {return 'Your changes will be lost!'};"))),
                      # Sidebar tabs
                      tabItems(
                        tabItem(tabName = "HomeTab",
                                includeMarkdown("app_source/home.Rmd")
                                
                        ),
                        tabItem(tabName = "PrepareTab",
                                fluidRow(
                                  valueBoxOutput("ScreenedValue", width = 3),
                                  valueBoxOutput("StagedValue", width = 3),
                                  valueBoxOutput("UniqueValue", width = 3),
                                  valueBoxOutput("NewValue", width = 3)
                                ),
                                fluidRow(
                                  box(width = 12,
                                      h3(strong("Not Your First Search? Upload List of Previously-Screened Titles")),
                                      actionButton("RunningListOptions", "Upload", style = "float:left"))
                                  
                                ),
                                fluidRow(
                                  box(width = 12,
                                      h3(strong("Papers to Search"), align = "center"),
                                      div(actionButton("StageFromFile", "Upload List of Titles and Years"),
                                          style = "float:right"), br(), 
                                      dataTableOutput("StagedTable"), br(),
                                      downloadButton("DownloadStaged",
                                                     label = "Download List of Papers to be Searched (.csv)")
                                  )
                                ),
                                # fluidRow(
                                #   box(width = 12,
                                #       textInput("LookupInput",
                                #                 label = h4(strong("Look up a paper in the Microsoft Academic database")),
                                #                 placeholder = 'Enter a Title, DOI, or Microsoft Academic ID (if multiple IDs, separate by comma)'),
                                #       actionButton("LookupButton",
                                #                    label = "Search"),
                                #       dataTableOutput("LookupTable"), br(),
                                #       actionButton("LookupPush",
                                #                    label = "Push to Staging Area")
                                #   )
                                # )
                                # fluidRow(
                                #   box(width = 12,
                                #       h3(strong("Search options"), align = "center"),
                                #       checkboxInput("GetAbstracts", "Fetch Abstracts", FALSE)
                                #   )
                                # )
                                
                        ),
                        tabItem(tabName = "RunTab",
                                fluidRow(
                                  box(width = 12,
                                      h2(strong("Search Output"), align = "center"),
                                      actionButton("ComprehensiveSearch",
                                                   label = "Search"),
                                      textOutput("search_summary_msg"), br(),
                                      ## TODO: download updated running list
                                      dataTableOutput("OutputTable")
                                  ),
                                  box(width = 12,
                                      h3(strong("Download"), align = "center"),
                                      downloadButton("DownloadOutput",
                                                     label = "Download Output (.csv)"),
                                      downloadButton("DownloadRIS", label = "Download Output (.RIS)"),
                                      downloadButton("UpdatedRunningListDownload", label = "Download Running List"))
                                )
                        ),
                        #tabItem(tabName = "ManualTab"),
                        tabItem(tabName = "StatisticsTab",
                                fluidRow(
                                  tabBox(id = "StatisticsTabset", width = 12, height = "750px",
                                         tabPanel(value = "SummaryTab", h4(strong(" Summary ")),
                                                  
                                                  fluidRow(
                                                    box(width = 12, plotOutput("YearsPlot"))
                                                  ),
                                                  fluidRow(
                                                    box(width = 12, plotOutput("AuthorsPlot")),
                                                  ),
                                                  fluidRow(
                                                    box(width = 12, plotOutput("JournalsPlot")),
                                                  )
                                                  
                                          ) #,
                                         # tabPanel(value = "WordCloudTab", h4(strong(" Word Cloud ")),
                                         #          
                                         #          fluidRow(
                                         #            
                                         #            box(width = 3,
                                         #                sliderInput("WordCloudSize", label = "Adjust Text Size",
                                         #                            value = 0.5, min = 0.1, max = 1, step = 0.1),
                                         #                dataTableOutput("WordTable")
                                         #            ),
                                         #            
                                         #            box(width = 9,
                                         #                h3(strong("Word Cloud from Titles"), align = "center"),
                                         #                br(),
                                         #                wordcloud2Output("WordCloud")
                                         #            )
                                         #            
                                         #          )
                                         # ),
                                         # tabPanel(value = "NetworkVizTab", h4(strong(" Network Visualization ")),
                                         #          visNetworkOutput("visualnetwork")
                                         # )
                                  )
                                )
                        ),
                        tabItem(tabName = "SettingsTab",
                                
                                fluidRow(
                                  box(width = 4,
                                      
                                      # API key inputs
                                      h3(strong("Set API keys"), align = "center"),
                                      textInput("MA_key",
                                                h4(strong("Microsoft Academic Key"), "<",
                                                   a("GET", href="https://msr-apis.portal.azure-api.net/products/project-academic-knowledge"), "> :"),
                                                placeholder = "(Leave blank to use stored key)"),
                                      textInput("EL_key", 
                                                h4(strong("Elsevier Key"), "<",
                                                   a("GET", href="https://dev.elsevier.com"), "> :"),
                                                placeholder = "(Leave blank to use stored key)")
                                  ),
                                  box(width = 2,
                                      
                                      h3(strong("Developer tools", align = "center")),
                                      actionButton("DebugBrowser", "Call debugger")
                                      )
                                )
                        ),
                        tabItem(tabName = "FAQTab",
                                includeMarkdown("app_source/faq.Rmd")),
                        tabItem(tabName = "AboutTab",
                                includeMarkdown("app_source/about.Rmd"))
                      )
                      )
                      )
