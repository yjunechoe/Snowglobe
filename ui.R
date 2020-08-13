## Setup

library(shiny)
library(shinydashboard)
source('app_source/snowballer_source.R')

options(shiny.maxRequestSize=500*1024^2)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Snowballer"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = "HomeTab", icon = icon("home")),
                        menuItem("Upload Running List", tabName = "RunningListTab", icon = icon("list-alt")),
                        menuItem("Stage Papers", tabName = "PrepareTab", icon = icon("dashboard")),
                        menuItem("Run Search", tabName = "RunTab", icon = icon("project-diagram")),
                        menuItem("Search Statistics", tabName = "StatisticsTab", icon = icon("chart-bar")),
                        menuItem("Manual", tabName = "ManualTab", icon = icon("file-alt")),
                        menuItem("Settings", tabName = "SettingsTab", icon = icon("gear")),
                        menuItem("About", tabName = "AboutTab", icon = icon("info"))
                      )
                    ),
                    dashboardBody(
                      # Navigation prompt
                      tags$head(tags$script(HTML("
                                                 // Enable navigation prompt
                                                 window.onbeforeunload = function() {
                                                 return 'Your changes will be lost!';
                                                 };
                                                 "))),  
                      tabItems(
                        tabItem(tabName = "HomeTab",
                                includeMarkdown("app_source/snowballer_home.Rmd")
                                
                        ),
                        tabItem(tabName = "RunningListTab",
                                fluidRow(
                                  valueBoxOutput("LastSearchValue", width = 3),
                                  valueBoxOutput("SearchNumberValue", width = 3),
                                  valueBoxOutput("PapersSnowballedValue", width = 3),
                                  valueBoxOutput("PreviouslySearchedValue", width = 3)
                                ),
                                fluidRow(
                                  box(width = 3,
                                      h2(strong("Not Your First Search?"), align = "center"),
                                      fileInput("RunningList",
                                                h4(strong("Upload your running list of papers:"))),
                                      h2(strong("First Search?"), align = "center"),
                                      h4(strong("If you are starting your project from scratch, skip to the Prepare Search tab.")),
                                      br(),
                                      h4(strong("OR, if you have a list of papers you've found so far, follow these steps:")),
                                      br(),
                                      h4("1. Download the template below and fill it out much as possible."),
                                      downloadButton("RunningListTemplateDownload", label = "Download Template"),
                                      fileInput("RunningListTemplate",
                                                HTML("<br><h4>2. Upload it back here:</h4>")),
                                      h4("3. Keep your processed running list."),
                                      downloadButton("RunningListDownload", label = "Download Running List"),
                                      HTML("<br><br><h4>4. Manually search for and fill in missing IDs where necessary 
                                           and upload your edited running list of papers at the very top.</h4>")
                                  ),
                                  box(width = 9,
                                      h3(strong("Running List"), align = "center"),
                                      dataTableOutput("UploadedTable")
                                  )
                                )
                        ),
                        tabItem(tabName = "PrepareTab",
                                fluidRow(
                                  valueBoxOutput("StagedValue", width = 4),
                                  valueBoxOutput("UniqueValue", width = 4),
                                  valueBoxOutput("NewValue", width = 4)
                                ),
                                fluidRow(
                                  box(width = 12,
                                      textInput("LookupInput",
                                                label = h4(strong("Look up a paper in the Microsoft Academic database")),
                                                placeholder = 'Enter a Title, DOI, or Microsoft Academic ID (if multiple IDs, separate by comma)'),
                                      actionButton("LookupButton",
                                                   label = "Search"),
                                      dataTableOutput("LookupTable"), br(),
                                      actionButton("LookupPush",
                                                   label = "Push to Staging Area")
                                  )
                                ),
                                fluidRow(
                                  box(width = 12,
                                      h3(strong("Staging Area"), align = "center"),
                                      div(actionButton("StageFromFile", "Stage Directly from File"),
                                          style = "float:right"), br(), 
                                      dataTableOutput("StagedTable"), br(),
                                      downloadButton("DownloadStaged",
                                                     label = "Download Staged Papers (.csv)")
                                  )
                                )
                        ),
                        tabItem(tabName = "RunTab",
                                fluidRow(
                                  box(width = 3,
                                      h3(strong("Search options"), align = "center"),
                                      checkboxInput("GetAbstracts", "Add Abstracts", FALSE),
                                      div(style = "margin-bottom:10px"),
                                      actionButton("ComprehensiveSearch",
                                                   label = "Search"),
                                      h3(strong("Search Info"), align = "center"),
                                      textOutput("search_summary_msg"), br(),
                                      h3(strong("Download"), align = "center"),
                                      downloadButton("DownloadOutput",
                                                     label = "Download Output (.csv)")
                                      ## TODO: download updated running list
                                  ),
                                  box(width = 9,
                                      h2(strong("Search Output"), align = "center"),
                                      dataTableOutput("OutputTable")
                                  )
                                )
                        ),
                        tabItem(tabName = "ManualTab"),
                        tabItem(tabName = "StatisticsTab",
                                fluidRow(
                                  tabBox(id = "StatisticsTabset", width = 12, height = "750px",
                                         tabPanel(value = "SummaryTab", h4(strong(" Summary "))),
                                         tabPanel(value = "WordCloudTab", h4(strong(" Word Cloud ")),
                                                  
                                                  fluidRow(
                                                    
                                                    box(width = 3,
                                                        sliderInput("WordCloudSize", label = "Adjust Text Size",
                                                                    value = 0.5, min = 0.1, max = 1, step = 0.1),
                                                        dataTableOutput("WordTable")
                                                    ),
                                                    
                                                    box(width = 9,
                                                        h3(strong("Word Cloud from Titles"), align = "center"),
                                                        br(),
                                                        wordcloud2Output("WordCloud")
                                                    )
                                                    
                                                  )
                                         ),
                                         tabPanel(value = "NetworkVizTab", h4(strong(" Network Visualization ")),
                                                  visNetworkOutput("visualnetwork"))
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
                                  )
                                )
                        )
                      )
                    )
)