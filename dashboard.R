library(shiny)
library(shinydashboard)
library(tidytext)
library(glue)

library(cleanNLP)
library(wordcloud2)


cnlp_init_udpipe()

options(shiny.maxRequestSize=500*1024^2)

source('app_source/snowballer_source.R')


ui <- dashboardPage(skin = "black",
                    
  dashboardHeader(title = strong("Snowballer")),
   
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "HomeTab", icon = icon("home")),
      menuItem("Upload Running List", tabName = "RunningListTab", icon = icon("list-alt")),
      menuItem("Prepare Search", tabName = "PrepareTab", icon = icon("dashboard")),
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
                    br(),
                    
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

                    HTML("<br><br><h4>4. Fill in missing IDs where necessary and
                         upload your edited running list of papers at the very top.</h4>")
                    
                    
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
                              label = h4(strong("Look up a paper in the Microsoft Academic database:")),
                              placeholder = "Enter a Title, DOI, Microsoft Academic ID, or Microsoft Academic page URL"),

                    actionButton("LookupButton",
                                label = "Search"),
                    
                    dataTableOutput("LookupTable"), br(),
                    
                    actionButton("LookupPush",
                                 label = "Push to Staging Area")
                )
              ),
              
              
              fluidRow(
                box(width = 12,
                    h3(strong("Staging Area"), align = "center"), br(),
                    
                    dataTableOutput("StagedTable"), br(),
                    
                    downloadButton("DownloadStaged",
                                   label = "Download Staged Papers")
                    
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
                    
                    downloadButton("DownloadOutput",
                                   label = "Download Output")

                    
                ),
                
                
                box(width = 9,
                    h2(strong("Search Output"), align = "center"),
                    dataTableOutput("OutputTable")
                    
                )
              )
      ),
      
      
      
      
      tabItem(tabName = "ManualTab"
              
              
              
              
              
              
              ),
              
      
              
              
      
      tabItem(tabName = "StatisticsTab",
              
              fluidRow(
                tabBox(id = "StatisticsTabset", width = 12, height = "750px",
                         
                       
                      tabPanel(h3(strong(" Info ")), value = "Statistics Info"),
                       
                      
                      tabPanel(h3(strong(" Word Cloud ")), value = "WordCloudTab",
                               
                               fluidRow(
                                 
                                 box(width = 2,
                                     h2("hi"),
                                     sliderInput("WordCloudSize", label = "Wordcloud Text Size",
                                                 value = 0.5, min = 0.1, max = 1, step = 0.1)
                                 ),
                                 
                                 box(width = 7,
                                     wordcloud2Output("WordCloud")
                                 ),
                                 
                                 box(width = 3,
                                     dataTableOutput("WordTable")
                                 )
                                 
                               )
                      ),
                      
                      
                      tabPanel(h3(strong(" Network Visualization ")), value = "NetworkVizTab",
                               visNetworkOutput("visualnetwork"))
                  
                  
                )
                
                
              )
              
      ),
      
      
      
      
      tabItem(tabName = "SettingsTab",
              
              fluidRow(
                box(width = 4,
                    
                    # API key inputs
                    h3(strong("Set API keys"), align = "center"),
                    textInput("MA_key", h4(strong("Microsoft Academic Key"), "<",
                                           a("GET", href="https://msr-apis.portal.azure-api.net/products/project-academic-knowledge"), "> :"),
                              placeholder = "(Leave blank to use stored key)"),
                    textInput("EL_key", h4(strong("Elsevier Key"), "<",
                                           a("GET", href="https://dev.elsevier.com"), "> :"),
                              placeholder = "(Leave blank to use stored key)")
                )
              )
              
      )
      
      
      
    )
    
  )
)
server <- function(input, output) {
  
  
  
  ######################
  ##                  ##
  ##   Progress Tab   ##
  ##                  ##
  ######################
  
  
  
  ### Running List Uploaded ###
  
  # process uploaded list
  uploaded_list <- reactive({
    if(is.null(input$RunningList)) return (NULL)
    if(file.exists(input$RunningList$datapath)){
      read.csv(input$RunningList$datapath)
    } else {return(NULL)}
  })

  
  ### Running List Template ###
  
  # template download
  output$RunningListTemplateDownload <- downloadHandler(
    filename = function() {"Template.csv"},
    content = function(file) {
      write_csv(tibble(Title = character(), DOI = character(),
                       PMID = numeric()),
                file)
    }
  )
  
  # process uploaded template
  uploaded_template <- reactive({
    if(is.null(input$RunningListTemplate)) return (NULL)
    if(file.exists(input$RunningListTemplate$datapath)){
      read.csv(input$RunningListTemplate$datapath)
    } else {return(NULL)}
  })
  
  # search papers in template
  template_searched <- reactive({
    if(!is.null(uploaded_template())){

      showModal(modalDialog(glue("Looking up {nrow(uploaded_template())} paper(s)..."), footer=NULL))
      tic <- Sys.time()
      
      result <- fill.template(uploaded_template())
      toc <- Sys.time() - tic
      
      showModal(modalDialog(
        title = strong(glue("Lookup Complete - {round(toc[[1]], 2)} {units(toc)}
                             - {nrow(filter(result, is.na(ID)))} Failed to Find")),
        HTML(glue('Could not find {nrow(filter(result, is.na(ID)))} of the {nrow(result)} papers.
                   <br> Try manually searching for them on the Microsoft Academic
                   {a("search engine", href="https://academic.microsoft.com/home")}.<br>
                   <br> If there are missing IDs, please fill them in for as many papers as you can find.
                   <br> Then, remove any papers with missing IDs from your running list.')),
        footer = NULL, easyClose = TRUE))
      
      result
    }
  })
  


  
  ### Running List ###
  
  # intermediate df
  running_list <- reactive({
    uploaded_list() %||% template_searched()
  })
  # boolean - is the df uploaded as a template?
  is_template <- reactive({
    !is.null(running_list()) && identical(running_list(), template_searched())
  })
  
  
  
  # Download running list
  output$RunningListDownload <- downloadHandler(
    filename = function() {paste0("Running_List", format(Sys.time(), "_%Y_%m_%d_%H_%M"), ".csv")},
    content = function(file) {write_csv(running_list() %>% 
                                          mutate(Date = format(Sys.time(), "%a %b %d %X %Y"),
                                                 Searched_from = "Start") %>% 
                                          relocate(Date, Searched_from) %>% 
                                          mutate(Abstract = NA),
                                        file)}
  )
  
  
  # display table
  output$UploadedTable <- renderDataTable({
    
    rowgroup <- if(!is_template()){
      list(dataSrc = match("Date", names(running_list())))
    } else {NULL}
    
    dt <- datatable(running_list() %||% tibble(` ` = numeric()),
              extensions = c('Responsive', 'RowGroup'),
              options = list(scrollX = TRUE,
                             rowGroup = rowgroup),
              selection = 'none')
    
    if (!is.null(running_list())) {
      dt <- dt %>%
        formatStyle('ID', target = 'row',
                    backgroundColor = styleEqual(NA, 'LightCoral'))
    }
    
    dt
      
  })
  
  
  previously_snowballed <- reactive({
    if(!is_template()){
      running_list()$Searched_from %>% 
        unique() %>% 
        map(~str_match_all(.x, "\\d+")) %>% 
        unlist()}
  })
  
  
  
  ### ValueBoxes ###
  
  output$LastSearchValue <- renderValueBox({
    valueBox(
      if(!is_template()){
        last_date <- running_list()[nrow(running_list()),]$Date
        if (length(last_date) == 0) { NA
        } else {str_extract(last_date, "^\\w{3} \\w{3} \\d+")}
      } else {NA},
      color = "red",
      subtitle = "Last Search")
  })
  
  output$SearchNumberValue <- renderValueBox({
    valueBox(
      if(!is_template()){
        unique(running_list()$Searched_from) %>% 
          str_detect("\\d") %>% sum
      } else {0},
      color = "aqua",
      subtitle = "Total Searches")
  })
  
  output$PapersSnowballedValue <- renderValueBox({
    valueBox(length(previously_snowballed()),
             color = "purple",
             subtitle = "Total Papers Snowballed")
  })
  
  output$PreviouslySearchedValue <- renderValueBox({
    valueBox(length(running_list()$ID) %||% 0,
             color = "yellow",
             subtitle = "Total Papers Found")
  })
  
  
  
  
  
  
  #####################
  ##                 ##
  ##   Prepare Tab   ##
  ##                 ##
  #####################
  
  
  ### Lookup Box ###

  
  # Look up paper
  single_search_result <- eventReactive(input$LookupButton, {
    search_input <- input$LookupInput
      
    result <- if(str_length(search_input) > 0){
        # MAG ID
      if(str_detect(search_input, "^\\d+$")) {
        search_tryCatch("Microsoft Academic ID", scrape.tidy(as.numeric(search_input)))
        # DOI
      } else if(str_detect(search_input, "10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+")) {
        search_tryCatch("DOI", doi.search.tidy(search_input))
        # Title
      } else if(str_detect(search_input, "academic.microsoft.com")){
        search_tryCatch("Microsoft Academic page URL",
                        scrape.tidy(as.numeric(str_extract(search_input, "(?<=paper.)\\d+(?=.reference)"))))
      } else {
        search_tryCatch("Title", title.search.tidy(search_input))
      }
    }
    
    if(sum(result$Citations, result$References, na.rm = T) > 1000){
      showModal(modalDialog(
        title = strong("WARNING: High Density Paper"),
        HTML("Over 1000 connections (citations + references) were found for this paper.
              <br>Including this paper in the search will make the search take a while.
              <br>Make sure that you really do intend to snowball this paper!"),
        footer = NULL, easyClose = TRUE))
    }
    
    result
    
  }, ignoreNULL = FALSE)
  
  # Display paper
  output$LookupTable <- renderDataTable({
    datatable(single_search_result() %||% col_format,
              rownames = FALSE,
              options = list(dom = 't'))
  })
  
  # Click row for more info
  observeEvent(input$LookupTable_rows_selected, {
    browseURL(paste0("https://academic.microsoft.com/paper/",
                     single_search_result()[input$LookupTable_rows_selected, "ID"]))
  })
  

  
  ### Staging Area ###
  
  # initialize empty df
  data <- reactiveValues(staged = col_format)
  
  # Push handler
  observeEvent(input$LookupPush, {
    lookup <- single_search_result()$ID
    if(lookup %in% data$staged$ID){
      showModal(modalDialog(
        title = strong("ACTION BLOCKED: Paper Already Staged"),
        "Duplicates are not allowed.",
        footer = NULL, easyClose = TRUE))
    } else if(lookup %in% previously_snowballed()) {
      showModal(modalDialog(
        title = strong("ACION BLOCKED: Paper Already Snowballed"),
        HTML("You have already snowballed this paper.<br>
             Check the running list of IDs you uploaded in the <b>Setup</b> tab."),
        footer = NULL, easyClose = TRUE))
    } else {
      data$staged <- bind_rows(data$staged, single_search_result())
    }
  })
  
  # Display staged papers
  output$StagedTable <- renderDataTable({
    datatable(data$staged)
  })
  
  # Click row to delete
  observeEvent(input$StagedTable_rows_selected, {
    data$staged <- data$staged[-input$StagedTable_rows_selected,]
  })
  
  # Download staged papers
  output$DownloadStaged <- downloadHandler(
    filename = function() {paste0("Snowball_Staged", format(Sys.time(), "_%Y_%m_%d_%H_%M"), ".csv")},
    content = function(file) {write_csv(data$staged, file)}
  )
  
  
  
  
  
  
  ### Search Connections (offline database ID search) ###
  
  all_connections <- reactive({
    snowball_connections(data$staged$ID)
  })
  
  unique_found <- reactive({
    unique(all_connections()$to)
  })
  
  previous <- reactive({
    c(running_list()$ID, previously_snowballed())
  })
  
  new <- reactive({
    unique_found()[!unique_found() %in% previous()]
  })
  
  
  
  
  
  ### ValueBoxes ###
  
  output$StagedValue <- renderValueBox({
    valueBox(nrow(data$staged),
             color = "aqua",
             subtitle = "Papers Staged")
  })
  
  output$UniqueValue <- renderValueBox({
    valueBox(length(unique_found()) - length(new()),
             color = "purple",
             subtitle = "Previously-Found Papers Detected")
  })
  
  output$NewValue <- renderValueBox({
    valueBox(length(new()),
             color = "yellow",
             subtitle = "New Papers Detected")
  })
  
  
  
  
  
  
  
  
  #################
  ##             ##
  ##   Run Tab   ##
  ##             ##
  #################
  
  
  ### Search Options ###
  
  comprehensive_output <- eventReactive(input$ComprehensiveSearch, {

    showModal(modalDialog(glue("[Comprehensive Search] Fetching {length(new())} paper(s)..."), footer=NULL))
    tic <- Sys.time()

    result <- scrape.tidy(new())
    toc <- Sys.time() - tic
    
    if(input$GetAbstracts){
      result <- result %>% 
        mutate(Abstract = map_chr(ID, ~scrape.abst.ID(.x)$Abstract))
    }

    showModal(modalDialog(
      title = strong(glue("Comprehensive Search Complete - {round(toc[[1]], 2)} {units(toc)}")),
      HTML(glue('Input: {nrow(data$staged)} papers. <br>
                 Output: {nrow(result)} papers. <br>
                 Abstracts Failed to Find: {sum(is.na(result$Abstract))}')),
      footer = NULL, easyClose = TRUE))

    result

  })

  
  
  ### Search Summary ###
  
  search_summary_txt <- eventReactive(input$LookupPush, {
    
    glue("There are {nrow(data$staged)} papers staged as inputs for this snowball search.
         From these {nrow(data$staged)} inputs, {length(unique_found())} papers were detected,
         of which {(length(unique_found()) - length(new())) %||% 0} were found to be duplicates
         after comparing with the running list of {nrow(running_list()) %||% 0} papers
         that was uploaded in the Upload Runnign List tab.
         This search will return information for the {length(new())} new papers.")
  })
  
  output$search_summary_msg <- renderText({
    search_summary_txt()
  })
  
  
  
  
  
  ### Display Output Table ###
  
  final_output <- reactive({
    comprehensive_output()
  })
  
  output$OutputTable <- renderDataTable({
    datatable(final_output(),
              extensions = 'Responsive')
  })
  
  # Click row for more info
  observeEvent(input$OutputTable_rows_selected, {
    browseURL(paste0("https://academic.microsoft.com/paper/",
                     single_search_result()[input$LookupTable_rows_selected, "ID"]))
  })
  
  
  
  # TODO 8: way to fill abstracts easily (separate ID+abstracts df in a diff tab that reactively gets left_joined?)
  
  
  
  ### Download Output ###
  
  output$DownloadOutput <- downloadHandler(
    filename = function() {paste0("Search_Output", format(Sys.time(), "_%Y_%m_%d_%H_%M"), ".csv")},
    content = function(file) {write_csv(final_output(), file)}
  )
  
  
  
  
  ###########################
  ##                       ##
  ##   Search Statistice   ##
  ##                       ##
  ###########################
  
  
  ## Word Cloud ##

  tokens <- reactive({
    showModal(modalDialog(glue("Preparing..."), footer=NULL))
    df <- final_output() %>% 
      pull(Title) %>% 
      cnlp_annotate() %>% 
      pluck(1) %>% 
      rename(Word = lemma) %>%
      count(Word) %>% 
      filter(str_detect(Word, "^[:alpha:][[:alpha:][:punct:]]*")) %>% 
      filter(!Word %in% stop_words$word) %>% 
      mutate(Word = ifelse(str_detect(Word, "^[A-Z]+$"), Word, tolower(Word))) %>% 
      select(-n) %>% 
      count(Word) %>% 
      rename(Count = n) %>% 
      arrange(desc(Count))
    showModal(modalDialog("Complete!", footer=NULL, easyClose = TRUE))
    
    df
  })
    

  output$WordTable <- renderDataTable({
    datatable(tokens())
  })
  
  output$WordCloud <- renderWordcloud2({
    tokens() %>% 
      rename(word = Word, freq = Count) %>% 
      wordcloud2(size = input$WordCloudSize)
  })
  
  
  ## Network Visualization ##
  
  nodes <- reactive({
    bind_rows(
      tibble(id = new(),
             group = "Newly Found"),
      tibble(id = unique_found()[unique_found() %in% previous()],
             group = "Previously Found"),
      tibble(id = data$staged$ID,
             group = "Snowball Inputs")
    ) %>% 
      mutate(
        color.border = "black",
        info = map(id, fast.scrape),
        title = map_chr(info,
                        ~paste("<p><b>ID:</b>", .x$ID,
                               "<br><b>Title:</b>", .x$Title,
                               "<br><b>Year:</b>", .x$Year,
                               "<br><b>DOI:</b>",
                               if(!is.na(.x$DOI)){
                                 a(.x$DOI, href=glue("https://doi.org/{.x$DOI}"))
                               }else{NA},
                               "<br><b>Publication Type</b>:", .x$Pub_type,
                               if(.x$ID %in% running_list()$ID){
                                 paste("<br><b>Date Found:</b>",
                                       filter(running_list(), ID == .x$ID)$Date %>% 
                                         str_extract("^\\w{3} \\w{3} \\d+"))
                               }else{NULL},
                               "</p>")
        )
      ) %>% 
      select(-info)
  })
  
  edges <- reactive({
    all_connections() %>% 
      add_count(from) %>% 
      mutate(dashes = direction == "forward",
             length = n^(4/7) * 25) %>% 
      select(-n)
  })
  
  visnet <- reactive({
    
    showModal(modalDialog(glue("Preparing..."), footer=NULL))
    
    graph <- visNetwork(nodes(), edges()) %>%
      visLayout(randomSeed = 97) %>% 
      visPhysics(maxVelocity = 10, timestep = 1, enabled = FALSE) %>% 
      visGroups(groupname = "Snowballed", color = "#7a89ce") %>%
      visGroups(groupname = "Newly Found", color = "#f29886") %>% 
      visGroups(groupname = "Previously Found", color = "#ece6cc") %>% 
      visNodes(color = list(border = "black")) %>% 
      visEdges(color = list(color = "grey60")) %>% 
      visLegend(addEdges = tibble(color = "skyblue",
                                  label = c("Forward", "Backward"),
                                  dashes = c(TRUE, FALSE))) %>% 
      visInteraction(dragNodes = FALSE, keyboard = TRUE) %>% 
      visOptions(highlightNearest = list(enabled = TRUE), width = "100%", height = "150%")
    if (nrow(nodes()) > 1000 | max(count(edges(), from)$n) > 500) {
      showModal(modalDialog("WARNING: Network is too large (nodes > 1000) and/or too dense (degrees > 500)",
                            footer = NULL, easyClose = TRUE))
    }
    else {
      graph <- graph %>%
        visPhysics(enabled = TRUE, stabilization = FALSE) %>% 
        visInteraction(dragNodes = TRUE) %>% 
        visEdges(arrows = "to")}
    
    showModal(modalDialog("Complete!", footer=NULL, easyClose = TRUE))
    
    graph
  })
  
  output$visualnetwork <- renderVisNetwork({visnet()})
  
  

  
  
  ######################
  ##                  ##
  ##   Settings Tab   ##
  ##                  ##
  ######################
  
  
  ### API Keys ###
  
  # Set at first lookup
  observeEvent(input$LookupButton, {
    if (!input$MA_key == "") {Sys.setenv(MICROSOFT_ACADEMIC_KEY = input$MA_key)}
    if (!input$EL_key == "") {Sys.setenv(ELSEVIER_SCOPUS_KEY = input$EL_key)}
    opts <<- list(key = Sys.getenv("ELSEVIER_SCOPUS_KEY"))
  })
  
  
  
}







shinyApp(ui, server)