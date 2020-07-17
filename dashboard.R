library(shiny)
library(shinydashboard)
source('app_source/snowballer_source.R')

cnlp_init_udpipe()

options(shiny.maxRequestSize=500*1024^2)


ui <- dashboardPage(skin = "black",
                    
  dashboardHeader(title = "Snowballer"),
   
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
      
      
      tabItem(tabName = "HomeTab",
              box(h1("This is the home tab"))),
      
      
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
                    
                    downloadButton("DownloadOutput",
                                   label = "Download Output (.csv)")

                    
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

      show_modal_progress_line(text = glue("Looking up {nrow(uploaded_template())} paper(s) uploaded from template..."))
      
      tic <- Sys.time()
      
      
      result <- col_format
      
      possibly_na <- function(f,x){
        possibly(f, otherwise = NA)(x)
      }
      
      df <- as_tibble(uploaded_template()) %>% 
        modify(~ifelse(.x == "", NA, .x))
      
      for (i in 1:nrow(df)){
        
        update_modal_progress(
          value = i / nrow(df),
          text = glue("Looking up {nrow(uploaded_template())} paper(s) uploaded from template...  
                      {i}/{nrow(df)} ({round(i / nrow(df), 2)*100}%)")
        )
        
        temp <- NA
        while (identical(temp, NA)){
          if (!is.na(df[i,]$DOI)){
            temp <- possibly_na(doi.search.tidy, df[i,]$DOI)
          }
          if (!is.na(df[i,]$Title)){
            temp <- possibly_na(title.search.tidy, df[i,]$Title)
          }
          if (!is.null(df$PMID) && !is.na(df[i,]$PMID)){
            PMID_info <- PMID.search(df[i,]$PMID)
            temp <- possibly_na(doi.search.tidy, PMID_info[1,]$DOI)
          }
          if (identical(temp, NA)){
            temp <- df[i,] %>% 
              select(Title, DOI)
          }
        }
        
        result <- bind_rows(result, temp)
      }
      
      remove_modal_progress()
      
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
    content = function(file) {
      df <- if(!is.null(running_list()$Date) & !is.null(running_list()$Searched_from)){running_list()}
            else{running_list() %>% 
                 mutate(Date = format(Sys.time(), "%a %b %d %X %Y"),
                        Searched_from = "Start") %>% 
                 relocate(Date, Searched_from) %>% 
                 mutate(Abstract = NA)}
      write_csv(df, file)
    }
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
  lookup_result <- eventReactive(input$LookupButton, {
    
    # search error message
    search_tryCatch <- function(type, .f){
      tryCatch(.f, error = function(e){
        showModal(modalDialog(title = strong("ERROR: Search Failed"),
                              glue("No paper with this {type} found."),
                              footer = NULL, easyClose = TRUE))
      })
    }
    
    search_input <- input$LookupInput
      
    result <- if(str_length(search_input) > 0){
        # MAG ID
      if(str_detect(search_input, "^\\d+((,)\\s+\\d+)*$")) {
        IDs <- search_input %>% 
          str_remove_all(",") %>% 
          str_split("\\s+") %>% 
          pluck(1) %>% 
          as.numeric()
        search_tryCatch("Microsoft Academic ID", scrape.tidy(IDs))
        # DOI
      } else if(str_detect(search_input, "10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+")) {
        search_tryCatch("DOI", doi.search.tidy(search_input))
        # URL
      } else if(str_detect(search_input, "academic.microsoft.com")){
        search_tryCatch("Microsoft Academic page URL",
                        scrape.tidy(as.numeric(str_extract(search_input, "(?<=paper.)\\d+(?=.reference)"))))
        # Title
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
    datatable(lookup_result() %||% col_format,
              rownames = FALSE,
              options = list(dom = 'rtip'))
  })
  
  # Click row for more info
  observeEvent(input$LookupTable_rows_selected, {
    browseURL(paste0("https://academic.microsoft.com/paper/",
                     lookup_result()[input$LookupTable_rows_selected,]$ID))
  })
  

  
  ### Staging Area ###
  
  
  # initialize empty df
  data <- reactiveValues(staged = col_format)
  
  
  # Push handler
  observeEvent(input$LookupPush, {
    lookup <- lookup_result()$ID
    if(any(lookup %in% data$staged$ID)){
      showModal(modalDialog(
        title = strong("ACTION BLOCKED: Paper Already Staged"),
        "Duplicates are not allowed.",
        footer = NULL, easyClose = TRUE))
    } else if(any(lookup %in% previously_snowballed())) {
      showModal(modalDialog(
        title = strong("ACION BLOCKED: Paper Previously Snowballed"),
        HTML("You have already snowballed this paper in your ongoing project.<br>
             Double check the running list of IDs you uploaded in the <b>Setup</b> tab."),
        footer = NULL, easyClose = TRUE))
    } else {
      data$staged <- bind_rows(data$staged, lookup_result())
    }
  })
  
  
  

  ##  Stage directly from file upload ##
  
  observeEvent(input$StageFromFile, {
    showModal(modalDialog(
      title = h3(strong("Stage Papers Directly"), align = 'center'),
      HTML("<h4><b>Do you want to upload papers to snowball search in bulk?</b></h4>
            Download the template below, fill it out as much as you can, then upload it back here.<br>"),
      br(),
      downloadButton("StagedTemplateDownload", label = "Download Template"),
      fileInput(inputId = "FileToStage", ""),
      textOutput("dummy"),
      footer = NULL, easyClose = TRUE
    ))
  })
  
  # download template
  output$StagedTemplateDownload <- downloadHandler(
    filename = function() {"Template.csv"},
    content = function(file) {
      write_csv(tibble(Title = character(), DOI = character(),
                       PMID = numeric()),
                file)
    }
  )

  # upload file to stage
  staging_file <- reactive({
    if(is.null(input$FileToStage)) return (NULL)
    if(file.exists(input$FileToStage$datapath)){
      read.csv(input$FileToStage$datapath)
    } else {return(NULL)}
  })
  
  # searching modal
  staged_file_searched <- reactive({
    if(!is.null(staging_file())){
      
      show_modal_progress_line(text = glue("Looking up and staging {nrow(staging_file())} paper(s) from template..."))
      
      tic <- Sys.time()
      
      result <- col_format
      for (i in 1:nrow(staging_file())){
        
        update_modal_progress(
          value = i / nrow(staging_file()),
          text = glue("Looking up and staging {nrow(staging_file())} paper(s) from template...
                      {i}/{nrow(staging_file())} ({round(i / nrow(staging_file()), 2)*100}%)")
        )
        
        temp <- fill.template(staging_file()[i,])
        
        result <- bind_rows(result, temp)
      }
      
      remove_modal_progress()
      
      toc <- Sys.time() - tic
      
      attr(result, "missing_rows") <- which(is.na(result$ID))
      
      showModal(modalDialog(
        title = strong(glue("Staging Complete - {round(toc[[1]], 2)} {units(toc)}
                             - Failed on {nrow(filter(result, is.na(ID)))} Papers")),
        HTML(glue('{nrow(filter(result, is.na(ID)))} of the {nrow(result)} papers could not be found in the database
                   and cannot be be staged. <br>
                   Try manually searching for the missing papers on the Microsoft Academic
                   {a("search engine", href="https://academic.microsoft.com/home")}.<br>
                   If you find the page for the paper, enter its url into the search box to manually add it.<br><br>
                   <b>After you finish reviewing any missing papers, PRESS THE BUTTON BELOW to
                   push the uploaded papers to the staging area.</b><br><br>')),
        div(actionButton("PushBulk", "Confirm"), style = "float:right"),
        br(),br(),
        h4(strong("Missing Papers"), align = "center"),
        dataTableOutput("StagedMissing"),
        footer = NULL, easyClose = TRUE))
      
      return(result)
      
    }
  })

  # This does nothing but the code breaks when I remove it
  output$dummy <- renderText({
    # DO NOT REMOVE - a weird piece in reactivity chain
    dontbreak <- staging_file()[attr(staged_file_searched(), "missing_rows"),]
    return(NULL)
  })
  
  output$StagedMissing <- renderDataTable({
    datatable(staging_file()[attr(staged_file_searched(), "missing_rows"),],
              options = list(dom = 't'), rownames = FALSE)
  })
  
  observeEvent(input$PushBulk, {
    
    dups <- staged_file_searched()$ID[staged_file_searched()$ID %in% c(data$staged$ID, uploaded_list()$ID)]
    
    dup_removed <- staged_file_searched()[-attr(staged_file_searched(), "missing_rows"),] %>% 
      filter(!ID %in% dups)
    
    # TODO - diff options for diff size connections (block action when 10,000+)
    data$staged <- bind_rows(data$staged, dup_removed)
    
    showModal(modalDialog(
      title = "Complete!",
      HTML(glue("{nrow(dup_removed)} papers will be staged after removing {length(dups)} duplicates.<br>
                Click on a row to individually remove a paper from the staging area.<br><br>
                Proceed to the <b>Run Search</b> tab after reviewing your staged papers.")),
      footer = NULL, easyClose = TRUE
    ))
    
  })
  
  
  
  ## Staged Table ##
  
  # Display staged papers
  output$StagedTable <- renderDataTable({
    datatable(data$staged, options = list(dom = 'rltip'))
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
  
  
  ### Search ###
  
  comprehensive_output <- eventReactive(input$ComprehensiveSearch, {
  
    tic <- Sys.time()
      
    # paper info
  
    show_modal_progress_line(text = glue("Looking up information about {length(new())} paper(s)..."))

    result <- imap_dfr(new(),
                     ~{
                       update_modal_progress(
                         value = .y / length(new()),
                         text = glue("Looking up information about {length(new())} paper(s)...  
                                     {.y}/{length(new())} ({round(.y / length(new()), 2)*100}%)")
                       )
                       scrape.tidy(.x)
                     })
    
    remove_modal_progress()
    
    
    # fetch abstract
    
    if(input$GetAbstracts){
      
      show_modal_progress_line(text = glue("Fetching abstracts of {length(new())} paper(s)..."),
                               color = "#796e5b")
      
      # MAG search
      result <- result %>% 
        mutate(Abstract = 
                 imap_chr(ID,
                          ~{
                            update_modal_progress(
                              value = .y / length(new()),
                              text = glue("Fetching abstracts of {length(new())} paper(s)...  
                                          {.y}/{length(new())} ({round(.y / length(new()), 2)*100}%)")
                            )
                            scrape.abst.ID(.x)$Abstract
                          }
                 ))
      
      remove_modal_progress()
      
      
      # Check other databses with DOI for abstracts that are missing or incomplete
      
      show_modal_progress_line(text = glue("Checking other sources for abstracts..."),
                               color = "#75795b")

      tryCatchnull <- function(f){tryCatch(f, error = function(e){NULL})}
      
      missing <- which(!is.na(result$DOI) & (is.na(result$Abstract) | str_detect(result$Abstract, "[.]{3}$")))
      
      counter <- 1
      for (i in missing){
        
        print(i)
        
        update_modal_progress(
          value = counter / length(missing),
          text = "Checking other sources for abstracts..."
        )
        counter <- counter + 1
        
        doi <- result[i,]$DOI
        
        abstract <-
          tryCatchnull(ft_abstract(doi, from = "semanticscholar")$semanticscholar[[1]]$abstract) %||%
          tryCatchnull(ft_abstract(doi, from = "plos")$plos[[1]]$abstract) %||%
          tryCatchnull(ft_abstract(doi, from = "crossref")$crossref[[1]]$abstract) %||%
          tryCatchnull(ft_abstract(doi, from = "scopus", scopusopts = scopusopts)$scopus[[1]]$abstract)
       
        result[i,]$Abstract <- abstract %||% NA
               
      }
      
    }
    
    
    toc <- Sys.time() - tic
    
    showModal(modalDialog(
      title = strong(glue("Search Complete - {round(toc[[1]], 2)} {units(toc)}")),
      HTML(glue('Input: {nrow(data$staged)} papers. <br>
                 Output: {nrow(result)} papers. <br>
                 Abstracts Failed to Fetch: {ifelse(input$GetAbstracts, sum(is.na(result$Abstract)), "Not Searched")} 
                  {if(input$GetAbstracts){
                    paste0(" (", round(nrow(filter(result, Pub_type == pub.key(1) & is.na(Abstract)))/
                                       nrow(filter(result, Pub_type == pub.key(1))), 2),
                          "% of Journal Articles)")
                  }}')),
      footer = NULL, easyClose = TRUE))

    return(result)

  })

  
  
  ### Search summary dialogue ###
  
  search_summary_txt <- eventReactive(input$LookupPush, {
    
    glue("There are {nrow(data$staged)} papers staged as inputs for this snowball search.
         From these {nrow(data$staged)} inputs, {length(unique_found())} papers were detected,
         of which {(length(unique_found()) - length(new())) %||% 0} were found to be duplicates
         after comparing with the running list of {nrow(running_list()) %||% 0} papers
         that was uploaded in the Upload Running List tab.
         This search will return information for the {length(new())} new papers.")
  })
  
  output$search_summary_msg <- renderText({
    search_summary_txt()
  })
  
  
  
  ### Package up output ###
  
  final_output <- reactive({
    # TODO incorporate quick search?
    comprehensive_output() %>% 
      mutate(Date = format(Sys.time(), "%a %b %d %X %Y"),
             Searched_from = paste(data$staged$ID, collapse = ", ")) %>% 
      relocate(Date, Searched_from) %>% 
      mutate(Density = map_int(ID, ~sum(all_connections()$to == .x)),
             Connections = map_chr(ID,
                                   ~paste(all_connections()$from[all_connections()$to == .x],
                                          collapse = ", "))) %>% 
      arrange(-Density)
  })
  
  
  ### Display Output Table ###
  
  output$OutputTable <- renderDataTable({
    datatable(
      final_output() %>%
        select(-Date, -Searched_from) %>% 
        relocate(Density, Connections, .after = References),
      selection = 'single',
      extensions = c('Responsive', 'FixedHeader'),
      options = list(buttons = c('copy', 'csv', 'excel', 'print'),
                     fixedHeader = TRUE)
    )
  })
  
  # Click row to navigate to MAG page
  observeEvent(input$OutputTable_rows_selected, {
    showModal(modalDialog(h4(strong("Navigate to this paper's page on the Microsoft Academic database?")),
                          footer = actionButton("Navigate", "Confirm"), easyClose = TRUE))
  })
  
  observeEvent(input$Navigate, {
    browseURL(paste0("https://academic.microsoft.com/paper/",
                     final_output()[input$OutputTable_rows_selected,]$ID))
    removeModal()
  })
  
  
  
  # TODO: add way to fill abstracts easily (separate ID+abstracts df in a diff tab that reactively gets left_joined?)
  
  
  
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
  
  
  ## Summary Plots ##
  
  #  TODO - 1/(2+3) layout
  # 1. year hist ogram/density
  # 2. top authors
  # 3. top journals
  
  
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
    removeModal()
    df
  })
    

  output$WordTable <- renderDataTable({
    datatable(tokens(), options = list(dom = 'rtip'))
  })
  
  output$WordCloud <- renderWordcloud2({
    tokens() %>% 
      rename(word = Word, freq = Count) %>% 
      wordcloud2(size = input$WordCloudSize,
                 backgroundColor = "#f3f4f3")
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
                        ~paste("<p><b>ID:</b>",
                               a(.x$ID, href=glue("https://academic.microsoft.com/paper/{.x$ID}")),
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
    
    showModal(modalDialog(
      glue("Preparing..."), footer=NULL)
    )
    
    graph <- visNetwork(nodes(), edges()) %>%
      visLayout(randomSeed = 97) %>% 
      visPhysics(maxVelocity = 10, timestep = 1, enabled = FALSE) %>% 
      visGroups(groupname = "Snowballed", color = "#7a89ce") %>%
      visGroups(groupname = "Newly Found", color = "#f29886") %>% 
      visGroups(groupname = "Previously Found", color = "#ece6cc") %>% 
      visNodes(color = list(border = "black")) %>% 
      visEdges(color = list(color = "grey60")) %>% 
      visLegend(addEdges = tibble(color = "grey60",
                                  label = c("Forward", "Backward"),
                                  dashes = c(TRUE, FALSE))) %>% 
      visInteraction(dragNodes = FALSE, keyboard = TRUE) %>% 
      visOptions(highlightNearest = list(enabled = TRUE),
                 width = "100%", height = "150%")
    
    if (nrow(nodes()) > 1000 | max(count(edges(), from)$n) > 500) {
      showModal(modalDialog("WARNING: Network is too large (nodes > 1000) and/or too dense (degrees > 500)",
                            footer = NULL, easyClose = TRUE))
    } else {
      graph <- graph %>%
        visPhysics(enabled = TRUE, stabilization = FALSE) %>% 
        visInteraction(dragNodes = TRUE) %>% 
        visEdges(arrows = "to")
      removeModal()
    }
    
    return(graph)
    
  })
  
  output$visualnetwork <- renderVisNetwork({visnet()})
  
  # TODO - download network data? html?

  
  
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