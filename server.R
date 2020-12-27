server <- function(input, output) {
  
  
  #####################
  ##                 ##
  ##   Running List  ##
  ##                 ##
  ##################### 
  
  
  
  
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
        downloadButton("RunningListDownload", "Download Formatted Running List"),
        footer = NULL, easyClose = TRUE))
      
      result
      
    }
  })
  
  
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
  
  #from run tab output
  output$UpdatedRunningListDownload <- downloadHandler(
    filename = function() {paste0("Running_List", format(Sys.time(), "_%Y_%m_%d_%H_%M"), ".csv")},
    if(!is.null(running_list())){
      content = function(file) {
        df <- full_join(running_list(), final_output())
        write_csv(df, file)
      }
    } else {
      content = function(file) {
        df <- final_output()
        write_csv(df, file)
      }
    }
  )
  
  
  
  # display table - from v1 with "Running List Tab"
  # output$UploadedTable <- renderDataTable({
  #   
  #   rowgroup <- if(!is_template()){
  #     list(dataSrc = match("Date", names(running_list())))
  #   } else {NULL}
  #   
  #   dt <- datatable(running_list() %||% tibble(` ` = numeric()),
  #                   extensions = c('Responsive', 'RowGroup'),
  #                   options = list(scrollX = TRUE,
  #                                  rowGroup = rowgroup),
  #                   selection = 'none')
  #   
  #   if (!is.null(running_list())) {
  #     dt <- dt %>%
  #       formatStyle('ID', target = 'row',
  #                   backgroundColor = styleEqual(NA, 'LightCoral'))
  #   }
  #   
  #   dt
  #   
  # })
  
  
  previously_snowballed <- reactive({
    if(!is_template()){
      running_list()$Searched_from %>% 
        unique() %>% 
        map(~str_match_all(.x, "\\d+")) %>% 
        unlist()}
  })
  
  
  
  ### ValueBoxes ###
  
  output$ScreenedValue <- renderValueBox({
    valueBox(length(running_list()),
             color = "red",
             subtitle = "Last Search")
  })
  
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
  
  #####
  #temporarily deprecated
  # Look up paper
  # lookup_result <- eventReactive(input$LookupButton, {
  #   
  #   # search error message
  #   search_tryCatch <- function(type, .f){
  #     tryCatch(.f, error = function(e){
  #       showModal(modalDialog(title = strong("ERROR: Search Failed"),
  #                             glue("No paper with this {type} found."),
  #                             footer = NULL, easyClose = TRUE))
  #     })
  #   }
  #   
  #   search_input <- input$LookupInput
  #   
  #   result <- if(str_length(search_input) > 0){
  #     # MAG ID
  #     if(str_detect(search_input, "^\\d+((,)\\s+\\d+)*$")) {
  #       IDs <- search_input %>% 
  #         str_remove_all(",") %>% 
  #         str_split("\\s+") %>% 
  #         pluck(1) %>% 
  #         as.numeric()
  #       search_tryCatch("Microsoft Academic ID", scrape.tidy(IDs))
  #       # DOI
  #     } else if(str_detect(search_input, "10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+")) {
  #       search_tryCatch("DOI", doi.search.tidy(toupper(search_input)))
  #       # URL
  #     } else if(str_detect(search_input, "academic.microsoft.com")){
  #       search_tryCatch("Microsoft Academic page URL",
  #                       scrape.tidy(as.numeric(str_extract(search_input, "(?<=paper.)\\d+(?=.reference)"))))
  #       # Title
  #     } else {
  #       search_tryCatch("Title", title.search.tidy(search_input))
  #     }
  #   }
  #   
  #   if(sum(result$Citations, result$References, na.rm = T) > 1000){
  #     showModal(modalDialog(
  #       title = strong("WARNING: High Density Paper"),
  #       HTML("Over 1000 connections (citations + references) were found for this paper.
  #            <br>Including this paper in the search will make the search take a while.
  #            <br>Make sure that you really do intend to snowball this paper!"),
  #       footer = NULL, easyClose = TRUE))
  #     
  #   }
  #   if(result$Citations == 0 | result$References == 0){
  #     showModal(modalDialog(
  #       title = strong("WARNING: 0 Density Paper"),
  #       HTML("There are no connections (citations + references) found for this paper.
  #            <br> This is likely due to it being a recent paper and not having been udpated in the Microsoft Academic database.
  #            <br>You should search this paper's references by hand."),
  #       footer = NULL, easyClose = TRUE))
  #   }
  #   
  #   
  #   result
  #   
  # }, ignoreNULL = FALSE)
  #####
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
        title = strong("ACTION BLOCKED: Paper Previously Snowballed"),
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
      title = h3(strong("Upload List To Be Searched"), align = 'center'),
      HTML("
           Download the template below, fill it out as much as you can, then upload it back here.<br>"),
      br(),
      downloadButton("StagedTemplateDownload", label = "Download Template"),
      fileInput(inputId = "FileToStage", ""),
      textOutput("dummy"),
      footer = NULL, easyClose = TRUE
      ))
    
    
    
  })
  
  observeEvent(input$RunningListOptions, {
    showModal(modalDialog(
      title = h3(strong("Upload Already-Searched Titles"), align = 'center'),
      HTML("First search? Download the template below, fill it out as much as you can, then upload it to be formatted<br>"),
      downloadButton("StagedTemplateDownload", label = "Download Template"),
      fileInput(inputId = "RunningListTemplate", "From Database Search (to be Formatted)"),
      HTML("Already have a running list from snowballer? Upload it here.<br>"),
      fileInput(inputId = "RunningList", "Snowballer-Formatted Running List"),
      textOutput("dummy"),
      footer = NULL, easyClose = TRUE
      ))
  })
  
  # download template
  output$StagedTemplateDownload <- downloadHandler(
    filename = function() {"Template.csv"},
    content = function(file) {
      write_csv(tibble(
        Title = character(),
        DOI = character(),
        PMID = numeric(),
        PMCID = numeric()
      ), file)
    }
  )
  
  # upload file to stage
  staging_file <- reactive({
    if(!is.null(input$FileToStage) && file.exists(input$FileToStage$datapath)) {
      read_csv(input$FileToStage$datapath)
    }
  })
  

  # searching modal
  staged_file_searched <- reactive({
    if(!is.null(staging_file())){
      
      show_modal_progress_line(text = glue("Looking up and staging {nrow(staging_file())} paper(s) from template..."))
      
      time_mark <- Sys.time()
      
      n_staged <- nrow(staging_file())
      
      result <- imap_dfr(
        1:n_staged,
        ~ {
          
          update_modal_progress(
            value = .y / n_staged,
            text = glue("Looking up and staging {n_staged} paper(s) from template...
                      {.y}/{n_staged} ({round(.y / n_staged, 2)*100}%)")
          )
          
          fill.template.row(staging_file()[.x,])
          
        }
      )
      
      update_modal_progress(value = 1, text = "Formatting...")
      
      missing_rows <- which(is.na(result$Id))
      staged_dups <- sum(table(result$Id) > 1) # TODO notify duplicates in template
      
      result <- result %>% 
        filter(!is.na(Id)) %>% 
        distinct(Id, .keep_all = TRUE)
      
      result <- possibly_null(format.tidy, filter(result, !is.na(Id))) %||% bind_cols(staging_file(), ID = NA)
      
      attr(result, "missing_rows") <- missing_rows
      attr(result, "staged_dups") <- staged_dups
      
      remove_modal_progress()
      
      time_mark <- Sys.time() - time_mark
      
      showModal(modalDialog(
        title = strong(glue("Staging Complete - {round(time_mark[[1]], 2)} {units(time_mark)} - ",
                            if (length(missing_rows) > 0) {"Failed on {length(missing_rows)}/{n_staged} Papers."}
                            else {"All {n_staged} staged papers found!"})),
                  HTML(glue('<b>After you finish reviewing missing papers and warnings, PRESS THE "CONFIRM" BUTTON BELOW to
                  push the uploaded papers to the staging area.</b><br><br>')),
        h4(strong("High Density Papers"), align = "center"),
        HTML("Over 1000 connections (citations + references) were found for these papers.
             <br>Including this paper in the search will make the search take a while.
             <br>Make sure that you really do intend to snowball this paper! <br>"),
        dataTableOutput("HighDensity"),
        h4(strong("Papers Without Connections"), align = "center"),
        HTML("There are no connections (citations + references) found for these papers.
             <br> This is likely due to it being a recent paper and not having been udpated in the Microsoft Academic database.
             <br>You should search these papers' references by hand. <br>"),
        dataTableOutput("LowDensity"),
        h4(strong("Missing Papers"), align = "center"),
        HTML(glue('{sum(is.na(result$ID))} of the {nrow(result)} papers could not be found in the database and cannot be be staged.
                  <br>Try manually searching for the missing papers on {a("Microsoft Academic", href="https://academic.microsoft.com/home")}.
                  <br>If you find the page for the paper, add it to your list of titles verbatim.<br><br>')),
        dataTableOutput("StagedMissing"),
        div(actionButton("PushBulk", "Confirm"), style = "float:right"),
        footer = NULL, easyClose = TRUE)
      )
      
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
  
  output$LowDensity <- renderDataTable({
    df <- staged_file_searched() %>%
      filter(Citations %in% c(NA, 0) & References %in% c(NA, 0)) %>%
      filter(!is.na(Year)) %>%
      select(Title, Citations, References)
    datatable(df, options = list(dom = 't'), rownames = FALSE)
  })
  
  output$HighDensity <- renderDataTable({
    df <- staged_file_searched() 
    df$total_links <- rowSums(df[,c("Citations", "References")], na.rm = T)
    df$high_d <- ifelse(df$total_links >= 1000, TRUE, FALSE)
      df <- df %>%
      filter(!is.na(Year)) %>%
      filter(high_d == TRUE) %>%
      select(Title, Citations, References)
  datatable(df, options = list(dom = 't'), rownames = FALSE)
  })
  
  
  observeEvent(input$PushBulk, {
    
    dups <- staged_file_searched()$ID[staged_file_searched()$ID %in% c(data$staged$ID, previously_snowballed())]
    
    dup_removed <- staged_file_searched() %>%
      filter(!row_number() %in% attr(staged_file_searched(), "missing_rows"),
             !ID %in% dups)
    
    # TODO - diff options for diff size connections (block action when 10,000+)
    data$staged <- bind_rows(data$staged, dup_removed)
    
    showModal(modalDialog(
      title = "Complete!",
      HTML(glue("{nrow(dup_removed)} papers will be staged after removing
                {length(dups) + attr(staged_file_searched(), 'staged_dups')} duplicates.<br>
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
  output$ScreenedValue <- renderValueBox({
    valueBox(nrow(running_list()),
             color = "red",
             subtitle = "Papers Already Screened")
  })
  
  output$StagedValue <- renderValueBox({
    valueBox(nrow(data$staged),
             color = "aqua",
             subtitle = "Papers Ready to Search")
  })
  
  output$UniqueValue <- renderValueBox({
    tryCatch({
      valueBox(length(unique_found()) - length(new()),
        color = "purple",
        subtitle = "Previously-Found Papers")
    }, error = function(cond){
        valueBox(0,
          color = "purple",
          subtitle = "Previously-Found Papers")
    })
  })
  
  output$NewValue <- renderValueBox({
    tryCatch({
      valueBox(length(new()),
        color = "yellow",
        subtitle = "New Papers Detected")
    }, error=function(cond){
      valueBox(0,
               color = "yellow",
               subtitle = "New Papers Detected")
    }) 
  })
  
  
  
  #################
  ##             ##
  ##   Run Tab   ##
  ##             ##
  #################
  
  
  ### Search ###
  
  fast_output <- eventReactive(input$ComprehensiveSearch, {fast.scrape()})
  
  comprehensive_output <- eventReactive(input$ComprehensiveSearch, {
    
    time_mark <- Sys.time()
    
    # paper info
    
    show_modal_progress_line(text = glue("Looking up information about {length(new())} discovered paper(s)..."))
    
    result <- imap_dfr(
      new(),
      ~{
        update_modal_progress(
          value = .y / length(new()),
          text = glue("Looking up information about {length(new())} discovered paper(s)...  
                       {.y}/{length(new())} ({round(.y / length(new()), 2)*100}%)")
        )
        scrape(.x)
      })
    
    update_modal_progress(value = 1, text = "Formatting...")
    
    result <- format.tidy(result)
    
    remove_modal_progress()
    
    
    # fetch abstract
    
    if(input$GetAbstracts){
      
      show_modal_progress_line(text = glue("Fetching abstracts of {length(new())} discovered paper(s)..."),
                               color = "#D7AA2D")
      
      # MAG search
      result <- result %>% 
        mutate(
          Abstract = imap_chr(
            1:nrow(result),
            ~{
              update_modal_progress(
                value = .y / length(new()),
                text = glue("Fetching abstracts of {length(new())} discovered paper(s)...  
                             {.y}/{length(new())} ({round(.y / length(new()), 2)*100}%)")
              )
              abst <- scrape.abst.ID(result$ID[.x])
              if (is.na(abst) && !is.na(result$DOI[.x])) {
                DOI <- result$DOI[.x]
                abst <- scrape.abst.DOI(DOI, "semanticscholar") %||%
                  scrape.abst.DOI(DOI, "plos") %||%
                  scrape.abst.DOI(DOI, "crossref") %||%
                  scrape.abst.DOI(DOI, "scopus") %||%
                  NA
              }
              abst
            })
        )
      
      remove_modal_progress()
      
      
      missing <- which(!is.na(result$DOI) & (is.na(result$Abstract) | str_detect(result$Abstract, "[.]{3}$")))
      
    }
    
    
    time_mark <- Sys.time() - time_mark
    
    showModal(modalDialog(
      title = strong(glue("Search Complete - {round(time_mark[[1]], 2)} {units(time_mark)}")),
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
      selection = list(mode = "single", target = 'cell'),
      extensions = c('Responsive', 'FixedHeader'),
      options = list(buttons = c('copy', 'csv', 'excel', 'print'),
                     fixedHeader = TRUE)
    )
  })
  
  # Click ID or DOI cell to navigate
  observeEvent(input$OutputTable_cells_selected, {

    if (nrow(input$OutputTable_cells_selected) == 1 && !is.na(input$OutputTable_cells_selected[1,2])) {
      
      if (input$OutputTable_cells_selected[1,2] == 1) {
        showModal(modalDialog(
          h4(strong("Open this paper's page on the Microsoft Academic database in a new window?")),
          footer = actionButton("NavigateMAG", "Proceed"), easyClose = TRUE
        ))
      }
      
      if (input$OutputTable_cells_selected[1,2] == 7) {
        showModal(modalDialog(
          h4(strong("Open this paper's DOI link in a new window?")),
          footer = actionButton("NavigateDOI", "Proceed"), easyClose = TRUE
        ))
      }
      
    }
  })
  
  # Navigate to MAG
  observeEvent(input$NavigateMAG, {
    browseURL(paste0(
      "https://academic.microsoft.com/paper/",
      final_output()$ID[input$OutputTable_cells_selected[1,1]]
    ))
    removeModal()
  })
  
  # Navigate to DOI
  observeEvent(input$NavigateDOI, {
    
    if (!is.na(final_output()$DOI[input$OutputTable_cells_selected[1,1]])) {
      browseURL(paste0(
        "https://doi.org/",
        final_output()$DOI[input$OutputTable_cells_selected[1,1]]
      ))
      removeModal()
    } else {
      showModal(modalDialog(h4(strong("No DOI was found for this paper.")), easyClose = TRUE))
    }
    
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

  # years plot
  
  years_plot <- reactive({
    comprehensive_output() %>% 
      select(ID, Year) %>% 
      inner_join(
        all_connections(),
        by = c("ID" = "to")
      ) %>% 
      mutate(direction = factor(direction, levels = c("forward", "backward"))) %>%
      ggplot(aes(Year, fill = direction)) +
      geom_histogram(bins = 15, color = "white") +
      scale_fill_manual(values = c("grey70", "grey30")) +
      labs(y = NULL, x = NULL) +
      theme_classic() +
      theme(
        panel.ontop = TRUE,
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'white')
      )
  })
  
  output$YearsPlot <- renderPlot(years_plot(), res = 150)
  
  
  # authors plot
  
  authors_plot <- reactive({
    comprehensive_output() %>% 
      select(ID, Authors) %>% 
      mutate(Authors = str_split(Authors, ", ")) %>% 
      unnest(Authors) %>% 
      inner_join(
        all_connections(),
        by = c("ID" = "to")
      ) %>% 
      mutate(
        direction = factor(direction, levels = c("forward", "backward")),
        Authors = fct_lump(Authors, n = 15, ties.method = "first")
      ) %>% 
      filter(Authors != "Other") %>%
      mutate(Authors = fct_rev(fct_infreq(Authors))) %>% 
      ggplot(aes(Authors, fill = direction)) +
      geom_bar(color = "white") +
      coord_flip() +
      scale_fill_manual(values = c("grey70", "grey30")) +
      scale_y_continuous(
        expand = expansion(c(0.02, 0.05)),
        breaks = function(x) unique(as.integer(pretty(x)))
      ) +
      labs(y = NULL, x = NULL) +
      theme_classic() +
      theme(
        panel.ontop = TRUE,
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = 'white')
      )
  })
  
  output$AuthorsPlot <- renderPlot(authors_plot(), res = 150)
  
  
  # journals plot
  
  journals_plot <- reactive({
    comprehensive_output() %>% 
      filter(!is.na(Journal)) %>% 
      select(ID, Journal) %>% 
      inner_join(
        all_connections(),
        by = c("ID" = "to")
      ) %>% 
      mutate(
        direction = factor(direction, levels = c("forward", "backward")),
        Journal = fct_lump(Journal, n = 10, ties.method = "first")
      ) %>% 
      filter(Journal != "Other") %>%
      mutate(
        Journal = text_split(Journal),
        Journal = fct_rev(fct_infreq(Journal))
      ) %>% 
      ggplot(aes(Journal, fill = direction)) +
      geom_bar(color = "white") +
      coord_flip() +
      scale_fill_manual(values = c("grey70", "grey30")) +
      scale_y_continuous(
        expand = expansion(c(0.02, 0.05)),
        breaks = function(x) unique(as.integer(pretty(x)))
      ) +
      labs(y = NULL, x = NULL) +
      theme_classic() +
      theme(
        axis.text.y = element_text(hjust = 0, margin = margin(r = .5, unit = "cm")),
        panel.ontop = TRUE,
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = 'white')
      )
  })
  
  output$JournalsPlot <- renderPlot(journals_plot(), res = 150)
  
  
  ## Word Cloud ##
  
  tokens <- reactive({
    showModal(modalDialog(glue("Preparing..."), footer=NULL))
    cnlp_init_udpipe()
    df <- final_output() %>% 
      pull(Title) %>% 
      cnlp_annotate() %>% 
      pluck(1) %>% 
      count(Word = lemma) %>%
      filter(
        str_detect(Word, "^[:alpha:][[:alpha:][:punct:]]*"),
        !Word %in% stop_words$word
      ) %>% 
      mutate(Word = tolower(Word)) %>% 
      select(-n) %>% 
      count(Word, name = "Count", sort = TRUE)
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
      tibble(
        id = new(),
        group = "Newly Found"
      ),
      tibble(
        id = unique_found()[unique_found() %in% previous()],
        group = "Previously Found"
      ),
      tibble(
        id = data$staged$ID,
        group = "Snowballed"
      )
    ) %>% 
      mutate(
        color.border = "black",
        info = map(id, fast.scrape)
      ) %>% 
      rowwise() %>% 
      mutate(
        title = paste0(
          "<p>",
          "<b>ID: </b>", a(info$PaperID, href=glue("https://academic.microsoft.com/paper/{info$PaperID}")),
          "<br><b>Title: </b>", info$OriginalTitle,
          "<br><b>Year: </b>", info$Year,
          "<br><b>DOI: </b>", if(!is.na(info$Doi)){a(info$Doi, href=glue("https://doi.org/{info$Doi}"))},
          "<br><b>Publication Type</b>: ", info$DocType,
          if(info$PaperID %in% running_list()$ID){
            paste(
              "<br><b>Date Found: </b>",
              filter(running_list(), ID == info$ID)$Date %>% 
                str_extract("^\\w{3} \\w{3} \\d+")
            )
          },
          "</p>"
        )
      ) %>% 
      ungroup() %>% 
      select(-info)
  })
  
  edges <- reactive({
    all_connections() %>% 
      add_count(from, name = "length") %>% 
      mutate(
        dashes = direction == "forward",
        length = length^(4/7) * 25
      )
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