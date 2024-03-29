server <- function(input, output, session) {
  
  
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
    }
  })
  
  
  ### Running List Template ###
  
  # template download
  output$RunningListTemplateDownload <- downloadHandler(
    filename = function() {"Template.csv"},
    content = function(file) {
      write_csv(tibble(
        Title = character(),
        Year = integer()
      ), file)
    }
  )
  
  # read uploaded template
  uploaded_running_list_template <- reactive({
    if(is.null(input$RunningListTemplate)) return (NULL)
    if(file.exists(input$RunningListTemplate$datapath)){
      read_csv(input$RunningListTemplate$datapath, col_types = 'ci')
    }
  })
  
  filled_running_list_template <- reactiveVal(NULL)
  
  # processes uploaded template
  observeEvent(input$RunningListTemplate, {
    if (!is.null(uploaded_running_list_template())) {
      if (identical(colnames(uploaded_running_list_template()), c("Title", "Year"))) {
        uploaded_running_list <- uploaded_running_list_template()
        n_uploaded <- nrow(uploaded_running_list)
        
        if (n_uploaded == 0) {
          showModal(modalDialog(
            title = strong("ACTION BLOCKED: No papers in template"),
            tags$p("Please fill out the template before uploading."),
            footer = NULL, easyClose = TRUE
          ))
        } else {
          
          # Fill template
          show_modal_progress_line(text = glue("Looking up {nrow(staging_file())} paper(s) from template..."))
          time_mark <- Sys.time()
          result <- map(1L:n_uploaded, ~ {
            update_modal_progress(
              value = .x / n_uploaded,
              text = glue("Looking up and staging {n_uploaded} paper(s) from template...
                            {.x}/{n_uploaded} ({round(.x / n_uploaded, 2)*100}%)")
            )
            safely(fill.template.row, otherwise = uploaded_running_list[.x,])(uploaded_running_list[.x,])
          }
          )
          
          # Format and save filled template
          update_modal_progress(value = 1, text = "Formatting...")
          errors <- map(result, 2L)
          result <- bind_rows(map(result, 1L))
          missing_rows <- which(is.na(result$Id))
          result <- format.tidy(result)
          filled_running_list_template(result)
          remove_modal_progress()
          
          # Dialog
          showModal(modalDialog(
            title = strong(glue("Complete: {nrow(result)}/{n_uploaded} papers found")),
            if (length(missing_rows) == 0) {
              tags$p("Download the filled template and upload it back in as", tags$strong("Snowglobe-Formatted Running List"))
            } else {
              tags$div(
                tags$p("One or more papers from the uploaded running list could not be found in the database"),
                tags$p("The papers in the following rows could not be found:"),
                tags$p(
                  paste(missing_rows, collapse = ", "),
                  style = "margin:10px; padding:10px; background-color:ghostwhite; border: 1px solid steelblue; line-height:1.7em;"
                ),
                tags$div(
                  tags$p("Options for missing rows:"),
                  tags$li("Check for any typos/add more details and re-try"),
                  tags$li("Manually search MAG. and add in IDs to partially-filled template"),
                  tags$li("If cannot be found on MAG, separately search-by-hand"),
                  style = "margin:10px; border: 3px dashed salmon; padding:10px; background-color:lightpink;"
                ),
                if (nrow(result) > 0) { tags$p("Download the partially-filled template below:") }
              )
            },
            downloadButton("FilledRunningListDownload", label = "Download Filled Running-List Template"),
            footer = NULL, easyClose = TRUE
          ))
          
        }
        
      } else {
        showModal(modalDialog(
          title = strong("ACTION BLOCKED: Incorrect formatting of the template"),
          HTML("The uploaded file do not have follow the formatting specified in the template.<br>
                Please fill out the rows of the template file without modifying the column names."),
          footer = NULL, easyClose = TRUE
        ))
      }
    }
  })
  
  output$FilledRunningListDownload <- downloadHandler(
    filename = function() { paste0("Running_List", format(Sys.time(), "_%Y_%m_%d_%H_%M"), ".csv") },
    content = function(file) {
      filled_running_list_template() %>% 
        mutate(
          Date = format(Sys.time(), "%a %b %d %X %Y"),
          Searched_from = "Start",
          Abstract = NA
        ) %>% 
        relocate(Date, Searched_from) %>% 
        write_csv(file)
    }
  )
  
  
  
  # intermediate df
  running_list <- reactive({
    uploaded_list()
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
    running_list()$Searched_from %>% 
      unique() %>% 
      map(~str_match_all(.x, "\\d+")) %>% 
      unlist()
  })
  
  
  
  ### ValueBoxes ###
  
  output$ScreenedValue <- renderValueBox({
    valueBox(length(running_list()),
             color = "red",
             subtitle = "Last Search")
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
      HTML("Have titles from a database search, but haven't used SnowGlobe? Download the template below, fill it out as much as you can, then upload it to be formatted<br>"),
      downloadButton("StagedTemplateDownload", label = "Download Template"),
      fileInput(inputId = "RunningListTemplate", "From Database Search (to be Formatted)"),
      HTML("Already have a running list from Snowglobe? Upload it here.<br>"),
      fileInput(inputId = "RunningList", "Snowglobe-Formatted Running List"),
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
        Year = integer()
      ), file)
    }
  )
  
  # upload file to stage
  staging_file <- reactive({
    if(!is.null(input$FileToStage) && file.exists(input$FileToStage$datapath)) {
      read_csv(input$FileToStage$datapath, col_types = 'ci')
    }
  })
  
  
  # searching modal
  staged_file_searched <- reactive({
    if(!is.null(staging_file())){
      
      show_modal_progress_line(text = glue("Looking up and staging {nrow(staging_file())} paper(s) from template..."))
      
      time_mark <- Sys.time()
      
      n_staged <- nrow(staging_file())
      
      queries <- make.query.direct(staging_file()$Title, staging_file()$Year)
      
      result_list <- map(seq_len(n_staged), ~{
        
        update_modal_progress(
          value = .x / n_staged,
          text = glue("Looking up and staging {n_staged} paper(s) from template...
                      {.x}/{n_staged} ({round(.x / n_staged, 2)*100}%)")
        )
        
        out <- send.query.direct(queries[.x])
        
      })
      
      update_modal_progress(value = 1, text = "Formatting...")
      
      result <- format.query.direct(result_list, staging_file()$Title)
      result$References <- map_int(result$ID, ~ nrow(backward.search(.x)))
      result$Citations <- map_int(result$ID, ~ nrow(forward.search(.x)))
      
      missing_rows <- which(is.na(result$ID))
      staged_dups <- sum(table(result$ID) > 1)
      
      result <- result %>% 
        filter(!is.na(ID)) %>% 
        distinct(ID, .keep_all = TRUE)
      
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
        HTML("Over 1000 connections (citations + references) were found for these papers."),
        dataTableOutput("HighDensity"),
        h4(strong("Papers Without Connections"), align = "center"),
        HTML("There are no connections (citations + references) found for these papers.
             <br> This is likely due to it being a recent paper and not having been udpated in the Microsoft Academic database.
             <br>You should search these papers' references by hand. <br>"),
        dataTableOutput("LowDensity"),
        h4(strong("Missing Papers"), align = "center"),
        HTML(glue('{sum(is.na(result$ID))} of the {nrow(result)} papers could not be found in the database and cannot be be staged.')),
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
    
    # remove duplicates from staging area + previous snowballs
    dups <- staged_file_searched()$ID[staged_file_searched()$ID %in% c(data$staged$ID, previously_snowballed())]
    
    dup_removed <- staged_file_searched() %>%
      filter(!ID %in% dups)
    
    # TODO - diff options for diff size connections (block action when 10,000+)
    data$staged <- bind_rows(data$staged, dup_removed)
    
    showModal(modalDialog(
      title = "Complete!",
      HTML(glue("{nrow(dup_removed)} paper(s) will be staged after removing
                {length(dups) + attr(staged_file_searched(), 'staged_dups')} duplicates.<br>
                Click on a row to individually remove a paper from the staging area.<br><br>
                Proceed to the <strong>Run Search</strong> tab after reviewing your staged papers.")),
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
    valueBox(nrow(running_list()) %||% "Missing",
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
    
    n_new <- length(new())
    
    show_modal_progress_line(text = glue("Fetching {n_new} discovered paper(s)..."))
    
    result <- fast.scrape(new())
    
    update_modal_progress(value = 1, text = "Formatting...")
    
    colnames(result) <- c("ID", "Title", "Year", "Pub_type", "DOI")
    
    remove_modal_progress()
    
    # fetch abstract (NO LONGER SUPPORTED)
    # commented out because only removing the UI part caused an error in searching - also deleted some text that called GetAbstracts
    
    # if(FALSE && input$GetAbstracts){
    # 
    #   show_modal_progress_line(text = glue("Fetching abstracts of {length(new())} discovered paper(s)..."),
    #                            color = "#D7AA2D")
    # 
    #   # MAG search
    #   result <- result %>%
    #     mutate(
    #       Abstract = imap_chr(
    #         1:nrow(result),
    #         ~{
    #           update_modal_progress(
    #             value = .y / length(new()),
    #             text = glue("Fetching abstracts of {length(new())} discovered paper(s)...
    #                          {.y}/{length(new())} ({round(.y / length(new()), 2)*100}%)")
    #           )
    #           abst <- scrape.abst.ID(result$ID[.x])
    #           if (is.na(abst) && !is.na(result$DOI[.x])) {
    #             DOI <- result$DOI[.x]
    #             abst <- scrape.abst.DOI(DOI, "semanticscholar") %||%
    #               scrape.abst.DOI(DOI, "plos") %||%
    #               scrape.abst.DOI(DOI, "crossref") %||%
    #               scrape.abst.DOI(DOI, "scopus") %||%
    #               NA
    #           }
    #           abst
    #         })
    #     )
    # 
    #   remove_modal_progress()
    # 
    # 
    #   missing <- which(!is.na(result$DOI) & (is.na(result$Abstract) | str_detect(result$Abstract, "[.]{3}$")))
    # 
    # }


    time_mark <- Sys.time() - time_mark

    showModal(modalDialog(
      title = strong(glue("Search Complete - {round(time_mark[[1]], 2)} {units(time_mark)}")),
      HTML(glue('Input: {nrow(data$staged)} papers. <br>
                Output: {nrow(result)} papers. <br>')),
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
  
  #RIS Formatting
  RIS_output <- reactive({
    x <- comprehensive_output()
    x <- x %>% select(Pub_type, Title, Year, DOI)
    colnames(x) <- c("TY", "TI", "PY", "DO")
    x$TY <- ifelse(x$TY == "", "Unknown", x$TY) 
    x$PY <- as.character(x$PY)
    x$ER <- "zyxw"
    
    ris <- pivot_longer(x, TY:ER, names_to = "var")
    ris <- ris %>% filter(!value == "")
    ris <- ris %>% filter(!var == "xx")
    ris$value <- str_replace(ris$value, pattern = "zyxw", "")
    ris_final <- paste0(ris$var, "  - ", ris$value)
    return(ris_final)
  })
  
  
  ### Display Output Table ###
  
  output$OutputTable <- renderDataTable({
    datatable(
      final_output() %>%
        select(-Date, -Searched_from) %>% 
        relocate(Density, Connections, .after = last_col()),
      selection = list(mode = "single", target = 'cell'),
      extensions = c('Responsive', 'FixedHeader'),
      options = list(buttons = c('copy', 'csv', 'excel', 'print'),
                     fixedHeader = TRUE)
    )
  })
  
  # Click ID or DOI cell to navigate
  observeEvent(input$OutputTable_cells_selected, {
    
    if (nrow(input$OutputTable_cells_selected) == 1 && !is.na(input$OutputTable_cells_selected[1,2])) {
      
      # DISABLED MA redirect
      # if (input$OutputTable_cells_selected[1,2] == 1) {
      #   showModal(modalDialog(
      #     h4(strong("Open this paper's page on the Microsoft Academic database in a new window?")),
      #     footer = actionButton("NavigateMAG", "Proceed"), easyClose = TRUE
      #   ))
      # }
      
      if (input$OutputTable_cells_selected[1,2] == 5) {
        showModal(modalDialog(
          h4(strong("Open this paper's DOI link in a new window?")),
          footer = actionButton("NavigateDOI", "Proceed"), easyClose = TRUE
        ))
      }
      
    }
  })
  
  # DISABLED Navigate to MAG
  # observeEvent(input$NavigateMAG, {
  #   browseURL(paste0(
  #     "https://academic.microsoft.com/paper/",
  #     final_output()$ID[input$OutputTable_cells_selected[1,1]]
  #   ))
  #   removeModal()
  # })
  
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
  
  
  
  ### Download Output ###
  
  output$DownloadOutput <- downloadHandler(
    filename = function() {paste0("Search_Output", format(Sys.time(), "_%Y_%m_%d_%H_%M"), ".csv")},
    content = function(file) {write_csv(final_output(), file)}
  )
  
  output$DownloadRIS <- downloadHandler(
    filename = function() {paste0("Search_Output", format(Sys.time(), "_%Y_%m_%d_%H_%M"), ".ris")},
    content = function(file) {write(RIS_output(), file)}
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
  
  ### Debugger ###
  observeEvent(input$DebugBrowser, {
    browser()
  })
  
  
  
}