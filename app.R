options(warn=-1)
options(shiny.maxRequestSize=100*1024^2)
source('app_source/snowballer_source.R')

ui <- fluidPage(theme = shinytheme("readable"),
  titlePanel("Snowballer"),
  sidebarLayout(
    sidebarPanel(
      h2("Setup"),
      textInput("MA_key", p("Microsoft Academic API Key <",
                            a("GET", href="https://msr-apis.portal.azure-api.net/products/project-academic-knowledge"), "> :"),
                placeholder = "(Leave blank to use stored key)"),
      textInput("EL_key", p("Elsevier API Key <",
                            a("GET", href="https://dev.elsevier.com"), "> :"),
                placeholder = "(Leave blank to use stored key)"),
      fileInput("screened", "Upload Running List of Paper IDs:"),
      h2("Get IDs"),
      fileInput("to_find"," Find Paper IDs on Microsoft Academic Using Title and/or DOI:"),
      downloadButton("paperIDs", "Paper IDs"),
      h2("Snowball"),
      textInput("input_id", "Paper IDs to Snowball (comma separated):"),
      actionButton("do_check", "Check for Repeats"),
      p(),
      verbatimTextOutput("check"),
      checkboxInput("get_abstracts", "Get Abstracts", FALSE),
      checkboxInput("toggle_abstracts", "Toggle Abstracts", TRUE),
      actionButton("do_quick_search", tags$b("Run Search (Quick)")),
      actionButton("do_search", tags$b("Run Search (Comprehensive)")),
      h2("Download"),
      downloadButton("downloadData", "Results"),
      downloadButton("downloadUpdated", "Updated ID List"),
      h2("End"),
      actionButton("disconnect", "Disconnect")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary",
                           br(),
                           p("Number of searched papers read-in:",
                             textOutput("searched_num", inline = TRUE)),
                           br(),
                           p("Paper ID(s) being searched:",
                             verbatimTextOutput("input")),
                           br(),
                           p("Papers found from backward references:",
                             textOutput("back_search", inline = TRUE)),
                           p("Papers found from forward citations:",
                             textOutput("frwd_search", inline = TRUE)),
                           p("Unique papers found (total):",
                             textOutput("unq_found", inline = TRUE)),
                           p("Unique papers found (duplicates removed):",
                             textOutput("found_dup_rm", inline = TRUE))
                  ),
                  tabPanel("Input Data", 
                           dataTableOutput("input_table"),
                           value = "table"
                  ),
                  tabPanel("Output Data", 
                           dataTableOutput("output_table"),
                           value = "table"
                  ),
                  tabPanel("Search Statistics",
                           verbatimTextOutput("skim"),
                           plotOutput("plot_year"),
                           plotOutput("plot_author"),
                           plotOutput("plot_journal")
                  ),
                  tabPanel("Manual",
                           includeMarkdown("app_source/snowballer_manual.Rmd")
                  )
      )
    )
  )
)


server <- function(input, output) {
  
  # read in papers to find IDs for
  to_find_papers <- reactive({
    if(is.null(input$to_find)) return (NULL)
    if(file.exists(input$to_find$datapath)){
      read.csv(input$to_find$datapath)
    } else {return(NULL)}
  })
  found_IDs <- reactive({search.IDs(to_find_papers())})
  
  # read in screened
  screened_data <- reactive({
    if(is.null(input$screened)) return (NULL)
    if(file.exists(input$screened$datapath)){
      read.csv(input$screened$datapath)
    } else {return(NULL)}
  })
  output$searched_num <- renderText(nrow(screened_data()))
  
  # Search input setup
  ## check repeats
  input_check <- eventReactive(input$do_check, {
    repeats <- input_id() %in%
      as.numeric(unlist(str_split(paste(screened_data()$Searched_from, collapse = ", "), ", ")))
    if (length(input_id()[repeats]) != 0){
      input_id()[repeats]
    } else {FALSE}
  })
  output$check <- renderPrint(input_check())
  ## store ID
  input_id <- reactive({as.numeric(strsplit(input$input_id,', ')[[1]])})
  output$input <- renderText({input_id()})
  
  
  
  # offline database search
  ## backward search
  b.data <- reactive({backward.search(input_id())})
  output$back_search <- renderText({nrow(b.data())})
  ## forward search
  f.data <- reactive({forward.search(input_id())})
  output$frwd_search <- renderText({nrow(f.data())})
  ## unique list of IDs found from backward + forward
  found_original <- reactive({
    unique(c(b.data()$Backward_References, f.data()$Forward_Citations))
  })
  output$unq_found <- renderText({length(found_original())})
  ## remove duplicates
  already_found <- reactive({found_original()[found_original() %in% screened_data()$ID]})
  found <- reactive({found_original()[!found_original() %in% screened_data()$ID]})
  output$found_dup_rm <- renderText({length(found())})
  
  
  
  # search
  ## scrape input
  input_data <- eventReactive({c(input$do_search, input$do_quick_search)}, {
    showModal(modalDialog(paste("Fetching", length(input_id()), "paper(s)..."), footer=NULL))
    inputs <- scrape.tidy(input_id())
    removeModal()
    if (input$get_abstracts) {
      inner_join(inputs, scrape.abst.ID(input_id()), by = "ID")
    } else {inputs}
  })
  # search setup
  observeEvent(input$do_search, {
    to_display <<- "c"
    if (!input$MA_key == "") {Sys.setenv(MICROSOFT_ACADEMIC_KEY = input$MA_key)}
    if (!input$EL_key == "") {Sys.setenv(ELSEVIER_SCOPUS_KEY = input$EL_key)}
    opts <<- list(key = Sys.getenv("ELSEVIER_SCOPUS_KEY"))
  })
  observeEvent(input$do_quick_search, {
    to_display <<- "q"
  })
  # comprehensive search output
  output_data_c <- eventReactive(input$do_search, {
    showModal(modalDialog(paste("[Comprehensive Search] Fetching", length(found()), "paper(s)..."), footer=NULL))
    tic <- Sys.time()
    outputs <- scrape.tidy(found())
    if (input$get_abstracts) {
      outputs <- inner_join(outputs, scrape.abst.ID(found()), by = "ID")
      for (i in which(!is.na(outputs$DOI) & is.na(outputs$Abstract))) {
        outputs[i,"Abstract"] <- tryCatch({outputs[i,"Abstract"] <- ft_abstract(x = outputs[i,]$DOI, from = "scopus", scopusopts = opts)$scopus[[1]]$abstract},
                                           error = function(cond){outputs[i,"Abstract"] <- NA})
      }
    }
    toc <- round(as.numeric(Sys.time() - tic, units = "secs"), 3)
    showModal(modalDialog(title = "Search Log",
                          HTML(paste("<b>Time taken:</b>", toc, paste0("seconds (", round(toc/60, 1), " minutes)"),
                                     "<br> <b>Papers searched:</b>", length(found()),
                                     "<br> <b>Papers failed to fetch:</b>", length(found()) - nrow(outputs),
                                     "<br>", paste(found()[!found() %in% outputs$ID], collapse = "<br>"))),
                          footer = NULL, easyClose = TRUE))
    outputs
  })
  ## quick search output
  output_data_q <- eventReactive(input$do_quick_search, {
    showModal(modalDialog(paste("[Quick Search] Fetching", length(found()), "paper(s)..."), footer=NULL))
    tic <- Sys.time()
    outputs <- fast.scrape(found())
    outputs <- tibble(ID = outputs$PaperID, Title = outputs$OriginalTitle, Year = outputs$Year,
                      Authors = NA, Journal = NA, Pub_type = NA, Citations = NA, References = NA)
    if (input$get_abstracts) {outputs <- inner_join(outputs, scrape.abst.ID(found()), by = "ID")} else {outputs$Abstract = NA}
    toc <- round(as.numeric(Sys.time() - tic, units = "secs"), 3)
    showModal(modalDialog(title = "Search Log",
                          HTML(paste("<b>Time taken:</b>", toc, paste0("seconds (", round(toc/60, 1), " minutes)"),
                                     "<br> <b>Papers searched:</b>", length(found()),
                                     "<br> <b>Papers failed to fetch:</b>", length(found()) - nrow(outputs),
                                     "<br>", paste(found()[!found() %in% outputs$ID], collapse = "<br>"))),
                          footer = NULL, easyClose = TRUE))
    outputs
  })
  
  # converge
  
  output_data <- reactive({
    if (to_display == "q") {output_data_q() %>% select_if(function(x){!all(is.na(x))})}
    else if (to_display == "c") {output_data_c()}
  })
  
  # DataTables
  output$input_table <- renderDT(input_data(), class = "display compact")
  output$output_table <- renderDT(if (input$get_abstracts & !input$toggle_abstracts) {select(output_data(), -10)}
                                  else {output_data()},
                                  class = "display compact",
                                  selection = "single",
                                  options = list(pageLength = 25,
                                                 lengthMenu = list(c(25, 50, 100, -1),
                                                                   c("25", "50", "100", "All")),
                                                 scrollX = TRUE,
                                                 autoWidth = TRUE))
  
  observeEvent(input$output_table_rows_selected, {
    paper_doi <- output_data()[input$output_table_rows_selected, "DOI"]
    if (!is.na(paper_doi)) {browseURL(paste0("https://doi.org/", paper_doi))}
  })
  
  
  
  # visualization
  ## setup
  searched_data_original <- reactive({
    backwards_data <- output_data_c() %>%
      filter(ID %in% b.data()$Backward_References) %>% 
      mutate(type = "backward")
    forwards_data <- output_data_c() %>%
      filter(ID %in% f.data()$Forward_Citations) %>% 
      mutate(type = "forward")
    rbind(backwards_data, forwards_data)
  })
  overlap <- reactive({
    searched_data_original()[duplicated(select(searched_data_original(), -type)),]
  })
  searched_data <- reactive({
    s <- searched_data_original()[!duplicated(select(searched_data_original(), -type)),]
    if (!is.null(nrow(overlap))) {s <- s[s$ID %in% overlap$ID,]$type = "both"} ; s
  })
  
  ## overall summary
  output$skim <- renderPrint({
    showModal(modalDialog("Summarizing...", footer=NULL))
    my_skim <- skim_with(numeric = sfl(hist = NULL),
                         character = sfl(whitespace = NULL, empty = NULL),
                         factor = sfl(ordered = NULL))
    skim_data <- mutate(output_data_c(),
                        ID = as.factor(ID),
                        Journal = as.factor(Journal),
                        Pub_type = as.factor(Pub_type))
    select(my_skim(skim_data), -c(n_missing, complete_rate))
  })
  
  ## year summary
  output$plot_year <- renderPlot({
    ggplot(searched_data(), aes(x = Year, fill = fct_relevel(type, "forward", "backward"))) +
      geom_rect(data = input_data(),
                aes(xmin = min(Year), xmax = max(Year), ymin = 0, ymax = Inf),
                fill = "skyblue", alpha = 0.1, show.legend = FALSE) +
      geom_histogram(bins = 25, color = 'white') +
      scale_x_continuous(breaks = seq(min(searched_data()$Year), max(searched_data()$Year), 10)) +
      labs(title = "Year Data", y = "Articles Found", fill = "Search Type") +
      geom_vline(aes(xintercept = median(input_data()$Year)), linetype = 2) + 
      scale_fill_grey() + theme_bw()
  })
  
  ## author summary
  author_data <- reactive({
    a <- tibble(author = unlist(mutate(output_data_c(), Authors = str_split(Authors, ', '))$Authors))
    group_by(a, author) %>% count() %>% arrange(desc(n))
  })
  author_plot_data <- reactive({
    author_data()[1:min(15,nrow(author_data())),]
  })
  
  output$plot_author <- renderPlot({
    ggplot(author_plot_data(), aes(x = fct_reorder(author, desc(n)), y = n)) +
      geom_col(color = 'white') +
      labs(title = "Author Data", x = 'Authors (Top 15)', y = "Count") +
      coord_flip() + theme_bw()
  })
  
  ## journal summary
  journal_data <- reactive({
    searched_data() %>% filter(!is.na(Journal)) %>% 
      group_by(Journal) %>% count() %>% arrange(desc(n))
  })
  journal_plot_data <- reactive({
    journal_data()[1:min(15,nrow(journal_data())),]
  })
  output$plot_journal <- renderPlot({
    removeModal()
    ggplot(journal_plot_data(), aes(x = fct_reorder(Journal, desc(n)), y = n)) +
      geom_col(color = 'white') +
      labs(title = "Journal Data", x = 'Journals (Top 15)', y = "Count") +
      coord_flip() + theme_bw()
  })
  
  
  
  # download
  output$paperIDs <- downloadHandler(
    filename = function()  {"screened.csv"},
    content = function(file) {
      write_csv(found_IDs(), file)
    }
  ) ########################TO DO#####
  
  output$downloadData <- downloadHandler(
    filename = function() {paste0("Snowball_Results", format(Sys.time(), "_%Y_%m_%d_%H_%M"), ".csv")},
    content = function(file) {
      write.csv(as_tibble(cbind(Date = format(Sys.time(), "%a %b %d %X %Y"),
                                Searched_from = paste(input_id(), collapse = ", "),
                                output_data())),
                file, row.names = FALSE)
    }
  )
  output$downloadUpdated <- downloadHandler(
    filename = function() {"Screened.csv"},
    content = function(file) {
      write.csv(bind_rows(mutate(screened_data(), Pub_type = as.character(Pub_type)),
                          as_tibble(cbind(Date = format(Sys.time(), "%a %b %d %X %Y"),
                                          Searched_from = paste(input_id(), collapse = ", "),
                                          output_data()))),
                file, row.names = FALSE)
    }
  )
  
  
  
  # disconnect
  observeEvent(input$disconnect, {
    dbDisconnect(con)
    stopApp()
  })
  
}

shinyApp(ui, server)