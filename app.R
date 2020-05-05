options(warn=-1)
options(shiny.sanitize.errors = TRUE)
options(shiny.maxRequestSize=500*1024^2)
source('app_source/snowballer_source.R')

ui <- fluidPage(
  theme = shinytheme("readable"),
  titlePanel("Snowballer"),
  sidebarLayout(
    sidebarPanel(
      tags$style(type="text/css", HTML("menutop ul {padding: 0; margin: 0; list-style: none;}")),
      h2("Setup"),
      textInput("MA_key", p("Microsoft Academic API Key <",
                            a("GET", href="https://msr-apis.portal.azure-api.net/products/project-academic-knowledge"), "> :"),
                placeholder = "(Leave blank to use stored key)"),
      textInput("EL_key", p("Elsevier API Key <",
                            a("GET", href="https://dev.elsevier.com"), "> :"),
                placeholder = "(Leave blank to use stored key)"),
      fileInput("screened", "Upload Running List of Papers with Microsoft Academic IDs:"),
      checkboxInput("to_output", "Send to OUTPUT DATA tab", FALSE),
      h2("Get IDs"),
      tippy("<b>Upload Papers to Search on Microsoft Academic:</b>",
            tooltip = 'Upload a CSV file with with <i> one or more </i> of the following column formats:
            <li> "Title" and "DOI" </li> <li> "PMID" (PubMed ID) </li> <li> "PMCID" (PubMed Central ID) </li>
            <br> Download button below outputs a CSV file with the following columns: </br>
            <li> ID (Microsoft Academic ID) </li> <li> Title </li> <li> Year </li> <li> Authors </li>
            <li> Journal </li> <li> Pub_type </li> <li> DOI </li> <li> Citations </li> <li> References </li>'),
      fileInput("to_find", ""),
      downloadButton("paperIDs", "Download Paper IDs"),
      h2("Search"),
      textInput("input_id", "Paper IDs to Snowball (comma separated):"),
      checkboxInput("get_abstracts", "Get Abstracts", FALSE),
      actionButton("do_quick_search", tags$b("Run Search (Quick)")),
      actionButton("do_search", tags$b("Run Search (Comprehensive)")),
      p(),
      p(tags$b("Output Display Options")),
      checkboxInput("toggle_abstracts", "Toggle Abstracts", TRUE),
      checkboxInput("select_NA", "Show Missing Abstracts", FALSE),
      h2("Download"),
      downloadButton("downloadData", "Download Results"),
      downloadButton("downloadUpdated", "Download Updated ID List")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary",
                           br(),
                           p("Number of paper IDs uploaded:",
                             textOutput("searched_num", inline = TRUE)),
                           br(),
                           p("Paper IDs being searched:",
                             textOutput("input_n", inline = TRUE),
                             verbatimTextOutput("input")),
                           br(),
                           p("Papers found from backward references:",
                             textOutput("back_search", inline = TRUE)),
                           p("Papers found from forward citations:",
                             textOutput("frwd_search", inline = TRUE)),
                           p("Unique papers found from forward/backward searches:",
                             textOutput("unq_found", inline = TRUE)),
                           p("Total unique papers found (duplicates from uploaded IDs removed):",
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
                  tabPanel("Network Visualization", 
                           visNetworkOutput("visualnetwork"),
                           p(),
                           downloadLink("downloadNetwork", "Download Visual Network (.html)")
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
      read_csv(input$to_find$datapath)
    } else {return(NULL)}
  })
  found_IDs <- reactive({
    if (!is.null(to_find_papers()$PMID)) {search.IDs(PMID.search(to_find_papers()$PMID))}
    else if (!is.null(to_find_papers()$PMCID)) {search.IDs(PMID.search(to_find_papers()$PMCID, type = "pmc"))}
    else{search.IDs(to_find_papers())}
  })
  
  
  # read in running list
  screened_data <- reactive({
    if(is.null(input$screened)) return (NULL)
    if(file.exists(input$screened$datapath)){
      read.csv(input$screened$datapath)
    } else {return(NULL)}
  })
  output$searched_num <- renderText(nrow(screened_data()))
  
  
  # Search input setup
  ## remove repeats
  input_orig <- reactive({as.numeric(strsplit(input$input_id,', ')[[1]])})
  repeats <- reactive({as.numeric(unlist(str_split(paste(screened_data()$Searched_from, collapse = ", "), ", ")))})
  input_id <- reactive({unique(input_orig()[!input_orig() %in% repeats()])})
  ## display IDs to search  
  output$input <- renderText({input_id()})
  output$input_n <- renderText({length(input_id())})
  
  
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
  
  
  
  # online query
  ## scrape input
  input_data <- eventReactive({c(input$do_search, input$do_quick_search)}, {
    showModal(modalDialog(paste("Fetching", length(input_id()), "paper(s)..."), footer=NULL))
    inputs <- scrape.tidy(input_id())
    removeModal()
    if (input$get_abstracts) {
      inputs <- inner_join(inputs, scrape.abst.ID(input_id()), by = "ID")
    } else {inputs <- inputs}
    original_titles <- fast.scrape(input_id()) %>% select(ID, Title)
    inner_join(select(inputs, -Title), original_titles, by = "ID") %>% select(ID, Title, everything())
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
      # MAG fetch
      outputs <- inner_join(outputs, scrape.abst.ID(found()), by = "ID")
      outputs <- mutate(outputs, Abstract = ifelse(grepl(Abstract, pattern = "[.]{3}$"), NA, Abstract)) # remove incomplete abstracts
      # crossref fetch
      for (i in which(!is.na(outputs$DOI) & is.na(outputs$Abstract))) {
        outputs[i,"Abstract"] <- tryCatch({outputs[i,"Abstract"] <- cr_abstract(outputs[i,]$DOI)},
                                          error = function(cond){outputs[i,"Abstract"] <- NA})
      }
      # scopus fetch
      for (i in which(!is.na(outputs$DOI) & is.na(outputs$Abstract))) {
        outputs[i,"Abstract"] <- tryCatch({outputs[i,"Abstract"] <- ft_abstract(x = outputs[i,]$DOI,
                                                                                from = "scopus", scopusopts = opts)$scopus[[1]]$abstract},
                                          error = function(cond){outputs[i,"Abstract"] <- NA})
      }
    }
    original_titles <- fast.scrape(found()) %>% select(ID, Title)
    toc <- round(as.numeric(Sys.time() - tic, units = "secs"), 3)
    showModal(modalDialog(title = "Search Log",
                          HTML(paste("<b>Time taken:</b>", toc, paste0("seconds (", round(toc/60, 1), " minutes)"),
                                     "<br> <b>Papers searched:</b>", length(found()),
                                     "<br> <b>Papers failed to fetch:</b>", length(found()) - nrow(outputs),
                                     "<br>", paste(found()[!found() %in% outputs$ID], collapse = "<br>"))),
                          footer = NULL, easyClose = TRUE))
    inner_join(select(outputs, -Title), original_titles, by = "ID") %>% select(ID, Title, everything())
  })
  
  ## quick search output
  output_data_q <- eventReactive(input$do_quick_search, {
    showModal(modalDialog(paste("[Quick Search] Fetching", length(found()), "paper(s)..."), footer=NULL))
    tic <- Sys.time()
    outputs <- fast.scrape(found())
    outputs <- tibble(ID = outputs$ID, Title = outputs$Title, Year = outputs$Year, Pub_type = outputs$Pub_type, DOI = outputs$DOI,
                      Authors = NA, Journal = NA, Citations = NA, References = NA)
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
  display_outdata <- reactive({
    if (input$to_output) {df <- screened_data()}
    else {df <- output_data()}
    if (input$select_NA) {df <- filter(df, is.na(Abstract))}
    if (input$get_abstracts & !input$toggle_abstracts) {
      df <- tryCatch({df <- select(df, -Abstract)},
                     error = function(cond){df <- df})
    }
    df
  })
  output$output_table <- renderDT(display_outdata(), class = "display compact", selection = "single",
                                  options = list(pageLength = 25, lengthMenu = list(c(25, 50, 100, -1), 
                                                                                    c("25", "50", "100", "All"))))
  
  # cells clickable
  observeEvent(input$output_table_rows_selected, {
    paper_doi <- display_outdata()[input$output_table_rows_selected, "DOI"]
    if (!is.na(paper_doi)) {browseURL(paste0("https://doi.org/", paper_doi))}
    else {browseURL(paste0("https://academic.microsoft.com/paper/",
                           display_outdata()[input$output_table_rows_selected, "ID"]))}
  })
  
  
  
  # results summary
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
  
  ## skim summary
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
      scale_fill_manual(labels = c("forward", "backward"), values = c("grey20", "grey60")) +
      theme_bw()
  })
  
  ## author summary
  author_data <- reactive({
    a <- data.table(searched_data())[, list(Authors = unlist(strsplit(Authors, ", "))), by = type]
    total_count <- a %>% group_by(Authors) %>% count() %>% rename(n_total = n)
    each_count <- a %>% group_by(Authors, type) %>% count() %>% rename(n_type = n)
    inner_join(each_count, total_count, by = "Authors") %>% ungroup()
  })
  top_authors <- reactive({
    (author_data() %>% select(-contains("type")) %>% unique() %>% arrange(desc(n_total)))$Authors[1:15]
  })
    
  output$plot_author <- renderPlot({
    author_data()[author_data()$Authors %in% top_authors(), ] %>% 
    ggplot(aes(x = fct_relevel(Authors, top_authors()), y = n_type,
               fill = fct_relevel(type, c("forward", "backward")))) +
      geom_col(color = 'white') +
      labs(title = "Author Data", x = 'Authors (Top 15)', y = "Count", fill = "Direction") +
      coord_flip() + theme_bw() +
      scale_fill_manual(labels = c("forward", "backward"), values = c("grey20", "grey60")) +
      scale_y_continuous(breaks = pretty(1:max(author_data()$n_total)))
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
  
  
  
  # network analysis for result output
  ## full network
  network <- reactive({
    f <- f.data() %>%
      rename(from = ID, to = Forward_Citations) %>% 
      mutate(direction = "forward")
    b <- b.data() %>%
      rename(from = ID, to = Backward_References) %>% 
      mutate(direction = "backward")
    rbind(b, f)
  })
  ## network between input and output
  output_data_results <- reactive({
    net <- network() %>% group_by(to) %>%
      summarize(Density = length(unique(unlist(list(from)))),
                Connections = paste(unique(unlist(list(from))), collapse = ", "))
    res <- inner_join(output_data(), rename(net, ID = to), by = "ID")
    cbind(Searched_from = paste(input_id(), collapse = ", "), res)
  })
  ## network between inputs
  input_data_results <- reactive({
    net <- network() %>% filter(to %in% input_id()) %>% group_by(to) %>%
      summarize(Density = length(unique(unlist(list(from)))),
                Connections = paste(unique(unlist(list(from))), collapse = ", ")) %>% 
      rename(ID = to)
    res <- merge(input_data(), net, all.x = TRUE)
    res[is.na(res$Density),"Density"] = 0
    res[is.na(res$Connections),"Connections"] = ""
    cbind(Searched_from = "INPUT", res)
  })
  
  
  # network graph
  ## graph setup
  nodes <- reactive({
    s <- screened_data()$ID[!screened_data()$ID %in% input_id()]
    t <- rbind(tibble(id = unique(network()$from), group = "Snowballed"),
               tibble(id = unique(network()$to[!network()$to %in% network()$from]), group = "Newly Found"))
    t$color.border <- "grey"
    t[t$group == "Newly Found" & t$id %in% s,"group"] <- "Previously Found"
    hover_info <- fast.scrape(t$id) %>%
      mutate(title = paste0("<p><b>ID:</b> ", ID,
                            "<br><b>Title:</b> ", Title,
                            "<br><b>Year:</b> ", Year,
                            "<br><b>DOI:</b> ", DOI ,
                            "<br><b>Publication Type</b>: ", Pub_type, 
                            "</p>")) %>% 
      select(ID, title) %>% rename(id = ID)
    inner_join(t, hover_info, by = "id")
  })
  edges <- reactive({
    ledges <<- tibble(color = "skyblue", label = c("Forward", "Backward"), dashes = c(TRUE, FALSE))
    scale <- count(network(), from) %>% mutate(length = n^(4/7) * 25) %>% select(-n)
    network() %>%
      mutate(dashes = (direction == "forward")) %>% 
      left_join(scale, by = "from")
  })
  ## graph aesthetics
  visnet <- reactive({
    graph <- visNetwork(nodes(), edges()) %>%
      visLayout(randomSeed = 97) %>% 
      visPhysics(maxVelocity = 10, timestep = 1, enabled = FALSE) %>% 
      visGroups(groupname = "Snowballed", color = "skyblue") %>%
      visGroups(groupname = "Newly Found", color = "lightgreen") %>% 
      visGroups(groupname = "Previously Found", color = "lightgoldenrodyellow") %>% 
      visNodes(color = list(border = "grey")) %>% 
      visEdges(color = list(color = "skyblue")) %>% 
      visLegend(addEdges = ledges) %>% 
      visInteraction(dragNodes = FALSE, keyboard = TRUE) %>% 
      visOptions(highlightNearest = list(enabled = TRUE), nodesIdSelection = TRUE, selectedBy = "group",
                 width = "200%", height = "200%")
    if (nrow(nodes()) > 1000 | max(count(edges(), from)$n) > 500) {
      showModal(modalDialog("WARNING: Network is too large (nodes > 1000) and/or too dense (degrees > 500)",
                            footer = NULL, easyClose = TRUE))
    }
    else {
      graph <- graph %>%
        visPhysics(enabled = TRUE, stabilization = FALSE) %>% 
        visInteraction(dragNodes = TRUE) %>% 
        visEdges(arrows = "to")}
    graph
  })
  ## graph output
  output$visualnetwork <- renderVisNetwork({visnet()})
  
  
  # download
  ## download search results
  output$paperIDs <- downloadHandler(
    filename = function()  {"ID_list.csv"},
    content = function(file) {
      write_csv(found_IDs(), file)
    }
  )
  ## download snowball results
  output$downloadData <- downloadHandler(
    filename = function() {paste0("Snowball_Results", format(Sys.time(), "_%Y_%m_%d_%H_%M"), ".csv")},
    content = function(file) {
      write.csv(as_tibble(cbind(Date = format(Sys.time(), "%a %b %d %X %Y"),
                                bind_rows(input_data_results(), output_data_results()))),
                file, row.names = FALSE)
    }
  )
  ## download updated ID list
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
  ## download visual network
  output$downloadNetwork <- downloadHandler(
    filename = function() {paste0("Snowball_Network", format(Sys.time(), "_%Y_%m_%d_%H_%M"), ".html")},
    content = function(con) {visnet() %>% visSave(con)}
  )
  
  
}

shinyApp(ui, server)