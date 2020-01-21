#options(warn=-1)
source('app_source/snowballer_source.R')

library(shiny)
library(DT)

ui <- fluidPage(
    
    # Application title
    titlePanel("Snowballer"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h2("Setup"),
            textInput("api_key", p("Microsoft Academic API Key:"),
                      placeholder = "(Leave blank to use stored key)"),
            fileInput("file_name","Upload Searched IDs"),
            h2("Search"),
            textInput("input_id", "Article IDs (comma delimited):"),
            actionButton("do_check","Check for Repeats"),
            p(),
            verbatimTextOutput("check"),
            actionButton("do_search", tags$b("Run Search")),
            h2("Write"),
            downloadButton("downloadData", "Download Results"),
            p(),
            downloadButton("downloadUpdated", "Download Updated ID List"),
            h2("End"),
            actionButton("disconnect", "Disconnect")
        ),
        
        # Show a plot of the generated distribution
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
    
    # read in screened
    screened_data <- reactive({
        if(is.null(input$file_name)) return (NULL)
        if(file.exists(input$file_name$datapath)){
            read.csv(input$file_name$datapath)
        } else {return(NULL)}
    })
    
    output$searched_num <- renderText(nrow(screened_data()))
    #output$searched_last <- renderText({tail(screened_data(), 1)$Date})
    
    
    
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
        unique(c(as.numeric(b.data()$Backward_References), as.numeric(f.data()$Forward_Citations)))
        })
    output$unq_found <- renderText({length(found_original())})
    ## remove duplicates
    already_found <- reactive({found_original()[found_original() %in% screened_data()$ID]})
    found <- reactive({found_original()[!found_original() %in% screened_data()$ID]})
    output$found_dup_rm <- renderText({length(found())})

    
    
    # online database search
    ## scrape input
    input_data <- eventReactive(input$do_search, {
        showModal(modalDialog(paste("Fetching", length(input_id()), "paper(s)..."), footer=NULL))
        inputs <- scrape.tidy(input_id())
        input_abst <- scrape.abst.tidy(input_id())
        removeModal()
        inputs %>% 
            rename(ID = Id, Title = Ti, Year = Y, Authors = AA, Journal = J.JN,
                   Pub_type = Pt, Citations = CC, References = RId) %>% 
            select(ID, Title, Year, Authors, Journal, Pub_type, Citations, References) %>% 
            inner_join(input_abst, by = "ID")
    })
    ## scrape output
    observeEvent(input$do_search, {
        if (!input$api_key == "") {Sys.setenv(MICROSOFT_ACADEMIC_KEY = input$api_key)}
    })
    output_data <- eventReactive(input$do_search, {
        showModal(modalDialog(paste("Fetching", length(found()), "paper(s)... (~200/min)"), footer=NULL))
        outputs <- scrape.tidy(found())
        output_abst <- scrape.abst.tidy(found())
        removeModal()
        outputs %>% 
            rename(ID = Id, Title = Ti, Year = Y, Authors = AA, Journal = J.JN,
                   Pub_type = Pt, Citations = CC, References = RId) %>% 
            select(ID, Title, Year, Authors, Journal, Pub_type, Citations, References) %>% 
            inner_join(output_abst, by = "ID")
    })
    
    
    
    # DataTables
    output$input_table <- renderDT(input_data(), class = "display compact")
    output$output_table <- renderDT(output_data(), class = "display compact")
    
    
    
    # visualization
    ## setup
    searched_data_original <- reactive({
        backwards_data <- output_data() %>%
            filter(ID %in% as.numeric(b.data()$Backward_References)) %>% 
            mutate(type = "backward")
        forwards_data <- output_data() %>%
            filter(ID %in% as.numeric(f.data()$Forward_Citations)) %>% 
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
        skim_with(numeric = list(hist = NULL), integer = list(hist = NULL))
        skim(mutate(output_data(),
                    ID = as.factor(ID),
                    Journal = as.factor(Journal),
                    Pub_type = as.factor(Pub_type)))
    })
    
    ## year summary
    output$plot_year <- renderPlot({
        ggplot(searched_data(), aes(x = Year, fill = type)) +
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
        a <- tibble(author = unlist(mutate(output_data(), Authors = str_split(Authors, ', '))$Authors))
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
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("Snowball_Results", format(Sys.time(), "_%Y_%m_%d_%H_%M"), ".csv")
        },
        content = function(file) {
            write.csv(as_tibble(cbind(Date = format(Sys.time(), "%a %b %d %X %Y"),
                                      Searched_from = paste(input_id(), collapse = ", "),
                                      output_data())),
                      file, row.names = FALSE)
        }
    )
    output$downloadUpdated <- downloadHandler(
        filename = function() {"screened.csv"},
        content = function(file) {
            write.csv(rbind(screened_data(),
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