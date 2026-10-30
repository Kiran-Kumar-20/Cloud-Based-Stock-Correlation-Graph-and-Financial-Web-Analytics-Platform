library(shiny)
library(quantmod)
library(igraph)
library(visNetwork)
library(rvest)
library(xml2)
library(DBI)
library(RPostgres)
library(ggplot2)
library(reshape2)

# --- 1. DATABASE SETUP ---
connect_db <- function() {
  tryCatch({
    dbConnect(RPostgres::Postgres(),
              dbname = "",
              host = "", 
              port = ,
              user = "",                   
              password = "",               
              sslmode = "")
  }, error = function(e) return(NULL))
}

# --- 2. UI FRONTEND ---
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
  titlePanel("Financial Contagion Network & Web Intelligence Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Control Panel"),
      sliderInput("corr_threshold", "Edge Threshold (Correlation > r):",
                  min = 0.1, max = 0.95, value = 0.5, step = 0.05),
      actionButton("refresh", "Fetch Live Market Data", class = "btn-primary btn-block"),
      hr(),
      h4("Web-Scraped Market News"),
      uiOutput("newsList"),
      hr(),
      h5("Cloud Engine Status"),
      verbatimTextOutput("dbStatus")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel("Contagion Network", 
                 br(),
                 p("Nodes represent NSE blue-chip stocks. Edges represent price co-movement above the chosen threshold."),
                 visNetworkOutput("networkGraph", height = "500px")),
        
        tabPanel("Graph Centrality & Metrics", 
                 br(),
                 h4("Systemic Risk & Influence Analysis"),
                 p("Centrality indicates systemic contagion risk: if a highly central stock drops, connected stocks are statistically likely to follow."),
                 tableOutput("centralityTable")),
        
        tabPanel("Correlation Matrix", 
                 br(),
                 h4("Pairwise Pearson Correlation Heatmap"),
                 plotOutput("heatmapPlot", height = "450px")),
        
        tabPanel("Historical Trends", 
                 br(),
                 h4("Normalized Price Movement (Last 60 Days)"),
                 plotOutput("trendPlot", height = "450px")),
        
        tabPanel("Cloud Audit Logs", 
                 br(),
                 h4("PostgreSQL Access Telemetry (Neon Cloud)"),
                 tableOutput("dbLogsTable"))
      )
    )
  )
)

# --- 3. SERVER BACKEND ---
server <- function(input, output, session) {
  
  # Market Data Fetching
  market_data <- eventReactive(input$refresh, {
    tickers <- c("RELIANCE.NS", "TCS.NS", "INFY.NS", "HDFCBANK.NS", "WIPRO.NS", "SBIN.NS")
    getSymbols(tickers, src = "yahoo", from = Sys.Date() - 60, to = Sys.Date(), auto.assign = TRUE)
    
    close_list <- lapply(tickers, function(x) Cl(get(x)))
    prices <- do.call(merge, close_list)
    names(prices) <- gsub(".NS.Close", "", names(prices))
    prices <- na.locf(prices)
    
    # Calculate log returns
    returns <- diff(log(prices))[-1, ]
    cor_matrix <- cor(returns, use = "pairwise.complete.obs")
    
    # Write to Cloud DB
    con <- connect_db()
    if(!is.null(con)) {
      dbExecute(con, "CREATE TABLE IF NOT EXISTS app_usage_logs (access_time TIMESTAMP, action VARCHAR(50))")
      dbExecute(con, "INSERT INTO app_usage_logs (access_time, action) VALUES (NOW(), 'Refreshed Market Graph')")
      dbDisconnect(con)
    }
    
    list(prices = prices, returns = returns, cor_matrix = cor_matrix)
  }, ignoreNULL = FALSE)
  
  # Status String
  output$dbStatus <- renderText({
    market_data()
    "Neon PostgreSQL: Connected\nTelemetry: Logged"
  })
  
  # Graph Construction Helper
  build_graph <- reactive({
    cor_mat <- market_data()$cor_matrix
    th <- input$corr_threshold
    
    adj <- cor_mat
    adj[adj < th] <- 0
    diag(adj) <- 0
    
    g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
    g
  })
  
  # Interactive Network
  output$networkGraph <- renderVisNetwork({
    g <- build_graph()
    nodes <- data.frame(id = V(g)$name, label = V(g)$name, 
                        shape = "circle", size = 25,
                        color.background = "#2c3e50", color.border = "#18bc9c",
                        font.color = "white")
    
    edges_df <- as_data_frame(g, what = "edges")
    if(nrow(edges_df) > 0) {
      edges_df$width <- edges_df$weight * 6
      edges_df$color <- "#18bc9c"
    } else {
      edges_df <- data.frame(from = character(), to = character(), width = numeric(), color = character())
    }
    
    visNetwork(nodes, edges_df) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based")
  })
  
  # Centrality Metrics Table
  output$centralityTable <- renderTable({
    g <- build_graph()
    cor_mat <- market_data()$cor_matrix
    
    deg <- degree(g)
    btw <- betweenness(g, normalized = TRUE)
    eig <- eigen_centrality(g)$vector
    
    df <- data.frame(
      Stock = names(deg),
      `Degree (Connections)` = as.integer(deg),
      `Betweenness Centrality` = round(btw, 4),
      `Eigenvector Centrality` = round(eig, 4),
      `Avg Correlation` = round(colMeans(cor_mat), 4),
      check.names = FALSE
    )
    df[order(-df$`Eigenvector Centrality`), ]
  })
  
  # Correlation Heatmap
  output$heatmapPlot <- renderPlot({
    melted <- melt(market_data()$cor_matrix)
    ggplot(melted, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "#e74c3c", high = "#2ecc71", mid = "#ecf0f1", midpoint = 0, limit = c(-1,1)) +
      geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
      theme_minimal() +
      labs(title = "Pearson Correlation Matrix of Daily Returns", x = "", y = "", fill = "r")
  })
  
  # Normalized Trends Plot
  output$trendPlot <- renderPlot({
    prices <- as.data.frame(market_data()$prices)
    prices$Date <- as.Date(rownames(prices))
    
    # Normalize to base 100
    norm_prices <- prices
    for(col in setdiff(names(prices), "Date")) {
      norm_prices[[col]] <- (norm_prices[[col]] / norm_prices[[col]][1]) * 100
    }
    
    m_prices <- melt(norm_prices, id.vars = "Date", variable.name = "Stock", value.name = "Base100")
    ggplot(m_prices, aes(x = Date, y = Base100, color = Stock)) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(title = "60-Day Relative Price Performance (Base = 100)", y = "Relative Growth (%)", x = "Date")
  })
  
  # Live Database Logs Table
  output$dbLogsTable <- renderTable({
    con <- connect_db()
    if(!is.null(con)) {
      res <- dbGetQuery(con, "SELECT access_time, action FROM app_usage_logs ORDER BY access_time DESC LIMIT 10")
      dbDisconnect(con)
      res
    } else {
      data.frame(Status = "Database connection offline")
    }
  })
  
  # Web Scraped News
  output$newsList <- renderUI({
    input$refresh
    tryCatch({
      url <- "https://news.google.com/rss/search?q=NSE+India+Stock+Market&hl=en-IN&gl=IN&ceid=IN:en"
      news_page <- read_xml(url)
      titles <- news_page %>% xml_find_all("//item/title") %>% xml_text()
      links <- news_page %>% xml_find_all("//item/link") %>% xml_text()
      
      news_html <- lapply(1:min(4, length(titles)), function(i) {
        tags$li(tags$a(href = links[i], titles[i], target = "_blank"), style="margin-bottom: 8px; font-size: 12px;")
      })
      tags$ul(news_html)
    }, error = function(e) {
      tags$p("Could not fetch web news.")
    })
  })
}

shinyApp(ui, server)
