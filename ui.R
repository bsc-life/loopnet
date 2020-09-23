library(visNetwork)
library(shinyjs)

ui <- fluidPage(

	titlePanel(title=div(img(src="BSC_logo.png", height = 80, width = 60), HTML("<i>loopnet</i> &#x2015; Protein interactomes at chromatin loops"))),

  sidebarLayout(
    sidebarPanel(
      h2("Input files"),
      fileInput("file1", ""),
      actionButton("refresh", "Refresh session"),
      br(),
      h2("Node-Level Statistics"),
      radioButtons("node_stats", "",
                    choices = c("Degree centrality" = "degree_centrality",
                    "Page Rank" = "page_rank",
                    "Betweenness centrality" = "betweenness_centrality",
                    "None" = "none"), selected = "none"),
    ),
    mainPanel(
      textOutput("text"),
      visNetworkOutput("network"),
      br(),
      tableOutput("ranked_nodes")
  )
)
)