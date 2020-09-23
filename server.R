library(stringr)
library(igraph)
library(plyr)
library(visNetwork)
library(shinyjs)

server <- function(input, output, session) {

	observe({
	
	req(input$file1)

    a <- read.table(input$file1$datapath,sep='\t',header=T)
    
    tfs_E <- unique(unlist(lapply(str_split(a$st,"::"),
    				function(x) head(x,n=1))))
    tfs_P <- unique(unlist(lapply(str_split(a$st,"::"),
    				function(x) tail(x,n=1))))
    tfs_EP <- intersect(tfs_E,tfs_P)

    rrr <- function(x){
			df <- data.frame()
			for(i in 1:(length(x)-1)){
				df <- rbind(df,cbind(x[i],x[i+1]))
			}
			return(df)
		}

	edges <- unique(ldply(lapply(str_split(a$st,"::"),function(x) rrr(x)), data.frame))
	colnames(edges) <- c('from','to')

	nodes <- as.character(unique(unlist(edges)))
	nodes <- data.frame('id'=nodes, 'title'=nodes,
						'color.background'=ifelse(nodes%in%tfs_EP,"tomato",
							ifelse(nodes%in%tfs_E,"lightblue",
								ifelse(nodes%in%tfs_P, "purple","grey80"))),
						'color.border'=ifelse(nodes%in%tfs_EP,"tomato",
							ifelse(nodes%in%tfs_E,"lightblue",
								ifelse(nodes%in%tfs_P, "purple","grey80")))
						)

	G <- graph_from_data_frame(edges, directed=FALSE, vertices=nodes$id)
	txt <- paste0('num of edges:', gsize(G),' num of nodes:', gorder(G)) 
	output$text <- renderText({ txt })

			if(input$node_stats == "degree_centrality"){
				nodes$value <- degree(G)
			}
			if(input$node_stats == "page_rank"){
				nodes$value <- page_rank(G, directed = FALSE)$vector
			}
			if(input$node_stats == "betweenness_centrality"){
				nodes$value <- betweenness(G, v = V(G), directed = FALSE)
			}
			if(input$node_stats == "none"){
				nodes$value <- 1
			}

	output$network  <- renderVisNetwork({
		visNetwork(nodes, edges) %>%
  		visIgraphLayout() %>%
  		visEdges(color = "rgba(84,84,84,0.5)") %>%
  		#visPhysics(stabilization = FALSE) %>% visEdges(smooth = FALSE)
      	visEvents(select = "function(nodes){Shiny.onInputChange('current_node_id', nodes.nodes);}") %>%
      	visOptions(highlightNearest = TRUE)
	})

	#select a node
	myNode <- reactiveValues(selected = '')
	observeEvent(input$current_node_id, {
    	myNode$selected <- input$current_node_id
  	})
  	output$selected_node <- renderPrint({
  		req(myNode$selected)
  		myNode$selected
  	})

  	#print ranked nodes
  	output$ranked_nodes <- renderTable({
  		if(input$node_stats!='none'){
  			
			tab <- nodes[order(nodes$value,decreasing=T),c('title','value')]
			if(input$node_stats == "degree_centrality"){
				tag <- "Degree centrality"
			}
			if(input$node_stats == "page_rank"){
				tag <- "PageRank"
			}
			if(input$node_stats == "betweenness_centrality"){
				tag <- "Betweenness centrality"
			}
			colnames(tab) <- c('Gene',tag)
			head(tab,100)
		}
  	}, caption = "Nodes ranking", caption.placement = getOption("xtable.caption.placement", "top"), width = '200%')


  	# refresh button
  	observeEvent(input$refresh, {
    	session$reload()
  	})


	})
}
