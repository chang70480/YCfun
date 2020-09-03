#' Two-mode to one-mode
#'
#' Two-mode network transform into one-mode network. This function can make 1-mode network matrix,binary edgelist, weight edgelist, and you can set it is directed or not.
#'
#' @param edge.list two mode network(event-perosn) edgelist
#' @param type 1 output: event-evnet ;2 output:person-person
#' @param binary output is 1/0
#' @param cut.at recode vale more than or equal to \code{cut.at} into 1,and else into 0
#' @param twopath 1-mode network transformed from 2-mode is undirect, but it sitll can choose keep two path edge(1-2,2-1) or not(only 1-2)
#' @param outpute choose which datatype you want. "adjacency"=matrix, "edgelist1"=binary edgelist,"edgelist2"=edgelist with weight variable,"edgelist3"=edgelist repeat N multiplied by weight)
#'
#' @return edgelist or matrix
#' @export
#' @examples You can see \href{https://chang70480.github.io/YCfun/inst/html_demo/twomode_onemode.html}{here}.
twomode_onemode <- function(edge.list=edge.list,type=1,binary=F,cut.at=1,twopath=F,outpute=c("adjacency","edgelist1","edgelist2","edgelist3")){
	require(igraph)
	if(!is.data.frame(edge.list))stop()
	if(ncol(edge.list)!=2)stop()
	library(Matrix)
	igraph <- graph.data.frame(d = edge.list,directed = F)
	V(igraph)$type <- bipartite_mapping(igraph)$type
	#bi.matrix <- as_incidence_matrix(igraph,sparse = T)

	# if(type==2){
	# 	matrix <- Matrix::t(bi.matrix) %*% bi.matrix
	# }else if(type==1){
	# 	matrix <- bi.matrix %*% Matrix::t(bi.matrix)
	# }

	ne <- bipartite.projection(igraph, multiplicity = T)[[type]]
	matrix <- ne[,]
	diag(matrix) <- 0
	if(binary){
		matrix[matrix<cut.at] <- 0
		matrix[matrix>=cut.at] <- 1
	}
	if(!twopath){
		matrix[lower.tri(matrix)] <- 0
	}


	if(outpute[1]=="adjacency"){
		return(matrix)
	}else if(outpute[1]=='edgelist1'|outpute[1]=='edgelist2'|outpute[1]=='edgelist3'){

		b_matrix <- matrix
		b_matrix[b_matrix>=1] <- 1

		igraph.matrix <- graph_from_adjacency_matrix(b_matrix,mode="undirected")
		el <- as_edgelist(igraph.matrix)
		colnames(el) <- c("V1","V2")
		el <- tibble::as_data_frame(el)
		if(outpute[1]=='edgelist2' & !binary){
			el <- el  %>% add_column(weight= as.numeric(E(ne)$weight))
		}
		if(outpute[1]=='edgelist3' & !binary){
			el <- el  %>% add_column(weight= as.numeric(E(ne)$weight))
			posi <- lapply(1:nrow(el), function(i){rep(i,el$weight[i])}) %>% unlist()
			el <- el[posi,-3]
		}
		names(el)[1:2] <- c("from","to")


		return(el)
	}
}
