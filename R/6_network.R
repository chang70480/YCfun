#' \code{getRC} get R,C coordinate
#'
#' get matrix cell R,C coordinate with specific condition
#'
#' @param matrix matrix or igraph matrix
#' @param condiction example matrix>3
#' @param weight show values in matrix
#' @param undirected undirected
#' @param igraph matrix is igraph data?
#'
#' @return edgelist data_frame
#' @export
getRC <- function(matrix,condiction,weight=F,undirected=F,igraph=F){
	require(igraph)

	if(is.null(rownames(matrix)))rownames(matrix) <- 1:nrow(matrix)
	if(is.null(colnames(matrix)))colnames(matrix) <- 1:ncol(matrix)
	if(undirected)matrix[lower.tri(matrix,diag = F)] <- 0
	if(!igraph){
		condiction <- as.vector(condiction)
		edgelist<- data_frame(from=rep(rownames(matrix),ncol(matrix))[condiction],
							  to=rep(colnames(matrix),each=ncol(matrix))[condiction]
		) %>% arrange(from,to)
		if(weight){
			edgelist$weight <- matrix[condiction]
		}
	}else{
		matrix[condiction] <- 1
		matrix[!condiction] <- 0
		ig <- graph.adjacency(matrix)
		edgelist <- get.edgelist(ig) %>% as.data.frame.matrix() %>% rename(from=1,to=2) %>% as.tibble()
		edgelist$from <- as.character(edgelist$from)
		edgelist$to <- as.character(edgelist$to)
	}

	return(edgelist)
}

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
		el <- as_edgelist(igraph.matrix) %>% tibble::as_data_frame(.)
		if(outpute[1]=='edgelist2' & !binary){
			el <- el  %>% add_column(weight= E(ne)$weight)
		}
		if(outpute[1]=='edgelist3' & !binary){
			el <- el  %>% add_column(weight= E(ne)$weight)
			posi <- lapply(1:nrow(el), function(i){rep(i,el$weight[i])}) %>% unlist()
			el <- el[posi,-3]
		}
		names(el)[1:2] <- c("from","to")


		return(el)
	}
}



#' \code{isolate_posi} find network layout outliers
#'
#' find network layout outliers and move them close center
#'
#' @param l matrix. network plot position
#' @param a recale time
#' @param irq define outliers index
#'
#' @return matrix
#' @export
isolate_posi <- function(l,a=0.7,irq=1.5){
	require(igraph)

	IQR_outliers <- function(x,dis_time=1.5) {
		if(any(is.na(x)))
			stop("x is missing values")
		if(!is.numeric(x))
			stop("x is not numeric")
		Q3<-quantile(x,0.75)
		Q1<-quantile(x,0.25)
		IQR<-(Q3-Q1)
		left<- (Q1-(dis_time*IQR))
		right<- (Q3+(dis_time*IQR))
		return(x <left|x>right)
	}
	xd <- median(l[,1])
	yd <- median(l[,2])
	len <- (apply(l,1,function(i) sum((i-c(xd,yd))^2)^.5))
	posi <- IQR_outliers(len,dis_time = irq)
	if(any(posi)){
		l[posi,] <- apply(matrix(l[posi,],ncol = 2),1,function(i) ((i-c(xd,yd))*(a^2))+c(xd,yd)) %>%t()
	}
	return(l)
}
