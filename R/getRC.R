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
