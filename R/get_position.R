#' \code{get_position} get position
#'
#' get index value
#'
#' @param vec vector
#' @param search conditoin
#' @param ret1 return all or first one, default is first one.
#'
#' @return index vector
#' @export
get_position <- function(vec,search,ret1=1){
	ret <- 1:length(vec)[(!is.na(vec))&(vec==search)]
	if(is.null(ret1)!=T){
		return(ret[ret1])
	}else{
		return(ret)
	}

}
