#' \code{check_unique} check all vlaue is unique
#'
#' check all vlaue is unique
#' @param vec vector
#'
#' @return vector
#' @export
check_unique <- function(vec=NULL){
	return(!(vec %in% vec[duplicated(vec)]))
}
