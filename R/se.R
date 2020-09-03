#' \code{se} standard error
#'
#' get standard error
#' @param numeric  an numeric vector
#'
#' @return standard error
#' @export
se <- function(numeric=NULL){
	return(sd(numeric,na.rm = T)/sqrt(length(na.omit(numeric))))
}
