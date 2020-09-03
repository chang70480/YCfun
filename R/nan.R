#' \code{nan} Count NA
#'
#' @param vec vector
#'
#' @return table list
#' @export
nan <- function(vec){
	nas <- sum(is.na(vec))
	return(c("All"=length(vec),nNA=length(vec)-nas,"NA"=nas))
}
