#' recode NA
#'
#' recode NA into specific value
#'
#' @param vec vector
#' @param to recode into ?
#' @param Inf_include recode including Inf?
#'
#' @export
#' @return vector
nato <- function(vec,to,Inf_include=T){
	if(Inf_include){
		vec[is.na(vec)|is.infinite(vec)] <- to
	}else{
		vec[is.na(vec)] <- to
	}
	return(vec)
}
