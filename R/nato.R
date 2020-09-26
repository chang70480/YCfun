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
nato <- function(vec,to,Inf_include=T,NaN_include=T){
	vec[is.na(vec)] <- to
	if(Inf_include) vec[is.infinite(vec)] <- to
	if(NaN_include) vec[is.nan(vec)] <- to
	return(vec)
}
