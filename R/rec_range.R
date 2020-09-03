#' rescale numeric range
#'
#' \code{rec_range}
#' @param nuc.vec an numeric vector
#' @param to.min rescale max value
#' @param to.max rescale min value
#' @param dot round to N'th decimal place
#'
#' @return the rescale numeric vector
#' @export
rec_range<- function(nuc.vec=NULL,to.min=NULL,to.max=NULL,dot=0){
	if((is.null(to.min)&is.null(to.max)))stop()
	if(is.null(to.min))to.min <- min(nuc.vec,na.rm = T)
	if(is.null(to.max))to.max <- max(nuc.vec,na.rm = T)
	temp.nuc <- nuc.vec-min(nuc.vec,na.rm = T)
	temp.nuc <- temp.nuc*((to.max-to.min)/max(temp.nuc,na.rm = T))
	temp.nuc <- temp.nuc+to.min
	return(round(temp.nuc,dot))
}
