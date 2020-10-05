
#' Title
#'
#' @param DT_
#' @param expa
#' @param keep
#' @param NULLtoNA
#'
#' @return
#' @export
#'
#' @examples
my_expand <- function(DT_,expa,keep,NULLtoNA=F){
	if(NULLtoNA){
		n <- sapply(DT_[[expa]],function(x){
			x_ <- length(x)
			if(x_==0)x_ <- 1
			x_
		}) %>% unlist()

		var <- sapply(DT_[[expa]], function(i){
			if(is.null(i))i <- NA
			i
		}) %>% unlist() %>% as.vector()
	}else{
		n <- sapply(DT_[[expa]],length) %>% unlist()
		var <- unlist(DT_[[expa]]) %>% as.vector()
	}

	DT_ <- DT_[rep(1:nrow(DT_),n),]
	DT_[[expa]] <- var
	if(!missing(keep))DT_ %<>% dplyr::select(keep,expa)
	DT_
}
