#' \code{select_max} Find most frequence
#'
#' select value appearing most frequence in vector
#'
#' @param vec vector
#' @param rank select frequence rank, default=1
#' @param na.rm omits NA
#' @param na.replace NA recode to ?
#' @param check0 if length==0,stop
#'
#' @return 1 length vector
#' @export
#' @examples
#' select_max(c(2,2,2,3,3,4,4,4,4,5))
#' #5
select_max <- function(vec,rank=1,na.rm='no',na.replace=NA,check0=T){
	if(check0){
		if(length(vec)==0)stop()
	}
	cls <- class(vec)[1]
	temp <- vec %>% table(useNA = na.rm) %>% sort(decreasing = T) %>% .[rank] %>% names
	temp <- ifelse(is.null(temp),na.replace,temp)
	switch(cls,
		   character = return(as.character(temp)),
		   numeric = return(as.numeric(temp)),
		   integer = return(as.integer(temp)),
		   logical =return(as.logical(temp)),
		   factor = return(factor(temp,levels = levels(vec))),
		   Date = return(ymd(temp)),
		   POSIXct = return(ymd_hms(temp))
	)
}
