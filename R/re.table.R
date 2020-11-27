#' \code{re.table} recode table
#'
#' recode data attribete's table
#' @param vec vector or data_frame
#' @param len lenght of table summary
#'
#' @return vector or data_frame
#' @export re.table
re.table <- function(vec=vec,len=25){
	ll <- function(vec2,len){
		lens <- ifelse(length(vec2)<len,length(vec2),len)
		if(lens!=0) return(vec2[1:lens])
			else return(table(as.numeric()))

	}
	if("data.frame" %in% class(vec)){
		for (i in 1:length(vec)) {
			tab <- table(vec[[i]],useNA = 'always')
			attr(vec[[i]],"table") <- ll(tab,len=len)
		}
	}else{
		tab <- table(vec,useNA = 'always')
		attr(vec,"table") <- ll(tab,len=len)
	}
	return(vec)
}

