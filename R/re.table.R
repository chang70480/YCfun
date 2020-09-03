#' \code{re.table} recode table
#'
#' recode data attribete's table
#' @param vec vector or data_frame
#'
#' @return vector or data_frame
#' @export re.table
re.table <- function(vec=vec){
	if("data.frame" %in% class(vec)){
		for (i in 1:length(vec)) {
			attr(vec[[i]],"table") <- table(vec[[i]],useNA = 'always')
		}
	}else{
		attr(vec,"table") <- table(vec,useNA = 'always')
	}
	return(vec)
}
