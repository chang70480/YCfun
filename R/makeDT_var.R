
#' \code{makeDT.var} generate varible label table
#'
#' In spss, there are two table in one data, varible label table and varible value table. However, there is only varible value table in R, now this funcion can help you generate varible label table.
#'
#' @param DT data_frame
#'
#' @return varible label table
#' @export
makeDT_var <- function(DT=DT,from=c("frame","variable")){
	temp <- data.frame(var=names(DT)
						 ,var.labels=rep("",length(DT)),stringsAsFactors = F)
	if(from[1]=="frame"){
		variable.labels <- attr(DT,"variable.labels")
		if(!is.null(variable.labels)) temp$var.labels <- variable.labels
	}else {
		for (i in 1:length(DT)) {
			variable.labels <- attr(DT[[i]],'variable.labels')
			if(!is.null(variable.labels)) temp$var.labels[i] <- variable.labels
		}
	}

	#temp %<>% as_tibble()
	return(temp %>% as_tibble())
}

