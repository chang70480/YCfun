
#' \code{makeDT.var} generate varible label table
#'
#' In spss, there are two table in one data, varible label table and varible value table. However, there is only varible value table in R, now this funcion can help you generate varible label table.
#'
#' @param DT data_frame
#'
#' @return varible label table
#' @export
makeDT_var <- function(DT=DT){
	temp <- data.frame(題號=names(DT)
						 ,內容=rep("",length(DT)),stringsAsFactors = F)
	for(i in 1:length(DT)){
		temp.leb <- as.character(attr(DT,"variable.labels"))[i]
		if(is.na(temp.leb)){
			temp$內容[i] <- ""
		}else{
			temp$內容[i] <- temp.leb
		}
	}
	#temp %<>% as_tibble()
	return(temp %>% as_tibble())
}

