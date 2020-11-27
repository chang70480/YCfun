#' high efficiency recode
#'
#' when recode rules are more than 10000, this function is more efficienct
#' @param recordtab data_frame. The table of the recode rule. V1 is old value; V2 is new value
#' @param old vector. recoded aims
#' @param quickNA efficienct option
#' @copy copy else
#'
#' @return recoded vector
#' @export
change_posi <- function(recordtab,old=NULL,quickNA=T,copy=F){
	if(quickNA & !copy){
		old[!(old %in% recordtab[[1]])] <- NA
	}else if(copy){
		copylist <- unique(old[! old %in% recordtab[[1]]])
	}
	recordtab[[1]] <- as.character(recordtab[[1]])
	NAr <- data_frame(V1="NA",V2=NA)
	names(NAr) <- names(recordtab)
	if(exists("copylist")){
		copyr <- data_frame(V1=copylist,V2=copylist)
		names(copyr) <- names(recordtab)
		recordtab <- bind_rows(recordtab,copyr,NAr)
	}else{
		recordtab <- bind_rows(recordtab,NAr)
	}




	new <- recordtab[[2]]
	names(new) <- recordtab[[1]]

	out <- new[as.character(old)]
	names(out) <- NULL
	len  <- nan(old)[2]
	len2 <- nan(out)[2]
	if(len!=len2){
		cat(paste0("NA:",len,"->",len2,"\n"))
	}
	return(out)
}


