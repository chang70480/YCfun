#' \code{coltype} set reading datatype
#'
#' can set datatpy when using read_csv or read_excel more efficiently
#'
#' @param len set variable datatype
#' @param at setting variable index
#' @param defult defult datatype. read_csv using "?", and read_excel using "guess"
#' @param readfun reading function, set which function do you use to read file.
#'
#' @return coltype
#' @export
coltype <- function(len,at=list(),defult=c("?","guess"),readfun=c("read_csv","read_excel")){
	lookupcols<- function (x){
		switch(x, `_` = , `-` = col_skip(), `?` = col_guess(), c = col_character(),
			   f = col_factor(), d = col_double(), i = col_integer(),
			   l = col_logical(), n = col_number(), D = col_date(),
			   T = col_datetime(), t = col_time(), stop("Unknown shortcut: ",
			   										 x, call. = FALSE))
	}
	if(readfun[1]=="read_excel"){
		vec <- rep(defult,len)
		if(length(at)!=0){
			for(i in names(at)){
				vec[as.numeric(i)] <- at[[i]]
			}
		}
		return(vec)
	}else if(readfun[1]=="read_csv"){
		veccols <- cols(default)
		for(i in 1:len){
			veccols$cols[[i]] <- lookupcols(default)
		}
		if(length(at)!=0){
			for(i in 1:length(at)){
				for(j in at[[i]]){
					veccols$cols[[j]] <- lookupcols(names(at)[i])
				}
			}
		}
		return(veccols)
	}
}
