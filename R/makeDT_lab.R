#' \code{makeDT_lab} read spss and dta
#'
#' This function is based on \code{foreign} and \code{readstata13} this two package, but use this function can operater data easier after some recode dealing.
#'
#' There are three attribute providing for user to konw variable information in each variables. One is \code{variable.labels}, which provides explanation of variable, the other is \code{value.labels},which provides value label for user to contrast value to it's meaning, and the other is \code{table} which summarises variable distribution.
#'
#' @param path file path
#' @param type default="sav". Read dta(stata) or sav(spss)
#' @param label_length default=200. how long \code{variable.labels} you want
#' @param reencode default="Utf-8". some data need to change into "Big5"
#'
#' @return data_frame
#' @export
makeDT_lab <- function(path="",type=c("sav","dta"),label_length=200,reencode=NULL){
	ll <- function(vec){
		if(length(vec)>label_length){
			return(vec[1:label_length])
		}else{
			return(vec)
		}
	}
	if(type[1]=="sav"){
		library(foreign)
		if(is.null(reencode)){
			DT <- read.spss(file = path,use.value.labels = F,to.data.frame = T)
		}else{
			DT <- read.spss(file = path,use.value.labels = F,to.data.frame = T,reencode = reencode)

		}
		temp.lab<- attr(DT,"variable.labels")
		name <- names(DT)

		f_v <- names(DT)[sapply(DT, class)=="factor"]
		for (i in f_v) {
			DT[[i]] <- as.character(DT[[i]])
			DT[[i]] <- gsub(" ","",DT[[i]])
		}
		for(i in 1:length(name)){
			attr(DT[,name[i]],"variable.labels") <- as.character(temp.lab[i])
			attr(DT[,name[i]],"table") <- table(DT[,name[i]],useNA = "always")
		}
		return(DT %>% as_tibble())
	}else if(type[1]=="dta"){

		library(readstata13)
		if(is.null(reencode)){
			DT_ <- read.dta13(path,convert.factors = F)
			DT_n <- read.dta13(path,convert.factors = F)
		}else{
			DT_ <- read.dta13(path,convert.factors = F,encoding = reencode)
			DT_n <- read.dta13(path,convert.factors = F,encoding = reencode)
		}

		attr(DT_n,"variable.labels") <- varlabel(DT_n)
		name_ <- get.label.name(DT_)
		for (l in 1:length(DT_)) {
			if(name_[l]!=""){
				attr(DT_n[[l]],"value.labels") <- ll(get.label(DT_,name_[l]))
			}
			attr(DT_n[[l]],"variable.labels") <- as.character(varlabel(DT_)[l])
			if(name_[l]!=""){
				attr(DT_n[[l]],"table") <- table(DT_n[[l]],useNA = "always")
			}
		}



		return(DT_n %>% as_tibble())

	}
}

