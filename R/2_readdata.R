#' \code{makeDT.lab} read spss and dta
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
			DT_ <- read.dta13(path,convert.factors = T,generate.factors = T,nonint.factors = T)
			DT_n <- read.dta13(path,convert.factors = T,generate.factors = T,nonint.factors = F)
		}else{
			DT_ <- read.dta13(path,convert.factors = T,generate.factors = T,nonint.factors = T,encoding = reencode)
			DT_n <- read.dta13(path,convert.factors = T,generate.factors = T,nonint.factors = F,encoding = reencode)
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



#' \code{tscs} show tscs Data
#'
#' There are many tscs R's Data in YCfun package, and they are dispersed in \code{tscsPhase1-7}.
#'
#' @return data
#' @export
#' @examples tscs()
tscs <- function(){
cat("\nin tscsPhase1
tscs851-綜合		tscs852-宗教休閒家庭
\nin tscsPhase2
tscs851-綜合		tscs901-綜合
tscs902-宗教休閒家庭	tscs911-家庭教育
tscs912-心理衛生		tscs921-社會階層
tscs922-政治文化		tscs931-大眾傳播
tscs932-政治文化		tscs941-文化價值
tscs942-宗教
\nin tscsPhase3
tscs951-綜合問卷		tscs952-家庭、人際
tscs961-家庭		tscs962-東亞比較調查
tscs971_l-社會階層長卷	tscs971_s-社會階層短卷
tscs972-社會網絡與社區	tscs981-大眾傳播
tscs982-政治文化		tscs991-文化價值
tscs992-宗教
\nin tscsPhase4
tscs001-綜合問卷		tscs002-人際、休閒
tscs011-家庭		tscs012-社會問題
tscs013-失業		tscs021-社會階層
tscs022-性別		tscs031-大眾傳播
tscs032-國家認同		tscs041-公民權
tscs042-宗教文化
\nin tscsPhase5
ssm05tw-東亞社會階層與流動	tscs051-綜合
tscs052-工作與生活	tscs061-家庭
tscs062-公民與國家	tscs071-社會階層
tscs072-休閒生活		tscs081大眾傳播
tscs082-全球化與文化	tscs091-社會不平等
tscs092-宗教與文化
\nin tscsPhase6
tscs101-綜合		tscs102-環境
tscs111-家庭		tscs112-健康與醫療照顧
tscs121-社會階層		tscs122-性別
tscs131-社會風險		tscs132-國家認同
tscs141-公民權		tscs142-宗教與文化
\nin tscsPhase7
tscs151-綜合		tscs152-生活與工作
tscs161-家庭		tscs162-公民與國家
tscs171-社會階層		tscs172-社會網絡
tscs181-宗教與文化")
}
