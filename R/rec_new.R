#' \code{rec.new} YC style recode
#'
#' #' Same as \code{\link{rec_new}}
#'
#' @param vec vector, function can recode four type vector: numeric,factor,character,logical,
#' @param rec recode rule. example:"1,2,4=1;5:9=2;10=NA;else=copy"
#' @param type character. What new recoded vector type you want. "n":numeric. "f":factor. "c":character. "l":logical.
#' @param ref it works only when type=="f". You can chose which factor level being reference group.
#' @param level it works only when type=="f". You can rerank  factor levels order.
#' @param labels it works only when type=="f". Recording recode rule.
#' @param keep_vl default=F. Keep value.labels.
#'
#' @return vector
#' @export rec.new
rec.new <- function(vec=NULL,rec=NULL,type=c("n","f","c","l"),ref=NULL,level=NULL,labels=T,keep_vl=F){
	if(is.null(vec)|is.null(vec))stop()
	type <- type[1]
	if(class(vec)=="factor")vec <- as.character(vec)
	temp.vec<- switch(type,
					  n=as.numeric(rec(vec,rec=rec,as.num = T)),
					  f=as.factor(rec(vec,rec=rec,as.num = F)),
					  c=as.character(rec(vec,rec=rec,as.num = T)),
					  l=logical(rec(vec,rec=rec,as.num = T)),
					  NA)
	if(!is.null(level)){
		temp.vec <- factor(temp.vec,levels = level)
	}
	if(!is.null(ref)){
		temp.vec <- factor(temp.vec,levels = unique(c(ref,levels(temp.vec))))
	}
	for (i in names(attributes(vec))){
		if(grepl("table",i)){
			attr(temp.vec,"old.table") <- attr(vec,i)
			next
		}
		attr(temp.vec,i) <- attr(vec,i)
	}
	if(labels){
		attr(temp.vec,"new_labels") <- paste0(rec,"   class:",class(temp.vec))
	}
	attr(temp.vec,"table") <- table(temp.vec,useNA = "always")
	if(!keep_vl)attr(temp.vec,"value.labels") <- NULL
	return(temp.vec)
}

#' \code{rec_new} YC style recode
#'
#' Same as \code{\link{rec.new}}
#'
#' @param vec vector, function can recode four type vector: numeric,factor,character,logical,
#' @param rec recode rule. example:"1,2,4=1;5:9=2;10=NA;else=copy"
#' @param type character. What new recoded vector type you want. "n":numeric. "f":factor. "c":character. "l":logical.
#' @param ref it works only when type=="f". You can chose which factor level being reference group.
#' @param level it works only when type=="f". You can rerank  factor levels order.
#' @param labels it works only when type=="f". Recording recode rule.
#' @param keep_vl default=F. Keep value.labels.
#'
#' @return vector
#' @export
rec_new <- function(vec=NULL,rec=NULL,type=c("n","f","c","l"),ref=NULL,level=NULL,labels=T,keep_vl=F){
	if(is.null(vec)|is.null(vec))stop()
	type <- type[1]
	if(class(vec)=="factor")vec <- as.character(vec)
	temp.vec<- switch(type,
					  n=as.numeric(rec(vec,rec=rec,as.num = T)),
					  f=as.factor(rec(vec,rec=rec,as.num = F)),
					  c=as.character(rec(vec,rec=rec,as.num = T)),
					  l=logical(rec(vec,rec=rec,as.num = T)),
					  NA)
	if(!is.null(level)){
		temp.vec <- factor(temp.vec,levels = level)
	}
	if(!is.null(ref)){
		temp.vec <- factor(temp.vec,levels = unique(c(ref,levels(temp.vec))))
	}
	for (i in names(attributes(vec))){
		if(grepl("table",i)){
			attr(temp.vec,"old.table") <- attr(vec,i)
			next
		}
		attr(temp.vec,i) <- attr(vec,i)
	}
	if(labels){
		attr(temp.vec,"new_labels") <- paste0(rec,"   class:",class(temp.vec))
	}
	attr(temp.vec,"table") <- table(temp.vec,useNA = "always")
	if(!keep_vl)attr(temp.vec,"value.labels") <- NULL

	return(temp.vec)
}


#' \code{rec.dym} recode dummy variables
#'
#' recode factor into dummy variables
#' @param vec nominal variable
#' @param DT Data
#' @param name dummy variables' name
#' @param nominal dependent variable of nominal regression type
#'
#' @return Data
#' @export rec.dym
rec.dym <- function(vec=vec,DT=DT,name="dym",nominal=F){
	if(is.factor(vec)==F)stop("vec must be factor")
	temp.name<- levels(vec)
	for(i in 1:nlevels(vec)){
		DT[[paste0(name,temp.name[i])]] <- as.numeric(vec==temp.name[i])
		if(nominal&i!=1){
			DT[[paste0(name,temp.name[i])]][(DT[[paste0(name,temp.name[i])]]==0)&(DT[[paste0(name,temp.name[1])]]==0)] <- NA
		}
	}
	if(nominal)DT[[paste0(name,temp.name[1])]] <- NULL
	return(DT)

}
