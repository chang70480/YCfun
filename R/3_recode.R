#' rescale numeric range
#'
#' \code{rec_range}
#' @param nuc.vec an numeric vector
#' @param to.min rescale max value
#' @param to.max rescale min value
#' @param dot round to N'th decimal place
#'
#' @return the rescale numeric vector
#' @export
rec_range<- function(nuc.vec=NULL,to.min=NULL,to.max=NULL,dot=0){
	if((is.numeric(nuc.vec)&is.numeric(nuc.vec)&is.numeric(nuc.vec))==F)
		temp.nuc <- nuc.vec-min(nuc.vec,na.rm = T)
	temp.nuc <- temp.nuc*((to.max-to.min)/max(temp.nuc,na.rm = T))
	temp.nuc <- temp.nuc+to.min
	return(round(temp.nuc,dot))
}


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
#'
#' @return vector
#' @export rec.new
rec.new <- function(vec=NULL,rec=NULL,type=c("n","f","c","l"),ref=NULL,level=NULL,labels=T){
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
#'
#' @return vector
#' @export
rec_new <- function(vec=NULL,rec=NULL,type=c("n","f","c","l"),ref=NULL,level=NULL,labels=T){
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
	if(is.factor(vec)==F)stop()
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

#' Title recode NA in vector
#'
#' recode NA into ???
#' @param vec vector
#' @param to recode into ?
#'
#' @return vector
nato <- function(vec,to){
	vec[is.na(vec)] <- to
	return(vec)
}


#' Title recode NA in vector
#'
#' recode NA into ???
#' @param vec vector
#' @param to recode into ?
#'
#' @return vector
#' @export
nato <- function(vec,to){
	vec[is.na(vec)] <- to
	return(vec)
}

#' Title high efficiency recode
#'
#' when recode rules are more than 10000, this function is more efficienct
#' @param recordtab data_frame. The table of the recode rule. V1 is old value; V2 is new value
#' @param old vector. recoded aims
#' @param quickNA efficienct option
#'
#' @return recoded vector
#' @export
change_posi <- function(recordtab,old=NULL,quickNA=T){
	if(quickNA){
		old[!(old %in% recordtab[[1]])] <- NA
	}
	recordtab[[1]] <- as.character(recordtab[[1]])
	NAr <- data_frame(V1="NA",V2=NA)
	names(NAr) <- names(recordtab)
	recordtab <- bind_rows(recordtab,NAr)

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
