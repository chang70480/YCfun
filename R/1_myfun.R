#' \code{se} standard error
#'
#' get standard error
#' @param numeric  an numeric vector
#'
#' @return standard error
#' @export
se <- function(numeric=NULL){
	return(sd(numeric,na.rm = T)/sqrt(length(na.omit(numeric))))
}


#' \code{regpred} predict order reg
#'
#' predict marginal effect in order regression
#'
#' @param model order regression result
#' @param at parameter
#' @param level_name order level name
#' @param factor_list factor in regresion
#'
#' @return dataframe
#' @export
regpred <- function(model=model,at=list(),level_name=as.character(),factor_list=NULL){
    temp_ <- apply(na.omit(model$model[-1]),2,mean)
    predDT <- as_tibble(matrix(temp_,length(at[[1]]),length(temp_),byrow = T))
    colnames(predDT) <- names(temp_)
    for(i in names(at)){
        predDT[i] <- at[[i]]
    }

    if(is.null(factor_list)) {
    	if(class(model)[1]=="lm"){
    		p <- predict(model,newdata = predDT) %>%
    			as_tibble() %>% bind_cols(as_tibble(at))
    	}else{
    		p <- predict(model,newdata = predDT,type =ifelse(class(model)[1]=="glm","response","probs")) %>%
    			as_tibble() %>% bind_cols(as_tibble(at))
    	}

    }else if(class(model)[1]=="lm"){ p <- predict(model,newdata = predDT) %>% as_tibble() %>% bind_cols(as_tibble(factor_list))
    }else{
    	p <- predict(model,newdata = predDT,type = ifelse(class(model)[1]=="glm","response","probs")) %>% as_tibble() %>% bind_cols(as_tibble(factor_list))
    }

    names(p)[1:length(level_name)] <- as.character(level_name)
    p <- gather(level_name,key = "Y level",value = "Probability",data = p)
    p$`Y level` <- factor(p$`Y level`,levels = level_name)
        return(p)
}

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




#' \code{nan} Count NA
#'
#' @param vec vector
#'
#' @return table list
#' @export
nan <- function(vec){
	nas <- sum(is.na(vec))
	return(c("All"=length(vec),nNA=length(vec)-nas,"NA"=nas))
}



#' \code{check_unique} check all vlaue is unique
#'
#' check all vlaue is unique
#' @param vec vector
#'
#' @return vector
#' @export
check_unique <- function(vec=NULL){
	return(!(vec %in% vec[duplicated(vec)]))
}


#' \code{select_max} Find most frequence
#'
#' select value appearing most frequence in vector
#'
#' @param vec vector
#' @param rank select frequence rank, default=1
#' @param na.rm omits NA
#' @param na.replace NA recode to ?
#' @param check0 if length==0,stop
#'
#' @return 1 length vector
#' @export
#' @examples
#' select_max(c(2,2,2,3,3,4,4,4,4,5))
#' #5
select_max <- function(vec,rank=1,na.rm='no',na.replace=NA,check0=T){
	if(check0){
		if(length(vec)==0)stop()
	}
	cls <- class(vec)[1]
	temp <- vec %>% table(useNA = na.rm) %>% sort(decreasing = T) %>% .[rank] %>% names
	temp <- ifelse(is.null(temp),na.replace,temp)
	switch(cls,
		   character = return(as.character(temp)),
		   numeric = return(as.numeric(temp)),
		   integer = return(as.integer(temp)),
		   logical =return(as.logical(temp)),
		   factor = return(factor(temp,levels = levels(vec))),
		   Date = return(ymd(temp)),
		   POSIXct = return(ymd_hms(temp))
	)
}


#' list duplicated data
#'
#' @param DT_ data_frame
#' @param ... comapring variable
#'
#' @return data_frame. duplicated data
#' @export
all_duplicate <- function(DT_,...){
	DT2_<- DT_ %>% select(...)
	DT_ %>% filter(duplicated(DT2_) | duplicated(DT2_, fromLast=TRUE)) %>% return()
}


#' \code{get_position} get position
#'
#' get index value
#'
#' @param vec vector
#' @param search conditoin
#' @param ret1 return all or first one, default is first one.
#'
#' @return index vector
#' @export
get_position <- function(vec,search,ret1=1){
	ret <- 1:length(vec)[(!is.na(vec))&(vec==search)]
	if(is.null(ret1)!=T){
		return(ret[ret1])
	}else{
		return(ret)
		}

}



#' \code{gg_color_hue} generate ggplot colors.
#'
#' This function can generate html color codes from ggplot color styles.
#'
#' @param n How many color hueps do you want?
#'
#' @return thml  color codes.
#' @export
#' @examples
#' gg_color_hue(5)
#' #F8766D #A3A500 #00BF7D #00B0F6 #E76BF3
gg_color_hue <- function(n) {
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]
}
