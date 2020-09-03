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
