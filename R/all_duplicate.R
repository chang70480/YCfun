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
