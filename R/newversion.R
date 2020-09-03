#' Title new version data names
#'
#' @param path file path ,without filename
#' @param filename filename
#' @param nversion new version
#' @param update
#'
#' @return
#' @export
#'
#' @examples
newversion <- function(path,filename,nversion=F,update=T){
	namelist_ <- grep(filename,dir(path),value = T)
	version <- str_sub(namelist_,-9,-7)
	version <- str_split(version,"\\.") %>% unlist() %>% as.numeric()
	if(nversion) version[1] <- version[1]+1
	if(update)   version[2] <- version[2]+1
	paste0(path,"/",filename,"_",paste(version,collapse = "."),".RData")
}
