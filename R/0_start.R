#' YC package start
#'
#' @return NULL
#' @export
#'
#' @examples YCstart()
#' @importFrom sjmisc rec
#' @import tibble
#' @import readr
#' @import readxl
#' @import dplyr
#' @import foreign
#' @import readstata13
#' @import magrittr
YCstart <- function(){
	ch <- NULL
	for (i in c("sjmisc","tibble","readr","readxl","magrittr","dplyr","foreign","readstata13")) {
		if(! i %in%rownames(installed.packages())){
			install.packages(i)
			ch <- T
		}
	}
	if(is.null(ch))cat("\nall have been installed!")
}

