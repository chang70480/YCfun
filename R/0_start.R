#' YC package start
#'
#' @return NULL
#' @export
#'
#' @examples YCstart()
#' @import sjmisc
#' @import tibble
#' @import readr
#' @import readxl
#' @import magrittr
#' @import dplyr
#' @import foreign
#' @import readstata13
YCstart <- function(){
	ch <- NULL
	for (i in c("sjmisc","tibble","readr","readxl","magrittr","dplyr","foreign","readstata13")) {
		if(! i %in%rownames(installed.packages())){
			install.packages(i)
			ch <- T
		}
	}
	library(sjmisc)
	library(tibble)
	library(readr)
	library(readxl)
	library(magrittr)
	library(dplyr)
	library(foreign)
	library(readstata13)
	if(is.null(ch))cat("\nall have been installed!")
}

