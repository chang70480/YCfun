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
	for (i in c("sjmisc","tibble","readr","readxl","magrittr","dplyr","foreign","readstata13")) {
		if(! i %in%rownames(installed.packages()))install.packages(i)
	}
	library(sjmisc)
	library(tibble)
	library(readr)
	library(readxl)
	library(magrittr)
	library(dplyr)
	library(foreign)
	library(readstata13)
}

