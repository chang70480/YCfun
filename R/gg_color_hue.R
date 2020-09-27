#' \code{gg_color_hue} generate ggplot colors.
#'
#' This function can generate html color codes from ggplot color styles.
#'
#' @param n How many color hueps do you want?
#' @param change
#' @return thml  color codes.
#' @export
#' @examples
#' gg_color_hue(5)
#' #F8766D #A3A500 #00BF7D #00B0F6 #E76BF3
gg_color_hue <- function(n,change=list()){
	if(!is.numeric(n)) n <- length(unique(n))
	if(length(names(change))==0 & length(change)>0){
		names(change) <- as.character((n-length(change)+1):n)
	}
	len <- length(change)
	n2 <- n-len
	col <- as.character(1:n)

	col[!((1:n) %in% as.numeric(names(change)))] <- hcl(h = seq(15, 375, length = n2 + 1), l = 65, c = 100)[1:n2]
	col[as.numeric(names(change))] <- unlist(change)
	col
}
