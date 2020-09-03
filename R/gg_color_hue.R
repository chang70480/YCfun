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
