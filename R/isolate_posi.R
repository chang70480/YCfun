
#' \code{isolate_posi} find network layout outliers
#'
#' find network layout outliers and move them close center
#'
#' @param l matrix. network plot position
#' @param a recale time
#' @param irq define outliers index
#'
#' @return matrix
#' @export
isolate_posi <- function(l,a=0.7,irq=1.5){
	require(igraph)

	IQR_outliers <- function(x,dis_time=1.5) {
		if(any(is.na(x)))
			stop("x is missing values")
		if(!is.numeric(x))
			stop("x is not numeric")
		Q3<-quantile(x,0.75)
		Q1<-quantile(x,0.25)
		IQR<-(Q3-Q1)
		left<- (Q1-(dis_time*IQR))
		right<- (Q3+(dis_time*IQR))
		return(x <left|x>right)
	}
	xd <- median(l[,1])
	yd <- median(l[,2])
	len <- (apply(l,1,function(i) sum((i-c(xd,yd))^2)^.5))
	posi <- IQR_outliers(len,dis_time = irq)
	if(any(posi)){
		l[posi,] <- apply(matrix(l[posi,],ncol = 2),1,function(i) ((i-c(xd,yd))*(a^2))+c(xd,yd)) %>%t()
	}
	return(l)
}
