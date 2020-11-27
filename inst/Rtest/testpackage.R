##renew
{
library(devtools)
library(roxygen2)
	roxygenise(clean = T)
	document()
}
##記得去github存檔
##install
{
library(devtools)
library(YCfun)
detach("package:YCfun", unload=TRUE)
remove.packages("YCfun")
install_github("chang70480/YCfun")

library(YCfun)

}





