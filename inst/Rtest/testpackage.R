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
detach("package:YCfun", unload=TRUE)
remove.packages("YCfun")
install_github("chang70480/YCfun")

library(YCfun)
}



YCstart()

YC
help(package="YCfun")
help(package="dplyr")

?devtools
?YCfun
tscs()

?twomode_onemode
rec_new(1:4,"1,2=0;3,4=1")
rec.new(1:4,"1,2=0;3,4=1")

##

library(rmarkdown)
render("~/Documents/GitHub/YCfun/README.Rmd",md_document())

##





