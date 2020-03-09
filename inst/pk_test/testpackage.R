library(devtools)
library(roxygen2)
roxygenise(clean = T)
document()
load_all(".")

library(YCfun)


##
detach("package:YCfun", unload=TRUE)
remove.packages("YCfun")
#install.packages("~/Google 雲端硬碟/社會系/R/YCfun_0.1.1.tar.gz", repos = NULL, type = "source")



library(devtools)
install_github("chang70480/YCfun",)
?install_github
library(YCfun)

rec_new(1:4,"1,2=0;3,4=1")
rec.new(1:4,"1,2=0;3,4=1")
