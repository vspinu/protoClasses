library(devtools)
install(pkg = "~/TVC/devtools/")
install(pkg = "~/TVC/roxygen/")
## install_github("roxygen", "renozao", "s4")

test_file("~/TVC/roxygen/inst/tests/test-rd-s4.R")
setwd("~/works/")
getwd()
library(roxygen2)
options(width = 80)

clear_caches()
system("rm -r -f protoClasses.roxygen")
roxygenize("protoClasses", "protoClasses.roxygen")
check("protoClasses.roxygen")

test_dir("~/works/protoClasses/etc/tests/")
aaa
## roxygen2:::roxygenize("betfairly", "betfairly.system")
## roxygen("R CMD Rd2pdf --output=protoClasses_manual.pdf --force protoClasses.roxygen ")
## system("cp sysdata.rda betfairly.roxygen/R/")
## ## system("rm betfairly.roxygen/R/sysdata.rda")
## ## system("R CMD check betfairly.roxygen")
## ## tools::showNonASCII(readLines("./betfairly/R/betfair.R"))

## system("rm betfairly.roxygen/R/.Rhistory")
system("R CMD build protoClasses.roxygen")

system("R CMD check protoClasses_1.0.tar.gz")

install.packages("betfairly_1.12.tar.gz")
sdf

tenv <- new.env()
setPackageName("mytest",tenv)

sys.source("./protoClasses/R/funcs.R", tenv)
sys.source("./protoClasses/R/classes.R", tenv)
roxygen2:::cleanup_s4(tenv)

sdf
sys.source("./protoClasses/R/test.R", tenv)
getGenerics(tenv)
getClasses(tenv)
## attach(tenv)

for(cls in getClasses(tenv)){
    print(cls)
    removeClass(cls,tenv)
}

removeClass("envProtoClass",tenv)

