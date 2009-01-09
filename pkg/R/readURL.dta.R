`readURL.dta` <-
function(url){
tmp <- tempfile(pattern = "file", tmpdir = tempdir())
on.exit(file.remove(tmp))
download.file(url,tmp,mode="wb",quiet=T)
DF <- foreign:::read.dta(tmp)
##DF <- read.dta(tmp)
}

