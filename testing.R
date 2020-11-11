#-----------------------------------------------
# Any bits of R code needed at the top level 
#-----------------------------------------------
options(download.file.method = "libcurl")

# final checks
devtools::check_rhub()

# for final submission:
devtools::release()
#-----------------------------------------------
# search for any specific words or phrases in: .R, .md, .Rmd

man <- list.files('man',full.names=T, pattern='.Rd')
r <- list.files('R',full.names=T, pattern='.R')
vig <- list.files('vignettes',full.names=T, pattern='.Rmd')
files <- c(man,r,vig)
for(n in 1:length(files)){
	x <- readLines(files[n])
	i <- grepl('par(',x, fixed = T)
	bad <- x[i]
	if(length(bad)>0)print(paste(files[n], bad))
	}
#-----------------------------------------------
par(mar=c(5,4,1.5,1.5))


