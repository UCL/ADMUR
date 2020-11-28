#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
# Any bits of R code needed at the top level 
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
# change between dontrun to donttest
#----------------------------------------------------------------------------------------------
# one big frustration is the requirement for a CRAN submission to use donttest in slow examples in the manuals,
# but I can't find a way to prevent travis CI from running this, so the following will do the switch
#----------------------------------------------------------------------------------------------
files <- list.files('man',full.names=T, pattern='.Rd')

# switch to 'dontrun' for normal use
for(n in 1:length(files)){
	x <- readLines(files[n])
	x <- gsub("donttest{", "dontrun{", x, fixed=T)
	writeLines(x,con=files[n])
	}

# switch to 'donttest' for a CRAN submission
for(n in 1:length(files)){
	x <- readLines(files[n])
	x <- gsub("dontrun{", "donttest{", x, fixed=T)
	writeLines(x,con=files[n])
	}
#----------------------------------------------------------------------------------------------
# to submit to the CRAN:
#----------------------------------------------------------------------------------------------
# first ensure manuals are switched to 'donttest'
# then:
options(download.file.method = "libcurl")

# spell checks
devtools::spell_check()

# multi-platform rhub checks
devtools::check_rhub(platforms = NULL)

# win builder checks
https://win-builder.r-project.org/upload.aspx

# for final submission:
devtools::release()
#----------------------------------------------------------------------------------------------
# search for any specific words or phrases in: .R, .md, .Rmd
#----------------------------------------------------------------------------------------------
pattern <- 'Sexpr'
man <- list.files('man',full.names=T, pattern='.Rd')
r <- list.files('R',full.names=T, pattern='.R')
vig <- list.files('vignettes',full.names=T, pattern='.Rmd')
files <- c(man,r,vig)
for(n in 1:length(files)){
	x <- readLines(files[n])
	i <- grepl(pattern,x, fixed = T)
	bad <- x[i]
	if(length(bad)>0)print(paste(files[n], bad))
	}
#----------------------------------------------------------------------------------------------



