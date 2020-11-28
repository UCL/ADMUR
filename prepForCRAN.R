#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
# Any bits of R code needed at the top level 
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
mathjaxr::preview_rd("convertPars", type = "pdf")
#----------------------------------------------------------------------------------------------
# to submit to the CRAN:
# https://kalimu.github.io/post/checklist-for-r-package-submission-to-cran/
#----------------------------------------------------------------------------------------------
options(download.file.method = "libcurl")

# spell checks
devtools::spell_check()

# multi-platform rhub checks
devtools::check_rhub(platforms = NULL, env_vars = c('_R_CHECK_DONTTEST_EXAMPLES_' = "false"))

# check downstream dependencies from code in directory 'dependency checks'

# win builder checks
devtools::check_win_release()
devtools::check_win_devel()

# for final submission:
devtools::release()

#----------------------------------------------------------------------------------------------
# search for any specific words or phrases in: .R, .md, .Rmd
#----------------------------------------------------------------------------------------------
pattern <- '\\mjseqn'
pattern <- '\\mjsdeqn'
pattern <- 'Philo'
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



