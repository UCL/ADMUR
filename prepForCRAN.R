#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
# Any bits of R code needed at the top level 
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
# to submit to the CRAN:
# https://kalimu.github.io/post/checklist-for-r-package-submission-to-cran/
#----------------------------------------------------------------------------------------------
options(download.file.method = "libcurl")

# 1. check downstream dependencies from code in directory 'dependency checks'
# 2. ensure tarball is < 5MB

# 3. spell checks
devtools::spell_check()

# 4. win builder checks
devtools::check_win_release()
devtools::check_win_devel()

# 5. multi-platform rhub checks.
#  default for check-rhub is R CMD --no-manual, which throws a note. Therefore change check_args to ""
#  default for check-rhub is to run the slow examples (donttest). Therefore change enrionmental variables to '_R_CHECK_DONTTEST_EXAMPLES_' = "false"
devtools::check_rhub(platforms = NULL, check_args = "", env_vars = c('_R_CHECK_DONTTEST_EXAMPLES_' = "false"))

# 6. final submission:
devtools::release()
#----------------------------------------------------------------------------------------------
# search for any specific words or phrases in: .R, .md, .Rmd
#----------------------------------------------------------------------------------------------
pattern <- '\\mjseqn'
pattern <- '\\mjsdeqn'
pattern <- 'Philo'
pattern <- 'svg'
pattern <- 'power law'
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

