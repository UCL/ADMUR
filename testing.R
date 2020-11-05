#-----------------------------------------------
# Any bits of R code needed at the top level 
#-----------------------------------------------
options(download.file.method = "libcurl")

# final checks
devtools::check_rhub()

# for final submission:
devtools::release()
#-----------------------------------------------
