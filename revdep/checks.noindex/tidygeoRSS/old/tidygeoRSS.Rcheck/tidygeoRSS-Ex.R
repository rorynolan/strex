pkgname <- "tidygeoRSS"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('tidygeoRSS')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("tidygeo")
### * tidygeo

flush(stderr()); flush(stdout())

### Name: tidygeo
### Title: Extract a tidy data frame from geoRSS, geo-Atom and geoJSON
###   feeds
### Aliases: tidygeo

### ** Examples

## Not run: tidygeo("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.atom")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
