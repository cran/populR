## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----sarea, fig.height = 5, fig.width = 5, fig.align = "center"---------------

# attach library
library(populR)

# load data
data('src')
data('trg')

source <- src
target <- trg

# plot data
plot(source['geometry'], col = "#634B56", border = NA)
plot(target['geometry'], col = "#FD8D3C", add = T)


## ----setup, message=FALSE, warning=FALSE--------------------------------------

# attach libraries 
library(populR)
library(areal)
library(sf)

# load data
data('src')
data('trg')

source <- src
target <- trg

# populR - awi
awi <- pp_estimate(target = target, source = source, spop = pop, sid = sid, 
                   method = awi)
# populR - vwi
vwi <- pp_estimate(target = target, source = source, spop = pop, sid = sid, 
                   volume = floors, method = vwi)

# areal - sum weights
aws <- aw_interpolate(target, tid = tid, source = source, sid = 'sid', 
                      weight = 'sum', output = 'sf', extensive = 'pop')
# areal - total weights
awt <- aw_interpolate(target, tid = tid, source = source, sid = 'sid', 
                      weight = 'total', output = 'sf', extensive = 'pop')

# sf - total weights
sf <- st_interpolate_aw(source['pop'], target, extensive = TRUE)


## -----------------------------------------------------------------------------

# sum initial values
sum(source$pop)

# populR - awi
sum(awi$pp_est)

# populR - vwi
sum(vwi$pp_est)

# areal - awt
sum(awt$pop)

# areal - aws
sum(aws$pop)

# sf
sum(sf$pop)



## -----------------------------------------------------------------------------

# order values using tid
awi <- awi[order(awi$tid),]
vwi <- vwi[order(vwi$tid),]

# get values and create a df
awi_values <- awi$pp_est
vwi_values <- vwi$pp_est

awt_values <- awt$pop
aws_values <- aws$pop

sf_values <- sf$pop

df <- data.frame(vwi = vwi_values, awi = awi_values, aws = aws_values,
                 awt = awt_values, sf = sf_values)

df[1:20,]


## -----------------------------------------------------------------------------

target


## -----------------------------------------------------------------------------
rf <- awi$rf

df <- cbind(rf, df)

df[1:20,]


## ----scatter, fig.height = 7, fig.width = 7.2, fig.align = "center", message=FALSE, warning=FALSE----

awi_error <- pp_compare(df, estimated = awi, actual = rf, title = "awi vs actual")
awi_error

vwi_error <- pp_compare(df, estimated = vwi, actual = rf, title = "vwi vs actual")
vwi_error

sf_error <- pp_compare(df, estimated = sf, actual = rf, title = "sf vs actual")
sf_error

awt_error <- pp_compare(df, estimated = awt, actual = rf, title = "awt vs actual")
awt_error

aws_error <- pp_compare(df, estimated = aws, actual = rf, title = "aws vs actual")
aws_error



## -----------------------------------------------------------------------------

library(microbenchmark)

# performance comparison
microbenchmark(
  suppressWarnings(pp_estimate(target = target, source = source, spop = pop, sid = sid, 
                   method = awi)),
  suppressWarnings(pp_estimate(target = target, source = source, spop = pop, sid = sid, 
                   volume = floors, method = vwi)),
  aw_interpolate(target, tid = tid, source = source, sid = 'sid', 
                      weight = 'sum', output = 'sf', extensive = 'pop'),
  aw_interpolate(target, tid = tid, source = source, sid = 'sid', 
                      weight = 'total', output = 'sf', extensive = 'pop'),
  suppressWarnings(st_interpolate_aw(source['pop'], target, extensive = TRUE))
)



