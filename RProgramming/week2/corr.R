
source("complete.R")
corr <- function(directory, threshold = 0) {
  flist <- list.files(directory, full.names=TRUE)  
  datf = complete(directory)
  idth = datf[datf["nobs"] > threshold, ]$id
  cc=numeric(length(idth))
  ii=0
  corrr = numeric()
  for (i in idth) {
    ii=ii+1
    dat <- data.frame()
    dat <- rbind(dat, read.csv(flist[i]))
    dat = na.omit(dat)
    cc[ii]=cor(dat[2], dat[3])
  }
  return(cc)
}



