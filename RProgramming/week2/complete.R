complete <- function(directory, id = 1:332) {
  flist <- list.files(directory, full.names=TRUE)  
  nobs <- numeric(length(id))
  ii=0
  for (i in id) {
    ii=ii+1
    dat <- data.frame()
    dat <- rbind(dat, read.csv(flist[i]))
    nobs[ii]= sum(complete.cases(dat))
    # nobs[ii]=length(which(is.na(dat[2])==F))
  }
  out = data.frame(id,nobs)
  return(out)
}


