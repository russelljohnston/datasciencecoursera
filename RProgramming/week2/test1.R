pollutantmean <- function(directory, pollutant, id = 1:332) {
  # print(directory)
  flist <- list.files(directory, full.names=TRUE)  
  dat <- data.frame()
  for (i in id) {
    dat <- rbind(dat, read.csv(flist[i]))
  }
  # mean = mean (dat[pollutant], na.rm = TRUE, dims = 1)
  print(dat[pollutant])  
}





