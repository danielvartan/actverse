data_actstudio <- function(mypath) {

  fileNames <- list.files(path = mypath, full.names = TRUE)

  dataList <- lapply(fileNames, function(x)
  {
    read.csv(
      file = x,
      header = T,
      sep = ";",
      dec = ".",
      stringsAsFactors = FALSE
    )
  })


  return(dataList)

}
