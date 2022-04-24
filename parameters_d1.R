
parameters_d1 <- function(result,loadteste){


  S_tib <- as.character(result$Start_TIB)


  y <- 1
  j <- 1

  t <- length(loadtest[3,])
  h <- vector()
  m <- vector()

  for (i in 1:length(loadtest[2,])){

    if (is.na(STIB[y+1])) {
      h[y] <- sum(loadtest[3,j:t] == 1)
      m[y] <- sum(loadtest[3,j:t] == 2)
      break()
    }


    if (loadtest[2,i] == STIB[y+1]){
      h[y] <- sum(loadtest[3,j:i-1] == 1)
      m[y] <- sum(loadtest[3,j:i-1] == 2)
      j <- i
      y = y + 1
      print(loadtest[2,i])
    }


  }



  return(df1)



}
