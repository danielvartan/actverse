
circular_summary <- function(start_sleep,finish_sleep){

  hours_S <- c(unlist(start_sleep, use.names=FALSE))
  hours_E <- c(unlist(finish_sleep, use.names=FALSE))

  hours_S.circ <- circular (hours_S,
                            template = "clock24",
                            units = "hours" ,
                            rotation = "clock")

  hours_E.circ <- circular (hours_E,
                            template = "clock24",
                            units = "hours" ,
                            rotation = "clock")

  mean.circ <- mean.circular(hours_S.circ)
  sd.circ <- sd.circular(hours_S.circ)

  meanE.circ <- mean.circular(hours_E.circ)
  sdE.circ <- sd.circular(hours_E.circ)

  result_S_M <- as.numeric(mean.circ) %% 24
  result_S_DP <- as.numeric(sd.circ) %% 24

  result_E_M <- as.numeric(meanE.circ) %% 24
  result_E_DP <- as.numeric(sdE.circ) %% 24


  print("INICIO DO SONO (MEDIA) :")
  print(hms(times(result_S_M/24)))

  print("INICIO DO SONO (DESVIO PADRAO) :")
  print(hms(times(result_S_DP/24)))


  print("FIM DO SONO (MEDIA) :")
  print(hms(times(result_E_M/24)))

  print("FIM DO SONO (DESVIO PADRAO) :")
  print(hms(times(result_E_DP/24)))


  data_summary <- list(result_S_M,result_S_DP,result_E_M,result_E_DP)

  data_summary <- as.data.frame(do.call(cbind, data_summary))

  return(data_summary)
}
