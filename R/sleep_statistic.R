#' Sleep statist
#'
#' @description
#'
#' Algorithm that calculates the main variables derived from the activity
#' log, based on the article 'Actigraphy-Based Assessment of Sleep Parameters'
#' by author Desta Fekedulegn.
#'
#' Algorithm that calculates: Time in Bed (TIB), Sleep Period (O-O Interval),
#' Sleep Minutes during TIB (SMIN), True Sleep Minutes (TSMIN), Percent Sleep
#' (PSLP), Sleep Efficiency (SE), Mean Activity during TIB (AMEAN), Median
#' Activity during TIB (AMED), Activity Standard Deviation during TIB (ASD),
#' Wake up After Sleep Onset (WASO), Latency to Persistent Sleep (LPS) and Wake
#' Minutes during TIB (WMIN).
#'
#' If you want to see more details about this paper, we recommend you to access
#' this link:
#' https://academic.oup.com/annweh/article/64/4/350/5735350?login=false
#'
#' @details
#'
#' This algorithm uses the time circle of the data as a straight "line", aided
#' by the date and time.
#'
#' There are two markers of this line in the algorithm, named "start" and
#' "finish". These markers help to identify the beginning and the end of the
#' sleep period respectively.
#'
#' The "Start" marker has the function of assisting in marking the beginning of
#' sleep, using the "window_time" parameter. This marker will check if the next
#' few minutes (defined by the parameter) are marked as a sleep state. If so, it
#' marks the sleep start time, otherwise it advances to the next minute and
#' checks again.
#'
#' The "Finish" marker has the function to help end sleep, as well as to change
#' days during the sleep period.
#'
#' *Understand change of day in the sleep period as the sleep period that starts
#' on one day and ends on another.
#'
#' Example: starts at 08:00 p.m. and ends at 06:00 a.m.
#'
#' @param analyze A matrix or array containing a vector with dates, vector with
#'   times in the format 'hh:mm:ss', vector with states in the range of (1,2)
#'   being: 1 - Sleep 2 - Resting, a vector with ZCM values, vector with PIM
#'   values and vector with TAT values.
#' @param window_time A string, formated as `hh:mm:ss`, that sets the minimum
#'   continuous value to set sleep. This will be use for calculate Sleep Onset
#'   and Sleep Offset (O-O Interval).
#' @param pkg This parameter defines if the algorithm will calculate Mean and
#'   Standard Deviation of the Circular time Series. This algorithm calculates
#'   the variables "mean" and "Standard Deviation" using the Circular Package.
#'   Note: If you don't need it,use as a value for the 'pkg' as 'N'. Valid
#'   options: `"Y"` and `"N"`.
#'
#' @return A data frame containing the calculated variables:
#'
#' Week Day, Date, Time in Bed (TIB), Sleep Period (O-O Interval), Sleep Minutes
#' during TIB (SMIN), True Sleep Minutes (TSMIN), Percent Sleep (PSLP), Sleep
#' Efficiency (SE), Mean Activity during TIB (AMEAN), Median Activity during TIB
#' (AMED), Activity Standard Deviation during TIB (ASD), Wake up After Sleep
#' Onset (WASO), Latency to Persistent Sleep (LPS), Wake Minutes during TIB
#' (WMIN).
#'
#' @family sleep statistics functions
#' @export
sleep_statistic <- function(analyze, window_time = "00:30:00",
                            pkg = "Y") {

  #Vetores marcadores
  v_time <- c(analyze[2,])
  v_date <- c(analyze[1,])
  v_state <- c(analyze[3,])
  v_state <- as.numeric(v_state)


  #Vetores auxiliadores das analises
  v_ZCM <- as.numeric(c(analyze[4,]))
  v_PIM <- as.numeric(c(analyze[5,]))
  v_TAT <- as.numeric(c(analyze[6,]))



  #Janela de sono definido pelo usuario
  print("SLEEP WINDOW: ")
  print(window_time)
  window_time <- timeToDec(window_time)

  #Converte hora para decimal
  v_time <- timeToDec(v_time)


  #Conjunto de variaveis temporarios/auxiliares
  f <- 1
  t <- 1
  h <- 1
  g <- 1
  p <- 1
  k <- 1
  y <- 1
  j <- 1
  temp <- 1
  occur <- 1


  #Vetores de analises
  start_sleep <- vector()
  finish_sleep <- vector()
  date_sleep <- vector()
  diff_sleep <- vector()
  on_sleep <- vector()
  of_sleep <- vector()
  sle_diff <- vector()
  smin <- vector()
  tsmin <- vector()
  sol <- vector()
  pslp <- vector()
  sleep_ef <- vector()
  AMEAN <- vector()
  AMED <- vector()
  ASD <- vector()
  WASO <- vector()
  LPS <- vector()
  WMIN <- vector()


   #Loops que auxiliam na deteccao de inicio/fim do TIB e O-O Interval
   #junto com os marcadores
   for (i in 1:length(v_time)){

    #Calcula a diferanca do tempo
    difference <- v_time[i] - v_time[i+1]

    #Primeiro elemento
    #Marca o inicio do start
    if (i == 1){
      start_sleep[f] <- v_time[i]
      date_sleep[h] <- v_date[i]
      f = f + 1
      h = h + 1
    }

    #Checa se o proximo elemento e nao nulo
    if (is.na(v_state[i+1]) == FALSE){

      #Inicializa as variaveis temporarios para marcar o TIB e o O-O Interval
      if (i == 1) {

        #Pode ser mudado (Seguindo a janela definida pelo autor/paper)
        #Soma a quantidade de "1" e "2"
        occur <- sum(v_state[i:(i+20)] == 1)
        trueoccur <- sum(v_state[i:(i+20)] == 2)

        #Caso seja valido, marca o inicio do O-O Interval
        if (occur >= 19 && trueoccur <= 1 && v_state[i] == 1 && v_state[i+20] == 1){
          on_sleep[y] <- v_time[i]
          p = i
          y = y + 1
        }

        }

        if (y == t){
        temp = i
        occur <- sum(v_state[temp:(temp+20)] == 1)
        trueoccur <- sum(v_state[temp:(temp+20)] == 2)

        if (occur >= 19 && trueoccur <= 1 && v_state[i] == 1 && v_state[i+20] == 1 ){
          on_sleep[y] <- v_time[i]
          p = i
          y = y + 1
        }
    }

    }
    #Verifica se os marcadores chegaram ao fim da linha temporal
    if (is.na(v_time[i+1]) && is.na(v_date[i+1])){
      temp_state <- vector()
      temp_state <- v_state[1:i]
      vt <- tapply(seq_along(temp_state),temp_state,max)
      of_sleep[j] <- (v_time[vt[1]] + 0.0167)

      #Marca no vetor os valores encontrado para as variaveis
      smin[t] <- sum(v_state[k:i] == 1)
      tsmin[j] <- sum(v_state[p:vt[1]] == 1)
      WASO[j] <- paste(sum(v_state[p:vt[1]] == 2)," Minutes",collapse = " ")
      WMIN[j] <- paste(sum(v_state[k:i] == 2)," Minutes",collapse = " ")
      AMEAN[j] <- round(mean(v_ZCM[k:i]),digits = 2)
      AMED[j] <- round(median(v_ZCM[k:i]),digits = 2)
      ASD[j] <- round(sd(v_ZCM[k:i]),digits = 2)

      finish_sleep[t] <- (v_time[i] + 0.0167)

      print("End of Data")
      break()
    }
    #Verifica se houve mudanca de dia no periodo de sono
    if (v_date[i] == v_date[i+1]){
     if (abs(difference) >= window_time && abs(difference) < (24 - window_time)){
        start_sleep[f] <- v_time[i+1]
        finish_sleep[t] <- (v_time[i] + 0.0167)
        date_sleep[h] <- v_date[i+1]
        temp_state <- vector()
        temp_state <- v_state[1:i]
        vt <- tapply(seq_along(temp_state),temp_state,max)
        of_sleep[j] <- (v_time[vt[1]] + 0.0167)

        #Marca no vetor os valores encontrado para as variaveis
        smin[t] <- sum(v_state[k:i] == 1)
        tsmin[j] <- sum(v_state[p:vt[1]] == 1)
        WASO[j] <- paste(sum(v_state[p:vt[1]] == 2)," Minutes",collapse = " ")
        WMIN[j] <- paste(sum(v_state[k:i] == 2)," Minutes",collapse = " ")
        AMEAN[j] <- round(mean(v_ZCM[k:i]),digits = 2)
        AMED[j] <- round(median(v_ZCM[k:i]),digits = 2)
        ASD[j] <- round(sd(v_ZCM[k:i]),digits = 2)

        k = i + 1
        j = j + 1
        t = t + 1
        f = f + 1
        h = h + 1
      }
   }
    #Verifica se houve mudanca de dia no periodo de sono
    else if (v_date[i] != v_date[i+1]){

      if (abs(difference) <= (24 - window_time)){
        start_sleep[f] <- v_time[i+1]
        finish_sleep[t] <- (v_time[i] + 0.0167)
        date_sleep[h] <- v_date[i+1]
        temp_state <- vector()
        temp_state <- v_state[1:i]
        vt <- tapply(seq_along(temp_state),temp_state,max)
        of_sleep[j] <- (v_time[vt[1]] + 0.0167)


        #Marca no vetor os valores encontrado para as variaveis
        tsmin[j] <- sum(v_state[p:vt[1]] == 1)
        smin[t] <- sum(v_state[k:i] == 1)
        WASO[j] <- paste(sum(v_state[p:vt[1]] == 2)," Minutes", collapse = " ")
        WMIN[j] <- paste(sum(v_state[k:i] == 2)," Minutes",collapse = " ")
        AMEAN[j] <- round(mean(v_ZCM[k:i]),digits = 2)
        AMED[j] <- round(median(v_ZCM[k:i]),digits = 2)
        ASD[j] <- round(sd(v_ZCM[k:i]),digits = 2)

        k = i + 1
        j = j + 1
        t = t + 1
        f = f + 1
        h = h + 1

      } else {
        date_sleep[h-1] <- paste(date_sleep[h-1], "+1" ,collapse = " ")
      }
    }

  }

   for (i in 1:length(start_sleep)){

    if (finish_sleep[i] > start_sleep[i]){
      diff_sleep[i] <- finish_sleep[i] - start_sleep[i]

    }

    if (finish_sleep[i] < start_sleep[i]){

      temp <- 0
      temp <- 23.9997 - start_sleep[i]
      diff_sleep[i] <- finish_sleep[i] + temp
    }
  }

   for (i in 1:length(on_sleep)){

    if (of_sleep[i] > on_sleep[i]){
      sle_diff[i] <- of_sleep[i] - on_sleep[i]
    }

    if (of_sleep[i] < on_sleep[i]){
      temp <- 0
      temp <- 23.9997 - on_sleep[i]
      sle_diff[i] <- of_sleep[i] + temp
    }



   }

   for (i in 1:length(on_sleep)){
     sol[i] <- on_sleep[i] - start_sleep[i]

     pslp[i] <- (smin[i]/(diff_sleep[i]*60))*100
     pslp[i] <- round(as.numeric(pslp[i],digits = 2))
     pslp[i] <- paste(pslp[i],"%",collapse = " ")


     sleep_ef[i] <- (tsmin[i]/(sle_diff[i]*60))*100
     sleep_ef[i] <- round(as.numeric(sleep_ef[i],digits = 2))
     sleep_ef[i] <- paste(sleep_ef[i],"%",collapse = " ")


     LPS[i] <- abs(on_sleep[i] - start_sleep[i])


   }




  #Pacote que auxilia nas analises de horarios circulares (EM REVISAO)
  #if (pkg == 'Y'){

  #hours_S <- c(unlist(start_sleep, use.names=FALSE))
  #hours_E <- c(unlist(finish_sleep, use.names=FALSE))

  #hours_S.circ <- circular (hours_S,
  #                  template = "clock24",
  #                  units = "hours" ,
  #                  rotation = "clock")

  #hours_E.circ <- circular (hours_E,
  #                          template = "clock24",
  #                          units = "hours" ,
  #                          rotation = "clock")

  #mean.circ <- mean.circular(hours_S.circ)
  #sd.circ <- sd.circular(hours_S.circ)

  #meanE.circ <- mean.circular(hours_E.circ)
  #sdE.circ <- sd.circular(hours_E.circ)


  #result_S_M <- as.numeric(mean.circ) %% 24
  #result_S_DP <- as.numeric(sd.circ) %% 24


  #result_E_M <- as.numeric(meanE.circ) %% 24
  #result_E_DP <- as.numeric(sdE.circ) %% 24


  #print("INICIO DO SONO (MEDIA) :")
  #print(hms(times(result_S_M/24)))

  #print("INICIO DO SONO (DESVIO PADRAO) :")
  #print(hms(times(result_S_DP/24)))


  #print("FIM DO SONO (MEDIA) :")
  #print(hms(times(result_E_M/24)))

  #print("FIM DO SONO (DESVIO PADRAO) :")
  #print(hms(times(result_E_DP/24)))


  #data_summary <- list(result_S_M,result_S_DP,result_E_M,result_E_DP)

  #data_summary <- as.data.frame(do.call(cbind, data_summary))

#}


  if(pkg == 'Y'){
   circular_summary(start_sleep,finish_sleep)
  }


  #Cria o Dataframe para mostrar os resultados
  data_analyzed <- list(weekdays(as.Date(date_sleep,"%d/%m/%Y"),
                   abbreviate = FALSE),date_sleep, start_sleep, finish_sleep,
                   diff_sleep,on_sleep,of_sleep,sle_diff,smin,tsmin,sol,pslp,
                   sleep_ef,AMEAN,AMED,ASD,WASO,LPS,WMIN)

  data_analyzed <- as.data.frame(do.call(cbind, data_analyzed))


  data_analyzed <- rename(data_analyzed, WeekDay = V1, Date = V2,
                          Start_TIB = V3, End_TIB = V4, Diff_TIB = V5,
                          Sleep_onset = V6, Sleep_offset = V7,
                          Diff_TSP = V8,SMIN = V9,TSIM = V10,SOL = V11,
                          PSLP = V12,SE = V13,AMEAN = V14, AMED = V15,ASD = V16,
                          WASO = V17,LPS = V18,WMIN = V19)





  #Variaveis que auxiliam na conversao de decimal para TIMES
  temp1 <- as.numeric(data_analyzed$Start_TIB)

  temp2 <- as.numeric(data_analyzed$End_TIB)

  temp3 <- as.numeric(data_analyzed$Diff_TIB)

  temp4 <- as.numeric(data_analyzed$Sleep_onset)

  temp5 <- as.numeric(data_analyzed$Sleep_offset)

  temp6 <- as.numeric(data_analyzed$Diff_TSP)

  temp7 <- as.numeric(data_analyzed$SOL)

  temp8 <- as.numeric(data_analyzed$LPS)


  data_analyzed$Start_TIB<- times(temp1 / 24)

  data_analyzed$End_TIB <- times(temp2 / 24)

  data_analyzed$Diff_TIB <- times(temp3 / 24)

  data_analyzed$Sleep_onset <- times(temp4 / 24)

  data_analyzed$Sleep_offset <- times(temp5 / 24)

  data_analyzed$Diff_TSP <- times (temp6 / 24)

  data_analyzed$SOL <- times (temp7 / 24)

  data_analyzed$LPS <- times (temp8 / 24)



  #Retorna o DataFrame
  return(data_analyzed)

}

collumn_stat <- function(log_Data,vector_collumn) {
    Analyze <- log_Data %>% select(vector_collumn)

    Result <- Analyze %>% group_by(Analyze %>% select(vector_collumn[1])) %>%
        summarise_at(vector_collumn[2:length(vector_collumn)], mean, na.rm = TRUE)

    return(Result)

}

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
