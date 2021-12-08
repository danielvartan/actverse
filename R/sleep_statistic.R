

#FUNCAO QUE ANALISA O LOG (ACTSTUDIO)
sleep_statistic <- function(analyze,window_time,pkg,choise) {




  #Vetores iniciais
  v_time <- c(analyze[2,])

  v_date <- c(analyze[1,])

  v_state <- c(analyze[3,])
  v_state <- as.numeric(v_state)



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






   #Loops que auxiliam na deteccao de inicio/fim do TIB e O-O Interval
   for (i in 1:length(v_time)){

    difference <- v_time[i] - v_time[i+1]


    if (i == 1){

      #NEW
      #if (occur >= 30 && trueoccur == 0){
       #   on_sleep[y] <- v_time[i+1]
        #  print(on_sleep[y])
         # y = y + 1
      #}


      start_sleep[f] <- v_time[i]
      date_sleep[h] <- v_date[i]
      f = f + 1
      h = h + 1
    }

    if (is.na(v_state[i+1]) == FALSE){

      #30 -> 20
      if (i == 1) occur <- sum(v_state[1:20] == 1)

      if ((v_state[i] - v_state[i+1] == 0) && (y == t)){

        temp = i+1

        #30 -> 20
        occur <- sum(v_state[temp:(temp+20)] == 1)

        #30 -> 20
        trueoccur <- sum(v_state[temp:(temp+20)] == 2)

        #30 -> 20
        if (occur >= 20 && trueoccur == 0){
          on_sleep[y] <- v_time[i+1]
          p = i + 1
          y = y + 1
        }
    }

    }

    if (is.na(v_time[i+1]) && is.na(v_date[i+1])){

      temp_state <- vector()
      temp_state <- v_state[1:i]
      vt <- tapply(seq_along(temp_state),temp_state,max)

      #NEWW
      of_sleep[j] <- (v_time[vt[1]] + 0.0167)
      #print(vt)


      smin[t] <- sum(v_state[k:i] == 1)
      tsmin[j] <- sum(v_state[p:vt[1]] == 1)
      #print(smin[t])


      finish_sleep[t] <- (v_time[i] + 0.0167)

      print("End of Data")
      break()
    }



    if (v_date[i] == v_date[i+1]){
     if (abs(difference) >= window_time && abs(difference) < (24 - window_time)){

        start_sleep[f] <- v_time[i+1]
        finish_sleep[t] <- (v_time[i] + 0.0167)
        date_sleep[h] <- v_date[i+1]


        temp_state <- vector()
        temp_state <- v_state[1:i]

        vt <- tapply(seq_along(temp_state),temp_state,max)

        #NEWW
        of_sleep[j] <- (v_time[vt[1]] + 0.0167)
        #print(vt)

        smin[t] <- sum(v_state[k:i] == 1)
        tsmin[j] <- sum(v_state[p:vt[1]] == 1)

        #print(smin[t])

        k = i + 1
        j = j + 1
        t = t + 1
        f = f + 1
        h = h + 1
      }
   }

    else if (v_date[i] != v_date[i+1]){

      if (abs(difference) <= (24 - window_time)){

        start_sleep[f] <- v_time[i+1]
        finish_sleep[t] <- (v_time[i] + 0.0167)
        date_sleep[h] <- v_date[i+1]



        temp_state <- vector()
        temp_state <- v_state[1:i]
        vt <- tapply(seq_along(temp_state),temp_state,max)


        #NEWWWW
        of_sleep[j] <- (v_time[vt[1]] + 0.0167)
        #print(vt)


        tsmin[j] <- sum(v_state[p:vt[1]] == 1)
        smin[t] <- sum(v_state[k:i] == 1)
        #print(smin[t])

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


   }






   #Pacote que auxilia nas analises de horarios circulares (EM REVISAO)
   if (pkg == 1){


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



}


   #FUNCAO ERRADA (EM REVISAO)
   if (choise == "Y" || choise == "YES" || choise == "y" || choise == "yes" ){

    k <- as.numeric(readline('How many collumns to analyze ? : '))

    collumns_selected <- vector()

    for (i in 1:k) {

      collumns_selected[i] <- as.character(readline('Enter name collumn: '))

      if (!collumns_selected[i] %in% colnames(analyze)){

        warning("Name collumn not exist, try again")
        k <- k - 1

      }
    }

    collumn_stat(analyze,collumns_selected)

  }




  #Cria o Dataframe para mostrar os resultados
  data_analyzed <- list(weekdays(as.Date(date_sleep,"%d/%m/%Y"),
                   abbreviate = FALSE),date_sleep, start_sleep, finish_sleep,
                   diff_sleep,on_sleep,of_sleep,sle_diff,smin,tsmin,sol,pslp,
                   sleep_ef)

  data_analyzed <- as.data.frame(do.call(cbind, data_analyzed))


  data_analyzed <- rename(data_analyzed, WeekDay = V1, Date = V2,
                          Start_TIB = V3, End_TIB = V4, Diff_TIB = V5,
                          Sleep_onset = V6, Sleep_offset = V7,
                          Diff_TSP = V8,SMIN = V9,TSIM = V10,SOL = V11,
                          PSLP = V12,SE = V13)





  #Variaveis que auxiliam na conversao de decimal para TIMES
  temp1 <- as.numeric(data_analyzed$Start_TIB)

  temp2 <- as.numeric(data_analyzed$End_TIB)

  temp3 <- as.numeric(data_analyzed$Diff_TIB)

  temp4 <- as.numeric(data_analyzed$Sleep_onset)

  temp5 <- as.numeric(data_analyzed$Sleep_offset)

  temp6 <- as.numeric(data_analyzed$Diff_TSP)

  temp7 <- as.numeric(data_analyzed$SOL)


  data_analyzed$Start_TIB<- times(temp1 / 24)

  data_analyzed$End_TIB <- times(temp2 / 24)

  data_analyzed$Diff_TIB <- times(temp3 / 24)
  #data_analyzed$Diff_TIB <- times(temp3*60)

  data_analyzed$Sleep_onset <- times(temp4 / 24)

  data_analyzed$Sleep_offset <- times(temp5 / 24)

  data_analyzed$Diff_TSP <- times (temp6 / 24)

  data_analyzed$SOL <- times (temp7 / 24)



  #Retorna o DataFrame
  return(data_analyzed)

}
