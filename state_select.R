sleep_select <- function(x,log_Data,type_Data) {

 if (type_Data == "actverse" && x %in% log_Data$state){
  #TRABALHAR NISSO DEPOIS (REVISAO)
  }



  else if (type_Data == "actstudio" && x %in% log_Data$STATE) {


    #CRIA UMA LISTA COM AS VARIAVEIS DATE,TIME,STATE,ZCM,PIM,TAT
    df <- log_Data %>% select (DATE,TIME,STATE,ZCM,PIM,TAT) %>% filter (STATE %in% x)


    result_list <- apply(df[-7], 1, function(x) x [x != ""] )

    return(result_list)

  }

  else
    stop(" type error ")

}
