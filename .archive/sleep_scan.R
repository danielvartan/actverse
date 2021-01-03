# Load packages --------------------
library(tidyverse)
library(devtools)
library(usethis)


#funcao que retorna o log
logName <- function(logFile) {
    read.table (
        logFile,
        header = T,
        sep = ";",
        stringsAsFactors = FALSE,
        as.is = TRUE )
}

#funcao que seleciona os dias com base no state selecionado (0-2)
sleep_select <- function(x) {
    if (x < 3 && x >= 0) {
        filter(logTeste,logTeste$STATE == x)
    }
}

#variavel que recebe o log
logTeste <- logName(file.choose())

#transforma a coluna TIME do log para caracter
logTeste$TIME <- as.character(logTeste$TIME)

#funcao que tranforma HH::MM:SS format em decimal
dat<-c(logTeste$TIME)
logTeste$TIME <- sapply(strsplit(dat,":"),
                        function(x) {
                            x <- as.numeric(x)
                            x[1]+x[2]/60+x[3]/3600
                        }
)

#variavel que recebe os dados com base no state
sleep_Scan <- sleep_select(1)









