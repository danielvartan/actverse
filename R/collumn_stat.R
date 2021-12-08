collumn_stat <- function(log_Data,vector_collumn) {

  Analyze <- log_Data %>% select(vector_collumn)

  Result <- Analyze %>% group_by(Analyze %>% select(vector_collumn[1])) %>% summarise_at(vector_collumn[2:length(vector_collumn)], mean, na.rm = TRUE)

  return(Result)

}
