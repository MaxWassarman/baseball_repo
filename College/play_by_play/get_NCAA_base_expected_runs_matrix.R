library(tidyverse)

get_expected_runs_matrix=function(base_cd, outs, runs_rest_of_inn){
  ER=data_frame(base_cd,outs,runs_rest_of_inn)%>%
    group_by(base_cd,outs)%>%
    summarize(ERV=round(mean(runs_rest_of_inn),3))%>%
    ungroup()%>%
    mutate(state=paste(base_cd, outs, sep=' '))%>%
    arrange(outs)
  
  ER=matrix(ER$ERV, ncol=3, byrow=TRUE )
  rownames(ER)=c('_ _ _','X _ _','_ X _ ','X X _','_ _ X','X _ X','_ X X','X X X')
  colnames(ER)=c('0','1','2')
  
  
  return(ER)
}

#UPDATED old did not work
get_expected_runs_matrix_2 <- function(base_cd, outs, runs_rest_of_inn) {
  # Create a data frame and remove any NA values
  ER <- tibble(base_cd, outs, runs_rest_of_inn) |>
    filter(!is.na(base_cd), !is.na(outs), !is.na(runs_rest_of_inn))
  
  # Calculate ERV
  ER <- ER |>
    group_by(base_cd, outs) |>
    summarize(
      ERV = mean(runs_rest_of_inn, na.rm = TRUE),
      count = n(),
      .groups = 'drop'
    ) |>
    mutate(
      ERV = round(ERV, 3),
      state = paste(base_cd, outs)
    )
  
  # Create the matrix
  ER_matrix <- matrix(0, nrow = 8, ncol = 3)
  for (i in 1:nrow(ER)) {
    row <- as.numeric(ER$base_cd[i]) + 1
    col <- as.numeric(ER$outs[i]) + 1
    ER_matrix[row, col] <- ER$ERV[i]
  }
  
  rownames(ER_matrix) <- c('_ _ _', '1B _ _', '_ 2B _', '1B 2B _', '_ _ 3B', '1B _ 3B', '_ 2B 3B', '1B 2B 3B')
  colnames(ER_matrix) <- c('0', '1', '2')
  
  return(ER_matrix)
}