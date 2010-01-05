
order_sample <- function(data_df)
  {
  # Sorts a data.frame based on the first column
  # which is assumed to be the sample name.

  order_df <- order(data_df[,1])
  data_df <- data_df[order_df,]
  return(data_df)
  }

