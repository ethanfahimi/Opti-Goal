# function to one hot encode variables
one_hot_encode_variable <- function(data, var) {

  # identify unique values of variable
  vals <- glue('{var}{data[,var] %>% unique() %>% as_vector()}')
  
  # create encoded matrix
  encoded_matrix <- model.matrix(as.formula(glue("~ {var}")), data = data)
  encoded_matrix <- encoded_matrix[, -1]
  
  # convert encoded matrix to dataframe
  encoded_data <- as.data.frame(encoded_matrix)

  # add feature for baseline level
  miss_var <- colnames(encoded_data)
  new_col <- setdiff(vals, miss_var)
  encoded_data <- encoded_data %>% mutate(!!sym(new_col) := 1 - rowSums(.))
  
  # end
  return(encoded_data)
}
