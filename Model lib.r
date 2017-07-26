# clean data

# first to check the correlations between variables
n_predictors = dim(movie_data2[,select_columns[-1]])[2]
cor_matrix = matrix(0, n_predictors, n_predictors)
for (i in seq(n_predictors)){
  for (j in seq(n_predictors)){
    cor_matrix[i,j] = cor.test(as.numeric(movie_data2[,i+1]), as.numeric(movie_data2[,j+1]))$est    
  }
}
index = which(cor_matrix>0.7) # high correlation predictors
row_matrix = matrix(rep(seq(n_predictors), times=n_predictors), n_predictors, n_predictors)
column_matrix = matrix(rep(seq(n_predictors), each=n_predictors), n_predictors, n_predictors)
row_id = row_matrix[index]
column_id = column_matrix[index]
row_id
column_id


#
