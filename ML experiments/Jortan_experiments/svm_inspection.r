setwd("./Jortan_experiments")
load("linear_svm.Rdata")

#FINDING THE W VECTOR
apps <- names(linear_svm$train_svm)
w_vec <- list()
for( i in 1:length(apps) ){
    lin_svm <- linear_svm$train_svm[[ apps[i] ]]$finalModel
    w_vec[[ apps[i] ]] <- lin_svm@coef[[1]]%*%lin_svm@xmatrix[[1]]
}

print(w_vec)
