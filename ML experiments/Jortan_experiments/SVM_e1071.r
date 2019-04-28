if( !library(e1071, logical.return = T) ){install.packages('e1071')}

library(Pharming)
setwd("Z:/Lee Leavitt/ML experiments")

feature_fname <- list.files(pattern = "feature")
feature_space <- read.csv(feature_fname)

label_fname <- list.files(pattern = "label")
label_space <- read.csv(label_fname)

#########################################################
# Basic SVM
#########################################################


# Now i want to run 100 cross validation experiments with
#randomly selected rows

label_cn <- select.list( names(label_space), multiple = T)

train_success_rates <- c()
test_success_rates <- c()
tunes_svm <- list()
plot_dim <- ceiling(sqrt(length(label_cn)))
par( mfrow=c( plot_dim, plot_dim ) )
for( i in 1:length(label_cn) ){
    cat("\nI am going to score the ",label_cn[i],"response \n")
    feature_cn <- grep(label_cn[i], colnames(feature_space), value = T, ignore.case = T)
    total_data <- cbind( feature_space[feature_cn], label_space[ label_cn[i] ] )

    #Lets do a feature decomposition of this
    total_svd<-svd(total_data)
    plot(total_svd$u[,1], total_svd$u[,2], main = label_cn[i] )

    #Randomizer!
    all_rn <- row.names( feature_space )
    train_data_rn <- sample( all_rn, size = length( all_rn )*(2/3) )
    test_data_rn <- setdiff( all_rn, train_data_rn )
    train_data <- total_data[ train_data_rn, ]
    test_data <- total_data[ test_data_rn, ]
    #############################################################

    dat<-total_data
    train_y <- as.matrix(dat[,ncol(dat)])
    train_x <- as.matrix(dat[,1:ncol(dat)-1])

    dat<-test_data
    test_y <- as.matrix(dat[,ncol(dat)])
    test_x <- as.matrix(dat[,1:ncol(dat)-1])

    #SVM
    train_svm<-svm(train_x, train_y, type="C-classification")
    print(t(train_svm$coefs)%*%train_svm$SV)

    system.time( tunes_svm[[ label_cn[i] ]] <- tune(svm, train.x = train_x, train.y = train_y, kernel = 'radial', ranges=list(cost=10^(-1:2), gamma=c(.125,.25,.5, 1, 2) ) ) )

    print( tunes_svm[[i]] )

    #print(summary(train_svm))


    train_pred <- predict(train_svm, train_x)
    print(table(train_pred, train_y))
    pred<-table(train_pred, train_y)
    train_success_rates[i] <- sum(pred[1], pred[4]) / sum(pred)*100
    cat("\nYour training success rate is", train_success_rates[i] , "% \n")

    test_pred <- predict(train_svm, test_x)
    print(table(test_pred, test_y))
    pred<-table(test_pred, test_y)
    test_success_rates[i] <- sum(pred[1], pred[4]) / sum(pred)*100
    cat("\nYour testing success rate is", test_success_rates[i] , "% \n")

}
names(train_success_rates) <- label_cn
names(test_success_rates) <- label_cn
print(train_success_rates)
print(test_success_rates)









