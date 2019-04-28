if( !library(caret, logical.return = T) ){install.packages('caret');require(caret)}
if( !library(doParallel, logical.return = T) ){install.packages('doParallel');require(doParallel)}
setwd("Z:/Lee Leavitt/ML experiments/Kate_experiments")

#Load the data
feature_fname <- list.files(pattern = "feature")
feature_space <- read.csv(feature_fname)

label_fname <- list.files(pattern = "label")
label_space <- read.csv(label_fname)

#########################################################
# Basic SVM
#########################################################
label_cn <- select.list( names(label_space), multiple = T)

train_success_rates <- c()
test_success_rates <- c()
train_svm <- list()
train_pred <- list()
test_pred <- list()

plot_dim <- ceiling(sqrt(length(label_cn)))
par( mfrow=c( plot_dim, plot_dim ) )

for( i in 1:length(label_cn) ){
    cat("\nI am going to score the ",label_cn[i],"response \n")
    feature_cn <- grep(label_cn[i], colnames(feature_space), value = T, ignore.case = T)
    total_data <- cbind( feature_space[feature_cn], label_space[ label_cn[i] ]  )
    
    #Lets do a feature decomposition of this
    total_svd <- svd(total_data)
    plot(total_svd$u[,1], total_svd$u[,2], main = label_cn[i] )

    #Randomizer!
    all_rn <- row.names( feature_space )
    train_data_rn <- sample( all_rn, size = length( all_rn )*(2/3) )
    test_data_rn <- setdiff( all_rn, train_data_rn )
    train_data <- total_data[ train_data_rn, ]
    test_data <- total_data[ test_data_rn, ]
    #############################################################

    dat<-train_data
    train_y <- as.matrix(as.factor(dat[,ncol(dat)]) )
    train_x <- as.matrix(dat[,1:ncol(dat)-1])

    dat<-test_data
    test_y <- as.matrix(as.factor( dat[,ncol(dat)] ) )
    test_x <- as.matrix( dat[,1:ncol(dat)-1] )

    #SVM
    grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5))

    train_ctrl <- trainControl(method = 'repeatedcv', number=5, 
                    repeats = 2, allowParallel = T)

    cl <- makePSOCKcluster(8)
    registerDoParallel(cl) 
    train_svm[[ label_cn[i] ]] <- train(x = train_x, y = train_y, method = 'svmLinear',
                    trControl = train_ctrl,
                    preprocess = c('center', 'scale'),
                    tuneGrid = grid,
                    tuneLength = 10)
    stopCluster(cl)

    train_pred[[ label_cn[i] ]] <- predict(train_svm[[i]], train_x)
    print(confusionMatrix(train_pred[[i]], as.factor(train_y) ) )

    test_pred[[ label_cn[i] ]] <- predict(train_svm[[i]], test_x)
    print(confusionMatrix(test_pred[[i]], as.factor(test_y) ) )
}

linear_svm <- list( train_svm = train_svm, train_pred = train_pred, test_pred =test_pred)
save(linear_svm, file="linear_svm.Rdata")





