setwd("C:/Users/madman435/Documents/cs 6350/hw4/")
train_dat<-read.csv("train.csv", header=F)
names(train_dat)<-c('variance','skewness','curtosis','entropy','label')
train_dat[train_dat$label==0,'label']<- -1

test_dat<-read.csv("test.csv", header=F)
names(test_dat)<-c('variance','skewness','curtosis','entropy','label')
test_dat[test_dat$label==0,'label']<- -1

dat<-train_dat
y <- as.matrix(dat[,ncol(dat)])
x <- as.matrix(dat[,1:ncol(dat)-1])
a<-as.matrix(sample(c(0), 872, T))
ub<-100/873
#lb<-0


SVM_dual<-function(a){
	return(
		'objective' =( (1/2) * sum( a%*%t(a) * y%*%t(y) * x%*%t(x) ) ) - sum(a)
		'gradient' = 
	#	'objective' = ( (1/2) * sum( x%*%t(x) * y%*%t(y) * a%*%t(a) ) ) - sum(x)
	)
}

#sum(a*y)
eval_g_eq <-  function(a){
	constr <- t(a) %*% y
	return(list("constraints"=constr))
}

lb <- rep(0,length(a))
#lb <- c(0, a)
ub <- rep(ub,length(a))
#ub<- c(a, ub)

local_opts <- list( 
	"algorithm"="NLOPT_LD_LBFGS", 
	"xtol_rel" = 1.0e-7) 

opts <- list(  "algorithm"="NLOPT_LD_LBFGS",
	"xtol_rel" = 1.0e-7, 
	"maxeval" = 1000, 
	"local_opts" = local_opts )

#opts <-	list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel"=1.0e-8)

res <- nloptr( 
	x0=c(a), 
	eval_f=SVM_dual, 
	lb=lb, 
	ub=ub, 
	eval_g_eq=eval_g_eq, 
	opts=opts)
	#x=x,
	#y=y) 
	
	# # print( res )
res <- auglag(
	x0=a, 
	fn=SVM_dual,
	gr=NULL,	
	lower=lb, 
	upper=ub, 
	heq=eval_g_eq, 
	localsolver="LBFGS")
	# # ,
	# # x=x,
	# # y=y) 

##################################
##################################

require(e1071)
setwd("C:/Users/madman435/Documents/cs 6350/hw4/")
train_dat<-read.csv("train.csv", header=F)
names(train_dat)<-c('variance','skewness','curtosis','entropy','label')
train_dat[train_dat$label==0,'label']<- -1

test_dat<-read.csv("test.csv", header=F)
names(test_dat)<-c('variance','skewness','curtosis','entropy','label')
test_dat[test_dat$label==0,'label']<- -1

dat<-train_dat
train_y <- as.matrix(dat[,ncol(dat)])
train_x <- as.matrix(dat[,1:ncol(dat)-1])

dat<-test_dat
test_y <- as.matrix(dat[,ncol(dat)])
test_x <- as.matrix(dat[,1:ncol(dat)-1])
	
C <- list(c(100,873), c(500,873), c(700,873))
gam<- c(0.01, 0.1, 0.5, 1, 2, 5, 10, 100)
for( i in 1:length(C) ){
	for( j in 1:length(gam)){
		train_svm<-svm(train_x, train_y,kernel='radial', type="C-classification", cost=C[[i]][1]/C[[i]][2], gamma=gam[j])
		
		train_pred <- predict(train_svm, train_x)
		train_sum <- table(train_pred, train_y)
		
		test_pred <- predict(train_svm, test_x)
		test_sum <- table(test_pred, test_y)
		
		train_val<-round(sum(train_sum[c(2,3)])/sum(train_sum), digits=5)
		test_val<-round(sum(test_sum[c(2,3)])/sum(test_sum), digits=5)
		cat( "$",C[[i]][1],"/",C[[i]][2],"$ &",gam[j],"&",train_val, "&", test_val, "\\\\ \\hline \n" )
	}
}

##C	
C <- list(c(100,873), c(500,873), c(700,873))
gam<- c(0.01, 0.1, 0.5, 1, 2, 5, 10, 100)
for( i in 1:length(C) ){
	for( j in 1:length(gam)){
		train_svm<-svm(train_x, train_y,kernel='radial', type="C-classification", cost=C[[i]][1]/C[[i]][2], gamma=gam[j])
		
		cat( "$",C[[i]][1],"/",C[[i]][2],"$ &",gam[j],"&",dim(train_svm$SV)[1], "\\\\ \\hline \n" )
	}
}
	
	
##C	
C <- list(c(100,873), c(500,873), c(700,873))
gam<- c(0.01, 0.1, 0.5, 1, 2, 5, 10, 100)
for( i in 1:length(C) ){
	for( j in 1:length(gam)){
		train_svm<-svm(train_x, train_y,kernel='radial', type="C-classification", cost=C[[i]][1]/C[[i]][2], gamma=gam[j])
		
		cat( "$",C[[i]][1],"/",C[[i]][2],"$ &",gam[j],"&",dim(train_svm$SV)[1], "\\\\ \\hline \n" )
	}
}
	

	
	
	
	
	
	
	
	
	
	
