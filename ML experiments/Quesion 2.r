setwd("C:/Users/madman435/Documents/cs 6350/hw4/")
train_dat<-read.csv("train.csv", header=F)
names(train_dat)<-c('variance','skewness','curtosis','entropy','label')
train_dat[train_dat$label==0,'label']<- -1

test_dat<-read.csv("test.csv", header=F)
names(test_dat)<-c('variance','skewness','curtosis','entropy','label')
test_dat[test_dat$label==0,'label']<- -1

dat<-train_dat
w <- t(as.matrix( rep(0, dim(dat)[2]-1 ) ))
C <- c(1/873, 10/873, 50/873, 100/873, 300/873, 500/873, 700/873)
N <- dim(dat)[1] #number of examples
T <- 100
gam_t <- 0.01 # c( 0.01, 0.005, 0.0025)
d <- 1
for(k in 1:length(C)){
	gam_t <- 0.01 # c( 0.01, 0.005, 0.0025)
	for( j in 1:T ){
		dat[sample(dim(dat)[1]),]
		for(i in 1:N){
			yi <- dat[ i , ncol(dat) ]
			xi <- dat[ i , 1:(ncol(dat)-1) ]
			#Perform a logical assesment
			logic <- yi * w%*%t(xi)
			if( logic <= 1 ){
				w <- t( ( (1-gam_t) * t(w) ) + gam_t*C[k]*N*yi*t(xi))
			}else{
				w <- t( (1-gam_t) * t(w) )
			}
		}
		gam_t <- gam_t / ( 1 + j )
		#print(gam_t)
		#print(w)
	}
	#now compute the test success
	print(C[k])
	print(paste("W:",w))
	success<-0
	for(i in 1:dim(train_dat)[1]){
		if(sign(w%*%t(train_dat[i,1:ncol(dat)-1]))==train_dat[i,ncol(dat)]){
			success<-success+1
		}
	}
	print(paste("Train Success:",1 - success/dim(train_dat)[1] ))

	success<-0
	for(i in 1:dim(test_dat)[1]){
		if(sign(w%*%t(test_dat[i,1:ncol(dat)-1]))==test_dat[i,ncol(dat)]){
			success<-success+1
		}
	}
	print(paste("Test Success:", 1 - success/dim(test_dat)[1] ))
}

