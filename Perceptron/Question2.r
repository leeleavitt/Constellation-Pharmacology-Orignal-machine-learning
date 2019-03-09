setwd("C:/Users/madman435/Documents/cs 6350/hw3/bank-note/")

train_dat<-read.csv("train.csv", header=F)
names(train_dat)<-c('variance','skewness','curtosis','entropy','label')
train_dat[train_dat$label==0,'label']<- -1

test_dat<-read.csv("test.csv", header=F)
names(test_dat)<-c('variance','skewness','curtosis','entropy','label')
test_dat[test_dat$label==0,'label']<- -1

(w<-rep(0.1,4))
b <- 0
r <- .1
for(j in 1:10){
	true_tracker<-0
	for(i in 1:dim(train_dat)[1]){
		a<-( t(w) %*% t( as.matrix( train_dat[i,1:4] ) ) ) + b
		if( as.numeric(train_dat[i,'label'] * a) <= 0 ){
			w<- as.numeric( w + r * train_dat[i,'label'] * train_dat[i,1:4] )
			b<- b+train_dat[i,'label']
			true_tracker<-true_tracker+1
		}
	}
	#print(true_tracker)

	prediction_error<-0
	for(i in 1:dim(test_dat)[1]){
		w_vec<-t( c(w,b) )
		xi<-t( as.matrix( cbind(test_dat[i,1:4],1) ) )
		
		if(sign( w_vec %*% xi ) != test_dat[i,'label']){
			prediction_error<-prediction_error+1
		}
	}
	print(w)
	print(b)
	print(prediction_error/dim(test_dat)[1]*100)
}

# This function assumes the last collumn is the label space
# It also retruns the w vector with the bias term appended to the end
perceptron<-function(dat, maxIter=10, learn_rate=0.1){
	(w<-rep(0,4))
	b <- 0
	r <- learn_rate
	for(j in 1:maxIter){
		for(i in 1:dim(dat)[1]){
			a<-( t(w) %*% t( as.matrix( dat[ i, (1:ncol(dat)-1) ] ) ) ) + b
			if( as.numeric(dat[i,ncol(dat)] * a) <= 0 ){
				w<- as.numeric( w + r * dat[i,ncol(dat)] * dat[i,(1:ncol(dat)-1)] )
				b<- b+dat[i,ncol(dat)]
			}
		}
	}
	return(c(w,b))
}

#This function returns a calculated success rate for the input dataset
perceptron_test<-function(dat,w){
	prediction_error<-0
	for( i in 1:dim(dat)[1] ){
		w_vec<-t( w )
		xi<-t( as.matrix( cbind( dat[ i, 1:( ncol(dat)-1 ) ], 1 ) ) )
		
		if( sign( w_vec %*% xi ) != dat[ i,ncol(dat) ] ){
			prediction_error<-prediction_error+1
		}
	}
	return( (prediction_error/dim(dat)[1]) * 100)
}

new_w<-perceptron(train_dat, 10, 0.1)
perceptron_test(test_dat,new_w)

###########################################################
###########################################################
# This function assumes the last collumn is the label space
# It also retruns the w vector with the bias term appended to the end
perceptron_voted<-function(dat, maxIter=10, learn_rate=0.1){
	(w<-rep(0,4))
	wm<-list()
	wm[[1]]<-w
	k <- 1
	c_vote<-c()
	c_vote[1]<-0
	b <- 0
	r <- learn_rate
	for(j in 1:maxIter){
		for(i in 1:dim(dat)[1]){
			a<-( t(wm[[k]]) %*% t( as.matrix( dat[ i, (1:ncol(dat)-1) ] ) ) ) + b
			if( as.numeric(dat[i,ncol(dat)] * a) <= 0 ){
				k<-k+1
				wm[[k]]<- as.numeric( wm[[k-1]] + r * dat[i,ncol(dat)] * dat[i,(1:ncol(dat)-1)] )
				#b<- b+dat[i,ncol(dat)]
				c_vote[k] <- 1
			}else{c_vote[k]<-c_vote[k]+1}
		}
	}
	return(list(wm, c_vote))
}

perceptron_voted_prediction<-function(dat, w){
	prediction_error<-0
	for(j in 1:dim(dat)[1]){
		if(j%%100==0){
			print(j)
		}
		s_sum<-c()
		for(i in 1:length(w[[2]]) ){
			s_sum[i]<-w[[2]][i] * sign( as.matrix( dat[j,1:(ncol(dat)-1)] ) %*% t(t(w[[1]][[i]])) )
		}	

		if( sign(sum(s_sum))!=dat[j, ncol(dat)] ){
			prediction_error<-prediction_error+1
		}
	}
print(paste("Your error is = ",	prediction_error/dim(dat)[1]*100))
}

new_w<-perceptron_voted(train_dat, 10, 0.1)
perceptron_voted_prediction(test_dat, new_w)

###########################################################
###########################################################
# This function assumes the last collumn is the label space
# It also retruns the w vector with the bias term appended to the end
perceptron_average<-function(dat, maxIter=10, learn_rate=0.1){
	(w<-rep(0,4))
	b <- 0
	(u<-rep(0,4))
	betA<-0
	ce<-1
	r <- learn_rate
	for(j in 1:maxIter){
		for(i in 1:dim(dat)[1]){
			a<-( t(w) %*% t( as.matrix( dat[ i, (1:ncol(dat)-1) ] ) ) ) + b
			if( as.numeric(dat[i,ncol(dat)] * a) <= 0 ){
				w<- as.numeric( w + r * dat[i,ncol(dat)] * dat[i,(1:ncol(dat)-1)] )
				b<- b+dat[i,ncol(dat)]
				u<- as.numeric( u + r * dat[i,ncol(dat)] * ce * dat[i,(1:ncol(dat)-1)] )
				betA<- betA + dat[i,ncol(dat)] * ce 
			}
			ce<-ce+1
		}
	}
	return(c(w-((1/ce)*u),b-((1/ce)*betA)))
}

#This function returns a calculated success rate for the input dataset
perceptron_test<-function(dat,w){
	prediction_error<-0
	for( i in 1:dim(dat)[1] ){
		w_vec<-t( w )
		xi<-t( as.matrix( cbind( dat[ i, 1:( ncol(dat)-1 ) ], 1 ) ) )
		
		if( sign( w_vec %*% xi ) != dat[ i,ncol(dat) ] ){
			prediction_error<-prediction_error+1
		}
	}
	return( (prediction_error/dim(dat)[1]) * 100)
}

new_w<-perceptron_average(train_dat, 10, 0.1)
perceptron_test(test_dat,new_w)
