setwd("C:/Users/madman435/Documents/cs 6350/hw2/LMS/concrete/")

train_dat<-read.csv('train.csv', header=F)
cnames<-c('Cement',
'Slag',
'Fly ash',
'Water',
'SP',
'Coarse Aggr',
'Fine Aggr',
'label')
colnames(train_dat)<-cnames
test_dat<-read.csv('test.csv', header=F)
cnames<-c('Cement',
'Slag',
'Fly ash',
'Water',
'SP',
'Coarse Aggr',
'Fine Aggr',
'label')
colnames(test_dat)<-cnames


##################################################
dat<-train_dat
label<-dat['label']
dat_att<-dat[1:(ncol(dat)-1)]
(r<-round(seq(.014, .005, length.out=8), digits=3))
norm_watchers_list<-list()
norm_watcher<-c()
dev.new(width=5, height=10)
par(mfrow=c(4,2), bty='l')
for(k in 1:length(r)){
wt<-rep(0,ncol(dat)-1)

print(k)
	for(j in 1:20){
		sum_val<-rep(0,ncol(dat)-1)
		for(i in 1:dim(dat)[1]){
			sum_val1<- -( label[i,] - (as.matrix(dat_att[i,])%*%wt) ) * dat_att[i,]
			sum_val<-(sum_val+sum_val1)
			#print(sum_val)
		}
		sum_val<-as.numeric(sum_val)
		wt1<-wt-(r[k]*sum_val)
		norm_watcher[j]<-norm(as.matrix(wt-wt1))		
		wt<-wt1
	}
	print(wt)
norm_watchers_list[[k]]<-norm_watcher
plot(norm_watchers_list[[k]], main=paste("r=",r[k]), type='l', lwd=3)
}
##################################################
#The cost fucntion should be .014
dat<-train_dat
label<-dat['label']
dat_att<-dat[1:(ncol(dat)-1)]
r<-0.014
wt<-rep(0,ncol(dat)-1)

for(j in 1:20){
	sum_val<-rep(0,ncol(dat)-1)
	for(i in 1:dim(dat)[1]){
		sum_val1<- -( label[i,] - (as.matrix(dat_att[i,])%*%wt) ) * dat_att[i,]
		sum_val<-(sum_val+sum_val1)
		#print(sum_val)
	}
	sum_val<- -as.numeric(sum_val)
	wt1<-wt-(r*sum_val)
	norm_watcher[j]<-norm(as.matrix(wt-wt1))		
	wt<-wt1
}

print(wt)
#########################################################################################
##################################################
dat<-train_dat
label<-dat['label']
dat_att<-dat[1:(ncol(dat)-1)]
(r<-round(seq(.014, .005, length.out=8), digits=3))
(r<-seq(1e-1, 1e-8, length.out=8))

r<-c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8)

norm_watchers_list<-list()
norm_watcher<-c()
dev.new(width=5, height=10)
par(mfrow=c(4,2), bty='l')
for(k in 1:length(r)){
	wt<-rep(0,ncol(dat)-1)
	print(k)
	costs<-c()
	for(j in 1:500){
		s<-sample(dim(dat)[1])[1]
		wt<- as.numeric(wt+( 0.01*( label[s,] - (as.matrix(dat_att[s,])%*%wt) ) * dat_att[s,] ) )
		sum_val<-rep(0,ncol(dat)-1)
		for(i in 1:dim(dat)[1]){
			sum_val1<- -( label[i,] - (as.matrix(dat_att[i,])%*%wt) ) * dat_att[i,]
			sum_val<-(sum_val+sum_val1)
		}
		costs[j]<-norm(as.matrix(sum_val))
	}
norm_watchers_list[[k]]<-costs
plot(norm_watchers_list[[k]], main=paste("r=",r[k]), type='l', lwd=3)
}	
	
	

dat<-as.matrix(dat)
(t(dat[,1:7]) %*% dat[,1:7])^(-1) %*% t(dat[,8] %*% dat[,1:7])





















for(j in 1:dim(dat_samp)[1]){

	for( i in 1:dim(dat_samp)[2]-1){
		wt[i] <- wt[i] + r * (dat_samp[j,'y'] - (wt[i] * dat_samp[j,i])) * dat_samp[j,i]
	}
print(wt)
}

for(i in 1:dim(dat_samp)[1]){
	sum_vals[[i]]<-(dat_samp[i,'y']-(w1*dat_samp[i,1:3]))*dat_samp[i,1:3]
}



(w<-rep(0, 7))
y<-train_dat[1,'label']
xij<-c()
xi<-train_dat[1,1:7]
for(i in 1:length(w)){
	xij[i]<-w[i]*xi[i]
}

###########################
#Lets caluculate the sample dataset
x1<-c(1,1,-1,1,3)
x2<-c(-1,1,1,2,-1)
x3<-c(2,3,0,-4,-1)
y<-c(1,4,-1,2,0)
dat_samp<-cbind(x1,x2,x3,y)

################
w1=c(0,0,0)
b1=0
sum_vals<-list()
for(i in 1:dim(dat_samp)[1]){
	sum_vals[[i]]<-(dat_samp[i,'y']-(w1*dat_samp[i,1:3]))*dat_samp[i,1:3]
}
-colSums(Reduce(rbind, sum_vals))

sum_vals<-list()
for(i in 1:dim(dat_samp)[1]){
	sum_vals[[i]]<-(dat_samp[i,'y']-(w1*dat_samp[i,1:3]))
}
-colSums(Reduce(rbind, sum_vals))
#################################################


w1=c(-1,1,-1)
b1=-1
sum_vals<-list()
for(i in 1:dim(dat_samp)[1]){
	sum_vals[[i]]<-(dat_samp[i,'y']-(w1*dat_samp[i,1:3]))*dat_samp[i,1:3]
}
-colSums(Reduce(rbind, sum_vals))

sum_vals<-list()
for(i in 1:dim(dat_samp)[1]){
	sum_vals[[i]]<-(dat_samp[i,'y']-(w1*dat_samp[i,1:3]))
}
-colSums(Reduce(rbind, sum_vals))
#################################################

w1=c((1/2),-(1/2),(1/2))
b1=1
sum_vals<-list()
for(i in 1:dim(dat_samp)[1]){
	sum_vals[[i]]<-(dat_samp[i,'y']-(w1*dat_samp[i,1:3]))*dat_samp[i,1:3]
}
-colSums(Reduce(rbind, sum_vals))

sum_vals<-list()
for(i in 1:dim(dat_samp)[1]){
	sum_vals[[i]]<-(dat_samp[i,'y']-(w1*dat_samp[i,1:3]))
}
-colSums(Reduce(rbind, sum_vals))
#################################################



(t(dat_samp[,1:3]) %*% dat_samp[,1:3])^(-1) %*% t(dat_samp[,4] %*% dat_samp[,1:3])

#####################3
#Stochastic gradient descent

wt<-c(0,0,0)
b=0
r=0.1

for(j in 1:dim(dat_samp)[1]){
	for( i in 1:dim(dat_samp)[2]-1){
		wt[i] <- wt[i] + r * (dat_samp[j,'y'] - (wt[i] * dat_samp[j,i]) - b) * dat_samp[j,i]
	}
print(wt)
}































