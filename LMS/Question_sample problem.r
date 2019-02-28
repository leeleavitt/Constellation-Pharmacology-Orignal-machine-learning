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































