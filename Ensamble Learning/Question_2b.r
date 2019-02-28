setwd("C:/Users/madman435/Documents/cs 6350/hw2/Ensamble Learning/")
rm(list=ls())
source("C:\\Users\\madman435\\Documents\\cs 6350\\hw2\\Ensamble Learning\\id3.functions.r")
main_dir<-getwd()
setwd( paste0(main_dir,"/bank") )

#############################################################################
#Import and change the data
#############################################################################
#############################################################################
#Import the training Data
train_dat<-read.csv("train.csv", header=F)
colnames(train_dat)<-c("age", "job", "marital", "education", "default", "balance", "housing", "loan", "contact", "day", "month", "duration", "campaign", "pdays", "previous", "poutcome", "label")

#import the Test data
test_dat<-read.csv("test.csv", header=F)
colnames(test_dat)<-c("age", "job", "marital", "education", "default", "balance", "housing", "loan", "contact", "day", "month", "duration", "campaign", "pdays", "previous", "poutcome", "label")

#now load the functin created
if( ! library(data.tree,logical.return=T) ){ install.packages('data.tree') }

#Convert data to binary
for(i in 1:dim(train_dat)[2]){
	if( class(train_dat[,i])=='integer' | class(train_dat[,i])=='numeric' ){
		med_to_split<-median(train_dat[,i])
		train_dat[train_dat[,i] <= med_to_split, i]<-0
		train_dat[train_dat[,i] >= med_to_split, i]<-1
	}
}
#summary(train_dat)

for(i in 1:dim(test_dat)[2]){
	if( class(test_dat[,i])=='integer' | class(test_dat[,i])=='numeric' ){
		med_to_split<-median(test_dat[,i])
		test_dat[test_dat[,i] <= med_to_split, i]<-0
		test_dat[test_dat[,i] >= med_to_split, i]<-1
	}
}
#summary(test_dat, maxsum=500)
#summary(test_dat,maxsum=500)
################################
#now lets create a randomized dataset with replcement

source("C:\\Users\\madman435\\Documents\\cs 6350\\hw2\\Ensamble Learning\\id3.functions.r")
samp_dat_suc<-c()
train_dat_suc<-c()
test_dat_suc<-c()
for(i in 1:1000){
	if(i%%10==0){
		print(i)
	}
	samp<-sample(seq(1,5000,1), replace=T)
	samp_dat<-train_dat[samp,]
	#for( i in 1:T)
	node<-Node$new("trained_tree")
	ht<-ID3_2(samp_dat, att=names(samp_dat)[c(1:16)], lab=names(samp_dat)[17], node=node, info_gain_func="E", max_depth=16)

	samp_dat_suc[i]<-success_predictor_2(samp_dat, ht, 17)
	train_dat_suc[i]<-success_predictor_2(train_dat, ht, 17)
	test_dat_suc[i]<-success_predictor_2(test_dat, ht, 17)
}

combine_tests<-cbind(samp_dat_suc, train_dat_suc, test_dat_suc)

dense_funcs<-list()
for( i in 1:dim(combine_tests)[2] ){
	dense_funcs[[i]]<-density(combine_tests[,i])
}

graphics.off()
dev.new(width=8, height=8)
par(bg='gray90', bty='l', mfrow=c(2,1))
require(RColorBrewer)
cols<-brewer.pal(3, "Dark2")

plot(combine_tests[,1],combine_tests[,2], type='l', col=cols[1], lwd=2, ylim=c(min(combine_tests[,2:ncol(combine_tests)]), max(combine_tests[,2:ncol(combine_tests)]) ), ylab="Successfull Prediciton", xlab="Tree Iteration", main="Predicted Success vs Tree (1 to 1000)" )
lines(combine_tests[,1],combine_tests[,3], type='l', col=cols[2], lwd=2)
lines(combine_tests[,1],combine_tests[,4], type='l', col=cols[3], lwd=2)
#legend('topleft', c("Sample", "Train", "Test"), lwd=2, col=cols, bty='n')

plot(dense_funcs[[1]], type='l', lwd=2, col=cols[1], main="Distribution of Successes", xlim=c(min(combine_tests[,2:ncol(combine_tests)]), max(combine_tests[,2:ncol(combine_tests)]) ), ylim=c(0, 150) )
lines(dense_funcs[[2]], col=cols[2], lwd=2)
lines(dense_funcs[[3]], col=cols[3], lwd=2)

legend('topleft', c("Sample", "Train", "Test"), lwd=2, col=cols, bty='n')














