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
##########################################################
##########################################################
##########################################################
source("C:\\Users\\madman435\\Documents\\cs 6350\\hw2\\Ensamble Learning\\id3.functions.r")

train_suc_exp<-list()
test_suc_exp<-list()
train_suc_exp_avg<-list()
test_suc_exp_avg<-list()

train_dat_suc<-c()
test_dat_suc<-c()
train_dat_suc_avg<-c()
test_dat_suc_avg<-c()
dev.new(width=8, height=8)
exps<-c(4)
for(j in 1:length(exps)){
	for(i in 1:1000){
		node<-Node$new("trained_tree")
		ht<-ID3_RF(train_dat, att=names(train_dat)[c(1:16)], lab=names(train_dat)[17], node=node, info_gain_func="E", max_depth=16, feature_subset=exps[j])
		
		train_dat_suc[i]<-(1-success_predictor(train_dat, ht, 17))
		train_dat_suc_avg[i]<-sum(train_dat_suc)/i
		
		test_dat_suc[i]<-(1-success_predictor(test_dat, ht, 17))
		test_dat_suc_avg[i]<-sum(test_dat_suc)/i
		if(i%%10==0){
			print(i)
			graphics.off()
			par(bg='gray90', bty='l', mfrow=c(2,2) )
			require(RColorBrewer)
			cols<-brewer.pal(3, "Dark2")
			plot(train_dat_suc~seq(1,i,1), col=cols[1], type='l', lwd=3, ylim=c(0, .3), main="Trained Data Success")
			plot(train_dat_suc_avg~seq(1,i,1), col=cols[1], type='l', lwd=3, main="Trained Data Success")

			plot(test_dat_suc~seq(1,i,1), col=cols[2], type='l', lwd=3, ylim=c(0,.3), main="Tested Data Success")
			plot(test_dat_suc_avg~seq(1,i,1), col=cols[2], type='l', lwd=3, main="Tested Data Success")

			#legend('topleft', c("Train", "Test"), lwd=2, col=cols, bty='n')

		}
	}
	train_suc_exp[[j]]<-train_dat_suc
	test_suc_exp[[j]]<-test_dat_suc
	train_suc_exp_avg[[j]]<-train_dat_suc_avg
	test_suc_exp_avg[[j]]<-test_dat_suc_avg
	alarm()
	scan()
}


















