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


bagged_trees<-list()
for(j in 1:100){
	if(j%%10==0){
		print(j)
	}
	##Step 1
	samp<-sample(seq(1,1000,1))
	samp_dat<-train_dat[samp,]
	#Step 2 learn 1000 trees
	trees_500<-list()
	start_time<-Sys.time()
	for(i in 1:200){
		if(i%%10==0){
			print(i)
		}
		node<-Node$new("trained_tree")
		trees_500[[i]]<-ID3_2(samp_dat, att=names(samp_dat)[c(1:16)], lab=names(samp_dat)[17], node=node, info_gain_func="E", max_depth=16)
	}
	end_time<-Sys.time()
	print(paste("I made 1000 trees in ",end_time-start_time))
	print(paste("I Have bagged Trees",j, "for you"))
	save(trees_500, file=paste("trees_500",j,".rdata"))
	rm(trees_500)
	gc()
}


#Now to calcualte the Bias and Variance















