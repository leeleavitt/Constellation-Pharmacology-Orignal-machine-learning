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
#############################################################################
##########################################################################################################################################################
#The Other thing needed is to convert the label to 1 and -1
train_dat$label<-as.numeric(train_dat$label)
train_dat[train_dat$label==1,'label']<- -1
train_dat[train_dat$label==2,'label']<- 1

test_dat$label<-as.numeric(test_dat$label)
test_dat[test_dat$label==1,'label']<- -1
test_dat[test_dat$label==2,'label']<- 1

#####################################
#Now create Dt for up to 1000
T<-1000
Dt<-rep( 1/dim(train_dat)[1], dim(train_dat)[1] )
source("C:\\Users\\madman435\\Documents\\cs 6350\\hw2\\Ensamble Learning\\id3.functions.r")

at<-c()
et<-c()
test_dat_suc<-c()
train_dat_suc<-c()
ht<-list()
dev.new(width=10, height=5)
for( j in 1:T){
	node<-Node$new("trained_tree")
	ht[[j]]<-ID3_3(train_dat, att=names(train_dat)[c(1:16)], lab=names(train_dat)[17], node=node, info_gain_func="E2", max_depth=2, weight=Dt)
	#Tool to plot
	SetEdgeStyle(ht[[j]] , fontname='helvetica', label=GetEdgeLabel)
	SetNodeStyle(ht[[j]] , shape="box", label=GetNodeLabel)
	plot(ht[[j]])
	#Now calculate the error

	e<-success_predictor_et(train_dat, ht[[j]], 17)
	train_dat_suc[j]<-success_predictor_2(train_dat, ht[[j]], 17)
	test_dat_suc[j]<-success_predictor_2(test_dat, ht[[j]], 17)
	#print(sum(test_dat_suc[i]))
	e<-e[!is.na(e)]
	et[j]<-sum(Dt[e])
	at[j]<-(1/2)*log( (1-et[j])/et[j] )
	print(at[j])
	#now for each value of Dt
	for(i in 1:length(Dt)){
		( ht_xi<-as.numeric(tree_Predict(ht[[j]], train_dat[i,])) )
		(yi<-train_dat[i,'label'])
		Dt[i]<-(Dt[i]/sum(Dt))*exp(-at[j]*yi*ht_xi)
	}
	if(j%%10==0){
		print(j)
		par(mfrow=c(1,3))
		plot(seq(from=1, to=j, by=1), train_dat_suc, main="Train", type='l', col='red', ylim=c(0.8,1))
		plot(seq(from=1, to=j, by=1), test_dat_suc, main="Test",type='l', col='blue',ylim=c(0.8,1))
		plot(seq(from=1, to=j, by=1), at, main="at",type='l', col='blue')		
	}
}




























#for( i in 1:T)
node<-Node$new("trained_tree")
ht<-ID3_3(train_dat, att=names(train_dat)[c(1:16)], lab=names(train_dat)[17], node=node, info_gain_func="E2", max_depth=2, weight=Dt_new)

#Tool to plot
SetEdgeStyle(ht , fontname='helvetica', label=GetEdgeLabel)
##SetNodeLabel(bob, label=GetNodeLabel)
SetNodeStyle(ht , shape="box", label=GetNodeLabel)
plot(ht)
#Now calculate the error



#now for each value of Dt
for(i in 1:length(Dt)){
	( ht_xi<-as.numeric(tree_Predict(ht, train_dat[i,])) )
	(yi<-train_dat[i,'label'])
	Dt_new[i]<-(Dt_new[i]/sum(Dt_new))*exp(-at*yi*ht_xi)
}

#for( i in 1:T)
node<-Node$new("trained_tree")
ht<-ID3_3(train_dat, att=names(train_dat)[c(1:16)], lab=names(train_dat)[17], node=node, info_gain_func="E2", max_depth=2, weight=Dt_new)

#Tool to plot
SetEdgeStyle(ht , fontname='helvetica', label=GetEdgeLabel)
##SetNodeLabel(bob, label=GetNodeLabel)
SetNodeStyle(ht , shape="box", label=GetNodeLabel)
plot(ht)
#Now calculate the error








