#ID3 programs

#Functions to determine best split
info_gain_E<-function(example_set, label_space){
	att_entropys<-c()
	#LOOP THROUGH ALL ATTRIbuTES
	for( h in 1:length(example_set) ){
		(K_labs<-unique(label_space))
		(att_space<-unique(example_set[,h]))
		att_val_ent<-c()
		#LOOP THROUGH EACH ATTRIBUTES UNIQUE VALUES
		for( i in 1:length(att_space) ){
			(space_summary<-label_space[ example_set[,h]==att_space[i] ])
			pys<-c()
			#FIND THE ENTROPY OF EACH ATTRIBUTES VALUES IN REGARDS TO THE LABEL_SPACE
			for( j in 1:length(K_labs) ){
				py<-length(space_summary[ space_summary==K_labs[j] ])/length(space_summary)
				pys[j] <- py*log2(py)
				if(pys[j]=="NaN"){pys[j]<-0}else{}
			}
			(att_val_ent[i]<- -( length(space_summary)/dim(example_set)[1] ) * sum(pys) )
		}
	att_entropys[h]<- sum(att_val_ent)
	}
	names(att_entropys)<-names(example_set)
	cat("\nThe entropy of each attribute is\n")
	print(att_entropys)

	
	#Now calculate the label spaces entropy
	label_sum<-summary(as.factor(label_space))
	label_entropy<-c()
	p<-c()
	for(i in 1:length(label_sum)){
		p[i]<-label_sum[i]/sum(label_sum)
		label_entropy[i]<- ( p[i]*log2(p[i]) )
	}

	label_entropy <- -sum(label_entropy)
	cat("\nThe entropy of the label space is\n")
	print(label_entropy)
	#The information gain of each attribute is
	cat("\n The Information gain of each att is\n")
	information_gain<-label_entropy-att_entropys
	print(information_gain)
}

info_gain_GI<-function(example_set, label_space){
	att_entropys<-c()
	#LOOP THROUGH ALL ATTRIbuTES
	for( h in 1:length(example_set) ){
		(K_labs<-unique(label_space))
		(att_space<-unique(example_set[,h]))
		att_val_ent<-c()
		#LOOP THROUGH EACH ATTRIBUTES UNIQUE VALUES
		for( i in 1:length(att_space) ){
			(space_summary<-label_space[ example_set[,h]==att_space[i] ])
			pys<-c()
			#FIND THE ENTROPY OF EACH ATTRIBUTES VALUES IN REGARDS TO THE LABEL_SPACE
			for( j in 1:length(K_labs) ){
				py<-length(space_summary[ space_summary==K_labs[j] ])/length(space_summary)
				pys[j] <- py^2
			}
			
			(att_val_ent[i]<- ( length(space_summary)/dim(example_set)[1] ) * (1 - sum(pys) ) )
		}
	att_entropys[h]<- sum(att_val_ent)
	}
	names(att_entropys)<-names(example_set)
	
	
	#Now calculate the label spaces entropy
	label_sum<-summary(as.factor(label_space))
	label_entropy<-c()
	p<-c()
	for(i in 1:length(label_sum)){
		p[i]<-label_sum[i]/sum(label_sum)
		label_entropy[i]<- p[i]^2
	}

	label_entropy <- 1-sum(label_entropy)
	cat("\nYour labels entropy is\n")
	print(label_entropy)
	#The information gain of each attribute is
	cat("\n The Information gain of each att is\n")
	information_gain<-label_entropy-att_entropys
	print(information_gain)
}

info_gain_ME<-function(example_set, label_space){
	att_entropys<-c()
	#LOOP THROUGH ALL ATTRIbuTES
	for( h in 1:length(example_set) ){
		#(K_labs<-unique(label_space))
		(att_space<-unique(example_set[,h]))
		att_val_ent<-c()
		#LOOP THROUGH EACH ATTRIBUTES UNIQUE VALUES
		pys<-c()
		for( i in 1:length(att_space) ){
			(space_summary<-label_space[ example_set[,h]==att_space[i] ])
			#min(summary(as.factor(space_summary)))
			#FIND THE ENTROPY OF EACH ATTRIBUTES VALUES IN REGARDS TO THE LABEL_SPACE
			(pys[i]<-1-(max(summary( as.factor(space_summary)) )/length(space_summary)) )
			if(pys[i]==1){pys[i]=0}
			(att_val_ent[i]<- ( length(space_summary)/dim(example_set)[1] ) * pys[i] )
		}
	att_entropys[h]<- sum(att_val_ent)
	}
	names(att_entropys)<-names(example_set)
	
	att_entropys<-round(att_entropys, digits=3)
	
	#Now calculate the label spaces entropy
	
	(label_ME<- 1 - max(summary(as.factor(label_space)))/length(label_space) )
	
	cat("\nYour labels Majority Error is\n")
	print(label_ME)
	#The information gain of each attribute is
	cat("\n The Information gain of each att is\n")
	information_gain<-label_ME-att_entropys
	print(information_gain)
}

#Tree Algorithm
#S= Example space as a data.frame
#att= atribute names of the label space
#info_gain_func= name of the function to use, out of E, GI, or ME

ID3<-function(S, att, lab, node, branch="NA", info_gain_func="E"){
	#if( !library(data.tree, logical.return=T) ){install.packages('data.tree')}
	info_gain<-get( paste0("info_gain_", info_gain_func) )
	#if all examples have same label 
	(label_summary<-summary(as.factor(S[,lab])))
	
	#if( length(att) == 0 ){
	#	new_lab<-max( summary( S[,lab] ) )
	#	child<-node$Add         Child( new_lab )
	#	print(child)
	#}
	
	if( length(label_summary)<2 ){
		print("UGH")
		print(branch)
		(node$AddChild( names(label_summary) , branch=as.character(branch)))
		#print(child)
	}else{
		#1.
		#calculate info_gain
		att_ig<-info_gain( S[att], S[,lab] )
		#print(att_ig)
		#now grab the name with highest gain
		##print(A)
		( A <- sample( names(att_ig[att_ig==max(att_ig)[1]]) )[1] )
		#this is our root node in the main tree
		#print(branch)
		child<-node$AddChild(A, branch=as.character(branch), info_gain=att_ig[att_ig==max(att_ig)[1]][1] )
		#child$branch<-branch

		#2
		#now find all branches of the root node and calculate best next split based on 
		(branches<-unique(S[,A]))
		for( i in 1:length(branches) ){
			print(branches[i])
			#( branch<-branches[i] )
			( Sv <- S[ S[,A]==branches[i], ] )
				
			if( dim(Sv)[1]==0 ){
				label_to_add<-S[1 ,'label']
				child<-node$AddChild(label_to_add, branch = as.character(branches[i]) )
			}else{
				(A_new<-setdiff( att, A ))
				print(A_new)
				ID3(Sv, A_new ,lab, child, as.character(branches[i]), info_gain_func)
			}
		}
	}
#print(node, 'branch')
return(node)
}

###############
#Function to make tree pretty
GetEdgeLabel <- function(node) {
  if (!node$isRoot && node$branch!="NA") {
    label = paste0(node$branch)
  } else {
    label = ""
  }
  return (label)
}

GetNodeLabel <-function(node) {
	if( is.numeric(node$info_gain) ){
		label=paste0(node$name,"=", round(as.numeric(node$info_gain), digits=3))
	}else{
		label=node$name
	}
	return(label)
}



