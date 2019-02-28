#ID3 programs
if( ! library(data.tree,logical.return=T) ){ install.packages('data.tree') }


#Functions to determine best split
info_gain_E<-function(example_set, label_space){
	att_entropys<-c()
	#LOOP THROUGH ALL ATTRIbuTES
	for( h in 1:length(example_set) ){
		#Determine the unique labels within the Label Space
		(K_labs<-unique(label_space))
		#Deermine the unique values in the attributes
		(att_space<-unique(example_set[,h]))
		att_val_ent<-c()
		#LOOP THROUGH EACH ATTRIBUTES UNIQUE VALUES
		for( i in 1:length(att_space) ){
			#Collect subset of labels that coorespond to the attributes
			(space_summary<-label_space[ example_set[,h]==att_space[i] ])
			pys<-c()
			#FIND THE ENTROPY OF EACH ATTRIBUTES VALUES IN REGARDS TO THE LABEL_SPACE
			for( j in 1:length(K_labs) ){
				#Now determine the proportion of values that contain the specified label
				py<-length(space_summary[ space_summary==K_labs[j] ])/length(space_summary)
				#Find the entropy of this proportion
				pys[j] <- py*log2(py)
				if(pys[j]=="NaN"){pys[j]<-0}else{}
			}
			(att_val_ent[i]<- -( length(space_summary)/dim(example_set)[1] ) * sum(pys) )
		}
	att_entropys[h]<- sum(att_val_ent)
	}
	names(att_entropys)<-names(example_set)
	
	#Now calculate the label spaces entropy
	label_sum<-summary(as.factor(label_space))
	label_entropy<-c()
	p<-c()
	for(i in 1:length(label_sum)){
		(p[i]<-label_sum[i]/sum(label_sum))
		(label_entropy[i]<- ( p[i]*log2(p[i]) ))
		if(label_entropy[i]=="NaN"){label_entropy[i]<-0}else{}
	}

	label_entropy <- -sum(label_entropy)
	information_gain<-label_entropy-att_entropys
	return(information_gain)
	print(information_gain)
}
#Functions to determine best split
info_gain_E2<-function(example_set, label_space, weight){
	att_entropys<-c()
	#LOOP THROUGH ALL ATTRIbuTES
	for( h in 1:length(example_set) ){
		#Determine the unique labels within the Label Space
		(K_labs<-unique(label_space))
		#Deermine the unique values in the attributes
		(att_space<-unique(example_set[,h]))
		att_val_ent<-c()
		#LOOP THROUGH EACH ATTRIBUTES UNIQUE VALUES
		for( i in 1:length(att_space) ){
			#Collect subset of labels that coorespond to the attributes
			(space_summary<-label_space[ example_set[,h]==att_space[i] ])
			(weight_summary<-weight[ example_set[,h]==att_space[i] ])
			pys<-c()
			#FIND THE ENTROPY OF EACH ATTRIBUTES VALUES IN REGARDS TO THE LABEL_SPACE
			#for( j in 1:length(K_labs) ){
				#Now determine the proportion of values that contain the specified label
				py<-sum(weight[ space_summary==K_labs[1] ])/sum(weight_summary)
				#Find the entropy of this proportion
				pys[1] <- py*log2(py)
				if(pys[1]=="NaN"){pys[1]<-0}else{}
			#}
			(att_val_ent[i]<- -( length(space_summary)/dim(example_set)[1] ) * sum(pys) )
		}
	att_entropys[h]<- sum(att_val_ent)
	}
	names(att_entropys)<-names(example_set)
	
	#Now calculate the label spaces entropy
	label_sum<-summary(as.factor(label_space))
	lab_unique<-unique(label_space)
	label_entropy<-c()
	p<-c()
	for(i in 1:length(lab_unique)){
		(p[i]<-sum(weight[label_space==lab_unique[i]])/sum(weight))
		(label_entropy[i]<- ( p[i]*log2(p[i]) ))
		if(label_entropy[i]=="NaN"){label_entropy[i]<-0}else{}
	}
	
	label_entropy <- -sum(label_entropy)
	information_gain<-label_entropy-att_entropys
	#print(information_gain)
	return(information_gain)
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
	#cat("\nYour labels entropy is\n")
	#print(label_entropy)
	#The information gain of each attribute is
	#cat("\n The Information gain of each att is\n")
	information_gain<-label_entropy-att_entropys
	#print(information_gain)
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
	
	#cat("\nYour labels Majority Error is\n")
	#print(label_ME)
	#The information gain of each attribute is
	#cat("\n The Information gain of each att is\n")
	information_gain<-label_ME-att_entropys
	#print(information_gain)
}

#Tree Algorithm
#S= Example space as a data.frame
#att= atribute names of the label space
#info_gain_func= name of the function to use, out of E, GI, or ME
ID3<-function(S, att, lab, node, branch="NA", info_gain_func="E", max_depth=3){
	#keep track of depth of tree
	max_depth=max_depth-1
	#obtain the info_gain funciton to use
	(info_gain<-get( paste0("info_gain_", info_gain_func) ))
	(label_summary<-unique(S[,lab]))
	print(label_summary)
	if( length(label_summary)==1 | max_depth==0 ){
		lab_sum<-summary(S[,lab])
		lab_sum1<-sample(names(lab_sum[lab_sum==max(lab_sum)]))[1]
		(node_names<-lab_sum1)
		(node$AddChild( as.character(node_names), branch=''))
	}else{
		(att_ig<-info_gain( S[att], S[,lab] ) )
		(A <- sample(names(att_ig[ att_ig==max(att_ig) ]))[1])
		(branches<-unique(S[,A]))
		#print(branches)
		for( i in 1:length(branches) ){
			(node$branch<-A)
			#print(as.character(branches[i]))
			child<-node$AddChild(as.character(branches[i]), branch='', info_gain=att_ig[att_ig==max(att_ig)[1]][1] )
			( Sv <- S[ S[,A]==branches[i], ] )
			if( dim(Sv)[1]==0 ){
				label_to_add<-S[1 ,'label']
				child<-node$AddChild(as.character(label_to_add), branch = '' )
			}else{
				(A_new<-setdiff( att, A ))
				#print(A_new)
				ID3(Sv, A_new, lab, child, '', info_gain_func, max_depth )
			}
		}
	}
return(node)
}

#info_gain_func= name of the function to use, out of E, GI, or ME
ID3_2<-function(S, att, lab, node, branch="NA", info_gain_func="E", max_depth=3){
	#keep track of depth of tree
	max_depth<-max_depth-1
	#obtain the info_gain funciton to use
	(info_gain<-get( paste0("info_gain_", info_gain_func) ))
	(label_summary<-unique(S[,lab]))
	#print( paste( "You have ", length(label_summary), "labels" ) )
	if( length(label_summary)==1 | max_depth==0 ){
		( lab_sum<-summary(as.factor(S[,lab]) ) )
		( lab_sum1<-sample(names( lab_sum[ lab_sum==max(lab_sum) ] ) )[1] )
		( node_names <- lab_sum1 )
		( node$AddChild( as.character(node_names), branch=branch) )
	}else{
		#print(att)
		if( length(att)==0 ){
		#	print("YOU HAVE NO ATTRIBUTES LEFT")
			( lab_sum<-summary(S[,lab]) )
			( lab_sum1<-sample(names( lab_sum[ lab_sum==max(lab_sum) ] ) )[1] )
			( node_names <- lab_sum1 )
			( node$AddChild( as.character(node_names), branch='') )		
		}else{
			(att_ig<-info_gain( S[att], S[,lab] ) )
			#print(att_ig)
			(A <- sample(names(att_ig[ att_ig==max(att_ig) ]))[1])
			(node$branch<-as.character(A))
			(Vi<-unique(S[,A]))
			#print(branches)
			for( i in 1:length(Vi) ){
				child<-node$AddChild(as.character(Vi[i]), branch='')
				child$info_gain=att_ig[att_ig==max(att_ig)[1]][1] 
				( Sv <- S[ S[,A]==Vi[i], ] )
				if( dim(Sv)[1]==0 ){
					print("YOUR SUBSET IS EMPTY")
					(label_summary<-summary(S[,lab]))
					(max_label_value<-max(label_summary))
					(label_to_add<-label_summary[ label_summary==max_label_value ] )
					(label_to_add_name<-sample( names(label_to_add) )[1] )
					node$AddChild(as.character(label_to_add), branch = '' )
				}else{
					(A_new<-setdiff( att, A ))
					#print(A_new)
					ID3_2(Sv, A_new, lab, child, as.character(A), info_gain_func, max_depth )
				}
			}
		}
	}
return(node)
}

#info_gain_func= name of the function to use, out of E, GI, or ME
ID3_RF<-function(S, att, lab, node, branch="NA", info_gain_func="E", max_depth=3, feature_subset=4){
	if( ! feature_subset>length(att) ){
		att<-sample(att)[1:feature_subset]
	}else{att<-att}
	#keep track of depth of tree
	max_depth<-max_depth-1
	#obtain the info_gain funciton to use
	(info_gain<-get( paste0("info_gain_", info_gain_func) ))
	(label_summary<-unique(S[,lab]))
	#print( paste( "You have ", length(label_summary), "labels" ) )
	if( length(label_summary)==1 | max_depth==0 ){
		( lab_sum<-summary(as.factor(S[,lab]) ) )
		( lab_sum1<-sample(names( lab_sum[ lab_sum==max(lab_sum) ] ) )[1] )
		( node_names <- lab_sum1 )
		( node$AddChild( as.character(node_names), branch=branch) )
	}else{
		#print(att)
		if( length(att)==0 ){
		#	print("YOU HAVE NO ATTRIBUTES LEFT")
			( lab_sum<-summary(S[,lab]) )
			( lab_sum1<-sample(names( lab_sum[ lab_sum==max(lab_sum) ] ) )[1] )
			( node_names <- lab_sum1 )
			( node$AddChild( as.character(node_names), branch='') )		
		}else{
			(att_ig<-info_gain( S[att], S[,lab] ) )
			#print(att_ig)
			(A <- sample(names(att_ig[ att_ig==max(att_ig) ]))[1])
			(node$branch<-as.character(A))
			(Vi<-unique(S[,A]))
			#print(branches)
			for( i in 1:length(Vi) ){
				child<-node$AddChild(as.character(Vi[i]), branch='')
				child$info_gain=att_ig[att_ig==max(att_ig)[1]][1] 
				( Sv <- S[ S[,A]==Vi[i], ] )
				if( dim(Sv)[1]==0 ){
					print("YOUR SUBSET IS EMPTY")
					(label_summary<-summary(S[,lab]))
					(max_label_value<-max(label_summary))
					(label_to_add<-label_summary[ label_summary==max_label_value ] )
					(label_to_add_name<-sample( names(label_to_add) )[1] )
					node$AddChild(as.character(label_to_add), branch = '' )
				}else{
					(A_new<-setdiff( att, A ))
					#print(A_new)
					ID3_RF(Sv, A_new, lab, child, as.character(A), info_gain_func, max_depth, feature_subset)
				}
			}
		}
	}
return(node)
}


#info_gain_func= name of the function to use, out of E, GI, or ME
ID3_3<-function(S, att, lab, node, branch="NA", info_gain_func="E", max_depth=3, weight=weight){
	#keep track of depth of tree
	max_depth<-max_depth-1
	#obtain the info_gain funciton to use
	(info_gain<-get( paste0("info_gain_", info_gain_func) ))
	(label_summary<-unique(S[,lab]))
	#print( paste( "You have ", length(label_summary), "labels" ) )
	if( length(label_summary)==1 | max_depth==0 ){
		( lab_sum<-summary(as.factor(S[,lab]) ) )
		( lab_sum1<-sample(names( lab_sum[ lab_sum==max(lab_sum) ] ) )[1] )
		( node_names <- lab_sum1 )
		( node$AddChild( as.character(node_names), branch=branch) )
	}else{
		#print(att)
		if( length(att)==0 ){
		#	print("YOU HAVE NO ATTRIBUTES LEFT")
			( lab_sum<-summary(S[,lab]) )
			( lab_sum1<-sample(names( lab_sum[ lab_sum==max(lab_sum) ] ) )[1] )
			( node_names <- lab_sum1 )
			( node$AddChild( as.character(node_names), branch='') )		
		}else{
			(att_ig<-info_gain( S[att], S[,lab], weight ) )
			#print(att_ig)
			(A <- sample(names(att_ig[ att_ig==max(att_ig) ]))[1])
			(node$branch<-as.character(A))
			(Vi<-unique(S[,A]))
			#print(branches)
			for( i in 1:length(Vi) ){
				child<-node$AddChild(as.character(Vi[i]), branch='')
				child$info_gain=att_ig[att_ig==max(att_ig)[1]][1] 
				( Sv <- S[ S[,A]==Vi[i], ] )
				weightv<-weight[ S[,A]==Vi[i] ]
				if( dim(Sv)[1]==0 ){
					print("YOUR SUBSET IS EMPTY")
					(label_summary<-summary(S[,lab]))
					(max_label_value<-max(label_summary))
					(label_to_add<-label_summary[ label_summary==max_label_value ] )
					(label_to_add_name<-sample( names(label_to_add) )[1] )
					node$AddChild(as.character(label_to_add), branch = '' )
				}else{
					(A_new<-setdiff( att, A ))
					#print(A_new)
					ID3_3(Sv, A_new, lab, child, as.character(A), info_gain_func, max_depth,weightv )
				}
			}
		}
	}
return(node)
}

#info_gain_func= name of the function to use, out of E, GI, or ME
ID3_4<-function(S, att, lab, node, branch="NA", info_gain_func="E"){
	#keep track of depth of tree
	#obtain the info_gain funciton to use
	(info_gain<-get( paste0("info_gain_", info_gain_func) ))
	(label_summary<-unique(S[,lab]))
	#print( paste( "You have ", length(label_summary), "labels" ) )
	if( length(label_summary)==1){
		( lab_sum<-summary(as.factor(S[,lab]) ) )
		( lab_sum1<-sample(names( lab_sum[ lab_sum==max(lab_sum) ] ) )[1] )
		( node_names <- lab_sum1 )
		( node$AddChild( as.character(node_names), branch=branch) )
	}else{
		#print(att)
		if( length(att)==0 ){
		#	print("YOU HAVE NO ATTRIBUTES LEFT")
			( lab_sum<-summary(S[,lab]) )
			( lab_sum1<-sample(names( lab_sum[ lab_sum==max(lab_sum) ] ) )[1] )
			( node_names <- lab_sum1 )
			( node$AddChild( as.character(node_names), branch='') )		
		}else{
			(att_ig<-info_gain( S[att], S[,lab]) )
			(A <- sample(names(att_ig[ att_ig==max(att_ig) ]))[1])
			(node$branch<-as.character(A))
			(Vi<-unique(S[,A]))
			#print(branches)
			for( i in 1:length(Vi) ){
				child<-node$AddChild(as.character(Vi[i]), branch='')
				child$info_gain=att_ig[att_ig==max(att_ig)[1]][1] 
				( Sv <- S[ S[,A]==Vi[i], ] )
				if( dim(Sv)[1]==0 ){
					print("YOUR SUBSET IS EMPTY")
					(label_summary<-summary(S[,lab]))
					(max_label_value<-max(label_summary))
					(label_to_add<-label_summary[ label_summary==max_label_value ] )
					(label_to_add_name<-sample( names(label_to_add) )[1] )
					node$AddChild(as.character(label_to_add), branch = '' )
				}else{
					(A_new<-setdiff( att, A ))
					#print(A_new)
					ID3_4(Sv, A_new, lab, child, as.character(A), info_gain_func )
				}
			}
		}
	}
return(node)
}



success_predictor<-function(dat, tree, lab_dim=7){
	success_prediction<-0
	for( j in 1:dim(dat)[1]) {
		atts<-dat[j,1:(lab_dim-1)]
		atts_as_char<-c()
		for(i in 1:dim(atts)[2]){
			atts_as_char[i]<-as.character(atts[,i])
			names(atts_as_char)[i]<-names(atts[i])
		}
		#if(j%%1000==0){
		#	print(j)
		#}
		tryCatch(prediction<-tree_Predict(tree, atts_as_char), error=function(e){prediction<-"NA"} ) 
		
		if( prediction==as.character(train_dat[j,lab_dim]) ){
			success_prediction<-success_prediction+1
		}

	}
	print(success_prediction/dim(dat)[1])
}


success_predictor_2<-function(dat, tree, lab_dim=7){
	success_prediction<-0
	for( j in 1:dim(dat)[1]) {
		atts<-dat[j,1:(lab_dim-1)]
		#if(j%%1000==0){
		#	print(j)
		#}
		tryCatch(prediction<-tree_Predict(tree, atts), error=function(e) {prediction<<-'NA'} ) 
		
		if( prediction==as.character(train_dat[j,lab_dim]) ){
			success_prediction<-success_prediction+1
		}

	}
	return(success_prediction/dim(dat)[1])
}


tree_Predict<-function(tree, atts){
	if( tree$children[[1]]$isLeaf ){ return( tree$children[[1]]$name ) }
	child <- tree$children[[ as.character(atts[[ tree$branch ]]) ]]
	return( tree_Predict(child, atts) )
}

success_predictor_et<-function(dat, tree, lab_dim=7){
	success_prediction<-c()
	for( j in 1:dim(dat)[1]) {
		atts<-dat[j,1:(lab_dim-1)]
		#if(j%%1000==0){
		#	print(j)
		#}
		tryCatch(prediction<-tree_Predict(tree, atts), error=function(e){prediction<-"NA"} ) 
		if( prediction!=as.character(train_dat[j,lab_dim]) ){
			success_prediction[j]<-j
		}
	}
	return(success_prediction)
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



