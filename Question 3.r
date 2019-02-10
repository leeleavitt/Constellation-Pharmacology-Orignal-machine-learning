###Question 3

####################################################
#Attributes are
outlook<-c("ssorrrossrsoor")
temperature<-c("hhhmcccmcmmmhmm")
humidity<-c("hhhhnnnhnnnhnhn")
wind<-c("wswwwsswwwsswsw")

atts<-list(outlook, temperature, humidity, wind)


for(i in 1:length(atts)){
	atts[[i]]<-strsplit(atts[[i]],"")[[1]]
	}
#Outlook is missing
atts[[1]]<-c(atts[[1]],NA)
	
sample_space<-as.data.frame(Reduce(cbind, atts))

label_space<-"001110101111101"
label_space<-as.numeric(strsplit(label_space,"")[[1]])

example_set<-cbind(sample_space, label_space)
cat("\nThis is your example set plus label space\n\n")

names(example_set)<-c('outlook', 'temp', 'humidity', 'wind', 'label')
print(example_set)
######################################################
######################
#1. Use the most common value in the training data as the missing deatures

missing_value_sum<-summary(example_set[,1])
missing_val_rep<-sample(names(missing_value_sum[missing_value_sum==max(missing_value_sum)]))[1]
example_set[is.na(example_set$outlook),1]<-'o'


#now load the functin created
source("C:/Users/madman435/Documents/cs 6350/hw1/id3.functions.r")
node<-Node$new("id3_tree")
id3_tree<-ID3(example_set, c(1:4), c(5), node, "E", info_gain_func="E")
print(id3_tree)

SetEdgeStyle(id3_tree , fontname='helvetica', label=GetEdgeLabel)
#SetNodeLabel(bob, label=GetNodeLabel)
SetNodeStyle(id3_tree , shape="box", label=GetNodeLabel)
plot(id3_tree)

#2.














