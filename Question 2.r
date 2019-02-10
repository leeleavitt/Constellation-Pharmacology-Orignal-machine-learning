####################################################
#Attributes are
outlook<-c("ssorrrossrsoor")
temperature<-c("hhhmcccmcmmmhm")
humidity<-c("hhhhnnnhnnnhnh")
wind<-c("wswwwsswwwssws")

atts<-list(outlook, temperature, humidity, wind)


for(i in 1:length(atts)){
	atts[[i]]<-strsplit(atts[[i]],"")[[1]]
	}

sample_space<-as.data.frame(Reduce(cbind, atts))

label_space<-"00111010111110"
label_space<-as.numeric(strsplit(label_space,"")[[1]])

example_set<-cbind(sample_space, label_space)
cat("\nThis is your example set plus label space\n\n")

names(example_set)<-c('outlook', 'temp', 'humidity', 'wind', 'label')
print(example_set)
######################################################

#now load the functin created
source("C:/Users/madman435/Documents/cs 6350/hw1/id3.functions.r")
node<-Node$new("id3_tree")
id3_tree<-ID3(example_set, c(1:4), c(5), node, "ME", info_gain_func="ME")
print(id3_tree)

SetEdgeStyle(id3_tree , fontname='helvetica', label=GetEdgeLabel)
#SetNodeLabel(bob, label=GetNodeLabel)
SetNodeStyle(id3_tree , shape="box", label=GetNodeLabel)
plot(id3_tree)












