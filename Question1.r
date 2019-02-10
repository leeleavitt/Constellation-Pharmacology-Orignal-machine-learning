setwd("C:/Users/madman435/Documents/cs 6350/hw1/Question1.r")
#i am going to create id3 algorithm for the training data 
#within a boolean classifier
#Import and create the data
boo<-rbind( 
	c(0,0,1,0,0), 
	c(0,1,0,0,0), 
	c(0,0,1,1,1), 
	c(1,0,0,1,1), 
	c(0,1,1,0,0), 
	c(1,1,0,0,0), 
	c(0,1,0,1,0)
)
colnames(boo)<-c("x1","x2","x3","x4","y")
row.names(boo)<-seq(from=1, to=dim(boo)[1], by=1)
boo<-data.frame(boo)
cat("\nOur Set of S Examples are\n")
print(boo)
######################################

#now load the functin created
source("C:/Users/madman435/Documents/cs 6350/hw1/id3.functions.r")
node<-Node$new("id3_tree")
id3_tree<-ID3(boo, c(1:4), c(5), node, "E")
print(id3_tree)

SetEdgeStyle(id3_tree$x2 , fontname='helvetica', label=GetEdgeLabel)
#SetNodeLabel(bob, label=GetNodeLabel)
SetNodeStyle(id3_tree$x2 , shape="", label=GetNodeLabel)
plot(id3_tree$x2)


