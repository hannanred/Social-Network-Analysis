#Getting the nodes and edges ready
library(xlsx)
library(dplyr)
library(igraph)
sheet1<-read.xlsx("Data.xlsx",1,header=FALSE)
PTI<-sheet1[2:13,1]
PTI<-data.frame(PTI)
PTI$party="PTI"
names(PTI)[1]<-"name"
PMLN<-sheet1[15:24,1]
PMLN<-data.frame(PMLN)
PMLN$party="PMLN"
names(PMLN)[1]<-"name"
PPP<-sheet1[26:35,1]
PPP<-data.frame(PPP)
PPP$party="PPP"
names(PPP)[1]<-"name"

#nodes
nodes=PTI
nodes[13:22,1:2] = PPP
nodes[23:32,1:2] = PMLN
#edges
edges<-read.xlsx("Data.xlsx",2)
names(edges)[1]<-"Caller"
names(edges)[2]<-"Reciever"
names(edges)[3]<-"Number of Calls"


#Question 1
par(mar=c(0,0,0,0))
g<-graph_from_data_frame(d=edges,vertices=nodes,directed=FALSE)
V(g)$color<-NA
V(g)$color[V(g)$party == "PPP"]<-"red"
V(g)$color[V(g)$party == "PTI"]<-"gold"
V(g)$color[V(g)$party == "PMLN"]<-"pink"
V(g)$size=betweenness(g,directed=FALSE,weights=edges$'Number of Calls')
plot(g,vertex.label.color="black")

#Question 2
#PMLN
g=graph_from_data_frame(d=subset(edges,Caller %in% PMLN$name & Reciever %in% PMLN$name),vertices=PMLN,directed=TRUE)
plot(g,edge.arrow.size=0.4)

#PTI
g=graph_from_data_frame(d=subset(edges,Caller %in% PTI$name & Reciever %in% PTI$name),vertices=PTI,directed=TRUE)
plot(g,edge.arrow.size=0.4)


#PPP
g=graph_from_data_frame(d=subset(edges,Caller %in% PPP$name & Reciever %in% PPP$name),vertices=PPP,directed=TRUE)
plot(g,edge.arrow.size=0.4)

#Question 3

g=graph_from_data_frame(d=subset(edges,Caller %in% PTI$name & Reciever %in% PTI$name),vertices=PTI,directed=TRUE)
V(g)$size = strength(g, mode = "all", weights=subset(edges,Caller %in% PTI$name & Reciever %in% PTI$name)$'Number of Calls')/6
plot(g)
#Imran Khan

#Question 4
g=graph_from_data_frame(d=subset(edges,Caller %in% PMLN$name & Reciever %in% PMLN$name),vertices=PMLN,directed=TRUE)
V(g)$size = strength(g, mode = "out", weights=subset(edges,Caller %in% PMLN$name & Reciever %in% PMLN$name)$'Number of Calls')/6
plot(g)
#Shehbaz Sharif

#Question 5
g=graph_from_data_frame(d=subset(edges,Caller %in% PPP$name & Reciever %in% PPP$name),vertices=PPP,directed=TRUE)
V(g)$size = strength(g, mode = "in", weights=subset(edges,Caller %in% PPP$name & Reciever %in% PPP$name)$'Number of Calls')/6
plot(g)
#Asif Zardari 

#Question 6
g=graph_from_data_frame(d=subset(edges,Caller %in% PTI$name & Reciever %in% PTI$name),vertices=PTI,directed=TRUE)
ec<-eigen_centrality(g,directed=TRUE,weights=subset(edges,Caller %in% PTI$name & Reciever %in% PTI$name)$'Number of Calls')
ec$vector
#Imran Khan

#Question 7
PPP2PTI<-subset(edges,(Caller %in% PPP$name) & (Reciever %in% PTI$name) )
PPP2PMLN<-subset(edges,(Caller %in% PPP$name) & (Reciever %in% PMLN$name) )
PTI2PMLN<-subset(edges,(Caller %in% PTI$name) & (Reciever %in% PMLN$name) )
PTI2PPP<-subset(edges,(Caller %in% PTI$name) & (Reciever %in% PPP$name) )
PMLN2PTI<-subset(edges,(Caller %in% PMLN$name) & (Reciever %in% PTI$name) )
PMLN2PPP<-subset(edges,(Caller %in% PMLN$name) & (Reciever %in% PPP$name) )


elections <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(elections)<- c("Caller","Reciever","Number of Calls")
elections[1:2,1] = "PPP"
elections[1,2] = "PTI"
elections[1,3] = sum(PPP2PTI[,3])
elections[2,2] = "PMLN"
elections[2,3] = sum(PPP2PMLN[,3])
elections[3:4,1] = "PTI"
elections[3,2] = "PMLN"
elections[3,3] = sum(PTI2PMLN[,3])
elections[4,2] = "PPP"
elections[4,3] = sum(PTI2PPP[,3])
elections[5:6,1]="PMLN"
elections[5,2] = "PPP"
elections[5,3] = sum(PMLN2PPP[,3])
elections[6,2] = "PTI"
elections[6,3] = sum(PMLN2PTI[,3])

electionNodes<-data.frame(matrix(ncol=2,nrow=0))
electionNodes[1,1] ="PTI"
electionNodes[2,1] = "PMLN"
electionNodes[3,1] = "PPP"
electionNodes[1,2] = 'a'
electionNodes[2,2]='b'
electionNodes[3,2] = 'c'
View(electionNodes)

g=graph_from_data_frame(d=elections,vertices=electionNodes,directed=TRUE)

#Which recieves most communication
strength(g, mode = "in", weights=elections$'Number of Calls')
#PPP recieves most communication

#Which connects most with other parties
strength(g, mode = "out", weights=elections$'Number of Calls')
#PMLN connects the most with other parties

#Most interconnected
strength(g, mode = "all", weights=elections$'Number of Calls')
#PPP is the most interconnected