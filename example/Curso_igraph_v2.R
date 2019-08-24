#Create networks
rm(list = ls()) # Remove all the objects we created so far.
dev.off() #dont forget to close the device
library(miniCRAN)
library(network)
library(intergraph)
library(igraph) # Load the igraph package

nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

#Eliminate multiple links with aggregate
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

#converting the raw data to an igraph network object.
net <- graph.data.frame(links, nodes, directed=F)
net

#access to nodes, edges, and their attributes with:
E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"
# You can also manipulate the network matrix directly:
net[1,]
net[5,7]

#Let's make a first attempt to plot it.
plot(net) # not a pretty picture!

#Let's start fixing things by removing the loops And multiple edgesin the graph
net <- simplify(net, remove.multiple = T, remove.loops = T) 
plot(net, edge.arrow.size=.4)


# Import system fonts - may take a while, so DO NOT run this during the workshop.
library(extrafont) #n order to import fonts from the OS into R
font_import() 
fonts() # See what font families are available to you now.
loadfonts(device = "win") # use device = "pdf" for pdf plot output. 

plot(net, vertex.size=30)
plot(net, vertex.size=30, vertex.label.family="Arial Black" )

# Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, edge.curved=.1)

# Circle layout
l <- layout_in_circle(net)
plot(net, layout=l,vertex.label.cex=1.0)

# 3D sphere layout
l <- layout_on_sphere(net)
plot(net, layout=l,vertex.label.cex=1.0)

l <- layout_with_fr(net)
plot(net, layout=l)

l <- layout_with_kk(net)
plot(net, layout=l)

# Set edge color to light gray, the node & border color to orange 
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black") 

# Generate colors base on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net) 

#We can also override the attributes explicitly in the plot:
plot(net, edge.color="orange", vertex.color="gray50") 

#add a legend explaining the meaning of the colors we used:
plot(net) 
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#with semantic networks, we may be interested in plotting only the labels of the nodes:
plot(net, vertex.shape="none", vertex.label=V(net)$media, 
       vertex.label.font=2, vertex.label.color="gray40",
       vertex.label.cex=.7, edge.color="gray85")

#Let's color the edges of the graph based on their source node color. 
#We can get the starting node for each edge with the  get.edges igraph function.
edge.start <- get.edges(net, 1:ecount(net))[,1]
edge.col <- V(net)$color[edge.start]
plot(net, edge.color=edge.col, edge.curved=.1) 


