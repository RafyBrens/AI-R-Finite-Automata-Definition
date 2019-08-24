#CURSO igraph

#Create networks
rm(list = ls()) # Remove all the objects we created so far.
dev.off() #dont forget to close the device
library(miniCRAN)
library(network)
library(intergraph)
library(igraph) # Load the igraph package
g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F )
plot(g1) # A simple plot of the network - we'll talk more about plots later
class(g1)
g1
# named vertices
g3 <- graph( c("John", "Jim", "Jim", "Jill", "Jill", "John")) 
# When the edge list has vertex names, the number of nodes is not needed
plot(g3)
g3
# In named graphs we can specify isolates by providing a list of their names.
g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"),
             isolates=c("Jesse", "Janis", "Jennifer", "Justin") )
plot(g4, edge.arrow.size=.5, vertex.color="gold", vertex.size=15,
     vertex.frame.color="gray", vertex.label.color="black",
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)
g4
E(g4) # The edges of the object
V(g4) # The vertices of the object
#You can also examine the network matrix directly:
g4[]
g4[1,]
g4[,2]

#Add attributes to the network, vertices, or edges:
V(g4)$name # automatically generated when we created the network.
V(g4)$gender <- c("male", "male", "male", "male", "female", "female", "male")
E(g4)$type <- "email" # Edge attribute, assign "email" to all edges
E(g4)$weight <- 10 # Edge weight, setting all existing edges to 10
edge_attr(g4)
vertex_attr(g4)
graph_attr(g4)

#Another way to set attributes (you can similarly use set_edge_attr(), set_vertex_attr(), etc.):
g4 <- set_graph_attr(g4, "name", "Email Network")
g4 <- set_graph_attr(g4, "something", "A thing")
graph_attr_names(g4)
graph_attr(g4, "name")
graph_attr(g4)

#Delete attribute
g4 <- delete_graph_attr(g4, "something")
graph_attr(g4)

#Plot the Graph
plot(g4, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
     vertex.color=c( "pink", "skyblue")[1+(V(g4)$gender=="male")] )

#remove loops & multiple edges between the same nodes
g4s <- simplify( g4, remove.multiple = T, remove.loops = F,
                 edge.attr.comb=c(weight="sum", type="ignore") )
plot(g4s, vertex.label.dist=1.5)
g4s

#Specific graphs and graph models
#Empty graph
eg <- make_empty_graph(40)
plot(eg, vertex.size=10, vertex.label=NA)

#Full graph
fg <- make_full_graph(40)
plot(fg, vertex.size=10, vertex.label=NA)

fg <- make_full_graph(10)
plot(fg, vertex.size=20)

#Simple star graph
st <- make_star(40)
plot(st, vertex.size=10, vertex.label=NA)

#Tree graph
tr <- make_tree(40, children = 3, mode = "undirected")
plot(tr, vertex.size=15, vertex.label=NA)

#Ring graph
rn <- make_ring(40)
plot(rn, vertex.size=10, vertex.label=NA)

#Erdos-Renyi random graph model
#('n' is number of nodes, 'm' is the number of edges).
er <- sample_gnm(n=100, m=40)
plot(er, vertex.size=6, vertex.label=NA)

#Watts-Strogatz small-world model
sw <- sample_smallworld(dim=2, size=10, nei=1, p=0.2)
plot(sw, vertex.size=6, vertex.label=NA, layout=layout_in_circle)

#Barabasi-Albert preferential attachment model for scale-free graphs
ba <- sample_pa(n=100, power=1, m=1, directed=F)
plot(ba, vertex.size=6, vertex.label=NA)

#igraph can also give you some notable historical graphs.
zach <- graph("Zachary") # the Zachary carate club
plot(zach, vertex.size=10, vertex.label=NA)

#Rewiring a graph
#each_edge() is a rewiring method that changes the edge endpoints uniformly randomly with a
#probability prob.
rn.rewired <- rewire(rn, each_edge(prob=0.1))
plot(rn.rewired, vertex.size=10, vertex.label=NA)

rn.neigh = connect.neighborhood(rn, 5)
plot(rn.neigh, vertex.size=8, vertex.label=NA)

#Reading network data from files 
#DATASET 1
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

#Examine the data:
head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

#We will collapse all links of the same type between the same two nodes 
#by summing their weights, using aggregate()
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

#DATASET 2: matrix
nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)
#Examine the data:
head(nodes2)
head(links2)
#We can see that links2 is an adjacency matrix for a two-mode network:
links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)

#Turning networks into igraph objects
#DATASET 1
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
class(net)
head(net)
E(net) # The edges of the "net" object
V(net) # The vertices of the "net" object
E(net)$type # Edge attribute "type"
V(net)$media # Vertex attribute "media"
plot(net, edge.arrow.size=.4,vertex.label=NA)

#removing the loops in the graph.
net <- simplify(net, remove.multiple = F, remove.loops = T)
E(net) # The edges of the "net" object
V(net) # The vertices of the "net" object
E(net)$type # Edge attribute "type"
V(net)$media # Vertex attribute "media"
plot(net, edge.arrow.size=.4,vertex.label=NA)

# Set edge color to gray, and the node color to orange.
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7)

# Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, edge.curved=.1)

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]
# Set node size based on audience size:
V(net)$size <- V(net)$audience.size*0.7
# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label.color <- "black"
V(net)$label <- NA
# Set edge width based on weight:
E(net)$width <- E(net)$weight/6
#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net)


plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="#555555",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7)
#We can also override the attributes explicitly in the plot:
plot(net, edge.color="orange", vertex.color="gray50")

#plot with legend
plot(net)
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#plotting only the labels of the nodes:
plot(net, vertex.shape="none", vertex.label=V(net)$media,
       vertex.label.font=2, vertex.label.color="gray40",
       vertex.label.cex=.7, edge.color="gray85")
#plot with curve
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]
plot(net, edge.color=edge.col, edge.curved=.1)
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
      col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#Layouts Graphs
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(3,3), mar=c(1,1,1,1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net))
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }

hist(links$weight)
mean(links$weight)
sd(links$weight)

cut.off <- mean(links$weight)
net.sp <- delete_edges(net, E(net)[weight<cut.off])
plot(net.sp)

E(net)$width <- 1.5
plot(net, edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1],
     vertex.color="gray40", layout=layout.circle)

# another way to delete edges
net.m <- net - E(net)[E(net)$type=="hyperlink"] 
net.h <- net - E(net)[E(net)$type=="mention"]
# Plot the two links separately:
par(mfrow=c(1,2))
plot(net.h, vertex.color="orange", main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", main="Tie: Mention")

# Make sure the nodes stay in place in both plots:
l <- layout_with_fr(net)
plot(net.h, vertex.color="orange", layout=l, main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2", layout=l, main="Tie: Mention")

#Interactive plotting with tkplot
dev.off()
tkid <- tkplot(net) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
tk_close(tkid, window.close = T)
plot(net, layout=l)

#Network and node descriptives

#Density
#The proportion of present edges from all possible edges in the network.
edge_density(net, loops=F)

#Reciprocity
#The proportion of reciprocated ties (for a directed network).
reciprocity(net)
dyad_census(net) # Mutual, asymmetric, and null node pairs
2*dyad_census(net)$mut/ecount(net) # Calculating reciprocity

#Transitivity
#. global - ratio of triangles (direction disregarded) to connected triples.
#. local - ratio of triangles to connected triples each vertex is part of.
transitivity(net, type="global") # net is treated as an undirected network
transitivity(as.undirected(net, mode="collapse")) # same as above
transitivity(net, type="local")
triad_census(net) # for directed networks

#Diameter
#A network diameter is the longest geodesic distance (length of the shortest path between two nodes)
#in the network.
diameter(net, directed=F, weights=NA)
diameter(net, directed=F)

diam <- get_diameter(net, directed=T)
diam
class(diam)
as.vector(diam)

#Color nodes along the diameter:
vcol <- rep("gray40", vcount(net))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(net))
ecol[E(net, path=diam)] <- "orange"
# E(net, path=diam) finds edges along a path, here 'diam'
plot(net, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

#Node degrees
deg <- degree(net, mode="all")
plot(net, vertex.size=deg*3)
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")
#Degree distribution
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",
      xlab="Degree", ylab="Cumulative Frequency")

#Centrality & centralization
#Degree (number of ties)
degree(net, mode="in")
centr_degree(net, mode="in", normalized=T)
#Closeness (centrality based on distance to others in the graph)
closeness(net, mode="all", weights=NA)
centr_clo(net, mode="all", normalized=T)

#Eigenvector (centrality proportional to the sum of connection centralities)
#Values of the first eigenvector of the graph matrix
eigen_centrality(net, directed=T, weights=NA)
centr_eigen(net, directed=T, normalized=T)

#Betweenness (centrality based on a broker position connecting others)
#Number of geodesics that pass through the node or the edge.
betweenness(net, directed=T, weights=NA)
edge_betweenness(net, directed=T, weights=NA)
centr_betw(net, directed=T, normalized=T)

#Hubs and authorities
hs <- hub_score(net, weights=NA)$vector
as <- authority_score(net, weights=NA)$vector
par(mfrow=c(1,2))
plot(net, vertex.size=hs*50, main="Hubs")
plot(net, vertex.size=as*30, main="Authorities")

#Distances and paths
mean_distance(net, directed=F)
mean_distance(net, directed=T)

distances(net) # with edge weights
distances(net, weights=NA) # ignore weights
dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], to=V(net), weights=NA)
dist.from.NYT
# Set colors to plot the distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]
plot(net, vertex.color=col, vertex.label=dist.from.NYT, edge.arrow.size=.6,
     vertex.label.color="white")

news.path <- shortest_paths(net,
                            from = V(net)[media=="MSNBC"],
                            to = V(net)[media=="New York Post"],
                            output = "both") # both path nodes and edges
