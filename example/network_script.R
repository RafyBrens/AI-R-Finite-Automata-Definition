## A simple example with a couple of actors
## The typical case is that these tables are read in from files....
library(igraph)
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)
plot(g)
## The opposite operation
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")

# http://novyden.blogspot.com/2016/01/r-graph-objects-igraph-vs-network.html
library(igraph)
library(network)
library(intergraph)

# igraph 
pkg.igraph = graph_from_edgelist(edges.mat, directed = TRUE)
pkg.network.from.igraph = asNetwork(pkg.igraph)
all.equal(length(get.edgelist(pkg.igraph)), length(as.matrix(pkg.network.from.igraph, "edgelist")))

# network
pkg.network = network(edges.mat)
pkg.igraph.from.network = asIgraph(pkg.network)
all.equal(length(as.matrix(pkg.network, "edgelist")), length(get.edgelist(pkg.igraph.from.network)))

library(miniCRAN)

cranInfo = pkgAvail()

plot(makeDepGraph(c("network"), availPkgs = cranInfo))
plot(makeDepGraph(c("igraph"), availPkgs = cranInfo))

# Load the igraph package (install if needed)
# http://re-design.dimiter.eu/?p=547
require(igraph)

# Data format. The data is in 'edges' format meaning that each row records a relationship (edge) between two people (vertices).
# Additional attributes can be included. Here is an example:
#	Supervisor	Examiner	Grade	Spec(ialization)
#	AA		BD		6	X	
#	BD		CA		8	Y
#	AA		DE		7	Y
#	...		...		...	...
# In this anonymized example, we have data on co-supervision with additional information about grades and specialization. 
# It is also possible to have the data in a matrix form (see the igraph documentation for details)

# Load the data. The data needs to be loaded as a table first: 
#specify the path, separator(tab, comma, ...), decimal point symbol, etc.
#bsk<-read.table("http://www.dimiter.eu/Data_files/edgesdata3.txt", sep='t', dec=',', header=T)
bsk<-read.csv("network_example.csv", header=T)

# Transform the table into the required graph format:
bsk.network<-graph.data.frame(bsk, directed=F) #the 'directed' attribute specifies whether the edges are directed
# or equivelent irrespective of the position (1st vs 2nd column). For directed graphs use 'directed=T'

# Inspect the data:

V(bsk.network) #prints the list of vertices (people)
E(bsk.network) #prints the list of edges (relationships)
degree(bsk.network) #print the number of edges per vertex (relationships per people)

# First try. We can plot the graph right away but the results will usually be unsatisfactory:
plot(bsk.network)

#Subset the data. If we want to exclude people who are in the network only tangentially (participate in one or two relationships only)
# we can exclude the by subsetting the graph on the basis of the 'degree':

bad.vs<-V(bsk.network)[degree(bsk.network)<3] #identify those vertices part of less than three edges
bsk.network<-delete.vertices(bsk.network, bad.vs) #exclude them from the graph

# Plot the data.Some details about the graph can be specified in advance.
# For example we can separate some vertices (people) by color:

V(bsk.network)$color<-ifelse(V(bsk.network)$name=='CA', 'blue', 'red') #useful for highlighting certain people. Works by matching the name attribute of the vertex to the one specified in the 'ifelse' expression

# We can also color the connecting edges differently depending on the 'grade': 

E(bsk.network)$color<-ifelse(E(bsk.network)$grade==9, "red", "grey")

# or depending on the different specialization ('spec'):

E(bsk.network)$color<-ifelse(E(bsk.network)$spec=='X', "red", ifelse(E(bsk.network)$spec=='Y', "blue", "grey"))

# Note: the example uses nested ifelse expressions which is in general a bad idea but does the job in this case
# Additional attributes like size can be further specified in an analogous manner, either in advance or when the plot function is called:

V(bsk.network)$size<-degree(bsk.network)/10#here the size of the vertices is specified by the degree of the vertex, so that people supervising more have get proportionally bigger dots. Getting the right scale gets some playing around with the parameters of the scale function (from the 'base' package)

# Note that if the same attribute is specified beforehand and inside the function, the former will be overridden.
# And finally the plot itself:
par(mai=c(0,0,1,0)) 			#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
plot(bsk.network,				#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Organizational network example',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(bsk.network)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary
)

# Save and export the plot. The plot can be copied as a metafile to the clipboard, or it can be saved as a pdf or png (and other formats).
# For example, we can save it as a png:
png(filename="org_network.png", height=800, width=600) #call the png writer
#run the plot
dev.off() #dont forget to close the device
#And that's the end for now.

