# A look at the igraph package by Joseph Rickert
# https://blog.revolutionanalytics.com/2014/11/a-look-at-the-igraph-package.html

library(miniCRAN)
library(igraph)

pk <- c("igraph","agop","bc3net","BDgraph","c3net","camel",
        "cccd", "CDVine", "CePa", "CINOEDV", "cooptrees","corclass", "cvxclustr", "dcGOR",
        "ddepn","dils", "dnet", "dpa", "ebdbNet", "editrules",
        "fanovaGraph", "fastclime", "FisHiCal", 
        "flare", "G1DBN", "gdistance", "GeneNet", "GeneReg", "genlasso", "ggm", "gRapfa", "hglasso", 
        "huge", "igraphtosonia", "InteractiveIGraph", "iRefR", "JGL", "lcd", "linkcomm", "locits",
        "loe", "micropan", "mlDNA", "mRMRe", "nets", "netweavers", "optrees", "packdep", "PAGI", 
        "pathClass", "PBC", "phyloTop", "picasso", "PoMoS", "popgraph", "PROFANCY", "qtlnet", "RCA", 
        "ReliabilityTheory", "rEMM", "restlos", "rgexf", "RNetLogo", "ror", "RWBP", "sand", "SEMID", 
        "shp2graph", "SINGLE", "spacejam", "TDA", "timeordered", "tnet")


dg <- makeDepGraph(pk)
plot(dg,main="Network of reverse depends for igraph",cex=.4,vertex.size=8)
dev.off() #dont forget to close the device
#And that's the end for now.
