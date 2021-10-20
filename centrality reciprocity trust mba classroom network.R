rm(list = ls(all = TRUE))
library(igraph)
library(data.table)
setwd("C:\\Users\\jentl\\Downloads")

# task 1 -- getting adjacency matrix data into igraph
data = fread("anonymized_mba_classroom_network.csv", header = TRUE) #datatable version

# make the text choice data numeric
scale = cbind(c("Extremely Distrust  1", "Distrust  2", "Slightly Distrust  3", 
                "Neither Distrust Nor Trust  4", "Slightly Trust  5","Trust  6", 
                "Extremely Trust  7", "I don't know this person.", "This is my own name."), 
              c(-3, -2, -1, 0, 1, 2, 3, 0, 0))

# convenience function for column-wise replace in data.table
column_wise_replace = function(DT, x, y) {
	for(i in seq_along(x)){
		 for (j in seq_len(ncol(DT))){
    		set(DT,which(DT[[j]] == x[i]),j,y[i])
		}
	}
}

column_wise_replace(data, scale[,1], scale[,2])

# incomplete responses will leave blanks, which we don't want to sum as NAs later
blanks_to_0s = function(DT) {
		 for (j in seq_len(ncol(DT))){
    		set(DT,which(DT[[j]] == ""),j,0)
		}
	}

blanks_to_0s(data) # warning message is since we convert to numeric later on--can avoid this message with set(DT,which(DT[[j]] == ""),j,"0") inside of the above loop

# something similar for NAs could take the form of
# blanks_to_0s = function(DT) {
# 	for(i in seq_along(x)){
# 		 for (j in seq_len(ncol(DT))){
#     		set(DT,which(is.na(DT[[j]]),j,0)
# 		}
# 	}
# }
# this would be useful if the matrix were already numeric


# make adjacency matrix

# subset to just the trust choices, then sort on the columns and rows to make each entry match up
# data are undirected, so matrix will not be symmetric
adj = as.data.frame(data[,1:(ncol(data) - 5)])
rownames(adj) = adj[,1]
adj = adj[,-1]

# then, insert a row of 0s for participants who did not fill out the survey, 
# so they can still be represented in terms of ties received
insert = colnames(adj)[!colnames(adj) %in% intersect(colnames(adj), rownames(adj))]
insert = sapply(1:length(insert), function(i) c(insert[i], rep(0, ncol(adj))))
insert_df = as.data.frame(t(insert)[,-1])
colnames(insert_df) = colnames(adj)
rownames(insert_df) = insert[1,]

adj = rbind(adj, insert_df)

adj = adj[sort(rownames(adj)),sort(colnames(adj))]


# task 2 -- plot the trust relationships according to their strength and direction
trust_matrix = adj
# subset to just the trust choices first by getting rid of negatives
trust_matrix[trust_matrix < 0] = 0

trust = graph.adjacency(as.matrix(trust_matrix), "directed", weighted = TRUE)

plot.igraph(trust,vertex.label=NA,layout=layout.fruchterman.reingold, 
            vertex.label.color="black",edge.color="black",edge.width=E(trust)$weight,vertex.size = 12, 
            edge.arrow.size=.3,edge.curved=FALSE)

# for large layounts, use layout=kamada.kawai

# task 3 -- plot the distrust relationships according to their strength and direction, 
# and also color nodes in terms of how distrusted they are

# distrusting data must be numeric. igraph ignores blanks, which is okay for trust graph because no isolates,
# but for distrusting need blanks --> 0s for isolates to appear on the plot
distrust_matrix = apply(adj, 2, as.numeric)*-1
distrust_matrix[distrust_matrix < 0] = 0

# those who are the most distrusted have the highest column sums (indegree)
distrusted = colSums(distrust_matrix, na.rm = TRUE)

distrust = graph.adjacency(as.matrix(distrust_matrix), "directed", weighted = TRUE)
distrusted2 = degree(distruct, mode='in')


# igraph also gives the solution via degree(distrusting, mode = "in")

# solution 1 using rainbow, one way to label ordinally: start = 0 with end = 1/6 
# produces a gradient from red to yellow as distrustedness grows high
colrange = rainbow(length(unique(distrusted)), start = 0, end = 1/6) 
# colrange is like the colors object from the igraph example but produces a gradient instead
distrust = set_vertex_attr(distrust, "distrusted", index = V(distrust), as.factor(distrusted))

# assign color through igraph vertex attribute
# if you assign a vertex attribute as color, igraph will recognize it
V(distrust)$color = colrange[V(distrust)$distrusted]

plot.igraph(distrust,vertex.label=NA,layout=layout.fruchterman.reingold, 
            vertex.label.color="black",edge.color="black",edge.width=E(distrust)$weight,
            vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)

# solution 2 using colorRampPalette, another way to label ordinally -- here input "red" and "yellow" 
# by name and assign color through raw vertex attribute
colrange = colorRampPalette(c('red','yellow'))
distrusted_color = colrange(length(unique(distrusted)))[as.factor(distrusted)]

# and set the colors directly 
V(distrust)$color = distrusted_color

plot.igraph(distrust,vertex.label=NA,layout=layout.fruchterman.reingold, 
            vertex.label.color="black",edge.color="black",edge.width=E(distrust)$weight,
            vertex.size = 12, edge.arrow.size=.3,edge.curved=FALSE)


# task 4 -- plot the advice network, coloring the node who the most individuals 
# go to for advice in a different color

# now we are working with an edge list instead of a matrix

# getting edge list data into igraph
advice_edge1 = cbind(data[,1], data$V38)
advice_edge2 = cbind(data[,1], data$V39)
advice_edge3 = cbind(data[,1], data$V40)
advice_edge4 = cbind(data[,1], data$V41)
advice_edge5 = cbind(data[,1], data$V42)

advice_edges = rbind(advice_edge1, advice_edge2, advice_edge3, advice_edge4, advice_edge5)

colnames(advice_edges) = c("advisee", "advisor")
advice_edges = advice_edges[advisor != ""]

# can check the overlap between the two networks to make sure that matrix sizes will overlap later on
V(trust)$name[!V(trust)$name %in% as.character(c(advice_edges$advisee, advice_edges$advisor))]

# 32 is missing from advice network -- can add manually to edge list as an isolate by adding 
# as a self-loop and then simplifying to remove the loop
advice_edges = rbind(advice_edges, data.table(advisee = 32, advisor = 32))

advice_seeking = graph.data.frame(advice_edges, directed = TRUE)

advice_seeking = simplify(advice_seeking)

# the indiviual to whom the most people go to for advice appears the most times in the righthand column
advice_edges[, count := .N, by = advisor]
top_advisor = unique(advice_edges$"advisor"[advice_edges$count == max(advice_edges$count)])

# igraph also gives the solution via degree(advice_seeking, mode = "in")

V(advice_seeking)$name[which.max(degree(advice_seeking, mode='in'))]

# can set color of individual node by name
V(advice_seeking)$color = "light blue"
V(advice_seeking)$color[names(V(advice_seeking)) == top_advisor] = "pink"

# make the plot
plot.igraph(advice_seeking,vertex.label=NA,layout=layout.fruchterman.reingold, 
            vertex.label.color="black",edge.color="black",vertex.size = 12, edge.arrow.size=.3,
            edge.curved=FALSE)

# task 5 -- how many of the trust and advice relationships are reciprocated? 
# is this more or less than random chance? for this portion, treat the trust and 
# distrust relationships as binary (i.e., not weighted)\

# first, do this directly via the matrices

# manipulate each network as a matrix--needs to be numeric
trust_matrix = apply(adj, 2, as.numeric)
trust_matrix[trust_matrix < 0] = 0
trust_matrix[trust_matrix > 0] = 1

# can look at overlap of ties going in each direction, 1 from each source = 2. 
# this gives a count of how many people sent a tie that was also received. to get the number of relationships, 
# divide the number of people by two
sum(trust_matrix + t(trust_matrix) == 2)/2 #transpose of matrix = reverse tie direction

# can also use matrix formula: number of reciprocated relationships is equal to 1/2*trace(network*network)
sum(diag(trust_matrix%*%trust_matrix))/2

# for reciprocation to be totally up to chance, reciprocation and not reciprocation are equally as likely
# --50% chance of occurring

# divide number of reciprocated relationships by total relationships. 
# number of total relationships is equal to reciprocated plus non-reciprocated. 
# 1 and 0 coming from each source = 1. gives a count of how many people sent a tie that was not sent back.
# to get the number of relationships, divide this number by two
(sum(trust_matrix + t(trust_matrix) == 2)/2) / (sum(trust_matrix + t(trust_matrix) == 2)/2 + 
                                                  sum(trust_matrix + t(trust_matrix) == 1)/2)

# can also use the matrix formula: number of non-reciprocated relationships is 
# given by trace(network*t(network)) - trace(network*network)
(sum(diag(trust_matrix%*%trust_matrix))/2)/(sum(diag(trust_matrix%*%trust_matrix))/2 + 
                                              sum(diag(trust_matrix%*%t(trust_matrix))) - 
                                              sum(diag(trust_matrix%*%trust_matrix)))

# trust relationships are reciprocated more often than not/by chance 	
# igraph also gives the solution via 
dyad.census(trust)


# repeat for distrust
distrust_matrix = apply(adj, 2, as.numeric)*-1
distrust_matrix[distrust_matrix < 0] = 0
distrust_matrix[distrust_matrix > 0] = 1

sum(diag(distrust_matrix%*%distrust_matrix))/2

(sum(diag(distrust_matrix%*%distrust_matrix))/2)/(sum(diag(distrust_matrix%*%distrust_matrix))/2 + 
                                                    sum(diag(distrust_matrix%*%t(distrust_matrix))) - 
                                                    sum(diag(distrust_matrix%*%distrust_matrix)))

dyad.census(distrust)


# less reciprocation for distrusting

# last for advice--need to get into matrix form first. can do this directly in igraph
advice_matrix = as_adjacency_matrix(advice_seeking, sparse = FALSE) 

# then repeat
sum(diag(advice_matrix%*%advice_matrix))/2

(sum(diag(advice_matrix%*%advice_matrix))/2)/(sum(diag(advice_matrix%*%advice_matrix))/2 + 
                                                sum(diag(advice_matrix%*%t(advice_matrix))) - 
                                                sum(diag(advice_matrix%*%advice_matrix)))

# somewhat less reciprocation than by chance

# task 6 -- figure out how much overlap is there between the relationships in the trust network 
# and the advice networks

# if relationships exist in both networks, will receive a "1" from each

# note that it's necessary to sort the advice matrix on the row and column names so the vertices 
# line up with one another (igraph will do this automatically with the V(graph)$name parameter)
advice_matrix = advice_matrix[sort(rownames(advice_matrix)),sort(colnames(advice_matrix))]
sum(trust_matrix + advice_matrix == 2)

# can also use igraph's intersection
trust = graph.adjacency(as.matrix(trust_matrix), "directed")

overlap = intersection(trust, advice_seeking)
length(E(overlap))

# for the distrust and advice comparison
sum(distrust_matrix + advice_matrix == 2)

overlap = intersection(distrust, advice_seeking)
length(E(overlap))

# one individual both distrusts another and goes to them for advice!

# plot the overlap between the trust and advice networks, indicating which relationships are trust only, 
# advice only, or both


# manually manipulate weights to identify the presence of tie
advice_matrix[advice_matrix == 1] = 4 #4 is arbitrary, could have been any #>1

# red = only trust, blue = only advice, purple = both
trusting_advice = graph.adjacency(as.matrix(trust_matrix + advice_matrix), "directed", weighted = TRUE)
ranges = E(trusting_advice)$weight
ranges[ranges > 4] = "purple"
ranges[ranges == 4] = "dark blue"
ranges[ranges < 4] = "red"


# since edges are in the same order as before, 
# can just recycle the weights from earlier in the plotting parameters to preserve strength of 
# trust relationship
plot.igraph(trusting_advice,vertex.label=NA,layout=layout.fruchterman.reingold, 
            vertex.label.color="black",edge.color=ranges,edge.width=E(trust)$weight,vertex.size = 12, 
            edge.arrow.size=.3,edge.curved=FALSE)

