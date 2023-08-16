# compute the correlation matrix
corr.matrix <- cor(returns)

# compute euclidean distance
d <- sqrt(2*(1-corr.matrix))

# create graph from "d" (distance matrix)
g1 <- graph_from_adjacency_matrix(d, weighted = T, mode = "undirected")

# create MST from graph
mst1 <- mst(g1)

# implement an optimization algorithm to find the communitry structure
lou <- cluster_louvain(g1, resolution = 1.1)

# generate a GEM disposition
LOWG <- layout_with_gem(mst1)

# associate to each Ticker in the MST the corresponding classification from "cls"
e1 <- left_join(data.frame(Ticker = V(mst1)$name), cls)

# assign the Industry color in the MST
V(mst1)$color <- e1$Color..Industry.

# plot the MST
plot(mst1,layout=LOWG, vertex.label.color="black", vertex.label.cex=1)

# set MST edge thickness proportional to the distance
E(mst1)$width <- E(mst1)$weight*5

# remove label from the plot
V(mst1)$label <- NA

# plot the MST
plot(mst1,layout=LOWG, vertex.size=10, vertex.frame.color="#ffffff", vertex.label.color="black", vertex.label.cex=1)

# restore the MST
mst1 <- mst(g1)

# repeat the same procedure using sub-industry colors
V(mst1)$color <- e1$Color..Sub.industry.
plot(mst1,layout=LOWG, vertex.label.color="black", vertex.label.cex=1)
E(mst1)$width <- E(mst1)$weight*5
V(mst1)$label <- NA
plot(mst1,layout=LOWG, vertex.size=10, vertex.frame.color="#ffffff", vertex.label.color="black", vertex.label.cex=1)

# create a distance from distance matrix
d1 <- as.dist(d)

# create the single linkage hierarchical clusters
cl <- hclust(d1, method = "single")

# create a deprogram
dcl <- as.dendrogram(cl)

# create a function that assign to each leaf the corresponding industry color
col.Leaf <- function(n){
  a <- attributes(n)
  col_treatment <- cls$Color..Industry.[cls$Ticker==a$label]
  attr(n,"nodePar")<-c(a$nodePar,list(col=col_treatment,pch=20, cex=3.5, lab.cex=1))
  
  return(n)
}

# apply the function the the dendrogram
dL <- dendrapply(dcl, col.Leaf)

# plot the dendrogram
plot(dL)

# repeat the same procedure for the sub-industry 
col.Leaf2 <- function(n){
  a <- attributes(n)
  col_treatment <- cls$Color..Sub.industry.[cls$Ticker==a$label]
  attr(n,"nodePar")<-c(a$nodePar,list(col=col_treatment,pch=20, cex=3.5, lab.cex=1))
  
  return(n)
}
dL2 <- dendrapply(dcl, col.Leaf2)
plot(dL2)


# create a distance matrix ordered according to the clustering order
od <- d[rev(cl$labels[cl$order]),rev(cl$labels[cl$order])]

# plot the unordered distance heatmap
melt(d) %>% ggplot(aes(Var1, Var2, fill=value)) + geom_tile() + scale_fill_viridis_c() + labs(x="", y="") +
  scale_x_discrete(labels=1:36) + scale_y_discrete(labels=1:36) + labs(fill = "Distance")


# plot the ordered distance heatmap
melt(od) %>% ggplot(aes(Var1, Var2, fill=value)) + geom_tile() + scale_fill_viridis_c() + labs(x="", y="") +
  scale_x_discrete(labels=1:36) + scale_y_discrete(labels=1:36) + labs(fill = "Distance")

