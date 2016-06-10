library('network')
library('sna')
library('ggplot2')
library('GGally')
library('animation')

load('c:/Documents and Settings/chr/Desktop/ideas_network.RData')

net = network(adj)

ggnet2(net, size = "degree", size.cut = 3, node.color = "black", edge.size = 1, edge.color = "grey")


xy = gplot.layout.fruchtermanreingold(net, NULL)
net %v% "x" = xy[, 1]
net %v% "y" = xy[, 2]

for ( i in 1:277 ) {
	net %v% paste0('t', i) <- c(rep(1, i), rep(NA, 277-i))
}

myg <- function() {
	for ( i in 1:277 ) {
		print(ggnet2(net, size = 3, mode = c("x", "y"), color = "black", na.rm = paste0('t', i))) #plot(1:i)
		ani.pause()
	}
}

saveGIF(myg())