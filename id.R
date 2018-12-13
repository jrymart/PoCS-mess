most_connected <- which(degs==max(degs))
mc_nbhd <- neighbors(id_network, most_connected)
big_hub <- induced_subgraph(id_network, mc_nbhd)
plot(big_hub, edge.arrow.size=.4, 
     vertex.size=3, vertex.label=NA, layout=l)



library(igraph)
library(ggplot2)
library(fitdistrplus)
library(qgraph)
setwd('C:/Users/joe/Documents/uvm/pocs/wikipedia/testy/idtalk/gml')
id_graph_files <- list.files('.', pattern='\\.gml$')
id_graphs <- lapply(id_graph_files, function (x) read_graph(x,format='graphml') )
id_union <- disjoint_union(id_graphs)
id_vs <- vertex_attr(id_union)
contract_list <- match(id_vs$user, id_vs$user)
id_contract <- contract(id_union, contract_list, list("first"))
duplicates <- contract_list-1:vcount(id_union)
to_delete <- which(duplicates !=0)
to_delete <- to_delete[to_delete<=length(V(id_contract))]
id_smushed <- delete_vertices(id_contract, to_delete)
id_network <- simplify(id_smushed)
write_graph(id_network, 'id_network.graphml', format='graphml')
vcount(id_network)
l <- layout_with_kk(id_network)
plot(id_network, edge.arrow.size=.4, 
     vertex.size=3, vertex.label=NA, layout=l)
tkplot(id_network)
compdist <- component_distribution(id_network)
comps <- components(id_network)
degdist <- degree_distribution(id_network)
degs <- degree(id_network)
qplot(degdist)
qplot(degs)
cdf = ecdf(degs)
qplot(cdf)

hist(degs, main="Histogram for degrees of ID talk page")
plot(density(degs), main="Density estimate of ID degrees")
plot(ecdf(degs),main="CDF of ID degrees")
deg.stand <- (degs-mean(degs))/sd(degs)
qqnorm(deg.stand)
abline(0,1)
# soooo not poison?
x.teo<-rweibull(n=length(V(id_network)),shape=2, scale=1)
qqplot(x.teo, deg.stand, main="Q-Q Plot for Weibull Dist")
x.poi<-rpois(n=length(V(id_network)),lambda=.01) 
qqplot(x.poi, deg.stand, main="Q-Q Plot for Poison Dist with $\\lambda=.1$")
abline(0,1)
hist(x.poi)
hist(degs)

big_comp <- decompose(id_network, max.comps=1)[[1]]
length(V(big_comp))
length(V(id_network))
bc_degs = degree(big_comp)
library(poweRlaw)
#now power law
data_pl <- displ$new(bc_degs)
data_pl$getXmin()
data_pl$getPars()
est <- estimate_pars(data_pl)
est_pl <- estimate_xmin(data_pl)
data_pl$setPars(est)
#data_pl$setXmin(est_pl)
plot(data_pl)
lines(data_pl,col=2)
bs_p <- bootstrap_p(data_pl, no_of_sims=1000, threads=4)
plot(bs_p)
bs_p$p
bs_p$gof

bc_degs_1 <- bc_degs[bc_degs<40]
bc_degs_2 <- bc_degs[bc_degs>=40]
data_pl_1 <- displ$new(bc_degs_1)
data_pl_2 <- displ$new(bc_degs_2)
est_pl_1 <- estimate_xmin(data_pl_1)
est_pl_2 <- estimate_xmin(data_pl_2)
data_pl_1$setXmin(est_pl_1)
data_pl_2$setXmin(est_pl_2)
plot(data_pl_1)
lines(data_pl_1,col=2)
bs_p_1 <- bootstrap_p(data_pl_1, threads=2)
plot(bs_p_1)
plot(bs_p_1)

# ok...enough for power law things
average.path.length(big_comp)
transitivity(big_comp)
