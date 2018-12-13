library(igraph)
setwd('C:/Users/joe/Documents/uvm/pocs/wikipedia/testy/idtalk/gml')
id_graph_files <- list.files('.', pattern='\\.gml$')
id_graphs <- lapply(id_graph_files, function (x) read_graph(x,format='graphml') )
id_union <- disjoint_union(id_graphs)
id_vs <- vertex_attr(id_union)
example_id_graph <- id_graphs[[3]]
plot(id_graphs[[3]])
plot(id_union)
ex_vs <- vertex_attr(example_id_graph)
# a list corresponding to vertices where the value corresponds to the earliest vertex with that
# user in the graph
contract_list <- match(ex_vs$user, ex_vs$user)
contract_example <- contract(example_id_graph, contract_list, list("first"))
plot(contract_example)
# worry not about the odd plot.  All those isolated vertices are the duplicate users.  Their 
# edges have been transfered to the "in place" vertices

# Now, to remove them.

# 1:vcount(G) creates a list (1,2,3,..) i.e. the vertex corresponding to list entry
# we subtract that from the list above to get a list of all "out of place" vertices
# A zero in the list corresponds to an "in place" vertex, that is, that users "first" 
# occurance in the graph

# wait a second this duplicates list doesnt actually do what you want, as contract returns 
# a wierd set of vertices!
# either figure out how to remove all vertices with the irksome "character(0)" name, or 
# isolate the largest component, as that seems to be what's important anyway?  Remove all 
# all isolated vertices?
# wait just subset the delete vertices to the right list? 

# OK I fixed it fix the comments
duplicates <- contract_list-1:vcount(example_id_graph)
to_delete <- which(duplicates !=0)
to_delete <- to_delete[to_delete<=length(V(contract_example))]
smushed_example <- delete_vertices(contract_example, to_delete)
plot(smushed_example, vertex.label=NA)
l <- layout_with_kk(smushed_example)
plot(smushed_example, edge.arrow.size=.4, edge.width=0.5, 
     vertex.size=5, vertex.label=NA, layout=l)
example_network <- simplify(smushed_example)
plot(example_network, edge.arrow.size=.4, vertex.label=NA, layout=l)

id_contract_list <- match(id_vs$user, id_vs$user)
id_contract <- contract(id_union, id_contract_list, list("first"))

setwd('C:/Users/joe/Documents/uvm/pocs/wikipedia/watched_pages2')
exg <- read_graph('Aristotle.comment_network.graphml',format='graphml') 
l <- layout_with_kk(exg)
plot(exg, edge.arrow.size=.4, edge.width=0.5, 
     vertex.size=5, vertex.label=NA, layout=l)
ex_vs <- vertex_attr(exg)
# a list corresponding to vertices where the value corresponds to the earliest vertex with that
# user in the graph
contract_list <- match(ex_vs$user, ex_vs$user)
contract_example <- contract(exg, contract_list, list("first"))
l <- layout_with_kk(contract_example)
plot(contract_example,  edge.arrow.size=.4, edge.width=0.5, 
     vertex.size=5, vertex.label=NA, layout=l)
duplicates <- contract_list-1:vcount(exg)
to_delete <- which(duplicates !=0)
to_delete <- to_delete[to_delete<=length(V(contract_example))]
smushed_example <- delete_vertices(contract_example, to_delete)
l <- layout_with_kk(smushed_example)
plot(smushed_example,  edge.arrow.size=.4, edge.width=0.5, 
     vertex.size=5, vertex.label=NA, layout=l)
