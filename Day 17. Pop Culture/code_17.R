# visualizacion de datos ----
library(tidyverse)
library(igraph)
library(networkD3)

# Datos ----
nodos = readxl::read_xlsx("relaciones One Piece.xlsx",
                          sheet = "nodos")

nodos %>%
  group_by(personaje) %>%
  count() %>%
  arrange(-n)

enlaces = readxl::read_xlsx("relaciones One Piece.xlsx",
                            sheet = "conexiones")

unique(enlaces$origen)[!(unique(enlaces$origen) %in% unique(nodos$personaje))]
unique(enlaces$destino)[!(unique(enlaces$destino) %in% unique(nodos$personaje))]

# Armamos los datos de la red ----
network <- graph_from_data_frame(d=enlaces,
                                 vertices=nodos,
                                 directed=F)
class(network)
plot(network)

# Generamos datos compatibles para un force Network
Links = networkD3::igraph_to_networkD3(network)[[1]]
Nodes = networkD3::igraph_to_networkD3(network)[[2]]
Nodes$size = nodos$tamaño
class(Nodes)
Nodes$group = nodos$tipo

# Funcion para presionarle a algun nodo de interes
MyClickScript <- 'alert("Seleccionaste a  " + d.name + " que esta en la fila " +
       (d.index + 1) +  " de tu dataframe original");'

net = forceNetwork(Links = Links,
                   Nodes = Nodes,
                   Nodesize = "size",
                   NodeID = "name",
                   Group = "group",
                   Value = "value",
                   # height="550px",
                   # width="350px",
                   radiusCalculation = JS("Math.sqrt(d.nodesize)+6"),
                   linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                   charge = -900,              # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 14,
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   linkWidth = JS("function(d) {return d.linkWidth;}+2"),
                   zoom = T,
                   opacityNoHover = TRUE,
                   clickAction = MyClickScript,
                   colourScale = JS('d3.scaleOrdinal().range(["salmon","red","orange"]);'))

net


htmlwidgets::saveWidget(net, "OPnetworkD3.html")
