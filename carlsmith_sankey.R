library(networkD3)
library(dplyr)

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("By 2070",
  "By 2070",
    "it will possible and financially feasible to build Advanced, Planning, Strategically aware—systems.",
  "It will become possible and financially feasible to build APS systems."
  ), 
  target=c("It will not become possible and financially feasible to build APS systems.",
  "It will become possible and financially feasible to build APS systems.",
    "Power seeking AI is not an existential risk.",
  "Power seeking AI is an existential risk."), 
  value=c(35, 65, 35, 65))
  )
 
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
  as.character(links$target)) %>% unique()
)
 
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
 
# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE)
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyBasic1.html"))