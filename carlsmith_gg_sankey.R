
feasible <- .65
incentives <- .8
misalined <- .4
powerseeking <- .65
impact <- .4

# Load package
library(networkD3)

carlsmith <- list(nodes = as.character(NA),
                links = as.matrix(NA, ncol = 3))
carlsmith$nodes <- as.data.frame(c("Will APS be feasible?",
                     "Will we want to make them?",
                     "Will it be harder to keep them aligned?",
                     "Will they do a lot of damage?",
                     "Would they disempower us?",
                     "No existential catastrophe",
                     "Existential catastrophe"))
names(carlsmith$nodes) <- "name"

carlsmith$links <- matrix(c(0, 1, feasible,
                            0, 5, 1 - feasible,
                            1, 2, feasible * incentives,
                            1, 5, feasible - feasible * incentives,
                            2, 3, feasible * incentives * misalined,
                            2, 5, feasible * incentives - feasible * incentives * misalined,
                            3, 4, feasible * incentives * misalined * powerseeking,
                            3, 5, feasible * incentives * misalined - feasible * incentives * misalined * powerseeking,
                            4, 6, feasible * incentives * misalined * powerseeking * impact,
                            4, 5, feasible * incentives * misalined * powerseeking - feasible * incentives * misalined * powerseeking * impact),
                    ncol = 3,
                    byrow = TRUE)
carlsmith$links = as.data.frame(carlsmith$links)
names(carlsmith$links) <- c("source", "target", "value")

# Add a 'group' column to each connection:
carlsmith$links$group <- as.factor(c("doom", "fine","doom", "fine","doom", "fine","doom", "fine","doom", "fine"))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
carlsmith$nodes$group <- as.factor(c(rep("grey_nodes",5),
                                    "fine",
                                    "doom"))

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["doom", "fine", "grey_nodes"]) .range(["red", "green", "grey"])'

# Thus we can plot it
p <- sankeyNetwork(Links = carlsmith$links,
            Nodes = carlsmith$nodes, Source = "source",
            Target = "target", Value = "value", NodeID = "name",
            units = "p", fontSize = 12, nodeWidth = 15,
            colourScale=my_color, LinkGroup="group", NodeGroup="group",
            nodePadding = 20, sinksRight = FALSE)
p

# save the widget
p <- sankeyNetwork(Links = links,
                   Nodes = nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   units = "p", fontSize = 12, nodeWidth = 15,
                   colourScale=my_color, LinkGroup="group", NodeGroup="group",
                   nodePadding = 20, sinksRight = FALSE)
p
carlsmith$nodes == nodes
carlsmith$links == links
str(carlsmith$links)
str(links)
