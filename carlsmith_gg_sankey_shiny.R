# Load packages
library(shiny)
library(shinydashboard)
library(networkD3)

ui <- shinyUI(fluidPage(

  titlePanel("What do you think will happen with AI?"),

  sidebarLayout(
    sidebarPanel(
      h5("From 0 = impossible to 1 = certain, what do you think is the probility that..."),
      sliderInput("feasible", "Advanced, Planning, and Strategically-aware systems will be feasible?",
                  value = 0.65, min = 0.01, max = 1, step = .01),
      sliderInput("incentives", "We will make them, despite the risks?",
                  value = 0.8, min = 0.01, max = 1, step = .01),
      sliderInput("misaligned", "If we make them, they would be hard to align?",
                  value = 0.4, min = 0.01, max = 1, step = .01),
      sliderInput("powerseeking", "If misaligned, they would do a lot of damage by seeking power?",
                  value = 0.65, min = 0.01, max = 1, step = .01),
      sliderInput("impact", "If they do a lot of damage, they could disempower us?",
                  value = 0.4, min = 0.01, max = 1, step = .01),
      width = 3),
    mainPanel(
      uiOutput('preamble'),
      tags$head(tags$style("#preamble{font-size: 16px;
                                 font-style: italic;
                                 }"
      )),
      sankeyNetworkOutput('plot'),#, width = "1400px", height = "600px"),
      textOutput('result_as_text'),
      tags$head(tags$style("#result_as_text{font-size: 16px;
                                 font-style: italic;
                                 }"
      )
      )
    )
    #tableOutput('table')
  )
))

server <- function(input, output) {
  nodes <- as.data.frame(c("Will APS be feasible?",
                                     "Will we want to make them?",
                                     "Will it be harder to keep them aligned?",
                                     "Could they do a lot of damage?",
                                     "Would they disempower us?",
                                     "No catastrophic outcome",
                                     "Existential catastrophe"))
  names(nodes) <- "name"

  links <- reactive({
    x <- as.data.frame(matrix(c(0, 1, input$feasible, "doom",
                           0, 5, 1 - input$feasible, "fine",
                           1, 2, input$feasible * input$incentives, "doom",
                           1, 5, input$feasible - input$feasible * input$incentives, "fine",
                           2, 3, input$feasible * input$incentives * input$misaligned, "doom",
                           2, 5, input$feasible * input$incentives - input$feasible * input$incentives * input$misaligned, "fine",
                           3, 4, input$feasible * input$incentives * input$misaligned * input$powerseeking, "doom",
                           3, 5, input$feasible * input$incentives * input$misaligned - input$feasible * input$incentives * input$misaligned * input$powerseeking, "fine",
                           4, 6, input$feasible * input$incentives * input$misaligned * input$powerseeking * input$impact, "doom",
                           4, 5, input$feasible * input$incentives * input$misaligned * input$powerseeking - input$feasible * input$incentives * input$misaligned * input$powerseeking * input$impact, "fine"),
                         ncol = 4,
                         byrow = TRUE))
    names(x) <- c("source", "target", "value", "group")
    x[,1:3] <- lapply(x[ ,1:3], as.numeric)
    x[, 4] <- as.factor(x[, 4])
    return(x)
  })
  p_as_table <- data.frame(prob = as.numeric(c(.9, .85, 1/2, 1/6, 1/7, 1/11, 1/31, 1/51, 1/101, 1/1001, 1/10001, 1/1000001, 1/1000000001)),
                           what = as.character(c("chance you're not really estimating existental risk, but are trying to be cheeky and see how high these statements can go",
                                                 "chance of a US citizen finishing high school",
                                                 "chance of flipped coin landing heads",
                                                 "chance of death in russian roulette",
                                                 "chance of death for a pedestrian hit by a car traveling at 45 kph",
                                                 "chance that a smoker’s habit will kill them before age 60",
                                                 "chance that having unprotected sex once will lead to pregnancy",
                                                 "chance of death for a US solider serving in the Vietnam War for one year",
                                                 "chance that your spouse cheated on you last month",
                                                 "chance of death for a US soldier deployed to the Iraq War for 4 years",
                                                 "chance of death from riding a motorbike 1100 kilometres",
                                                 "chance that you will die a violent or accidental death today",
                                                 "chance of being struck by lightning this week"
                                                 )))
  url <- a("Click this heading for the full report, or change the numbers here to match your views.",
           href = "https://arxiv.org/pdf/2206.13353.pdf")

  output$preamble <- renderUI({
     tagList("This visualisation represents our probability of surviving the transiton to a world with advanced artificial intelligence. Hopefully we're fine, but default numbers below come from Carlsmith's (2021) report for Open Philanthropy. ", url)
  })

  output$result_as_text <- renderText({
    paste("You think there's a ",
          round(links()$value[9]*100,2),
          "% chance of an existential catastrophe from AI, roughly the ",
          p_as_table$what[which.min(abs(p_as_table$prob - links()$value[9]))],
          sep = "")
  })


  # Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
  nodes$group <- as.factor(c(rep("grey_nodes",5),
                                       "fine",
                                       "doom"))

  # Give a color for each group:
  my_color <- 'd3.scaleOrdinal() .domain(["doom", "fine", "grey_nodes"]) .range(["red", "green", "grey"])'

  output$table <- renderTable(links())
  output$plot <- renderSankeyNetwork({
    sankeyNetwork(Links = links(),
                  Nodes = nodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  fontSize = 14, nodeWidth = 15,
                  colourScale = my_color, LinkGroup="group", NodeGroup="group",
                  nodePadding = 20, sinksRight = FALSE)
  })

}

shinyApp(ui, server)
