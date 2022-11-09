library(shiny)
library(networkD3)
library(dplyr)
ui <- fluidPage(
  selectInput(inputId = "school",
              label   = "School",
              choices =  c("alpha", "echo")),

  sankeyNetworkOutput("diagram")
)

server <- function(input, output) {

  dat <- data.frame(schname = c("alpha", "alpha", "alpha", "echo"),
                    next_schname = c("bravo", "charlie", "delta", "foxtrot"),
                    count = c(1, 5, 3, 4))

  links <- data.frame(source = dat$schname,
                      target = dat$next_schname,
                      value  = dat$count)
  nodes <- data.frame(name = c(as.character(links$source),
                               as.character(links$target)) %>%
                        unique)

  links$IDsource <- match(links$source, nodes$name) - 1
  links$IDtarget <- match(links$target, nodes$name) - 1

  links2 <-reactive({
    links %>%
      filter(source == input$school)
  })


  output$diagram <- renderSankeyNetwork({
    sankeyNetwork(
      Links = links2(),
      Nodes = nodes,
      Source = "IDsource",
      Target = "IDtarget",
      Value = "value",
      NodeID = "name",
      sinksRight = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)
