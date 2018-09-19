library(shiny)
library(gentelellaShiny)

shinyApp(
  ui = gentelellaPage(
    gentelellaBody()
  ),
  server = function(input, output, session) {}
)
