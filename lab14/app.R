library("tidyverse")
library("shiny")

ui_homerange <- fluidPage (
  
  selectInput("x", "Select fill variable", choices = c("trophic.guild", "thermoregulation"), selected = "trophic.guild"),
  plotOutput("plot", width = "500px", height = "400px")
  
)

server_homerange <- function(input, output) {
  output$plot <- renderPlot({
    
    ggplot(data= homerange, aes_string(x = "locomotion", fill = input$x)) + geom_bar(position = "dodge", alpha = .8, color = "black") + labs(x = NULL, fill = "Fill Variable") + theme_light(base_size = 18)
    
  })
  
  
}

shinyApp(ui_homerange, server_homerange)
