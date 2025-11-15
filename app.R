#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

source("cleaning.R")

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           selectInput(
             inputId = "hospital",
             label = "Filter Hospital",
             choices = linelist_vis$hospital_ord,
             selected = "All",
           )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ourPlot"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    server_plot <- function(data, hospital_filter = "All") {
        
      if (!("All" %in% hospital_filter)) {            
        data <- data %>%
          filter(hospital_ord == hospital_filter)
      }
        ggplot(
          data = data, 
            mapping = 
            aes(
              x = age_cat5, 
              y = wt_kg
            ))+ 
          geom_boxplot()+ 
          scale_color_brewer(palette="OrRd")
    }
    
    output$ourPlot <- renderPlot({
        server_plot(linelist_vis, hospital_filter = input$hospital)
    })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
