library(shinylive)
library(httpuv)
library(shiny)
library(plotly)
library(dplyr)

# Sample data (replace with your actual data)
bird_data <- readRDS("bird_pars.RDS")

ui <- fluidPage(
  titlePanel("Bird Species Survival Rates"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_species",
        "Select Species:",
        choices = unique(bird_data$sp),
        selected = "Ammodramus aurifrons"
      )
    ),
    mainPanel(
      plotlyOutput("survival_plot")
    )
  )
)

server <- function(input, output) {
  output$survival_plot <- renderPlotly({
    req(input$selected_species)
    
    # Filter data for selected species
    plot_data <- bird_data %>%
      filter(sp == input$selected_species) %>%
      arrange(site, t)  # Ensure proper line connections
    
    # Create interactive plot
    plot_ly(plot_data, 
            x = ~t, 
            y = ~median,
            color = ~site,
            colors = "Set1",  # Color palette
            type = 'scatter',
            mode = 'lines+markers',
            hoverinfo = 'text',
            text = ~paste(
              "<b>", input$selected_species, "</b><br>",
              "Site: ", site, "<br>",
              "Year: ", t, "<br>",
              "Estimate: ", round(median, 3), "<br>",
              "95% CI: (", round(lower, 3), ", ", round(upper, 3), ")"
            ),
            line = list(width = 2),
            marker = list(size = 8)) %>%
      layout(
        title = paste("Survival Rate:", input$selected_species),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Survival Rate", range = c(0, 1)),
        hoverlabel = list(bgcolor = "white", font = list(size = 14)),
        legend = list(title = list(text = "<b>Site</b>"))
      )
  })
}

shinyApp(ui, server)
