library(shinylive)
library(httpuv)
library(shiny)
library(plotly)
library(dplyr)
library(tidyverse)

# Sample data (replace with your actual data)
bird_data <- readRDS("bird_pars.RDS")
cap_data <- read.csv("n.cap.csv")
cap_data <- cap_data %>% pivot_longer(cols = 4:49, 
                                      names_to = 'year', 
                                      values_to = 'n') %>%
  mutate(year = gsub("X", "", year))

ui <- fluidPage(
  titlePanel("Bird Population Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_species",
        "Select Species:",
        choices = unique(bird_data$sp),
        selected = "Ammodramus aurifrons"
      ),
      width = 3  # Narrower sidebar
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Survival Rates", plotlyOutput("survival_plot")),
        tabPanel("Capture Counts", plotlyOutput("capture_heatmap"))
      ),
      width = 9  # Wider main panel
    )
  )
)

server <- function(input, output) {
  
  output$survival_plot <- renderPlotly({
    req(input$selected_species)
    
    plot_data <- bird_data %>%
      filter(sp == input$selected_species) %>%
      arrange(site, t)
    
    plot_ly(plot_data,
            x = ~t, 
            y = ~median,
            color = ~site,
            type = 'scatter',
            mode = 'lines+markers',
            hoverinfo = 'text',
            text = ~paste0(
              "<b>", input$selected_species, "</b><br>",
              "Site: ", site, "<br>",
              "Year: ", t, "<br>",
              "Estimate: ", round(median, 3), "<br>",
              "95% CI: (", round(lower, 3), ", ", round(upper, 3), ")"
            )) %>%
      layout(
        title = paste("Survival Rate:", input$selected_species),
        yaxis = list(range = c(0, 1))
      )
  })
  
  # New capture heatmap
  output$capture_heatmap <- renderPlotly({
    req(input$selected_species)
    
    heatmap_data <- cap_data %>%
      complete(sp, year, si, fill = list(n = 0)) %>%
      filter(sp == input$selected_species)
    
    plot_ly(
      data = heatmap_data,
      x = ~si,
      y = ~year,
      z = ~n,
      color = "RdPu",
      type = "heatmap",
      hoverinfo = "text",
      text = ~paste0(
        "<b>", input$selected_species, "</b><br>",
        "Site: ", si, "<br>",
        "Year: ", year, "<br>",
        "Captures: ", n
      )
    ) %>%
      layout(
        title = paste("Capture Counts:", input$selected_species),
        xaxis = list(title = "Site"),
        yaxis = list(title = "Year", autorange = "reversed"),  # Newest year on top
        margin = list(l = 100, r = 50)  # Adjust margins
      ) %>%
      colorbar(title = "Captures")
  })
}

shinyApp(ui, server)