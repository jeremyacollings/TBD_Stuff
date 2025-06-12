library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Your existing data loading code
bird_data <- readRDS("phi(sp,si,ti)p(sp)PARS.rds")
sp_table <- read.csv("sp_table.csv")
si_table <- read.csv("si_table.csv")
cap_data <- read.csv("n.cap.csv")
cap_data <- cap_data %>% pivot_longer(cols = 4:49, 
                                      names_to = 'year', 
                                      values_to = 'n') %>%
  mutate(year = gsub("X", "", year))

bird_data <- bird_data %>%
  mutate(sp = sp_table$sp[s], 
         si = si_table$si[si], 
         ti = cap_data$year[ti])

all_sites <- unique(bird_data$si)
site_colors <- setNames(
  c("#004468", "#a2260d", "#2d50bc", "#6f7500", "#975a00", 
    "#8c4f87", "#88a46d", "#fe5e9e", "#0c0003", "#c08bab"),  # Or choose another palette
  all_sites
)

ui <- fluidPage(
  titlePanel("Tropical Bird CJS Model"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_species",
        "Select Species:",
        choices = unique(bird_data$sp),
        selected = "Ammodramus aurifrons"
      ),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Survival Rates", plotlyOutput("survival_plot")),
        tabPanel("Capture Counts", plotlyOutput("capture_heatmap"))
      ),
      width = 9
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to get sites where the species was captured
  captured_sites <- reactive({
    req(input$selected_species)
    cap_data %>%
      filter(sp == input$selected_species, n > 0) %>%
      distinct(si) %>%
      pull(si)
  })
  
  output$survival_plot <- renderPlotly({
    req(input$selected_species)
    
    # Get the sites where this species was captured
    sites_with_captures <- captured_sites()
    
    plot_data <- bird_data %>%
      filter(sp == input$selected_species, si %in% sites_with_captures) %>%
      arrange(si, ti)
    
    # Only proceed if there's data to plot
    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>% 
               layout(title = paste("No capture data available for", input$selected_species)))
    }
    
    plot_sites <- unique(plot_data$si)
    colors_to_use <- site_colors[names(site_colors) %in% plot_sites]
    
    plot_ly(plot_data,
            x = ~ti, 
            y = ~median,
            color = ~si,
            colors = colors_to_use,
            type = 'scatter',
            mode = 'lines+markers',
            line=list(width=3),
            marker=list(size=10),
            hoverinfo = 'text',
            text = ~paste0(
              "<b>", input$selected_species, "</b><br>",
              "Site: ", si, "<br>",
              "Year: ", ti, "<br>",
              "Estimate: ", round(median, 3), "<br>",
              "95% CI: (", round(lower, 3), ", ", round(upper, 3), ")"
            )) %>%
      layout(
        title = paste("Annual Apparent Survival:", input$selected_species),
        yaxis = list(title = "Apparent Survival",range = c(0, 1)),
        xaxis = list(title = "Year")
      )
  })
  
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
        yaxis = list(title = "Year", autorange = "reversed"),
        margin = list(l = 100, r = 50)
      ) %>%
      colorbar(title = "Captures")
  })
}

shinyApp(ui, server)