# Library
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

# data
data <- read.csv("File Path")
# UI
ui <- fluidPage(
  titlePanel("Global Energy and Its Economic Impact"),
  fluidRow(
    column(6, selectInput("countrySelect", "Select Country", choices = unique(data$Entity))),  
    column(6, sliderInput("yearRange", "Select Year Range", min = min(data$Year), max = max(data$Year), value = c(min(data$Year), max(data$Year)))) 
  ),
  tabsetPanel(
    tabPanel("CO2 Emissions by Country",
             plotlyOutput("co2Plot"),
             plotlyOutput("energyPerCapitaPlot")
    ),
    tabPanel("Renewable vs Fossil Fuel Usage",
             plotlyOutput("energyPlot")
    ),
    tabPanel("GDP Growth and Per Capita",
             plotlyOutput("gdpBarPlot"),
             plotlyOutput("gdpLinePlot")
    )
  )
)

# server
server <- function(input, output) {
  output$co2Plot <- renderPlotly({
    filtered_data <- data %>%
      filter(Entity == input$countrySelect, Year >= input$yearRange[1], Year <= input$yearRange[2])
    gg <- ggplot(filtered_data, aes(x = Year, y = Value_co2_emissions_kt_by_country, fill = Entity)) +
      geom_bar(stat = "identity") +
      labs(title = "CO2 Emissions by Country", x = "Year", y = "CO2 Emissions (kt)") +
      guides(fill = FALSE)
    ggplotly(gg)
  })
  
  output$energyPerCapitaPlot <- renderPlotly({
    filtered_data <- data %>%
      filter(Entity == input$countrySelect, Year >= input$yearRange[1], Year <= input$yearRange[2])
    gg <- ggplot(filtered_data, aes(x = Year, y = Primary.energy.consumption.per.capita..kWh.person., color = Entity)) +
      geom_line() +
      labs(title = "Energy Consumption per Capita", x = "Year", y = "Energy Consumption (kWh/person)") +
      scale_color_manual(values = "black") +
      guides(color = FALSE) 
    ggplotly(gg)
  })
  
  output$energyPlot <- renderPlotly({
    filtered_data <- data %>%
      filter(Entity == input$countrySelect, Year >= input$yearRange[1], Year <= input$yearRange[2],
             !is.na(Electricity.from.fossil.fuels..TWh.), !is.na(Electricity.from.renewables..TWh.)) %>%
      select(Year, Electricity.from.fossil.fuels..TWh., Electricity.from.renewables..TWh.) %>%
      pivot_longer(cols = c("Electricity.from.fossil.fuels..TWh.", "Electricity.from.renewables..TWh."), names_to = "Type", values_to = "Value")
    
    p <- ggplot(filtered_data, aes(x = Year, y = Value, color = Type)) +
      geom_line() +
      labs(title = "Energy Usage: Fossil Fuels vs Renewable Energy",
           y = "Energy Consumption (TWh)", x = "Year") +
      scale_color_manual(values = c("Electricity.from.fossil.fuels..TWh." = "red", "Electricity.from.renewables..TWh." = "green"))
    ggplotly(p)
  })
  
  output$gdpBarPlot <- renderPlotly({
    filtered_data <- data %>%
      filter(Entity == input$countrySelect, Year >= input$yearRange[1], Year <= input$yearRange[2])
    gg <- ggplot(filtered_data, aes(x = Year, y = gdp_growth, fill = Entity)) +
      geom_bar(stat = "identity") +
      labs(title = "GDP Growth by Year", x = "Year", y = "GDP Growth (%)") +
      scale_fill_manual(values = "darkgreen") +
      guides(fill = FALSE) 
    ggplotly(gg)
  })
  
  output$gdpLinePlot <- renderPlotly({
    filtered_data <- data %>%
      filter(Entity == input$countrySelect, Year >= input$yearRange[1], Year <= input$yearRange[2])
    gg <- ggplot(filtered_data, aes(x = Year, y = gdp_per_capita, color = Entity)) +
      geom_line() +
      labs(title = "GDP Per Capita by Year", x = "Year", y = "GDP Per Capita (USD)") +
      scale_color_manual(values = "purple") +
      guides(color = FALSE) 
    ggplotly(gg)
  })
}

# Run 
shinyApp(ui = ui, server = server)
