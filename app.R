library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(plotly)
library(DT)

sales <- read_csv("sales_join.csv")

temp <- sales %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    quarter = date(floor_date(x = OrderDate, unit = "quarter"))
  ) %>% 
  group_by(quarter) %>% 
  summarise(
  COS = sum(COS),
  GP = sum(GP),
  Quantity = sum(Quantity)
  ) %>%
  ungroup() %>% 
  tail(1)

topcat <- sales %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    quarter = date(floor_date(x = OrderDate, unit = "quarter"))
  ) %>% 
  group_by(quarter, CategoryName) %>% 
  summarise(
    Quantity = sum(Quantity)
  ) %>% 
  ungroup() %>% 
  arrange(desc(Quantity)) %>% 
  filter(quarter == "2015-10-01") %>% 
  select(CategoryName, Quantity)


ui <- shinyUI(
  dashboardPagePlus(
    skin = "black-light",
    title = "Ngulik Data Dashboard",
    dashboardHeaderPlus(
      
      title = "Ngulik Data Dashboard"
      
    ),
    dashboardSidebar(
      
      sidebarMenu(
        menuItem(
          text = "Overview", 
          tabName = "page1",
          badgeLabel = "new", 
          badgeColor = "green",
          icon = icon("gears")
        )
      )
      
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "page1",

          fluidRow(
            boxPlus(
              title = "Overview Dashboard", 
              closable = TRUE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 12,
              
              infoBoxOutput(
                outputId = "cos", 
                width = 4
              ),
              
              infoBoxOutput(
                outputId = "gp",
                width = 4
              ),
              
              infoBoxOutput(
                outputId = "quantity",
                width = 4
              )
            )
          ),
          
          fluidRow(
            
            boxPlus(
              title = "Select Category Name", 
              closable = TRUE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 3,
              height = "600px",
              
              selectInput(
                inputId = "catname", 
                label = "Select Category Name", 
                choices = c(topcat$CategoryName),
                selected = "Sportwear"
              ),
              
              dataTableOutput(
                outputId = "topcat"
              )
            ),
            
            boxPlus(
              title = "Graph", 
              closable = TRUE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 9,
              height = "600px",
              
              plotlyOutput(
                outputId = "graph"
              )
            )
          )
        )
      )
    )
  )
)

server <- shinyServer(
  function(input, output){
    
    
    
    output$cos <- renderInfoBox({
      
     infoBox(title = "COS",
             value = temp %>% pull(COS), 
             subtitle = "Sum Cost of Sales", 
             color = "red",
             icon = icon("search-dollar"),
             fill = TRUE)
      
    })
    
    output$gp <- renderInfoBox({
      
      infoBox(title = "GP",
              value = temp %>% pull(GP),
              subtitle = "Sum of Gross Profit",
              color = "green",
              icon = icon("dollar-sign"),
              fill = TRUE)
      
    })
    
    output$quantity <- renderInfoBox({
      
      infoBox(title = "Quantity", 
              value = temp %>% pull(Quantity),
              subtitle = "Sum of Quantity",
              color = "blue",
              fill = TRUE)
      
    })
    

    
    output$topcat <- renderDataTable({
      
      datatable(topcat)
      
    })
    
    output$graph <- renderPlotly({
      
      
      top_country <- sales %>% 
        filter(CategoryName == input$catname) %>% 
        count(Country, sort = T) %>% 
        filter(Country != "United States") %>% 
        head(3) %>% 
        pull(Country)
      
      data_viz <- sales %>% 
        filter(
          Country %in% top_country,
          CategoryName == input$catname
        ) %>% 
        mutate(
          month = as_date(floor_date(OrderDate, unit = "month"))
        ) %>% 
        group_by(month, Country) %>% 
        summarise(Quantity = sum(Quantity)) %>% 
        group_by(Country) %>% 
        pad() %>% 
        ungroup() %>% 
        mutate(
          quantity_amount = replace_na(Quantity, 0),
          Country = as.factor(Country)
        )
      
      plot1 <- data_viz %>% 
        ggplot(mapping = aes(x = month, y = Quantity)) +
        geom_area(
          aes(fill = Country),
          alpha = 0.65
        ) +
        scale_fill_manual(values = c("red", "dodgerblue", "pink")) +
        labs(
          title = "Monthly quantity amount dynamics",
          caption = "Source: Daqing Chen, Sai Liang Sain",
          x = NULL,
          y = NULL
        ) +
        facet_wrap(facets = vars(Country), nrow = 3, scales = "free_y") +
        theme_minimal() +
        theme(
          legend.position = "top"
        )
      
      ggplotly(plot1) %>% 
        config(displayModeBar = F) %>% 
        layout(autosize = TRUE, height = 540) 
    })
  }
)

shinyApp(ui = ui, server)