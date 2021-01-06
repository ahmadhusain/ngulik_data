

# global ------------------------------------------------------------------


# load library ------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(lubridate)
library(padr)
library(plotly)
library(shinythemes)
library(sever)
library(shinyjs)
library(shinyURL)


# credential users --------------------------------------------------------

credentials <- data.frame(
  user = c("user1", "user2"),
  pass = c("xxx", "zzz")
)


# data wrangling ----------------------------------------------------------


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



# UI ----------------------------------------------------------------------


ui <- shinyUI(
  
  fluidPage(
    
    use_sever(),


# Login menu  -------------------------------------------------------------

    
    tags$head(
      tags$link(rel="stylesheet", type="text/css",href="style.css"),
      tags$script(type="text/javascript", src = "md5.js"),
      tags$script(type="text/javascript", src = "passwdInputBinding.js")    ),
    useShinyjs(),
    
    titlePanel(
      "Ngulik Data: Shiny Dashboard",
      tags$head(
        tags$link(rel = "icon", type = "image/png", href = "logo_algo.png"),
        tags$title("Series - Shiny Dashboard")
      )
    ),
    br(),
    br(),
    

# UI output ---------------------------------------------------------------

    uiOutput("app")
  )
)

server <- function(input, output, session) {
  

# conditional login -------------------------------------------------------

  
  sever(
    bg_color = "black"
  )
  
  shinyURL.server()
  
  USER <- reactiveValues(Logged = FALSE)
  
  
  observeEvent({input$.login  
    input$ENTERKeyPressed}, {
      if (isTRUE(nrow(credentials %>% 
                      filter(user == input$.username & pass == input$.password)) != 0)) {
        USER$Logged <- TRUE
      } else {
        show("message")
        output$message = renderText("Invalid user name or password")
        delay(2000, hide("message", anim = TRUE, animType = "fade"))
      }
    }
  )


# render UI ---------------------------------------------------------------

    
  
  output$app <- renderUI(
    
    if (!isTRUE(USER$Logged)) {
      fluidRow(
        
        
        tags$script(
          '$(document).on("keyup", function(e) {
            if(e.keyCode == 13){Shiny.onInputChange("ENTERKeyPressed", Math.random());}
        });'
        ),
        
        column(width=4, offset = 4,
               wellPanel(id = "login",
                         textInput(".username", "Username:"),
                         passwordInput(".password", "Password:"),
                         div(actionButton(".login", "Log in"), style="text-align: center;")
               ),
               textOutput("message")
        ))
    } else {
      dashboardPagePlus(
        skin = "black-light",
        title = "Ngulik Data Dashboard",
        dashboardHeaderPlus(
          title = ""
        ),
        dashboardSidebar(
          sidebarMenu(
            menuItem(
              text = "Overview",
              tabName = "page1",
              badgeLabel = "new",
              badgeColor = "green",
              icon = icon("gear")
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
                  status = "primary",
                  solidHeader = TRUE,
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
                  status = "primary",
                  solidHeader = FALSE,
                  width = 3,
                  
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
                  status = "primary",
                  solidHeader = FALSE,
                  width = 9,
                  
                  plotlyOutput(
                    outputId = "graph"
                  )
                  
                )
              )
              
            )
          )
        )
      )
    }
  )
 
  output$cos <- renderInfoBox({
    
    infoBox(
      title = "COS",
      value = temp %>% pull(COS),
      subtitle = "Total Cost of Sales",
      color = "red",
      icon = icon("search-dollar"),
      fill = TRUE
    )
    
  })
  
  output$gp <- renderInfoBox({
    
    infoBox(
      title = "Gross Profit",
      value = temp %>% pull(GP),
      subtitle = "Total of Gross Profit",
      color = "green",
      icon = icon("dollar-sign"),
      fill = TRUE
    )
    
  })
  
  output$quantity <- renderInfoBox({
    
    infoBox(
      title = "Quantity",
      value = temp %>% pull(Quantity),
      subtitle = "Total of Quantity",
      color = "blue",
      fill = TRUE
    )
    
    
  })
  
  output$topcat <- renderDataTable({
    
    datatable(topcat)
    
  })
  
  output$graph <- renderPlotly({
    
    
    top_country <- sales %>% 
      filter(CategoryName == input$catname) %>% 
      group_by(Country) %>% 
      summarise(Quantity = sum(Quantity)) %>% 
      arrange(desc(Quantity)) %>% 
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
      summarise(GP = sum(GP)) %>% 
      group_by(Country) %>% 
      pad() %>% 
      ungroup() %>% 
      mutate(
        Country = as.factor(Country)
      )
    
    plot1 <- data_viz %>% 
      ggplot(mapping = aes(x = month, y = GP)) +
      geom_area(
        aes(fill = Country),
        alpha = 0.65
      ) +
      scale_fill_manual(values = c("red", "dodgerblue", "pink")) +
      labs(
        title = "Monthly Gross Profit amount dynamics",
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

shinyApp(ui, server)