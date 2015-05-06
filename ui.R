library(shiny)


shinyUI(navbarPage("LA Crime",
                
  tabPanel("Top 10 Crimes",  
      plotOutput("topten"),
      hr(),
      
      fluidRow(
              column(3,
                      radioButtons("group", label = "View", choices = list("All (including ongoing investigations)", "Adult Activity" , "Juvenile Activity" ), selected = "All (including ongoing investigations)")),
              column(3,
                      h6("Adult and Juvenile activity are for closed cases. There are continuing investigations that are not recorded yet for their activity")),
              column(5,
                      dateRangeInput("date1", label = "Select what dates to view from", start = as.Date("2013-01-01"), end = as.Date("2014-05-28"), min = as.Date("2013-01-01"), max = as.Date("2014-05-28")))
               )
        ),
  tabPanel("Crimes Time Series",
           plotOutput("crime"),
           hr(),
           
           fluidRow(column(5,
                           dateRangeInput("date2", label = "Select what dates to view from", start = as.Date("2013-01-01"), end = as.Date("2014-05-28"), min = as.Date("2013-01-01"), max = as.Date("2014-05-28")),
                           checkboxInput("view", label = "View Groups", value = FALSE)
                           ))
           ),
  tabPanel("Arrests Time Series",
           plotOutput("arrests"),
           hr(),
           fluidRow(column(5,
                           dateRangeInput("date3", label = "Select what dates to view from", start = as.Date("2013-01-01"), end = as.Date("2014-05-28"), min = as.Date("2013-01-01"), max = as.Date("2014-05-28")),
                           checkboxInput("view2", label = "Split Juevenile and Adult Arrests", value = FALSE)
                           
                           )
                           )
           
           )

))