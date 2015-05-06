library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(readr)

crime <- read_csv("data/LAPD_Crime_and_Collision_Raw_Data_for_2013.csv")
crime$`Date Rptd` <- parse_date(crime$`Date Rptd`, format = "%m/%d/%Y") #parse_date from readr package
crime$`DATE OCC` <- parse_date(crime$`DATE OCC`, format = "%m/%d/%Y")
crime <- tbl_df(crime)





shinyServer(function(input,output){
  
  ##################for tab panel 1 with top ten crimes###########################
  
  dates.data1 <- reactive({
    
    crime %>% filter(`Date Rptd` < input$date1[2] & `Date Rptd` > input$date1[1]) %>% select(`Date Rptd`, `Crm Cd Desc`,`Status Desc`)
    
  })
  
  adult.juv <- reactive({
    
    if(input$group == "Adult Activity") #not sure if the if else statements are the way to go but they work
    {
      dates.data1() %>% filter(`Status Desc` == "Adult Arrest" | `Status Desc` == "Adult Other") 
    }
    else if(input$group == "Juvenile Activity")
    {
      dates.data1() %>% filter(`Status Desc` == "Juv Arrest" | `Status Desc` == "Juv Other")
    }
    else dates.data1()
    
  })
  
  
  output$topten <- renderPlot({
    grouped <- adult.juv() %>% group_by(`Crm Cd Desc`) %>% summarise(count = n()) %>% arrange(desc(count))
    
    p <- ggplot(grouped[1:10,], aes(x = reorder(`Crm Cd Desc`, count), y = count, fill = reorder(`Crm Cd Desc`,count))) + geom_bar(stat = "identity")
    p <- p + geom_text(aes(label = count), vjust = -.2) + ylab("Number of Occurances") + xlab("") + ggtitle("Top 10 Crimes in LA from January 2013 - May 2014 ")
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.y = element_text(face = "italic", size = 14),
                   plot.title = element_text(face = "bold.italic", size = 16),
                   panel.background = element_rect(fill = "white"),
                   panel.grid.major = element_line(color = "grey"))
    p <- p + guides(fill = guide_legend(reverse = TRUE)) + labs(fill = "Type of Crime") + scale_fill_brewer(palette = "Paired") 
    p
    
  })
  
#######################################################

####################for nav bar panel 2 (crimes)###################

dates.data2 <- reactive({
  
  crime %>% filter(`Date Rptd` < input$date2[2] & `Date Rptd` > input$date2[1]) %>% select(`Date Rptd`, `Crm Cd Desc`,`Status Desc`)
  
})

check <- reactive({
  
  if(input$view == TRUE)
  {
    dates.data2() %>% group_by(`Date Rptd`, `Status Desc`) %>% summarise(count = n())
  }
  else
  {
    dates.data2() %>% group_by(`Date Rptd`) %>% summarise(count = n())
  }
  
})


group.nogroup <- reactive({
  if(input$view == TRUE)
  {
    p <- ggplot(check(), aes(x = `Date Rptd`, y = count, color = `Status Desc`)) + geom_line()
    p <- p + xlab("Date") + ggtitle("Number of Crimes by Date in LA from January 2013 - May 2014") + ylab("Count") + labs(color = "Classification")
    p <- p + theme( axis.title.y = element_text(face = "italic", size = 14),
                    axis.title.x = element_text(face = "italic", size = 14),
                    plot.title = element_text(face = "bold.italic", size = 16)
    )
    p
    
  }
  else
  {
    p <- ggplot(check(), aes(x= `Date Rptd`, y = count)) + geom_line(color = "#006699")
    p <- p + xlab("Date") + ggtitle("Number of Crimes by Date in LA from January 2013 - May 2014") + ylab("Count") 
    p <- p + theme( axis.title.y = element_text(face = "italic", size = 14),
                    plot.title = element_text(face = "bold.italic", size = 16)
    )
    p
    
  }
})

  
output$crime <- renderPlot({
  
  group.nogroup()
  
  
})  

############################################################

###########################Tab panel for arrests##################

dates.data3 <- reactive({
  
  crime %>% filter(`Date Rptd` < input$date3[2] & `Date Rptd` > input$date3[1]) %>% select(`Date Rptd`, `Crm Cd Desc`,`Status Desc`)
  
})


check2 <- reactive({
  
  if(input$view2 == TRUE)
  {
    dates.data3() %>% group_by(`Date Rptd`, `Status Desc`) %>% summarise(count = n()) %>% filter(`Status Desc` == "Adult Arrest" | `Status Desc` == "Juv Arrest")
  }
  
  else
  {
    dates.data3() %>% filter(`Status Desc` == "Adult Arrest" | `Status Desc` == "Juv Arrest") %>% group_by(`Date Rptd`) %>% summarise(count = n()) 
  }
  
})


arrests.data <- reactive({
  
  
  if(input$view2 == TRUE)
  {
    p <- ggplot(check2(), aes(x = `Date Rptd`, y = count, color = `Status Desc`)) + geom_line()
    p <- p + xlab("Date") + ylab("Count") + ggtitle("Number of Arrests by Date in LA from January 2013 - May 2014") + labs(color = "")
    p <- p + theme(axis.title.x = element_text(face = "italic", size = 14),
                   axis.title.y = element_text(face = "italic", size = 14),
                   plot.title = element_text(face = "bold.italic", size = 16))
    p <- p + scale_colour_brewer(palette = "Set2")
    p
    
  }
  
  else
  {
    p <- ggplot(check2(), aes(x = `Date Rptd`, y = count)) + geom_line(color = "#330066")
    p <- p + xlab("Date") + ylab("Count") + ggtitle("Number of Arrests by Date in LA from January 2013 - May 2014") 
    p <- p + theme(axis.title.x = element_text(face = "italic", size = 14),
                   axis.title.y = element_text(face = "italic", size = 14),
                   plot.title = element_text(face = "bold.italic", size = 16))
    p
  }
  
})

output$arrests <- renderPlot({
  
  arrests.data()
  
})

  
  
})