library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)

elephants <- read_csv("data/elephants_data/elephants.csv") %>%
  clean_names()

ui <- dashboardPage(
  
  dashboardHeader(title="African Elephants"),
  
  dashboardSidebar(
    
    radioButtons("y",
                 "Select Variable:",
                 choices = c("age",
                             "height"),
                 selected = "age")
  ),
  
  dashboardBody(
    
    helpText("Source: Phyllis C. Lee, Luc F. Bussière, C. Elizabeth Webber, Joyce H. Poole, Cynthia J. Moss; Enduring consequences of early experiences: 40 year effects on survival and success among African elephants (Loxodonta africana). Biol Lett 23 April 2013; 9 (2): 20130011. https://doi.org/10.1098/rsbl.2013.0011"),
    
    plotOutput("plot", width="600px", height= "500px")
    
  )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    if(input$y == "age"){
      elephants %>% 
        ggplot(aes(x = sex, y = .data[[input$y]], fill=sex))+
        geom_boxplot(alpha=0.6)+
        labs(title="Age by Sex", x="Sex", y="Age (Years)")+
        theme_minimal()+
        theme(plot.title=element_text(size=rel(1.3), hjust=0.5, face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"),
              axis.text.x=element_text(face = "bold"),
              axis.text.y=element_text(face = "bold"),
              legend.position = "none")
    }else{
      elephants %>% 
        ggplot(aes(x = sex, y = .data[[input$y]], fill=sex))+
        geom_boxplot(alpha=0.6)+
        labs(title="Height by Sex", x="Sex", y="Height (cm)")+
        theme_minimal()+
        theme(plot.title=element_text(size=rel(1.3), hjust=0.5, face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"),
              axis.text.x=element_text(face = "bold"),
              axis.text.y=element_text(face = "bold"),
              legend.position = "none")
    }
  })
}

shinyApp(ui, server)