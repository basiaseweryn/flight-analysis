#SHINY
library(shiny)
r1992
ui <- fluidPage(
  
  # Application title
  titlePanel("opoznienie w dniach tygodnia na przestrzeni lat"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Year",
                  "Year:",
                  min = 1987,
                  max = 2008,
                  value = 2000)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    f1<- function(frame){
      frame %>%
        select(DayOfWeek, ArrDelay, DepDelay) %>%
        na.omit() %>% 
        group_by(DayOfWeek) %>%
        summarise(MeanArrDelay = mean(ArrDelay),MeanDepDelay = mean(DepDelay)) -> y
      
      meanarrdelay <- data.frame(DayOfWeek = 1:7, MeanDelay = y$MeanArrDelay,
                                 Delay = "arr")
      meandepdelay <- data.frame(DayOfWeek = 1:7, MeanDelay = y$MeanDepDelay,
                                 Delay = "dep")
      z <- rbind(meanarrdelay, meandepdelay)
      
      ggplot(data = z, aes(x = DayOfWeek , y = MeanDelay, color = Delay)) +
        geom_line()
    }
    if(input$Year == 1987){
      f1(r1987)
    } else if(input$Year  ==1988){
      f1(r1988)
    }else if(input$Year  ==1989){
      f1(r1989)
    }else if(input$Year  ==1990){
      f1(r1990)
    }else if(input$Year  ==1991){
      f1(r1991)
    }else if(input$Year  ==1992){
      f1(r1992)
    }else if(input$Year  ==1993){
      f1(r1993)
    } else if(input$Year  ==1994){
      f1(r1994)
    }else if(input$Year  ==1995){
      f1(r1995)
    }else if(input$Year  ==1996){
      f1(r1996)
    }else if(input$Year  ==1997){
      f1(r1997)
    }else if(input$Year  ==1998){
      f1(r1998)
    }else if(input$Year  ==1999){
      f1(r1999)
    }else if(input$Year  ==2000){
      f1(r2000)
    }else if(input$Year  ==2001){
      f1(r2001)
    }else if(input$Year  ==2002){
      f1(r2002)
    }else if(input$Year  ==2003){
      f1(r2003)
    }else if(input$Year  ==2004){
      f1(r2004)
    }else if(input$Year  ==2005){
      f1(r2005)
    }else if(input$Year  ==2006){
      f1(r2006)
    }else if(input$Year  ==2007){
      f1(r2007)
    }else{f1(r2008)}
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)