library(shiny)
ui <- fluidPage(
  
  # Application title
  titlePanel("JAKI PROCENT LOTOW MIALO OPZONIENIE WIEKSZE NIZ 15 MIN, PODZIAL NA DNI TYGODNIA"),
  
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
    f2 <- function(frame){
      frame %>%
        select(DayOfWeek, DepDelay) %>%
        na.omit() %>%
        filter(DepDelay > 30) %>%
        group_by(DayOfWeek) %>%
        summarise(CountDelay = n()) -> a
      frame %>% 
        select(DayOfWeek, FlightNum) %>% 
        group_by(DayOfWeek)%>%
        summarize(NumberOfFlights = n()) -> b
      cbind(a,b[2]) -> pom
      frame %>%
        select(DayOfWeek, ArrDelay) %>%
        na.omit() %>%
        filter(ArrDelay > 30) %>%
        group_by(DayOfWeek) %>%
        summarise(CountDelay = n()) -> c
      cbind(c,b[2]) -> pom2
      pom %>% mutate(Percent = 100*CountDelay/NumberOfFlights) -> pom
      pom2 %>% mutate(Percent = 100*CountDelay/NumberOfFlights) -> pom2
      pom %>% mutate(Delay = "Dep") -> pom
      pom2 %>% mutate(Delay = "Arr") -> pom2
      rbind(pom2,pom) -> wynik
      ggplot(data = wynik, aes(x = DayOfWeek, y = Percent, color = Delay))+ geom_line()
    }
    if(input$Year == 1987){
      f2(r1987)
    } else if(input$Year  ==1988){
      f2(r1988)
    }else if(input$Year  ==1989){
      f2(r1989)
    }else if(input$Year  ==1990){
      f2(r1990)
    }else if(input$Year  ==1991){
      f2(r1991)
    }else if(input$Year  ==1992){
      f2(r1992)
    }else if(input$Year  ==1993){
      f2(r1993)
    } else if(input$Year  ==1994){
      f2(r1994)
    }else if(input$Year  ==1995){
      f2(r1995)
    }else if(input$Year  ==1996){
      f2(r1996)
    }else if(input$Year  ==1997){
      f2(r1997)
    }else if(input$Year  ==1998){
      f2(r1998)
    }else if(input$Year  ==1999){
      f2(r1999)
    }else if(input$Year  ==2000){
      f2(r2000)
    }else if(input$Year  ==2001){
      f2(r2001)
    }else if(input$Year  ==2002){
      f2(r2002)
    }else if(input$Year  ==2003){
      f2(r2003)
    }else if(input$Year  ==2004){
      f2(r2004)
    }else if(input$Year  ==2005){
      f2(r2005)
    }else if(input$Year  ==2006){
      f2(r2006)
    }else if(input$Year  ==2007){
      f2(r2007)
    }else{f2(r2008)}
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

