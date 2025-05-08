# NAJWIEKSZE OPOZNIENIA W DNNIACH TYGODNIA - PODZIAL NA LATA, WYKRES Z 
# PODZIALEM NA LATA, POZNIEJ DANE ZMERGOWANE ZE WSZYSTKICH LAT I KOLEJNY
# WYKRES NA DNI TYGODNIA, EWENTUALNIE POROWNANIE W OKRESACH
# SWIATECZNYCH, WAKACYJNYCH

library(tidyverse)
library(dplyr)
library(ggplot2)
library(shiny)

setwd('C:\\Users\\basiu\\OneDrive\\Pulpit\\sem 2\\PDU\\pd4')
r1987 <- read.csv("1987.csv.bz2")
r1988 <- read.csv("1988.csv.bz2")
r1989 <- read.csv("1989.csv.bz2")
r1990 <- read.csv("1990.csv.bz2")
r1991 <- read.csv("1991.csv.bz2")
r1992 <- read.csv("1992.csv.bz2")
r1993 <- read.csv("1993.csv.bz2")
r1994 <- read.csv("1994.csv.bz2")
r1995 <- read.csv("1995.csv.bz2")
r1996 <- read.csv("1996.csv.bz2")
r1997 <- read.csv("1997.csv.bz2")
r1998 <- read.csv("1998.csv.bz2")
r1999 <- read.csv("1999.csv.bz2")
r2000 <- read.csv("2000.csv.bz2")
r2001 <- read.csv("2001.csv.bz2")
r2002 <- read.csv("2002.csv.bz2")
r2003 <- read.csv("2003.csv.bz2")
r2004 <- read.csv("2004.csv.bz2")
r2005 <- read.csv("2005.csv.bz2")
r2006 <- read.csv("2006.csv.bz2")
r2007 <- read.csv("2007.csv.bz2")
r2008 <- read.csv("2008.csv.bz2")
r2008

data_list <- list(r1987,r1988,r1989,r1990,r1991,r1992,r1993,r1994,r1995,
                  r1996,r1997,r1998,r1999,r2000,r2001,r2002,r2003,r2004,
                  r2005,r2006,r2007,r2008)
lapply(data_list, as.data.frame)
is.data.frame(data_list[1])

# "Year"              "Month"             "DayofMonth"        "DayOfWeek"         "DepTime"          
# "CRSDepTime"        "ArrTime"           "CRSArrTime"        "UniqueCarrier"     "FlightNum"        
# "TailNum"           "ActualElapsedTime" "CRSElapsedTime"    "AirTime"           "ArrDelay"         
# "DepDelay"          "Origin"            "Dest"              "Distance"          "TaxiIn"           
# "TaxiOut"           "Cancelled"         "CancellationCode"  "Diverted"          "CarrierDelay"     
# "WeatherDelay"      "NASDelay"          "SecurityDelay"     "LateAircraftDelay"

         
#SREDNIE OPOZNIENIE Z DNIACH TYGODNIA
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
  ggplot(data = z, aes(x = DayOfWeek , y = MeanDelay, color = Delay)) + geom_line()
  
}
f1(r1987)
f1(r1988)
f1(r1989)
f1(r1990)
f1(r1991)
f1(r1992)
f1(r1993)
f1(r1994)
f1(r1995)
f1(r1996)
f1(r1997)
f1(r1998)
f1(r1999)
f1(r2000)
f1(r2001)
f1(r2002)
f1(r2003)
f1(r2004)
f1(r2005)
f1(r2006)
f1(r2007)
f1(r2008)


#JAKI PROCENT LOTOW MIALO OPZONIENIE WIEKSZE NIZ 15 MIN, PODZIAL NA DNI TYGODNIA

f2 <- function(frame){
  frame %>%
    select(DayOfWeek, DepDelay) %>%
    na.omit() %>%
    filter(DepDelay > 15) %>%
    group_by(DayOfWeek) %>%
    summarise(CountDelay = n()) -> a
  frame %>% 
    select(DayOfWeek, FlightNum) %>% 
    group_by(DayOfWeek)%>%
    summarize(NumberOfFlights = n()) -> b
  cbind(a,b[2]) -> pom
  pom
  frame %>%
    select(DayOfWeek, ArrDelay) %>%
    na.omit() %>%
    filter(ArrDelay > 15) %>%
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
f2(r1987)
f2(r1988)
f2(r1989)
f2(r1990)
f2(r1991)
f2(r1992)
f2(r1993)
f2(r1994)
f2(r1995)
f2(r1996)
f2(r1997)
f2(r1998)
f2(r1999)
f2(r2000)
f2(r2001)
f2(r2002)
f2(r2003)
f2(r2004)
f2(r2005)
f2(r2006)
f2(r2007)
f2(r2008)

#NAJBARDZIEJ OPOZNIONY LOT - DEPARTURE I ARRIVAL
f3<-function(frame){
  frame%>% 
    select(Year,UniqueCarrier, FlightNum, TailNum, ArrDelay, Origin,
                  Dest) %>%
    arrange(desc(ArrDelay)) %>% 
    mutate(Delay = "Arr") %>%
    slice_head(n=1)->a
  
  frame%>%
    select(Year,UniqueCarrier, FlightNum, TailNum, DepDelay, Origin,
                  Dest) %>%
    arrange(desc(DepDelay)) %>%
    mutate(Delay = "Dep")%>%
    slice_head(n=1)->b
  colnames(a)[5] <- "MaxDelay"
  colnames(b)[5] <- "MaxDelay"
  rbind(a,b)
}
rbind(f3(r1987),f3(r1988),f3(r1989),
f3(r1990),
f3(r1991),
f3(r1992),
f3(r1993),
f3(r1994),
f3(r1995),
f3(r1996),
f3(r1997),
f3(r1998),
f3(r1999),
f3(r2000),
f3(r2001),
f3(r2002),
f3(r2003),
f3(r2004),
f3(r2005),
f3(r2006),
f3(r2007),
f3(r2008)) -> delays
delays
ggplot(data = delays, aes(x = Year, y = MaxDelay, fill = Delay))+
  geom_bar(stat = "identity") +geom_hline(yintercept = 500) + theme_minimal()

#LOTNISKA Z NAJWIEKSZYMI OPOZNIENIAMI

f4 <- function(frame){
  frame %>% 
    select(Origin, DepDelay) %>% 
    na.omit() %>%
    group_by(Origin) %>% 
    summarise(MeanDelay = mean(DepDelay))%>%
    arrange(desc(MeanDelay)) %>%
    slice_head(n=1)-> u
  year <-frame[1,1]
  u %>% mutate(Year = year)
}
rbind(f4(r1987),
f4(r1988),
f4(r1989),
f4(r1990),
f4(r1991),
f4(r1992),
f4(r1993),
f4(r1994),
f4(r1995),
f4(r1996),
f4(r1997),
f4(r1998),
f4(r1999),
f4(r2000),
f4(r2001),
f4(r2002),
f4(r2003),
f4(r2004),
f4(r2005),
f4(r2006),
f4(r2007),
f4(r2008)) -> f4_all

f4_all
write.csv2(f4_all, "f4_head1_version2_all.csv", row.names=FALSE, col.names = TRUE, quote=FALSE,sep=",")

#LINIE LOTNICZE Z NAJWIEKSZYMI OPOZNIENIAMI
f5 <- function(frame){
  frame %>% 
    select(UniqueCarrier,DepDelay) %>% 
    na.omit() %>%
    group_by(UniqueCarrier) %>% 
    summarise(MeanDelay = mean(DepDelay))%>%
    mutate(Type = "Dep")%>%
    arrange(desc(MeanDelay)) %>%
    slice_head(n=3)-> u
  frame %>%
    select(UniqueCarrier, ArrDelay) %>%
    na.omit() %>%
    group_by(UniqueCarrier) %>%
    summarise(MeanDelay = mean(ArrDelay)) %>%
    mutate(Type = "Arr") %>%
    arrange(desc(MeanDelay)) %>%
    slice_head(n=3)-> w
  colnames(u)[1] <- "Carrier"
  colnames(w)[1] <- "Carrier"
  rbind(u,w) -> res
  year <- frame[1,1]
  res %>% mutate(Year = year)
}
f5(r1987)
f5(r1988)#
f5(r1989)
f5(r1990)
f5(r1991)
f5(r1992)#
f5(r1993)
f5(r1994)
f5(r1995)#
f5(r1996)#
f5(r1997)
f5(r1998)#
f5(r1999)
f5(r2000)#
f5(r2001)#
f5(r2002)#
f5(r2003)
f5(r2004)
f5(r2005)
f5(r2006)#
f5(r2007)#
f5(r2008)#

rbind(f5(r1988),f5(r1992),f5(r1995),f5(r1996),f5(r1998),f5(r2000),f5(r2001),
      f5(r2002),f5(r2006),f5(r2007),f5(r2008)) -> f5_chosen

write.csv2(f5_chosen, "f5_chosen.csv", row.names=FALSE, col.names = TRUE, quote=FALSE,sep=",")



#SHINY

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

# f1(data_list[1])
# 
# input = 1999
# filename <- paste('r',input,sep="")
# 
# print(filename) 
# typeof(filename)
# open(filename)
# r1999
# open.connection(filename)


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

read.csv("carriers.csv") -> carriers
colnames(carriers)[1] <- "Carrier"
head(carriers)
head(f5_chosen)
f5_chosen %>% join_by(f5_chosen, carriers,by="Carrier")





