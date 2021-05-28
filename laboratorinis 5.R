# Sistemos nustatymai, bibliotekos reikalingos duomenų apdorojimui ir įkėlimui ####

Sys.setlocale("LC_COLLATE", "Lithuanian")
Sys.setlocale("LC_CTYPE", "Lithuanian")
options("scipen"=100, "digits"=4)
setwd("~/R/Duomenu vizualizavimas/Projektas")


library(dplyr)
library(shiny)
library(ggplot2)
library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(ggthemes)
library(plotly)
library(ggiraph)

# Sample workable data

data2 <- read.csv("rezultatai.csv", encoding = "UTF-8")
data2 <- data2[, -c(1, 2, 3, 4, 13, 17, 18, 19, 21)]
data2$SAV[data2$SAV == ""] <- "Užsienio lietuviai"
data2 <- data2 %>%
  group_by(VARDAS, PAVARDE, SAV) %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)
data2$Kandidatas <- paste(data2$VARDAS, "\n", data2$PAVARDE)
data2 <- data2[, -c(1,2)]

# Since I cannot enter the choices one by one..


Ctrs <- unique(data2$SAV)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Prezidento rinkimų I turo (2019 m.) rezultatų savivaldybėje palyginimas su bendrais rezultatais"),
  
  # Sidebar with dropdown
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "selects", choices = Ctrs,
                  label = "Pasirinkti savivaldybę:", multiple = F),
      width = 2
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      br(),
      br(),
      plotOutput("Plot1"),
      br(),
      br(),
      plotOutput("Plot2")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$Plot1 <- renderPlot({
    dalis1 <- data2 %>%
      filter(SAV == input$selects)
    
    dalis1$dalis <- dalis1$BALSU_VISO/sum(dalis1$BALSU_VISO)*100
    
    ggplot(data = dalis1, aes(x = Kandidatas, y = dalis, 
                              fill = Kandidatas)) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(round(dalis, 2), "%"), vjust = -0.5), size = 6) +
      theme_light() +
      ggtitle(paste(gsub("savivaldybė", "savivaldybės", input$selects), "balsavimo tendencijų palyginimas su bendrais rinkimų rezultatais")) +
      xlab("Kandidatas į prezidentus") +
      ylab("Savivaldybėje kandidato gautų balsų dalis \n (%)") +
      ylim(0, 50) +
      theme( 
        axis.title.y = element_text(size = 15, face = "bold"), 
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 20, face = "bold"),
        legend.title=element_text(size=15, face = "bold"), 
        legend.text=element_text(size=14))
  })
  
  output$Plot2 <- renderPlot({
    dalis1 <- data2 %>%
      filter(SAV == input$selects)
    dalis2 <- data2 %>%
      group_by(Kandidatas) %>%
      summarize_if(is.numeric, sum, na.rm=TRUE)
    dalis1$skirtumas = (dalis1$BALSU_VISO/sum(dalis1$BALSU_VISO) - dalis2$BALSU_VISO/sum(dalis2$BALSU_VISO))*100

    dalis1$spalva <- ifelse(dalis1$skirtumas < 0, "negative", "positive")
    
    ggplot(data = dalis1, aes(x = Kandidatas, 
                              y = skirtumas, fill = spalva)) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(round(skirtumas, 2), "%"), vjust = ifelse(skirtumas > 0, -0.5, 1)), size = 6) +
      theme_light() +
      ggtitle(paste(
        ifelse(grepl("savivaldybė", input$selects), 
               gsub("savivaldybė", "savivaldybės", input$selects),
               gsub("lietuviai", "lietuvių", input$selects)),
        "balsavimo tendencijų palyginimas su bendrais rinkimų rezultatais")) +
      xlab("Kandidatas į prezidentus") +
      ylab("Kandidato surinktų balsų dalies savivaldybėje ir \nvisų balsų dalies palyginimas (%)") +
      ylim(-25, 25) +
      labs(fill="Spalvos reikšmė") +
      scale_fill_manual(values=c(positive="darkblue",negative="darkred"), 
                        labels = c(positive = "Balsavo didesnė \nrinkėjų dalis nei \nvidutiniškai šalyje \n", 
                                   negative = "Balsavo mažesnė \nrinkėjų dalis nei \nvidutiniškai šalyje\n")) +
      theme( 
        axis.title.y = element_text(size = 15, face = "bold"), 
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 20, face = "bold"),
        legend.title=element_text(size=15, face = "bold"), 
        legend.text=element_text(size=14))
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


