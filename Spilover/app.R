#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("ggplot2")
#install.packages("zoo")
#install.packages("shiny")
library(zoo)
library(ggplot2)
library(shiny)


library(readxl)
library(pracma)
library(tidyverse)
library(lubridate)
library(fpp3)


library(tidyverse)
library(dplyr)
library(stats)

alldata <- readr::read_csv("ABSemp.csv")  |> 
  mutate(Quarter = yearquarter(my(Date))) |> 
  select(-Date, -`96 Total`) |> 
  filter(Quarter <= yearquarter("2019 Q4")) |> 
  select(-Quarter) 


# generate the total number 
alldata <- alldata |> 
  mutate(`96 Total` = rowSums(alldata[,1:ncol(alldata)]))


# generate the total amount of the data 

rawdata <- as.matrix(alldata) #drop the dates column
logdata <- log(rawdata) #log transform the data
d4logdata <- 100*(logdata[5:nrow(logdata),]-logdata[1:(nrow(logdata)-4),]) #compute YoY growth rates
phi <- read.csv("one_lagphi.csv",header=FALSE) #read the estimated VAR parameters
allfcasts <- read.csv("fcast0.csv")     #read the no-Covid forecasts
# # Define UI for application that draws a histogram
ui <- fluidPage(

  
  # Application title
  titlePanel("Employment Dynamics in Australia"),

  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(4, "Agriculture, Forestry and Fishing"),
        column(2,textInput("ag1", "May-2020", value="-9.5")),
        column(2,textInput("ag2", "Aug-2020", value="J")),
        column(2,textInput("ag3", "Nov-2020", value="F")),
        column(2,textInput("ag4", "Feb-2021", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Mining"),
        column(2,textInput("mi1", "", value="-2.9")),
        column(2,textInput("mi2", "", value="J")),
        column(2,textInput("mi3", "", value="F")),
        column(2,textInput("mi4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Manufacturing"),
        column(2,textInput("ma1", "", value="-4.1")),
        column(2,textInput("ma2", "", value="J")),
        column(2,textInput("ma3", "", value="F")),
        column(2,textInput("ma4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Utilities"),
        column(2,textInput("ut1", "", value="-0.2")),
        column(2,textInput("ut2", "", value="J")),
        column(2,textInput("ut3", "", value="F")),
        column(2,textInput("ut4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Construction"),
        column(2,textInput("co1", "", value="-6.4")),
        column(2,textInput("co2", "", value="J")),
        column(2,textInput("co3", "", value="F")),
        column(2,textInput("co4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Wholesale Trade"),
        column(2,textInput("wt1", "", value="-4.4")),
        column(2,textInput("wt2", "", value="J")),
        column(2,textInput("wt3", "", value="F")),
        column(2,textInput("wt4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Retail Trade"),
        column(2,textInput("rt1", "", value="-6.8")),
        column(2,textInput("rt2", "", value="J")),
        column(2,textInput("rt3", "", value="F")),
        column(2,textInput("rt4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Acc & Food"),
        column(2,textInput("af1", "", value="-33.4")),
        column(2,textInput("af2", "", value="J")),
        column(2,textInput("af3", "", value="F")),
        column(2,textInput("af4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Transport"),
        column(2,textInput("tr1", "", value="-3.0")),
        column(2,textInput("tr2", "", value="J")),
        column(2,textInput("tr3", "", value="F")),
        column(2,textInput("tr4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "ITC"),
        column(2,textInput("it1", "", value="-6.5")),
        column(2,textInput("it2", "", value="J")),
        column(2,textInput("it3", "", value="F")),
        column(2,textInput("it4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Finance & Insurance"),
        column(2,textInput("fi1", "", value="-1.0")),
        column(2,textInput("fi2", "", value="J")),
        column(2,textInput("fi3", "", value="F")),
        column(2,textInput("fi4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Real Estate"),
        column(2,textInput("re1", "", value="-11.0")),
        column(2,textInput("re2", "", value="J")),
        column(2,textInput("re3", "", value="F")),
        column(2,textInput("re4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Professional Services"),
        column(2,textInput("ps1", "", value="-5.6")),
        column(2,textInput("ps2", "", value="J")),
        column(2,textInput("ps3", "", value="F")),
        column(2,textInput("ps4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Admin Services"),
        column(2,textInput("as1", "", value="-10.0")),
        column(2,textInput("as2", "", value="J")),
        column(2,textInput("as3", "", value="F")),
        column(2,textInput("as4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Public Admin"),
        column(2,textInput("pa1", "", value="-5.1")),
        column(2,textInput("pa2", "", value="J")),
        column(2,textInput("pa3", "", value="F")),
        column(2,textInput("pa4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Education"),
        column(2,textInput("ed1", "", value="-2.0")),
        column(2,textInput("ed2", "", value="J")),
        column(2,textInput("ed3", "", value="F")),
        column(2,textInput("ed4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Health Care"),
        column(2,textInput("he1", "", value="-2.9")),
        column(2,textInput("he2", "", value="J")),
        column(2,textInput("he3", "", value="F")),
        column(2,textInput("he4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Arts & Recreation"),
        column(2,textInput("ar1", "", value="-27.0")),
        column(2,textInput("ar2", "", value="J")),
        column(2,textInput("ar3", "", value="F")),
        column(2,textInput("ar4", "", value="F"))
      ),
      tags$hr(),
      fluidRow(
        column(4, "Other Services"),
        column(2,textInput("os1", "", value="-12.0")),
        column(2,textInput("os2", "", value="J")),
        column(2,textInput("os3", "", value="F")),
        column(2,textInput("os4", "", value="F"))
      )
    ),
    mainPanel(
      plotOutput("plot"),
      h4("Notes and instructions:"),
      p("J stands for job-keeper and F stands for free. Job keeper keeps the employed numbers constant, and free allows the model to determine the employed numbers."),
      p("Enter J, F or any assumed number for the % change in employment in each quarter relative to the previous quarter for each sector. The model will then produce a red lineplot of the year-on-year growth rates in the next 5 years according to your scenario. The blue line on the graph is the expected path of employment growth without the covid-19 shock."),
#      p("The pre-filled scenario is the optimistic scenario, Scenario O, in Anderson, Caggiano, Vahid and Wong (2020). This scenario assumes that the change in employment in each sector between March 14 and April 18 published by the ABS on May 4 will be the change in employment in that sector in 2020Q2.")
      p("The pre-filled scenario is the optimistic scenario, Scenario O, in ", a(href="https://www.monash.edu/business/ebs/research/publications/ebs/wp20-2020.pdf", "Anderson, Caggiano, Vahid and Wong (2020)."), "This scenario assumes that the change in employment in each sector between March 14 and April 18 published by the ABS on May 4 will be the change in employment in that sector in 2020Q2."),
#      tableOutput("test")
      p("Following works will be done by the end of 2022 ( {Adding 96 sectors}) --Elvis Y.")
    )
  )
)


server <- function(input, output) {
  fraw1 = matrix(0,24,20)
  fraw1[1:4,]= rawdata[(nrow(rawdata)-3):nrow(rawdata),]
  fgr1 = matrix(0,24,20)
  fgr1[1:4,] = d4logdata[(nrow(d4logdata)-3):nrow(d4logdata),]
  x <- c("ag","mi","ma","ut","co","wt","rt","af","tr","it","fi","re","ps","as","pa","ed","he","ar","os")
  reacttoinput <- reactive({
#   browser()
    for (i in 5:24){
      for (j in 1:19){
        if (i<=8){
          if (eval(parse(text=paste("input$",x[j],i-4,sep=""))) == "J"){ 
#            fgr1[i,j] = fgr1[i-1,j]
            fraw1[i,j] = fraw1[i-1,j]
            fgr1[i,j] = 100*log(fraw1[i,j]/fraw1[i-4,j])
          }
          else if (eval(parse(text=paste("input$",x[j],i-4,sep=""))) == "F"){
            fgr1[i,j]= phi[1,j] + fgr1[(i-1),]%*%phi[(2:21),j] + fgr1[(i-2),]%*%phi[22:41,j] + fgr1[(i-3),]%*%phi[42:61,j] + fgr1[(i-4),]%*%phi[62:81,j]
            fraw1[i,j]=fraw1[i-4,j]*exp(fgr1[i,j]/100)
          }
          else {
#            fgr1[i,j] = as.numeric((eval(parse(text=paste("input$",x[j],i-4,sep="")))))
            fraw1[i,j] = fraw1[i-1,j]*(1+as.numeric((eval(parse(text=paste("input$",x[j],i-4,sep="")))))/100) #use this and the line below it if user inputs QoQ growth rates
            fgr1[i,j] = 100*log(fraw1[i,j]/fraw1[i-4,j])
          }
        }
        else {
          fgr1[i,j]= phi[1,j] + fgr1[(i-1),]%*%phi[(2:21),j] + fgr1[(i-2),]%*%phi[22:41,j] + fgr1[(i-3),]%*%phi[42:61,j] + fgr1[(i-4),]%*%phi[62:81,j]
          fraw1[i,j]=fraw1[i-4,j]*exp(fgr1[i,j]/100)
        }
      }
      fraw1[i,20] = sum(fraw1[i,1:19])
      fgr1[i,20]=100*log(fraw1[i,20]/fraw1[(i-4),20])
    }
    f1 = (cbind(fgr1[5:24,20],fraw1[5:24,20]))
    colnames(f1)=c("YoY1","Employment1")
    cbind(allfcasts,f1)
  })
  output$plot = renderPlot({
    ggplot(reacttoinput()) + geom_line(aes(x=Date,y=YoY),color="blue",size=2) + geom_line(aes(x=Date, y=YoY1),color="red",size=2) + xlab("Year-Quarter") + ylab("YoY Growth in Total Employment") + scale_x_yearqtr(format = "%YQ%q", breaks = seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.5), minor_breaks= seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.25)) + theme(legend.position = c(.95, .05))
  })
  #  output$test = renderTable(reacttoinput())
}

#Run the application
shinyApp(ui = ui, server = server)



