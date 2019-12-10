#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
getwd()
load("C02NorthernHemisphere.Rdata")
load("CanadianAvgSnow.Rdata")
load("CanadianMeanTemp.Rdata")
load("CanadianPrecip.Rdata")

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$CO2Plot <- renderPlot( {
    
    if(input$CO2=="Latitude30value"){
       plot(Co2North$YearDecimal,Co2North$Latitude30value,type="l",
         main = "CO2 levels at the 30th parallel North",
         xlab = "Year",ylab="CO2 ppm")
          }
    else if(input$CO2=="Latitude33value"){
       plot(Co2North$YearDecimal,Co2North$Latitude33value,type="l",
           main = "CO2 levels at the 33th parallel North",
           xlab = "Year",ylab="CO2 ppm")
           }
    else if(input$CO2=="Latitude37value"){
       plot(Co2North$YearDecimal,Co2North$Latitude37value,type="l",
           main = "CO2 levels at the 37th parallel North",
           xlab = "Year",ylab="CO2 ppm")
        }
    else if(input$CO2=="Latitude41value"){
       plot(Co2North$YearDecimal,Co2North$Latitude41value,type="l",
           main = "CO2 levels at the 41th parallel North",
           xlab = "Year",ylab="CO2 ppm")
        }
    else if(input$CO2=="Latitude44value"){
       plot(Co2North$YearDecimal,Co2North$Latitude44value,type="l",
           main = "CO2 levels at the 44th parallel North",
           xlab = "Year",ylab="CO2 ppm")
       }
    else if(input$CO2=="Latitude49value"){
       plot(Co2North$YearDecimal,Co2North$Latitude49value,type="l",
           main = "CO2 levels at the 49th parallel North",
           xlab = "Year",ylab="CO2 ppm")
       }
    else if(input$CO2=="Latitude53value"){
      plot(Co2North$YearDecimal,Co2North$Latitude53value,type="l",
           main = "CO2 levels at the 53th parallel North",
           xlab = "Year",ylab="CO2 ppm")
      }
    else if(input$CO2=="Latitude58value"){
        plot(Co2North$YearDecimal,Co2North$Latitude58value,type="l",
             main = "CO2 levels at the 58th parallel North",
             xlab = "Year",ylab="CO2 ppm")
      }
    else if(input$CO2=="Latitude64value"){
        plot(Co2North$YearDecimal,Co2North$Latitude64value,type="l",
             main = "CO2 levels at the 64th parallel North",
             xlab = "Year",ylab="CO2 ppm")
    }
    else if(input$CO2=="Latitude72value"){
      plot(Co2North$YearDecimal,Co2North$Latitude72value,type="l",
           main = "CO2 levels at the 72th parallel North",
           xlab = "Year",ylab="CO2 ppm")
    }
    else if(input$CO2=="Latitude90value"){
      plot(Co2North$YearDecimal,Co2North$Latitude90value,type="l",
           main = "CO2 levels at the 90th parallel North",
           xlab = "Year",ylab="CO2 ppm")
    }
 
    } )
  
  
  output$climatePlot <- renderPlot( {
    MeanTemp[MeanTemp$Spring==-9999.9,"Spring"]=0
    MeanTemp[MeanTemp$Summer==-9999.9,"Summer"]=0
    MeanTemp[MeanTemp$Autumn==-9999.9,"Autumn"]=0
    MeanTemp[MeanTemp$Winter==-9999.9,"Winter"]=0
    MeanTemp[MeanTemp$Annual==-9999.9,"Annual"]=0
    AllSnow[AllSnow$Annual==-9999.9,"Annual"]=0
    AllPrecip[AllPrecip$Annual==-9999.9,"Annual"]=0
   if(input$choice=="Temperature"){
     plot(MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Year"],
          MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Spring"],
          ylim = c(-30,30),type="l",main="temperature trends",
          ylab="degree temperature",xlab="year",col=2,lwd=1)
     lines(MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Year"],
           MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Summer"],
           type="l",col=3)
     lines(MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Year"],
           MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Autumn"],
           type="l",col=4)
     lines(MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Year"],
           MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Winter"],
           type="l",col=5)
     lines(MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Year"],
           MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Annual"],
           ylim = c(-20,20),type="l",col=6,lwd=1)
     if(input$Temp==TRUE){
       lines(ksmooth(MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Year"],
                     MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Annual"],
                     bandwidth = input$tempSlide),col=1,lwd=2)
       lines(ksmooth(MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Year"],
                     MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Summer"],
                     bandwidth = input$tempSlide),col=1,lwd=2)
       lines(ksmooth(MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Year"],
                     MeanTemp[MeanTemp$`InfoTemp[2]`==input$city,"Winter"],
                     bandwidth = input$tempSlide),col=1,lwd=2)
     }
     legend("topleft",c("spring avg","summer avg","autumn avg","winter avg","annual avg")
            ,col=c(2:6),cex=0.75,lwd=2)
   }
    
    else if(input$choice=="Snow"){
       plot(AllSnow[AllSnow$`InfoTemp[2]`==input$city,"Year"],
            AllSnow[AllSnow$`InfoTemp[2]`==input$city,"Annual"],
            ylim=c(0,1300),type="l",main="Trend of Snow",
            ylab="amount of snow",xlab="year",col=2,lwd=1)
       if(input$snow==TRUE){
         lines(ksmooth(AllSnow[AllSnow$`InfoTemp[2]`==input$city,"Year"],
                       AllSnow[AllSnow$`InfoTemp[2]`==input$city,"Annual"],
                       bandwidth = input$snowSlide),col=1,lwd=2)
         legend("topleft",c("actual line","moving average"),col=c(2,1),lwd = 2)}
    }
    
    else if(input$choice=="Precip"){
      plot(AllPrecip[AllPrecip$`InfoTemp[2]`==input$city,"Year"],
           AllPrecip[AllPrecip$`InfoTemp[2]`==input$city,"Annual"],
           ylim=c(0,5300),type="l",main="Trend of precip",
           ylab="amount of precip",xlab="year",col=2,lwd=1)
      if(input$precip==TRUE){
        lines(ksmooth(AllPrecip[AllPrecip$`InfoTemp[2]`==input$city,"Year"],
                      AllPrecip[AllPrecip$`InfoTemp[2]`==input$city,"Annual"],
                      bandwidth = input$precipSlide),col=1,lwd=2)
        legend("topleft",c("actual line","moving average"),col=c(2,1),lwd = 2)}
    }
   
    
  } )
  
  output$aboutText<-renderPrint({
    print("This shiny app is concerning the relationship between CO2 level and climate change. CO2 level is increasing with years and I tried to find if there is any influence on temperature, snowing and precip.")
    print("                           by Xueqi(Kay) Du. Second year intro R programing course STAT240 assignment.")
  }) 
  
} )
