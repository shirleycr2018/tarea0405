#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(readr)
library(lubridate)
library(dplyr)
library(dygraphs)
library(xts)
library(dplyr)
library(rsconnect)
library(quantmod)
library(forecast)
library(astsa)

rsconnect::setAccountInfo(name='shirleycr2018',
                          token='D2CEEC37EE2E9DC64B46857C888B445B',
                          secret='wnS90/ExFsEscWeWeLRAYofkOP6Mb+6KtbSqkuP2')

 


leer_indicadores <- function(iv_file){

    df_indicators<-read.csv(iv_file)
    return(df_indicators)
}
    
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

# with dates 

    
   output$dygraphhistoric <- renderDygraph({
       
       lv_indicadores <- input$indicadores 
#       print(lv_indicadores)
       
       if(lv_indicadores == "House Prices in the UK since 1953"){
#df_houseprices
           df_houseprices    <<- leer_indicadores("https://raw.githubusercontent.com/shirleycr2018/tarea0405/main/housePrices.csv")
           df_houseprices$Date <<- zoo::as.Date(df_houseprices$Date)
           df_houseprices <- df_houseprices
           xts_houseprices <- xts(df_houseprices$Price..All.,order.by = df_houseprices$Date)
           mydigraph <- dygraph(xts_houseprices, "House Prices in the UK since 1953")
           
           
           
       }

       if(lv_indicadores == "US Employment and Unemployment rates since 1940"){
#df_employment    
           df_employment  <<- leer_indicadores("https://raw.githubusercontent.com/shirleycr2018/tarea0405/main/employment_csv.csv")
#           df_employment <<- df_employment %>% select(-c("footnotes") )
           df_employment  <<- df_employment
           ts_employment   <- ts(df_employment[,-1], frequency = 12,start = c(min(df_employment$year)), end=c(max(df_employment$year)) )
           mydigraph <- dygraph(ts_employment, "US Employment and Unemployment rates since 1940")
       }
         
       if(lv_indicadores == "Bank of England Interest Rate"){
#df_insterest      
           df_insterest      <<- leer_indicadores("https://raw.githubusercontent.com/shirleycr2018/tarea0405/main/interest_rate.csv")
           df_insterest$date <- zoo::as.Date(df_insterest$date)
           df_insterest <<- df_insterest
           ts_insterest <- xts(df_insterest[,-1],order.by = df_insterest$date)
           mydigraph <- dygraph(ts_insterest, "Bank of England Interest Rate")
       }
       mydigraph
       
   })
   
  output$dygrapHW <- renderDygraph({
    
    lv_indicadores <- input$indicadores 
    lv_training    <- input$slider1 
    lv_confianza   <- input$slider2
    
    lv_confianza  <- ( as.numeric(lv_confianza) / 100 )

    
    if(lv_indicadores == "House Prices in the UK since 1953"){ 

      periodos<- ( nrow(df_houseprices) / 4 )
      periodos_train <<-  floor(periodos*lv_training/100)

      
      periodos_test  <<-  periodos - periodos_train
      variable_train <<- df_houseprices[1:periodos_train,]
      variable_test  <<- df_houseprices[-(1:periodos_train),]
      
      
      lv_min <- min(variable_train$Date)
      lv_max <- max(variable_train$Date)
      ts_houseprices_train <- ts(variable_train$Price..All., frequency = 4, start=c(year(lv_min)))
      

      
      lv_min <- min(variable_test$Date)
      lv_max <- max(variable_test$Date)
       
      
      ts_houseprices_test  <- ts(variable_test$Price..All., frequency = 4, start=c(year(lv_min)))
      
      HW<-HoltWinters(ts_houseprices_train) 
      

      
      hw_predict <- predict(HW, n.ahead = periodos_test, prediction.interval = TRUE, level=lv_confianza)
      

      
      all <- cbind(ts_houseprices_train, hw_predict)
       
     mygrahp <- dygraph(all, "HW House Prices in the UK since 1953") %>%
                dySeries("ts_houseprices_train", label = "Actual") %>%
                dySeries(c("hw_predict.lwr", "hw_predict.fit", "hw_predict.upr"), label = "Predicted_Price_All")
      
    }
    
    if(lv_indicadores == "US Employment and Unemployment rates since 1940"){

      df_employment$year <- ymd(sprintf("%d-01-01",df_employment$year))
      periodos <- nrow(df_employment)
      periodos_train<-floor(periodos*lv_training/100)
      if (periodos_train < 60){
        periodos_train <- 60
      }
      
      periodos_test <-  periodos - periodos_train 
      variable_train <- df_employment[1:periodos_train,]
      variable_test  <- df_employment[-(1:periodos_train),]
      
      lv_min <- min(variable_train$year)
      lv_max <- max(variable_train$year)  
      ts_employment_percent    <- ts( variable_train$employed_percent, frequency=1, start = c(year(lv_min)) )
      ts_unemployment_percent <- ts( variable_train$unemployed_percent, frequency=1, start = c(year(lv_min)) )
      
      HW1<-HoltWinters(ts_employment_percent)
      HW2<-HoltWinters(ts_unemployment_percent)
       
      hw_predict1 <- predict(HW1, n.ahead = periodos_test, prediction.interval = TRUE)
      hw_predict2 <- predict(HW2, n.ahead = periodos_test, prediction.interval = TRUE)
      
      all <- cbind(ts_employment_percent,ts_unemployment_percent , hw_predict1,hw_predict2 )
      
      mygrahp <- dygraph(all, "HW US Employment and Unemployment rates since 1940") %>%
        dySeries("ts_employment_percent", label = "Actual Employment") %>%
        dySeries("ts_unemployment_percent", label = "Actual UnEmployment") %>%
        dySeries(c("hw_predict1.lwr", "hw_predict1.fit", "hw_predict1.upr"), label = "Predictd_Employment")%>%
        dySeries(c("hw_predict2.lwr", "hw_predict2.fit", "hw_predict2.upr"), label = "Predictd_UnEmployment")
    } 
    if(lv_indicadores == "Bank of England Interest Rate"){
#   df_insterest 
     
      df_insterest1    <- df_insterest %>% 
                          group_by(month=floor_date(date, "month")) %>% 
                          summarize(rate  = mean(rate))
      
      periodos <- nrow(df_insterest1)
      periodos_train<-floor(periodos*lv_training/100)
      
      
      periodos_test <-  periodos - periodos_train 
      variable_train <- df_insterest1[1:periodos_train,]
      variable_test  <- df_insterest1[-(1:periodos_train),]
      
      lv_min <- min(variable_train$month)
      lv_max <- max(variable_train$month)
      ts_bank_rate   <- ts( variable_train$rate, frequency=12, start = c(year(lv_min)), end = c(year(lv_max)))
      
      HW<-HoltWinters(ts_bank_rate) 
      
      hw_predict <- predict(HW, n.ahead = periodos_test, prediction.interval = TRUE, level=lv_confianza)
      
      all <- cbind(ts_bank_rate, hw_predict)
      
      mygrahp <- dygraph(all, "HW Bank of England Interest Rate") %>%
        dySeries("ts_bank_rate", label = "Actual") %>%
        dySeries(c("hw_predict.lwr", "hw_predict.fit", "hw_predict.upr"), label = "Predicted_Price_All") 
    }
    
    mygrahp
  })
   
  output$dygrapAR <- renderDygraph({ 
    
    lv_indicadores <- input$indicadores 
    lv_training    <- input$slider1
    lv_confianza   <- input$slider2

  
    
    if(lv_indicadores == "House Prices in the UK since 1953"){ 

      periodos<- ( nrow(df_houseprices) / 4 )
      
      periodos_train <<-  floor(periodos*lv_training/100)
      
      periodos_test  <<-  periodos - periodos_train
      variable_train <<- df_houseprices[1:periodos_train,]
      variable_test  <<- df_houseprices[-(1:periodos_train),]
      
      
      lv_min <- min(variable_train$Date) 
      lv_max <- max(variable_train$Date)
      ts_houseprices_train <- ts(variable_train$Price..All., frequency = 4, start=c(year(lv_min)))
      
      lv_min <- min(variable_test$Date)
      lv_max <- max(variable_test$Date)
      
      
      ts_houseprices_test  <- ts(variable_test$Price..All., frequency = 4, start=c(year(lv_min)))
      
      md <- auto.arima(ts_houseprices_train)
      
      df_predict <- predict(md, n.ahead= periodos_test, level=as.numeric(lv_confianza))
      
      all <- cbind(ts_houseprices_train,df_predict$se, df_predict$pred)
      
      mygrahp1 <- dygraph(all, "Arima House Prices in the UK since 1953") %>%
                  dySeries("ts_houseprices_train", label = "Actual") %>%
                  dySeries("df_predict$se", label = "Se") %>%
                  dySeries("df_predict$pred", label = "Predicted")
      
      
    }
    if(lv_indicadores == "US Employment and Unemployment rates since 1940"){

      df_employment$year <- ymd(sprintf("%d-01-01",df_employment$year))
      periodos <- nrow(df_employment)
      periodos_train<-floor(periodos*lv_training/100)
      
      
      periodos_test <-  periodos - periodos_train 
      variable_train <- df_employment[1:periodos_train,]
      variable_test  <- df_employment[-(1:periodos_train),]
      
      lv_min <- min(variable_train$year)
      lv_max <- max(variable_train$year)
      ts_employment_percent    <- ts( variable_train$employed_percent, start = c(year(lv_min)) )
      ts_unemployment_percent <- ts( variable_train$unemployed_percent, start = c(year(lv_min)) )
 
      
    }
    if(lv_indicadores == "Bank of England Interest Rate"){
      #   df_insterest
      
      df_insterest1    <- df_insterest %>% 
        group_by(month=floor_date(date, "month")) %>% 
        summarize(rate  = mean(rate))
      
      periodos <- nrow(df_insterest1)
      periodos_train<-floor(periodos*lv_training/100)
      
      
      periodos_test <-  periodos - periodos_train 
      variable_train <- df_insterest1[1:periodos_train,]
      variable_test  <- df_insterest1[-(1:periodos_train),]
       
      lv_min <- min(variable_train$month)
      lv_max <- max(variable_train$month)
      ts_bank_rate   <- ts( variable_train$rate, frequency=12, start = c(year(lv_min)), end = c(year(lv_max)))
      
      md <- auto.arima(ts_bank_rate)
      
      df_predict1 <- predict(md, n.ahead= periodos_test, level=as.numeric(lv_confianza))
      
      all <- cbind(ts_bank_rate,df_predict1$se, df_predict1$pred)
      
      mygrahp1 <- dygraph(all, "Arima Bank of England Interest Rate") %>%
        dySeries("ts_bank_rate", label = "Actual") %>%
        dySeries("df_predict1$se", label = "Se") %>%
        dySeries("df_predict1$pred", label = "Predicted")
      
       
      
    }
    
    mygrahp1
    
    })

})
