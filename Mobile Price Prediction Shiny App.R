##### Installing useful Libraries ####
library("readxl")
library("ggplot2")
library("shiny")
library("dplyr")
library("shinyWidgets")
library('cluster')
library('shinythemes')
library("randomForest")
library("shinycssloaders")
library("GGally")




####  Importing Data and Modeling ####

#Reading in the data
mob_dt <- as.data.frame(read.csv("train.csv"))

#Changing the data types of the columns
fact_cols <- c("blue", "dual_sim", "four_g", "three_g",
               "touch_screen", "wifi", "price_range", "n_cores")

num_cols <- c("battery_power", "clock_speed", "fc", "int_memory",
              "m_dep", "mobile_wt", "pc", "px_height",
              "px_width", "ram", "sc_h", "sc_w", "talk_time")

mob_dt[,fact_cols] <- lapply(mob_dt[,fact_cols],as.factor)
mob_dt[,num_cols] <- lapply(mob_dt[,num_cols],as.numeric)
mob_num <- mob_dt[,c(num_cols,"price_range")]
mob_fact <- mob_dt[,fact_cols]

#Classification

X_cols <- c("battery_power", "dual_sim", "fc", "four_g", "n_cores", "px_height",
            "px_width", "ram")

histcols <- c("battery_power", "px_height", "px_width", "ram")

rfformula <- as.formula(paste("price_range ~ ", paste(X_cols, collapse= "+")))

class_model <- randomForest(rfformula, data = mob_dt, 
                            importance = TRUE, proximity = TRUE,
                            ntree = 400, mtry = 5)



####SHINY UI  ####

ui <- fluidPage(
  theme = shinythemes::shinytheme('simplex'),
  headerPanel('Mobile Price Prediction'),
  
  tabsetPanel(
    
   ##### Home tab ##### 
    
    tabPanel(
      
      title = "Home",
      headerPanel('The Price is Right!'),
      
      fluidRow(style='padding:30px;',
        
               
          span(p("We spend weeks, months, even years developing the right product. 
                 And we need to ensure that effort pays off. 
                 That is when pricing takes the lead. 
                 Getting the price right has the maximum impact on profitability. 
                 This app aims to help mobile phone companies determine the optimal price range that is prevailing in the market."),
              style = "font-size : 20px" ),
          
          HTML('<center> <img src = "mobilmoney.png" </center>'),
        
      ),
      
      fluidRow(style='padding:30px;',
      
               
          span(p("To get an idea of how our training data looks, you can choose any feature from
                 the dropdown list and see how it varies accross our target - price ranges from 0 to 3."),
              style = "font-size : 20px"),
          
          selectInput('feat', 'Numeric Variable', choices = num_cols, selected = "ram"),
          actionButton(inputId = "click2", label = "Run", width=100,
                       icon = icon("paper-plane"))
            
      ),
      
      mainPanel(
        
        plotOutput('plot3')
        
      ),
      
      fluidRow(style='padding:30px;',
               
               selectInput('feat2', 'Factor Variables', choices = fact_cols[-7], selected = "n_cores"),
               actionButton(inputId = "click3", label = "Run", width=100,
                            icon = icon("paper-plane"))
               
      ),
      
      mainPanel(
        
        plotOutput('plot4')
        
      )
      
    ),
    
    
    
    
   ##### EDA tab ##### 
    
    tabPanel(
      
      title = "EDA",
      headerPanel('Exploring Data'),
      
      fluidRow(style='padding:30px;',
               
               span(p("This app models Per-feature pricing defined by 8 product 
                      features with the higher priced range associated with a 
                      feature rich candidate."),
                    style = "font-size : 20px" ),

      ),
      
      fluidRow(style='padding:30px;',
               
               span(p("Below we have a plot which shows us how do the selected
                      variables interact with each other.
                      Select the variables you want to explore from the panel 
                      beside the graph."),
                    style = "font-size : 15px" ),
               
      ),
      
      mainPanel( 
        
        
        plotOutput('plot2')
        
      ),
      
      fluidRow(
        
        column(2, 
               
               checkboxGroupInput("feature", "Choose features:",
                                  choiceNames = list(tags$div(HTML('<i class="fa-battery-three-quarters" style = "color:#e95420;"></i> Battery')), 
                                                     tags$div(HTML('<i class="fa fa-window-restore" style = "color:#e95420;"></i> Dual Sim Slot')),
                                                     tags$div(HTML('<i class="fa fa-camera" style = "color:#e95420;"></i> Front Camera')), 
                                                     tags$div(HTML('<i class="fa fa-globe" style = "color:#e95420;"></i> 4G')),
                                                     tags$div(HTML('<i class="fa fa-microchip" style = "color:#e95420;"></i> Processor Cores')),
                                                     tags$div(HTML('<i class="fa fa-desktop" style = "color:#e95420;"></i> Resolution height (px)')),
                                                     tags$div(HTML('<i class="fa fa-desktop" style = "color:#e95420;"></i> Resolution width (px)')),
                                                     tags$div(HTML('<i class="fa fa-dashboard" style = "color:#e95420;"></i> Ram'))),
                                  choiceValues = list("battery_power", 
                                                      "dual_sim", 
                                                      "fc", 
                                                      "four_g", 
                                                      "n_cores", 
                                                      "px_height",
                                                      "px_width", 
                                                      "ram"),
                                  width = '100%',
                                  selected = c("battery_power", "dual_sim", "fc"))
               
        ),
        
        
      ),
      
      fluidRow(style='padding:30px;',
               
               span(p("Following is a cluster plot to check if there are any
                      visual clusters in our data.
                      Select the columns and number of clusters from the side 
                      panel and hit the Explore button to see the plot."),
                    style = "font-size : 15px" ),
               
      ),
      
      mainPanel( 
        
        
        plotOutput('plot1'), 
        br(),
        
      ),
      
      fluidRow(
        
        column(4, 
               
               selectInput('Xcol', 'X Variable', num_cols, selected = "ram"),
               
        ),
        
        column(4, 
               
               selectInput('Ycol', 'Y Variable', num_cols, selected = "mobile_wt"),
               
        ),
        
        column(4, 
               
               numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
               
        ),
        
        column(4, 
               
               actionButton(inputId = "click1", label = "Explore", icon("paper-plane"))
        ),
        
      ),
      
      

    
    
      
  ),
    
    
   ##### Prediction tab ##### 
    
    
    tabPanel(
    
              title = "Prediction",
              headerPanel('Price Range Prediction'),
              
              tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #e95420;
                                                  border-top: 1px solid #ba431a ;
                                                  border-bottom: 1px solid #ba431a ;}

                            /* changes the colour of the number tags */
                           .irs-from, .irs-to, .irs-single { background: #e95420 }'
              ))
              ),
              tags$head(tags$style(HTML(' [for=ifc]+span>.irs>.irs-single, [for=ifc]+span>.irs-bar-edge, [for=ifc]+span>.irs-bar {
                                                  background: #e95420;
                                                  border-top: 1px solid #ba431a ;
                                                  border-bottom: 1px solid #ba431a ;} 

                            [for=ifc]{color:red;}'
                            
              ))
              ),
              
              fluidRow( 
                
                column(4, 
                       
                       numericInput(
                         "iram", h4("How much Ram does it have? (MB)"), value = 3700
                       ),
                       
                ),
                
                column(4, 
                       
                       numericInput(
                         "ibatp", h4("What is the Battery capacity in mAh?"), value = 589
                       ),
                       
                ),
                
                column(4,
                       
                       sliderInput(
                         "incores", h4("How many cores?"), min = 1, max = 8, value = 3
                       ),
                       
                ),
                
              ),
              
              
              fluidRow(
                
                
                column(4,
                       
                       radioButtons(
                         "fourg", h4("Does it have 4g?"), choices = list("Yes" = 1, "No" = 0),
                         selected = 1
                       ),
                       
                ),
                
                column(4, 
                       
                       sliderInput(
                         "ifc", h4("What's the selfie camera size? (Megapixel)"), 
                         min = 0, max = 20, value = 7
                       ),
                       
                       
                ),
                
                column(4, 
                       
                       radioButtons(
                         "duals", h4("Does it have dual sim?"), choices = list("Yes" = 1, "No" = 0),
                         selected = 1
                       ),
                       
                ),
                
              ),
              
              
              fluidRow( 
                
                
                column(4,
                       
                       numericInput(
                         "ipxh", h4("What is screen height in pixels?"), value = 441
                       ),
                       
                ),
                
                column(4,
                       
                       
                       numericInput(
                         "ipxw", h4("What is screen width in pixels?"), value = 810
                       ),
                ),
                
                column(4,
                       
                       actionButton("predbtn", label = "Predict the price!", icon("paper-plane")),
                       
                       helpText(span(em("PSST! --(Change ram to 1700 MB to see the prediction 
                                change to a lower price range)--")), style = "color:grey;")
                ),
                
                
              ),
              
              br(),
              
              
    mainPanel( 
                
      span(
            shinycssloaders::withSpinner(
                  htmlOutput(outputId = "prediction"), type = 4
                                        ), 
            style = "font-size: 30px;")
                
            ) 
            
  
    ),
    
    
  ),
  

  

)



#### SHINY SERVER ####

server <- function(input, output) {
  
  #### Home Tab ####
  
  mob_dt3 <- eventReactive(input$click2,{
    
    mob_gr<-mob_num[, c(input$feat,'price_range')]
    names(mob_gr) <- c(input$feat,'price_range')
    mob_gr
    
  })
  
  output$plot3 <- renderPlot({
    ggplot()+
      geom_histogram(data = mob_dt3(), aes(x = mob_dt3()[,1]), binwidth = 100)+
      facet_wrap(~price_range) +
      xlab(input$feat) + 
      theme(panel.background = element_rect(fill = "#fcfcfc"),
            plot.background = element_rect(fill = "#fcfcfc"),
            panel.grid.major = element_blank()) 
  })
  
  mob_dt4 <- eventReactive(input$click3,{
  
    mob_gr <- mob_fact[, c(input$feat2,'price_range')]
    names(mob_gr) <- c(input$feat2,'price_range')
    mob_gr
    
  })
  
  output$plot4 <- renderPlot({
    ggplot()+
      geom_bar(data = mob_dt4(), aes(x = mob_dt4()[,1], y = (..count..)/sum(..count..)))+
      facet_wrap(~price_range) +
      xlab(input$feat2) + 
      ylab("Prportion") +
      theme(panel.background = element_rect(fill = "#fcfcfc"),
            plot.background = element_rect(fill = "#fcfcfc"),
            panel.grid.major = element_blank()) 
  })
  
  #### EDA Tab ####
  
  mob_dt1 <- eventReactive(input$click1,{
    mob_dti <- mob_dt[,c(input$Xcol, input$Ycol)]
    mob_dtsc <- data.frame(scale(mob_dti))
    cls <- kmeans(mob_dtsc, centers = input$clusters)
    mob_dti$cluster <- as.character(cls$cluster)
    mob_dti
  })

  
  output$plot2 <- renderPlot({
    
    ggpairs(mob_dt, columns =input$feature, ggplot2::aes(color=price_range)) +
      theme(panel.background = element_rect(fill = "#fcfcfc"),
            plot.background = element_rect(fill = "#fcfcfc"),
            panel.grid.major = element_blank()) 
    
  })
  
  
  output$plot1 <- renderPlot({
    
    ggplot() +
      geom_point(data = mob_dt1(), 
                 aes(x = mob_dt1()[[1]], y =mob_dt1()[[2]], color =cluster))+
      labs(x=input$Xcol, y = input$Ycol) + 
      theme(panel.background = element_rect(fill = "#fcfcfc"),
            plot.background = element_rect(fill = "#fcfcfc"),
            panel.grid.major = element_blank()) 
    
  })
  
  
  
  #### Prediction Tab ####
  
  datatopred <- eventReactive(input$predbtn,{
    
    Sys.sleep(1.5)
    inpts <- c(input$ibatp,
               input$duals,
               input$ifc,
               input$fourg,
               input$incores,
               input$ipxh,
               input$ipxw, 
               input$iram)
    inpts <- rbind(mob_dt[1, X_cols], inpts)
    inpts[,c(1,3,6,7,8)] <- lapply(inpts[,c(1,3,6,7,8)],as.numeric)
    inpts <- inpts[-1,]
    
  })
  
  
  output$prediction <- renderText(
    

    if (as.character(predict(class_model, datatopred())) == '3'){
      HTML(paste("The price range predicted for the given inputs is 3. ",
             "It is the highest band ranging from $800 to $1200. ",
             "Common competitors in this range are flagship phones from 
             Apple and Samsung like iphone 13 pro and Galaxy Fold 2.",
             sep = "<br/>"))
    }
    else if (as.character(predict(class_model, datatopred())) == '2'){
      HTML(paste("The price range predicted for the given inputs is 2. ",
             "It is the second highest band ranging from $600 to $800. ",
             "Common competitors in this range are sub-flagship phones from
             Apple and Samsung like iphone 13 and Galaxy Note 21.", 
             sep = "<br/>"))
    }
    else if (as.character(predict(class_model, datatopred())) == '1'){
      HTML(paste("The price range predicted for the given inputs is 1. ",
             "It is the mid range band starting from $300 up to $600. ",
             "Common competitors in this range are mid-range phones from
             One Plus and Google like One plus 9 and Pixel 6." ,
             sep = "<br/>"))
    }
    else if (as.character(predict(class_model, datatopred())) == '0'){
      HTML(paste("The price range predicted for the given inputs is 0. ",
             "It is the lowest range including everything up till $300. ",
             "Common competitors in this range are entry-level phones from
             Samsung and Motorola like Galaxy M31s and Moto G7. ", 
             sep = "<br/>"))
    }
    
  )
  
}



##### Calling App ####
shinyApp(ui = ui, server = server)


