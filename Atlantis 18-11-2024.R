
#install.packages(c('BBmisc','shiny','shinyWidgets','rclipboard','calibrate','ggplot2',
#'DT','tidyverse','glue','data.table','plyr','dplyr','tidyr','tibble','purrr',
#'zoo','shiny','shinydashboard','networkD3','stringi','Hmisc','formattable'))




library(BBmisc)
library(shiny)
library(shinyWidgets )
library(rclipboard)
library(calibrate)
library(ggplot2)
library(DT)
library(tidyverse)
library(glue)
library(data.table)
library(plyr)
library(dplyr)
require(tidyr)
library(tibble)
library(purrr)
library(zoo)
library(shiny)
library(shinydashboard)
library(networkD3)
library(stringi)
library(formattable)
library(Hmisc)

#get.values <- function(input){
#  # YOU CAN ENTER YOUR CODE IN HERE
#  df_input <- input$vdat()[,1]
#  return(df_input)
#}

callback <- c(
  "var dt = table.table().node();",
  "$(dt).selectable({",
  "  distance : 10,",
  "  selecting: function(evt, ui){",
  "    $(this).find('tbody tr').each(function(i){",
  "      if($(this).hasClass('ui-selecting')){",
  "        table.row(i).select();",
  "      }",
  "    });",
  "  }",
  "}).on('dblclick', function(){table.rows().deselect();});"
)

ui <- navbarPage(
  titlePanel(""),
  
  tabPanel("Introductory Information",
           sidebarLayout(
             sidebarPanel(
               h2("ValuesPlan"),
               h4("Version Beta 1.0"),
               br(),
               br(),
               p("Values driven management planning software created by Michael Smith and Christian Wagner"),
               br(),
               br(),
               
             br(),
             br()
             ),
             
             
           mainPanel(
               p("Welcome to ValuesPlan. We strongly recommend that you read the papers listed below.
                 The management framework that ValuesPlan is developed around 
                 and key terms and definitions are clearly articulated by 
                 Wallace and colleagues (Wallace 2012; Wallace et al. 2021, 2016, 2020)."),
               p("Importantly, ValuesPlan is built around eliciting information from 
                 stakeholders and as such, it is very important that you try to minimise 
                 the issues that are commonly associated with elicitations. Important 
                 information on elicitations are provided in several of the references
                 (e.g., Wallace et al. 2016; Smith et al. 2016, 2020, 2015)."),
               p("There are also a number of technical papers describing some of the different 
                 sections of the software (e.g., Wagner et al. 2015; Pourabdollah et al. 2014, 2015; 
                 Smith et al. 2016, 2020, 2015; Wallace et al. 2016)."),
               p("We rely heavily upon the DECSYS system (Ellerby et al. 2019) to produce CSV files 
               to input into ValuesPLan. DECSYS can be found at: https://www.lucidresearch.org/decsys.html. Finally, 
               an example of a fictitious management plan created using this software and DECSYS is provided in
                 Smith and Wagner (2022)."),

               p("This is a Beta version. We plan to include the Fuzzy Logic approach to modelling each value as a function of each property developed by Smith et al. 2016 and we are looking
                 into using the Stakeholder Element Importance Ratings as priors for a Bayesian analysis of element utility. Most of the data 
                 is currently imported via csv files. However, the model utility and benefit analyses are currently conducted by hand within the software.
                 We will look to automate at least some of these parts of the softare in the future. Example csv files that aligh with the Atlantis
                 compendium book are provided."),
               br(),
               br(),
               p("We would like to acknowledge the contributions made by Amir Pourabdullah, Yixin Qi, Alex Mohajeri, and Bethany Denton"),
               br(),
               br(),
               p("We dedicate this software to Ken Wallace (1950 to 2021). Ken's insights, vision, patience,
               friendship, and determination made this work possible. He is greatly missed."),
               
               p(strong("References")),
               

p("Ellerby Z., McCulloch J., Young J. & Wagner C. (2019) DECSYS  Discrete and Ellipse-based response Capture SYStem. 2019 IEEE Int. Conf. Fuzzy Syst. FUZZ-IEEE."),
p("Pourabdollah A., Wagner C., Miller S., Smith M. J. & Wallace K. (2014) Towards data-driven environmental planning and policy design-leveraging fuzzy logic to operationalize a planning framework. In: 2014 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE) pp. 2230-2237 2014 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE)."),
p("Pourabdollah A., Wagner C., Smith M. J. & Wallace K. (2015) Real-world utility of non-singleton fuzzy logic systems: A case of environmental management. In: 2015 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE) pp. 1 - 8 2015 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE)."),
p("Smith M. J. & Wagner C. (2022) Managing Atlantis. A natural resource management plan for the Phoenix Catchment. Perth, Western Australia."),
p("Smith M. J., Wagner C., Wallace K. J., Pourabdollah A. & Lewis L. (2016) The contribution of nature to people: Applying concepts of values and properties to rate the management importance of natural elements. J. Environ. Manage. doi: 10.1016/j.jenvman.2016.02.007."),
p("Smith M., Jackson C., Palmer N. & Palmer B. (2020) A structured analysis of risk to important wildlife elements in three Australian Wildlife Conservancy sanctuaries. Ecol. Manag. Restor. 21 , 42-50."),
p("Smith M., Wallace K., Lewis L. & Wagner C. (2015) A structured elicitation method to identify key direct risk factors for the management of natural resources. Heliyon 1 , e00043."),
p("Wagner C., Smith M. J., Wallace K. & Pourabdollah A. (2015) Generating Uncertain Fuzzy Logic Rules from Surveys: Capturing Subjective Relationships between Variables from Human Experts. In: 2015 IEEE International Conference on Systems, Man, and Cybernetics pp. 2033-2038 2015 IEEE International Conference on Systems, Man, and Cybernetics."),
p("Wallace K. J. (2012) Values: drivers for planning biodiversity management. Environ. Sci. Policy doi: 10.1016/j.envsci.2011.12.001."),
p("Wallace K. J., Jago M., Pannell D. J. & Kim M. K. (2021) Wellbeing, values, and planning in environmental management. J. Environ. Manage. doi: 10.1016/j.jenvman.2020.111447."),
p("Wallace K. J., Wagner C. & Smith M. J. (2016) Eliciting human values for conservation planning and decisions: A global issue. J. Environ. Manage. doi: 10.1016/j.jenvman.2015.12.036."),
p("Wallace K., Kim M., Rogers A. & Jago M. (2020) Classifying human wellbeing values for planning the conservation and use of natural resources. J. Environ. Manage. doi: 10.1016/j.jenvman.2019.109955.")



)
)
),
  
  tabPanel("Stakeholder Values Analysis",
      sidebarLayout(
      sidebarPanel(fileInput("file1", "Import your Values Rating CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
                ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      tableOutput("contents")
                  ),
    
      mainPanel(
        chooseSliderSkin("Modern"),
      column(3,h3("Mid Point"), uiOutput("sliders.centroid")),
      column(3,h3("Spread"), uiOutput("sliders.spread")),
      column(3, plotOutput("PlotValues", 500,600))
      
      
      )
  )),

  tabPanel("Stakeholder Element Analysis",
           tabPanel("Element by Value",
           sidebarLayout(
             sidebarPanel(fileInput("file.elements", "Import your Element Rating CSV File",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
             ),
             tags$hr(),
             h5("This may take a few secs"),
             checkboxInput("header", "Header", TRUE),
             tableOutput("contents.f")
               ),
             
  
             mainPanel(
               tabsetPanel(type = "tabs",
                 tabPanel("Value:Element Information",
                   column(3,h3("Mid Point"), uiOutput("sliders.centroid.v.e")),
                   column(3,h3("Spread"), uiOutput("sliders.spread.v.e")),
                   column(6, plotOutput("PlotValueElements", 1500,800))
                   
               ),
                  
                 tabPanel("Stakeholder Element Utility",
                                  plotOutput("PlotElementsStakeholderUtility",800,800)
                          ) 
               
             ))
             ))),
  
 
  
  tabPanel("Model Utility - Part 1",
           sidebarLayout(
             sidebarPanel(
               h5("You can reload previous data if you need to"),
               fileInput("file.up", "Choose File", accept = c(accept = c(".csv", ".tsv"))),
               checkboxInput("header", "Header", TRUE),
               radioButtons("Add.Data", "Upload Data",
                            c("No","Yes"), selected = "No"),
               #textInput("Manage.name.1", "Enter the management option:"),
               textInput("Prop.name.1", "Enter the property name:"),
               textInput("Element.name.1", "Enter the element name:"),
               textInput("Value.name.1", "Enter the value name:"),
               numericInput("obs", "Enter the maximum property value:",5000, min=0, max=10000000000),
               numericInput("p.obs", "Enter the current central property estimate:",650, min=0, max=10000000000),
               numericInput("p.obs.l", "Enter the current low property estimate:",600, min=0, max=10000000000),
               numericInput("p.obs.h", "Enter the current high property estimate:",700, min=0, max=10000000000)
               #numericInput("f.obs", "Enter the central property estimate you expect at the end of the management period:",2500, min=0, max=10000000000),
               #numericInput("f.obs.l", "Enter the low property estimate you expect at the end of the management period:",2000, min=0, max=10000000000),
               #numericInput("f.obs.h", "Enter the high property estimate you expect at the end of the management period:",3000, min=0, max=10000000000)
             
               
             ),
             
             mainPanel(    
               column(12,plotOutput("PlotRules")),
               h5("Adjust the graph with the sliders"),
               HTML("<br>"),
               column(5,uiOutput("sliders.rules")),
               downloadButton("download", "Save table if you will need to upload later"),
               tableOutput("Slider.Res"),
               #uiOutput("clip"),
               tableOutput("Benefit.u"),
               #tableOutput("data.up.2.benefit"),
               p("Copy and paste info from this table into an excel sheet and save as a CSV file.
                 You can build that file up with the expected utility for each element and then 
                 upload your data and assess the model utility on the next tab")
                  ))),
  
  tabPanel("Model Utility - Part 2",
           sidebarLayout(
             sidebarPanel(fileInput("file3", "Import Your Utility CSV File",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
             ),
             tags$hr(),
             checkboxInput("header", "Header", TRUE),
             tableOutput("contents.model"),
             ),
             
             mainPanel(
               column(3,h3("Mid Point"), uiOutput("sliders.centroid.model")),
               column(3,h3("Spread"), uiOutput("sliders.spread.model")),
               column(3, plotOutput("PlotUtility", 500,600))
             )
           )),
  
tabPanel("Stakeholder-Model Utility Comparison",
           mainPanel(
              plotOutput("Comparison.Plot", 1000,600)
           ),
           value = "page4_upload"),

tabPanel("Risk Analysis",
         tabPanel("Element by Risk Factor",
                  sidebarLayout(
                    sidebarPanel(fileInput("file.risk", "Import your Risk Analysis CSV File",
                                           accept = c(
                                             "text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")
                    ),
                    tags$hr(),
                    h5("This may take a few secs"),
                    checkboxInput("header", "Header", TRUE),
                    tableOutput("contents.risk"),
                    #tableOutput("TestR")
                    ),
                    
                    
                    mainPanel(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Element:Risk Information",
                                           column(3,h3("Mid Point"), uiOutput("sliders.centroid.v.risk")),
                                           column(3,h3("Spread"), uiOutput("sliders.spread.v.risk")),
                                           column(6, plotOutput("PlotElementsRisk", 2500,800))
                                  ),
                                  
                                  tabPanel("Overall Element Risk",
                                           plotOutput("PlotRiskAverage",800,800)
                                  ),
                                  tabPanel("Overall Risk Factor Risk",
                                           plotOutput("PlotRiskAverageFact",800,800)
                                  ) 
                      ))
                  ))),







tabPanel("Benefit Analysis - Part 1",
         sidebarLayout(
           sidebarPanel(
             h5("You can reload previous data if you need to"),
             fileInput("file.up.benefit", "Choose File", accept = c(accept = c(".csv", ".tsv"))),
             checkboxInput("header", "Header", TRUE),
             radioButtons("Add.Data.benefit", "Upload Data",
                          c("No","Yes"), selected = "No"),
             textInput("Manage.name.1.benefit", "Enter the management option:"),
             textInput("Prop.name.1.benefit", "Enter the property name:"),
             textInput("Element.name.1.benefit", "Enter the element name:"),
             textInput("Value.name.1.benefit", "Enter the value name:"),
             numericInput("obs.benefit", "Enter the maximum property value:",5000, min=0, max=10000000000),
             numericInput("p.obs.benefit", "Enter the current central property estimate:",650, min=0, max=10000000000),
             numericInput("p.obs.l.benefit", "Enter the current low property estimate:",600, min=0, max=10000000000),
             numericInput("p.obs.h.benefit", "Enter the current high property estimate:",700, min=0, max=10000000000),
             numericInput("f.obs.benefit", "Enter the central property estimate you expect at the end of the management period:",2500, min=0, max=10000000000),
             numericInput("f.obs.l.benefit", "Enter the low property estimate you expect at the end of the management period:",2000, min=0, max=10000000000),
             numericInput("f.obs.h.benefit", "Enter the high property estimate you expect at the end of the management period:",3000, min=0, max=10000000000)
             #tableOutput("testit"),
             #tableOutput("testit2")
             
             
           ),
           
           mainPanel(    
             column(12,plotOutput("PlotRules.benefit")),
             h5("Adjust the graph with the sliders"),
             HTML("<br>"),
             column(5,uiOutput("sliders.rules.benefit")),
             downloadButton("download.benefit", "Save table if you will need to upload later"),
             tableOutput("Slider.Res.benefit"),
             #uiOutput("clip"),
             tableOutput("Benefit.o"),
             p("Copy and paste info from this table into an excel sheet and save as a CSV file.
                 You can build that file up with the expected benefit for each management option and then 
                 upload your data and assess the benefits on the next tab")
           ))),
         

tabPanel("Benefit Analysis - Part 2",
         sidebarLayout(
           sidebarPanel(fileInput("fileBA2", "Import Your Benefit CSV File",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")
           ),
           tags$hr(),
           checkboxInput("header", "Header", TRUE),
           tableOutput("contents.model.BA2"),
           #tableOutput("BenefitOrdered.0")
           ),
           
           mainPanel(
             column(3,h3("Mid Point"), uiOutput("sliders.centroid.model.BA2")),
             column(3,h3("Spread"), uiOutput("sliders.spread.model.BA2")),
             column(3, plotOutput("PlotUtility.BA2", 1000,1000))
           )
         )),


tabPanel("Management Option Costs",
         sidebarLayout(
           sidebarPanel(fileInput("file.cost", "Import your Management Cost CSV File",
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")
           ),
           tags$hr(),
           checkboxInput("header", "Header", TRUE),
           tableOutput("cost"),
           #tableOutput("CostOrdered.0")
           ),
           
           mainPanel(
             column(3,h3("Mid Point"), uiOutput("sliders.centroid.cost")),
             column(3,h3("Spread"), uiOutput("sliders.spread.cost")),
             column(3, plotOutput("PlotCosts", 500,600))
           )
         )),

tabPanel("Benefit/Cost Analysis",
         sidebarLayout(
           sidebarPanel(
             #tableOutput("textlabelsBC"),
             tableOutput("BC.ordered")
           
           ),
           
           mainPanel(
             plotOutput("PlotBC", 800,600),
             plotOutput("BCFinal", 800,800)
           )
         )
         )
)



#autoFillDF <- read.csv('C:/Mike/Atlantis/DRFs.csv', header=TRUE, stringsAsFactors = FALSE)

server <- function(input, output) {
  
  
###########################################################################################################
######################################## Values Analysis ##################################################
###########################################################################################################
  
  
  output$contents <- renderTable({

    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header)
  })

  data <- reactive({ 
    req(input$file1) ##  require that the input is available
    
    inFile <- input$file1 
    
    df <- read.csv(inFile$datapath, header = input$header)#, quote = input$quote)#sep = input$sep)
    
    return(df)
  })
  
  # Create vdat and sort
  vdat <- reactive({
    vdat.1<-data()
    return(vdat.1)
  })
  
  observe<-reactive({
    updateSelectInput(session, "id_value", choices = vdat()[,1])
  })
  
  # calculate total number of rows
  total.rows <- reactive({
    tot.row<-nrow(vdat())
    return(tot.row)
  })
  
  output$sliders.centroid <- renderUI({
  
    lapply(1:total.rows(), function(i) {
      vdat.2<-data.frame(vdat())
      sliderInput(inputId = paste0("ind.c", i),label = vdat.2[i,1], 
                  min = 0, max = 100, value = vdat.2[i,2], step = 1)
      
    })
  })
  
  output$sliders.spread <- renderUI({
    
    lapply(1:total.rows(), function(i) {
      vdat.2<-data.frame(vdat())
      sliderInput(inputId = paste0("ind.s", i),label = vdat.2[i,1], 
                  min = 0, max = 100, value = c(vdat.2[i,3],vdat.2[i,4]), step = 1)
      
    })
})
  
  data.cent<-reactive({
    data.cent.1 <- vector(mode="numeric")
    for (i in 1:total.rows()){
      mass <- as.numeric(input[[paste0("ind.c", i)]])[]
      data.cent.1[i] <- mass}
    
    return(data.cent.1)  
  })
  
  data.spre<-reactive({
    data.spre.1 <- array(0, dim=c(total.rows(),2))
    
    mass.s.1 <- numeric()
    mass.s.2 <- numeric()
    
      for (i in 1:total.rows()){
    
      mass.s.1[i] <- as.numeric(input[[paste0("ind.s", i)]])[1]
      mass.s.2[i] <- as.numeric(input[[paste0("ind.s", i)]])[2]}
      data.spre.1 <- data.frame(cbind(mass.s.1, mass.s.2))
    
    return(data.spre.1)  
  })
  output$Data.Spre<-renderTable({data.spre()})
  
  output$PlotValues <- renderPlot({
    vdat.3<-data.frame(vdat())
    num.vals<-seq(1,total.rows())
    par(mar = c(20, 4, 2, 2))
    plot(data.cent(),pch=16, ylim=c(0,100), xaxt="n", xlim=c(1,total.rows()), xlab="", ylab="End-state Value Importance")
    
    axis(1,at=seq(1,total.rows()), label=vdat.3[,1], las=2)
    
    arrows(x0=num.vals, y0=data.spre()[,1], x1=num.vals, y1=data.spre()[,2],
           code=0, angle=90, length=0.5, col="grey", lwd=4)
    
    points(data.cent(),pch=16)
    })
  
  
  output$test.v <- renderTable({
    data.spre()
  })

###########################################################################################################
######################################## Element Analysis ##################################################
###########################################################################################################


output$contents.e <- renderTable({
  
  inFile <- input$file.elements
  
  if (is.null(inFile))
    return(NULL)
  
  read.csv(inFile$datapath, header = input$header)
})


data.e <- reactive({ 
  req(input$file.elements) ##  require that the input is available
  
  inFile <- input$file.elements 
  df.e <- read.csv(inFile$datapath, header = input$header)#, quote = input$quote)#sep = input$sep)
  return(df.e)
})


output$contents.f<-renderTable({
data.e()[,1:5]
})


# Create vdat.e
vdat.e <- reactive({
  vdat.1.e<-data.e()
  vdat.1.e<-vdat.1.e[ order(vdat.1.e$Value,vdat.1.e$Element), ]
  return(vdat.1.e)
})

# calculate total number of rows
total.rows.e <- reactive({
  tot.row.e<-nrow(vdat.e())
  return(tot.row.e)
})

output$sliders.centroid.v.e <- renderUI({

 lapply(1:total.rows.e(), function(i) {
    vdat.2.e<-data.frame((vdat.e()))
    sliderInput(inputId = paste0("ind.c.e", i),label = h5(vdat.2.e[i,1],": ", vdat.2.e[i,2]), 
                min = 0, max = 100, value = vdat.2.e[i,3], step = 1)

  })
})

output$sliders.spread.v.e <- renderUI({
  
  lapply(1:total.rows.e(), function(i) {
    vdat.2.e<-data.frame((vdat.e()))
    sliderInput(inputId = paste0("ind.s.e", i),label = h5(vdat.2.e[i,1],": ", vdat.2.e[i,2]), 
                min = 0, max = 100, value = c(vdat.2.e[i,4],vdat.2.e[i,5]), step = 1)
  })
})

data.cent.v.e<-reactive({
  data.cent.1.v.e <- vector(mode="numeric")
  for (i in 1:total.rows.e()){
    mass.v.e <- as.numeric(input[[paste0("ind.c.e", i)]])[]
    data.cent.1.v.e[i] <- mass.v.e}
  
  return(data.cent.1.v.e)  
})



data.spre.v.e<-reactive({
  data.spre.1.v.e <- array(0, dim=c(total.rows.e(),2))
  
  mass.s.1.spre <- numeric()
  mass.s.2.spre <- numeric()
  
  for (i in 1:total.rows.e()){
    
    mass.s.1.spre[i] <- as.numeric(input[[paste0("ind.s.e", i)]])[1]
    mass.s.2.spre[i] <- as.numeric(input[[paste0("ind.s.e", i)]])[2]}
  data.spre.1.v.e <- data.frame(cbind(mass.s.1.spre, mass.s.2.spre))
  
  return(data.spre.1.v.e)  
})


output$PlotValueElements <- renderPlot({
  
  vdat.3.e<-data.frame(vdat.e())
  num.vals.e<-seq(1,nrow(vdat.3.e))
  vdat.3.e$Value_and_Element = paste(vdat.3.e$Value,vdat.3.e$Element)
  vdat.3.e$Val.Number <- as.numeric( factor(vdat.3.e$Value) )
  
  par(mar = c(20, 4, 2, 2))
  
    plot(data.cent.v.e(), pch=16, ylim=c(0,100), xaxt="n",  
         xlab="", ylab="End-state Value Importance")
  
  axis(1,at=seq(1,total.rows.e()), label=vdat.3.e$Value_and_Element, las=2)
  
    arrows(x0=num.vals.e, y0=data.spre.v.e()[,1], x1=num.vals.e, y1=data.spre.v.e()[,2],
           code=0, angle=90, length=0.5, col="grey", lwd=4)
  
    points(data.cent.v.e(),pch=16, cex=1.5, col=vdat.3.e$Val.Number)


})



data.cent.ave<-reactive({
  vdat.4.e<-data.frame(vdat.e())  
  data.cent.ave.1 <- data.frame(cbind(vdat.4.e[,1:2],data.cent.v.e(),vdat.4.e[,6:8]))
  data.cent.ave.1$adj_est<-data.cent.ave.1[,3]*(data.cent.ave.1[,4]/100)
  data.cent.ave.2<-aggregate(data.cent.ave.1[,7], list(data.cent.ave.1[,2]), FUN=mean)
 
  
    return(data.cent.ave.2)  
})

data.spre.low.ave<-reactive({
  vdat.4.e.low.spre<-data.frame(vdat.e())  
  data.spre.low.ave.1 <- data.frame(cbind(vdat.4.e.low.spre[,1:2],data.spre.v.e()[,1],vdat.4.e.low.spre[,6:8]))
	data.spre.low.ave.1$Adj_est<-data.spre.low.ave.1[,3]*(data.spre.low.ave.1[,5]/100)
  data.spre.low.ave.2<-aggregate(data.spre.low.ave.1[,7], list(data.spre.low.ave.1[,2]), FUN=mean)
 
  return(data.spre.low.ave.2)  
})

data.spre.high.ave<-reactive({
  vdat.4.e.high.spre<-data.frame(vdat.e())  
  data.spre.high.ave.1 <- data.frame(cbind(vdat.4.e.high.spre[,1:2],data.spre.v.e()[,2],vdat.4.e.high.spre[,6:8]))
	data.spre.high.ave.1$Adj_est<-data.spre.high.ave.1[,3]*(data.spre.high.ave.1[,6]/100)
  data.spre.high.ave.2<-aggregate(data.spre.high.ave.1[,7], list(data.spre.high.ave.1[,2]), FUN=mean)

  return(data.spre.high.ave.2)  
})

output$PlotElementsStakeholderUtility <- renderPlot({
  num.vals.e.ave<-seq(1,nrow(data.cent.ave()))
  
  par(mar = c(30, 4, 2, 2))
  plot(data.cent.ave()[,2], pch=16, ylim=c(0,100), xaxt="n",  
       xlab="", ylab="Mean Importance (Calculated over all end-state values)")
  
  axis(1,at=num.vals.e.ave,label=data.spre.low.ave()[,1], las=2)
  
 arrows(x0=num.vals.e.ave, y0=data.spre.low.ave()[,2], x1=num.vals.e.ave, y1=data.spre.high.ave()[,2],
      code=0, angle=90, length=0.5, col="grey", lwd=4)
  
  points(data.cent.ave()[,2],pch=16, cex=1.5)
  
  
})





###########################################################################################################
######################################## Risk Analysis ##################################################
###########################################################################################################


output$contents.risk <- renderTable({
  
  inFile <- input$file.risk
  
  if (is.null(inFile))
    return(NULL)
  
  read.csv(inFile$datapath, header = input$header)
})

data.risk <- reactive({ 
  req(input$file.risk) ##  require that the input is available
  
  inFile <- input$file.risk 
  df.risk <- read.csv(inFile$datapath, header = input$header)#, quote = input$quote)#sep = input$sep)
  return(df.risk)
})

# Create vdat.risk
vdat.risk <- reactive({
  vdat.1.risk<-data.risk()
  vdat.1.risk<-vdat.1.risk[ order(vdat.1.risk$Element,vdat.1.risk$Risk), ]
  return(vdat.1.risk)
})

# calculate total number of rows
total.rows.risk <- reactive({
  tot.row.risk<-nrow(vdat.risk())
  return(tot.row.risk)
})

output$sliders.centroid.v.risk <- renderUI({
  
  lapply(1:total.rows.risk(), function(i) {
    vdat.2.risk<-data.frame((vdat.risk()))
    sliderInput(inputId = paste0("ind.c.risk", i),label = h5(vdat.2.risk[i,1],": ", vdat.2.risk[i,2]), 
                min = 0, max = 100, value = vdat.2.risk[i,3], step = 1)
    
  })
})



output$sliders.spread.v.risk <- renderUI({
  
  lapply(1:total.rows.risk(), function(i) {
    vdat.2.risk<-data.frame((vdat.risk()))
    sliderInput(inputId = paste0("ind.s.risk", i),label = h5(vdat.2.risk[i,1],": ", vdat.2.risk[i,2]), 
                min = 0, max = 100, value = c(vdat.2.risk[i,4],vdat.2.risk[i,5]), step = 1)
  })
})

data.cent.v.risk<-reactive({
  data.cent.1.v.risk <- vector(mode="numeric")
  for (i in 1:total.rows.risk()){
    mass.v.risk <- as.numeric(input[[paste0("ind.c.risk", i)]])[]
    data.cent.1.v.risk[i] <- mass.v.risk}
  
  return(data.cent.1.v.risk)  
})


data.spre.v.risk<-reactive({
  data.spre.1.v.risk <- array(0, dim=c(total.rows.risk(),2))
  
  mass.r.1.spre <- numeric()
  mass.r.2.spre <- numeric()
  
  for (i in 1:total.rows.risk()){
    
    mass.r.1.spre[i] <- as.numeric(input[[paste0("ind.s.risk", i)]])[1]
    mass.r.2.spre[i] <- as.numeric(input[[paste0("ind.s.risk", i)]])[2]}
  data.spre.1.v.risk <- data.frame(cbind(mass.r.1.spre, mass.r.2.spre))
  
  return(data.spre.1.v.risk)  
})



output$PlotElementsRisk <- renderPlot({
  
  vdat.3.risk<-data.frame(vdat.risk())
  num.vals.risk<-seq(1,nrow(vdat.3.risk))
  vdat.3.risk$Element_Risk = paste(vdat.3.risk$Element,vdat.3.risk$Risk)
  vdat.3.risk$Element.Number <- as.numeric( factor(vdat.3.risk$Element) )
  
  par(mar = c(20, 4, 2, 2))
  
  plot(data.cent.v.risk(), pch=16, ylim=c(0,100), xaxt="n",  
       xlab="", ylab="Direct Risk Factor Importance")
  
  axis(1,at=seq(1,total.rows.risk()), label=vdat.3.risk$Element_Risk, las=2)
  
  arrows(x0=num.vals.risk, y0=data.spre.v.risk()[,1], x1=num.vals.risk, 
         y1=data.spre.v.risk()[,2],
         code=0, angle=90, length=0.5, col="grey", lwd=4)
  
  points(data.cent.v.risk(),pch=16, cex=1.5, col=vdat.3.risk$Element.Number)
  
  
})



data.cent.ave.risk<-reactive({
  vdat.4.risk<-data.frame(vdat.risk())  
  data.cent.ave.risk.1 <- data.frame(cbind(vdat.4.risk[,1:2],data.cent.v.risk()))
  data.cent.ave.risk.2<-aggregate(data.cent.ave.risk.1[,3], list(data.cent.ave.risk.1[,1]), FUN=mean)   
  return(data.cent.ave.risk.2)  
})

data.spre.low.ave.risk<-reactive({
  vdat.4.risk.low.spre<-data.frame(vdat.risk())  
  data.spre.low.risk.ave.1 <- data.frame(cbind(vdat.4.risk.low.spre[,1:2],data.spre.v.risk()[,1]))
  data.spre.low.risk.ave.2<-aggregate(data.spre.low.risk.ave.1[,3], list(data.spre.low.risk.ave.1[,1]), FUN=mean)
  
  return(data.spre.low.risk.ave.2)  
})


data.spre.high.ave.risk<-reactive({
  vdat.4.risk.high.spre<-data.frame(vdat.risk())  
  data.spre.high.risk.ave.1 <- data.frame(cbind(vdat.4.risk.high.spre[,1:2],data.spre.v.risk()[,2]))
  data.spre.high.risk.ave.2<-aggregate(data.spre.high.risk.ave.1[,3], list(data.spre.high.risk.ave.1[,1]), FUN=mean)
  
  return(data.spre.high.risk.ave.2)  
})



output$PlotRiskAverage <- renderPlot({
  num.vals.risk.ave<-seq(1,nrow(data.cent.ave.risk()))
  
  par(mar = c(20, 4, 2, 2))
  plot(data.cent.ave.risk()[,2], pch=16, xaxt="n",  ylim=c(0,100),
       xlab="", ylab="Mean Importance (Calculated over all risk factors)")
  
  axis(1,at=seq(1,nrow(data.cent.ave.risk())), label=data.cent.ave.risk()[,1], las=2)
  
  arrows(x0=num.vals.risk.ave, y0=data.spre.low.ave.risk()[,2],
         x1=num.vals.risk.ave, y1=data.spre.high.ave.risk()[,2],
         code=0, angle=90, length=0.5, col="grey", lwd=4)
  
  points(data.cent.ave.risk()[,2],pch=16, cex=1.5)
  
  
})


#############################################################################################################
data.cent.ave.risk.Fact<-reactive({
  vdat.4.risk.Fact<-data.frame(vdat.risk())  
  data.cent.ave.risk.Fact.1 <- data.frame(cbind(vdat.4.risk.Fact[,1:2],data.cent.v.risk()))
  data.cent.ave.risk.Fact.2<-aggregate(data.cent.ave.risk.Fact.1[,3], list(data.cent.ave.risk.Fact.1[,2]), FUN=mean)   
  
  return(data.cent.ave.risk.Fact.2)  
})



data.spre.low.ave.risk.Fact<-reactive({
  vdat.4.risk.low.spre.Fact<-data.frame(vdat.risk())  
  data.spre.ave.low.risk.Fact.1 <- data.frame(cbind(vdat.4.risk.low.spre.Fact[,1:2],data.spre.v.risk()[,1]))
    data.spre.ave.low.risk.Fact.2<-aggregate(data.spre.ave.low.risk.Fact.1[,3], list(data.spre.ave.low.risk.Fact.1[,2]), FUN=mean)   
  return(data.spre.ave.low.risk.Fact.2)  
})

data.spre.high.ave.risk.Fact<-reactive({
  vdat.4.risk.high.spre.Fact<-data.frame(vdat.risk())  
  data.spre.ave.high.risk.Fact.1 <- data.frame(cbind(vdat.4.risk.high.spre.Fact[,1:2],data.spre.v.risk()[,2]))
  data.spre.ave.high.risk.Fact.2<-aggregate(data.spre.ave.high.risk.Fact.1[,3], list(data.spre.ave.high.risk.Fact.1[,2]), FUN=mean)   
  return(data.spre.ave.high.risk.Fact.2)  
})

output$PlotRiskAverageFact <- renderPlot({
  num.vals.risk.ave.Fact<-seq(1,nrow(data.cent.ave.risk.Fact()))
  
  par(mar = c(20, 4, 2, 2))
  plot(data.cent.ave.risk.Fact()[,2], pch=16, xaxt="n", ylim=c(0,100),
       xlab="", ylab="Mean Importance (Calculated over all elements)")
  
  axis(1,at=num.vals.risk.ave.Fact, label=data.cent.ave.risk.Fact()[,1], las=2)
  
  arrows(x0=num.vals.risk.ave.Fact, y0=data.spre.low.ave.risk.Fact()[,2],
         x1=num.vals.risk.ave.Fact, y1=data.spre.high.ave.risk.Fact()[,2],
         code=0, angle=90, length=0.5, col="grey", lwd=4)
  
  points(data.cent.ave.risk.Fact()[,2],pch=16, cex=1.5)
  
  
})

###########################################################################################################
######################################## Utility Analysis ##################################################
###########################################################################################################


output$Max.Property <- renderText({ input$obs })
output$Current.Property <- renderText({ input$p.obs })
#output$Future.Property <- renderText({ input$f.obs })

data.up.1 <- reactive({ 
  req(input$file.up)
  
  ext <- tools::file_ext(input$file.up$name)
  switch(ext,
         csv = vroom::vroom(input$file.up$datapath, delim = ","),
         tsv = vroom::vroom(input$file.up$datapath, delim = "\t"),
         validate("Invalid file; Please upload a .csv or .tsv file")
  )
})


data.up <- reactive({ 
  df.up<-data.frame(data.up.1())
  return(df.up)
})

output$sliders.rules <- renderUI({
  
  if(input$Add.Data == "Yes"){    
    
    lapply(1:7, function(i) {
      noUiSliderInput(
        inputId = paste0("noui", i), 
        min = 0, max = 100, step = 1,
        value = c(data.up()[i,4],data.up()[i,3],data.up()[i,5]), 
        tooltips = FALSE,
        orientation = "vertical",
        direction = c("rtl"),
        color = "#FF0000", inline = TRUE,
        width = "30px", height = "300px"
      )})#})
    
  } else {
    
    lapply(1:7, function(i) {
      noUiSliderInput(
        inputId = paste0("noui", i), 
        min = 0, max = 100, step = 1,
        value = c(0,50,100), 
        tooltips = FALSE,
        orientation = "vertical",
        direction = c("rtl"),
        color = "#FF0000", inline = TRUE,
        width = "30px", height = "300px"
      )})
  }
})


###### Organise Slider data  

PropValData.2 <- reactive({ 
  x.data=seq(1,input$obs, length = 7)
  y.data.centroid<-c(input$noui1[2],input$noui2[2],input$noui3[2],input$noui4[2],input$noui5[2],input$noui6[2],input$noui7[2])
  y.data.low<-c(input$noui1[1],input$noui2[1],input$noui3[1],input$noui4[1],input$noui5[1],input$noui6[1],input$noui7[1])
  y.data.high<-c(input$noui1[3],input$noui2[3],input$noui3[3],input$noui4[3],input$noui5[3],input$noui6[3],input$noui7[3])
  PropValData.1<-data.frame(cbind(x.data,y.data.centroid, y.data.low, y.data.high))
  colnames(PropValData.1)<-c("Property_score","Value_centroid","Value_low","Value_high")
  return(PropValData.1)
})


output$PropValData<-renderTable({CentCalc()})

###### Estimate Current Element-Property Value
CentCalc <- reactive({
  CentCalc.1 <- data.frame(x = c(-Inf, input$p.obs, input$p.obs), 
                           y = c(approx(PropValData.2()[,1], PropValData.2()[,2], input$p.obs)$y, 
                                 approx(PropValData.2()[,1], PropValData.2()[,2], input$p.obs)$y, -Inf))
  return(CentCalc.1)
})

LowCalc <- reactive({
  LowCalc.1 <- data.frame(x = c(-Inf, input$p.obs.l, input$p.obs.l), 
                          y = c(approx(PropValData.2()[,1], PropValData.2()[,3], input$p.obs.l)$y, 
                                approx(PropValData.2()[,1], PropValData.2()[,3], input$p.obs.l)$y, -Inf))
  return(LowCalc.1)
})                      

HighCalc <- reactive({
  HighCalc.1 <- data.frame(x = c(-Inf, input$p.obs.h, input$p.obs.h), 
                           y = c(approx(PropValData.2()[,1], PropValData.2()[,4], input$p.obs.h)$y, 
                                 approx(PropValData.2()[,1], PropValData.2()[,4], input$p.obs.h)$y, -Inf))
  return(HighCalc.1)
})




output$PlotRules <- renderPlot({
  PropValData<-data.frame(PropValData.2())
  
  plot(PropValData$Property_score,PropValData$Value_centroid, ylim=c(0,100), 
       pch=16, type = "b", lwd=3, xlab=input$Prop.name.1, ylab=input$Value.name.1)
  
  points(PropValData$Property_score, PropValData$Value_low, type = "b", col="grey")
  points(PropValData$Property_score,PropValData$Value_high, type = "b", col="grey")
  
  #### Current
  arrows(x0=CentCalc()[2,1], y0=0, x1=CentCalc()[2,1], y1=CentCalc()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.15), lwd=2)
  arrows(x0=0, y0=CentCalc()[2,2], x1=CentCalc()[2,1], y1=CentCalc()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.15), lwd=2)
  
  arrows(x0=LowCalc()[2,1], y0=0, x1=LowCalc()[2,1], y1=LowCalc()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.15), lwd=2)
  arrows(x0=0, y0=LowCalc()[2,2], x1=LowCalc()[2,1], y1=LowCalc()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.15), lwd=2)
  
  arrows(x0=HighCalc()[2,1], y0=0, x1=HighCalc()[2,1], y1=HighCalc()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.15), lwd=2)
  arrows(x0=0, y0=HighCalc()[2,2], x1=HighCalc()[2,1], y1=HighCalc()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.15), lwd=2)
  
})      


Model.Utility <- reactive({

  Model.Utility.1<-data.frame(cbind(input$Prop.name.1,input$Element.name.1,input$Value.name.1, CentCalc()[2,2], LowCalc()[2,2], HighCalc()[2,2]))
  
  colnames(Model.Utility.1)<-c("Property", "Element","Value","Current Value","Current Lower","Current Upper")
  
  return(Model.Utility.1)
})

output$Benefit.u<- renderTable(Model.Utility())
output$data.up.2<-renderTable(data.up())
output$Slider.Res<-renderTable(PropValData.2())


output$download <- downloadHandler(
  filename = function() {
    paste0(input$Prop.name.1,input$Element.name.1,input$Value.name.1, Sys.Date(), ".csv", sep="")
    
    
  },
  content = function(file) {
    write.csv(PropValData.2(), file)
  }
)





###########################################################################################################
########################################## Model Utility ##################################################
###########################################################################################################


data.model <- reactive({ 
  req(input$file3) ##  require that the input is available
  
  inFile <- input$file3 
  
  df.3 <- read.csv(inFile$datapath, header = input$header)#, quote = input$quote)#sep = input$sep)
  
  return(df.3)
})

# Create vdat and sort
vdat.model <- reactive({
  vdat.1.model<-data.model()
  vdat.1.model<-vdat.1.model[ order(vdat.1.model$Element,vdat.1.model$Value), ]
  
  return(vdat.1.model)
})

# calculate total number of rows
Numrows <- reactive({
  tot.row.model<-nrow(vdat.model())
  return(tot.row.model)
})

Numcols <- reactive({
  tot.col.model<-ncol(vdat.model())
  return(tot.col.model)
})


n.prop<-reactive({
  n.prop.1<-(Numcols()-5)/4
  return(n.prop.1)
})


cent.prop.final<-reactive({
  
  cent.prop<-array(0, dim = c(Numrows(),(n.prop())))
  spre.prop.l<-array(0, dim = c(Numrows(),(n.prop())))
  spre.prop.h<-array(0, dim = c(Numrows(),(n.prop())))
  
  cent.prop<-data.frame(cent.prop)
  spre.prop.l<-data.frame(spre.prop.l)
  spre.prop.h<-data.frame(spre.prop.h)
  
  for(i in 1:n.prop()){
    cent.prop[,i]<-  vdat.model()[,(i+2)]
    spre.prop.l[,i]<-vdat.model()[,(i+2+n.prop())]
    spre.prop.h[,i]<-vdat.model()[,(i+2+(2*n.prop()))]
  }
  
  cent.prop.all.1<-  cbind(data.frame(vdat.model()[,1],cent.prop[,]))
  spre.prop.all.l.1<-cbind(data.frame(vdat.model()[,1],spre.prop.l[,]))
  spre.prop.all.h.1<-cbind(data.frame(vdat.model()[,1],spre.prop.h[,]))
 
  
  cent.prop.all.2<-cent.prop.all.1[,2]*(vdat.model()[,Numcols()-2]/100)
  cent.prop.all<-  cbind(data.frame(vdat.model()[,1],cent.prop.all.2))
  
  spre.prop.all.l.2<-spre.prop.all.l.1[,2]*(vdat.model()[,Numcols()-1]/100)
  spre.prop.all.l<-  cbind(data.frame(vdat.model()[,1],spre.prop.all.l.2))
  
  spre.prop.all.h.2<-spre.prop.all.h.1[,2]*(vdat.model()[,Numcols()]/100)
  spre.prop.all.h<-  cbind(data.frame(vdat.model()[,1],spre.prop.all.h.2))
  
  
  cent.prop.all.mean<-    aggregate(cent.prop.all[,-1], by = list(cent.prop.all[,1]), FUN = mean)
  spre.prop.all.l.mean<-aggregate(spre.prop.all.l[,-1], by = list(spre.prop.all.l[,1]), FUN = mean)
  spre.prop.all.h.mean<-aggregate(spre.prop.all.h[,-1], by = list(spre.prop.all.h[,1]), FUN = mean)
  
  
  cent.prop.final.1<-data.frame(cbind(cent.prop.all.mean,spre.prop.all.l.mean[,-1],spre.prop.all.h.mean[,-1]))
  colnames(cent.prop.final.1)<-c("Element","Mean","Low","High")
  return(cent.prop.final.1)
})


  
  
  
output$contents.model <- renderTable({
  cent.prop.final()
})


output$sliders.centroid.model <- renderUI({
  
  lapply(1:nrow(cent.prop.final()), function(i) {
    vdat.2.model<-data.frame(cent.prop.final())
    sliderInput(inputId = paste0("ind.c.model", i),label = vdat.2.model[i,1], 
                min = 0, max = 100, value = vdat.2.model[i,2], step = 1)
    
  })
})

output$sliders.spread.model <- renderUI({
  
  lapply(1:nrow(cent.prop.final()), function(i) {
    vdat.2.model<-data.frame(cent.prop.final())
    sliderInput(inputId = paste0("ind.s.model", i),label = vdat.2.model[i,1], 
                min = 0, max = 100, value = c(vdat.2.model[i,3],vdat.2.model[i,4]), step = 1)
    
  })
})

data.cent.model<-reactive({
  data.cent.1.model <- vector(mode="numeric")
  for (i in 1:nrow(cent.prop.final())){
    mass.model <- as.numeric(input[[paste0("ind.c.model", i)]])[]
    data.cent.1.model[i] <- mass.model}
  
  return(data.cent.1.model)})



data.spre.v.model<-reactive({
  data.spre.1.v.model <- array(0, dim=c(nrow(cent.prop.final()),2))
  
  mass.m.1.spre <- numeric()
  mass.m.2.spre <- numeric()
  
  for (i in 1:nrow(cent.prop.final())){
    
    mass.m.1.spre[i] <- as.numeric(input[[paste0("ind.s.model", i)]])[1]
    mass.m.2.spre[i] <- as.numeric(input[[paste0("ind.s.model", i)]])[2]
    }
  
    data.spre.1.v.model <- cbind(data.frame(mass.m.1.spre, mass.m.2.spre))
  
  return(data.spre.1.v.model)  
})




output$PlotUtility <- renderPlot({
  vdat.3.model<-data.frame(cent.prop.final())
  num.vals.model<-seq(1,nrow(cent.prop.final()))
  par(mar = c(20, 4, 2, 2))
  plot(data.cent.model(),pch=16, ylim=c(0,100), xaxt="n", xlim=c(1,nrow(cent.prop.final())), xlab="", 
       ylab="Element Utility")
  
  axis(1,at=num.vals.model, label=vdat.3.model[,1], las=2)
  
  arrows(x0=num.vals.model, y0=data.spre.v.model()[,1], x1=num.vals.model, y1=data.spre.v.model()[,2],
         code=0, angle=90, length=0.5, col="grey", lwd=4)
  
  points(data.cent.model(),pch=16)
})

output$TestSpre<-renderTable({data.spre.v.model()})






###########################################################################################################
############################################# Comparison ##################################################
###########################################################################################################


output$Comparison.Plot <- renderPlot({
 
 plot(data.cent.model(),data.cent.ave()[,2],pch=16, ylim=c(0,100), xlim=c(0,100),
       ylab="Stakeholder Utility",
       xlab="Model Utility")
  
   arrows(x0=data.cent.model(), y0=data.spre.low.ave()[,2], x1=data.cent.model(), 
         y1=data.spre.high.ave()[,2],
         code=0, angle=90, length=0.5, col="grey", lwd=4)
  
  arrows(x0=data.spre.v.model()[,1],y0=data.cent.ave()[,2], 
         x1=data.spre.v.model()[,2],y1=data.cent.ave()[,2],
         code=0, angle=90, length=0.5, col="grey", lwd=4)
  
  abline(0,1, col="red", lw=1.5)
  
  text(x=data.cent.model(),y=data.cent.ave()[,2], lab=cent.prop.final()[,1], cex=1.2, pos=3)
  
  points(data.cent.model(),data.cent.ave()[,2],pch=16)
})




###########################################################################################################
######################################## Benefit Analysis ##################################################
###########################################################################################################


output$Max.Property.benefit <- renderText({ input$obs.benefit })
output$Current.Property.benefit <- renderText({ input$p.obs.benefit })
output$Future.Property.benefit <- renderText({ input$f.obs.benefit })

data.up.1.benefit <- reactive({ 
  req(input$file.up.benefit)
  
  ext <- tools::file_ext(input$file.up.benefit$name)
  switch(ext,
         csv = vroom::vroom(input$file.up.benefit$datapath, delim = ","),
         tsv = vroom::vroom(input$file.up.benefit$datapath, delim = "\t"),
         validate("Invalid file; Please upload a .csv or .tsv file")
  )
})


data.up.benefit <- reactive({ 
  df.up.benefit<-data.frame(data.up.1.benefit())
  return(df.up.benefit)
})

output$sliders.rules.benefit <- renderUI({
  
  if(input$Add.Data.benefit == "Yes"){    
    
    lapply(1:7, function(i) {
      noUiSliderInput(
        inputId = paste0("noui.benefit", i), 
        min = 0, max = 100, step = 1,
        value = c(data.up.benefit()[i,4],data.up.benefit()[i,3],data.up.benefit()[i,5]), 
        tooltips = FALSE,
        orientation = "vertical",
        direction = c("rtl"),
        color = "#FF0000", inline = TRUE,
        width = "30px", height = "300px"
      )})#})
    
  } else {
    
    lapply(1:7, function(i) {
      noUiSliderInput(
        inputId = paste0("noui.benefit", i), 
        min = 0, max = 100, step = 1,
        value = c(0,50,100), 
        tooltips = FALSE,
        orientation = "vertical",
        direction = c("rtl"),
        color = "#FF0000", inline = TRUE,
        width = "30px", height = "300px"
      )})
  }
})


###### Organise Slider data  

PropValData.2.benefit <- reactive({ 
  x.data.benefit=seq(1,input$obs.benefit, length = 7)
  y.data.centroid.benefit<-c(input$noui.benefit1[2],input$noui.benefit2[2],input$noui.benefit3[2],input$noui.benefit4[2],input$noui.benefit5[2],input$noui.benefit6[2],input$noui.benefit7[2])
  y.data.low.benefit<-c(input$noui.benefit1[1],input$noui.benefit2[1],input$noui.benefit3[1],input$noui.benefit4[1],input$noui.benefit5[1],input$noui.benefit6[1],input$noui.benefit7[1])
  y.data.high.benefit<-c(input$noui.benefit1[3],input$noui.benefit2[3],input$noui.benefit3[3],input$noui.benefit4[3],input$noui.benefit5[3],input$noui.benefit6[3],input$noui.benefit7[3])
  PropValData.1.benefit<-data.frame(cbind(x.data.benefit,y.data.centroid.benefit, y.data.low.benefit, y.data.high.benefit))
  colnames(PropValData.1.benefit)<-c("Property_score","Value_centroid","Value_low","Value_high")
  return(PropValData.1.benefit)
})


output$PropValData.benefit<-renderTable({CentCalc.benefit()})

###### Estimate Current Element-Property Value
CentCalc.benefit <- reactive({
  CentCalc.1.benefit <- data.frame(x = c(-Inf, input$p.obs.benefit, input$p.obs.benefit), 
                           y = c(approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,2], input$p.obs.benefit)$y, 
                                 approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,2], input$p.obs.benefit)$y, -Inf))
  return(CentCalc.1.benefit)
})

LowCalc.benefit <- reactive({
  LowCalc.1.benefit <- data.frame(x = c(-Inf, input$p.obs.l.benefit, input$p.obs.l.benefit), 
                          y = c(approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,3], input$p.obs.l.benefit)$y, 
                                approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,3], input$p.obs.l.benefit)$y, -Inf))
  return(LowCalc.1.benefit)
})                      

HighCalc.benefit <- reactive({
  HighCalc.1.benefit <- data.frame(x = c(-Inf, input$p.obs.h.benefit, input$p.obs.h.benefit), 
                           y = c(approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,4], input$p.obs.h.benefit)$y, 
                                 approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,4], input$p.obs.h.benefit)$y, -Inf))
  return(HighCalc.1.benefit)
})


###### Estimate Future Element-Property Value
CentCalc.f <- reactive({
  CentCalc.f.1 <- data.frame(x = c(-Inf, input$f.obs.benefit, input$f.obs.benefit), 
                             y = c(approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,2], input$f.obs.benefit)$y, 
                                   approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,2], input$f.obs.benefit)$y, -Inf))
  return(CentCalc.f.1)
})

LowCalc.f <- reactive({
  LowCalc.f.1 <- data.frame(x = c(-Inf, input$f.obs.l.benefit, input$f.obs.l.benefit), 
                            y = c(approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,3], input$f.obs.l.benefit)$y, 
                                  approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,3], input$f.obs.l.benefit)$y, -Inf))
  return(LowCalc.f.1)
})                      

HighCalc.f <- reactive({
  HighCalc.f.1 <- data.frame(x = c(-Inf, input$f.obs.h.benefit, input$f.obs.h.benefit), 
                             y = c(approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,4], input$f.obs.h.benefit)$y, 
                                   approx(PropValData.2.benefit()[,1], PropValData.2.benefit()[,4], input$f.obs.h.benefit)$y, -Inf))
  return(HighCalc.f.1)
})


output$PlotRules.benefit <- renderPlot({
  PropValData.benefit<-data.frame(PropValData.2.benefit())
  
  plot(PropValData.benefit$Property_score,PropValData.benefit$Value_centroid, ylim=c(0,100), 
       pch=16, type = "b", lwd=3, xlab=input$Prop.name.1.benefit, ylab=input$Value.name.1.benefit)
  
  points(PropValData.benefit$Property_score, PropValData.benefit$Value_low, type = "b", col="grey")
  points(PropValData.benefit$Property_score,PropValData.benefit$Value_high, type = "b", col="grey")
  
  #### Current
  arrows(x0=CentCalc.benefit()[2,1], y0=0, x1=CentCalc.benefit()[2,1], y1=CentCalc.benefit()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.15), lwd=2)
  arrows(x0=0, y0=CentCalc.benefit()[2,2], x1=CentCalc.benefit()[2,1], y1=CentCalc.benefit()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.15), lwd=2)
  
  arrows(x0=LowCalc.benefit()[2,1], y0=0, x1=LowCalc.benefit()[2,1], y1=LowCalc.benefit()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.15), lwd=2)
  arrows(x0=0, y0=LowCalc.benefit()[2,2], x1=LowCalc.benefit()[2,1], y1=LowCalc.benefit()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.15), lwd=2)
  
  arrows(x0=HighCalc.benefit()[2,1], y0=0, x1=HighCalc.benefit()[2,1], y1=HighCalc.benefit()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.15), lwd=2)
  arrows(x0=0, y0=HighCalc.benefit()[2,2], x1=HighCalc.benefit()[2,1], y1=HighCalc.benefit()[2,2],code=0, angle=90, length=0.5, lty=2, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.15), lwd=2)
  
  #### Future          
    arrows(x0=CentCalc.f()[2,1], y0=0, x1=CentCalc.f()[2,1], y1=CentCalc.f()[2,2],code=0, angle=90, length=0.5, lty=2, col="green", lwd=3)
    arrows(x0=0, y0=CentCalc.f()[2,2], x1=CentCalc.f()[2,1], y1=CentCalc.f()[2,2],code=0, angle=90, length=0.5, lty=2, col="green", lwd=3)
  
    arrows(x0=LowCalc.f()[2,1], y0=0, x1=LowCalc.f()[2,1], y1=LowCalc.f()[2,2],code=0, angle=90, length=0.5, lty=2, col="orange", lwd=3)
    arrows(x0=0, y0=LowCalc.f()[2,2], x1=LowCalc.f()[2,1], y1=LowCalc.f()[2,2],code=0, angle=90, length=0.5, lty=2, col="orange", lwd=3)
  
    arrows(x0=HighCalc.f()[2,1], y0=0, x1=HighCalc.f()[2,1], y1=HighCalc.f()[2,2],code=0, angle=90, length=0.5, lty=2, col="orange", lwd=3)
    arrows(x0=0, y0=HighCalc.f()[2,2], x1=HighCalc.f()[2,1], y1=HighCalc.f()[2,2],code=0, angle=90, length=0.5, lty=2, col="orange", lwd=3)
  
})      


Benefit <- reactive({
  Benefit.centroid.1 <-CentCalc.f()[2,2]-CentCalc.benefit()[2,2]
  Benefit.high.1 <-HighCalc.f()[2,2]-CentCalc.benefit()[2,2]#abs(HighCalc.benefit()[2,2]) - CentCalc.f()[2,2]   #-HighCalc.benefit()[2,2]
  #Benefit.high.1<-Benefit.high.1.1-Benefit.centroid.1
  Benefit.low.1 <- LowCalc.f()[2,2]-CentCalc.benefit()[2,2] #-LowCalc.benefit()[2,2]
  #Benefit.low.1 <-Benefit.low.1.1+Benefit.centroid.1
  
  Benefit.1<-data.frame(cbind(input$Manage.name.1.benefit,input$Prop.name.1.benefit,input$Element.name.1.benefit,input$Value.name.1.benefit, CentCalc.benefit()[2,2], LowCalc.benefit()[2,2], HighCalc.benefit()[2,2],Benefit.centroid.1,Benefit.low.1,Benefit.high.1))
  colnames(Benefit.1)<-c("Management Option","Property", "Element","Value","Current Value","Current Lower","Current Upper","Benefit Centroid","Benefit Lower","Benefit Upper")

  return(Benefit.1)
})



output$Benefit.o<- renderTable(Benefit())
output$testit<-renderTable(HighCalc.f())
output$testit2<-renderTable(LowCalc.f())

output$data.up.2.benefit<-renderTable(data.up.benefit())
output$Slider.Res.benefit<-renderTable(PropValData.2.benefit())



output$download.benefit <- downloadHandler(
  filename = function() {
    paste0(input$Prop.name.1.benefit,input$Element.name.1.benefit,input$Value.name.1.benefit, Sys.Date(), ".csv", sep="")
    
    
  },
  content = function(file) {
    write.csv(PropValData.2.benefit(), file)
  }
)




###########################################################################################################
########################################## Benefit Analysis Part 2 ##################################################
###########################################################################################################

data.model.BA <- reactive({ 
  req(input$fileBA2) ##  require that the input is available
  
  inFile <- input$fileBA2 
  
  df.BA2 <- read.csv(inFile$datapath, header = input$header)#, quote = input$quote)#sep = input$sep)
  
  return(df.BA2)
})

# Create vdat and sort
vdat.model.BA2 <- reactive({
  vdat.1.model.BA2<-data.model.BA()
  vdat.1.model.BA2<-vdat.1.model.BA2[ order(vdat.1.model.BA2$OptionNumber,vdat.1.model.BA2$Element,vdat.1.model.BA2$Value), ]
  
  return(vdat.1.model.BA2)
})

# calculate total number of rows
Numrows.BA2 <- reactive({
  tot.row.model.BA2<-nrow(vdat.model.BA2())
  return(tot.row.model.BA2)
})

Numcols.BA2 <- reactive({
  tot.col.model.BA2<-ncol(vdat.model.BA2())
  return(tot.col.model.BA2)
})


n.prop.BA2<-reactive({
  n.prop.1.BA2<-(Numcols.BA2()-3)/4
  return(n.prop.1.BA2)
})


cent.prop.final.BA2<-reactive({
  
  cent.prop.BA2<-array(0, dim = c(Numrows.BA2(),(n.prop.BA2())))
  spre.prop.l.BA2<-array(0, dim = c(Numrows.BA2(),(n.prop.BA2())))
  spre.prop.h.BA2<-array(0, dim = c(Numrows.BA2(),(n.prop.BA2())))
  
  cent.prop.BA2<-data.frame(cent.prop.BA2)
  spre.prop.l.BA2<-data.frame(spre.prop.l.BA2)
  spre.prop.h.BA2<-data.frame(spre.prop.h.BA2)
  
  for(i in 1:n.prop.BA2()){
    cent.prop.BA2[,i]<-  vdat.model.BA2()[,(i+3)]
    spre.prop.l.BA2[,i]<-vdat.model.BA2()[,(i+3+n.prop.BA2())]
    spre.prop.h.BA2[,i]<-vdat.model.BA2()[,(i+3+(2*n.prop.BA2()))]
  }
  
  cent.prop.all.BA2<-  cbind(data.frame(vdat.model.BA2()[,1],vdat.model.BA2()[,2],cent.prop.BA2[,]))
  spre.prop.all.l.BA2<-cbind(data.frame(vdat.model.BA2()[,1],vdat.model.BA2()[,2],spre.prop.l.BA2[,]))
  spre.prop.all.h.BA2<-cbind(data.frame(vdat.model.BA2()[,1],vdat.model.BA2()[,2],spre.prop.h.BA2[,]))
  
  cent.prop.all.mean.BA2.1<-aggregate(cent.prop.all.BA2[,-c(1:2)], by = list(cent.prop.all.BA2[,1],cent.prop.all.BA2[,2]), FUN = mean)
  spre.prop.all.l.mean.BA2.1<-aggregate(spre.prop.all.l.BA2[,-c(1:2)], by = list(spre.prop.all.l.BA2[,1],spre.prop.all.l.BA2[,2]), FUN = mean)
  spre.prop.all.h.mean.BA2.1<-aggregate(spre.prop.all.h.BA2[,-c(1:2)], by = list(spre.prop.all.h.BA2[,1],spre.prop.all.h.BA2[,2]), FUN = mean)
  
  cent.prop.all.mean.BA2.2<-cent.prop.all.mean.BA2.1[,-2]
  spre.prop.all.l.mean.BA2.2<-spre.prop.all.l.mean.BA2.1[,-2]
  spre.prop.all.h.mean.BA2.2<-spre.prop.all.h.mean.BA2.1[,-2]
  
  cent.prop.all.mean.BA2<-aggregate(cent.prop.all.mean.BA2.2[,-1], by = list(cent.prop.all.mean.BA2.2[,1]), FUN = mean)
  spre.prop.all.l.mean.BA2<-aggregate(spre.prop.all.l.mean.BA2.2[,-1], by = list(spre.prop.all.l.mean.BA2.2[,1]), FUN = mean)
  spre.prop.all.h.mean.BA2<-aggregate(spre.prop.all.h.mean.BA2.2[,-1], by = list(spre.prop.all.h.mean.BA2.2[,1]), FUN = mean)
  
  
  #cent.prop.all.mean.BA2<-aggregate(cent.prop.all.BA2[,-c(1:2)], by = list(cent.prop.all.BA2[,1],cent.prop.all.BA2[,2]), FUN = mean)
  #spre.prop.all.l.mean.BA2<-aggregate(spre.prop.all.l.BA2[,-c(1:2)], by = list(spre.prop.all.l.BA2[,1],spre.prop.all.l.BA2[,2]), FUN = mean)
  #spre.prop.all.h.mean.BA2<-aggregate(spre.prop.all.h.BA2[,-c(1:2)], by = list(spre.prop.all.h.BA2[,1],spre.prop.all.h.BA2[,2]), FUN = mean)
  
  
    
  cent.prop.final.1.BA2<-data.frame(cbind(cent.prop.all.mean.BA2,spre.prop.all.l.mean.BA2[,-1],spre.prop.all.h.mean.BA2[,-1]))
  
  colnames(cent.prop.final.1.BA2)<-c("Option","Mean","Low","High")
  return(cent.prop.final.1.BA2)
})





output$contents.model.BA2 <- renderTable({
  
  cent.prop.final.BA2()
})


output$sliders.centroid.model.BA2 <- renderUI({
  
  lapply(1:nrow(cent.prop.final.BA2()), function(i) {
    vdat.2.model.BA2<-data.frame(cent.prop.final.BA2())
    sliderInput(inputId = paste0("ind.c.model.BA2", i),label = vdat.2.model.BA2[i,1], 
                min = -100, max = 100, value = vdat.2.model.BA2[i,2], step = 1)
    
  })
})

output$sliders.spread.model.BA2 <- renderUI({
  
  lapply(1:nrow(cent.prop.final.BA2()), function(i) {
    vdat.2.model.BA2<-data.frame(cent.prop.final.BA2())
    sliderInput(inputId = paste0("ind.s.model.BA2", i),label = vdat.2.model.BA2[i,1], 
                min = -100, max = 100, value = c(vdat.2.model.BA2[i,3],vdat.2.model.BA2[i,4]), step = 1)
    
  })
})

data.cent.model.BA2<-reactive({
  data.cent.1.model.BA2 <- vector(mode="numeric")
  for (i in 1:nrow(cent.prop.final.BA2())){
    mass.model.BA2 <- as.numeric(input[[paste0("ind.c.model.BA2", i)]])[]
    data.cent.1.model.BA2[i] <- mass.model.BA2}
  
  return(data.cent.1.model.BA2)  })



data.spre.v.model.BA2<-reactive({
  data.spre.1.v.model.BA2 <- array(0, dim=c(nrow(cent.prop.final.BA2()),2))
  
  mass.m.1.spre.BA2 <- numeric()
  mass.m.2.spre.BA2 <- numeric()
  
  for (i in 1:nrow(cent.prop.final.BA2())){
    
    mass.m.1.spre.BA2[i] <- as.numeric(input[[paste0("ind.s.model.BA2", i)]])[1]
    mass.m.2.spre.BA2[i] <- as.numeric(input[[paste0("ind.s.model.BA2", i)]])[2]
  }
  
  data.spre.1.v.model.BA2 <- cbind(data.frame(mass.m.1.spre.BA2, mass.m.2.spre.BA2))
  
  return(data.spre.1.v.model.BA2)  
})




output$PlotUtility.BA2 <- renderPlot({
  vdat.3.model.BA2<-data.frame(cent.prop.final.BA2())
  num.vals.model.BA2<-seq(1,nrow(cent.prop.final.BA2()))
  par(mar = c(30, 4, 0, 0))
  plot(data.cent.model.BA2(),pch=16, ylim=c(min(data.spre.v.model.BA2()[,1]),max(data.spre.v.model.BA2()[,2])), xaxt="n", xlim=c(1,nrow(cent.prop.final.BA2())), xlab="", 
       ylab="Mean Estimate of Benefit")
  
  axis(1,at=num.vals.model.BA2, label=vdat.3.model.BA2[,1], las=2)
  
  arrows(x0=num.vals.model.BA2, y0=data.spre.v.model.BA2()[,1], x1=num.vals.model.BA2, y1=data.spre.v.model.BA2()[,2],
         code=0, angle=90, length=0.5, col="grey", lwd=4)
  
  points(data.cent.model.BA2(),pch=16)
})


BenefitOrdered<-reactive({
  BenefitOrdered.1<-data.frame(cbind(cent.prop.final.BA2()[,1],data.cent.model.BA2(),data.spre.v.model.BA2()))  
  BenefitOrdered.1<-BenefitOrdered.1[order(BenefitOrdered.1[,1]),]
  return(BenefitOrdered.1)
})

output$BenefitOrdered.0 <- renderTable({
  BenefitOrdered()
})
###########################################################################################################
######################################## Costs Analysis ##################################################
###########################################################################################################


output$cost <- renderTable({
  
  inFile <- input$file.cost
  
  if (is.null(inFile))
    return(NULL)
  
  read.csv(inFile$datapath, header = input$header)
})

data.cost <- reactive({ 
  req(input$file.cost) ##  require that the input is available
  
  inFile <- input$file.cost 
  
  df.cost <- read.csv(inFile$datapath, header = input$header)#, quote = input$quote)#sep = input$sep)
  
  return(df.cost)
})

# Create vdat and sort
vdat.cost <- reactive({
  vdat.cost.1<-data.cost()
  return(vdat.cost.1)
})


# calculate total number of rows
total.rows.cost <- reactive({
  tot.row.cost<-nrow(vdat.cost())
  return(tot.row.cost)
})


output$sliders.centroid.cost <- renderUI({
  
  
  lapply(1:total.rows.cost(), function(i) {
    vdat.cost.2<-data.frame(vdat.cost(), digits=2)
    sliderInput(inputId = paste0("ind.c.cost", i),label = vdat.cost.2[i,1], 
                min = 0, max = max(vdat.cost.2[,2]) + max(vdat.cost.2[,4]), value = vdat.cost.2[i,2], step = 1)
    
  })
})


output$sliders.spread.cost <- renderUI({
  
  lapply(1:total.rows.cost(), function(i) {
    vdat.cost.2<-data.frame(vdat.cost(), digits=2)
    sliderInput(inputId = paste0("ind.s.cost", i),label = vdat.cost.2[i,1], 
                min = 0, max = max(vdat.cost.2[,2]) + max(vdat.cost.2[,4]), value = c(vdat.cost.2[i,3],vdat.cost.2[i,4]), step = 1)
    
  })
})


data.cent.cost<-reactive({
  data.cent.cost.1 <- vector(mode="numeric")
  for (i in 1:total.rows.cost()){
    mass.cost <- as.numeric(input[[paste0("ind.c.cost", i)]])[]
    data.cent.cost.1[i] <- mass.cost}
  
  return(data.cent.cost.1)  
})



data.spre.cost<-reactive({
  data.spre.cost.1 <- array(0, dim=c(total.rows.cost(),2))
  
  mass.s.cost.1 <- numeric()
  mass.s.cost.2 <- numeric()
  
  for (i in 1:total.rows.cost()){
    
    mass.s.cost.1[i] <- as.numeric(input[[paste0("ind.s.cost", i)]])[1]
    mass.s.cost.2[i] <- as.numeric(input[[paste0("ind.s.cost", i)]])[2]}
  data.spre.cost.1 <- data.frame(cbind(mass.s.cost.1, mass.s.cost.2))
  
  return(data.spre.cost.1)  
})


CostOrdered<-reactive({
CostOrdered.1<-data.frame(cbind(vdat.cost()[,1],data.cent.cost(),data.spre.cost()))  
CostOrdered.1<-CostOrdered.1[order(CostOrdered.1[,1]),]
return(CostOrdered.1)
})

output$PlotCosts <- renderPlot({
  vdat.cost.3<-data.frame(vdat.cost())
  num.vals.cost<-seq(1,total.rows.cost())
  par(mar = c(20, 4, 2, 2))
  plot(data.cent.cost(),pch=16, ylim=c(0,max(data.cent.cost()+data.spre.cost()[,2])), xaxt="n", xlim=c(1,total.rows.cost()), xlab="", ylab="Expected Cost")
  
  axis(1,at=seq(1,total.rows.cost()), label=vdat.cost.3[,1], las=2)
  
  arrows(x0=num.vals.cost, y0=data.spre.cost()[,1], x1=num.vals.cost, y1=data.spre.cost()[,2],
         code=0, angle=90, length=0.5, col="grey", lwd=4)
  
  points(data.cent.cost(),pch=16)
})

output$CostOrdered.0 <- renderTable({
  CostOrdered()
  
})



###########################################################################################################
########################################## Benefit-Cost Analysis ##################################################
###########################################################################################################


BC<-reactive({
  
  BC.1<-BenefitOrdered()[,2]/CostOrdered()[,2]  
  BC.1<-data.frame(BC.1)
  format.df(BC.1, digits=4)
  #BC.1<-formattable(BC.1,format="f",digits=4)
  BC.1<-cbind(BenefitOrdered()[,1],BC.1)
  
  
  colnames(BC.1)<-c("Management Option","Predicted Benefit/Unit cost (millions Drachma)")
  
  rownames(BC.1)<-BC.1[,1]
  
  return(BC.1)  
})


textlabels<-reactive({
  textlabels.1<-data.frame(cbind(data.cost()[,1],data.cent.model.BA2(),data.cent.cost()))
  colnames(textlabels.1)<-c("Management_Option","Benefit","Cost")
  
  return(textlabels.1)
})

output$PlotBC <- renderPlot({

  ggplot(CostOrdered(), aes(x=CostOrdered()[,2], y=BenefitOrdered()[,2])) +
    labs(y = "Estimated Benefit", x = "Estimated Cost (millions of Drachma)") +
    geom_point() + 
    geom_pointrange(aes(ymin=BenefitOrdered()[,3], ymax=BenefitOrdered()[,4]),color="grey", size=1.5) +
    geom_pointrange(aes(xmin=CostOrdered()[,3], xmax=CostOrdered()[,4]),color="grey", size=1.5) +
    theme(axis.text.x = element_text(size = 16))+
    theme(axis.text.y = element_text(size = 16))+
    theme(axis.title.y = element_text(size = 20))+
    theme(axis.title.x = element_text(size = 20))+
    geom_text(label=CostOrdered()[,1],nudge_x = 0.75, nudge_y = 1.5, check_overlap = T)
    
})

output$textlabelsBC <- renderTable({
  textlabels()
  
})

output$contents.benefit <- renderTable({
  cent.prop.final.benefit()
  
  })


BC.or<-reactive({
  BC.or.1<-BC()[order(BC()[,2]),]
  #format.df(BC.or.1[,2], digits=4)
  colnames(BC.or.1)<-c("Management Option","Benefit/Unit Cost")
return(BC.or.1)  
})

output$BC.ordered <- renderTable({
  BC.or()
})

output$BCFinal <- renderPlot({
  #par(mar = c(25, 4, 2, 2))
  
  #barplot(BC.or()[,2],
  #        names.arg=BC.or()[,1],las=2)

  
  ggplot(BC.or(), aes(BC.or()[,1], BC.or()[,2])) +
    geom_bar(stat = "identity") +
    geom_point()+
    labs(x="", y = "Estimated Benefit/Unit Cost") +
    theme(axis.text.x = element_text(size = 16, angle = 90))+
    theme(axis.text.y = element_text(size = 16))+
    theme(axis.title.y = element_text(size = 20))+   
    theme(legend.position="none")

  
  
})

}
shinyApp(ui, server)
