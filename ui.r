library(shiny)
#install.packages("shinyforms")
library(shinyforms)
#install.packages("shinyjs")
library(shinyjs)
#install.packages("shinythemes")
library(shinythemes)
library(dplyr)
#install.packages("VIM")
library("VIM")
#install.packages("imputeTS")
library("imputeTS")
#install.packages("ExprFunction")
#rm(list=ls(all.names=TRUE))
rm(data)

ui <- fluidPage(
  titlePanel(title="DataCleaning"),
  #theme="darkly",themeSelector(),
  tabsetPanel(id="navbar",
              tabPanel("tab1",sidebarLayout(
                sidebarPanel(
                  fileInput("file1", "Choose CSV File",
                            multiple = FALSE,
                            accept = c(".csv"))
                  ,checkboxInput(inputId = "header",label = "Header")
                  ,selectInput("Dataprint","Select a choice",
                               choices=c("Full Data Set","Quantitative Data","Qualitative Data"),
                               selected = "Full Data Set")
                  ,actionButton("OKay","Okay")),
                mainPanel(tableOutput("table1")))),
              tabPanel("tabage",sidebarLayout(
                sidebarPanel(
                  uiOutput("ui"),
                  selectInput("impu","Select the imputation method to fill Columns"
                              ,choices = c("HOT DECK IMPUTATION" = "hdi",
                                           "BASIC NUMERIC IMPUTATION" = "bni",
                                           "K-IMPUTATION"= "kimpu",
                                           "INTERPOLATION" = "intu"))
                  ,
                  conditionalPanel(
                    condition = "input.impu == 'bni'",
                    selectInput(
                      "mmm", "Choose one ",
                      c("Mean",
                        "Median",
                        "Mode")))
                  
                  , actionButton("Impute","Impute")     
                ),mainPanel(tableOutput("table2")))),
              tabPanel("Outlier",sidebarLayout(
                sidebarPanel(
                  uiOutput("uiout"),
                  radioButtons("outlier",label = h1("Outliers"),choices = c( "Dont Remove Outliers"="r1","Remove Outliers"="r2"),selected = NULL)),
                mainPanel(
                  verbatimTextOutput("done")))),
              
              tabPanel("Graph",sidebarLayout(
                sidebarPanel(
                  selectInput("graph","Data Visulization",
                              choices=c("Scatter Plot 2D"="s2",
                                        "3d scatterplot"="s3",
                                        "Bubble Chart" = "b"
                              )),
                  conditionalPanel(
                    condition="input.graph == 's2'",
                    uiOutput("ui1.1"),
                    uiOutput("ui1.2")
                  ),
                  
                  conditionalPanel(  
                    condition="input.graph == 's3'",
                    uiOutput("ui2.1"),
                    uiOutput("ui2.2"),
                    uiOutput("ui2.3")
                  ),
                  
                  conditionalPanel(  
                    condition="input.graph == 'b'",
                    uiOutput("ui3.1"),
                    uiOutput("ui3.2"))
                )
                ,install.packages("devtools")
                mainPanel(
                  plotOutput("plot1")
                )
              )
              )
  ))


