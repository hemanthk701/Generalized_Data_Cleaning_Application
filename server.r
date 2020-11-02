server <- function(input, output,session){
  
  #shinyjs::onclick("my_img",  updateTabsetPanel(session, inputId="navbar", selected="tabage"))
  #a <- input$file1
  #n <-c(names(input$file1))
  num1 <- reactive({
    input$file1
    if (is.null(input$file1)) return(NULL)
    data <- read.csv(input$file1$datapath,header = input$header,sep = ",",stringsAsFactors = FALSE)
    data <- replace(data,data == "",values = NA)
    #data <- data[, -which(colSums(is.na(data)) > 0.5)]
    num = 0
    char = 0
    for (i in 1:length(names(data))) 
    {
      a <- c(data[,i])
      for (j in 1:length(a))
      {if (is.na(as.numeric(a[j])) == "TRUE")
      { #print(a[j])
        char = char+1}
        else
        { num = num + 1} 
        #print(num)
        #print(char)
        if (num >= char) 
        {
          data[,i]<- as.numeric(a)
          num=0
        }
        else
        {data[,i] <- as.character(a)
        char = 0}
      } }
    isolate( write.csv(data,file="E:/Shiny/data1.csv",row.names = FALSE))
    return(data)
  })
  #return(data)
  #if (input$Dataprint == "Full Data Set")
  #{return(data)}
  #else if (input$Dataprint == "Quantitative Data")
  #{b=select_if(data,is.numeric)
  #return(b)}
  #else if (input$Dataprint == "Qualitative Data" )
  #{c=select_if(data,is.character)
  #return(c)
  #}
  #observeEvent(input$Ok,{
  #       data <- num1()})
  #write.csv(num1(),file="E:/Shiny/data.csv")
  
  output$table1 <- renderTable({
    #data <- read.csv(input$file1$datapath,header = input$header,sep = ",")
    #num1()
    #data <- read.csv(file ="E:/Shiny/data.csv",header = input$header,stringsAsFactors = FALSE)
    if (input$Dataprint == "Full Data Set")
    {num1()}
    else if (input$Dataprint == "Quantitative Data")
    {b=select_if(num1(),is.numeric)
    b}
    else if (input$Dataprint == "Qualitative Data" )
    {c=select_if(num1(),is.character)
    c
    }
  })
  
  observeEvent(input$OKay, {
    updateTabsetPanel(session = session, inputId = "navbar", selected = "tabage")
  })
  output$ui <- renderUI( { 
    data1 <- read.csv(file="E:/Shiny/data1.csv",header = input$header,stringsAsFactors = FALSE)
    t <- c(names(select_if(data1,is.numeric)))
    selectInput("Col","Select a Column ",choices = t)
  } )
  hotdeck <- reactive({
    q <-  read.csv(file="E:/Shiny/data1.csv",header = input$header,stringsAsFactors = FALSE)
    m <- q[,input$Col]
    final_result <- na_random(m) 
    q[,input$Col] <- final_result
    return(q)
  })
  kn <- reactive({
    data <- read.csv(file="E:/Shiny/data1.csv",header = input$header,stringsAsFactors = FALSE)
    q <- select_if(num1(),is.numeric)
    w <- input$Col
    kimp=kNN(data, numFun = weighted.mean,weightDist = TRUE, imp_var = FALSE)
    #kimp = kimp[-c(length(names(data))+1)]
    return(kimp)
    
  })
  interpol <- reactive({
    q <-  read.csv(file="E:/Shiny/data1.csv",header = input$header,stringsAsFactors = FALSE)
    linear1=na_interpolation(q[,input$Col])
    q[,input$Col] <- linear1
    return(q)
  })
  num <- reactive({
    q <-  read.csv(file="E:/Shiny/data1.csv",header = input$header,stringsAsFactors = FALSE)
    if (input$mmm == "Mean")
    { n = na_mean(q[,input$Col], option= "mean")
    q[,input$Col] <- n
    return(q)
    }
    else if (input$mmm == "Median")
    {
      n = na_mean(q[,input$Col], option= "median")
      q[,input$Col] <- n
      return(q)
    }
    else if (input$mmm == "Mode")
    {
      n <- na_mean(q[,input$Col], option= "mode")
      q[,input$Col] <- n
      return(q)
    }
  })
  #"BASIC NUMERIC IMPUTATION","K-IMPUTATION","INTERPOLATION
  Dataset1 <- reactive({
    if(input$Impute == 0) return()
    if (input$impu == "hdi")
    { k <-hotdeck()
    (write.csv(k,file="E:/Shiny/data1.csv",row.names = FALSE))
    return(k)
    }
    else if (input$impu =="bni")
    {k<- num()
    (write.csv(k,file="E:/Shiny/data1.csv",row.names = FALSE))
    return(k)
    }
    else if (input$impu == "kimpu")
    { k <- kn()
    (write.csv(k,file="E:/Shiny/data1.csv",row.names = FALSE))
    return(k)
    }
    else if( input$impu == "intu")  
    {k<- interpol()
    (write.csv(k,file="E:/Shiny/data1.csv",row.names = FALSE))
    return(k)
    }
    (write.csv(k,file="E:/Shiny/data1.csv",row.names = FALSE))
    return(k)
  })
  output$table2 <-  renderTable({
    Dataset1()
  })
  Dataset2 <- observe({
    if(input$Impute == 0 ) return()
    e <- Dataset1()
    write.csv(e,file="E:/Shiny/data1.csv",row.names = FALSE)
  })
  output$ui1.1 <- renderUI({
    g <- (read.csv(file ="E:/Shiny/data1.csv",header = TRUE,stringsAsFactors = FALSE))
    selectInput("s2x","x-axis",choices = c(names(select_if(g,is.numeric))))
  } )
  output$ui1.2 <- renderUI({
    g <- (read.csv(file ="E:/Shiny/data1.csv",header = TRUE,stringsAsFactors = FALSE))
    selectInput("s2y","y-axis",choices = c(names(select_if(g,is.numeric))))
  } )
  output$ui2.1 <- renderUI({
    g <- read.csv(file ="E:/Shiny/data1.csv",header = TRUE,stringsAsFactors = FALSE)
    selectInput("s3x","x-axis",choices = c(names(select_if(g,is.numeric))))
  })
  output$ui2.2 <- renderUI({
    g <- read.csv(file ="E:/Shiny/data1.csv",header = TRUE,stringsAsFactors = FALSE)
    selectInput("s3y","y-axis",choices = c(names(select_if(g,is.numeric))))
  })
  output$ui2.3 <- renderUI({
    g <- read.csv(file ="E:/Shiny/data1.csv",header = TRUE,stringsAsFactors = FALSE)
    selectInput("s3z","z-axis",choices = c(names(select_if(g,is.numeric))))
  })
  output$ui3.1 <- renderUI({
    g <- read.csv(file ="E:/Shiny/data1.csv",header = TRUE,stringsAsFactors = FALSE)
    selectInput("bx","x-axis",choices = c(names(select_if(g,is.character))))
  })
  output$ui3.2 <- renderUI({
    g <- read.csv(file ="E:/Shiny/data1.csv",header = TRUE,stringsAsFactors = FALSE)
    selectInput("by","y-axis",choices = c(names(select_if(g,is.character))))
  })
  output$plot1 <- renderPlot({
    g <- read.csv(file ="E:/Shiny/data1.csv",header = TRUE,stringsAsFactors = FALSE)
    if (input$graph == "s2" )
    {
      plot(x=g[,input$s2x],y=g[,input$s2y])
    }
    else if (input$graph == "s3")
    {
      plot3d( 
        x=g[,input$s3x], y=g[,input$s3y], z=g[,input$s3z])
      
    }
    else if (input$graph == "b")
    {
      ggplot(g, aes(x = "input$bx", y = "input$by")) 
      #+   geom_point(aes(color = cyl, size = qsec), alpha = 0.5) +
      # scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
      #scale_size(range = c(0.5, 12))
    }
  }) 
  output$uiout <- renderUI( { 
    data1 <- read.csv(file="E:/Shiny/data1.csv",header = input$header,stringsAsFactors = FALSE)
    t <- c(names(select_if(data1,is.numeric)))
    checkboxGroupInput("out","Select Columns to remove its outlier",choices = t)
  } )
  box <- reactive({
    m <-  read.csv(file="E:/Shiny/data1.csv",header = input$header,stringsAsFactors = FALSE)
    outlier <- boxplot(m[,input$out])
    m <- m[-which(m[,input$out] %in% outlier )]
    return(m)
  })
  output$done <- renderText({
    if (input$outlier == "r1"){"Outlier Not removed"}
    else if(input$outlier == "r2"){
      box()
      "Outlier Removed"}
  })
}
