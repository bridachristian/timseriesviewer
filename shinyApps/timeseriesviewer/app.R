# rm(list = ls())
library(shiny)
library(shinyjs)
library(DT)
# library(DataQualityCheckEuracAlpEnv)
library(ggplot2)
library(plotly)
library(reshape2)
library(dplyr)


# ---- define a function to read data in the proper format ----
# it should be moved in a R package and imported from there!
read_TOA5 = function(PATH, SEP,                           # read and import data well formatted
                     DATETIME_HEADER, DATETIME_FORMAT,
                     DATA_FROM_ROW, HEADER_ROW_NUMBER){
  
  header <- read.csv(PATH,sep =  SEP, nrows = DATA_FROM_ROW-1,header = F,stringsAsFactors = F,na.strings = c(NA, "NaN", "NAN"))
  header_colnames <- header[HEADER_ROW_NUMBER,]
  
  # ---- read data ----
  data <- read.csv(PATH,sep = SEP, skip = DATA_FROM_ROW - 1,header = F,stringsAsFactors = F,na.strings = c(NA, "NaN", "NAN"))
  
  colnames(header) = header_colnames
  colnames(data) = header_colnames
  
  # ---- convert data to numeric ----
  data[, which(colnames(data) != DATETIME_HEADER)] <- sapply(data[, which(header_colnames != DATETIME_HEADER)], function(x) as.numeric(x)) # <- convert all to numeric
  
  # ---- convert date to POSIXct ----
  date_chr = data[,which(colnames(data) == DATETIME_HEADER)]
  time <- as.POSIXct(strptime(x = date_chr, format = DATETIME_FORMAT), tz = 'Etc/GMT-1')
  data[,which(colnames(data) == DATETIME_HEADER)] <- time
  
  # not_w <- which(colnames(data) != DATETIME_HEADER)
  # for(i in not_w){
  #   data[,i] <- as.numeric(data[,i])
  # }
  
  return(data)
}

# ---- UI -----
ui <- fluidPage(
  
  # Application title
  titlePanel("Eurac AlpEnv Time series viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Choose CSV/DAT File (< 50MB)",
                multiple = TRUE),
      helpText(em("Note: The file should have a TOA5 header, the datetime column name should be TIMESTAMP and the datetime format should be YYYY-mm-dd hh:mm")),
      
      # selectInput("timeselect", "Select the datetime column","", multiple = FALSE),
      selectInput("select", "Select column to display","", multiple = FALSE),
      helpText(em("Note: Select the variable you want to plot.")),
      actionButton(inputId = "prev_var",label = "Previous variable",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      actionButton(inputId = "next_var",label = "Next variable",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      textOutput("var_select"),
      DT::dataTableOutput("myheader")
      # actionButton("update", "Update Data set", class = "btn-primary",style='padding:4px; font-size:120%')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4(textOutput("time_selection")),
      plotlyOutput("tsPlot"),
      h4(tags$span(style="color:red",textOutput("note"))),
      #tableOutput("mytable")
      DT::dataTableOutput("mytable")
      
      
    )
  )
)

# ---- SERVER -----
server <- function(input, output,session) {
  options(shiny.maxRequestSize = 50*1024^2)   # Max 50 MB to import
  
  values <- reactiveValues(df_data = NULL)
  
  # ---- GENERAL SETTINGS ----
  SEP = ","
  DATETIME_FORMAT = "%Y-%m-%d %H:%M"
  DATA_FROM_ROW = 5
  HEADER_ROW_NUMBER = 2
  DATETIME_HEADER = "TIMESTAMP"
  
  # datetime_selected = DATETIME_HEADER
  
  observeEvent(input$dataset, {
    
    
    input_dataset = input$dataset
    path =  input$dataset$datapath
    name =  input$dataset$name
    
    header <- read.csv(file = path,sep =  SEP, nrows = DATA_FROM_ROW-1,header = F,stringsAsFactors = F,na.strings = c(NA, "NaN", "NAN"))
    header_colnames <- header[HEADER_ROW_NUMBER,]
    h_c = unname(unlist(header_colnames))
    
    # choices_admitted = h_c
    choices_admitted = h_c[-which(h_c == DATETIME_HEADER)]
    # updateSelectInput(session, inputId = "timeselect", label = "Select the datetime column", choices = choices_admitted,selected = choices_admitted[1])
    updateSelectInput(session, inputId = "select", label = "Select columns to display", choices = choices_admitted,selected = choices_admitted[1])
    
    
    data = read_TOA5(PATH = path, SEP = SEP,                           # read and import data well formatted
                     # DATETIME_HEADER = input$timeselect , DATETIME_FORMAT = DATETIME_FORMAT,
                     DATETIME_HEADER = DATETIME_HEADER , DATETIME_FORMAT = DATETIME_FORMAT,
                     DATA_FROM_ROW = DATA_FROM_ROW, HEADER_ROW_NUMBER = HEADER_ROW_NUMBER)
    
    colnames(header) = colnames(data)
    
    values$df_data <- data
    values$name <- name
    values$time <- data[,DATETIME_HEADER]
    values$header <- header
    values$choices_admitted <- choices_admitted
  })
  
  # ---- Next and previous button reactive ----
  
  observeEvent(input$next_var,{ 
    values$count <- which(values$choices_admitted == input$select)
    if(values$count < length(values$choices_admitted)){
    updateSelectInput(session, inputId = "select", label = "Select columns to display", choices = values$choices_admitted,selected = values$choices_admitted[values$count+1])
    }
  
  })
  
  observeEvent(input$prev_var,{
    values$count <- which(values$choices_admitted == input$select)
    if(values$count > 1){
    updateSelectInput(session, inputId = "select", label = "Select columns to display", choices = values$choices_admitted,selected = values$choices_admitted[values$count-1])
      
    }
  })
  
  
  # ---- Add text to highlight period of data plotted ----
  output$time_selection <- renderText({
    if(!is.null(input$dataset)){
      t_start = values$time[1]
      t_end = values$time [length(values$time )]
      paste("Data from ", t_start, " until ", t_end)
    }else{
      "Please, upload the data file!"
    }
    
  })
  
  # ---- Add text to highlight misunderstanding date format (GMT ) ----
  output$note <- renderText({
    if(!is.null(input$dataset)){
      "Note: TIMESTAMP format is GMT!"
    }
    # paste("",span(style="color:red", "Note: TIMESTAMP format is GMT!")
    # h4(tags$div(,sep = "")))
    
  })
  
  # ---- Export table of data selected ----
  output$mytable  <- renderDataTable({
    if(!is.null(input$dataset)){
      # df  = values$df_data[,c(datetime_selected,input$select)]
     
      df  = values$df_data[,c(DATETIME_HEADER,input$select)]
      DT::datatable(df)
      # %>%  formatDate(
      #   columns = DATETIME_HEADER, dateFormat = "%Y-%m-%d %H:%M",
      #   method =  "toLocaleString",params = list("en-EN",list(year = 'numeric', month = 'numeric', timeZone = "Etc/GMT-1")))
    }
  })
  
  # ---- Export table of information about time sampling and units ----
  output$myheader  <- renderDataTable({
    if(!is.null(input$dataset)){
      h_df  = values$header[3:4,input$select]
      # names(h_df) = c("Units", "Sampling Method")
      h_df = cbind(c("Units:", "Sampling Method:"),h_df)
      colnames(h_df) = c(input$select, " ")
      DT::datatable(h_df,options = list(searching = FALSE, ordering = FALSE,lengthChange = FALSE, info = FALSE,paging = FALSE))
      
    }
  })
  
  # ---- Export table of information about time sampling and units ----
  output$tsPlot <- renderPlotly({
    if(!is.null(input$dataset)){
     
      data_new = values$df_data[,c(DATETIME_HEADER,input$select)]
      
      t_start = min(values$time,na.rm = T)
      t_end = max(values$time,na.rm = T) 
      diff_1_2 = values$time[2]-values$time[1]
      # mytime = seq(from = t_start, to =t_end, by = diff_1_2 )

      all_days = data.frame(DATETIME_HEADER = seq(from = t_start, to = t_end, by = diff_1_2))
      colnames(all_days) = DATETIME_HEADER
      
      dd_complete = left_join(all_days, data_new, by = DATETIME_HEADER)
      colnames(dd_complete) = c("datetime","value")
      
      # m_data =  melt(data_new, id.vars = DATETIME_HEADER)
      # colnames(m_data)[1] = "datetime"
      
      # g1 = ggplot(m_data, aes(x = datetime, y = value ))+
      g1 = ggplot(dd_complete, aes(x = datetime, y = value ))+
        geom_line()+
        # geom_point()+
        scale_y_continuous(name = as.character(input$select))+
        # scale_x_datetime(date_breaks = "2 weeks")+
        scale_x_datetime(timezone = "Etc/GMT-1")+
        theme_bw()+
        ggtitle(label = values$name, subtitle = input$select)
      
      p1 = ggplotly(g1)
      
      p1
    }
    # plot(data$TIMESTAMP, data[,which(colnames(data) == input$yval)])
  })
  
}

shinyApp(ui = ui, server = server)
