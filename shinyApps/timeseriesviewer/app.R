library(shiny)
library(DT)
# library(DataQualityCheckEuracAlpEnv)
library(ggplot2)
library(plotly)
library(reshape2)


# ---- UI -----
ui <- fluidPage(
  
  # Application title
  titlePanel("Eurac AlpEnv Time series viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Choose CSV/DAT File (< 50MB)",
                multiple = TRUE),
      radioButtons("table_format", label = h3("Radio buttons"),choices = list("TOA5 table" = 1, "Standard table" = 2)),
      helpText(em("Note: The file should have a TOA5 header, the datetime column name should be TIMESTAMP and the datetime format should be YYYY-mm-dd hh:mm")),
      
      selectInput("timeselect", "Select the datetime column","", multiple = FALSE),
      selectInput("select", "Select column to display","", multiple = FALSE),
      helpText(em("Note: Select the variable you want to plot.")),
      actionButton(inputId = "next_var",label = "Next variable"),
      h4(textOutput("var_select")),
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
  
  # ---- Read file and update "select" ----
  
  # datetime_selected = DATETIME_HEADER
  SEP = ","
  DATETIME_FORMAT = "%Y-%m-%d %H:%M"
  DATA_FROM_ROW = 5
  HEADER_ROW_NUMBER = 2
  
  observeEvent(input$dataset, {
    input_dataset = input$dataset
    path =  input$dataset$datapath
    name =  input$dataset$name
    # datetime_selected = DATETIME_HEADER
    
    # path = "C:/Users/CBrida/Desktop/GitLab/timseriesviewer/data/M1old_2017.csv"
    # path = "C:/Users/CBrida/Desktop/GitLab/timseriesviewer/data/LTSER_IT25_Matsch_Mazia_1570174112.csv"
    # input$table_format = 2
    
    
    
    
    header <- read.csv(path,sep =  SEP, nrows = DATA_FROM_ROW,header = F,stringsAsFactors = F,na.strings = c(NA, "NaN", "NAN"))
    header_colnames <- header[HEADER_ROW_NUMBER,]
    h_c = unname(unlist(header_colnames))
    
    choices_admitted = h_c
    updateSelectInput(session, inputId = "timeselect", label = "Select the datetime column", choices = choices_admitted,selected = choices_admitted[1])
    updateSelectInput(session, inputId = "select", label = "Select columns to display", choices = choices_admitted,selected = choices_admitted[2])
    
    # AGGIUNGERE CONTROLLO SU input$timeselect --> formato deve essere corretto! 
    
    # time_selected = input$timeselect
    
    
    source("C:/Users/CBrida/Desktop/GitLab/timseriesviewer/R/read_TOA5.R")
    source("C:/Users/CBrida/Desktop/GitLab/timseriesviewer/R/read_standard.R")
    
    # datetime_selected = input$timeselect
    data = read_TOA5(path, SEP,                           # read and import data well formatted
                     DATETIME_HEADER = input$timeselect , DATETIME_FORMAT,
                     DATA_FROM_ROW, HEADER_ROW_NUMBER)
    values$df_data <- data
    values$name <- name
    values$time <- values$df_data[,datetime_selected]
    values$header <- header
    
  })
  
  # observeEvent(input$dataset, {
  #   # ---- Settings -----
  #   
  #   # --------------------
  #   
  #   # observeEvent(input$select, {
  #   # req(input$timeselect)
  #   input_dataset = input$dataset
  #   path =  input$dataset$datapath
  #   name =  input$dataset$name
  #   # datetime_selected = DATETIME_HEADER
  #   
  #   path = "C:/Users/CBrida/Desktop/GitLab/timseriesviewer/data/M1old_2017.csv"
  #   source("C:/Users/CBrida/Desktop/GitLab/timseriesviewer/R/read_TOA5.R")
  #   
  #   header <- read.csv(PATH,sep =  SEP, nrows = DATA_FROM_ROW,header = F,stringsAsFactors = F,na.strings = c(NA, "NaN", "NAN"))
  #   header_colnames <- header[HEADER_ROW_NUMBER,]
  #   
  #   data_list = read_TOA5 = function(PATH = path, SEP,                      
  #                                    DATETIME_HEADER, DATETIME_FORMAT, DATETIME_SAMPLING,
  #                                    DATA_FROM_ROW, HEADER_ROW_NUMBER)
  # 
  #   values$df_data <- data
  #   values$name <- name
  #   values$time <- values$df_data[,datetime_selected]
  #   values$header <- header
  #   
  #   choices_admitted = colnames(data)[-which(colnames(data) == datetime_selected)]
  #   updateSelectInput(session, inputId = "select", label = "Select columns to display", choices = choices_admitted)
  #   
  # })
  
  # ---- Add text to highlight period of data plotted ----
  # output$time_selection <- renderText({
  #   if(!is.null(input$dataset)){
  #     t_start = values$time[1]
  #     t_end = values$time [length(values$time )]
  #     paste("Data from ", t_start, " until ", t_end)
  #   }else{
  #     "Please, upload the data file!"
  #   }
  #   
  # })
  
  # ---- Add text to highlight misunderstanding date format (GMT ) ----
  # output$note <- renderText({
  #   if(!is.null(input$dataset)){
  #     "Note: TIMESTAMP format is GMT!"
  #   }
  #   # paste("",span(style="color:red", "Note: TIMESTAMP format is GMT!")
  #   # h4(tags$div(,sep = "")))
  #   
  # })
  
  # ---- Export table of data selected ----
  # output$mytable  <- renderDataTable({
  #   if(!is.null(input$dataset)){
  #     # df  = values$df_data[,c(datetime_selected,input$select)]
  #     df  = values$df_data[,c(input$timeselect,input$select)]
  #     DT::datatable(df)
  #   }
  # })
  
  # ---- Export table of information about time sampling and units ----
  # output$myheader  <- renderDataTable({
  #   if(!is.null(input$dataset)){
  #     if(input$table_format == 1){
  #     h_df  = values$header[3:4,input$select]
  #     # names(h_df) = c("Units", "Sampling Method")
  #     h_df = cbind(c("Units:", "Sampling Method:"),h_df)
  #     colnames(h_df) = c(input$select, " ")
  #     DT::datatable(h_df,options = list(searching = FALSE, ordering = FALSE,lengthChange = FALSE, info = FALSE,paging = FALSE))
  #     }
  #   }
  # })
  
  # ---- Export table of information about time sampling and units ----
  # output$tsPlot <- renderPlotly({
  #   if(!is.null(input$dataset)){
  #     
  #     t_start = values$time[1]
  #     t_end = values$time [length(values$time )]
  #     
  #     # data_new = values$df_data[,c(datetime_selected,input$select)]
  #     data_new = values$df_data[,c(input$timeselect,input$select)]
  #     
  #     
  #     # m_data =  melt(data_new, id.vars = "TIMESTAMP")
  #     m_data =  melt(data_new, id.vars = input$timeselect)
  #     colnames(m_data)[1] = "datetime"
  #     g1 = ggplot(m_data, aes(x = datetime, y = value ))+
  #       geom_line()+
  #       scale_y_continuous(name = as.character(input$select))+
  #       # scale_x_datetime(date_breaks = "2 weeks")+
  #       scale_x_datetime(timezone = "Etc/GMT-1")+
  #       theme_bw()+
  #       ggtitle(label = values$name)
  #     
  #     p1 = ggplotly(g1)
  #     
  #     p1
  #   }
  #   # plot(data$TIMESTAMP, data[,which(colnames(data) == input$yval)])
  # })
  
}

shinyApp(ui = ui, server = server)
