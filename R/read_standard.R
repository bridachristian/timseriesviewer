
read_standard = function(PATH, SEP,                           # read and import data well formatted
                     DATETIME_HEADER, DATETIME_FORMAT){
  
  header <- read.csv(PATH,sep =  SEP, nrows = 1,header = F,stringsAsFactors = F,na.strings = c(NA, "NaN", "NAN"))
  header_colnames <- header[1,]
  
  # ---- delme ----
  # first_row = header[DATA_FROM_ROW,]
  # header = header[-nrow(header),]
  # 
  # str(first_row)
  # toMatch <- c("-", "/")
  # matches <- unique(grep(paste(toMatch,collapse="|"),first_row, value=FALSE))
  
  # h_c = as.vector(t(header_colnames))
  # updateSelectInput(session, inputId = "timeselect", label = "Select columns to display", choices = h_c)
  
  # ---- read data ----
  data <- read.csv(path,sep = SEP, skip = 1,header = F,stringsAsFactors = F,na.strings = c(NA, "NaN", "NAN"))
  
  colnames(header) = header_colnames
  colnames(data) = header_colnames
  
  # datetime_selected = input$timeselect
  
  data[, which(header_colnames != datetime_selected)] <- sapply(data[, which(header_colnames != datetime_selected)], function(x) as.numeric(x)) # <- convert all to numeric
  w <- which(colnames(data) == datetime_selected)
  date_chr = data[,w]
  time <- as.POSIXct(strptime(x = date_chr, format = DATETIME_FORMAT), tz = 'Etc/GMT-1') # Error in strptime(x = as.character(date_chr), format = DATETIME_FORMAT):input string is too long
  data[,w] <- time
  not_w <- which(colnames(data) != datetime_selected)
  for(i in not_w){
    data[,i] <- as.numeric(data[,i])
  }
  
  output = data
  return(output)
}