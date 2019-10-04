
# PATH = path 
# SEP =","
# DATETIME_HEADER = input$timeselect
# DATETIME_FORMAT = "%Y-%m-%d %H:%M"
# DATA_FROM_ROW = 5
# HEADER_ROW_NUMBER = 2


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