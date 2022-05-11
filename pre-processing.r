files=c("puerto_montt_160101_191231.csv",
"osorno_160101_191231.csv",
"temuco_160101_191231.csv",
"los_angeles_160101_191231.csv",
"chillan_160101_191231.csv",
"linares_160101_191231.csv",
"rancagua_160101_191231.csv",
"san_fernando_160101_191231.csv",
"talca_160101_191231.csv",
"curico_160101_191231.csv")

cities=c("puerto_montt",
        "osorno",
        "temuco",
        "los_angeles",
        "chillan",
        "linares",
        "rancagua",
        "san_fernando",
        "talca",
        "curico")

dir="raw/"

csv2df <- function(dir="raw/",city="curico",ext="_160101_191231.csv"){
  require(readr)
  require(dplyr)
  filename=paste(dir,city,ext,sep="")
  print(filename)
  dataset <- read_delim(filename, ";", escape_double = FALSE, trim_ws = TRUE)
  #mutate(city<-city)
  #new_colnames<-c("date","hour","validated","preliminary","not_validated","unknown")
  colnames(dataset)<-c("date","hour","validated","preliminary","not_validated","unknown")
  dataset%>%
    mutate(city=city)
}

dataset=data.frame()
for(c in cities) {
  if(dim(dataset)[1]==0)
    dataset<-csv2df(city=c)
  else
    dataset<-rbind(dataset,csv2df(city=c))
}
#library(readr)
#dataset <- read_delim("raw/chillan_160101_191231.csv", ";", escape_double = FALSE, trim_ws = TRUE)

sum(!is.na(dataset$validated))
sum(!is.na(dataset$preliminary))
sum(!is.na(dataset$not_validated))
sum(!is.na(dataset$unknown))

dataset<-select(dataset,-starts_with("unknown"))
dataset<-select(dataset,-starts_with("hour"))
dataset<-select(dataset,-starts_with("unknown"))

library(lubridate)
dataset$date[20]
date<-ymd(dataset$date)
dataset$date<-date

df<-dataset %>% select(date,validated,city)
df <- df[complete.cases(df),]
df=rename(df,PM25=validated)

class(df)
write.csv(df,"air-quality.csv", row.names = F)

df <- filter(df, city == "curico")
min(df$date)
max(df$date)
library(ggplot2)
df<-as.data.frame(df)
ggplot(df,                            # Draw ggplot2 time series plot
       aes(x = date,
           y = validated)
       ) +
  geom_line()

class(df)
class(data)

# Dummy data
data <- data.frame(
  day = as.Date("2017-06-14") - 0:364,
  value = runif(365) + seq(-140, 224)^2 / 10000
)

# Most basic bubble plot
p <- ggplot(data, aes(x=day, y=value)) +
  geom_line() + 
  xlab("")
p


