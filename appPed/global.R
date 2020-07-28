source("funcs.R")

pedata <- load_pedata()
s <- pedata$data[[57]]
sy <- filter(s, year==2019)