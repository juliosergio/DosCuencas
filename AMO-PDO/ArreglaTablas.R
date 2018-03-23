#######################
# ArreglaTablas.R
#
#  arregla las tablas AMO y PDO
#######################
library(tidyverse)
library(lubridate)
source("../RR/MiBiblioteca.R", chdir = T)

t1 <- read.table("amon.us.data.txt", header = T)
t2 <- read.table("pdo.data.txt", header = T)

t1 <- t1 %>% gather(X.01.15:X.12.15, key = "month", value = "value") %>%
    arrange(Year, month)
# Recortamos elementos inexistentes
t1 <- t1[!is.na(t1$value),]
t1 <- t1 %>%
    mutate(type = "AMO", 
           Date = as.Date(gsub("X?\\.", "-", paste0(Year,month))),
           trail12 = mov.mean(value,12)
           ) %>%
    dplyr::select(Date, type, value, trail12) 

t2 <- t2 %>% gather(X.01.15:X.12.15, key = "month", value = "value") %>%
    arrange(Year, month) 
# Recortamos elementos inexistentes
t2 <- t2[!is.na(t1$value),]
t2 <- t2 %>%
    mutate(type = "PDO", 
           Date = as.Date(gsub("X?\\.", "-", paste0(Year,month))),
           trail12 = mov.mean(value,12)
    ) %>%
    dplyr::select(Date, type, value, trail12)

# Ahora hacemos un stack con las dos tablas:

tt <- rbind(t1,t2)

p <- ggplot(data = tt, mapping = aes(x=Date, y=value))
p + geom_line() + geom_line(aes(y=trail12),color="red",size=1) + 
    facet_grid(type ~ ., scales = "free_y") + geom_hline(yintercept = 0)

p + geom_line(aes(y=trail12),color="red",size=1) + facet_grid(type ~ ., scales = "free_y") +
    geom_hline(yintercept = 0)

