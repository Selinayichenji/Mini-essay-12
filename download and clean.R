
library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(googlesheets4)
library(readr)

x <- read_sheet('https://docs.google.com/spreadsheets/d/1tGFHHICtguU5_S_QuCBpW0awQIGF-X7gY30j0URRlok/edit#gid=864982580')

#Cleaning
x$Date_of_Birth <- as.Date(x$`Date of Birth`, format = "%Y-%m-%d")
x$Date_of_Death <- as.Date(x$`Date of Death`, format = "%Y-%m-%d")

x <- x[!is.na(x$Date_of_Birth) & !is.na(x$Date_of_Death),]

x$Age_at_Death <- as.integer(format(x$Date_of_Death, "%Y")) - as.integer(format(x$Date_of_Birth, "%Y"))

x <- x %>% filter(Age_at_Death <= 100)

x$Birthplace <- as.character(x$Birthplace)
x$Residence <- as.character(x$Residence)
x <- x %>%
  filter(!grepl("\\.\\.\\.|\\?| '|---", Birthplace))
x <- x %>%
  filter(!grepl("\\.\\.\\.|\\?| '|---", Residence))

x <- x %>%
  mutate(YearMonth = format(Date_of_Death, "%Y-%m"))
x <- x %>%
  mutate(YearMonth = as.Date(paste0(YearMonth, "-01")))

x$Age_at_Death <- as.numeric(x$Age_at_Death)

write.csv(x, "death_certificate.csv")
