library(ggpubr)
library(ggplot2)
library(dplyr)
library("cowplot")
library("arulesViz")

# Se carga la cabecera de la base de datos.
head <- c("animal name","hair",
          "feathers","eggs","milk","airborne","aquatic","predator",
          "toothed","backbone","breathes","venomous","fins","legs",
          "tail","domestic","catsize","type")

# Se cargan los datos
data <- read.table("zoo.data",header = FALSE, sep = ",")
data$type <- factor(data$type)


# Se agrega cabecera a la base de datos
colnames(data) <- head


#############################################

data$type <- factor(data$type)
num.wide.data <- data[,-1] # se usa cuando se necesiten los valores numericos
data.wide <- data[,-1]
# Para hacer un resumen de las variables adecuado se transforman las columnas con valores 1 y 0 a booleanos
data.wide[["hair"]] <- as.logical(data.wide[["hair"]])
data.wide[["feathers"]] <- as.logical(data.wide[["feathers"]])
data.wide[["eggs"]] <- as.logical(data.wide[["eggs"]])
data.wide[["milk"]] <- as.logical(data.wide[["milk"]])
data.wide[["airborne"]] <- as.logical(data.wide[["airborne"]])
data.wide[["aquatic"]] <- as.logical(data.wide[["aquatic"]])
data.wide[["predator"]] <- as.logical(data.wide[["predator"]])
data.wide[["toothed"]] <- as.logical(data.wide[["toothed"]])
data.wide[["backbone"]] <- as.logical(data.wide[["backbone"]])
data.wide[["breathes"]] <- as.logical(data.wide[["breathes"]])
data.wide[["venomous"]] <- as.logical(data.wide[["venomous"]])
data.wide[["fins"]] <- as.logical(data.wide[["fins"]])
data.wide[["tail"]] <- as.logical(data.wide[["tail"]])
data.wide[["domestic"]] <- as.logical(data.wide[["domestic"]])
data.wide[["catsize"]] <- as.logical(data.wide[["catsize"]])

summary(data.wide)

# Graficos de barras para el analisis 

# hair

dfh <- data.wide %>%
  group_by(type,hair) %>%
  summarise(counts = n()) 

hh <- ggplot(data = dfh, mapping = aes(x = type, y= counts,  fill = hair)) + geom_bar(stat="identity",position = "dodge")

# feathers

dff <- data.wide %>%
  group_by(type,feathers) %>%
  summarise(counts = n()) 

hf <- ggplot(data = dff, mapping = aes(x = type, y= counts,  fill = feathers)) + geom_bar(stat="identity",position = "dodge")

# eggs

dfe <- data.wide %>%
  group_by(type,eggs) %>%
  summarise(counts = n()) 

he <- ggplot(data = dfe, mapping = aes(x = type, y= counts,  fill = eggs)) + geom_bar(stat="identity",position = "dodge")


# milk

dfm <- data.wide %>%
  group_by(type,milk) %>%
  summarise(counts = n()) 

hm <- ggplot(data = dfm, mapping = aes(x = type, y= counts,  fill = milk)) + geom_bar(stat="identity",position = "dodge")


# airborne

dfa <- data.wide %>%
  group_by(type,airborne) %>%
  summarise(counts = n()) 

ha <- ggplot(data = dfa, mapping = aes(x = type, y= counts,  fill = airborne)) + geom_bar(stat="identity",position = "dodge")


# aquatic

dfaq <- data.wide %>%
  group_by(type,aquatic) %>%
  summarise(counts = n()) 

haq <- ggplot(data = dfaq, mapping = aes(x = type, y= counts,  fill = aquatic)) + geom_bar(stat="identity",position = "dodge")


# predator

dfp <- data.wide %>%
  group_by(type,predator) %>%
  summarise(counts = n()) 

hp <- ggplot(data = dfp, mapping = aes(x = type, y= counts,  fill = predator)) + geom_bar(stat="identity",position = "dodge")


# toothed

dft <- data.wide %>%
  group_by(type,toothed) %>%
  summarise(counts = n()) 

ht <- ggplot(data = dft, mapping = aes(x = type, y= counts,  fill = toothed)) + geom_bar(stat="identity",position = "dodge")


# backbone

dfb <- data.wide %>%
  group_by(type,backbone) %>%
  summarise(counts = n()) 

hb <- ggplot(data = dfb, mapping = aes(x = type, y= counts,  fill = backbone)) + geom_bar(stat="identity",position = "dodge")


# breathes

dfbr <- data.wide %>%
  group_by(type,breathes) %>%
  summarise(counts = n()) 

hbr <- ggplot(data = dfbr, mapping = aes(x = type, y= counts,  fill = breathes)) + geom_bar(stat="identity",position = "dodge")


# venomous

dfv <- data.wide %>%
  group_by(type,venomous) %>%
  summarise(counts = n()) 

hv <- ggplot(data = dfv, mapping = aes(x = type, y= counts,  fill = venomous)) + geom_bar(stat="identity",position = "dodge")


# fins

dffi <- data.wide %>%
  group_by(type,fins) %>%
  summarise(counts = n()) 

hfi <- ggplot(data = dffi, mapping = aes(x = type, y= counts,  fill = fins)) + geom_bar(stat="identity",position = "dodge")


# legs

dfl <- data.wide %>%
  group_by(type,legs) %>%
  summarise(counts = n()) 

hl <- ggplot(data = dfl, mapping = aes(x = type, y= counts,  fill = legs)) + geom_bar(stat="identity",position = "dodge")


# tail

dfta <- data.wide %>%
  group_by(type,tail) %>%
  summarise(counts = n()) 

hta <- ggplot(data = dfta, mapping = aes(x = type, y= counts,  fill = tail)) + geom_bar(stat="identity",position = "dodge")


# domestic

dfd <- data.wide %>%
  group_by(type,domestic) %>%
  summarise(counts = n()) 

hd <- ggplot(data = dfd, mapping = aes(x = type, y= counts,  fill = domestic)) + geom_bar(stat="identity",position = "dodge")


# catsize

dfc <- data.wide %>%
  group_by(type,catsize) %>%
  summarise(counts = n()) 

hc <- ggplot(data = dfc, mapping = aes(x = type, y= counts,  fill = catsize)) + geom_bar(stat="identity",position = "dodge")

