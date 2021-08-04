library(ggpubr)
library(ggplot2)
library(dplyr)
library(FSelector)
library(NoiseFiltersR)
library("cowplot")
library("arulesViz")

# Se carga la cabecera de la base de datos.
head <- c("animal name","hair",
          "feathers","eggs","milk","airborne","aquatic","predator",
          "toothed","backbone","breathes","venomous","fins","legs",
          "tail","domestic","catsize","type")

# Se cargan los datos
data <- read.table("zoo.data",header = FALSE, sep = ",")

# Se agrega cabecera a la base de datos
colnames(data) <- head

# se generan valores de info_gain y ratio_gain
atr.info <- information.gain(type~., data)
atr.ratio <- gain.ratio(type~., data)

# creación de data frame que contiene los valores de info gain y ratio gain
peso.atr <- data.frame(
  head[head!="type"],
  atr.info$attr_importance,
  atr.ratio$attr_importance
)

colnames(peso.atr) <- c("atributo", "info_gain", "ratio_gain")

#Eliminacion de atributos
data <- subset( data, select = -c(aquatic, venomous, predator, domestic, catsize) )
data <- data[-26, ]

tabla <- table(data$type)
prob <- prop.table(tabla)

#############################################

num.wide.data <- data[,-1] # se usa cuando se necesiten los valores numericos
data$type <- factor(data$type)
data.wide <- data[,-1]

# Para hacer un resumen de las variables adecuado se transforman las columnas con valores 1 y 0 a booleanos

data.wide[["hair"]] <- as.logical(data.wide[["hair"]])
data.wide[["feathers"]] <- as.logical(data.wide[["feathers"]])
data.wide[["eggs"]] <- as.logical(data.wide[["eggs"]])
data.wide[["milk"]] <- as.logical(data.wide[["milk"]])
data.wide[["airborne"]] <- as.logical(data.wide[["airborne"]])
data.wide[["toothed"]] <- as.logical(data.wide[["toothed"]])
data.wide[["backbone"]] <- as.logical(data.wide[["backbone"]])
data.wide[["breathes"]] <- as.logical(data.wide[["breathes"]])
data.wide[["fins"]] <- as.logical(data.wide[["fins"]])
data.wide[["tail"]] <- as.logical(data.wide[["tail"]])
data.wide[["legs"]] <- factor(data.wide[["legs"]])

summary(data.wide)

# # Graficos de barras para el analisis 
# 
# # hair
# 
# dfh <- data.wide %>%
#   group_by(type,hair) %>%
#   summarise(counts = n()) 
# 
# hh <- ggplot(data = dfh, mapping = aes(x = type, y= counts,  fill = hair)) + geom_bar(stat="identity",position = "dodge")
# 
# 
# # feathers
# 
# dff <- data.wide %>%
#   group_by(type,feathers) %>%
#   summarise(counts = n()) 
# 
# hf <- ggplot(data = dff, mapping = aes(x = type, y= counts,  fill = feathers)) + geom_bar(stat="identity",position = "dodge")
# 
# 
# # eggs
# 
# dfe <- data.wide %>%
#   group_by(type,eggs) %>%
#   summarise(counts = n()) 
# 
# he <- ggplot(data = dfe, mapping = aes(x = type, y= counts,  fill = eggs)) + geom_bar(stat="identity",position = "dodge")
# 
# 
# # milk
# 
# dfm <- data.wide %>%
#   group_by(type,milk) %>%
#   summarise(counts = n()) 
# 
# hm <- ggplot(data = dfm, mapping = aes(x = type, y= counts,  fill = milk)) + geom_bar(stat="identity",position = "dodge")
# 
# 
# # airborne
# 
# dfa <- data.wide %>%
#   group_by(type,airborne) %>%
#   summarise(counts = n()) 
# 
# ha <- ggplot(data = dfa, mapping = aes(x = type, y= counts,  fill = airborne)) + geom_bar(stat="identity",position = "dodge")
# 
# 
# # toothed
# 
# dft <- data.wide %>%
#   group_by(type,toothed) %>%
#   summarise(counts = n()) 
# 
# ht <- ggplot(data = dft, mapping = aes(x = type, y= counts,  fill = toothed)) + geom_bar(stat="identity",position = "dodge")
# 
# 
# # backbone
# 
# dfb <- data.wide %>%
#   group_by(type,backbone) %>%
#   summarise(counts = n()) 
# 
# hb <- ggplot(data = dfb, mapping = aes(x = type, y= counts,  fill = backbone)) + geom_bar(stat="identity",position = "dodge")
# 
# 
# # breathes
# 
# dfbr <- data.wide %>%
#   group_by(type,breathes) %>%
#   summarise(counts = n()) 
# 
# hbr <- ggplot(data = dfbr, mapping = aes(x = type, y= counts,  fill = breathes)) + geom_bar(stat="identity",position = "dodge")
# 
# 
# # fins
# 
# dffi <- data.wide %>%
#   group_by(type,fins) %>%
#   summarise(counts = n()) 
# 
# hfi <- ggplot(data = dffi, mapping = aes(x = type, y= counts,  fill = fins)) + geom_bar(stat="identity",position = "dodge")
# 
# 
# # legs
# 
# dfl <- data.wide %>%
#   group_by(type,legs) %>%
#   summarise(counts = n()) 
# 
# hl <- ggplot(data = dfl, mapping = aes(x = type, y= counts,  fill = legs)) + geom_bar(stat="identity",position = "dodge")
# 
# 
# # tail
# 
# dfta <- data.wide %>%
#   group_by(type,tail) %>%
#   summarise(counts = n()) 
# 
# hta <- ggplot(data = dfta, mapping = aes(x = type, y= counts,  fill = tail)) + geom_bar(stat="identity",position = "dodge")

# se agrupan las clases 3 5 y 7, en la clase 3
new.data.wide <- data.wide
new.data.wide <- mutate(new.data.wide, type = case_when(type == 1 ~ "mamifero",
                                     type == 2 ~ "ave",
                                     type == 3 ~ "otro",
                                     type == 4 ~ "pez",
                                     type == 5 ~ "otro",
                                     type == 6 ~ "insecto",
                                     type == 7 ~ "otro"))
new.data.wide$type <- factor(new.data.wide$type)



new.reglas <- apriori(
  data = new.data.wide, 
  parameter=list(support = 0.03, minlen = 1, maxlen = 11, target="rules"),
  appearance=list(rhs = c("type=mamifero", "type=ave", "type=pez", "type=otro", "type=insecto"))
)

new.inspeccion <- inspect(sort(x = new.reglas, decreasing = TRUE, by = "confidence"))

# # Creación de reglas
# reglas <- apriori(
#   data = data.wide, 
#   parameter=list(support = 0.03, minlen = 1, maxlen = 11, target="rules"),
#   appearance=list(rhs = c("type=1", "type=2", "type=3","type=4","type=5","type=6","type=7"))
# )
# 
# inspeccion <- inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))
