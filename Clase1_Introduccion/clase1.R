# Clase 1. Comandos basicos

# Comandos basicos

2 + 2
2+2

2 - 2
2 * 2
2 ^ 2
2 / 2

log(2)
log10(2)
log10(2)

exp(2)

mean(2)
sd(2)

# Cargar base de datos

Pajaros <- read.csv("Clase2_Correlacion/PinzonesAfricanos.csv")

summary(Pajaros)
str(Pajaros)

table(Pajaros$Especie)
table(Pajaros[ ,2 ])
table(Pajaros$Peso)

View(Pajaros)
head(Pajaros)
tail(Pajaros)

# Crear nueva columna

IMC <- Pajaros$Peso / Pajaros$LargoDePico
IMC
summary(IMC)

Pajaros$IMC <- Pajaros$LargoDePico / Pajaros$Peso
head(Pajaros)

# guardar tabla

write.csv(file = "Clase1_Introduccion/Pajaros.csv", Pajaros)

# Instalar paquetes

# install.packages("tidyverse")
library(tidyverse)

ggplot(data = Pajaros, aes(x= Peso, y= IMC)   ) +
  geom_point(color = "red", size= 2.3, shape = 15, alpha = 0.4) +
  labs(title = "Peso de Pinzones", 
       y = "Indice masa corporal inventado",
       x= "Peso (gr)",
       caption = "Datos provenientes") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face= 1, size = 16, family = "sans") ) +
  theme(axis.title.x = element_text(hjust = 0.5, face= 4) )

ggsave("Clase1_Introduccion/pinzones.png", plot = last_plot(), 
       units = "cm", height = 10, width = 13, dpi = 450)

dev.off()


