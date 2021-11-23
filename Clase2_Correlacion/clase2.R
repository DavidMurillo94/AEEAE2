# Clase 2. Normalidad y Correlacion

# Cargar los paquetes
library(tidyverse)
library(car)

# Cargar la base de datos

Pinzones <- read.csv("Clase2_Correlacion/PinzonesAfricanos.csv")

# Aplicar prueba de normalidad

shapiro.test(Pinzones$Peso)
shapiro.test(Pinzones$LargoDePico)

qqPlot(Pinzones$Peso)
qqPlot(Pinzones$LargoDePico)

# Analisis de correlacion
  # Pearson = Distribucion normal
  # Spearman = No distribucion normal
  # Kendall = No distribucion normal

cor.test(Pinzones$Peso, Pinzones$LargoDePico, method = "spearman")
cor.test(Pinzones$LargoDePico, Pinzones$Peso, method = "spearman")

cor.test(Pinzones$Peso, Pinzones$LargoDePico, method = "kendall")

cor.test(Pinzones$Peso, Pinzones$LargoDePico, method = "pearson")

# Grafico

ggplot(data= Pinzones, aes(x= Peso, y= LargoDePico)) +
  geom_point()+
  geom_smooth(method = lm) +
  facet_wrap(~Especie)



# Separar especies

CRU.WAXB <- subset(Pinzones, Especie == "CRU.WAXB")
View(CRU.WAXB)

WB.SPARW <- subset(Pinzones, Especie == "WB.SPARW")


P1 <- ggplot(data= CRU.WAXB, aes(x= Peso, y= LargoDePico)) +
  geom_point()+
  geom_smooth(method = lm)+
  labs(title = "waxb",
       y= "Largo de pico(mm)",
       x= "Peso(gr)")+
  theme_classic()

P1

P2<- ggplot(data= WB.SPARW, aes(x= Peso, y= LargoDePico)) +
  geom_point()+
  geom_smooth(method = lm)+
  labs(title = "SPARW",
       y="",
       x="Peso(gr)")+
  theme_classic()

P2

library(cowplot)

plot_grid(P1, P2, labels = c("A)","B)"),
          ncol= 2,
          nrow= 1,
          label_size = 14,
          label_colour = "red")


?plot_grid

Pinzones2 <- Pinzones[ ,c(-1,-3)]
View(Pinzones2)


library(PerformanceAnalytics)

chart.Correlation()
?chart.Correlation


data("managers")
View(managers)

chart.Correlation(managers[,1:8], histogram=TRUE, pch="+", method = "spearman")

library(psych)
corPlot(managers[,1:8], cex = 1.2, main = "Matriz de correlación")

library(corrplot)

corrplot(cor(managers[,1:8]),        # Matriz de correlación
         method = "shade", # Método para el gráfico de correlación
         type = "full",    # Estilo del gráfico (también "upper" y "lower")
         diag = TRUE,      # Si TRUE (por defecto), añade la diagonal
         tl.col = "black", # Color de las etiquetas
         bg = "white",     # Color de fondo
         title = "Correlacion",       # Título
         col = NULL)       # Paleta de colores

data("iris")

png("Clase2_Correlacion/Correlacion.png", units = "cm", height = 12, width = 12, res = 600)
p1 <- corrplot(cor(iris[,c(1,3,4)]), 
         method = "shade",
         type = "full",
         diag = TRUE,
         tl.col = "black",
         bg = "white",
         col= NULL)
dev.off()


x11()
corrplot(cor(iris[,c(1,3,4)]), 
         method = "shade",
         type = "full",
         diag = TRUE,
         tl.col = "black",
         bg = "white",
         col= NULL)
