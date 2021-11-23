# Clase 3. Kruskall Wallis

# Cargar paquetes

library(tidyverse)
library(car)
library(FSA)
library(readxl)

# Cargar base de datos

Pinguinos <- read_excel("Clase3_Kruskal_y_dunn/pinguinos.xlsx")

str(Pinguinos)

# Determinar distribucion normal

qqPlot(Pinguinos$bill_length_mm)
qqPlot(Pinguinos$bill_depth_mm)
qqPlot(Pinguinos$body_mass_g)

# Kruskal wallis

kruskal.test(body_mass_g ~ sex, data= Pinguinos)

kruskal.test(body_mass_g ~ island, data= Pinguinos)

# prueba de contraste dunn

dunnTest(body_mass_g ~ island, data= Pinguinos)

# Preparar tabla

Tablares <- Summarize(body_mass_g ~ island, data= Pinguinos)
Tablares

Tablares$IC95 <- 1.96 * Tablares$sd / sqrt(Tablares$nvalid)
Tablares

# Grafico

ggplot(data= Tablares, aes(x= island, y=mean))+
  geom_point(shape= 15, size= 3, color= "gray") +
  geom_errorbar(width= 0.4,  aes(ymin= mean - IC95,
                    ymax= mean + IC95))+
  theme_classic()


