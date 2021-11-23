# Clase 9. NMDS permanova

#Cargar paquetes
library(tidyverse)
library(vegan)

# Cargar base de datos

Abundancia <- read.csv("Clase10_NMDS_Permanova/DAves.csv")
Sitios <- read.csv("Clase10_NMDS_Permanova/Sitios.csv")

UnaTabla <- cbind(Sitios, Abundancia)

# NMDS
 # Analisis de similaridad

Aves_nmds <- metaMDS(Abundancia,  try = 1000, k = 3, distance = "bray")

# stress
# ~0.1 muy bueno
# ~0.15 - 0.20 bueno
#~0.21 - 0.29 regular
# >0.30 no es bueno, malo

Aves_nmds$stress

# Prepara las tablas de las dim

Especies <- data.frame(Aves_nmds$species)
Especies$Especie <- rownames(Especies)
View(Especies)

Sistema <- data.frame(Aves_nmds$points,
                      Sistema = Sitios$Sistema)
View(Sistema)

# Grafico nmds

SistemasDAI <- Sistema %>% 
  filter(Sistema %in% c("BOS", "DAIB", "SOL"))

EspeciesImp <- Especies %>% 
  filter(Especie == "CARPUS")

ggplot()+
  geom_point(data= SistemasDAI, aes(x= MDS1, y= MDS2, color= Sistema))+
  stat_ellipse(data= SistemasDAI, size= 1, geom = "polygon", alpha= 0.5,
               aes(x= MDS1, y= MDS2, color = Sistema, fill= Sistema))+
  geom_text(data= EspeciesImp, size= 3, color = "black",
            aes(x= MDS1, y= MDS2, label = Especie))+
  theme_classic()

?stat_ellipse

ggplot()+
  geom_point(data= SistemasDAI, aes(x= MDS2, y= MDS3, color= Sistema))+
  geom_text(data= EspeciesImp, size= 3, color = "black", alpha= 0.7,
            aes(x= MDS2, y= MDS3, label = Especie))+
  stat_ellipse(data= SistemasDAI, geom = "polygon", alpha= 0.5,
               aes(x= MDS2, y= MDS3, color = Sistema, fill= Sistema))+
  theme_classic()



