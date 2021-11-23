# Clase 5. Curva de acumulacion de especies

# Cargar paquetes
library(tidyverse)
library(vegan)

# Cargar base de datos 

Abundancia <- read.csv("Clase5_Rarefaccion/Abundancia.csv")

View(Abundancia)

# CAE

Aves_cae <- poolaccum(Abundancia[ , 12:129])
plot(Aves_cae)

# Preparar tablas

table(Abundancia$Sistema)
Bosque <- filter(Abundancia, Sistema == "BOSQUE")
Daib <- filter(Abundancia, Sistema == "DAIB")
Daic <- filter(Abundancia, Sistema == "DAIC")
Sol <- filter(Abundancia, Sistema == "SOL")
Sombra <- filter(Abundancia, Sistema == "SOMBRA")

# CAE para sistema

Bosque_cae <- specaccum(Bosque[, 12:129], method = "rarefaction") 
Daib_cae <- specaccum(Daib[, 12:129], method = "rarefaction") 
Daic_cae <- specaccum(Daic[, 12:129], method = "rarefaction") 
Sol_cae <- specaccum(Sol[, 12:129], method = "rarefaction") 
Sombra_cae <- specaccum(Sombra[, 12:129], method = "rarefaction") 

# Preparar tablas para grafico
Bosque_cae

Bosque_tabla <- data.frame(Sistema = "BOSQUE",
                           Riqueza = Bosque_cae$richness,
                           DS = Bosque_cae$sd,
                           Sitios = Bosque_cae$sites)

Daib_tabla <- data.frame(Sistema = "DAIB",
                           Riqueza = Daib_cae$richness,
                           DS = Daib_cae$sd,
                           Sitios =Daib_cae$sites)

Daic_tabla <- data.frame(Sistema = "DAIC",
                         Riqueza = Daic_cae$richness,
                         DS = Daic_cae$sd,
                         Sitios =Daic_cae$sites)

Sol_tabla <- data.frame(Sistema = "SOL",
                         Riqueza = Sol_cae$richness,
                         DS = Sol_cae$sd,
                         Sitios =Sol_cae$sites)


Sombra_tabla <- data.frame(Sistema = "SOMBRA",
                        Riqueza = Sombra_cae$richness,
                        DS = Sombra_cae$sd,
                        Sitios =Sombra_cae$sites)

# Combinar tablas

TablaFinal <- rbind(Bosque_tabla, Daib_tabla, Daic_tabla, Sol_tabla, Sombra_tabla)
View(TablaFinal)

TablaFinal$IC95 <- 1.96 * TablaFinal$DS / sqrt(1)
View(TablaFinal)

# Grafico

ggplot(data= TablaFinal, aes(x= Sitios, y= Riqueza))+
  geom_line(aes(color = Sistema))+
  geom_ribbon(alpha= 0.2, linetype = 0 , aes(ymin= Riqueza - IC95,
                  ymax= Riqueza + IC95,
                  color= Sistema,
                  fill= Sistema))+
  theme_classic() +
  scale_color_manual(values = c("darkgreen", "lightgreen",
                                "orange", "red", "gray"))+
  scale_fill_manual(values = c("darkgreen", "lightgreen",
                                "orange", "red", "gray"))+
  labs(title = "Curva de acumulacion de especies",
       caption = "Intervalos al 95%")


# Segunda parte
View(TablaFinal)


Bosque_final <- filter(TablaFinal, Sistema == "BOSQUE")
Daib_final <- filter(TablaFinal, Sistema == "DAIB")
Daic_final <- filter(TablaFinal, Sistema == "DAIC")
Sol_final <- filter(TablaFinal, Sistema == "SOL")
Sombra_final <- filter(TablaFinal, Sistema == "SOMBRA")


Tablagrafica <- rbind(Bosque_final[44,], 
                           Daib_final[44,],
                      Daic_final[44,],
                      Sol_final[41,],
                      Sombra_final[47,])
Tablagrafica

ggplot(data= Tablagrafica, aes(x= Sistema, y= Riqueza))+
  geom_point(size= 1, shape= 15)+
  geom_errorbar(aes(ymin= Riqueza- IC95,
                    ymax = Riqueza + IC95), 
                width = 0.5) +
  theme_classic()+
  labs(title = "Riqueza de especies",
       caption = "Con intervalos al 95%")

# Diversidad

Abundancia$Diversidad <- diversity(Abundancia[ ,12:129], index = "simpson")
Abundancia$Diversidad2 <- diversity(Abundancia[ ,12:129], index = "shannon")
Abundancia$Diversidad3 <- diversity(Abundancia[ ,12:129], index = "invsimpson")

View(Abundancia)


# Tabla resumen Tidyverse

DiverRes <- Abundancia %>% 
  group_by(Sistema) %>%
  summarise(Media = mean(Diversidad2), 
            DS = sd(Diversidad2),
            Muestras = n(),
            IC95 = 1.96 * DS / sqrt(Muestras))
DiverRes            
  
ggplot(data= DiverRes, aes(x= Sistema, y= Media))+
  geom_point()+
  geom_errorbar(aes(ymin = Media - IC95,
                    ymax= Media + IC95),
                width = 0.4)







