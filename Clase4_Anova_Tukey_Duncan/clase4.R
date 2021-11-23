# Clase 4 ANOVA, Tukey, Duncan 

# Cargar paquetes

library(tidyverse)
library(car)
library(FSA)

# Cargar base de datos

Vegetacion <- read.csv("Clase4_Anova_Tukey_Duncan/PRAWterrVegData2.csv")
str(Vegetacion)


# Prueba de normalidad

qqPlot(Vegetacion$Underheight)
shapiro.test(Vegetacion$Underheight)

# ANOVA
Vegetacion$year <- factor(Vegetacion$year)
str(Vegetacion)

Veg_anova <- aov(Underheight ~ year, data= Vegetacion)
summary(Veg_anova)


# Prueba de contraste
A <-TukeyHSD(Veg_anova, "year")
Resultados_tukey <- data.frame(A$year)
Resultados_tukey
class(Resultados_tukey)


library(agricolae)
Vege_duncan <- duncan.test(Veg_anova, "year", console = TRUE)
Vege_duncan
plot(Vege_duncan)


LSD.test(Vegetacion$Underheight, Vegetacion$year,112,1020,
         alpha = 0.05, p.adj = "bonferroni",group = T,
         console = T)

# Preparar tabla

TablaRes <- Summarize(Underheight ~ year, data= Vegetacion)
TablaRes$IC95 <- 1.96 * TablaRes$sd / sqrt(TablaRes$n)
TablaRes

# Grafico

ggplot(data= TablaRes, aes(x= year, y= mean))+
  geom_point(size = 1, aes(color = year))+
  geom_errorbar(width = 0.7, aes(ymin= mean - IC95,
                               ymax = mean + IC95,
                               color= year))+
  scale_color_manual(values = c("black", "darkgreen", "red", "darkblue"))+
  theme_classic()+
  labs(title = "Diferencias de alturas de arboles por year",
       y= "Altura de arboles")+
  annotate("text", x= "2009", y= 100, label = "a")+
  annotate("text", x= "2010", y= 120, label= "b")+
  annotate("text", x= "2011", y= 140, label = "bc")+
  annotate("text", x= "2012", y= 165, label= "c")

# T test
data("iris")
View(iris)
str(iris)

t.test(iris$Sepal.Length, iris$Sepal.Width)

t.test(Sepal.Length ~ Species, data= iris)

class(iris$Species)
ANOVA <-aov(Sepal.Length ~ Species, data= iris)
summary(ANOVA)




Resultados_anova

Resultados_anova <- summary(Veg_anova)

sink("Clase4_Anova_Tukey_Duncan/Resultados.txt")
Resultados_anova
print(Resultados_anova)

library(ggstatsplot)
ggbetweenstats(Vegetacion,
               year,Underheight,
               plot.type = "box")


