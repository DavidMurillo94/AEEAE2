# Clase 9 
  # Permanova, Anosim, MRPP, Simper, Indicador de especies

# Analisis de similaridad
 # complemento al PCA y al NMDS

# cargar paquetes
# install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(vegan)
library(indicspecies)
library(devtools)
library(pairwiseAdonis)

# Cargar base de datos

Aves <- read.csv("Clase_11_ANOSIM_MRPP_SIMPER/DAves.csv")
Sitios <- read.csv("Clase_11_ANOSIM_MRPP_SIMPER/Sitios.csv")

# Permanova

adonis(Aves ~ Sistema, Sitios, permutations = 999, method = "bray")

adonis(Aves ~ Sitios$Sistema, permutations = 999, method = "bray")

# Prueba de contraste de permanova

pairwise.adonis(Aves, Sitios$Sistema, sim.method = "bray", perm = 999)

# Anosim

anosim(Aves, Sitios$Sistema, permutations = 999, distance = "bray")
Aves_simper<- simper(Aves, Sitios$Sistema, permutations = 999)
summary(Aves_simper)

# MRPP

mrpp(Aves, Sitios$Sistema, permutations = 999, distance = "bray")

aves_mrpp <- with(Sitios, meandist(vegdist(Aves,method="bray"), Sistema))
aves_mrpp
plot(aves_mrpp)

Aves_permanova <-pairwise.adonis(Aves, Sitios$Sistema, sim.method = "bray", perm = 999)

Perma_tabla <- data.frame(Aves_permanova)
View(Perma_tabla)
mrpp_tabla <- data.frame(aves_mrpp)
View(mrpp_tabla)

sink("Clase_11_ANOSIM_MRPP_SIMPER/Tabla_simper.txt")
Aves_simper
print(Aves_simper)

plot(aves_mrpp, main = "Disimilaridad entre sistema", sub = "Yoro, Honduras",
     xlab= "Sistema", ylab ="Disimilitud (indide = Bray-curtis)",
     col = "black", ylim = c(0.65, 0.80), cex = 0.7)
?plot

# Indicador de especies
Aves_indicadoras <- multipatt(Aves, Sitios$Sistema, func = "r.g", control = how(nperm = 999))
summary(Aves_indicadoras)
Aves_indicadoras


