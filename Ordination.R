source("Descriptive.R")


#visualize dissimilarity of the samples in regard to species abundance. This allowed us to detect differences in community composition between the six sites and two categories.


#make df with only number of individuals of each species per site x as dune
bumble_ord <- bumblebees |>   
  group_by(site, state) |> 
  count(species)  |> pivot_wider(names_from = species, 
                                 values_from = n, values_fill = 0) |> 
  as.data.frame() |>
  dplyr::select(-c('site':'state')) 

#remove B. for bombus
names(bumble_ord) = str_remove(names(bumble_ord),"^B\\. ") 

#extract first two letters of species name
names(bumble_ord) = substr(names(bumble_ord),1,3) 

#log transform data common for count data to lessen the effect of very abundant species

transformed <- log1p(bumble_ord) 

#perform rda of transformed data
ord <- rda(transformed) 

#df with state and site same lenght as bumble_ord
state_df <- bumblebees[, c("site", "state")] |>
  distinct()


############################# PCA ############################################

plot(ord, disp="site", type="n", xlim = c(-2, 1.5))

ordihull(ord, state_df$state, col=1:2, lwd=1, scaling = "species") 

points(ord, disp="species", pch=19, col="darkgreen", cex=1, scaling = "species")

text(ord, labels =  names(bumble_ord), pos = c(1,1,1,1,1,1,1,1,1), offset = 0.2, display = "species", col = "darkgreen", cex = 0.9, scaling = "species") 

text(ord, labels =  str_to_title(state_df$site), pos = c(2,1,4,3,4,2), offset = 0.2, display = "site", col = "darkblue", cex = 0.9, scaling = "species") 

legend("topleft", legend = c("Open", "Overgrown"), col = 1:2, pch = 2, pt.cex = 1, cex = .8, text.col = "black", horiz = F , bty = "n", inset = c(0.05, 0.05))


summary(ord) #shows eiegenvalues

#Appendix

############################ NMDS ########################################

# Compute Bray-Curtis dissimilarity matrix
dist_matrix <- vegdist(transformed, method = "bray")


# Perform NMDS
nmds_result <- metaMDS(dist_matrix, k = 2, trymax = 100)
# 
# Warning message:
#   In metaMDS(dist_matrix, k = 2, trymax = 100) :
#   stress is (nearly) zero: you may have insufficient data

#dataset too small for NMDS


############################# making matrices for RDA #################################

#preparing data
abundance <- bumblebees |> 
  group_by(site, state, period, species) |> 
  count(species)  |> 
  pivot_wider(names_from = species, 
              values_from = n, values_fill = 0)

#change order of sites
abundance <- abundance[c(1,2,3,4,11,12,5,6,7,8,9,10),] 

all <- left_join(abundance, vegetation_means, by = "site") 

#species matrix
species_matrix <- all[ , c(4:12)] 

#environmental matrix
env_matrix <- all[ , -c(4:12)] 

#log transform species data
species_matrix <- log1p(species_matrix) 


############################## rda #####################################

#remove B. for bombus
names(species_matrix) = str_remove(names(species_matrix),"^B\\. ") 

#extract first two letters of species name
names(species_matrix) = substr(names(species_matrix),1,2) 

#run the RDA
rda <- rda(species_matrix ~ state + period + `Calluna_vulgaris_%`, data = env_matrix)

summary(rda) #shows eigenvalues

#test for significance of each variablex
anova.cca(rda, step = 1000, by = "term")
#period is statistically significant, p = 0.007

anova.cca(rda, step = 1000, by = "axis")
#RDA1 significant, p = 0.026

anova.cca(rda)
#full model is statisticaly significant, p = 0.032

############################## plot rda #####################################

#extract scores
#for categorical: extract from centroids, for continous: extract from biplot


calluna_1 <- rda$CCA$biplot[3,1]
calluna_2 <- rda$CCA$biplot[3,2]
open_1 <- rda$CCA$centroids[1,1]
open_2 <- rda$CCA$centroids[1,2]
overgrown_1 <- rda$CCA$centroids[2,1]
overgrown_2 <- rda$CCA$centroids[2,2]
first_1 <- rda$CCA$centroids[3,1]
first_2 <- rda$CCA$centroids[3,2]
last_1 <- rda$CCA$centroids[4,1]
last_2 <- rda$CCA$centroids[4,2]


# Create a biplot of the RDA, scaled as species
plot(rda, type = "n", scaling = "species") # Create an empty plot
points(rda, display = "species",pch = 19, col = "darkgreen", scaling = "species") # Add species scores
text(rda, labels =  names(species_matrix), pos = c(1,1,1,1,1,1,1,1,1), offset = 0.5, display = "species", col = "darkgreen", cex = 0.9, scaling = "species") #species names
points(rda, display = "site",pch = rep(rep(c(3, 3), each = 6), times = 2),  col = rep(rep(c("black", "red"), each = 6), times = 2)) 
arrows(0, 0, rda$CCA$biplot[3,1], rda$CCA$biplot[3,2], length=0.1, angle=20, col="blue", lty=1, lwd=2) # 
text(calluna_1, calluna_2, labels="Calluna", pos=4, col="blue", cex=1.2)
text(open_1, open_2, labels="open", pos=4, col="blue", cex=1.2)
text(overgrown_1, overgrown_2, labels="overgrown", pos=3, col="blue", cex=1.2)
text(first_1, first_2, labels="first", pos=1, col="blue", cex=1.2)
text(last_1, last_2, labels="last", pos=4, col="blue", cex=1.2)
legend("topleft", legend = c("Open sites", "Overgrown sites"), col = 1:2, pch = 3, pt.cex = 1, cex = 0.8, text.col = "black", horiz = F , bty = "n", inset = c(0.05, 0.05))




#scaled as site

# plot(rda, type = "n", scaling = "site") # Create an empty plot
# points(rda, display = "species", pch = 19, col = "darkgreen", scaling = "site") # Add species scores
# text(rda, labels =  str_remove(names(species_matrix),"^B\\. "), pos = c(1,1,1,1,1,3,1,1,1), offset = 0.5, display = "species", col = "darkgreen", cex = 0.9, scaling = "site") #species names
# points(rda, display = "site",pch = rep(rep(c(3, 3), each = 6), times = 2),  col = rep(rep(c("red", "black"), each = 6), times = 2)) 
# arrows(0, 0, rda$CCA$biplot[3,1], rda$CCA$biplot[3,2], length=0.1, angle=20, col="blue", lty=1, lwd=2) # 
# text(rda$CCA$biplot[3,1], rda$CCA$biplot[3,2], labels="calluna cover", pos=4, col="blue", cex=1.2)
# text(rda$CCA$centroids[1,1], rda$CCA$centroids[1,2], labels="open", pos=4, col="blue", cex=1.2)
# text(rda$CCA$centroids[2,1], rda$CCA$centroids[2,2], labels="overgrown", pos=3, col="blue", cex=1.2)
# text(rda$CCA$centroids[3,1], rda$CCA$centroids[3,2], labels="first", pos=1, col="blue", cex=1.2)
# text(rda$CCA$centroids[4,1], rda$CCA$centroids[4,2], labels="last", pos=4, col="blue", cex=1.2)
# legend("topleft", legend = c("site period x", "site period x"), col = 1:2, pch = 3, pt.cex = 1, cex = .8, text.col = "black", horiz = F , bty = "n", inset = c(0.05, 0.05))




