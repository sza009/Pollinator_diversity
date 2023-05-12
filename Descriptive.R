#load packages
library(tidyverse)
library(vegan)
library(Rmisc) #for summarySE
library(ggplot2)
#library(lmtest)
library(MASS) #for negative binomial
library(ggbeeswarm) #violin plots
library(performance) # checkzero
#library(emmeans)
library(pscl)  #vuong test
library(patchwork) #combine plots
library(rnaturalearth) 
library(rnaturalearthhires) 
library(sf) #for maps
#library(cowplot)
library(ggspatial) #maps


#import data
bumblebees <- read_csv2(file ="pollinators.bumblebees.csv") |> 
  slice(-(308:362)) |> 
  select(-(23:31)) |> 
  mutate(site = str_to_sentence(site)) |> #all sites to capital letter |> 
  mutate(species = if_else (species == "complex", true = "B. lucorum complex", false = species)) |> 
  rename(period = periode)  #change typo "periode to period"   
#if it does not work, reload dplyr

bees <- read_csv2(file ="pollinators.bees.csv") |> 
  mutate(site = str_to_sentence(site))    
bees <- bees[-(57:306),] 

hoverflies <- read_delim(file ="pollinators.hoverflies.csv") |> 
  mutate(site = str_to_sentence(site))
hoverflies <- hoverflies[-(7:22),] 

vegetation <- read_csv2(file ="vegetation.csv") |> 
  mutate(site = str_to_sentence(site)) 

#create abundance plot
abund <- ggplot(bumblebees, aes(x = site, fill = species)) + 
  geom_bar() +
  theme_bw() +
  labs(y = "Total abundance", fill = str_to_title("species") ) +
  facet_wrap(facets =vars(str_to_title(state)), scale="free_x") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.x = element_blank(), legend.text = element_text(face = "italic")) +
  guides(fill = guide_legend(ncol = 5))

#create relative abundance plot
rel_abun <- ggplot(bumblebees, aes(x = site, fill = species)) + 
  geom_bar(position = "fill") +
  theme_bw() +
  labs(y = "Percent abundance", fill = str_to_title("species") ) +
  facet_wrap(facets =vars(str_to_title(state)), scale="free_x") +
  scale_y_continuous(expand = c(0.01, 0), labels = scales::label_percent()) +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.x = element_blank(), legend.text = element_text(face = "italic")) +
  guides(fill = guide_legend(ncol = 5))

#combine plots
abund + rel_abun + plot_layout(guides = 'collect', ncol = 2, widths = c(0.5, 0.5))  &  theme(plot.margin = margin(10,10,10,10), legend.position='bottom')



################################################################################

#plot species and sample method
bumble_method <- bumblebees |>  #barplot number of specier per site
  group_by(site, method) |> 
  count(species) 


ggplot(data = bumble_method,
                mapping = aes(x = site, y = n, fill = method)) +
  geom_col() +
  labs(x = "Site", y = "Count", fill = str_to_title("method")) +
  facet_wrap(facets =vars(species)) +
  scale_color_viridis_d() +
  theme_bw() +
  theme(axis.title.x = element_blank()) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#For Appendix

################################################################################

#abundance of Complex
complexs_periode <- bumblebees |>
  filter(species == 'B. lucorum complex') |> 
  group_by(period, state, site) |> 
  count(species) 


ggplot(data = complexs_periode,
                       mapping = aes(x = period, y = n, fill = state)) +
  geom_col() +
  labs(x = "Periode", y = "B. lucorum complex abundance") +
  facet_wrap(facets =vars(site)) +
  theme_bw() 


#abundance of B. jonellus
jonellus_periode <- bumblebees |>
  filter(species == 'B. jonellus') |> 
  group_by(period, state, site) |> 
  count(species) 


ggplot(data = jonellus_periode,
       mapping = aes(x = period, y = n, fill = state)) +
  geom_col() +
  labs(x = "Periode", y = "B. jonellus abundance") +
  facet_wrap(facets =vars(site)) +
  theme_bw() 



#abundance of B. muscorum
muscorum_periode <- bumblebees |>
  filter(species == 'B. muscorum') |> 
  group_by(period, state, site) |> 
  count(species) 


ggplot(data = muscorum_periode,
       mapping = aes(x = period, y = n, fill = state)) +
  geom_col() +
  labs(x = "Periode", y = "B. muscorum abundance") +
  facet_wrap(facets =vars(site)) +
  theme_bw() 



################################################################################

#dispersion of honeybees
ggplot(bees, aes(x = site, fill = )) + 
  geom_bar(fill = "lightblue") +
  theme_bw() +
  labs(x = "Site", y = "Apis melifera abundance") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.title.x = element_blank())

#Appendix

################################################################################

#dispersion of hoverlfies
ggplot(hoverflies, aes(x = site, 
                       y = number_individuals,
                       fill = state)) +
  labs(x = "Site", y = "Hoverfly abundance", fill = str_to_title("state")) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme(axis.title.x = element_blank()) + 
  theme_bw()

#Appendix

################################################################################

#find means of vegetation values

vegetation_means <- vegetation |>
  group_by(site) |>
  summarise(across(c('cover_heather_%':'Tree_cover_1km_%'), .fns = mean)) 

#vegetation means of functional groups

functional_groups <- vegetation_means |> 
  select('site', 'cover_heather_%', 'cover_shrubs_%', 'cover_herbs_%', 'cover_gramniods_%', 'cover_moss_%')

################################################################################

#dispersion of Calluna vulgaris

#summary
sum_calluna <- summarySE(vegetation, measurevar =  "Calluna_vulgaris_%",
                         groupvar = c("state", "site"), na.rm = TRUE)


# #violinplot
vegetation |> 
  ggplot(aes(site, `Calluna_vulgaris_%` )) + 
  geom_violin(fill = "lightblue") +
  geom_quasirandom(varwidth = TRUE) +
  facet_wrap(facets =vars(str_to_title(state)), scale="free_x") +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Cover Calluna (%)" )


#anova to test if difference is significant

# Compute the analysis of variance
res.aov <- aov(`Calluna_vulgaris_%` ~ state, data = sum_calluna)
# Summary of the analysis
summary(res.aov)

#P-value is 0.00757, signifikant different between calluna cover between sites


#find out if you can actually use anova, normality?

#assumptions for anova
plot(res.aov, 1)
plot(res.aov, 2)

#Shapiro test to see if normality is violated
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

################################################################################

#plot tree cover 1 km radius

#make into table
vegetation |> 
  ggplot(aes(x = site, y = `Tree_cover_1km_%`)) +
  geom_col( fill = "lightblue") +
  facet_wrap(facets =vars(str_to_title(state)), scale="free_x") +
  theme(axis.title.x = element_blank()) +
  labs(y = "Tree cover (%)" ) +
  theme_bw() 
  #coord_cartesian(ylim = c(0, 100))


#anova to test if difference is significant

# Compute the analysis of variance
res.aov_2 <- aov(`Tree_cover_1km_%` ~ state, data = vegetation)
# Summary of the analysis
summary(res.aov_2)

################################################################################

vegetation |> 
  ggplot(aes(x = site, y = `Vaccinium myrtillus_%`)) +
  geom_col( fill = "lightblue") +
  facet_wrap(facets =vars(state), scale="free_x") +
  theme(axis.title.x = element_blank()) +
  labs(y = " Cover Vaccinium myrtillus (%)" ) +
  theme_bw() 



################################################################################

vegetation |> 
  ggplot(aes(x = site, y = `Vaccinium_vitis_idaea_%`)) +
  geom_col( fill = "lightblue") +
  facet_wrap(facets =vars(state), scale="free_x") +
  theme(axis.title.x = element_blank()) +
  labs(y = "Cover Vaccinium vitis idaea (%)" ) +
  theme_bw() 


################################################################################

vegetation |> 
  ggplot(aes(x = site, y = `Erica tetralix_%`)) +
  geom_col( fill = "lightblue") +
  facet_wrap(facets =vars(state), scale="free_x") +
  theme(axis.title.x = element_blank()) +
  labs(y = "Cover Erica tetralix (%)" ) +
  theme_bw() 

################################################################################

vegetation |> 
  ggplot(aes(x = site, y = `Vaccinium uliginosum_%`)) +
  geom_col( fill = "lightblue") +
  facet_wrap(facets =vars(state), scale="free_x") +
  theme(axis.title.x = element_blank()) +
  labs(y = " Cover Vaccinium myrtillus (%)" ) +
  theme_bw() 


################################################################################

#relationship between temp and abundance
temp <- bumblebees |>
  #filter(species=='B. muscorum'| species=='B. jonellus') |> 
  filter(method == 'net') |> 
  group_by(`temperature_C`, date, site, period) |> 
  count(species) 

temp |> 
  ggplot(aes(x = `temperature_C`, y =n, color = period) ) +
  geom_point()  +
  facet_wrap(facets =vars(site), scale="free_x") +
  geom_smooth(method = 'lm') +
  theme_bw() +
  labs(x = "Temperature (°C)", y = "Bombus abundance", color = str_to_title("period"))

#no relationship found between number of individuals captured and temperature
#appendix

################################################################################

#relationship between wind and abundance
wind_table <- bumblebees |> 
  #filter(species=='B. muscorum'| species=='B. jonellus') |> 
  filter(method == 'net') |> 
  group_by(`wind_m/s`, date, site, period) |> 
  count(species) 


wind_table |> 
  ggplot(aes(x = `wind_m/s`, y =n, color = period) ) +
  geom_point()  +
  facet_wrap(facets =vars(site), scale="free_x") +
  geom_smooth(method = 'lm') +
  theme_bw() +
  labs(x = "Wind (m/s)", y = "Bombus abundance", color = str_to_title("period"))


#no relationship found between number of individuals captured and wind

################################################################################

#for calculations
bumble_count_all <- bumblebees |>  
  filter(species=='B. soroeensis') |>
  group_by(site, period) |> 
  count(species) 

################################################################################

#for calculations
bumble_count <- bumblebees |>  
  filter(species=='B. jonellus') |>
  group_by(site, period) |> 
  count(species) 


#make table with all info
bumble <- bumblebees |>   
  group_by(site, state, period) |> 
  count(species)  |> pivot_wider(names_from = species, 
                                 values_from = n, values_fill = 0) |> 
  as.data.frame() 

#appendix

