source("Descriptive.R")

## glm models

#make df with total abundance of specialist species and generalist group

bumble_abundance <- bumblebees |> 
  group_by(site, state, period, species) |> 
  filter(species=='B. muscorum'| species=='B. jonellus' | species=='B. lucorum complex') |> 
  count(species)  |> 
  mutate(total = sum(n)) |> 
  pivot_wider(names_from = species, 
              values_from = n, values_fill = 0) 


#combine abundance and vegetation means
specialist_total_PT_net <- left_join(bumble_abundance, vegetation_means, by = "site") 

#distributions
table(bumble_abundance$`B. lucorum complex`)
table(bumble_abundance$`B. muscorum`)
table(bumble_abundance$`B. jonellus`)

########################## test correlations ##################################
check_cor <- specialist_total_PT_net[-(1:3)] #remove non-numeric variables
cor(check_cor)
#correlated variables: Vaccinium uliginosum_%, Empetrum nigrum_% , Arctostaphylos uva-ursi_%, Epilobium_%, Vaccinium_vitis_idaea_%, Salix_repens_%, Sorbus_aucuparia_%, Succisa_prantensis_%, Hieracium_%, `Erica cinerea_%`, moss, shrub, herb, tree, gramnniods


#check correlations between period and Calluna cover

# Recode the "state" variable to test 
specialist_total_PT_net$state_recode <- ifelse(specialist_total_PT_net$state == "open", 0, 1)
cor.test(specialist_total_PT_net$`Calluna_vulgaris_%`, specialist_total_PT_net$state_recode)
#Calluna cover and state is correlated, can only use one in my glms

################################# testing AIC of models B. jonellus ###########

#trying different models and comparing AIC

m1 <- glm.nb(`B. jonellus` ~ `period` +`Calluna_vulgaris_%`, data = specialist_total_PT_net)
summary(m1) 

m2 <- glm.nb(`B. jonellus` ~ state + period, data = specialist_total_PT_net)
summary(m2)

AIC(m1, m2) 

#AICs are very close, but choose m4 because of research questions

################################# model selection ######################

m3 <- glm.nb(`B. muscorum` ~ `period` +`Calluna_vulgaris_%`, data = specialist_total_PT_net)
summary(m7) 

m4 <- glm.nb(`B. muscorum` ~ state + period, data = specialist_total_PT_net) #periodelast signifikant
summary(m4)
check_zeroinflation(m4) #model is underfitting zeros

AIC(m3, m4) 

#choosing the same variables for the other species based on research questions

#fitting a zero inflation because of underfitting of zeros
m_zi <- zeroinfl(`B. muscorum` ~ state + period, dist = 'negbin', data = specialist_total_PT_net)
summary(m_zi)
#periodlast significant

#compare nb and zero inflated model
vuong(m4, m_zi) Felt
#model2 > model1

#likelihood ratio test
lrtest(m4, m_zi) 
#use model 2

#finding model for B. lucorum complex
nb_com <- glm.nb(`B. lucorum complex` ~ state + period, data = specialist_total_PT_net)
summary(nb_com)
check_zeroinflation(nb_com) #model is underfitting zeros

l_zi <- zeroinfl(`B. lucorum complex` ~ state + period | 1, dist = 'negbin', data = specialist_total_PT_net)
summary(l_zi)

################### Test if negative binomial or Poisson ###################

m1_poisson <- glm(`B. jonellus` ~ state + period, family = poisson, data = specialist_total_PT_net)
anova(m1_poisson) #poisson not a good fit

m2_poisson <- glm(`B. muscorum` ~ period + state, family = poisson, data = specialist_total_PT_net)
anova(m2_poisson) #poisson not a good fit

# Overall, the Analysis of Deviance table suggests that neither of the predictor variables have a significant effect on the response and that the model may not be a good fit for the data.

#R-club
################### Final models ###################

#zero-inflated models not performing a lot better than negativ binomial

m2 <- glm.nb(`B. jonellus` ~ state + period, data = specialist_total_PT_net)
summary(m2)

m4 <- glm.nb(`B. muscorum` ~ state + period, data = specialist_total_PT_net) #periodelast signifikant
summary(m4)

nb_com <- glm.nb(`B. lucorum complex` ~ state + period, data = specialist_total_PT_net)
summary(nb_com)


################ trying to work around separation issue #####################


# Combine "overgrown" and "open but no B. muscorum" categories
specialist_total_PT_net$state_new <- ifelse(specialist_total_PT_net$state == "open" & specialist_total_PT_net$`B. muscorum` == 0,
                                            "open_no_B_muscorum", specialist_total_PT_net$state)

# Fit negative binomial regression model
m <- glm.nb(`B. muscorum` ~ state_new + period, data = specialist_total_PT_net)

# Check model summary
summary(m)



#method 2

# Remove the "overgrown" state
specialist_total_PT_net <- subset(specialist_total_PT_net, state == "open")

# Fit a hurdle model
hurdle_model <- hurdle(`B. muscorum` ~ period, data = specialist_total_PT_net, dist = "negbin")
summary(hurdle_model)

hurdle_model <- hurdle(`B. muscorum` ~ state + periode, data = specialist_total_PT_net, dist = "negbin")


# Fit a hurdle model
hurdle_model <- hurdle(`B. muscorum` ~ state + period, data = specialist_total_PT_net, dist = "negbin")

# Summarize the model
summary(hurdle_model)




