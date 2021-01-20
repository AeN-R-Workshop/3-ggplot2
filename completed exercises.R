rm(list = ls())

library(effects)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(palmerpenguins)
library(ggrepel)
library(gridExtra)
library(egg)
library(GGally)
library(grid)

setwd("")

data("penguins")

head(penguins)

penguins<-na.omit(penguins)

default_colors<-c("#F8766D", "#00BA38", "#619CFF")

#############################

#### exercises

#############################

##############################

#### exercise 1, plot the estimated phytoplankton production (Pc) from the model fit over a sequence of PAR values

##############################

library(phytotools)

#### Growth curve: PE dataset example #### from Package 'phytotools'

PAR <- c(5,10,20,50,100,150,250,400,800,1200) # photosynthetic active radiation: µmol m-2 s-1
Pc <- c(1.02,1.99,3.85,9.2,15.45,21.3,28.8,34.5,39.9,38.6) # phytoplankton production: mg C m-3 hr-1

myfit <- fitEP(PAR, Pc) # fit model

#Plot input data
plot(PAR, Pc, xlim=c(0,1500), ylim=c(0,40), xlab="PAR", ylab="Pc")

#Add model fit
E <- seq(0,1500,by=1) # vector of PAR values from 0 to 1500
with(myfit,{
  P <- E/((1/(alpha[1]*eopt[1]^2))*E^2+(1/ps[1]-2/(alpha[1]*eopt[1]))*E+(1/alpha[1]))
  lines(E,P)
})

#######################

#### plot the same data in ggplot

######################

# you will have to create create two data frames, one with the raw data that shuld be plotted as points geom_point
# and the other with the calculated values based on the model estimates in myfit

PE_data<-data.frame(PAR=PAR,Pc=Pc)

PE_data_fit<-data.frame(P=E/((1/(myfit$alpha[1]*myfit$eopt[1]^2))*E^2+(1/myfit$ps[1]-2/(myfit$alpha[1]*myfit$eopt[1]))*E+(1/myfit$alpha[1])),
                        E=E)

# to include superscript and subscripts you can use the bqoute() function in labs()
#       x= bquote('PAR,'~(µmol~m^-2~s^-1)), 
#       y= bquote('Pc,'~(mg~C~m^-3~hr^-1)))+ 

exercise1<-ggplot(PE_data, aes(x=PAR,y=Pc))+
  labs(title = "Phytoplankton production",
       x= bquote('PAR,'~(µmol~m^-2~s^-1)), 
       y= bquote('Pc,'~(mg~C~m^-3~hr^-1)))+ 
  geom_point()+
  geom_line(PE_data_fit, mapping=aes(x=E, y=P))+
  theme(axis.text.x = element_text(colour="black",size=10),
        axis.text.y = element_text(colour="black",size=10),
        axis.title.x = element_text(colour="black",size=10),
        axis.title.y = element_text(colour="black",size=10),
        plot.title = element_text(colour = "black", size = 8))

pdf("exercise_fig1.pdf", width = 3, height = 3, paper = "special") # needs fixing

plot(exercise1)

dev.off()

################################################

#### exercise 2, plot the length weight relationship of fish and the predicted model estimates

################################################

library(effects) # predicts model estimates from many types of models
library(FSAdata) # has a data set on the length and weight relationship of ruffe, a small benthic fish in temperate and boreal northern hemisphere freshwaters
data(RuffeSLRH92)
head(RuffeSLRH92) 

ruffe_mod<-lm(log(weight)~log(length),data=RuffeSLRH92) # fits the log log relationship of lenght and weight

# predict the weight at certain lenghts from the model above by the Effect() function
# the xlevels argument can be used to get predictions for specific time intervals e.g. seq(min(RuffeSLRH92$length),max(RuffeSLRH92$length),1)
# the effect object can be transformed into a data frame by the data.frame() function
# plot the predictions as lines geom_line(), and plot the length-weight data from RuffeSLRH92 as points geom_point()

eff_ruff<-data.frame(Effect("length", mod = ruffe_mod, transformation=list(link=log, inverse=exp),
                            xlevels=list(length=seq(min(RuffeSLRH92$length),max(RuffeSLRH92$length),1))))

exercise2<-ggplot(RuffeSLRH92, aes(x=length, y=weight))+
  geom_point(pch=21, size=0.5, color="gray")+
  geom_line(eff_ruff, mapping=aes(x=length, y=exp(fit)), size=0.2)+
  geom_line(eff_ruff, mapping=aes(x=length, y=exp(lower)), linetype="dotted", size=0.1)+
  geom_line(eff_ruff, mapping=aes(x=length, y=exp(upper)), linetype="dotted", size=0.1)+
  labs(title = "Length-weight relationship of ruffe", x="Length (mm)", y=" Weight (g)")+
  theme(axis.text.x = element_text(colour="black",size=10),
        axis.text.y = element_text(colour="black",size=10),
        axis.title.x = element_text(colour="black",size=10),
        axis.title.y = element_text(colour="black",size=10),
        plot.title = element_text(colour = "black", size = 10),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.margin = margin(2,10,0,0))

pdf("exercise_fig2.pdf", width = 3, height = 3, paper = "special")

plot(exercise2)

dev.off()

###############################

#### exercise 3, plot model predictions from a mixed model as lines geom_line()
#  and calculate and add averages to plot geom_point() 

###############################

library(lme4) # fits mixed models
library(effects) # predicts model estimates from many types of models

data("ChickWeight")

coplot(weight ~ Time | Chick, data = ChickWeight, # 50 chickens with repeated measurements of their weight
       type = "b", show.given = FALSE)            

str(ChickWeight)

# fits a model where the weight is a function of time and diet,
# the growth over time is allowed to be non-linear by fitting polynomials
# the interaction means that the growth curves are allowed to be different for the different diets
# random effect is chicken ID, each chicken is a replicate, whereas measurements within each chicken are non-independent i.e. repeated measuremetns

mod_chicken<-lmer(weight~poly(Time,3)*Diet+(1|Chick), data = ChickWeight)       

# use the Effect() function to predict the chickweight over time and for the different diets: c("Time", "Diet")
# the xlevels argument can be used to get predictions for specific time intervals e.g. seq(0,22,1)
# the effect object can be transformed into a data frame by the data.frame() function
# plot the predictions as lines geom_line()

chicken_eff<-data.frame(Effect(c("Time", "Diet"), mod_chicken, xlevels=list(Time=seq(0,22,1))))

# calculate the mean weight of chickens for each diet at each time point e.g. by dplyr piping 
# plot the means as points geom_point()

ChickWeight_mean <- ChickWeight %>%
  group_by(Diet,Time) %>%
  summarise(weight=mean(weight))

exercise3<-ggplot(chicken_eff, aes(x=Time, y=fit, color=Diet))+
  geom_line()+
  geom_point(ChickWeight_mean, mapping = aes(x=Time, y=weight, color=Diet))+
  labs(title = "Chicken weight ~ diet", x="Time (days)", y=" Weight (g)")+
  theme(axis.text.x = element_text(colour="black",size=10),
        axis.text.y = element_text(colour="black",size=10),
        axis.title.x = element_text(colour="black",size=10),
        axis.title.y = element_text(colour="black",size=10),
        legend.position = "bottom",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.margin = margin(2,10,0,0),
        plot.title = element_text(colour = "black", size = 10))

pdf("exercise_fig3.pdf", width = 3, height = 3, paper = "special")

plot(exercise3)

dev.off()

############################

### exercise 4, plot averages of penguin weights for each species and sex in a bar graph geom_bar

############################

# calculate averages of penguin weights for each species and sex by using dplyr pipes
# or by using base R commands using split() e.g. s<-split(penguins,interaction(penguins$species,penguins$sex))
# then calculate the average wight in each data frame in the list of data frames 
# e.g.body_mass_g=unlist(lapply(1:6, function(x) mean(s[[x]]$body_mass_g)))

head(penguins)

# dplyr

new_penguins<-  penguins %>%
  group_by(sex,species) %>%
  summarize(body_mass_g=mean(body_mass_g))

str(new_penguins)

# base R

s<-split(penguins,interaction(penguins$species,penguins$sex))

new_penguins1<-data.frame(body_mass_g=unlist(lapply(1:6, function(x) mean(s[[x]]$body_mass_g))),
                          sex=unlist(lapply(1:6, function(x) (s[[x]]$sex[1]))),
                          species=unlist(lapply(1:6, function(x) (s[[x]]$species[1]))))

# use the geom_bar() function to create a bar graph, you will have to 
# use the arguments stat= and position=


exercise4<-ggplot(new_penguins1, aes(x=species, y=body_mass_g, fill=sex))+
  labs(title = "Female and male weights of three penguin species",
       x="Species",
       y="Body mass (g)",
       fill="Sex")+
  geom_bar(stat="identity", position=position_dodge(), color="black")+
  theme(axis.text.x = element_text(colour="black",size=10),
        axis.text.y = element_text(colour="black",size=10),
        axis.title.x = element_text(colour="black",size=10),
        axis.title.y = element_text(colour="black",size=8),
        legend.position = "bottom",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.margin = margin(2,10,0,0),
        plot.title = element_text(colour = "black", size = 10))



pdf("exercise_fig4.pdf", width = 4, height = 3, paper = "special") # needs fixing

plot(exercise4)

dev.off()

###############################

##### exercise 5, plot the egg laying date of the three penguin species for each year in a stacked bar graph

###############################

head(penguins_raw)

# create a column of year, so you can separate the dates for the three different years and plot these as facets 

penguins_raw$year<-format(as.Date(penguins_raw$`Date Egg`, format="%d/%m/%Y"),"%Y") 

# use the geom_bar() function to create a bar graph, you will have to use the arguments stat= and position=
# use the facet_grid() function to create panels of each year, argument scales= will help  

exercise5<-ggplot(penguins_raw, aes(x=`Date Egg`, fill=Species, group=interaction(Species,year,`Date Egg`)))+
  labs(title = "Egg laying date of three penguin species",
       y="Counts",
       x="Egg laying date")+
  geom_bar(stat="count", position = "stack", width = 0.9)+
  scale_fill_manual(values = default_colors, labels=c("P. adeliae","P. antarcticus","P. papua"))+ # change the label names in the legend
  facet_grid(. ~ year, scales="free_x")+
  theme(axis.text.x = element_text(colour="black",size=7),
              axis.text.y = element_text(colour="black",size=10),
              axis.title.x = element_text(colour="black",size=10),
              axis.title.y = element_text(colour="black",size=10),
              plot.title = element_text(colour = "black", size = 10),
              legend.position = "bottom",
              legend.text = element_text(size=10),
              legend.title = element_text(size = 10),
              legend.key.size = unit(10,"point"),
              legend.spacing.y = unit(-.25,"cm"),
              legend.margin=margin(0,5,8,5), 
              legend.box.margin=margin(-10,10,-3,10),
              legend.box.spacing = unit(0.5,"cm"),
              plot.margin = margin(2,2,0,0))

pdf("exercise_fig5.pdf", width = 5, height = 3, paper = "special") # needs fixing

plot(exercise5)

dev.off()



