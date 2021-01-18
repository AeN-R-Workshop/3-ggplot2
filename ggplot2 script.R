rm(list = ls()) # clears the workspace

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

str(penguins)

penguins<-na.omit(penguins)

default_colors<-c("#F8766D", "#00BA38", "#619CFF")

###### fig1

pdf("fig1.pdf", width = 3.5, height = 3.5, paper = "special") # creates a file called fig1.pdf in your working directory
                                                                                     
ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm))+ # plotting writes to file
  labs(title = "figure 1")+
  geom_point()

dev.off() # closes the graphics device

###### fig2

pdf("fig2.pdf", width = 7, height = 6, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,
                            y=bill_depth_mm,
                            color=species,
                            shape=sex,
                            size=body_mass_g))+
  labs(title = "figure 2")+
  geom_point()

dev.off()

###### fig3

pdf("fig3.pdf", width = 4.5, height = 3.5, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,
                            y=bill_depth_mm,color=species,
                            linetype=island))+
  labs(title = "figure 3")+
  geom_smooth(method = 'lm', se=F)

dev.off()

###### fig4

pdf("fig4.pdf", width = 4.5, height = 3.5, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm,color=species,linetype=island))+
  labs(title = "figure 4")+
  geom_smooth(method = 'lm', se=F)+
  geom_point(penguins, mapping=aes(shape=island)) # shape could also go directly in the ggplot( aes() ), 
                                                  # then there is no need to supply data and mapping

dev.off()

###### fig5

pdf("fig5.pdf", width = 3.8, height = 3.5, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm))+
  labs(title = "figure 5")+
  geom_point()

dev.off()

###### fig6

pdf("fig6.pdf", width = 3.8, height = 3.5, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm))+
  geom_point()+
  labs(title = "figure 6")+
  theme(axis.text.x = element_text(colour="black",size=13),
        axis.text.y = element_text(colour="black",size=13),  
        axis.title.x = element_text(colour="black",size=13),
        axis.title.y = element_text(colour="black",size=13),
        panel.grid = element_blank(), # removes the grid
        panel.background = element_rect(fill = "white", colour = "black")) # makes the background white, and draws a border around the panel area

dev.off()

###### fig 7

pdf("fig7.pdf", width = 4.5, height = 3.5, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm,color=species,linetype=island))+
  geom_smooth(method = 'lm', se=F)+
  labs(title = "figure 7")+
  geom_point(penguins, mapping=aes(shape=island))

dev.off()

###### fig 8

pdf("fig8.pdf", width = 4.5, height = 3.5, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm,color=species,linetype=island))+
  geom_smooth(method = 'lm', se=F)+
  labs(title = "figure 8")+
  scale_colour_manual(values = c("brown","darkgray","orange"))+ # custom colors
  scale_linetype_manual(values = c("dotted","solid","dashed"))+ # custom lines
  scale_shape_manual(values = c(21,22,23))+ # custom shapes
  geom_point(penguins, mapping=aes(shape=island))

dev.off()

###### fig 9

pdf("fig9.pdf", width = 4.5, height = 3.5, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm,color=species,linetype=island))+
  geom_smooth(method = 'lm', se=F)+
  geom_point(penguins, mapping=aes(shape=island))+
  labs(tag="a)", 
       title = "figure 9", 
       y="Bill depth (mm)", 
       x="Bill length (mm)", 
       color="Species", 
       linetype="Island", 
       shape="Island")

dev.off()

###### fig 10

pdf("fig10.pdf", width = 4.5, height = 3.5, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm,color=species,linetype=island))+
  geom_smooth(method = 'lm', se=F)+
  geom_point(penguins, mapping=aes(shape=island))+
  labs(title = "figure 10", y="Bill depth (mm)", x="Bill length (mm)")+
  scale_x_continuous(breaks = c(35,40,45,50,55,60))+
  scale_y_continuous(breaks = c(14,15,16,17,18,19,20,21))

dev.off()

###### fig 11

pdf("fig11.pdf", width = 4.5, height = 3.5, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm,color=species,linetype=island,shape=island))+
  geom_smooth(method = 'lm', se=F)+
  geom_point()+ 
  labs(title = "figure 11", 
       y="Bill depth (mm)", 
       x="Bill length (mm)",
       color="Species",
       shape="Island",
       linetype="Island")+
  scale_colour_manual(values = default_colors, labels=c("P. adeliae","P. antarcticus","P. papua"))+ # change the label names in the legend
  guides(linetype=guide_legend(override.aes = list(color="magenta")), # the size of linetype and shape cannot be changed separately as they are merged in the legend
         colour=guide_legend(override.aes = list(size=3)))

#  guides(color=F,linetype=F,shape=F) # turn off guides
  
dev.off()


###### fig 12

pdf("fig12.pdf", width = 3.5, height = 3.5, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm,color=species,shape=sex,size=body_mass_g))+
  labs(title = "figure 12")+
  geom_point()

dev.off()

###### fig13

pdf("fig13.pdf", width = 3.2, height = 3.5, paper = "special")

ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm,fill=species,shape=sex,size=body_mass_g,alpha=0.5))+
  geom_point(color="black")+
  scale_shape_manual(values=c(21,24))+
  scale_size_continuous(range = c(0.5, 3), breaks = c(3000,4500,6000))+
  scale_y_continuous(breaks = c(14,16,18,20))+
  labs(x="Bill length (mm)", y= "Bill depth (mm)", fill="Species", shape="Sex", size= "Body mass (g)", title = "figure 13")+
  guides(alpha=F, fill=guide_legend(override.aes = list(color=default_colors)))+
  theme(axis.text.x = element_text(colour="black",size=10),
        axis.text.y = element_text(colour="black",size=10),  
        axis.title.x = element_text(colour="black",size=11),
        axis.title.y = element_text(colour="black",size=11),
        legend.position = "bottom",
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        legend.key.size = unit(10,"point"),
        legend.box = "vertical",
        legend.spacing.y = unit(-.25,"cm"),
        legend.margin=margin(0,5,8,5), # for margins think trouble, top right bottom left
        legend.box.margin=margin(-10,10,-3,10), # for margins think trouble, top right bottom left
        legend.box.spacing = unit(0.5,"cm"),
        plot.margin = margin(2,2,0,0))  # for margins think trouble, top right bottom left

dev.off()

###### fig 14

fig14a<-ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm,color=species,linetype=island))+
  geom_smooth(method = 'lm', se=F)+
  labs(tag="a)", x="Bill length (mm)", y= "Bill depth (mm)", color="Species", linetype="Island", shape="Island", title = "figure 14a")+
  scale_colour_manual(values = c("brown","darkgrey","orange"))+
  scale_linetype_manual(values = c("dotted","solid","dashed"))+
  scale_shape_manual(values = c(21,22,23))+
  geom_point(penguins, mapping=aes(shape=island))+
  scale_y_continuous(breaks = c(14,16,18,20))+
  theme(axis.text.x = element_text(colour="black",size=7),
        axis.text.y = element_text(colour="black",size=7),  
        axis.title.x = element_text(colour="black",size=8),
        axis.title.y = element_text(colour="black",size=8),
        plot.title = element_text(colour = "black", size = 8),
        plot.tag = element_text(size = 6),
        plot.tag.position = c(0.04,0.98),               # x and y
        plot.margin = margin(2,2,0,0), 
        legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.title = element_text(size = 7),
        legend.key.size = unit(10,"point"),
        legend.box = "vertical",
        legend.spacing.y = unit(-.25,"cm"),
        legend.margin=margin(0,5,8,5), 
        legend.box.margin=margin(-10,10,-3,10),
        legend.box.spacing = unit(0.5,"cm"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"))

fig14b<-ggplot(data = penguins, aes(x=bill_length_mm,y=bill_depth_mm,color=species,shape=sex,size=body_mass_g))+
  geom_point()+
  scale_size_continuous(range = c(0.25, 1.5), breaks = c(3000,4500,6000))+
  labs(tag="b)", x="Bill length (mm)", y= "Bill depth (mm)", color="Species", shape="Sex", size= "Body mass (g)", title = "figure 14b")+
  scale_y_continuous(breaks = c(14,16,18,20))+
  theme(axis.text.x = element_text(colour="black",size=7),
        axis.text.y = element_text(colour="black",size=7),  
        axis.title.x = element_text(colour="black",size=8),
        axis.title.y = element_text(colour="black",size=8),
        plot.title = element_text(colour = "black", size = 8),
        plot.tag = element_text(size = 6),
        plot.tag.position = c(0.04,0.98), # x and y
        plot.margin = margin(2,2,0,0), 
        legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.title = element_text(size = 7),
        legend.key.size = unit(10,"point"),
        legend.box = "vertical",
        legend.spacing.y = unit(-.25,"cm"),
        legend.margin=margin(0,5,8,5), 
        legend.box.margin=margin(-10,10,-3,10),
        legend.box.spacing = unit(0.5,"cm"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"))

pdf("fig14.pdf", width = 5, height = 3, paper = "special",onefile = FALSE,family = "Helvetica")

# function ggarrange() is loaded from the egg package state the placement of figures with 
# the ncol= and nrow= arguments, labels= creates new tags
# gpar() function is loaded from the grid package, state font= and cex= (i.e. size) of the labels

ggarrange(fig14a,fig14b, ncol=2, nrow=1 ,labels = c("a)","b)"),
          label.args = list(gp = gpar(font = 1, cex = .8)))   

dev.off()

#############################

#### exercises

#############################

##############################

#### exercise 1, plot the estimated Pc from the model fit over a sequence of PAR values

##############################

library(phytotools)

#### Growth curve: PE dataset example #### from Package 'phytotools'

PAR <- c(5,10,20,50,100,150,250,400,800,1200) # photosynthetic active radiation: umol m-2 s-1
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

#### plot the same data with ggplot

######################

# you will have to create create two data frames using the data.frame() function, one with the raw data that shuld be plotted as points geom_point
# and the other with the calculated values based on the model estimates in myfit

PE_data<-data.frame(PAR=PAR,Pc=Pc) # creates a data frame of the Pc and Par data

# to include superscript and subscripts you can use the bqoute() function in labs()
#       x= bquote('PAR,'~(µmol~m^-2~s^-1)), 
#       y= bquote('Pc,'~(mg~C~m^-3~hr^-1)))+ 

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

# the inverse of log() is exp(), you will have to use exp() to transform the log data back to the original scale  

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

# calculate the mean weight of chickens for each diet at each time point e.g. by dplyr piping 
# plot the means as points geom_point()

ChickWeight_mean <- ChickWeight %>%
  group_by(Diet,Time) %>%
  summarise(weight=mean(weight))

############################

### exercise 4, plot averages of penguin weights for each species and sex in a bar graph geom_bar

############################

# calculate averages of penguin weights for each species and sex by using dplyr pipes
# or by using base r commands using split() e.g. s<-split(penguins,interaction(penguins$species,penguins$sex))
# then calculate the average wight in each data frame in the list of data frames 
# e.g.body_mass_g=unlist(lapply(1:6, function(x) mean(s[[x]]$body_mass_g)))

head(penguins)

s<-split(penguins,interaction(penguins$species,penguins$sex))

new_penguins<-data.frame(body_mass_g=unlist(lapply(1:6, function(x) mean(s[[x]]$body_mass_g))),
                         sex=unlist(lapply(1:6, function(x) (s[[x]]$sex[1]))),
                         species=unlist(lapply(1:6, function(x) (s[[x]]$species[1]))))

# use the geom_bar() function to create a bar graph, you will have to 
# use the arguments stat= and position=


###############################

##### exercise 5, plot the egg laying date of the three penguin species for each year in a stacked bar graph

###############################

# use the penguins_raw data frame

head(penguins_raw)

# create a column of year, so you can separate the dates for the three different years and plot these as facets 

penguins_raw$year<-format(as.Date(penguins_raw$`Date Egg`, format="%d/%m/%Y"),"%Y") 

# use the geom_bar() function to create a bar graph, you will have to use the arguments stat= and position=
# use the facet_grid() function to create panels of each year, arguments scales= will help  
# you are going to need to specify grouping of bars, it should be interaction(Species,year,`Date Egg`)

