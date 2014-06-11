######################################################
# Code for analysis performed in "Crowdsourcing pollinator attractiveness" 
# by Bahlai and Landis
######################################################

#bring data in
#when prompted, choose pollinatorg_goole.txt from dropbox/Pollinator_Services
pollinators<-read.table("http://figshare.com/download/file/1529789", header=TRUE, na.strings="")



#####################################
#  variables and descriptions
#
#Plant- plant species on which observations were made	
#
#BloomPeriod - bloom period of plant species, as classified by Tuell et al 2008
#
#BloomOrder- As BloomPeriod, but categorized as A, B, C, to enable alphabetical 
#		sorting for creating figures
#
#ImagesSearched	- Total number of search results evaluated in order to meet criteria
#		(see methods for criteria for evaluating search results)
#
#CriteriaMet- total number of images meeting criteria that were evaluated, before
#		reaching 200 search results	
#
#Totinsects- total number of images meeting criteria where 'bee-like' insects were visible
#	
#GoogleApis- total number of images meeting criteria where Apis bees were visible
#	
#GoogleNonApis	- total number of images meeting criteria where non-Apis bees were visible
#
#GoogleBees- total number of images meeting critertia where any type of bee was visible
#		(GoogleApis+GoogleNonApis)
#	
#GoogleSyrphids	- total number of images meeting criteria where syrphid flies were visible
#
#JuliannaNonApis- Non-apis bee counts by plant from Tuell et al 2008
#
#JuliannaRichness- Non-apis bee counts by plant from Tuell	
#
#JuliannaApis	- Apis bee counts by plant from Tuell
#
#JuliannaBees -total bees observed by Tuell (JuliannaApis+JuliannaNonApis)
#	
#AnnaSyrphid - syrphid counts from Fielder 2006 thesis by plant
#
#####################################


summary(pollinators)
attach(pollinators)
#because some searches did not have 30 images meeting criteria, search results
#need to be scaled to account for lower search sucess
# thus, multiply each count by 30/CriteriaMet


Totinsects<-Totinsects*30/CriteriaMet
GoogleApis<-GoogleApis*30/CriteriaMet
GoogleNonApis<-GoogleNonApis*30/CriteriaMet
GoogleBees<-GoogleBees*30/CriteriaMet
GoogleSyrphids<-GoogleSyrphids*30/CriteriaMet

#approach is to  model field observations using search engine observations as predictors.

#Because  net bee abundance varies dramatically by bloom season (Tuell et al 2008), also 
#include this categorical variable in models 

#Field observations are counts, so models with Poisson or Negative binomial error structure 
#are most appropriate- first try Poisson, if residual deviance/resid df >1, switch to 
#negative binomial model

#When multiple model structures are compared, use AIC to determine best performing model
#general model structure is 
#	Field_obs~search_engine_obs+BloomPeriod+search_engine_obs*BloomPeriod


#Because some of the searches were less sucessful than others, we can place less emphasis 
#on these potentially lower quality results by including a weighting factor in the model
#proportional to the inverse of the number of images that had to be searched to obtain
#the data, in the form of weights=1/ImagesSearched 

#####################################

#model total bees using the Tuell 2008 data

#####################################
library(pscl)

bees.p=glm(JuliannaBees~GoogleBees+BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(bees.p)
#no evidence of overdispersion. Use Poisson error structure

# test for interaction between bloom period and observations
bees.p.int=glm(JuliannaBees~GoogleBees*BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(bees.p.int)
#AIC goes up, so including interaction is not supported. bees.p is best model

#try simpler models
bees.p.noBP=glm(JuliannaBees~GoogleBees, weights=(1/ImagesSearched), family="poisson")
summary(bees.p.noBP)
bees.p.noGoogle=glm(JuliannaBees~BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(bees.p.noGoogle)

#bees.p is best model
pR2(bees.p)

#####################################

#model Apis bees using the Tuell 2008 data

#####################################

apis.p=glm(JuliannaApis~GoogleApis+BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(apis.p)
#no evidence of overdispersion. Use Poisson error structure

apis.p.int=glm(JuliannaApis~GoogleApis*BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(apis.p.int)
#AIC goes up, so including interaction is not supported.

#try simpler models
apis.p.noBP=glm(JuliannaApis~GoogleApis, weights=(1/ImagesSearched), family="poisson")
summary(apis.p.noBP)
apis.p.noGoogle=glm(JuliannaApis~BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(apis.p.noGoogle)

# apis.p.noGoogle is best model
#no significant effects observed with any model structure.
pR2(apis.p.noGoogle)

#####################################

#model Non-Apis bees using the Tuell 2008 data

#####################################

nonapis.p=glm(JuliannaNonApis~GoogleNonApis+BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(nonapis.p)
#no evidence of overdispersion. Use Poisson error structure

nonapis.p.int=glm(JuliannaNonApis~GoogleNonApis*BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(nonapis.p.int)
#AIC stays roughly the same with this more complex model structure,
#so including interaction is not supported. 

#try simpler models
nonapis.p.noBP=glm(JuliannaNonApis~GoogleNonApis, weights=(1/ImagesSearched), family="poisson")
summary(nonapis.p.noBP)
nonapis.p.noGoogle=glm(JuliannaNonApis~BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(nonapis.p.noGoogle)

#nonapis.p is best model
pR2(nonapis.p)

#####################################

#model syrphids using the Fielder 2006 data

#####################################

syrphid.p=glm(AnnaSyrphid~GoogleSyrphids+BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(syrphid.p)
#no evidence of overdispersion. Use Poisson error structure

syrphid.p.int=glm(AnnaSyrphid~GoogleSyrphids*BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(syrphid.p.int)
#AIC goes up, so including interaction is not supported. 

#try simpler models
syrphid.p.noBP=glm(AnnaSyrphid~GoogleSyrphids, weights=(1/ImagesSearched), family="poisson")
summary(syrphid.p.noBP)
syrphid.p.noGoogle=glm(AnnaSyrphid~BloomPeriod, weights=(1/ImagesSearched), family="poisson")
summary(syrphid.p.noGoogle)

#syrphid.p.noBP is best model
#no significant effects observed with any model structure.
pR2(syrphid.p.noBP)


#####################################

#plots

#####################################


#create a consistent colour palette for all plots
# palette1 is for bloom order
palette1<-c('royalblue3', 'khaki4', 'indianred2') 
# palette2 is for bloom period (alphabetical order)
palette2<-c('royalblue3', 'indianred2','khaki4')

#boxplots of bee data. No plots of syrphids because there was no bloom season effect
par(mfrow=c(2,2))
boxplot(JuliannaApis~BloomOrder, names=c("Early", "Middle", "Late"), col=palette1, ylab="Number of insects observed", main="A  Field observations- Apis bees")
boxplot(JuliannaNonApis~BloomOrder, names=c("Early", "Middle", "Late"),col=palette1, main="B  Field observations- nonApis bees")
boxplot(GoogleApis~BloomOrder, names=c("Early", "Middle", "Late"), col=palette1, ylab="Number of images with insects", xlab="Bloom season", main="C  Search results- Apis bees")
boxplot(GoogleNonApis~BloomOrder, names=c("Early", "Middle", "Late"), col=palette1,xlab="Bloom season", main="D  Search results- nonApis bees")


#plots of best model
#only non-apis bees model had significant regression parameters on its own
#create custom functions to represent best model
#need to create three separate functions because bloom period was significant but
#didn't interact with search result observations in best model
#use regression output from nonapis.p model computed above

library(ggplot2)
nonapis.early.func<-function(x){exp(0.58045+0.10082*x)}
nonapis.middle.func<-function(x){exp(0.58045+0.10082*x +1.56090)}
nonapis.late.func<-function(x){exp(0.58045+0.10082*x+2.91198)}


bee.plot<-ggplot(pollinators, aes(GoogleNonApis, JuliannaNonApis, color=factor(BloomPeriod), shape=factor(BloomPeriod)))+geom_point(colour="black", size = 4)+geom_point(size=2.5)+stat_function(fun=nonapis.early.func, colour="royalblue3", size=1)+stat_function(fun=nonapis.middle.func, colour="khaki4", size=1)+stat_function(fun=nonapis.late.func, colour="indianred2", size=1)+xlab("Images with bees")+ylab("Bees observed in field")+theme_bw()+ scale_colour_manual(name="Bloom\nSeason", breaks=c("Early","Middle","Late"),values=palette2) 


bee.plot+scale_shape_discrete(name="Bloom\nSeason", breaks=c("Early","Middle","Late"))+ theme(legend.key = element_blank())