
#--eseamon
#three_way_anova_rm.R
#
library(car)
library(RCurl)
library(lme4)
library(ez)
library(lattice)
library(ggplot2)


#options(scipen=999)

#-load data
Southern_ID_sumloss <- read.csv(text=getURL("https://raw.githubusercontent.com/erichseamon/dmine_anova/master/PNW_summary_all.csv"), header = TRUE)
Southern_ID_sumloss_all_sum  <- aggregate(loss ~ year + damagecause + county + commodity,  Southern_ID_sumloss, sum)
Southern_ID_count_all_count  <- aggregate(count ~ year + damagecause + county + commodity,  Southern_ID_sumloss, sum)
Southern_ID_sumloss_all_sum <- Southern_ID_sumloss_all_sum[Southern_ID_sumloss_all_sum$loss >= 1, ]


#-Loading all WHEAT claims for the palouse from 1989-2015
palouse_sumloss <- read.csv(text=getURL("https://raw.githubusercontent.com/erichseamon/dmine_anova/master/Palouse_summary_sumloss.csv"), header = TRUE)
palouse_counts <- read.csv(text=getURL("https://raw.githubusercontent.com/erichseamon/dmine_anova/master/Palouse_summary_counts.csv"), header = TRUE)

#box cox 
#bc <- boxcox(loss~damagecause*year*commodity, data=palouse_sumloss_aggregate, lambda = seq(-0.05, 0.1, len = 20))
#boxcox.lambda(bc, method = c("guerrero", "loglik"), lower = -1, upper = 2)


#use a cube transformation on loss for WHEAT claims
Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

Southern_ID_sumloss_all_sum$cube_loss <- Math.cbrt(Southern_ID_sumloss_all_sum$loss)
Southern_ID_count_all_count$cube_counts <- Math.cbrt(Southern_ID_count_all_count$count)

#--aggregate palouse
palouse_sumloss_aggregate <- aggregate(loss ~ damagecause + year + commodity + county, palouse_sumloss, mean)

#-calculate cube loss
palouse_sumloss_aggregate$cube_loss <- Math.cbrt(palouse_sumloss_aggregate$loss)

#-remove zeros
palouse_sumloss_aggregate <- subset(palouse_sumloss_aggregate, loss > 0)

#-use a log transform 

palouse_sumloss_aggregate$log10_loss <- log10(palouse_sumloss_aggregate$loss)
palouse_sumloss_aggregate$log_loss <- log(palouse_sumloss_aggregate$loss)

#-inverse transform
palouse_sumloss_aggregate$inverse_loss <- 1/palouse_sumloss_aggregate$loss

#sq root transformation
palouse_sumloss_aggregate$sqroot_loss <- sqrt(palouse_sumloss_aggregate$loss)

#--scale and center
palouse_sumloss_aggregate$scaled_cube_loss <- scale(palouse_sumloss_aggregate$cube_loss)
palouse_sumloss_aggregate$scaled_inverse_loss <- scale(palouse_sumloss_aggregate$inverse_loss, center = TRUE, scale = TRUE)

#--reduce years to 2001-2015
xxyear <- subset(palouse_sumloss_aggregate, year >= 2001)

ezDesign(xxyear, year, damagecause)
ezDesign(xxyear, county, damagecause)
ezDesign(xxyear, county, commodity)


#--subset to four commodities - Barley, Wheat, Apples, and Dry Peas
xx <- subset(xxyear, commodity == "BARLEY" | commodity == "WHEAT" | commodity == "APPLES" | commodity == "DRY PEAS" | commodity == "CHERRIES")

#--subset to a select set of damage causes - Drought, Heat, Hail, Frost, Freeze, Excessive Moisture, Cold Winter, Cold Weather, and Decline in Price
xxx <- subset(xx, damagecause == "Drought" | damagecause == "Heat" | damagecause == "Hail" | damagecause == "Frost" | damagecause == "Freeze" | damagecause == "Excessive Moisture/Precip/Rain" | damagecause == "Cold Winter" | damagecause == "Cold Wet Weather" | damagecause == "Decline in Price")


#examine missing data
ezDesign(xxx, year, damagecause)
ezDesign(palouse_sumloss_aggregate_cherries, county, year)
ezDesign(xxx, county, damagecause)
ezDesign(xxx, year, commodity)
ezDesign(xxx, county, commodity)

palouse_sumloss_aggregate <- xxx
xxx_wheat <- subset(xxx, commodity == "WHEAT")
xxx_apples <- subset(xxx, commodity == "APPLES")
xxx_cherries <- subset(xxx, commodity == "CHERRIES")
xxx_drypeas <- subset(xxx, commodity == "DRY PEAS")

palouse_sumloss_aggregate_apples <- xxx_apples
palouse_sumloss_aggregate_cherries <- xxx_cherries
palouse_sumloss_aggregate_drypeas <- xxx_drypeas
palouse_sumloss_aggregate_wheat <- xxx_wheat

palouse_sumloss_aggregate_apples <- subset(palouse_sumloss_aggregate_apples, county != "Columbia" & county != "Wasco")
palouse_sumloss_aggregate_apples$county <- factor(palouse_sumloss_aggregate_apples$county)

palouse_sumloss_aggregate_wheat <- subset(palouse_sumloss_aggregate_wheat, county != "Clearwater" & county != "Kootenai")
palouse_sumloss_aggregate_wheat$county <- factor(palouse_sumloss_aggregate_wheat$county)

palouse_sumloss_aggregate_cherries <- subset(palouse_sumloss_aggregate_cherries, county != "Adams" & county != "Union")
palouse_sumloss_aggregate_cherries$county <- factor(palouse_sumloss_aggregate_cherries$county)




#--Fill in missing data
palouse_sumloss_aggregate$year <- as.numeric(palouse_sumloss_aggregate$year)
#psa1 <- subset(palouse_sumloss_aggregate, year >= 2001)
palouse_sumloss_aggregate_apples$year <- as.numeric(palouse_sumloss_aggregate_apples$year)
palouse_sumloss_aggregate_wheat$year <- as.numeric(palouse_sumloss_aggregate_wheat$year)
palouse_sumloss_aggregate_drypeas$year <- as.numeric(palouse_sumloss_aggregate_drypeas$year)
palouse_sumloss_aggregate_cherries$year <- as.numeric(palouse_sumloss_aggregate_cherries$year)



#-turn year into character value
palouse_sumloss_aggregate_apples$year <- as.character(palouse_sumloss_aggregate_apples$year)
palouse_sumloss_aggregate_wheat$year <- as.character(palouse_sumloss_aggregate_wheat$year)
palouse_sumloss_aggregate_drypeas$year <- as.character(palouse_sumloss_aggregate_drypeas$year)
palouse_sumloss_aggregate_cherries$year <- as.character(palouse_sumloss_aggregate_cherries$year)

#psa1$year <- as.character(psa1$year)

#-factor year
#psa1$year <- factor(psa1$year)

palouse_sumloss_aggregate_apples$year <- factor(palouse_sumloss_aggregate_apples$year)
palouse_sumloss_aggregate_wheat$year <- factor(palouse_sumloss_aggregate_wheat$year)
palouse_sumloss_aggregate_drypeas$year <- factor(palouse_sumloss_aggregate_drypeas$year)
palouse_sumloss_aggregate_cherries$year <- factor(palouse_sumloss_aggregate_cherries$year)

palouse_sumloss_aggregate_apples$damagecause <- factor(palouse_sumloss_aggregate_apples$damagecause)
palouse_sumloss_aggregate_wheat$damagecause <- factor(palouse_sumloss_aggregate_wheat$damagecause)
palouse_sumloss_aggregate_drypeas$damagecause <- factor(palouse_sumloss_aggregate_drypeas$damagecause)
palouse_sumloss_aggregate_cherries$damagecause <- factor(palouse_sumloss_aggregate_cherries$damagecause)

palouse_sumloss_aggregate_apples$commodity <- factor(palouse_sumloss_aggregate_apples$commodity)
palouse_sumloss_aggregate_wheat$commodity <- factor(palouse_sumloss_aggregate_wheat$commodity)
palouse_sumloss_aggregate_drypeas$commodity <- factor(palouse_sumloss_aggregate_drypeas$commodity)
palouse_sumloss_aggregate_cherries$commodity <- factor(palouse_sumloss_aggregate_cherries$commodity)

palouse_sumloss_aggregate_apples$county <- factor(palouse_sumloss_aggregate_apples$county)
palouse_sumloss_aggregate_wheat$county <- factor(palouse_sumloss_aggregate_wheat$county)
palouse_sumloss_aggregate_drypeas$county <- factor(palouse_sumloss_aggregate_drypeas$county)
palouse_sumloss_aggregate_cherries$county <- factor(palouse_sumloss_aggregate_cherries$county)


alllevs_apples <- do.call(expand.grid, lapply(palouse_sumloss_aggregate_apples[c("damagecause", "year", "county", "commodity")], levels))

alllevs_wheat <- do.call(expand.grid, lapply(palouse_sumloss_aggregate_wheat[c("damagecause", "year")], levels))
alllevs_drypeas <- do.call(expand.grid, lapply(palouse_sumloss_aggregate_drypeas[c("damagecause", "year")], levels))
alllevs_cherries <- do.call(expand.grid, lapply(palouse_sumloss_aggregate_cherries[c("damagecause", "year")], levels))




alllevs2_apples <- merge(palouse_sumloss_aggregate_apples, alllevs_apples, all.y=TRUE)
alllevs2_apples$loss[is.na(alllevs2_apples$loss)] <- 0
alllevs2_apples$cube_loss[is.na(alllevs2_apples$cube_loss)] <- 0

alllevs2_wheat <- merge(palouse_sumloss_aggregate_wheat, alllevs_wheat, all.y=TRUE)
alllevs2_wheat$loss[is.na(alllevs2_wheat$loss)] <- 0
alllevs2_wheat$cube_loss[is.na(alllevs2_wheat$cube_loss)] <- 0


alllevs3 <- alllevs2
#alllevs3 <- subset(alllevs2, commodity == "WHEAT")

#EDA for apples
#-boxplots of data by cube loss by each of the three factors
boxplot(cube_loss ~ year, palouse_sumloss_aggregate_apples, las = 3)
boxplot(cube_loss ~ commodity, palouse_sumloss_aggregate_apples, las = 3)
boxplot(cube_loss ~ damagecause, palouse_sumloss_aggregate_apples, las = 3)
boxplot(cube_loss ~ county, palouse_sumloss_aggregate_apples, las = 3)

#missing data
ezDesign(palouse_sumloss_aggregate, year, damagecause)
ezDesign(palouse_sumloss_aggregate, year, county)
ezDesign(palouse_sumloss_aggregate, county, damagecause)
ezDesign(palouse_sumloss_aggregate, year, commodity)
ezDesign(palouse_sumloss_aggregate, county, commodity)


#missing data
ezDesign(alllevs3, year, damagecause)
ezDesign(alllevs3, year, county)
ezDesign(alllevs3, county, damagecause)
ezDesign(alllevs3, year, commodity)
ezDesign(alllevs3, county, commodity)

palouse_sumloss_aggregate <- alllevs3

#Testing

#-nortest for normality.  Shapiro Wilks doesnt work on values higher than 5000
library(nortest)
ad.test(palouse_sumloss_aggregate$cube_loss)$p.value

#palouse_sumloss_aggregate_sqroot <- palouse_sumloss_aggregate[-c(7:13)] 
#palouse_sumloss_aggregate_sqroot <- palouse_sumloss_aggregate_sqroot[-c(5)] 


#--Levene Test for homogeneity of variance

leveneTest(cube_loss ~ year, data=palouse_sumloss_aggregate, center = mean)

#---Bartlett Test - homogeneity of variances

bartlett.test(cube_loss ~ year, palouse_sumloss_aggregate)

#--Fligner-Killeen test for homoskedasticity

fligner.test(palouse_sumloss_aggregate$cube_loss, palouse_sumloss_aggregate$year)


#BARLEY

#subset dataset to limit commodities and damage causes 
xx <- subset(palouse_sumloss_aggregate, commodity == "BARLEY")


xxx <- subset(palouse_sumloss_aggregate, damagecause == "Drought" | damagecause == "Heat" | damagecause == "Hail" | damagecause == "Frost" | damagecause == "Freeze" | damagecause == "Excessive Moisture/Precip/Rain" | damagecause == "Cold Winter" | damagecause == "Cold Wet Weather" | damagecause == "Decline in Price")

#xyplot
xyplot(cube_loss ~ year|county, groups=damagecause, type= c("p", "r"), scales=list(x=list(rot=90)), data=xxx)



fit <- ezANOVA(data=xxx, 
        dv=loss, 
        wid=.(county), 
        within=.(year), 
        between = .(damagecause),
        type = 3) 


#fit <- aov(cube_loss~year*county*damagecause, data=xxx)


#linear mixed model
#model.a <- lmer(loss ~ year + damagecause + (1 | county/damagecause), data=xxx)
#summary(model.a)
#coefplot2(model.a)

#require(multcomp)
#summary(glht(model.a, linfct=mcp(year="Tukey")), test = adjusted(type = "bonferroni"))

model.b <- lmer(cube_loss ~ year + damagecause + (1 | county), data=xxx)
summary(model.b)
coefplot2(model.b)

require(multcomp)
summary(glht(model.b, linfct=mcp(year="Tukey")), test = adjusted(type = "bonferroni"))




#--WHEAT

xx <- subset(palouse_sumloss_aggregate, commodity == "WHEAT")
xxx <- subset(xx, damagecause == "Drought" | damagecause == "Heat" | damagecause == "Hail" | damagecause == "Frost" | damagecause == "Freeze" | damagecause == "Excessive Moisture/Precip/Rain" | damagecause == "Cold Winter" | damagecause == "Cold Wet Weather" | damagecause == "Decline in Price")

#xyplot
xyplot(cube_loss ~ year|county, groups=damagecause, type= c("p", "r"), scales=list(x=list(rot=90)), data=alllevs2_wheat)

#linear mixed model
#model.a <- lmer(cube_loss ~ year + (1 | county/damagecause), data=xxx)
#summary(model.a)
#coefplot2(model.a)

#require(multcomp)
#summary(glht(model.a, linfct=mcp(year="Tukey")), test = adjusted(type = "bonferroni"))

model.b <- lmer(cube_loss ~ year + damagecause + (1 | county), data=alllevs2_wheat)
summary(model.b)
coefplot2(model.b)

require(multcomp)
summary(glht(model.b, linfct=mcp(year="Tukey")), test = adjusted(type = "bonferroni"))


#--APPLES


#subset dataset to limit commodities and damage causes 
xx <- subset(palouse_sumloss_aggregate, commodity == "APPLES")
xxx <- subset(xx, damagecause == "Drought" | damagecause == "Heat" | damagecause == "Hail" | damagecause == "Frost" | damagecause == "Freeze" | damagecause == "Excessive Moisture/Precip/Rain" | damagecause == "Cold Winter" | damagecause == "Cold Wet Weather" | damagecause == "Decline in Price")

#xyplot
xyplot(cube_loss ~ year|county, groups=damagecause, type= c("p", "r"), scales=list(x=list(rot=90)), data=alllevs2_apples)

#linear mixed model
#model.a <- lmer(cube_loss ~ year + (1 | county/damagecause), data=xxx)
#summary(model.a)

#require(multcomp)
#summary(glht(model.a, linfct=mcp(year="Tukey")), test = adjusted(type = "bonferroni"))

model.b <- lmer(cube_loss ~ year + damagecause + (1 | county), data=alllevs2_apples)
summary(model.b)
coefplot2(model.b)

require(multcomp)
summary(glht(model.b, linfct=mcp(year="Tukey")), test = adjusted(type = "bonferroni"))

#--DRY PEAS


#subset dataset to limit commodities and damage causes 
xx <- subset(palouse_sumloss_aggregate, commodity == "DRY PEAS")
xxx <- subset(xx, damagecause == "Drought" | damagecause == "Heat" | damagecause == "Hail" | damagecause == "Frost" | damagecause == "Freeze" | damagecause == "Excessive Moisture/Precip/Rain" | damagecause == "Cold Winter" | damagecause == "Cold Wet Weather" | damagecause == "Decline in Price")

#xyplot
xyplot(cube_loss ~ year|county, groups=damagecause, type= c("p", "r"), scales=list(x=list(rot=90)), data=xxx)

#linear mixed model
#model.a <- lmer(cube_loss ~ year + (1 | county/damagecause), data=xxx)
#summary(model.a)

#require(multcomp)
#summary(glht(model.a, linfct=mcp(year="Tukey")), test = adjusted(type = "bonferroni"))

model.b <- lmer(cube_loss ~ year + damagecause + (1 | county), data=xxx)
summary(model.b)
coefplot2(model.b)

require(multcomp)
summary(glht(model.b, linfct=mcp(year="Tukey")), test = adjusted(type = "bonferroni"))





#reduce counties to three.  So xxxx has 6 damage causes, one commodity, three counties, and 27 years
#xxxx <- subset(xxx, county == "Whitman" | county == "Lincoln" | county == "Adams")

#ggplot(xxxx,aes(year, cube_loss, group=interaction(county,damagecause), col=county, shape=damagecause )) + 
#  facet_grid(~damagecause) +
#  geom_line(aes(y=cube_loss, lty=commodity), size=0.8) +
#  geom_point(alpha = 0.3) + 
#  geom_hline(yintercept=0, linetype="dashed") +
#  theme_bw()


#AOV using ezANOVA.  Use only damagecause as between because it takes a long 
#doesnt work

#AOV with ezANOVA.  Doesnt work due to missing data

ezANOVA(data=xxx, 
        dv=cube_loss, 
        wid=.(county), 
        within=.(year), 
        between = .(damagecause),
        type = 3) 

#Alternative AOV

fit = aov(sqroot_loss~year + Error(county/year), palouse_sumloss_aggregate)
summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests

