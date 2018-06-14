
#--eseamon
#three_way_anova_rm.R
#
library(car)
library(RCurl)
library(lme4)
library(ez)
library(lattice)
library(ggplot2)


options(scipen=999)

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


#-turn year into character value
palouse_sumloss_aggregate$year <- as.character(palouse_sumloss_aggregate$year)

#-factor year
palouse_sumloss_aggregate$year <- factor(palouse_sumloss_aggregate$year)

#EDA
#-boxplots of data by cube loss by each of the three factors
boxplot(cube_loss ~ year, palouse_sumloss_aggregate, las = 3)
boxplot(cube_loss ~ commodity, palouse_sumloss_aggregate, las = 3)
boxplot(cube_loss ~ damagecause, palouse_sumloss_aggregate, las = 3)
boxplot(cube_loss ~ county, palouse_sumloss_aggregate, las = 3)

#missing data
ezDesign(palouse_sumloss_aggregate, year, damagecause)
ezDesign(palouse_sumloss_aggregate, year, county)
ezDesign(palouse_sumloss_aggregate, county, damagecause)
ezDesign(palouse_sumloss_aggregate, year, commodity)
ezDesign(palouse_sumloss_aggregate, county, commodity)

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


#Modeling

#subset dataset to limit commodities and damage causes 
xx <- subset(palouse_sumloss_aggregate, commodity == "BARLEY")
xxx <- subset(xx, damagecause == "Drought" | damagecause == "Heat" | damagecause == "Hail" | damagecause == "Frost" | damagecause == "Freeze" | damagecause == "Excessive Moisture/Precip/Rain" | damagecause == "Cold Winter" | damagecause == "Cold Wet Weather")

#xyplot
xyplot(cube_loss ~ year|county, groups=damagecause, type= c("p", "r"), scales=list(x=list(rot=90)), data=xxx)

#linear mixed model
model.a <- lmer(cube_loss ~ year + (1 | county/damagecause), data=xxx)


require(multcomp)
summary(glht(model.a, linfct=mcp(year="Tukey")), test = adjusted(type = "bonferroni"))


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

