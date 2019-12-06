
#--eseamon
#three_way_anova_rm.R
#
library(car)
library(RCurl)
library(lme4)
library(ez)
library(lattice)
library(ggplot2)
library(coefplot2)


#options(scipen=999)

#-load data
Southern_ID_sumloss <- read.csv(text=getURL("https://raw.githubusercontent.com/erichseamon/dmine_anova/master/PNW_summary_all.csv"), header = TRUE)
Southern_ID_sumloss_all_sum  <- aggregate(loss ~ year + damagecause + county + commodity,  Southern_ID_sumloss, sum)
Southern_ID_count_all_count  <- aggregate(count ~ year + damagecause + county + commodity,  Southern_ID_sumloss, sum)
Southern_ID_sumloss_all_sum <- Southern_ID_sumloss_all_sum[Southern_ID_sumloss_all_sum$loss >= 1, ]


#-Loading all WHEAT claims for the palouse from 1989-2015
palouse_sumloss <- read.csv(text=getURL("https://raw.githubusercontent.com/erichseamon/dmine_anova/master/Palouse_summary_sumloss.csv"), header = TRUE)
palouse_counts <- read.csv(text=getURL("https://raw.githubusercontent.com/erichseamon/dmine_anova/master/Palouse_summary_counts.csv"), header = TRUE)

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

#missing data review of data from 2001-2015 for all commodities and all damage causes
#ezDesign(xxyear, year, damagecause)
#ezDesign(xxyear, county, damagecause)
#ezDesign(xxyear, county, commodity)

#Now lets subset by the four main commodities of interest.
#--subset to four commodities - Barley, Wheat, Apples, and Dry Peas
xx <- subset(xxyear, commodity == "BARLEY" | commodity == "WHEAT" | commodity == "APPLES" | commodity == "DRY PEAS" | commodity == "CHERRIES")

#--subset to a select set of damage causes - Drought, Heat, Hail, Frost, Freeze, Excessive Moisture, Cold Winter, Cold Weather, and Decline in Price
xxx <- subset(xx, damagecause == "Drought" | damagecause == "Heat" | damagecause == "Hail" | damagecause == "Frost" | damagecause == "Freeze" | damagecause == "Excessive Moisture/Precip/Rain" | damagecause == "Cold Winter" | damagecause == "Cold Wet Weather" | damagecause == "Decline in Price")


#examine missing data after narrowing commodities and damage causes to the most relevant
#ezDesign(xxx, year, damagecause)
#ezDesign(xxx, county, year)
#ezDesign(xxx, county, damagecause)
#ezDesign(xxx, year, commodity)
#ezDesign(xxx, county, commodity)

#palouse_sumloss_aggregate <- xxx

#now lets divide the data into four different model files that are commodity specific.
xxx_wheat <- subset(xxx, commodity == "WHEAT")
xxx_apples <- subset(xxx, commodity == "APPLES")
xxx_cherries <- subset(xxx, commodity == "CHERRIES")
xxx_drypeas <- subset(xxx, commodity == "DRY PEAS")

palouse_sumloss_aggregate_apples <- xxx_apples
palouse_sumloss_aggregate_cherries <- xxx_cherries
palouse_sumloss_aggregate_drypeas <- xxx_drypeas
palouse_sumloss_aggregate_wheat <- xxx_wheat

#-remove counties for each commodity file that have considerable missing data.  
#the reasoning is that those counties that have very little of the commodity in question
#may be inappropriate to fill in data as zero.  While there are sporadic commodity loss
#for these counties, there is a high liklihood that this commodity may NOT be grown
#for the missing years, vs just no commodity loss claims.  So we remove these select
#counties given the EzDesign data review of each commodity file

palouse_sumloss_aggregate_apples <- subset(palouse_sumloss_aggregate_apples, county != "Columbia" & county != "Wasco")
palouse_sumloss_aggregate_apples$county <- factor(palouse_sumloss_aggregate_apples$county)

palouse_sumloss_aggregate_wheat <- subset(palouse_sumloss_aggregate_wheat, county != "Kootenai")
palouse_sumloss_aggregate_wheat$county <- factor(palouse_sumloss_aggregate_wheat$county)

palouse_sumloss_aggregate_cherries <- subset(palouse_sumloss_aggregate_cherries, county != "Adams" & county != "Union")
palouse_sumloss_aggregate_cherries$county <- factor(palouse_sumloss_aggregate_cherries$county)

palouse_sumloss_aggregate_drypeas <- subset(palouse_sumloss_aggregate_drypeas, county != "Douglas" & county != "Gilliam" & county != "Adams")
palouse_sumloss_aggregate_drypeas$county <- factor(palouse_sumloss_aggregate_drypeas$county)


#EDA for apples
#-boxplots of data by cube loss by each of the three factors
boxplot(cube_loss ~ year, palouse_sumloss_aggregate_apples, las = 3)
boxplot(cube_loss ~ commodity, palouse_sumloss_aggregate_apples, las = 3)
boxplot(cube_loss ~ damagecause, palouse_sumloss_aggregate_apples, las = 3)
boxplot(cube_loss ~ county, palouse_sumloss_aggregate_apples, las = 3)

#EDA for wheat
#-boxplots of data by cube loss by each of the three factors
boxplot(cube_loss ~ year, palouse_sumloss_aggregate_wheat, las = 3)
boxplot(cube_loss ~ commodity, palouse_sumloss_aggregate_wheat, las = 3)
boxplot(cube_loss ~ damagecause, palouse_sumloss_aggregate_wheat, las = 3)
boxplot(cube_loss ~ county, palouse_sumloss_aggregate_wheat, las = 3)

#EDA for cherries
#-boxplots of data by cube loss by each of the three factors
boxplot(log10_loss ~ year, palouse_sumloss_aggregate_cherries, las = 3)
boxplot(cube_loss ~ commodity, palouse_sumloss_aggregate_cherries, las = 3)
boxplot(cube_loss ~ damagecause, palouse_sumloss_aggregate_cherries, las = 3)
boxplot(cube_loss ~ county, palouse_sumloss_aggregate_cherries, las = 3)

#EDA for dry peas
#-boxplots of data by cube loss by each of the three factors
boxplot(cube_loss ~ year, palouse_sumloss_aggregate_drypeas, las = 3)
boxplot(cube_loss ~ commodity, palouse_sumloss_aggregate_drypeas, las = 3)
boxplot(cube_loss ~ damagecause, palouse_sumloss_aggregate_drypeas, las = 3)
boxplot(cube_loss ~ county, palouse_sumloss_aggregate_drypeas, las = 3)


#examine missing data after narrowing commodities and damage causes to the most relevant
ezDesign(palouse_sumloss_aggregate_wheat, year, damagecause)
ezDesign(palouse_sumloss_aggregate_wheat, county, year)
ezDesign(palouse_sumloss_aggregate_wheat, county, damagecause)
ezDesign(palouse_sumloss_aggregate_wheat, year, commodity)
ezDesign(palouse_sumloss_aggregate_wheat, county, commodity)

#examine missing data after narrowing commodities and damage causes to the most relevant
ezDesign(palouse_sumloss_aggregate_apples, year, damagecause)
ezDesign(palouse_sumloss_aggregate_apples, county, year)
ezDesign(palouse_sumloss_aggregate_apples, county, damagecause)
ezDesign(palouse_sumloss_aggregate_apples, year, commodity)
ezDesign(palouse_sumloss_aggregate_apples, county, commodity)

#examine missing data after narrowing commodities and damage causes to the most relevant
ezDesign(palouse_sumloss_aggregate_cherries, year, damagecause)
ezDesign(palouse_sumloss_aggregate_cherries, county, year)
ezDesign(palouse_sumloss_aggregate_cherries, county, damagecause)
ezDesign(palouse_sumloss_aggregate_cherries, year, commodity)
ezDesign(palouse_sumloss_aggregate_cherries, county, commodity)

#examine missing data after narrowing commodities and damage causes to the most relevant
ezDesign(palouse_sumloss_aggregate_drypeas, year, damagecause)
ezDesign(palouse_sumloss_aggregate_drypeas, county, year)
ezDesign(palouse_sumloss_aggregate_drypeas, county, damagecause)
ezDesign(palouse_sumloss_aggregate_drypeas, year, commodity)
ezDesign(palouse_sumloss_aggregate_drypeas, county, commodity)

#--Prepare and Fill in missing data
palouse_sumloss_aggregate$year <- as.numeric(palouse_sumloss_aggregate$year)
palouse_sumloss_aggregate_apples$year <- as.numeric(palouse_sumloss_aggregate_apples$year)
palouse_sumloss_aggregate_wheat$year <- as.numeric(palouse_sumloss_aggregate_wheat$year)
palouse_sumloss_aggregate_drypeas$year <- as.numeric(palouse_sumloss_aggregate_drypeas$year)
palouse_sumloss_aggregate_cherries$year <- as.numeric(palouse_sumloss_aggregate_cherries$year)

#-turn year into character value
palouse_sumloss_aggregate_apples$year <- as.character(palouse_sumloss_aggregate_apples$year)
palouse_sumloss_aggregate_wheat$year <- as.character(palouse_sumloss_aggregate_wheat$year)
palouse_sumloss_aggregate_drypeas$year <- as.character(palouse_sumloss_aggregate_drypeas$year)
palouse_sumloss_aggregate_cherries$year <- as.character(palouse_sumloss_aggregate_cherries$year)

#-factor all group variables for each of the four commodities
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

#--create rows for missing data
alllevs_apples <- do.call(expand.grid, lapply(palouse_sumloss_aggregate_apples[c("damagecause", "year", "county", "commodity")], levels))
alllevs_wheat <- do.call(expand.grid, lapply(palouse_sumloss_aggregate_wheat[c("damagecause", "year", "county", "commodity")], levels))
alllevs_drypeas <- do.call(expand.grid, lapply(palouse_sumloss_aggregate_drypeas[c("damagecause", "year", "county", "commodity")], levels))
alllevs_cherries <- do.call(expand.grid, lapply(palouse_sumloss_aggregate_cherries[c("damagecause", "year", "county", "commodity")], levels))

alllevs2_drypeas <- merge(palouse_sumloss_aggregate_drypeas, alllevs_drypeas, all.y=TRUE)
alllevs2_drypeas$loss[is.na(alllevs2_drypeas$loss)] <- 0
alllevs2_drypeas$cube_loss[is.na(alllevs2_drypeas$cube_loss)] <- 0
alllevs2_drypeas$log10_loss[is.na(alllevs2_drypeas$log10_loss)] <- 0


alllevs2_apples <- merge(palouse_sumloss_aggregate_apples, alllevs_apples, all.y=TRUE)
alllevs2_apples$loss[is.na(alllevs2_apples$loss)] <- 0
alllevs2_apples$cube_loss[is.na(alllevs2_apples$cube_loss)] <- 0
alllevs2_apples$log10_loss[is.na(alllevs2_apples$log10_loss)] <- 0


alllevs2_wheat <- merge(palouse_sumloss_aggregate_wheat, alllevs_wheat, all.y=TRUE)
alllevs2_wheat$loss[is.na(alllevs2_wheat$loss)] <- 0
alllevs2_wheat$cube_loss[is.na(alllevs2_wheat$cube_loss)] <- 0
alllevs2_wheat$log10_loss[is.na(alllevs2_wheat$log10_loss)] <- 0



# try hurdle model:
alllevs2_wheat$non_zero<-as.numeric(alllevs2_wheat$loss!=0)

m1 <- glm(non_zero ~  year + damagecause * county, data = alllevs2_wheat, family = binomial(link = logit))

# checking goodness of fit
library(pscl)
library(ResourceSelection)
library(broom)
pR2(m1)
hoslem.test(alllevs2_wheat$non_zero, fitted(m1))







alllevs2_apples$non_zero<-as.numeric(alllevs2_apples$loss!=0)

#logistic regression for binary zero/non-zero loss
m1 <- glm(non_zero ~  year + damagecause * county, data = alllevs2_apples, family = binomial(link = logit))

# checking goodness of fit
library(pscl)
library(ResourceSelection)
library(broom)
pR2(m1)
hoslem.test(alllevs2_apples$non_zero, fitted(m1))

#check for outliers
plot(m1, which = 4, id.n = 3)

model.data<-augment(m1, data=alllevs2_apples)
ggplot(model.data, aes(rownames(alllevs2_apples), .std.resid)) + 
  geom_point(aes(color = non_zero), alpha = .5) +
  theme_bw()+expand_limits(y=c(-3,3))+scale_y_continuous(breaks=seq(-3, 3, 1))

# check fgr multicollinearity
round(vif(m1),3)

# results
summary(m1)

# now subset to just those observations with a loss greater than zero, and run linear regression (switched to log loss due to outliers)
m2 <- lm(log(loss) ~ year + damagecause + county, data = subset(alllevs2_apples, non_zero == 1))
summary(m2)
plot(m2)

coefplot(m2)

#possibly add 1


# and now try with county as a random effect
model.b <- lmer(log(loss) ~ year + damagecause + (1 | county), data=subset(alllevs2_apples, non_zero == 1))
summary(model.b)
plot(model.b)
qqnorm(resid(model.b))

coefplot2(model.b)

