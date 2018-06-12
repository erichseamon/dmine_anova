#--eseamon
#three_way_anova_rm.R
#
#three way anova with repeated measures on 26 county palouse region



Southern_ID_sumloss <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/PNW_summary_all.csv")
Southern_ID_sumloss_all_sum  <- aggregate(loss ~ year + damagecause + county + commodity,  Southern_ID_sumloss, sum)
Southern_ID_count_all_count  <- aggregate(count ~ year + damagecause + county + commodity,  Southern_ID_sumloss, sum)
Southern_ID_sumloss_all_sum <- Southern_ID_sumloss_all_sum[Southern_ID_sumloss_all_sum$loss >= 1, ]


#-Loading all WHEAT claims for the palouse from 1989-2015
palouse_sumloss <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_sumloss.csv")
palouse_counts <- read.csv("/dmine/data/USDA/agmesh-scenarios/Allstates/summaries/Palouse_summary_counts.csv")


#box cox 
bc <- boxcox(loss~damagecause*year*commodity, data=palouse_sumloss_aggregate, lambda = seq(-0.05, 0.1, len = 20))
boxcox.lambda(bc, method = c("guerrero", "loglik"), lower = -1, upper = 2)


#use a cube transformation on loss for WHEAT claims

Math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

Southern_ID_sumloss_all_sum$cube_loss <- Math.cbrt(Southern_ID_sumloss_all_sum$loss)
Southern_ID_count_all_count$cube_counts <- Math.cbrt(Southern_ID_count_all_count$count)


palouse_sumloss_aggregate <- aggregate(loss ~ damagecause + year + commodity + county, palouse_sumloss, mean)
palouse_sumloss_aggregate$cube_loss <- Math.cbrt(palouse_sumloss_aggregate$loss)


palouse_sumloss_aggregate <- subset(palouse_sumloss_aggregate, loss != 0)

#-use a log transform on the same WHEAT claims data
palouse_sumloss_aggregate$log10_loss <- log10(which(!is.na(palouse_sumloss_aggregate$loss)))
palouse_sumloss_aggregate$log_loss <- log(which(!is.na(palouse_sumloss_aggregate$loss)))

#-inverse transform
palouse_sumloss_aggregate$inverse_loss <- 1/(which(!is.na(palouse_sumloss_aggregate$loss)))

#sq root transformation
palouse_sumloss_aggregate$sqroot_loss <- sqrt(which(!is.na(palouse_sumloss_aggregate$loss)))

#--scale and center

palouse_sumloss_aggregate$scaled_cube_loss <- scale(palouse_sumloss_aggregate$cube_loss)
palouse_sumloss_aggregate$scaled_inverse_loss <- scale(palouse_sumloss_aggregate$inverse_loss, center = TRUE, scale = TRUE)


#-turn year into character value
palouse_sumloss_aggregate$year <- as.character(palouse_sumloss_aggregate$year)

#-factor year
palouse_sumloss_aggregate$year <- factor(palouse_sumloss_aggregate$year)

#-boxplots of data by cube loss by each of the three factors
boxplot(cube_loss ~ year, palouse_sumloss_aggregate, las = 3)
boxplot(cube_loss ~ commodity, palouse_sumloss_aggregate, las = 3)
boxplot(cube_loss ~ damagecause, palouse_sumloss_aggregate, las = 3)
boxplot(cube_loss ~ county, palouse_sumloss_aggregate, las = 3)

#-nortest for normality.  Shapiro Wilks doesnt work on values higher than 5000
library(nortest)
ad.test(palouse_sumloss_aggregate$cube_loss)$p.value



#--Levene Test for homogeneity of variance

leveneTest(sqroot_loss ~ year, data=palouse_sumloss_aggregate, center = mean)

#---Bartlett Test - homogeneity of variances

bartlett.test(sqroot_loss ~ year, palouse_sumloss_aggregate)

#--Fligner-Killeen test for homoskedasticity

fligner.test(palouse_sumloss_aggregate$sqroot_loss, palouse_sumloss_aggregate$year)





#------

#AOV

palouse_sumloss_aggregate_loss$year <- factor(palouse_sumloss_aggregate_loss$year)





fit <- aov(loss~damagecause * county * year + Error(X / (damagecause * county)), data=palouse_sumloss)
fit <- with(Southern_ID_sumloss_all_sum, aov(loss~damagecause * county * year + Error(generated_uid * year)))

Southern_ID_sumloss_all_sum <- Southern_ID_sumloss_all_sum[-5] 
Southern_ID_sumloss_all_sum <- Southern_ID_sumloss_all_sum[-6] 

palouse_sumloss_aggregate <- aggregate(cube_loss ~ damagecause + year + county + commodity, palouse_sumloss, mean)

palouse_sumloss_aggregate_loss <- aggregate(loss ~ damagecause + year + county + commodity, palouse_sumloss, mean)



aov.ex5 = aov(sqroot_loss~year*damagecause*commodity + Error(county/year), palouse_sumloss_aggregate  )

boxplot(palouse_sumloss_aggregate$sqroot_loss ~ palouse_sumloss_aggregate$commodity, las = 3)

boxplot(palouse_sumloss_aggregate$sqroot_loss ~ palouse_sumloss_aggregate$year, las = 3)

#--multcomp example

library(multcomp) 
amod <- aov(sqroot_loss ~ damagecause * year* commodity, data = palouse_sumloss_aggregate) 
wht <- glht(amod, linfct = mcp(damagecause = "Tukey")) 





summary(fit) #Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and Ftests