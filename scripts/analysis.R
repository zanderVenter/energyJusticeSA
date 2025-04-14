library(tidyverse)
library(sf)
library(anytime)
library(grid)
library(gridExtra)
library(haven)
library(ggdist)
library(lme4)
library(MASS)
library(BBmisc)
library(multcomp)
library(multcompView)
library(gamlss)
library(car)
library(ggeffects)
library(scales)

racePal <-  c("#F9837B","#ACAE19", "#19C58A", "#19B8F7","#E97AF4")



#### Import and organize Solar and energy panel data --------------------------------------

# Solar panel data
solar_areas <- read_csv('./DATA/solar_panel_areas.csv')


solar_time <- read_csv('./DATA/From_sampling/eskom_panel_sample - Sheet1.csv') %>%
  mutate(Name = as.character(Name)) %>%
  left_join(st_read('./DATA/For_sampling/solar_panels_sample.kml') %>%
              mutate(area = as.numeric(st_area(geometry))) %>% as_tibble() %>%
              dplyr::select(Name, area))
unique(solar_time$installationYear)

# Eskom loadshedding national level data from Eskomsepush

loadsheddingRaw <- read_csv('./DATA/EskomSePush Loadshedding History - EskomSePush_history.csv') %>%
  mutate(date = ymd_hms(created_at)) %>%
  dplyr::select(-created_at)
loadsheddingRaw %>%
  ggplot(aes(x=date, y=stage)) +
  geom_line()


# Loadshedding hourly

loadsheddingMins <- tibble(
  date = seq(ymd_hms(20140306090000),ymd_hms(20240101000000), by = 'mins')
) %>%
  left_join(loadsheddingRaw, by = 'date')%>%
  fill(stage, .direction = "down")
colSums(is.na(loadsheddingMins))

loadsheddingHrs <- loadsheddingMins %>%
  mutate(year = year(date)) %>%
  filter(year >= 2016 & year < 2024) %>%
  mutate(stage = ifelse(stage == 0, 0, 1)) %>%
  group_by(year) %>%
  summarise(hrs = sum(stage)/60) 


sum(loadsheddingHrs$hrs)
sum(loadsheddingHrs$hrs[8])/24
sum(loadsheddingHrs$hrs[8])/(365*24)
sum(loadsheddingHrs$hrs[8])/(365*24)


# Nighttime lights data
nightlights <- read_csv('./DATA/From_GEE/census_sal_lights_timeseries.csv') %>%
  filter(SAL_CODE %in% unique(solar_areas$SAL_CODE)) %>%
  mutate(radiance = mean) %>%
  dplyr::select(SAL_CODE, date, radiance) %>%
  drop_na(radiance)
colSums(is.na(nightlights))
hist(nightlights$radiance)

nightlightsAv <- nightlights %>%
  group_by(SAL_CODE) %>%
  summarise(avRadiance = mean(radiance, na.rm=T))

nightlightsChange <- nightlights %>%
  mutate(year = year(date)) %>%
  filter(year %in% c(2016, 2022)) %>%
  dplyr::select(-date ) %>%
  pivot_wider(values_from=radiance, names_from=year) %>%
  mutate(changeRadiance = `2022` - `2016`,
         changeRadiancePerc = ( `2022` - `2016`)/`2016`*100) %>%
  dplyr::select(SAL_CODE, changeRadiance, changeRadiancePerc)


nightlightsNational <- nightlights %>%
  mutate(dateYear = floor_date(date, 'year')) %>%
  group_by(dateYear) %>%
  summarise(radianceSE = sd(radiance)/sqrt(n()),
            radiance = mean(radiance)) 


nightlightsNational %>%
  ggplot(aes(x=dateYear, y=radiance))+
  geom_point() +
  geom_line() 

#### Import and organize Census data ----------------------------------------------------

# Read in census spatial data
censusSpatRaw <-st_read('./DATA/census.shp') %>%
  mutate(SAL_CODE = SAL_COD) %>%
  dplyr::select(SAL_CODE)
censusSpat_areas <- censusSpatRaw %>%
  filter( st_is_valid(geometry)) %>%
  mutate(area = as.numeric(st_area(geometry))) %>%
  as_tibble() %>%
  group_by(SAL_CODE) %>%
  summarise(area = sum(area))

selectedTracts <- read_csv('./DATA/census_filtered.csv')
selectedTracts %>% ggplot(aes(x=areaType )) + geom_bar() + coord_flip()
selectedTracts %>% ggplot(aes(x=geoType )) + geom_bar() + coord_flip()
nrow(selectedTracts)

income <- read_dta('./DATA/annual-household-income.dta') %>%
  mutate(SAL_CODE = sal_code) %>%
  dplyr::select(SAL_CODE, inchh_1:inchh_13) %>%
  gather(key, val, -SAL_CODE) %>%
  mutate(key = dplyr::recode(key, 
                      "inchh_1" = 0,
                      "inchh_2" = mean(c(1, 4800)),
                      "inchh_3" = mean(c(4801, 9600)),
                      "inchh_4" = mean(c(9601, 19600)),
                      "inchh_5" = mean(c(19601, 38200)),
                      "inchh_6" = mean(c(38201, 76400)),
                      "inchh_7" = mean(c(76401, 153800)),
                      "inchh_8" = mean(c(153801, 307600)),
                      "inchh_9" = mean(c(307601, 614400)),
                      "inchh_10" = mean(c(614401, 1228800)),
                      "inchh_11" = mean(c(1228801, 2457600)),
                      "inchh_12" = 3686400,
                      "inchh_13" = NULL
                      )) %>%
  drop_na() %>%
  mutate(key = as.numeric(key), 
         income = key*val) %>%
  group_by(SAL_CODE) %>%
  summarise(incomeAv = sum(income)/12, 
            householdNum = sum(val))
sum(income$householdNum)
income %>% ggplot(aes(x=householdNum)) + geom_histogram()


# Race
race <- read_csv('./DATA/CENSUSSA_SAL_race.csv') %>%
  filter(SAL_CODE %in% unique(selectedTracts$SAL_CODE))
race %>% gather(key, val, -SAL_CODE) %>% summarise(totalPol=sum(val))

race <- race %>%
  gather(race, val, african:other) %>%
  group_by(SAL_CODE) %>%
  mutate(popNum = sum(val), percRace = val/popNum*100) %>%
  dplyr::select(-val) %>%
  spread(race, percRace)

# Number of 90% dominant 
nrow(race  %>% ungroup() %>%
       gather(race, value, african:indian, white) %>%
       dplyr::select(race, value) %>% filter(value > 75))/nrow(
         race
       )

race  %>% ungroup() %>%
  gather(race, value, african:indian, white) %>%
  ggplot(aes(x=value, fill=race)) +
  geom_histogram()
hist(race$popNum)

censusFull <- selectedTracts %>%
  left_join(censusSpat_areas) %>%
  left_join(race) %>%
  left_join(income) %>%
  mutate(incomeAvTran = log(incomeAv), 
       incomeAvperCap = incomeAv/popNum,
       popDens = popNum/(area/1000000)) %>%
  group_by(SAL_CODE) %>%
  mutate(raceMax = max(african, white,coloured, indian, other),
         raceCat = ifelse(african == raceMax,"african",
                          ifelse(white==raceMax,"white",
                                 ifelse(coloured==raceMax,"coloured",
                                        ifelse(indian==raceMax,"indian","other"))))) %>%
  ungroup()
censusFull$raceCat <- factor(censusFull$raceCat, levels=c('african','white','coloured','indian', 'other'))

hist(censusFull$incomeAvperCap)
censusFull$incCat <- cut(censusFull$incomeAvperCap, 
                   breaks=c(-Inf, 5000, 10000, 15000, 20000, 25000,Inf), 
                   labels=c("< 5k","5k - 10k","10k - 15k", "15k - 20k", "20k - 25k", ">25K"))
censusFull$incCat2 <- cut(censusFull$incomeAvperCap, 
                         breaks=c(-Inf, 500, 2500, 12500,Inf), 
                         labels=c("Poor", "Low","Middle","High"))
censusFull %>% ggplot(aes(x=incCat2)) + geom_bar()

censusFull$incCatNum <- cut(censusFull$incomeAvperCap, 
                         breaks=c(-Inf, 5000, 10000, 15000, 20000, 25000,Inf), 
                         labels=FALSE)
censusFull$incCatNum <- censusFull$incCatNum*5000
censusFull %>% ggplot(aes(x=areaType )) + geom_bar() + coord_flip()

#### Merge datasets ------------------------------------------------------------
censusSolar <- solar_areas %>%
  left_join(censusFull) %>%
  left_join(nightlightsChange) %>%
  left_join(nightlightsAv) %>%
  # exteremly few industrial or commercial
  filter(areaType %in% c('residen_formal', 'small_hold', 'residen_informal')) %>%
  mutate(solarAreaPerHouse = solarArea/householdNum)
str(censusSolar)
str(censusFull)
censusSolar %>% group_by(incCat2) %>% count()
levels(censusSolar$raceCat) <- c('African', 'White', 'Coloured', 'Indian', 'Other')

# Percentage of total census number
nrow(censusSolar) / nrow(selectedTracts)* 100

samplePop <- solar_areas %>%
  left_join(censusFull) %>%
  ungroup() %>%
  summarise(householdNum = sum(householdNum))
samplePop$householdNum[1]

censusSolarTime <- solar_time  %>%
  filter(!installationYear %in% c('unsure', 'Earlier than 2008')) %>%
  mutate(installationYear = as.numeric(installationYear))%>%
  group_by(installationYear) %>%
  summarise(area = sum(area, na.rm=T)) %>%
  # area per capita cumulative sum
  mutate(area = area/samplePop$householdNum[1],
         areaCum = cumsum(area),
         areaLag = lag(areaCum)) %>%
  filter(installationYear >= 2016) %>%
  mutate(baseArea = min(areaCum),
         percChange2016 = areaCum/baseArea*100,
         percChangeAnnual = (areaCum-areaLag)/areaLag*100)

censusLightsTime <- censusSolar %>%
  left_join(nightlights, by = 'SAL_CODE') %>%
  mutate(year = year(date)) %>%
  left_join(loadsheddingHrs %>%
              mutate(loadshedHrs = hrs), by = 'year')

#### Time series nationa level --------------------------------------------------------

# National level nighttime lights and loadshedding

lsToPlot <- tibble(dateMonth = seq(ymd_hms(20160101000000),ymd_hms(20230101000000), by = 'years')) %>%
  mutate(year = year(dateMonth)) %>%
  left_join(loadsheddingHrs)
pt1 <-lsToPlot %>%
  ggplot(aes(x=dateMonth, y = hrs)) +
  geom_point(size=3, alpha=0.7) +
  geom_line(data = lsToPlot %>% drop_na(), linetype = 2) +
  geom_line() +
  labs(y = 'Load shedding hours',
       title='A)')  +
  theme_bw()+
  theme(axis.title.x = element_blank())
pt1
# maximum average load shedding of 2.7 in 2023

nlToPlot <- tibble(dateYear = seq(ymd_hms(20160101000000),ymd_hms(20230101000000), by = 'years')) %>%
  left_join(nightlightsNational)
pt2 <-nlToPlot %>%
  ggplot(aes(x=dateYear, y = radiance)) +
  geom_point(size=3, alpha=0.7) +
  #geom_errorbar(aes(ymin=radiance-radianceSE, ymax=radiance+radianceSE), width=0) +
  geom_line() +
  labs(y = expression(atop('Nighttime light DNB radiance ','('~nW~sr^-1~cm^-2~')')),
       title='C)')  +
  theme_bw()+
  theme(axis.title.x = element_blank())
pt2
(20.2-15.4)/15.4*100
# 31% increase since 2016
mean(censusSolar$changeRadiance)

pt3 <- censusSolarTime %>%
  ggplot(aes(x=installationYear, y=percChange2016)) +
  geom_point(size=3, alpha=0.7) +
  geom_line() +
  scale_x_continuous(limits=c(2016, 2023)) +
  labs(y = expression('Solar panel adoption (%'~Delta~'since 2016)'),
       title='B)')  +
  theme_bw()+
  theme(axis.title.x = element_blank())


tsGraphsOverall <- grid.arrange( pt1, pt3,pt2, ncol=3,
                           padding = unit(0, "line"),widths=c(1,1,1.1), newpage = T)
tsGraphsOverall
ggsave("tsGraphsOverall.png", tsGraphsOverall, width = 30, height=10, units='cm')



#### Response variable distribution histograms ------------------------------------

phist1 <- censusSolar %>%
  ggplot(aes(x=solarAreaPerHouse)) +
  geom_histogram() +
  labs(x = expression('Solar panel area per household ('~m^2~')'),
       y = 'Count of census tracts',
       title = 'A)') +
  theme_bw()
phist1

phist2 <- censusSolar %>%
  ggplot(aes(x=avRadiance)) +
  geom_histogram() +
  labs(x = expression('Average nighttime light DNB radiance ('~nW~sr^-1~cm^-2~')'),
       y = 'Count of census tracts',
       title = 'B)') +
  theme_bw()+
  theme(axis.title.y = element_blank())
phist2


phist3 <- censusSolar %>%
  ggplot(aes(x=changeRadiance)) +
  geom_histogram() +
  labs(x = expression(Delta~' nighttime light DNB radiance ('~nW~sr^-1~cm^-2~')'),
       y = 'Count of census tracts',
       title = 'C)') +
  theme_bw() +
  theme(axis.title.y = element_blank())
phist3

histDistributPlot <- grid.arrange( phist1, phist2,phist3, ncol=3,
                                   padding = unit(0, "line"),widths=c(1.1,1,1), newpage = T)
histDistributPlot
ggsave("histDistributPlot.png", histDistributPlot, width = 33, height=10, units='cm')


#### Solar adoption, race, income -----------------------------------------------


hist(censusSolar$solarArea)
hist(censusSolar$solarAreaPerHouse)
hist(log(censusSolar$solarAreaPerHouse + 0.001))
hist(censusSolar$changeRadiance)
hist(censusSolar$changeRadiancePerc)
hist(censusSolar$avRadiance)

colSums(is.na(censusSolar))

var <- 'incCat2'

fitUnivModelAndPlot <- function(var, axisLabel, title){
  
  censusSolarToModel <- censusSolar %>%
    mutate(predVar = .data[[var]])
  
  #  Fit a linear model (OLS regression)
  lm_model <- lm(log(solarAreaPerHouse + 0.0001) ~ predVar, data = censusSolarToModel)
  summary(lm_model)
  
  par(mfrow = c(2,2))  # Plot multiple diagnostic plots
  plot(lm_model)
  
  print(anova(lm_model))
  
  posthoc <- glht(lm_model, linfct = mcp(predVar = "Tukey"))
  print(summary(posthoc))
  
  predicted_values <- predict(lm_model, newdata = data.frame(predVar = unique(censusSolarToModel$predVar)), 
                              interval = "confidence")
  
  plot_data <- data.frame(
    predVar = unique(censusSolarToModel$predVar),
    median_log = predicted_values[, "fit"],    # Model-predicted log(median solar area)
    lower_log = predicted_values[, "lwr"],     # Lower bound (95% CI)
    upper_log = predicted_values[, "upr"]      # Upper bound (95% CI)
  )
  
  plot_data <- plot_data %>%
    mutate(
      median_value = exp(median_log) - 0.0001,
      lower_bound = exp(lower_log) - 0.0001,
      upper_bound = exp(upper_log) - 0.0001
    )
  print(plot_data)
  
  # Extract significance letters
  letters <- cld(posthoc, decreasing = TRUE)$mcletters$Letters
  lettersDF <- as_tibble(letters) %>%
    mutate(predVar = names(letters)) %>%
    left_join(plot_data)
  
  sri <- ggplot(plot_data, aes(x = predVar, y = median_value)) +
    geom_point(size = 3, color = "black") +  # Median values
    geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                  width = 0.1, color = "black") +  # 95% CI
    geom_text(aes(label = round(median_value, 2)), hjust=-0.45) +
    geom_text(data = lettersDF, 
              aes(x = predVar, y = upper_bound, label = value), inherit.aes=F, vjust=-0.35) +
    theme_bw() +
    scale_y_log10(labels = function(x) sprintf("%g", x)) +
    labs(title = title,
         x = axisLabel,
         y =   expression('Solar panel area per household ('~m^2~')')) 
  sri
  
  return (sri)
}

sri1 <- fitUnivModelAndPlot('incCat2', "Income level", "A)")
sri1

sri2 <-  fitUnivModelAndPlot('raceCat', "Race category", "B)")
sri2


solarRaceInc <- grid.arrange( sri1, sri2,ncol=2,
                                   padding = unit(0, "line"),widths=c(1,1), newpage = T)
solarRaceInc
ggsave("solarRaceInc.png", solarRaceInc, width = 22, height=10, units='cm')


censusSolar %>%
  group_by(raceCat) %>%
  summarise(median = median(solarAreaPerHouse))

getSolarCensusStats <- function(){
  hist(censusSolar$solarAreaPerHouse)
  
  # race categories
  3.496 / (0+0.138)
  
  # how many times greater than income discrepency?
  (1058 + 1335 + 4568)/3
  10852/2320
  # income discrepency is 4.7 times higher (10852 ZAR vs 2320 ZAR) in white census tracts compared to previously disadvantaged groups
  
}


#### Nighttime light changes, race and income ----------------------------------------------

hist(censusSolar$solarArea)
hist(censusSolar$solarAreaPerHouse)
hist(log(censusSolar$solarAreaPerHouse + 0.001))
hist(censusSolar$changeRadiance)
hist(censusSolar$avRadiance)
hist(censusSolar$changeRadiancePerc)


var <- 'raceCat'
yvar <- 'changeRadiance'

fitUnivModelAndPlot2 <- function(var, yvar, axisLabel, yaxisLabel, title){
  
  censusSolarToModel <- censusSolar %>%
    mutate(predVar = censusSolar[[var]],
           responseVar = .data[[yvar]])
  
  #  Fit a linear model (OLS regression)
  lm_model <- lm(responseVar ~ predVar, data = censusSolarToModel)
  summary(lm_model)
  
  par(mfrow = c(2,2))  # Plot multiple diagnostic plots
  plot(lm_model)
  
  print(anova(lm_model))
  
  posthoc <- glht(lm_model, linfct = mcp(predVar = "Tukey"))
  print(summary(posthoc))
  
  predicted_values <- predict(lm_model, newdata = data.frame(predVar = unique(censusSolarToModel$predVar)), 
                              interval = "confidence")
  
  plot_data <- data.frame(
    predVar = unique(censusSolarToModel$predVar),
    median_value = predicted_values[, "fit"],    # Model-predicted
    lower_value = predicted_values[, "lwr"],     # Lower bound (95% CI)
    upper_value = predicted_values[, "upr"]      # Upper bound (95% CI)
  )
  print(plot_data)
  
  
  # Extract significance letters
  letters <- cld(posthoc, decreasing = TRUE)$mcletters$Letters
  lettersDF <- as_tibble(letters) %>%
    mutate(predVar = names(letters)) %>%
    left_join(plot_data)
  
  nlri <- ggplot(plot_data, aes(x = predVar, y = median_value)) +
    geom_point(size = 3, color = "black") +  # Median values
    geom_errorbar(aes(ymin = lower_value, ymax = upper_value), 
                  width = 0.1, color = "black") +  # 95% CI
    geom_text(aes(label = round(median_value, 2)), hjust=-0.45) +
    geom_text(data = lettersDF, 
              aes(x = predVar, y = upper_value, label = value), inherit.aes=F, vjust=-0.35) +
    theme_bw() +
    labs(title = title,
         x = axisLabel,
         y =   yaxisLabel) 
  nlri
  
  return (nlri)
}

nlri1 <- fitUnivModelAndPlot2('incCat2', 'changeRadiance', 'Income level', expression(Delta~' Nighttime light DNB radiance '~' ('~nW~sr^-1~cm^-2~')'), "A)")
nlri1
nlri2 <- fitUnivModelAndPlot2('raceCat', 'changeRadiance', 'Race category', expression(Delta~' Nighttime light DNB radiance '~' ('~nW~sr^-1~cm^-2~')'), "B)")
nlri2


lightRaceInc <- grid.arrange( nlri1, nlri2,ncol=2,
                              padding = unit(0, "line"),widths=c(1,1), newpage = T)
lightRaceInc
ggsave("lightRaceInc.png", lightRaceInc, width = 22, height=10, units='cm')


getNighttimelightStats <- function(){
  
  # Model average
  (-2.64-6.57-2.99-4.12)/4
  (-5.81-2.72-5.66-4)/4
  
  # national aggregate change
  nightlightsNational$radiance[1] - nightlightsNational$radiance[7]
  (nightlightsNational$radiance[1] - nightlightsNational$radiance[7]) / nightlightsNational$radiance[1]
  
  
  censusSolar %>%
    summarise(changeRadiance = median(changeRadiance),
              changeRadiance = changeRadiance,
              changeRadiancePerc = median(changeRadiancePerc))
  censusSolar %>%
    group_by(raceCat) %>%
    summarise(changeRadiance = median(changeRadiance),
              changeRadiance = changeRadiance*7,
              changeRadiancePerc = median(changeRadiancePerc))
  
  
  censusSolar %>%
    mutate(raceCat2 = ifelse(raceCat == 'white', 'white', 'rest')) %>%
    group_by(raceCat2) %>%
    summarise(changeRadiance = median(changeRadiance),
              changeRadiance = changeRadiance*7,
              changeRadiancePerc = median(changeRadiancePerc))
}


#### Explaining nighttime lights ------------------------------------------
censusLightsTime

hist(censusLightsTime$changeRadiance)

names(censusSolar)
censusSolar %>%
  ggplot(aes(x=solarArea, y = changeRadiance)) +
  geom_smooth(method='lm')


l_all <- lm(changeRadiance ~ solarAreaPerHouse + incCat2 + raceCat + popNum,
            data = censusSolar )
summary(l_all)

par(mfrow = c(2,2))  # Plot multiple diagnostic plots
plot(l_all)

coef(l_all)
confint(l_all)

vif(l_all)

# Generate ANOVA results
anova_results <- anova(l_all)
anova_results
anova_solarArea <- anova_results["solarAreaPerHouse", ]  # Extract row for solarArea

# Format the text for annotation
anova_text <- paste0("F(", anova_solarArea[["Df"]], ", ", 
                     anova_results[["Df"]][nrow(anova_results)], ") = ",
                     round(anova_solarArea[["F value"]], 2), 
                     ", p = ", format.pval(anova_solarArea[["Pr(>F)"]], digits = 3))


# Generate model-predicted values using ggpredict
predicted_data <- ggpredict(l_all, terms = "solarAreaPerHouse")

# Plot predictions with confidence intervals
p90 <- quantile(predicted_data$x, c(0.90))[[1]]

mlp <- predicted_data %>%
  #as_tibble() %>%
  filter(x < p90) %>%
  ggplot(aes(x = x, y = predicted)) +
  geom_hline(yintercept = 0, linetype=2) +
  geom_line( size = 1) +  # Predicted regression line
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +  # 95% CI
  theme_bw() +
  #scale_x_log10() +
  labs(x = expression('Solar panel area per household ('~m^2~')'),
       y = expression(Delta~' Nighttime light DNB radiance '~' ('~nW~sr^-1~cm^-2~')')) +
  #xlim(0,20)+
  #ylim(-6, 2.5) +
  #coord_cartesian(xlim=c(0,p90), expand=FALSE) +
  annotate("text", x = max(predicted_data$x) * 0.3, y = max(predicted_data$conf.high) * 0.9,
           label = anova_text, size = 3, hjust = 0, color = "black")


mlp

ggsave("regPlot.png", mlp, width = 10, height=10, units='cm')



#### Load shedding and outages -------------------------------------------------
# Hourly outages graph

# From Eskomsepush data
hourlyOutages_esp <- loadsheddingMins %>%
  mutate(hour = hour(date),
         day = yday(date),
         year = year(date),
         dayNight = ifelse(hour >= 6 & hour < 18, 'day', 'night')) %>%
  filter(year >= 2016 & year < 2024)  %>%
  mutate(stagePresent = ifelse(stage == 0, 0, 1)) %>%
  group_by(dayNight, year, day) %>%
  summarise(hrs = sum(stagePresent)/60,
            stage = mean(stage)) %>%
  group_by(dayNight, year) %>%
  summarise(hrs_mean = mean(hrs),
            hrs_se = sd(hrs),
            stage_mean = mean(stage),
            stage_se = sd(stage))
hourlyOutages_esp

hourlyOutages_esp %>%
  summarise_at(vars(hrs_mean:stage_se), mean)



hp1 <- hourlyOutages_esp %>%
  ggplot(aes(x=year, y=hrs_mean, color=dayNight)) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_line(position = position_dodge(width = 0.25), alpha=0.6) +
  geom_errorbar(aes(ymin=hrs_mean-hrs_se, ymax=hrs_mean+hrs_se),
                position = position_dodge(width = 0.25),
                width = 0.15) +
  labs(x='Year',
       y = 'Hours of load shedding per 24-hr day',
       title = 'A)',
       color='')  +
  theme_bw() +
  theme(legend.position = c(0.5,0.7))
hp1

hp2 <- hourlyOutages_esp %>%
  ggplot(aes(x=year, y=stage_mean, color=dayNight)) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_line(position = position_dodge(width = 0.25), alpha=0.6) +
  geom_errorbar(aes(ymin=stage_mean-stage_se, ymax=stage_mean+stage_se),
                position = position_dodge(width = 0.25),
                width = 0.15) +
  labs(x='Year',
       y = 'Average loadshedding stage per 24-hr day',
       title = 'B)',
       color='')  +
  theme_bw() +
  theme(legend.position = c(0.5,0.7))
hp2

outageGraphHourly <- grid.arrange(hp1, hp2, nrow=2,
                                  padding = unit(0, "line"),heights=c(1,1), newpage = T)
ggsave("hourlyOutages.png", outageGraphHourly, width = 20, height=20, units='cm')

