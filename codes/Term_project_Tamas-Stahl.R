#######################
## Analysis of       ##
##    the 2018       ##
##      PISA test    ##
#######################



# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
require(scales)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)
library(ggpubr)
library(ggrepel)
library(gridExtra)
library(segmented)

# Call the data from github
my_url <- "https://raw.githubusercontent.com/tamasstahl/DA2-Term-Project/main/data/raw/Pisa_2018_Data.csv"
df <- read_csv( my_url )



# Checking histograms
summary( df )

df <-df %>% transmute( countries =  Countries,
                       OECD_or_not = `OECD or Partner`,
                       reading_mean = `Mean score reading`,
                       reading_mean_boys = `Boys means score reading`,
                       reading_mean_girls = `Girls Mean score reading`,
                       reading_score_diff = `Score diff reading`,
                       maths_mean = `Mean score maths`,
                       maths_mean_boys = `Boys mean score maths`,
                       maths_mean_girls = `Girls means score maths`,
                       maths_score_diff = `Score diff maths`,
                       science_mean = `Mean score science`,
                       science_mean_boys = `Boys mean score science`,
                       science_mean_girls = `Girls mean score science`,
                       science_score_diff = `Score diff science`,
                       internet_time_school = `Time spent on internet at school (minutes)`,
                       internet_time_home_weekdays = `Time spent on internet at home weekdays (minutes)`,
                       internet_time_home_weekends = `Time spent on internet at home weekends (minutes)`,
                       gov_public_schools_ratio = `Percentage of government or public schools`,
                       private_schools_ratio = `Percentage of private schools`,
                       life_sat = `Average life satisfaction (1-10)`,
                       bullying_ratio = `Percentage of frequently bullied students`)


df$life_sat[is.na(df$life_sat)] <- mean(df$life_sat, na.rm = TRUE)
df$life_sat <- round(df$life_sat, digits = 1)

df$private_schools_ratio[is.na(df$private_schools_ratio)] <- mean(df$private_schools_ratio, na.rm = TRUE)
df$private_schools_ratio <- round(df$private_schools_ratio, digits = 1)

df$gov_public_schools_ratio[is.na(df$gov_public_schools_ratio)] <- mean(df$gov_public_schools_ratio, na.rm = TRUE)
df$gov_public_schools_ratio <- round(df$gov_public_schools_ratio, digits = 1)

df$bullying_ratio[is.na(df$bullying_ratio)] <- mean(df$bullying_ratio, na.rm = TRUE)
df$bullying_ratio <- round(df$bullying_ratio, digits = 1)

df$life_sat[is.na(df$life_sat)] <- mean(df$life_sat, na.rm = TRUE)
df$life_sat <- round(df$life_sat, digits = 1)

df <- df %>% select(-c(15,16,17))
df <- df %>% filter( complete.cases( df ) )

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_bw() + 
  scale_fill_wsj()


## Data frame with internet data and only complete cases
df_internet <- read_csv( my_url )

df_internet <-df_internet %>% transmute( countries =  Countries,
                       OECD_or_not = `OECD or Partner`,
                       reading_mean = `Mean score reading`,
                       reading_mean_boys = `Boys means score reading`,
                       reading_mean_girls = `Girls Mean score reading`,
                       reading_score_diff = `Score diff reading`,
                       maths_mean = `Mean score maths`,
                       maths_mean_boys = `Boys mean score maths`,
                       maths_mean_girls = `Girls means score maths`,
                       maths_score_diff = `Score diff maths`,
                       science_mean = `Mean score science`,
                       science_mean_boys = `Boys mean score science`,
                       science_mean_girls = `Girls mean score science`,
                       science_score_diff = `Score diff science`,
                       internet_time_school = `Time spent on internet at school (minutes)`,
                       internet_time_home_weekdays = `Time spent on internet at home weekdays (minutes)`,
                       internet_time_home_weekends = `Time spent on internet at home weekends (minutes)`,
                       gov_public_schools_ratio = `Percentage of government or public schools`,
                       private_schools_ratio = `Percentage of private schools`,
                       life_sat = `Average life satisfaction (1-10)`,
                       bullying_ratio = `Percentage of frequently bullied students`)


df_internet$life_sat[is.na(df_internet$life_sat)] <- mean(df_internet$life_sat, na.rm = TRUE)
df_internet$life_sat <- round(df_internet$life_sat, digits = 1)

df_internet$private_schools_ratio[is.na(df_internet$private_schools_ratio)] <- mean(df_internet$private_schools_ratio, na.rm = TRUE)
df_internet$private_schools_ratio <- round(df_internet$private_schools_ratio, digits = 1)

df_internet$gov_public_schools_ratio[is.na(df_internet$gov_public_schools_ratio)] <- mean(df_internet$gov_public_schools_ratio, na.rm = TRUE)
df_internet$gov_public_schools_ratio <- round(df_internet$gov_public_schools_ratio, digits = 1)

df_internet$bullying_ratio[is.na(df_internet$bullying_ratio)] <- mean(df_internet$bullying_ratio, na.rm = TRUE)
df_internet$bullying_ratio <- round(df_internet$bullying_ratio, digits = 1)

df_internet$life_sat[is.na(df_internet$life_sat)] <- mean(df_internet$life_sat, na.rm = TRUE)
df_internet$life_sat <- round(df_internet$life_sat, digits = 1)

df_internet <- df_internet %>% filter( complete.cases( df_internet ) )

#Creating a dummy variable whether a country is OECD or Partners country
df_internet$OECD_or_not <- ifelse(df_internet$OECD_or_not == 'OECD', 1, 0)

summary(df_internet)

?labs_

df_internet %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_bw() + 
  scale_fill_wsj()

chck_sp <- function(x_var){
  ggplot( df_internet , aes(x = x_var, y = maths_mean)) +
    geom_point() +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(y = "Average reading scores") 
}

chck_sp(df_internet$bullying_ratio)

chck_sp(df_internet$OECD_or_not)

chck_sp(df_internet$internet_time_school)
chck_sp(log(df_internet$internet_time_school))

chck_sp(df_internet$internet_time_home_weekdays)
chck_sp(log(df_internet$internet_time_home_weekdays))

chck_sp(df_internet$gov_public_schools_ratio)

## Checking whether there is any difference in maths, reading, science score and the time spent on the internet during weekdays.
s1 <- ggplot( df_internet , aes(x = internet_time_home_weekdays, y = maths_mean)) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs(y = "Average maths scores") 

s2 <- ggplot( df_internet , aes(x = internet_time_home_weekdays, y = reading_mean)) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs(y = "Average reading scores")

s3 <- ggplot( df_internet , aes(x = internet_time_home_weekdays, y = science_mean)) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs(y = "Average science scores")

ggarrange(s1, s2, s3, nrow = 1)

Selected <-
  df %>%
  filter(countries %in% c("Georgia", "Colombia", "Indonesia", "Greece", "Jordan", "OECD average", "Japan", "Poland", "Singapore", "Finland", "Singapore", "Hungary", "United States"))

## Scores by country - we used df data frame as the scores and countries were needed only
ggplot(df, aes(x = reading_mean_boys, y = reading_mean_girls, label = countries)) +
  geom_point() +
  geom_point(data= Selected, color="red") +
  geom_text_repel(data= Selected, mapping = aes(label=countries), color="grey20", hjust = 2) +
  geom_abline(slope=1, intercept = 0) + 
  geom_abline(slope=1, intercept = 20, linetype = 2, color="grey50") + 
  geom_abline(slope=1, intercept = -20, linetype = 2, color="grey50") +
  theme_bw() + ggtitle("Reading Score in PISA 2018 by Gender") +
  xlab("PISA 2018 Reading Score for Boys") +
  ylab("PISA 2018 Reading Score for Girls")

ggplot(df, aes(x = maths_mean_boys, y = maths_mean_girls, label = countries)) +
  geom_point() +
  geom_point(data= Selected, color="red") +
  geom_text_repel(data= Selected, mapping = aes(label=countries), color="grey20", hjust = 2) +
  geom_abline(slope=1, intercept = 0) + 
  geom_abline(slope=1, intercept = 20, linetype = 2, color="grey50") + 
  geom_abline(slope=1, intercept = -20, linetype = 2, color="grey50") +
  theme_bw() + ggtitle("Maths Score in PISA 2018 by Gender") +
  xlab("PISA 2018 Maths Score for Boys") +
  ylab("PISA 2018 Maths Score for Girls")

ggplot(df, aes(x = science_mean_boys, y = science_mean_girls, label = countries)) +
  geom_point() +
  geom_point(data= Selected, color="red") +
  geom_text_repel(data= Selected, mapping = aes(label=countries), color="grey20", hjust = 2) +
  geom_abline(slope=1, intercept = 0) + 
  geom_abline(slope=1, intercept = 20, linetype = 2, color="grey50") + 
  geom_abline(slope=1, intercept = -20, linetype = 2, color="grey50") +
  theme_bw() + ggtitle("Science Score in PISA 2018 by Gender") +
  xlab("PISA 2018 Science Score for Boys") +
  ylab("PISA 2018 Science Score for Girls")

##Subset creating - Again we used the df data frame as now the score differences were needed
subset <- df %>% select(1, 2, 6, 10, 14)
subset1 <- subset %>% filter( complete.cases( subset ) )

subsetOECD <- filter(subset1, OECD_or_not == "OECD")
subsetPartners <- filter(subset1, OECD_or_not == "Partners")

## OECD
oecd1 <- ggplot(data=subsetOECD, aes(x=reorder(countries, maths_score_diff), y=maths_score_diff)) +
  geom_bar(stat = "identity", aes(fill=maths_score_diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(subsetOECD$maths_score_diff), size=1, color="black") +
  geom_vline(xintercept = "OECD average", size = 1, color = "red") +
  labs(x="", y="Maths")+
  scale_fill_gradient(name="Score Difference", low = "red", high = "green")+
  theme(legend.position = "none") +
  ggtitle("Maths Score Difference in PISA 2018 by Girls") 

oecd2 <- ggplot(data=subsetOECD, aes(x=reorder(countries, reading_score_diff), y=reading_score_diff)) +
  geom_bar(stat = "identity", aes(fill=reading_score_diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(subsetOECD$reading_score_diff), size=1, color="black") +
  geom_vline(xintercept = "OECD average", size = 1, color = "red") +
  labs(x="", y="Reading")+
  scale_fill_gradient(name="% Difference Level", low = "red", high = "green") +
  theme(legend.position = "none")+
  ggtitle("Reading Score Difference in PISA 2018 by Girls") 

oecd3 <- ggplot(data=subsetOECD, aes(x=reorder(countries, science_score_diff), y=science_score_diff)) +
  geom_bar(stat = "identity", aes(fill=science_score_diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(subsetOECD$science_score_diff), size=1, color="black") +
  geom_vline(xintercept = "OECD average", size = 1, color = "red") +
  labs(x="", y="Reading")+
  scale_fill_gradient(name="% Difference Level", low = "red", high = "green") +
  theme(legend.position = "none")+
  ggtitle("Science Score Difference in PISA 2018 by Girls")

OECDChart <- ggarrange(oecd1, oecd2, oecd3, nrow = 1)+
  ggtitle("OECD Difference in Scores")

library(gridExtra)
grid.arrange(oecd1, oecd3, oecd2, nrow = 1,
             top = 'Do Females perform better than Males?',
             bottom = 'Score Difference in OECD Countries from Females in 2018'
)

## Partners
partners1 <- ggplot(data=subsetPartners, aes(x=reorder(countries, maths_score_diff), y=maths_score_diff)) +
  geom_bar(stat = "identity", aes(fill=maths_score_diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(subsetPartners$maths_score_diff), size=1, color="black") +
  geom_vline(xintercept = "OECD average", size = 1, color = "red") +
  labs(x="", y="Maths")+
  scale_fill_gradient(name="Score Difference", low = "red", high = "green")+
  theme(legend.position = "none") +
  ggtitle("Maths Score Difference") 

partners2 <- ggplot(data=subsetPartners, aes(x=reorder(countries, reading_score_diff), y=reading_score_diff)) +
  geom_bar(stat = "identity", aes(fill=reading_score_diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(subsetPartners$reading_score_diff), size=1, color="black") +
  geom_vline(xintercept = "OECD average", size = 1, color = "red") +
  labs(x="", y="Reading")+
  scale_fill_gradient(name="% Difference Level", low = "red", high = "green") +
  theme(legend.position = "none")+
  ggtitle("Reading Score Difference") 

partners3 <- ggplot(data=subsetPartners, aes(x=reorder(countries, science_score_diff), y=science_score_diff)) +
  geom_bar(stat = "identity", aes(fill=science_score_diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(subsetPartners$science_score_diff), size=1, color="black") +
  geom_vline(xintercept = "OECD average", size = 1, color = "red") +
  labs(x="", y="Science")+
  scale_fill_gradient(name="% Difference Level", low = "red", high = "green") +
  theme(legend.position = "none")+
  ggtitle("Science Score Difference")

PartnersChart <- ggarrange(partners1, partners2, partners3, nrow = 1)

grid.arrange(partners1, partners3, partners2, nrow = 1,
             top = 'Do Females perform better than Males?',
             bottom = 'Score Difference in Partner Countries from Females in 2018'
)


numeric_df <- keep( df_internet , is.numeric )
cT <- cor(numeric_df , use = "complete.obs")

# Check for highly correlated values:
sum( abs( cT ) >= 0.8 & cT != 1 ) / 2
# Find the correlations which are higher than 0.8
id_cr <- which( abs( cT ) >= 0.8 & cT != 1 )
pair_names <- expand.grid( variable.names(numeric_df) , variable.names(numeric_df) )
# Get the pairs:
high_corr <- pair_names[ id_cr , ]
high_corr <- mutate( high_corr , corr_val = cT[ id_cr ] )
high_corr


## Models
# reg1: NO control, simple linear regression
reg1 <- lm_robust( reading_mean ~ internet_time_home_weekdays , data = df_internet )
summary( reg1 )



# reg2: NO controls, use piecewise linear spline(P.L.S) with a knot at 180
reg2 <- lm_robust( reading_mean ~ lspline( internet_time_home_weekdays , 180 ) , data = df_internet )
summary( reg2 )

# Extra for reg2: 
# Interactions for stratio: let a dummy be: internet_time_home_weekdays > 180, 
reg21 <- lm_robust( reading_mean ~ internet_time_home_weekdays + (internet_time_home_weekdays > 180) + internet_time_home_weekdays*(internet_time_home_weekdays > 180) , data = df_internet )
summary( reg21 )

# Finding the breaking points
reg1_lm <- lm( reading_mean ~ internet_time_home_weekdays , data = df_internet )
fit_seg <- segmented( reg1_lm , seg.Z = ~internet_time_home_weekdays, psi = list(internet_time_home_weekdays=130))
summary(fit_seg)

#reg3 =  reg2 + OECD dummy
reg3 <- lm_robust( reading_mean ~ lspline( internet_time_home_weekdays , 180 ) + OECD_or_not, data = df_internet )
summary( reg3 )

# reg4: reg3 + life_sat PLS at knots 6 and bullying ratio
reg4 <- lm_robust( reading_mean ~ lspline( internet_time_home_weekdays , 180 ) + OECD_or_not 
                   + lspline(life_sat,6) + bullying_ratio , data = df_internet )
summary( reg4 )

# Comparison
data_out <- "C:/Users/ADMIN/Desktop/CEU/DA2/Term-project/"
htmlreg( list(reg1 , reg2 , reg3 , reg4),
         type = 'html',
         custom.header = list("Average reading scores in 2018"=1:4),
         custom.model.names = c("(1)","(2)","(3)","(4)"),
         custom.coef.names = c("Intercept","internet at home on weekdays","internet at home on weekdays (<180 mins)","internet at home on weekdays (>=180 mins)",
                               "OECD_or_not","lspline(life_sat,6)1","lspline(life_sat,6)2","bullying ratio"),
         omit.coef = "OECD_or_not|life_sat",
         file = paste0( data_out ,'PISA_2018.html'), include.ci = FALSE,
         single.row = FALSE, siunitx = TRUE,
         custom.gof.rows = list( OECD_or_not = c("NO","NO","YES","YES"),
                                 Other_Special = c("NO","NO","NO","YES")))
  