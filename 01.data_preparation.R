#------------------------------------------------------------------------------
# PROJECT TITLE: 
# Intersectionele sekse- en herkomstverschillen in niet-werken:Een verklaring 
# op basis van sociaal kapitaal.
#
# SCRIPT: Data Preparation
#
# DATE: 09/05/2022
# NAME:
# - Jop Roeleven (j.e.roeleven@students.uu.nl)
# - Jos Slabbekoorn (j.slabbekoorn@uu.nl)
#------------------------------------------------------------------------------
# load packages
library(tidyverse)
library(haven)

#------------------------------------------------------------------------------
# load data
#------------------------------------------------------------------------------
# background variables 2018
avars18 <- haven::read_spss('./data/avars_201810_EN_1.0p.sav',
                            col_select = c(nomem_encr, geslacht, herkomstgroep, 
                                           leeftijd, aantalki, oplcat))
# background variables 2019
avars19 <- haven::read_spss('./data/avars_201910_EN_1.0p.sav',
                            col_select = c(nomem_encr, belbezig))

# leisure and integration 2018
cs <- haven::read_spss('./data/cs18k_EN_1.0p.sav',
                        col_select = c(nomem_encr, cs18k294, cs18k295, 
                                       cs18k296, cs18k297, cs18k298, 
                                       cs18k327, cs18k338, cs18k349, 
                                       cs18k360, cs18k371))

#------------------------------------------------------------------------------
# clean data
#------------------------------------------------------------------------------
# background variables 2018
avars18 <- avars18 %>% 
  arrange(nomem_encr) %>%
  mutate(female = haven::labelled(geslacht - 1,
                                  c(male = 0, female = 1)),
         dutch = haven::labelled(ifelse(herkomstgroep == 0, 1, 0),
                                 c(other = 0, dutch = 1)),
         west = haven::labelled(ifelse((herkomstgroep == 102 
                                        | herkomstgroep == 202), 1, 0),
                                c(other = 0, west = 1)),
         nonwest = haven::labelled(ifelse((herkomstgroep == 101 
                                        | herkomstgroep == 201), 1, 0),
                                   c(other = 0, nonwest = 1)),
         age = leeftijd,
         child = haven::labelled(ifelse(aantalki >=1, 1, 0),
                                 c(nochild = 0, child = 1)),
         educ = recode(as.factor(oplcat),
                       `1` = 8,  `2` = 12, `3` = 13.5, 
                       `4` = 16, `5` = 17, `6` = 18)) %>% 
  select(nomem_encr, female, dutch, west, nonwest, age, child, educ)
        
# get descriptives of avars before listwise deletion 
desc_avars_18 <- avars18 %>% 
  select(female, west, nonwest) %>% 
  summary()

# listwise deletion
n_before = nrow(avars18)
avars18 <- avars18 %>% drop_na()
n_after = nrow(avars18)
print(c(before = n_before, after = n_after, diff = n_before - n_after))

# background variables 2019
avars19 <- avars19 %>% 
  arrange(nomem_encr) %>%
  mutate(robfilter = haven::labelled(ifelse(belbezig == 8, 1, 0),
                                     c(exclude_in_robustness = 1, 
                                       include_in_robustness = 0)),
         unemp = haven::labelled(ifelse(belbezig %in% c(4, 5, 8, 11), 1, 
                                    ifelse(belbezig %in% c(1, 2, 3), 0, NA)),
                                 c(employed = 0, unemployed = 1))) %>% 
  select(nomem_encr, robfilter, unemp)

# get descriptives of avars before listwise deletion 
desc_avars_19 <- avars19 %>% 
  select(unemp) %>% 
  summary()

# listwise deletion
n_before = nrow(avars19)
avars19 <- avars19 %>% drop_na()
n_after = nrow(avars19)
print(c(before = n_before, after = n_after, diff = n_before - n_after))  

# leisure and social integration
cs <- cs %>% 
  arrange(nomem_encr) %>% 
  mutate(numcon = cs18k294 + cs18k295 + cs18k296 + cs18k297 + cs18k298,
         numwork = ifelse(replace_na(cs18k327, 9) < 3, 1, 0) 
                      + ifelse(replace_na(cs18k338, 9) < 3, 1, 0)
                      + ifelse(replace_na(cs18k349, 9) < 3, 1, 0)
                      + ifelse(replace_na(cs18k360, 9) < 3, 1, 0)
                      + ifelse(replace_na(cs18k371, 9) < 3, 1, 0)) %>% 
  select(nomem_encr, numwork, numcon)

#------------------------------------------------------------------------------
# clean data
#------------------------------------------------------------------------------
# merge and select data
df <- avars19 %>% 
  left_join(avars18, by = "nomem_encr") %>% 
  left_join(cs, by = "nomem_encr") %>% 
  filter((age >= 25) & (age <= 65)) %>% 
  drop_na()

# make interaction variables
df <- df %>% 
  mutate(f_d = female * dutch,
         f_w = female * west, 
         f_nw = female * nonwest)

df %>% write_sav(., './data/merged.sav')

#------------------------------------------------------------------------------
# Missing data analyses
#------------------------------------------------------------------------------

# people in sample have more working contacts
t.test(numwork, mu = mean(cs$numwork), data=df)

# people in sample have are not more unemployed
t.test(unemp, mu = mean(avars19$unemp), data=df)

# there is no disproportionate missingness among people with a western
# and non western migration background
t.test(west, mu = mean(avars18$west), data=df)
t.test(nonwest, mu = mean(avars18$nonwest), data=df)

# women were more likely to not be sample in our sample selection procedure
t.test(female, mu = mean(avars18$female), data=df)

# END