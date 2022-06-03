#------------------------------------------------------------------------------
# PROJECT TITLE: 
# Intersectionele sekse- en herkomstverschillen in niet-werken:Een verklaring 
# op basis van sociaal kapitaal.
#
# SCRIPT: Analyses
#
# DATE: 09/05/2022
# NAME:
# - Jop Roeleven (j.e.roeleven@students.uu.nl)
# - Jos Slabbekoorn (j.slabbekoorn@uu.nl)
#------------------------------------------------------------------------------
# load packages
library(tidySEM)
library(foreign)
library(haven)
library(tidyr)
library(lavaan)
library(semTools)
library(semTable)
library(psych)
library(tidyverse)

# make directory if not existent
conditional_make_dir <- function(path){
  if(!file.exists(path)){
    dir.create(path, showWarnings = FALSE)
  }
}
#------------------------------------------------------------------------------
# load data
LISS <- haven::read_sav("data/merged.sav")

# descriptive statistics
LISS %>% 
  select(unemp, female, dutch, west, nonwest, 
         numwork, numcon, age, child, educ) %>% 
  describe() %>%  print(., digits=3)

#correlation matrix
corrmat <- round(cor(LISS),3)
psych::corr.test(corrmat, method = "pearson") %>%  print(., digits=3)

#-----------------------------------------------------------------------------
# model specification
#-----------------------------------------------------------------------------
#model 0: 'leeg' model. Enkel controlevariabelen
model0 <- '#control variables
            unemp ~ child + educ + numcon'

# model 1: Controlevariabelen + sociaal kapitaal
model1 <- '#direct effects
             unemp ~ numwork + 
            #control variables 
             child + educ + numcon'

# model 2: Controlevariabelen + sociaal kapitaal + sekse + etniciteit zonder 
#          indirecte effecten
model2 <- '#direct effects
            unemp ~ female + west + nonwest + numwork + 
           #control variabless
            child + educ + numcon

           #covariate resid.var soc-cap  
            numwork ~ numcon'

# model 3 Controlevariabelen + sociaal kapitaal + sekse + etniciteit met 
#         indirecte effecten
model3 <- '#direct effects
            unemp ~ fy * female + wy * west + nwy * nonwest + my * numwork + 
            #control variables
            child + educ + numcon
               
           #indirect effects
            numwork ~ wm * west + nwm * nonwest + fm * female + numcon
           
           #effects
            ind_sex := fm * my
            ind_west := wm * my
            ind_nonwest := nwm * my
            
            tot_sex := ind_sex + fy
            tot_west := ind_west + wy
            tot_nonwest := ind_nonwest + nwy '

# model 4: Intersectional effects without mediation maths
model4 <- '#direct effects
            unemp ~ fy * female + wy * west + nwy * nonwest + my * numwork +
            #control
            child + educ + numcon +
            #interaction
            fwy * f_w + fnwy * f_nw
            
           #indirect effects
            numwork ~ wm * west + nwm * nonwest + fm * female +
            numcon + fwm * f_w + fnwm * f_nw
            
            fwm == 0
            fnwm == 0
            wm == 0 
            nwm == 0 
            fm == 0
            
           #effects (ref. msex)
            dir_mwest := wy
            dir_mnonwest := nwy
            dir_fsex := fy
            dir_fwest := fy + wy + fwy
            dir_fnonwest := fy + nwy + fnwy
            
           #ind effects
            ind_mwest := (wm) * my
            ind_mnonwest := (nwm) * my
            ind_fsex := (fm) * my
            ind_fwest := (fm + wm + fwm) * my
            ind_fnonwest := (fm + nwm + fnwm) * my

           #tot effects
            tot_mwest := dir_mwest + ind_mwest
            tot_mnonwest := dir_mnonwest + ind_mnonwest
            tot_fsex := dir_fsex + ind_fsex
            tot_fwest := dir_fwest + ind_fwest
            tot_fnonwest := dir_fnonwest + ind_fnonwest'

# Model 5: Intersectional effects including mediations paths
model5 <- '#direct effects
            unemp ~ fy * female + wy * west + nwy * nonwest + my * numwork +
            #controls
            child + educ + numcon +
            #interactions
            fwy * f_w + fnwy * f_nw
            
           #indirect effect
            numwork ~ wm * west + nwm * nonwest + fm * female +
            numcon + fwm * f_w + fnwm * f_nw
            
           #effects (ref. msex)
            dir_mwest := wy
            dir_mnonwest := nwy
            dir_fsex := fy
            dir_fwest := fy + wy + fwy
            dir_fnonwest := fy + nwy + fnwy
            
           #ind effects
            ind_mwest := (wm) * my
            ind_mnonwest := (nwm) * my
            ind_fsex := (fm) * my
            ind_fwest := (fm + wm + fwm) * my
            ind_fnonwest := (fm + nwm + fnwm) * my

           #tot effects
            tot_mwest := dir_mwest + ind_mwest
            tot_mnonwest := dir_mnonwest + ind_mnonwest
            tot_fsex := dir_fsex + ind_fsex
            tot_fwest := dir_fwest + ind_fwest
            tot_fnonwest := dir_fnonwest + ind_fnonwest'
#-----------------------------------------------------------------------------
## Run models using a 1000-fold bootstrap 
#-----------------------------------------------------------------------------
k = 1000
set.seed(291)

# model 0
fit_mod0_bs <- sem(model0, data = LISS, se = "bootstrap", bootstrap = k)
summary(fit_mod0_bs, fit.measures = TRUE)

# model 1
fit_mod1_bs <- sem(model1, data = LISS, se = "bootstrap", bootstrap = k)
summary(fit_mod1_bs, fit.measures = TRUE)

# model 2
fit_mod2_bs <- sem(model2, data = LISS, se = "bootstrap", bootstrap = k)
summary(fit_mod2_bs, fit.measures = TRUE)

# model 3
fit_mod3_bs <- sem(model3, data = LISS, se = "bootstrap", bootstrap = k)
summary(fit_mod3_bs, fit.measures = TRUE)

# model 4
fit_mod4_bs <- sem(model4, data = LISS, se = "bootstrap", bootstrap = k)
summary(fit_mod4_bs, fit.measures = TRUE)
parameterestimates(fit_mod4_bs, boot.ci.type = "bca.simple")

# model 5
fit_mod5_bs <- sem(model5, data = LISS, se = "bootstrap", bootstrap = k)
summary(fit_mod5_bs, fit.measures = TRUE)
parameterEstimates(fit_mod5_bs, boot.ci.type = "bca.simple")

# generate results table
mods = list(
  "0" = fit_mod0_bs,
  "1" = fit_mod1_bs,
  "2" = fit_mod2_bs, 
  "3" = fit_mod3_bs,
  "4" = fit_mod4_bs,
  "5" = fit_mod5_bs
)

conditional_make_dir("./results")
semTable(
  mods,
  columns=c("estsestars", "p"), 
  columnLabels=c('estsestars' = "Estimate(Std. Err.)"), 
  type="csv",
  file='./results/bootstrap.csv'
)

#-----------------------------------------------------------------------------
# robustness analyses
#-----------------------------------------------------------------------------
ROB <- LISS %>% 
  filter(robfilter == 0)

summary(ROB)

# model 0
fit_mod0r <- sem(model0, data = ROB, se = "bootstrap", bootstrap = k)
summary(fit_mod0r, fit.measures = TRUE)

# model 1
fit_mod1r <- sem(model1, data = ROB, se = "bootstrap", bootstrap = k)
summary(fit_mod1r, fit.measures = TRUE)

# model 2
fit_mod2r <- sem(model2, data = ROB, se = "bootstrap", bootstrap = k)
summary(fit_mod2r, fit.measures = TRUE)

# model 3
fit_mod3r <- sem(model3, data = ROB, se = "bootstrap", bootstrap = k)
summary(fit_mod3r, fit.measures = TRUE)

# model 4
fit_mod4r <- sem(model4, data = ROB, se = "bootstrap", bootstrap = k)
summary(fit_mod4r, fit.measures = TRUE)

# model 5
fit_mod5r <- sem(model5, data = ROB, se = "bootstrap", bootstrap = k)
summary(fit_mod5r, fit.measures = TRUE)

mods = list(
  "0" = fit_mod0r,
  "1" = fit_mod1r,
  "2" = fit_mod2r, 
  "3" = fit_mod3r,
  "4" = fit_mod4r,
  "5" = fit_mod5r
)

semTable(
  mods,
  columns=c("estsestars", "p"), 
  columnLabels=c('estsestars' = "Estimate(Std. Err.)"), 
  type="csv",
  file='./results/robustness.csv'
)
# END


