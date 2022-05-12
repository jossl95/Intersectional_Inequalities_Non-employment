#---------------------------------------------------
install.packages("tidySEM", dependencies = TRUE)
install.packages("foreign", dependencies = TRUE)
install.packages("haven", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("semTools", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("lavaan", dependencies = TRUE)
install.packages("semTable", dependencies = TRUE)
install.packages("tidyverse")
#---------------------------------------------------

#open packages 
library(tidySEM)
library(foreign)
library(haven)
library(tidyr)
#library("ggplot2")
library(lavaan)
library(semTools)
#library(psych)
library(semTable)

#merged2 voor scriptie definitie (N=2770); 
#merged3 voor aanvullende analyses met strakkere definitie werkloosheid (N=2482)
LISS <- read_sav("merged_MM2.sav")
LISS$female <- as.numeric(LISS$female)
LISS$unemp <- as.numeric(LISS$unemp)

summary(LISS)

#Normaalverdeling numerieke variabelen
normal_plot <- LISS[ , c(7,8,14)]
normal_plot <- pivot_longer(normal_plot, names(normal_plot))
ggplot(normal_plot, aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free_x")

#correlation matrix
corr <- LISS[ , c(6,8,3:5,2,11,10,7,18)]
corrmat <- round(cor(LISS),3)
psych::corr.test(corrmat) #correlation matrix all variables
psych::corr.test(corrmat, method = "pearson")

## MODEL SPECIFICATION:
#model 0 'leeg' model. Enkel controlevariabelen
model0 <- '#control variables
            unemp ~ child + educ + numcon + 0
           
           #covariates
            numwork ~~ numcon
            unemp ~~ numwork
            unemp ~~ female
            unemp ~~ west
            unemp ~~ nonwest'
fit_mod0 <- sem(model0, data = LISS)
summary(fit_mod0, fit.measures = TRUE)

# Not including covariates
model0a <- '#control variables
            unemp ~ child + educ + numcon'

fit_mod0a <- sem(model0a, data = LISS)
summary(fit_mod0a, fit.measures = TRUE)

#model 1 Controlevariabelen + sociaal kapitaal
model1 <- '#direct effect
            unemp ~ numwork + 
           #control variables 
            child + educ + numcon
          
           #covariate resid.var soc-cap  
            numwork ~ numcon
            
           #covariates
            unemp ~~ female 
            unemp ~~ west
            unemp ~~ nonwest'
fit_mod1 <- sem(model1, data = LISS)
summary(fit_mod1, fit.measures = TRUE)

#model 1 Controlevariabelen + sociaal kapitaal
model1a <- '#direct effect
             unemp ~ numwork + 
            #control variables 
             child + educ + numcon'
fit_mod1a <- sem(model1a, data = LISS)
summary(fit_mod1a, fit.measures = TRUE)
# num con? covar

#model 2 Controlevariabelen + sociaal kapitaal + sekse + etniciteit zonder indirecte effecten
model2 <- '#direct effect
            unemp ~ female + west + nonwest + numwork + 
           #control variables
            child + educ + numcon

           #covariate resid.var soc-cap  
            numwork ~ numcon'

fit_mod2 <- sem(model2, data = LISS)
summary(fit_mod2, fit.measures = TRUE)

#model 2 Controlevariabelen + sociaal kapitaal 
model2b <- '#direct effect
            unemp ~  numwork + 
           #control variables
            child + educ + numcon'

fit_mod2b <- sem(model2b, data = LISS)
summary(fit_mod2b, fit.measures = TRUE)

#model 3 Controlevariabelen + sociaal kapitaal + sekse + etniciteit met indirecte effecten
model3 <- '#direct effect
            unemp ~ fy * female + wy * west + nwy * nonwest + my * numwork + 
            #control variables
            child + educ + numcon
               
           #indirect effect
            numwork ~ wm * west + nwm * nonwest + fm * female + numcon
           
           #effects
            ind_sex := fm * my
            ind_west := wm * my
            ind_nonwest := nwm * my
            
            tot_sex := ind_sex + fy
            tot_west := ind_west + wy
            tot_nonwest := ind_nonwest + nwy '
fit_mod3 <- sem(model3, data = LISS)
summary(fit_mod3, fit.measures = TRUE)

#model 4 Intersectionele verschillen zonder mediatie
model4 <- '#direct effect
            unemp ~ fy * female + wy * west + nwy * nonwest + my * numwork +
            #control
            child + educ + numcon +
            #interaction
            fwy * f_w + fnwy * f_nw
            
           #indirect effect
            numwork ~ wm * west + nwm * nonwest + fm * female +
            numcon
            
           #effects
            fsex := fy
            fwest := wy + fwy + fy
            fnonwest := nwy + fnwy + fy
            
            mwest := wy
            mnonwest := nwy'

model4 <- '#direct effect
            unemp ~ fy * female + wy * west + nwy * nonwest + my * numwork +
            #control
            child + educ + numcon +
            #interaction
            fwy * f_w + fnwy * f_nw
            
           #indirect effect
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

fit_mod4 <- sem(model4, data = LISS)
summary(fit_mod4, fit.measures = TRUE)

#Model 5 Intersectionele effecten inclusief mediatie
model5 <- '#direct effect
            unemp ~ fy * female + wy * west + nwy * nonwest + my * numwork +
            #control
            child + educ + numcon +
            #interaction
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
fit_mod5 <- sem(model5, data = LISS)
summary(fit_mod5, fit.measures = TRUE)

## MODEL FIT
modcom.dir = compareFit(fit_mod0a, fit_mod1a, fit_mod2)
modcom.cap = compareFit(fit_mod0a, fit_mod2b)
modcom.ind = compareFit(fit_mod3, fit_mod4, fit_mod5)

summary(modcom.cap)

#eventuele suggesties voor verbeteringen van het model
mod_ind <- modificationindices(fit_mod5)
subset(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], mi > 5)

## MODEL RESTULS:
mods = list(
  "0" = fit_mod0a,
  "1" = fit_mod1a,
  "2" = fit_mod2, 
  "3" = fit_mod3,
  "4" = fit_mod4,
  "5" = fit_mod5
)
dir ="/Users/MacbookJos/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Documents/_Publications/2021/Int_Unemp"
semTable(
  mods,
  columns=c("estsestars", "p"), columnLabels=c('estsestars' = "Estimate(Std. Err.)"), 
  type="csv",
  file=file.path(dir, 'res_noboot.csv')
)


## BOOTSTRAP
k = 1000
# model 0
fit_mod0a_bs <- sem(model0a, data = LISS, se = "bootstrap", bootstrap = k)
summary(fit_mod0a_bs, fit.measures = TRUE)

# model 1
fit_mod1a_bs <- sem(model1a, data = LISS, se = "bootstrap", bootstrap = k)
summary(fit_mod1a_bs, fit.measures = TRUE)

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

## MODEL RESTULS:
mods = list(
  "0" = fit_mod0a_bs,
  "1" = fit_mod1a_bs,
  "2" = fit_mod2_bs, 
  "3" = fit_mod3_bs,
  "4" = fit_mod4_bs,
  "5" = fit_mod5_ns
)

dir ="/Users/MacbookJos/Library/CloudStorage/OneDrive-UniversiteitUtrecht/Documents/_Publications/2021/Int_Unemp"
semTable(
  mods,
  columns=c("estsestars", "p"), columnLabels=c('estsestars' = "Estimate(Std. Err.)"), 
  type="csv",
  file=file.path(dir, 'res_boot.csv')
)

## ROBUSTNES CHECKS
ROB <- read_sav("merged3.sav")
ROB$female <- as.numeric(ROB$female)

# use correct independent variables
ROB <-merge(subset(LISS, select=-unemp),
      ROB[c('nomem_encr', 'unemp')],
      by='nomem_encr')

summary(ROB)

## MODEL SPECIFICATION:
#model 0 'leeg' model. Enkel controlevariabelen
model0ra <- '#control variables
            unemp ~ child + educ + numcon'

fit_mod0ra <- sem(model0ra, data = ROB)
summary(fit_mod0ra, fit.measures = TRUE)

#model 1 Controlevariabelen + sociaal kapitaal
model1ra <- '#direct effect
             unemp ~ numwork + 
            #control variables 
             child + educ + numcon

            #covariate resid.var soc-cap  
             numwork ~ numcon'
fit_mod1ra <- sem(model1ra, data = ROB)
summary(fit_mod1ra, fit.measures = TRUE)

#model 2 Controlevariabelen + sociaal kapitaal + sekse + etniciteit zonder indirecte effecten
model2r <- '#direct effect
            unemp ~ female + west + nonwest + numwork + 
           #control variables
            child + educ + numcon

           #covariate resid.var soc-cap  
            numwork ~ numcon'

fit_mod2r <- sem(model2r, data = ROB)
summary(fit_mod2r, fit.measures = TRUE)

#model 3 Controlevariabelen + sociaal kapitaal + sekse + etniciteit met indirecte effecten
model3r <- '#direct effect
            unemp ~ fy * female + wy * west + nwy * nonwest + my * numwork + 
            #control variables
            child + educ + numcon
               
           #indirect effect
            numwork ~ wm * west + nwm * nonwest + fm * female + numcon
           
           #effects
            ind_sex := fm * my
            ind_west := wm * my
            ind_nonwest := nwm * my
            
            tot_sex := ind_sex + fy
            tot_west := ind_west + wy
            tot_nonwest := ind_nonwest + nwy '
fit_mod3r <- sem(model3r, data = ROB)
summary(fit_mod3r, fit.measures = TRUE)

#model 4 Intersectionele verschillen zonder mediatie
model4r <- '#direct effect
            unemp ~ fy * female + wy * west + nwy * nonwest + my * numwork +
            #control
            child + educ + numcon +
            #interaction
            fwy * f_w + fnwy * f_nw
            
           #indirect effect
            numwork ~ wm * west + nwm * nonwest + fm * female +
            numcon
            
           #effects
            fsex := fy
            fwest := wy + fwy
            fnonwest := nwy + fnwy
            
            mwest := wy
            mnonwest := nwy'
fit_mod4r <- sem(model4r, data = ROB)
summary(fit_mod4r, fit.measures = TRUE)

#Model 5 Intersectionele effecten inclusief mediatie
model5r <- '#direct effect
            unemp ~ fy * female + wy * west + nwy * nonwest + my * numwork +
            #control
            child + educ + numcon +
            #interaction
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
fit_mod5r <- sem(model5r, data = ROB)
summary(fit_mod5r, fit.measures = TRUE)

mods = list(
  "0" = fit_mod0ra,
  "1" = fit_mod1ra,
  "2" = fit_mod2r, 
  "3" = fit_mod3r,
  "4" = fit_mod4r,
  "5" = fit_mod5r
)
semTable(
  mods,
  columns=c("estsestars", "p"), columnLabels=c('estsestars' = "Estimate(Std. Err.)"), 
  type="csv",
  file=file.path(dir, 'res_ROB_noboot.csv')
)


