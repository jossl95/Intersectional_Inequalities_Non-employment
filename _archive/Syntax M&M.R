#---------------------------------------------------
install.packages("tidySEM", dependencies = TRUE)
install.packages("foreign", dependencies = TRUE)
install.packages("haven", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("semTools", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("lavaan", dependencies = TRUE)
#---------------------------------------------------

#open packages 

library(tidySEM)
library(foreign)
library(haven)
library(tidyr)
library(ggplot2)
library(lavaan)
library(semTools)
library(psych)

#merged2 voor scriptie definitie (N=2770); 
#merged3 voor aanvullende analyses met strakkere definitie werkloosheid (N=2482)
LISS <- read_sav("merged2.sav")

descriptives(LISS)

LISS$female <- as.numeric(LISS$female)
LISS$unemp <- as.numeric(LISS$unemp)

descriptives(LISS)

  #Normaalverdeling numerieke variabelen
normal_plot <- LISS[ , c(7,8,14)]
normal_plot <- pivot_longer(normal_plot, names(normal_plot))
ggplot(normal_plot, aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free_x")

  #correlation matrix
corr <- LISS[ , c(6,8,3:5,2,11,10,7,18)]
corrmat <- round(cor(corr),2)
psych::corr.test(corrmat) #correlation matrix all variables
psych::corr.test(corrmat, method = "pearson")

  #path models. Elk model eerst zonder en daarna met bootstrap.

#model 0 'leeg' model. Enkel controlevariabelen
model0 <- '#control variables
            unemp ~ child + educ + numcon
           
           #covariates
            numwork ~~ numcon
            unemp ~~ numwork
            unemp ~~ female
            unemp ~~ west
            unemp ~~ nonwest'
fit_mod0 <- sem(model0, data = LISS)
summary(fit_mod0, fit.measures = TRUE)

#fit_mod0_bs <- sem(model0, data = LISS, se = "bootstrap", bootstrap = 10000)
#summary(fit_mod0_bs, fit.measures = TRUE)

#model 1 Controlevariabelen + sociaal kapitaal
model1 <- '#direct effect
            unemp ~ numwork + 
           #control variables 
            child + educ + numcon
            
            numwork ~ numcon
            
           #covariates
            unemp ~~ female 
            unemp ~~ west
            unemp ~~ nonwest'
fit_mod1 <- sem(model1, data = LISS)
summary(fit_mod1, fit.measures = TRUE)

#fit_mod1_bs <- sem(model2, data = LISS, se = "bootstrap", bootstrap = 10000)
#summary(fit_mod1_bs, fit.measures = TRUE)

#model 2 Controlevariabelen + sociaal kapitaaal + sekse + etniciteit zonder indirecte effecten
model2 <- '#direct effect
            unemp ~ female + west + nonwest + numwork + 
            #control variables
            child + educ + numcon 
            
            numwork ~ numcon'
fit_mod2 <- sem(model2, data = LISS)
summary(fit_mod2, fit.measures = TRUE)


#model 3 Controlevariabelen + sociaal kapitaal + sekse + etniciteit met indirecte effecten
model3 <- '#direct effect
            unemp ~ fy * female + wy * west + nwy * nonwest + my * numwork + 
            #control variables
            child + educ + numcon
               
           #indirect effect
            numwork ~ wm * west + nwm * nonwest + fm * female + numcon
            
           #covariates
            
           #effects
            ind_sex := fm * my
            ind_west := wm * my
            ind_nonwest := nwm * my
            
            tot_sex := ind_sex + fy
            tot_west := ind_west + wy
            tot_nonwest := ind_nonwest + nwy '
fit_mod3 <- sem(model3, data = LISS)
summary(fit_mod3, fit.measures = TRUE)

#fit_mod3_bs <- sem(model3, data = LISS, se = "bootstrap", bootstrap = 10000)
#summary(fit_mod3_bs, fit.measures = TRUE)

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
            
           #covariates
            
           #direct effects
            fsex := fy
            fwest := wy + fwy
            fnonwest := nwy + fnwy
            
            msex := 0 * fy
            mwest := wy
            mnonwest := nwy'
fit_mod4 <- sem(model4, data = LISS)
summary(fit_mod4, fit.measures = TRUE)

#fit_mod4_bs <- sem(model4, data = LISS, se = "bootstrap", bootstrap = 10000)
#summary(fit_mod4_bs, fit.measures = TRUE)
#parameterestimates(fit_mod4_bs, boot.ci.type = "bca.simple")

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
            
           #covariates
          
           #direct effects
            fsex := fy
            fwest := wy + fwy
            fnonwest := nwy + fnwy
            msex := my
            mwest := wy
            mnonwest := nwy
            
           #on numwork for women
            fwestm := wm + fwm
            fnonwestm := nwm + fnwm
            
           #ind effects
            ind_fsex := 1 * fm * my
            ind_fwest := 1 * fm * my + 1 * wm * my + 1 * fwm * my
            ind_fnonwest := 1 * fm * my + 1 * nwm * my + 1 * fnwm * my
            ind_msex := 0 * fm * my + 0 * wm * my + 0 * nwm * my
            ind_mwest := 1 * wm * my
            ind_mnonwest := 1 * nwm * my'
fit_mod5 <- sem(model5, data = LISS)
summary(fit_mod5, fit.measures = TRUE)

#fit_mod5_bs <- sem(model5, data = LISS, se = "bootstrap", bootstrap = 10000)
#summary(fit_mod5_bs, fit.measures = TRUE)
#parameterestimates(fit_mod4_bs, boot.ci.type = "bca.simple")

#eventuele suggesties voor verbeteringen van het model
mod_ind <- modificationindices(fit_mod5)
subset(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], mi > 5)

