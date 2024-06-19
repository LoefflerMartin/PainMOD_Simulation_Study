# load and install required packages
packs <- c("faux", "lme4", "lmerTest", "tidyr", "tidyverse", "ggplot2", "ggridges", "tictoc")
if (length(setdiff(packs, rownames(installed.packages())))>0) {
  install.packages(setdiff(packs, rownames(installed.packages())))
}
lapply(packs, require, character.only=TRUE); rm(packs)


# WORK PACKAGE 1:

# DEFINE PARAMETERS FOR SIMULATION
h <- 0.8
l <- 0.1

n_subjects_min <- 30
n_subjects_max <- 150
n_iterations <- 1000


# Simulation of data for work package 1
DATA <- data.frame(Po=logical(), S=numeric())
tic("overall the script took the time to run: ")
  
for (S in n_subjects_min:n_subjects_max) {
  tic(paste("AMOUNT OF TIME IT TOOK TO SIMULATE DATA FOR n=", as.character(S), " SUBJECTS and i=", as.character(n_iterations), " ITERATIONS: ", sep = ""))
  Po <- logical()
  for (i in 1:n_iterations) {
    
    #print(paste("running simulation with number of subjects: ", as.character(S), "; and iteration number ", as.character(i), " of ", as.character(n_iterations), sep = ""))
    set.seed(i)
    
    # define covaraince matrix (upper right triangle)
    #         SIA_E SIA_C CPM_E CPM_C EIH_E EIH_C
    rho <- c(       h,    l,    l,    l,    l,    #SIA_E
                          l,    l,    l,    l,    #SIA_C
                                h,    l,    l,    #CPM_E
                                      l,    l,    #CPM_C
                                            h     #EIH_E
                                                  #EIH_C
                  )
    
    # define variable names
    vars <- c("SIA_E", "SIA_C", "CPM_E", "CPM_C", "EIH_E", "EIH_C")
    
    # stimulate data based on covariance matirx
    suppressWarnings(rho_urt <- rnorm_multi(varnames = vars, S, length(vars), r = rho, empirical = F))

    # adding the expected difference between control and experiemntal conditions, based on expected effect sizes
    # see formula 9 in https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3840331/ 
    r_main <- h # expected correlation between E and C in SIA and CPM conditions
    d_main <- 0.5 # expected effect for the difference between (SIA_E and SIA_E) and (CPM_E and EIH_E) respectively
    d_noeffect <- 0.1 # a difference between EIH_C and EIH_E of 0 is unrealistic, so setting it to 0.1
    r_noeffect <- h # expected correlation between E and C in EIH condition
    SD1 <- SD2 <- 1 # data is scaled so all SDs are  always 1
    Mdiff_main <- (d_main * (SD1^2 + SD2^2 - 2 * r_main * SD1 * SD2 )^0.5) / (2*(1-r_main))^0.5
    Mdiff_noeffect <- (d_noeffect * (SD1^2 + SD2^2 - 2 * r_noeffect * SD1 * SD2 )^0.5) / (2*(1-r_noeffect))^0.5
    rho_urt$SIA_E <- rho_urt$SIA_E + Mdiff_main
    rho_urt$CPM_E <- rho_urt$CPM_E + Mdiff_main
    rho_urt$EIH_E <- rho_urt$EIH_C + Mdiff_noeffect
    
    # manipulate dataframe
    rho_urt$ID <- 1:S
    rho_urt_long <- rho_urt %>% gather(stimulus, hypoalgesia, SIA_E:EIH_C, factor_key=TRUE)
    rho_urt_long$condition <- str_split_i(rho_urt_long$stimulus, "_", 2)
    rho_urt_long$stimulus <- str_split_i(rho_urt_long$stimulus, "_", 1)
    
    # create dataframe for mixed effects model of SIA version 2
    rho_urt_long_S2 <- 
      rho_urt_long %>% 
      group_by(ID) %>% 
      mutate(stimulus = factor(stimulus, levels = c('SIA', 'CPM', 'EIH'))) %>% 
      #mutate(
      #  stimulus = last(stimulus)
      #) %>% 
      mutate(condition = factor(condition, levels = c('E', 'C')))
    
    # calculate mixed effects model 
    modelS2 <- lmer(hypoalgesia ~ stimulus * condition  + (1 | ID), data = rho_urt_long_S2, REML = T)
    modelS2_sum <- summary(modelS2)$coefficients
    modelS2_aov <- anova(modelS2)
    
    po <- modelS2_aov[grep('stimulus:condition', rownames(modelS2_aov)), ]$`Pr(>F)` %>% sapply(function(x) x < 0.05)  # is the interaction effect of condition X stimulus significant (p<0.05)?
    Po <- c(Po, po)
  }
  data <- data.frame(Po, S)
  DATA <- rbind(DATA, data)
  toc()
}



# PLOT RESULTS

theme_set(theme_minimal())
DATA <- DATA %>%  mutate(S = factor(S))
POWER <- DATA %>%
  group_by(S) %>%
  summarize(Po = mean(Po, na.rm = TRUE)) %>% 
  mutate(S = as.numeric(as.character(S)))


required_n_95 <- min(POWER$S[POWER$Po > 0.95])
required_n_80 <- min(POWER$S[POWER$Po > 0.80])

ggplot(POWER, aes(x=S)) + 
  geom_area(aes(y = Po), fill = "#E69F00", 
            color = "#E69F00", alpha=0.5) +
  xlab("Number of Subjects") +
  ylab("Statistical Power") +
  geom_hline(yintercept = 0.95, linetype = "dashed", col = "red") +
  geom_hline(yintercept = 0.80, linetype = "dashed", col = "black") +
  geom_vline(xintercept = required_n_95, linetype = "dashed", col = "red") +
  geom_vline(xintercept = required_n_80, linetype = "dashed", col = "black")

toc()

