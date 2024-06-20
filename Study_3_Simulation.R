# load and install required packages
packs <- c("faux", "lme4", "lmerTest", "tidyr", "tidyverse", "ggplot2", "ggridges", "tictoc")
if (length(setdiff(packs, rownames(installed.packages())))>0) {
  install.packages(setdiff(packs, rownames(installed.packages())))
}
lapply(packs, require, character.only=TRUE); rm(packs)


# WORK PACKAGE 3:

# DEFINE PARAMETERS FOR SIMULATION
h <- 0.8
m <- 0.5
l <- 0.1

n_subjects_min <- 30
n_subjects_max <- 300
n_iterations <- 1000


# Simulation of data for work package 3
DATA <- data.frame(Po=logical(), S=numeric())
tic("overall the script took the time to run: ")

for (S in n_subjects_min:n_subjects_max) {
  tic(paste("AMOUNT OF TIME IT TOOK TO SIMULATE DATA FOR n=", as.character(S), " SUBJECTS and i=", as.character(n_iterations), " ITERATIONS: ", sep = ""))
  Po <- logical()
  for (i in 1:n_iterations) {
    
    #print(paste("running simulation with number of subjects: ", as.character(S), "; and iteration number ", as.character(i), " of ", as.character(n_iterations), sep = ""))
    set.seed(i)
    
    # define covaraince matrix (upper right triangle)
    #         SIA CPM EIH
    rho <- c(     l,  l,    #CPM
                      l     #EIH
                            )
    
    # define variable names
    vars <- c("SIA", "CPM", "EIH")
    
    # stimulate data based on covariance matirx
    # in WP3 we have two independent groups, data is therefore simulated independently
    # Experimental group
    suppressWarnings(rho_urt_exp <- rnorm_multi(varnames = vars, S, length(vars), r = rho, empirical = F))
    # Control group
    suppressWarnings(rho_urt_con <- rnorm_multi(varnames = vars, S, length(vars), r = rho, empirical = F))


    # subtracting the expected difference between control and experiemntal group from the control group, based on expected effect sizes
    # see formula 1 in https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3840331/
    d_main <- 0.5 # expected effect for the difference between EIH_C and EIH_E
    d_noeffect <- 0.1 # a difference of 0 for the difference between (SIA_E and SIA_E) and (CPM_E and EIH_E) is unrealistic, therfore setting it to 0.1
    SD1 <- SD2 <- 1 # data is scaled so all SDs are  always 1
    n1 <- n2 <- S # n needs to be changed in each iteration based on S
    
    Mdiff_main <- d_main * (((n1-1)*SD1^2 + (n2-1)^2)/ n1+n2-2 ) ^ 0.5
    Mdiff_noeffect <- d_noeffect * (((n1-1)*SD1^2 + (n2-1)^2)/ n1+n2-2 ) ^ 0.5
    
    rho_urt_con$EIH <- rho_urt_con$EIH - d_main
    rho_urt_con$SIA <- rho_urt_con$SIA - d_noeffect
    rho_urt_con$CPM <- rho_urt_con$CPM - d_noeffect

    
    # manipulate dataframe
    rho_urt_con$group <- "control"
    rho_urt_exp$group <- "experimental"
    rho_urt_con$ID <- 1:S
    rho_urt_exp$ID <- S+1:S
    rho_urt <- rbind(rho_urt_con, rho_urt_exp)
    rho_urt_long <- rho_urt %>% gather(stimulus, hypoalgesia, SIA:EIH, factor_key=TRUE)
    
    # create dataframe for mixed effects model
    rho_urt_long_me <- 
      rho_urt_long %>% 
      group_by(ID) %>% 
      mutate(stimulus = factor(stimulus, levels = c('SIA', 'CPM', 'EIH'))) %>% 
      #mutate(
      #  stimulus = last(stimulus)
      #) %>% 
      mutate(group = factor(group, levels = c('control', 'experimental')))
    
    # calculate mixed effects model 
    modelme <- lmer(hypoalgesia ~ group * stimulus  + (1 | ID), data = rho_urt_long_me, REML = T)
    modelme_sum <- summary(modelme)$coefficients
    modelme_aov <- anova(modelme)
    
    po <- modelme_aov[grep('group:stimulus', rownames(modelme_aov)), ]$`Pr(>F)` %>% sapply(function(x) x < 0.05)  # is the interaction effect of condition X stimulus significant (p<0.05)?
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

p <- ggplot(POWER, aes(x=S)) + 
  geom_area(aes(y = Po), fill = "#E69F00", 
            color = "#E69F00", alpha=0.5) +
  xlab("Number of Subjects") +
  ylab("Statistical Power") +
  geom_hline(yintercept = 0.95, linetype = "dashed", col = "red") +
  geom_hline(yintercept = 0.80, linetype = "dashed", col = "black") +
  geom_vline(xintercept = required_n_95, linetype = "dashed", col = "red") +
  geom_vline(xintercept = required_n_80, linetype = "dashed", col = "black") +
  geom_label( label=paste("n80=\n", as.character(required_n_80), sep = ""), 
              x=required_n_80, y=0.90, size =2,
              label.padding = unit(0.55, "lines"), label.size = 0.35,
              color = "black", fill="#69b3a2")+
  geom_label( label=paste("n95=\n", as.character(required_n_95), sep = ""), 
              x=required_n_95, y=1.05, size =2,
              label.padding = unit(0.55, "lines"), label.size = 0.35,
              color = "red", fill="#69b3a2")


setwd("C:/Martin/Code/R/PainMOD_Simulation_Study")
ggsave("Study_3_required_sample.png", p, width = 4, height = 3)

toc()

