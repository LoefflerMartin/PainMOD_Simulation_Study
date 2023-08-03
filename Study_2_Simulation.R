# load and install required packages
packs <- c("faux", "lme4", "lmerTest", "tidyr", "tidyverse", "ggplot2", "ggridges", "tictoc")
if (length(setdiff(packs, rownames(installed.packages())))>0) {
  install.packages(setdiff(packs, rownames(installed.packages())))
}
lapply(packs, require, character.only=TRUE); rm(packs)


# WORK PACKAGE 2:

# DEFINE PARAMETERS FOR SIMULATION
h <- 0.55
m <- 0.35
l <- 0.2
n <- 0.1
n_subjects_min <- 10
n_subjects_max <- 150
n_iterations <- 5000


# Simulation of data for work package 2
DATA <- data.frame(Po=logical(), S=numeric())
tic("overall the script took the time to run: ")

for (S in n_subjects_min:n_subjects_max) {
  tic(paste("AMOUNT OF TIME IT TOOK TO SIMULATE DATA FOR n=", as.character(S), " SUBJECTS and i=", as.character(n_iterations), " ITERATIONS: ", sep = ""))
  Pe <- numeric()
  Ps <- numeric()
  Po <- logical()
  for (i in 1:n_iterations) {
    
    #print(paste("running simulation with number of subjects: ", as.character(S), "; and iteration number ", as.character(i), " of ", as.character(n_iterations), sep = ""))
    set.seed(i)
    
    # define covaraince matrix (upper right triangle)
    #   DE1   DE2 DC1 DC2 DS1 DS2 IE1 IE2 IC1 IC2 IS1 IS2 TSK PCS FPQ vHISS AMAS HRE1 HRE2 HRC1 HRC2 HRS1 HRS2 HRVE1 HRVE2 HRVC1 HRVC2 HRVS1 HRVS2 RRE1 RRE2 RRC1 RRC2 RRS1 RRS2
    rho <- c( h,  n,  n,  n,  n,  n,  n,  n,  n,  n,  n,  l,  n,  n,  n,    n,   h,   n,   n,   n,   n,   n,   n,    n,    n,    n,    n,    n,    h,   n,   n,   n,   n,   n,   # DE1
              n,  n,  n,  n,  n,  n,  n,  n,  n,  n,  l,  n,  n,  n,    n,   n,   h,   n,   n,   n,   n,   n,    n,    n,    n,    n,    n,    n,   h,   n,   n,   n,   n,   # DE2
              n,  n,  n,  n,  n,  h,  n,  n,  n,  n,  h,  h,  n,    n,   n,   n,   h,   n,   n,   n,   n,    n,    h,    n,    n,    n,    n,   n,   h,   n,   n,   n,   # DC1
              n,  n,  n,  n,  n,  h,  n,  n,  n,  h,  h,  n,    n,   n,   n,   n,   h,   n,   n,   n,    n,    n,    h,    n,    n,    n,   n,   n,   h,   n,   n,   # DC2
              n,  n,  n,  n,  n,  h,  n,  n,  n,  n,  h,    n,   n,   n,   n,   n,   h,   n,   n,    n,    n,    n,    h,    n,    n,   n,   n,   n,   h,   n,   # DS1
              n,  n,  n,  n,  n,  h,  n,  n,  n,  n,    h,   n,   n,   n,   n,   n,   h,   n,    n,    n,    n,    n,    h,    n,   n,   n,   n,   n,   h,   # DS2
              l,  l,  l,  l,  l,  m,  n,  n,  n,    n,   l,   n,   n,   n,   n,   n,   l,    n,    n,    n,    n,    n,    l,   n,   n,   n,   n,   n,   # IE1
              l,  l,  l,  l,  m,  n,  n,  n,    n,   n,   l,   n,   n,   n,   n,   n,    l,    n,    n,    n,    n,    n,   l,   n,   n,   n,   n,   # IE2
              l,  l,  l,  n,  m,  m,  n,    n,   n,   n,   h,   n,   n,   n,   n,    n,    h,    n,    n,    n,    n,   n,   h,   n,   n,   n,   # IC1
              l,  l,  n,  m,  m,  n,    n,   n,   n,   n,   h,   n,   n,   n,    n,    n,    h,    n,    n,    n,   n,   n,   h,   n,   n,   # IC2
              l,  n,  n,  n,  h,    n,   n,   n,   n,   n,   h,   n,   n,    n,    n,    n,    h,    n,    n,   n,   n,   n,   h,   n,   # IS1
              n,  n,  n,  n,    h,   n,   n,   n,   n,   n,   h,   n,    n,    n,    n,    n,    h,    n,   n,   n,   n,   n,   h,   # IS2
              l,  l,  n,    n,   l,   l,   n,   n,   n,   n,   l,    l,    n,    n,    n,    n,    l,   n,   n,   n,   n,   n,   # TSK
              l,  n,    n,   n,   n,   m,   m,   n,   n,   n,    n,    m,    m,    n,    n,    n,   n,   m,   m,   n,   n,   # PCS
              n,    n,   n,   n,   m,   m,   n,   n,   n,    n,    m,    m,    n,    n,    n,   n,   m,   m,   n,   n,   # FPQ
              n,   n,   n,   n,   n,   m,   n,   n,    n,    n,    n,    m,    n,    n,   n,   n,   n,   m,   n,   # vHISS
              n,   n,   n,   n,   n,   m,   n,    n,    n,    n,    n,    m,    n,   n,   n,   n,   n,   m,   # AMAS
              n,   n,   n,   n,   n,   m,    n,    n,    n,    n,    n,    m,   n,   n,   n,   n,   n,   # HRE1
              n,   n,   n,   n,   n,    m,    n,    n,    n,    n,    n,   m,   n,   n,   n,   n,   # HRE2
              n,   n,   n,   n,    n,    m,    n,    n,    n,    n,   n,   m,   n,   n,   n,   # HRC1
              n,   n,   n,    n,    n,    m,    n,    n,    n,   n,   n,   m,   n,   n,   # HRC2
              n,   n,    n,    n,    n,    m,    n,    n,   n,   n,   n,   m,   n,   # HRS1
              n,    n,    n,    n,    n,    m,    n,   n,   n,   n,   n,   m,   # HRS2
              n,    n,    n,    n,    n,    m,   n,   n,   n,   n,   n,   # HRVE1
              n,    n,    n,    n,    n,   m,   n,   n,   n,   n,   # HRVE2
              n,    n,    n,    n,   n,   m,   n,   n,   n,   # HRVC1
              n,    n,    n,   n,   n,   m,   n,   n,   # HRVC2
              n,    n,   n,   n,   n,   m,   n,   # HRVS1
              n,   n,   n,   n,   n,   m,   # HRVS2
              n,   n,   n,   n,   n,   # RRE1
              n,   n,   n,   n,   # RRE2
              n,   n,   n,   # RRC1
              n,   n,   # RRC2
              n)   # RRS1
    # RRS2
    
    # define variable names
    vars <- c("DE1", "DE2", "DC1", "DC2", "DS1", "DS2", "IE1", "IE2", "IC1", "IC2", "IS1", "IS2",
              "TSK", "PCS", "FPQ", "vHISS", "AMAS",
              "HRE1", "HRE2", "HRC1", "HRC2", "HRS1", "HRS2",
              "HRVE1", "HRVE2", "HRVC1", "HRVC2", "HRVS1", "HRVS2",
              "RRE1", "RRE2", "RRC1", "RRC2", "RRS1", "RRS2")
    
    # stimulate data based on covariance matirx
    suppressWarnings(rho_urt <- rnorm_multi(varnames = vars, S, length(vars), r = rho, empirical = F))
    #get_params(rho_urt)
    
    
    # manipulate dataframe
    rho_urt$ID <- 1:S
    rho_urt_long_D <- rho_urt %>% select (starts_with("D") | "ID") %>% gather(stimulus, hypoalgesia, DE1:DS2, factor_key=TRUE)
    rho_urt_long_D$stimulus <- gsub("D", "", rho_urt_long_D$stimulus)
    rho_urt_long_I <- rho_urt %>% select (starts_with("I")) %>% gather(stimulus, imminence, IE1:IS2, factor_key=TRUE)
    rho_urt_long_I$stimulus <- gsub("I", "", rho_urt_long_D$stimulus)
    rho_urt_long_HR <- rho_urt %>% select (starts_with("HR") & !(starts_with("HRV")) | "ID") %>% gather(stimulus, HR, HRE1:HRS2, factor_key=TRUE)
    rho_urt_long_HR$stimulus <- gsub("HR", "", rho_urt_long_D$stimulus)
    rho_urt_long_HRV <- rho_urt %>% select (starts_with("HRV") | "ID") %>% gather(stimulus, HRV, HRVE1:HRVS2, factor_key=TRUE)
    rho_urt_long_HRV$stimulus <- gsub("HRV", "", rho_urt_long_D$stimulus)
    rho_urt_long_RR <- rho_urt %>% select (starts_with("RR") | "ID") %>% gather(stimulus, RR, RRE1:RRS2, factor_key=TRUE)
    rho_urt_long_RR$stimulus <- gsub("RR", "", rho_urt_long_D$stimulus)
    
    rho_urt_long <- left_join(rho_urt_long_D, rho_urt_long_I, c("ID", "stimulus"))
    rho_urt_long <- left_join(rho_urt_long, rho_urt_long_HR, c("ID", "stimulus"))
    rho_urt_long <- left_join(rho_urt_long, rho_urt_long_HRV, c("ID", "stimulus"))
    rho_urt_long <- left_join(rho_urt_long, rho_urt_long_RR, c("ID", "stimulus"))
    
    # create dataframe for mixed effects model of SIA version 2
    rho_urt_long_S2 <- 
      rho_urt_long %>% 
      group_by(ID) %>% 
      mutate(
        imminence = last(imminence),
        HR = first(HR),
        HRV = first(HRV),
        RR = first(RR)
      ) %>% 
      mutate(stimulus = factor(stimulus, levels = c('S2', 'E1', 'E2', 'C1', 'C2', 'S1')))
    
    # calculate mixed effects model 
    modelS2 <- lmer(hypoalgesia ~ stimulus * (imminence + HR + HRV + RR)  + (1 | ID), data = rho_urt_long_S2, REML = T)
    modelS2_sum <- summary(modelS2)$coefficients
    modelS2_aov <- anova(modelS2)
    
    po <- modelS2_aov[grep('imminence', rownames(modelS2_aov)), ]$`Pr(>F)` %>% sapply(function(x) x < 0.05)  %>% as.numeric() %>% sum() %>% `==`(2) # are both the main effect of imminence and the interaction effect of imminence X stimulus significant (p<0.05)?
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

