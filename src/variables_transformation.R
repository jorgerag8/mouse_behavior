library("tidyverse")
library("roll")

# Get tabular data of all trials
behavior = read_csv("/Users/jorgerag/Documents/UCSD/courses/capstone/data/behavior/behavior_data.csv")

# Selecting variables of interest
behavior = behavior %>% 
  mutate(threshold = ifelse(grepl("_1600_", File), 160, 80)) %>% 
  select(-c(File,A:Z, Experiment))

# Create variables with order of actions and inter press intervals
behavior = behavior %>% 
  mutate(succ_lp = ifelse(time >= threshold,1,0),
         succ_lp = ifelse(actions == 150, succ_lp, NA)) %>% 
  group_by(Subject, MSN, `Start Date`, `Start Time`, threshold) %>% 
  mutate(action_order = row_number()) %>% 
  group_by(Subject, MSN, `Start Date`, `Start Time`, threshold, actions) %>% 
  mutate(lp_order = row_number(),
         lp_order = ifelse(actions == 150, lp_order, NA),
         he_order = row_number(),
         he_order = ifelse(actions == 130, he_order, NA),
         aux = row_number(),
         aux = ifelse(actions == 100, aux, NA)) %>% 
  ungroup() %>% 
  fill(aux, .direction = "up") %>% 
  group_by(Subject, MSN, `Start Date`, `Start Time`, threshold, aux) %>% 
  mutate(ipi = ifelse(aux == 1, NA, sum(time))) %>% 
  ungroup() %>% 
  mutate(ipi = ifelse(actions == 100, ipi, NA)) %>% 
  select(-aux)

# Inter press interval n-2
behavior = behavior %>% 
  group_by(Subject, MSN, `Start Date`, `Start Time`, threshold) %>% 
  mutate(aux = ipi) %>% 
  fill(aux, .direction = "down") %>% 
  mutate(aux = ifelse(actions == 100, lag(aux, n=1), NA),
         ipi_n2 = ipi + aux) %>% 
  select(-aux)

# Past lever presses and MA
behavior = behavior %>% 
  group_by(Subject, MSN, `Start Date`, `Start Time`, threshold, actions) %>% 
  mutate(dur_n1 = ifelse(actions==150, lag(time, n=1), NA),
         dur_n2 = ifelse(actions==150, lag(time, n=2), NA),
         dur_n3 = ifelse(actions==150, lag(time, n=3), NA),
         dur_n4 = ifelse(actions==150, lag(time, n=4), NA),
         dur_n5 = ifelse(actions==150, lag(time, n=5), NA),
         dur_n6 = ifelse(actions==150, lag(time, n=6), NA)) %>% 
  mutate(ma = ifelse(actions==150,roll_mean(time, width = 60, min_obs = 7), NA))

# Create variable HE n-1 that checks for a head entry in between lever presses.
behavior = behavior %>% 
  group_by(Subject, MSN, `Start Date`, `Start Time`, threshold) %>% 
  mutate(he = ifelse(actions %in% c(120,130), 1, ifelse(actions == 150,0,NA))) %>% 
  fill(he, .direction="down") %>% 
  mutate(he = ifelse(is.na(he) & actions == 100,0, he),
         he = ifelse(actions == 150, NA, he)) %>%
  fill(he, .direction="down") %>% 
  mutate(he = ifelse(actions == 150, he, NA))

# Create Rew n-1, checks if the past lp was rewarded or not
behavior = behavior %>% 
  mutate(rew = ifelse(actions == 200, 1, ifelse(actions == 150, 0, NA))) %>% 
  fill(rew, .direction="down") %>% 
  mutate(rew = ifelse(is.na(rew) & actions == 100, 0, rew),
         rew = ifelse(actions == 150, NA, rew)) %>%
  fill(rew, .direction = "down") %>% 
  mutate(rew = ifelse(actions == 150, rew, NA))

# Create timestamp variable, timestamp (in ms) for when in a session a lever press occurred
behavior = behavior %>% 
  mutate(timestamp = ifelse(actions == 100, cumsum(time), NA))
  

# Shift so ipi and timestamp is on the same level as other variables
behavior = behavior %>% 
  fill(ipi, .direction = "down") %>% 
  fill(ipi_n2, .direction = "down") %>%
  fill(timestamp, .direction = "down") %>% 
  mutate(ipi = ifelse(actions == 150, ipi, NA),
         ipi_n2 = ifelse(actions == 150, ipi_n2, NA),
         timestamp = ifelse(actions == 150, timestamp, NA))

# Create percentage met, Overall % of presses that met criteria for a given session
behavior = behavior %>% 
  mutate(perc_met = mean(succ_lp, na.rm=TRUE),
         perc_met = ifelse(actions == 150, perc_met, NA)) %>% 
  ungroup()

# Add trial id
behavior = behavior %>% 
  group_by(Subject, MSN, `Start Date`, `Start Time`, threshold) %>% 
  mutate(trial_id = cur_group_id())
  

# Filter to stay just with lever presses and add trial id
behavior_lp = behavior %>% 
  filter(actions == 150)

# Export data
write_csv(behavior_lp, file = "../data/lme_data.csv")


  
  

