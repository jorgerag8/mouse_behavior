library(tidyverse)
library(lme4)
library(lmerTest)

# Get lme data
df_lme = read_csv("../data/lme_data.csv")

# Try to recreate paper model

simple_lme = lmer(time ~ dur_n1 + dur_n2 + dur_n3 + dur_n4 + dur_n5 + dur_n6 + ma + he + 
       rew + ipi + ipi_n2 + timestamp + perc_met + dur_n1:he + dur_n1:rew + 
      dur_n1:ipi + dur_n1:timestamp + dur_n1:perc_met + ma:he + ma:rew + ma:ipi + 
      ma:timestamp + ma:perc_met +  + (1 + Subject | `Start Date`), data = df_lme)

summary(simple_lme)
