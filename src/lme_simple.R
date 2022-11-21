library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)

# Get lme data
df_lme = read_csv("../data/lme_data.csv")

# Try to recreate paper model
simple_lme = lmer(time ~ dur_n1 + dur_n2 + dur_n3 + dur_n4 + dur_n5 + dur_n6 + ma + he + 
                    dur_n1:he + ma:he + rew + dur_n1:rew + ma:rew + ipi + dur_n1:ipi +
                    ma:ipi +  ipi_n2 + dur_n2:ipi_n2 + timestamp + dur_n1:timestamp +
                    ma:timestamp + perc_met + dur_n1:perc_met + ma:perc_met + 
                    (1 | Subject)  + (1 | `Start Date`), data = df_lme)
summary(simple_lme)

# Try augmented model
augmented_lme = lmer(time ~ dur_n1 + dur_n2 + dur_n3 + dur_n4 + dur_n5 + dur_n6 + ma + he + 
                    dur_n1:he + ma:he + rew + dur_n1:rew + ma:rew + ipi + dur_n1:ipi +
                    ma:ipi +  ipi_n2 + dur_n2:ipi_n2 + timestamp + dur_n1:timestamp +
                    ma:timestamp + perc_met + dur_n1:perc_met + ma:perc_met + 
                    freq_rew10 + (1 | Subject)  + (1 | `Start Date`), data = df_lme )
summary(augmented_lme)

# Try augmented model 2
augmented_lme2 = lmer(time ~ dur_n1 + dur_n2 + dur_n3 + dur_n4 + dur_n5 + dur_n6 + ma + he + 
                       dur_n1:he + ma:he + rew + dur_n1:rew + ma:rew + ipi + dur_n1:ipi +
                       ma:ipi +  ipi_n2 + dur_n2:ipi_n2 + timestamp + dur_n1:timestamp +
                       ma:timestamp + perc_met + dur_n1:perc_met + ma:perc_met + 
                       freq_rew10 + t_r + (1 | Subject)  + (1 | `Start Date`), data = df_lme )
summary(augmented_lme2)

# Model comparison

# Compact vs first augmented
# Get data into the same dimention

df_lme_aug = df_lme %>% 
  select(-c(actions:he_order), -t_r) %>% 
  drop_na()

# Estimate models
s_lme = lmer(time ~ dur_n1 + dur_n2 + dur_n3 + dur_n4 + dur_n5 + dur_n6 + ma + he + 
                    dur_n1:he + ma:he + rew + dur_n1:rew + ma:rew + ipi + dur_n1:ipi +
                    ma:ipi +  ipi_n2 + dur_n2:ipi_n2 + timestamp + dur_n1:timestamp +
                    ma:timestamp + perc_met + dur_n1:perc_met + ma:perc_met + 
                    (1 | Subject)  + (1 | `Start Date`), data = df_lme_aug)

# Try augmented model
aug_lme = lmer(time ~ dur_n1 + dur_n2 + dur_n3 + dur_n4 + dur_n5 + dur_n6 + ma + he + 
                       dur_n1:he + ma:he + rew + dur_n1:rew + ma:rew + ipi + dur_n1:ipi +
                       ma:ipi +  ipi_n2 + dur_n2:ipi_n2 + timestamp + dur_n1:timestamp +
                       ma:timestamp + perc_met + dur_n1:perc_met + ma:perc_met + 
                       freq_rew10 + (1 | Subject)  + (1 | `Start Date`), data = df_lme_aug )

summary(aug_lme)

anova(s_lme, aug_lme)

# first augmented vs second augmented
# Get data into the same dimention

df_lme_aug2 = df_lme %>% 
  select(-c(actions:he_order)) %>% 
  drop_na()

# Estimate models
aug1_lme = lmer(time ~ dur_n1 + dur_n2 + dur_n3 + dur_n4 + dur_n5 + dur_n6 + ma + he + 
                  dur_n1:he + ma:he + rew + dur_n1:rew + ma:rew + ipi + dur_n1:ipi +
                  ma:ipi +  ipi_n2 + dur_n2:ipi_n2 + timestamp + dur_n1:timestamp +
                  ma:timestamp + perc_met + dur_n1:perc_met + ma:perc_met + 
                  (1 | Subject)  + (1 | `Start Date`), data = df_lme_aug2 )

# Try augmented model
aug2_lme = lmer(time ~ dur_n1 + dur_n2 + dur_n3 + dur_n4 + dur_n5 + dur_n6 + ma + he + 
                  dur_n1:he + ma:he + rew + dur_n1:rew + ma:rew + ipi + dur_n1:ipi +
                  ma:ipi +  ipi_n2 + dur_n2:ipi_n2 + timestamp + dur_n1:timestamp +
                  ma:timestamp + perc_met + dur_n1:perc_met + ma:perc_met + 
                  t_r + (1 | Subject)  + (1 | `Start Date`), data = df_lme_aug2 )

summary(aug2_lme)

anova(aug1_lme, aug2_lme)

plot_summs(aug1_lme, aug2_lme)


