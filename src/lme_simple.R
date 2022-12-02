library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(effects)
library(sjPlot)

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

# standard error of coefficient
aug_se <- sqrt(diag(vcov(aug_lme)))[15]

# estimated coefficient
augs_coef <- fixef(aug_lme)[15]
augs_coef + 1.96*aug_se
augs_coef - 1.96*aug_se

# Coefficient plot
theme_set(theme_classic() +
            theme(text = element_text(size = 25)))

sjPlot::plot_model(aug_lme, terms = c("freq_rew10", "dur_n6", "dur_n5", 
                                      "dur_n4", "dur_n3", "dur_n2", "dur_n1"),
                   axis.labels=c("freq_R", "n-6", "n-5", "n-4", "n-3", "n-2",
                                 "n-1"), 
                   show.values=TRUE, show.p=TRUE, value.size = 6,
                   title="Effect of Past Behavior in Duration of Next LP",
                   colors = c("firebrick", "royalblue"))

#png(file="../plots/model_effects.png", width=1000, height=800)

# Freq_rew10 plot
effects_freq_rew10 <- effects::effect(term = "freq_rew10", mod= aug_lme)
summary(effects_freq_rew10)

# Save the effects values as a df:
x_freq = as.data.frame(effects_freq_rew10) %>% 
  mutate(fit = fit*10,
         lower = lower*10,
         upper = upper*10)

ggplot() + 
  #geom_point(data=df_lme_aug, aes(freq_rew10, time)) + 
  geom_point(data=x_freq, aes(x=freq_rew10, y=fit), color="blue") +
  geom_line(data=x_freq, aes(x=freq_rew10, y=fit), color="blue") +
  geom_ribbon(data=x_freq, aes(x=freq_rew10, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
  labs(x="Frequency of rewards in last 10 LP", y="Duration of next LP") +
  ggtitle("Effect of Frequency of Rewards in Duration of Next LP",)

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

# standard error of coefficient
aug2_se <- sqrt(diag(vcov(aug2_lme)))[16]

# estimated coefficient
augs_coef2 <- fixef(aug2_lme)[15]
augs_coef2 + 1.96*aug2_se
augs_coef2 - 1.96*aug2_se


