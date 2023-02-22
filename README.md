# mouse_behavior

A potential factor in the decreased utility of reward is the desensitization of the reward based on the density of the reward schedule.
In other words, the mouse is less interested in reward if it has already received a reward recently. This project examines the predictive power of the 
density of successful previous lever presses on the duration of the next lever press. We first replicated the results using linear mixed-effects models 
conducted in the source paper. Then, we estimate two augmented models: 
i) adding the frequency of rewards in the last 10 lever presses, and ii) adding the time from the last reward until the lever press. Lastly, we conducted 
a model comparison analysis between the baseline and the augmented models. The baseline model indicated that previous lever presses at t-1 to t-6 have 
statistically significant predictive power. For the augmented model, we determine to take into account the 10 past lever presses given that in the 
baseline study, 10 events is the limit at which previous events begin to demonstrate a lack of significant interaction effect on the current event at t. 
The augmented models show, with statistical significance, that as the frequency of rewards in the past lever presses increases, the duration of the lever 
press tends to decrease. On the other hand, the time passed from the last reward has a negative significant effect on the next lever press duration.

## Motivation

The article analyzes the continuous behavior of mice in a free-roam environment. Precisely, mice are placed into an operant box where if a lever is 
pressed for a certain threshold of duration (800ms) then the mouse is rewarded with sucrose or food pellets. Behavioral data is collected regarding the 
number of lever presses and the duration of each lever press. The article shows the ability of mice to increase the percentage of successful lever presses 
during a session and across sessions. Specifically, the authors found that the duration of the previous six lever presses has a statistically significant 
effect on the duration of the next lever press.

We make the prior assumption that the mice in the study are conditioned to associate lever presses with reward, then a shorter lever press indicates a 
decrease in utility of reward for reasons or combinations thereof including but not limited to: 1. over-stimulation of reward thus reducing the 
desirability of reward, 2. There are competing factors in the environment that distract the mice from the reward.

## Data

The data is sourced from (Schreiner et al., 2022) on mice foraging behavior. 

## Questions

i) is the frequency of reward strongly correlated with a decrease in reward utility? 

ii) is the time since the last reward correlated with a decrease in reward utility?

## References

Schreiner, D.C., Cazares, C., Renteria, R. et al. Information normally considered task-irrelevant drives decision-making and affects premotor circuit
recruitment. Nat Commun 13, 2134 (2022). https://doi.org/ 10.1038/s41467-022-29807-2
