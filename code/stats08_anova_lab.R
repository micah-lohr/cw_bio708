# ANOVA Lab
pacman::p_load(tidyverse, 
               ggplot2,
               pwr,
               DescTools)
# install.packages("pwr")
#install.packages("DescTools")

# 11.5.1 Application to PlantGrowth ---------------------------------------
data("PlantGrowth")
df_pg <- PlantGrowth

# (1) The data set consists of two columns: weight, group. Create figures similar to Figure 11.1.
df_pg %>% 
  mutate(group_label = case_when(
    group == "ctrl" ~ "Control",
    group=="trt1" ~ "Treatment 1",
    group=="trt2" ~ "Treatment 2"
  )) %>% 
  ggplot(aes(x = group_label,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) +
  labs(x="Group", y="Weight") +
  theme_classic()

# (2) Conduct an ANOVA to examine whether there are differences in weight among the different group.

summary(aov(weight~ group, data=df_pg))

# (2.5) Run a post-hoc test on the ANOVA to see where the sig. difference is 
pg_dunnett <- DunnettTest(weight ~ group, data = df_pg, control = "ctrl") #Dunnett's test compares groups to control group
print(pg_dunnett) # no sig. difference between treatment 1 and control or treatment 2 and control 

pg_model <- aov(weight~ group, data=df_pg)
pg_tukey <- TukeyHSD(pg_model)
print(pg_tukey) # average weight in treatment 1 and 2 are sig. different from each other 

# (3) Discuss what values to be reported in a scientific article?

# You would want to report the degrees of freedom, F-value, and p-value, and indicate that there is a statistically significant difference/that we can reject the null hypothesis.


# 11.5.2 Power Analysis ---------------------------------------------------

# Use a power analysis for comparing plant biomass between three different wetlands. 
# We expect a large effect size (Cohen's f=0.5) and want 80% power at a 0.05 significance level

pwr::pwr.anova.test(k=3, f=0.5, sig.level=0.05, power=0.8)

#output n gives us the sample size we would need in each group (13.89521, round up to 14?)

# (EXTRA) play around with changing values 
pwr::pwr.anova.test(k=3, f=0.5, sig.level=0.05, power=0.6)
# if you reduces the expected power to 60%, the sample size decreases to 9.34862 (round up to 10?)

pwr::pwr.anova.test(k=3, f=0.5, sig.level=0.01, power=0.8)
# if you reduce the alpha to 0.01, the sample size jumps up to 20

pwr::pwr.anova.test(k=10, f=0.5, sig.level=0.05, power=0.8)
# increase number of groups to 10, then sample size decreases

pwr::pwr.anova.test(k=3, f=0.8, sig.level=0.05, power=0.8)
# if you increase the effect size even more, the sample size goes down to 6

pwr::pwr.anova.test(k=3, f=0.25, sig.level=0.05, power=0.8)
# If you decrease the effect size, the sample size skyrockets (52.3966)

pwr::pwr.anova.test(k=10, n=20, f=0.5, sig.level=0.05)
# if you add in a sample size of 20 per group but take out power, then the power is nearly at 100%

#trying with changing multiple numbers:
pwr::pwr.anova.test(k=3, n=3, f=0.5, sig.level=0.05)
# small k and small n produce a lower power


