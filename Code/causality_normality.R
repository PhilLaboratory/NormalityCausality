# Load packages -------------------------------------------------------------------------------
library(corrr)
library(broom)
library(lme4)
library(tidyverse)

# Misc functions ------------------------------------------------------------------------------
# set ggplot theme 
theme_set(
  theme_bw()+
    theme(text = element_text(size = 20),
          panel.grid = element_blank()
    )
)

# Load data -----------------------------------------------------------------------------------
load("../../data/dataset1.Rdata")
load("../../data/dataset1.Rdata")
load("../../data/dataset2.Rdata")
vignettes = read.csv("../../data/vignetteCodes.csv")

load("../data/dataset1.Rdata")
load("../data/dataset1.Rdata")
load("../data/dataset2.Rdata")
vignettes = read.csv("../data/vignetteCodes.csv")

df.long = d15.1 %>% 
  bind_rows(d16.1) %>% 
  mutate(scenario = scenario %>% as.character() %>% as.numeric()) %>% 
  left_join(vignettes) %>% 
  select(-scenario) %>% 
  rename(scenario = name) %>% 
  select(-c(condition,condCode)) %>% 
  mutate_at(vars(-c(subj,response)),funs(tolower(.))) %>% 
  mutate(participant = as.factor(subj),
         participant = factor(subj,labels = 1:nlevels(participant)) %>% as.character() %>% as.numeric(),
         condition = ifelse(outcome %in% c('abnormal','normal'),'normality','morality')) %>% 
  select(participant,condition,scenario,norm,knowledge,outcome,question,response) %>% 
  mutate_at(vars(-c(participant,response)),funs(as.factor(.))) %>% 
  mutate(response = 8-response) %>% #so that higher values stand for higher agreement values 
  arrange(participant,condition,scenario,norm,knowledge,outcome,question)
  
rm(list = c('d15.1','d16.1','vignettes'))

# Plot results (bar graphs) -------------------------------------------------------------------------------

df.long %>% 
  spread(question,response) %>% 
  group_by(participant) %>%
  mutate_at(vars(cause,modal,probability,counterfactual),funs(scale(.))) %>%
  gather(question,response,c(cause,modal,probability,counterfactual)) %>% 
  ungroup() %>% 
  filter(question %in% c('cause','modal')) %>%
  # filter(question == 'cause') %>%
  # ggplot(aes(x = knowledge, y = response, group = question, fill = question))+
  ggplot(aes(x = knowledge, y = response, group = question, fill = question))+
  stat_summary(fun.y = mean, geom = 'bar', position = position_dodge(0.75), width = 0.75, color = 'black')+
  stat_summary(fun.data = mean_cl_boot, geom = 'linerange', position = position_dodge(0.75), size = 1)+
  # geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.2, dodge.width = 0.75),alpha = 0.05, shape = 21, color = 'black')+
  # facet_grid(condition~outcome,scales = 'free')+
  # facet_wrap(condition~outcome,scales = 'free')+
  # facet_grid(~)+
  # facet_wrap(outcome~norm)
  facet_grid(norm ~ outcome)
  coord_cartesian(ylim = c(1,7)) + 
  scale_y_continuous(breaks = 1:7)+
  # scale_x_discrete(label=abbreviate)+
  scale_fill_brewer(type = 'qual', palette = 3)

# Plot results (raster plots) ----------------------------------------------------------------

var1 = 'cause'
var2 = 'counterfactual'
var3 = 'outcome'

df.long %>% 
  filter(question %in% c(var1,var2)) %>%
  spread(question,response) %>% 
  count(.data[[var1]],.data[[var2]],.data[[var3]]) %>% 
  ggplot(aes(x=var1,y=var2,fill=n)) +
  geom_tile()+
  scale_fill_continuous(low = 'white',high = 'black')+
  labs(x = var1, y = var2)+
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 1:7)+
  facet_grid(~var3)

df.long %>% 
  spread(question,response) %>% 
  # count(cause)
  count(modal)
  
# Simple correlations ---------------------------------------------------------------------------------------

df.long %>% 
  filter(condition == 'normality') %>%
  # filter(condition == 'morality') %>%
  spread(question,response) %>% 
  select(cause,counterfactual,modal,probability) %>% 
  correlate() %>% 
  shave() %>% 
  fashion() 
  
  



# Simple regression ---------------------------------------------------------------------------

df.long %>% 
  spread(question,response) %>% 
  group_by(participant) %>%
  # mutate_at(vars(cause,modal,probability,counterfactual),funs(scale(.))) %>%
  gather(question,response,c(cause,modal,probability,counterfactual)) %>% 
  ungroup() %>% 
  # filter(condition == 'morality') %>%
  # filter(condition == 'normality') %>%
  # filter(norm == 'rational') %>%
  # filter(norm == 'moral') %>%
  group_by(condition,scenario,norm,knowledge,outcome,question) %>% 
  summarise(response = mean(response)) %>% 
  spread(question,response) %>% 
  ungroup() %>% 
  mutate_at(vars(cause,modal,probability,counterfactual),funs(scale(.))) %>%
  # do(tidy(lm(cause~modal*counterfactual,data=.)))
  lm(cause~modal*counterfactual,data=.) %>%
  # lm(cause~probability*counterfactual,data=.) %>%
  # lm(cause~modal*probability*counterfactual,data=.) %>%
  summary()
  
df.regression = df.long %>% 
  spread(question,response) %>% 
  mutate_at(vars(cause,modal,probability,counterfactual),funs(scale(.)))

lm1 = lmer(cause~modal*counterfactual+(modal*counterfactual|participant)+(1|scenario),data = df.regression)
lm2 = lmer(cause~modal+counterfactual+(modal*counterfactual|participant)+(1|scenario),data = df.regression)
anova(lm1,lm2)
