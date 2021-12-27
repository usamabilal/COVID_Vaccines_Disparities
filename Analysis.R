rm(list=ls())
library(tidyverse)
library(broom)
library(broom.mixed)
library(mixedup)
library(MASS)
library(readxl)
library(scales)
library(grid)
library(gridExtra)
library(lubridate)
library(ggrepel)
library(tidycensus)
library(reldist)
library(glmmTMB)
library(lme4)
library(mixedup)
select<-dplyr::select
load("Data/Clean_Data.Rdata")
# theme for figures
fontsize<-16
theme_vax<-theme_bw() +
  theme(axis.text=element_text(color="black", size=fontsize),
        axis.title=element_text(face="bold", color="black", size=fontsize),
        plot.title=element_text(face="bold", color="black", size=fontsize),
        strip.text =element_text(face="bold", color="black", size=fontsize),
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.text=element_text(size=fontsize),
        legend.title=element_text(size=fontsize, face="bold"))
# basic stats
summary(all_vax)
nrow(all_vax)
sum(all_vax$total_pop)
sum(all_vax$fully_vaccinated)
sum(all_vax$fully_vaccinated)/sum(all_vax$total_pop)*100
wtd.quantile(all_vax$fully_vaccinated_prop, q=c(0.1, 0.9), na.rm=T, weight=all_vax$total_pop)

# FIRST: TAble 1, descriptives
# State, City, # ZCTAs, Median Pop by ZCTA, city pop, % nonwhite, % poverty, % fully vaccinated
# p90 and p10 for each city
table1<-all_vax %>% 
  left_join(city_values) %>% 
  group_by(city_state, state, city) %>% 
  summarise(fully_vaccinated_total=sum(fully_vaccinated),
            n_zcta=n(),
            zcta_pop=median(total_pop),
            total_pop_total=sum(total_pop),
            svi_med=median(svi),
            svi_q1=quantile(svi, probs=0.25),
            svi_q3=quantile(svi, probs=0.75),
            city_pop=unique(pop),
            poverty=unique(pct_poverty),
            nonwhite=unique(pct_nonwhite)) %>% 
  mutate(fully_vaccinated_total=fully_vaccinated_total/1000000,
         total_pop_total=total_pop_total/1000000,
         prop=fully_vaccinated_total/total_pop_total,
         pop=format(city_pop/1000000, digits=2, nsmall=2),
         svi=paste0(format(svi_med, digits=2, nsmall=2), 
                    " [", format(svi_q1, digits=2, nsmall=2), "-",
                    format(svi_q3, digits=2, nsmall=2), "]"),
         poverty=format(poverty, digits=1, nsmall=1),
         nonwhite=format(nonwhite, digits=1, nsmall=1),
         prop=format(prop*100, digits=1, nsmall=1)) %>% 
  arrange(state, city_state) %>% 
  ungroup() %>% 
  select(city_state,n_zcta, zcta_pop, svi, pop, nonwhite, poverty, prop) %>% 
  # get total
  bind_rows(all_vax %>% 
              ungroup() %>% 
              summarise(fully_vaccinated_total=sum(fully_vaccinated),
                        n_zcta=n(),
                        zcta_pop=median(total_pop),
                        total_pop_total=sum(total_pop),
                        svi_med=median(svi),
                        svi_q1=quantile(svi, probs=0.25),
                        svi_q3=quantile(svi, probs=0.75)) %>% 
              bind_cols(city_values %>% 
                          filter(city%in%unique(all_vax$city)) %>% 
                          summarise(poverty=weighted.mean(pct_poverty, w=pop),
                                    nonwhite=weighted.mean(pct_nonwhite, w=pop))) %>% 
              mutate(fully_vaccinated_total=fully_vaccinated_total/1000000,
                     total_pop_total=total_pop_total/1000000,
                     prop=fully_vaccinated_total/total_pop_total,
                     pop=format(total_pop_total, digits=2, nsmall=2),
                     svi=paste0(format(svi_med, digits=2, nsmall=2), 
                                " [", format(svi_q1, digits=2, nsmall=2), "-",
                                format(svi_q3, digits=2, nsmall=2), "]"),
                     poverty=format(poverty, digits=1, nsmall=1),
                     nonwhite=format(nonwhite, digits=1, nsmall=1),
                     prop=format(prop*100, digits=1, nsmall=1)) %>% 
              mutate(city_state="Total****") %>% 
              select(city_state,n_zcta, zcta_pop, svi, pop, nonwhite, poverty, prop))

# sort cities
all_vax<-all_vax %>% 
  mutate(city_f=factor(city_state, levels=all_vax %>% arrange(state, city_state) %>% pull(city_state) %>% unique),
         not_fully_vaccinated=total_pop-fully_vaccinated,
         fully_vaccinated=as.integer(fully_vaccinated),
         not_fully_vaccinated=as.integer(not_fully_vaccinated)) 

# SECOND: graphical descriptives
# scatter plot (note 3 are excluded from figure)
figure1<-ggplot(all_vax, aes(x=svi_std, y=fully_vaccinated_prop)) +
  # geom_text(data=all_vax %>% filter(!duplicated(city_f)),
  #           aes(label=paste0("rho=", round(cor, digits=3))), x=1, y=.1)+
  geom_point(pch=21, color="black", fill="gray", aes(size=(total_pop)))+
  stat_smooth(method="loess", se=F, color="black", aes(weight=total_pop))+
  scale_y_continuous(limits=c(0, NA), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA))+
  scale_size_continuous( range=c(1, 3))+
  facet_wrap(~city_f, ncol=4)+
  guides(size=F)+
  coord_cartesian(ylim=c(0, 1))+
  labs(x="Social Vulnerability Index",
       y="Proportion Fully Vaccinated (%)")+
  theme_vax
# scatter plot unstd
af1<-ggplot(all_vax, aes(x=svi, y=fully_vaccinated_prop)) +
  # geom_text(data=all_vax %>% filter(!duplicated(city_f)),
  #           aes(label=paste0("rho=", round(cor, digits=3))), x=1, y=.1)+
  geom_point(pch=21, color="black", fill="gray", aes(size=(total_pop)))+
  stat_smooth(method="loess", se=F, color="black", aes(weight=total_pop))+
  scale_y_continuous(limits=c(0, NA), labels=scales::percent_format(accuracy = 1))+
  scale_x_continuous(limits=c(NA, NA))+
  scale_size_continuous( range=c(1, 3))+
  facet_wrap(~city_f, ncol=4)+
  guides(size=F)+
  coord_cartesian(ylim=c(0, 1))+
  labs(x="Social Vulnerability Index",
       y="Proportion Fully Vaccinated (%)")+
  theme_vax

# figure 2 (similar to 1 but in quintiles)
figure2_data<-all_vax %>% 
  group_by(region, city_f, svi_std_quint_f) %>% 
  summarise(vax=sum(fully_vaccinated), pop=sum(total_pop)) %>% 
  mutate(rate=vax/pop,
         term=factor(svi_std_quint_f, levels=c("Low", "Low-Medium", "Medium", "Medium-High", "High"),
                     labels=sub("-", " ", c("Low", "Low-Medium", "Medium", "Medium-High", "High")))) %>% 
  mutate(city_f_nostate_txca=sub("\\, TX", "", city_f),
         city_f_nostate_txca=sub("\\, CA", "", city_f_nostate_txca),
         city_f_nostate=substr(city_f, 1, regexpr("\\,", city_f)-1),
         city_f_nostate=sub("New York City", "NYC", city_f_nostate)) %>% 
  mutate(region=factor(region, levels=c("Midwest", "Northeast",
                                        "West", "South")))
figure2_data<-figure2_data %>% 
  full_join(figure2_data %>% filter(!duplicated(city_f)) %>% group_by(region) %>% mutate(city_id=factor(row_number()))%>% select(city_f           , region, city_id))

figure2<-ggplot(figure2_data, aes(x=term, y=rate, group=city_f)) +
  geom_line(aes(color=city_id))+
  geom_point(pch=21, color="black", aes(fill=city_id), size=5) +
  # geom_text(data=table2_figure %>% filter(term=="High"),aes(label=city_f, color=city_f),
  #           position=position_nudge(x=0.1, y=0),hjust=0)+
  # geom_text(data=table2_figure %>% filter(term=="High", 
  #                                         !grepl("Dxallas|Foxrt", city_f)),
  #                 aes(label=str_wrap(city_f_nostate, 16), color=city_f),size=5,
  #                 position=position_nudge(x=0.1, y=0),hjust=0)+
  geom_text_repel(data=figure2_data %>% filter(term=="High",
                                               !grepl("Dxallas|Fxort", city_f)),
                  aes(label=city_f_nostate, color=city_id),size=5,
                  direction="y",nudge_x=0.2, min.segment.length = 10, hjust=0)+
  guides(fill=F, color=F)+
  scale_y_continuous(limits=c(.20, 1), 
                     breaks=seq(.20, 1, by=0.2),
                     labels=scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = wrap_format(6),
                   expand=expansion(mult=c(0.1, 0.3)))+
  scale_color_brewer(type="qual", palette=2)+
  scale_fill_brewer(type="qual", palette=2)+
  labs(x="Social Vulnerability Index (City-Specific Quintiles)",
       y="% Fully Vaccinated")+
  facet_wrap(~region, ncol=2)+
  theme_vax
# figure 2b <- unstd 
figure2_data<-all_vax %>% 
  group_by(region, city_f, svi_quint_f) %>% 
  summarise(vax=sum(fully_vaccinated), pop=sum(total_pop)) %>% 
  mutate(rate=vax/pop,
         term=factor(svi_quint_f, levels=c("Low", "Low-Medium", "Medium", "Medium-High", "High"),
                     labels=sub("-", " ", c("Low", "Low-Medium", "Medium", "Medium-High", "High")))) %>% 
  mutate(city_f_nostate_txca=sub("\\, TX", "", city_f),
         city_f_nostate_txca=sub("\\, CA", "", city_f_nostate_txca),
         city_f_nostate=substr(city_f, 1, regexpr("\\,", city_f)-1),
         city_f_nostate=sub("New York City", "NYC", city_f_nostate)) %>% 
  mutate(region=factor(region, levels=c("Midwest", "Northeast",
                                        "West", "South")))
figure2_data<-figure2_data %>% 
  full_join(figure2_data %>% filter(!duplicated(city_f)) %>% group_by(region) %>% mutate(city_id=factor(row_number()))%>% select(city_f           , region, city_id))

af2<-ggplot(figure2_data, aes(x=term, y=rate, group=city_f)) +
  geom_line(aes(color=city_id))+
  geom_point(pch=21, color="black", aes(fill=city_id), size=5) +
  # geom_text(data=table2_figure %>% filter(term=="High"),aes(label=city_f, color=city_f),
  #           position=position_nudge(x=0.1, y=0),hjust=0)+
  # geom_text(data=table2_figure %>% filter(term=="High", 
  #                                         !grepl("Dxallas|Foxrt", city_f)),
  #                 aes(label=str_wrap(city_f_nostate, 16), color=city_f),size=5,
  #                 position=position_nudge(x=0.1, y=0),hjust=0)+
  geom_text_repel(data=figure2_data %>% filter(term=="High",
                                               !grepl("Dxallas|Fxort", city_f)),
                  aes(label=city_f_nostate, color=city_id),size=5,
                  direction="y",nudge_x=0.2, min.segment.length = 10, hjust=0)+
  geom_text_repel(data=figure2_data %>% filter(term=="Medium High",
                                               grepl("Jose|Francisco", city_f)),
                  aes(label=city_f_nostate, color=city_id),size=5,
                  direction="y",nudge_x=0.2, min.segment.length = 10, hjust=0)+
  guides(fill=F, color=F)+
  scale_y_continuous(limits=c(.20, 1), 
                     breaks=seq(.20, 1, by=0.2),
                     labels=scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = wrap_format(6),
                   expand=expansion(mult=c(0.1, 0.3)))+
  scale_color_brewer(type="qual", palette=2)+
  scale_fill_brewer(type="qual", palette=2)+
  labs(x="Social Vulnerability Index (Whole Sample Quintiles)",
       y="% Fully Vaccinated")+
  facet_wrap(~region, ncol=2)+
  theme_vax


# THIRD: total ineqs
# table 2: ineqs: first total part
# total inequities
table2_total<-all_vax %>% 
  group_by(city_state, state, city) %>% 
  summarise(p90=wtd.quantile(fully_vaccinated_prop, q=.9, na.rm=T, weight=total_pop),
            p10=wtd.quantile(fully_vaccinated_prop, q=.1, na.rm=T, weight=total_pop)) %>% 
  mutate(ratio=p90/p10,
         dif=p90-p10,
         p90=format(p90*100, digits=1, nsmall=1),
         p10=format(p10*100, digits=1, nsmall=1),
         ratio=format(ratio, digits=2, nsmall=2),
         dif=format(dif*100, digits=1, nsmall=1)) %>% 
  # also overall row
  bind_rows(all_vax %>% 
              summarise(p90=wtd.quantile(fully_vaccinated_prop, q=.9, na.rm=T, weight=total_pop),
                        p10=wtd.quantile(fully_vaccinated_prop, q=.1, na.rm=T, weight=total_pop)) %>% 
              mutate(ratio=p90/p10,
                     dif=p90-p10,
                     p90=format(p90*100, digits=1, nsmall=1),
                     p10=format(p10*100, digits=1, nsmall=1),
                     ratio=format(ratio, digits=2, nsmall=2),
                     dif=format(dif*100, digits=1, nsmall=1),
                     city_state="Total**", state="ZZ", city="ZZ") %>% 
              relocate(c(city_state, state, city), .before=1))

# for paper
table2_total %>% arrange(as.numeric(ratio))
table2_total %>% arrange(as.numeric(dif))

# FOURTH: linear models
# last af6: comparing RS vs city-specific slopes
# also get city-specific slopes from stratified models (main model only)
city_slopes_models<-all_vax %>% 
  group_by(city_state) %>% 
  group_modify(~{
    #.x<-all_vax %>% filter(city_state=="Philadelphia, PA")
    rii<-lm(log(fully_vaccinated_prop)~svi_std+pct_age1844+pct_age4564+pct_age65plus, data=.x)
    sii<-lm((fully_vaccinated_prop)*100~svi_std+pct_age1844+pct_age4564+pct_age65plus, data=.x)
    rii_unadj<-lm(log(fully_vaccinated_prop)~svi_std, data=.x)
    sii_unadj<-lm((fully_vaccinated_prop)*100~svi_std, data=.x)
    rii_unstd<-lm(log(fully_vaccinated_prop)~svi+pct_age1844+pct_age4564+pct_age65plus, data=.x)
    sii_unstd<-lm((fully_vaccinated_prop)*100~svi+pct_age1844+pct_age4564+pct_age65plus, data=.x)
    tibble(
      type=rep(c("adj_std", "unadj", "unstd"), times=2),
           outcome=c(rep("rii", times=3), rep("sii", times=3)),
           models=list(rii, rii_unadj, rii_unstd,
                    sii, sii_unadj, sii_unstd))
  })
# get RII and SII (adjusted) for Table2
rii_sii<-city_slopes_models %>% 
  filter(type=="adj_std") %>% 
  group_by(city_state) %>% 
  group_modify(~{
    cbind(.x %>% filter(outcome=="rii") %>% 
      pull(models) %>% `[[`(1) %>% tidy %>% 
      filter(term=="svi_std") %>% 
      mutate(rii=exp(estimate),
             rii_lci=exp(estimate-1.96*std.error),
             rii_uci=exp(estimate+1.96*std.error)) %>% 
      select(rii, rii_lci, rii_uci),
      .x %>% filter(outcome=="sii") %>% 
        pull(models) %>% `[[`(1) %>% tidy %>% 
        filter(term=="svi_std") %>% 
        mutate(sii=(estimate),
               sii_lci=(estimate-1.96*std.error),
               sii_uci=(estimate+1.96*std.error)) %>% 
        select(sii, sii_lci, sii_uci))
  }) %>% 
  mutate(city_state=factor(city_state))
table2_riisii<-rii_sii %>% 
  mutate(sii=paste0(round(sii, digits=2), " (", 
                    round(sii_lci, digits=2), ";",
                    round(sii_uci, digits=2), ")")) %>% 
  mutate(rii=paste0(round(rii, digits=2), " (", 
                    round(rii_lci, digits=2), ";",
                    round(rii_uci, digits=2), ")")) %>% 
  select(city_state, rii, sii) 
# missing total row: get from MLM models!

# sii vs rii
af3<-ggplot(rii_sii, aes(x=rii, y=sii)) +
  geom_hline(yintercept = 0, lty=2)+
  geom_vline(xintercept = 1, lty=2)+
  geom_linerange(aes(ymin=sii_lci, ymax=sii_uci), color="gray")+
  geom_linerange(aes(xmin=rii_lci, xmax=rii_uci), color="gray")+
  geom_point(pch=21, fill="gray", color="black", size=3) +
  scale_x_continuous(trans="log")+
  labs(x="Relative Index of Inequality (95% CI)",
       y="Slope Index of Inequality (95% CI)") +
  theme_vax
cor(rii_sii$rii, rii_sii$sii, method="spearman")

# age adjusted vs unadjusted
rii_sii_unadj<-city_slopes_models %>% 
  filter(type=="unadj") %>% 
  group_by(city_state) %>% 
  group_modify(~{
    cbind(.x %>% filter(outcome=="rii") %>% 
            pull(models) %>% `[[`(1) %>% tidy %>% 
            filter(term=="svi_std") %>% 
            mutate(rii=exp(estimate),
                   rii_lci=exp(estimate-1.96*std.error),
                   rii_uci=exp(estimate+1.96*std.error)) %>% 
            select(rii, rii_lci, rii_uci),
          .x %>% filter(outcome=="sii") %>% 
            pull(models) %>% `[[`(1) %>% tidy %>% 
            filter(term=="svi_std") %>% 
            mutate(sii=(estimate),
                   sii_lci=(estimate-1.96*std.error),
                   sii_uci=(estimate+1.96*std.error)) %>% 
            select(sii, sii_lci, sii_uci))
  }) %>% 
  mutate(city_state=factor(city_state))
rii_comp_unadj<-rii_sii %>% 
  select(city_state, rii, rii_lci, rii_uci) %>% 
  full_join(rii_sii_unadj %>% select(city_state, rii_unadj=rii, 
                                     rii_unadj_lci=rii_lci,
                                     rii_unadj_uci=rii_uci)) %>% ungroup
cor(rii_comp_unadj$rii, rii_comp_unadj$rii_unadj, method="spearman")
af4<-ggplot(rii_comp_unadj, aes(x=rii, y=rii_unadj)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_vline(xintercept = 1, lty=2)+
  geom_line(data=data.frame(rii=seq(0.3, 1.4, by=0.01),
                            rii_unadj=seq(0.3, 1.4, by=0.01)))+
  geom_linerange(aes(ymin=rii_unadj_lci, ymax=rii_unadj_uci), color="gray")+
  geom_linerange(aes(xmin=rii_lci, xmax=rii_uci), color="gray")+
  scale_y_continuous(trans="log", breaks=pretty_breaks(n=5),
                     limits=c(rii_comp_unadj %>% select(rii_lci, rii_unadj_lci) %>% min,
                              rii_comp_unadj %>% select(rii_uci, rii_unadj_uci) %>% max))+
  scale_x_continuous(trans="log", breaks=pretty_breaks(n=5),
                     limits=c(rii_comp_unadj %>% select(rii_lci, rii_unadj_lci) %>% min,
                              rii_comp_unadj %>% select(rii_uci, rii_unadj_uci) %>% max))+
  geom_point(pch=21, fill="gray", color="black", size=3) +
  labs(x="Adjusted Relative Index of Inequality (95% CI)",
       y="Unadjusted Relative Index of Inequality (95% CI)") +
  theme_vax

# unstandardized
rii_sii_unstd<-city_slopes_models %>% 
  filter(type=="unstd") %>% 
  group_by(city_state) %>% 
  group_modify(~{
    cbind(.x %>% filter(outcome=="rii") %>% 
            pull(models) %>% `[[`(1) %>% tidy %>% 
            filter(term=="svi") %>% 
            mutate(rii=exp(estimate),
                   rii_lci=exp(estimate-1.96*std.error),
                   rii_uci=exp(estimate+1.96*std.error)) %>% 
            select(rii, rii_lci, rii_uci),
          .x %>% filter(outcome=="sii") %>% 
            pull(models) %>% `[[`(1) %>% tidy %>% 
            filter(term=="svi") %>% 
            mutate(sii=(estimate),
                   sii_lci=(estimate-1.96*std.error),
                   sii_uci=(estimate+1.96*std.error)) %>% 
            select(sii, sii_lci, sii_uci))
  }) %>% 
  mutate(city_state=factor(city_state))
rii_comp_unstd<-rii_sii %>% 
  select(city_state, rii, rii_lci, rii_uci) %>% 
  full_join(rii_sii_unstd %>% select(city_state, rii_unstd=rii, 
                                     rii_unstd_lci=rii_lci,
                                     rii_unstd_uci=rii_uci)) %>% ungroup
cor(rii_comp_unstd$rii, rii_comp_unstd$rii_unstd, method="spearman")
af5<-ggplot(rii_comp_unstd, aes(x=rii, y=rii_unstd)) +
  geom_hline(yintercept = 1, lty=2)+
  geom_vline(xintercept = 1, lty=2)+
  geom_line(data=data.frame(rii=seq(0.4, 1.6, by=0.1),
                            rii_unstd=seq(0.4, 1.6, by=0.1)))+
  geom_linerange(aes(ymin=rii_unstd_lci, ymax=rii_unstd_uci), color="gray")+
  geom_linerange(aes(xmin=rii_lci, xmax=rii_uci), color="gray")+
  scale_y_continuous(trans="log", breaks=pretty_breaks(n=5),
                     limits=c(rii_comp_unstd %>% select(rii_lci, rii_unstd_lci) %>% min,
                              rii_comp_unstd %>% select(rii_uci, rii_unstd_uci) %>% max))+
  scale_x_continuous(trans="log", breaks=pretty_breaks(n=5),
                     limits=c(rii_comp_unstd %>% select(rii_lci, rii_unstd_lci) %>% min,
                              rii_comp_unstd %>% select(rii_uci, rii_unstd_uci) %>% max))+
  geom_point(pch=21, fill="gray", color="black", size=3) +
  labs(x="Relative Index of Inequality (95% CI)\nusing city-specific SVI",
       y="Relative Index of Inequality (95% CI)\nusing whole-sample SVI") +
  theme_vax

# FIFTH: multilevel
# svi-based ineqs  with random slopes
model_rii<-lmer(log(fully_vaccinated_prop)~svi_std+pct_age1844+pct_age4564+pct_age65plus+(svi_std|city_state), data=all_vax, REML=T)
model_sii<-lmer((fully_vaccinated_prop)*100~svi_std+pct_age1844+pct_age4564+pct_age65plus+(svi_std|city_state), data=all_vax, REML=T)
# svi-based ineqs  without random slopes
model_nors_rii<-lmer(log(fully_vaccinated_prop)~svi_std+pct_age1844+pct_age4564+pct_age65plus+(1|city_state), data=all_vax, REML=T)
model_nors_sii<-lmer((fully_vaccinated_prop)*100~svi_std+pct_age1844+pct_age4564+pct_age65plus+(1|city_state), data=all_vax, REML=T)
# svi-based ineqs with random slopes and dummies for region
model_rii_region_int<-lmer(log(fully_vaccinated_prop)~svi_std*region+pct_age1844+pct_age4564+pct_age65plus+(svi_std|city_state), data=all_vax, REML=T)
model_sii_region_int<-lmer((fully_vaccinated_prop)*100~svi_std*region+pct_age1844+pct_age4564+pct_age65plus+(svi_std|city_state), data=all_vax, REML=T)

# FOURTH (v2): wrap up table 2 by adding a row for total
table2_riisii<-table2_riisii %>% 
  # add total row = fixed effect
  bind_rows(full_join(model_rii %>%
                        tidy %>% 
                        filter(term=="svi_std") %>% 
                        mutate(rii=exp(estimate),
                               rii_lci=exp(estimate-1.96*std.error),
                               rii_uci=exp(estimate+1.96*std.error)) %>% 
                        mutate(rii=paste0(round(rii, digits=2), " (", 
                                          round(rii_lci, digits=2), ";",
                                          round(rii_uci, digits=2), ")")) %>% 
                        mutate(city_state="Total**") %>% 
                        select(city_state, rii),
                      model_sii %>%
                        tidy %>% 
                        filter(term=="svi_std") %>% 
                        mutate(sii=(estimate),
                               sii_lci=(estimate-1.96*std.error),
                               sii_uci=(estimate+1.96*std.error)) %>% 
                        mutate(sii=paste0(round(sii, digits=2), " (", 
                                          round(sii_lci, digits=2), ";",
                                          round(sii_uci, digits=2), ")")) %>% 
                        mutate(city_state="Total**") %>% 
                        select(city_state, sii)))

table2<-full_join(table2_total, table2_riisii) %>% 
  arrange(state, city) %>% 
  ungroup() %>% 
  select(-state, -city)
# for paper
rii_sii %>% arrange(rii)
rii_sii %>% arrange(sii)

# FIFTH (v2) ; appendix tables
# appendix table of coefficients (rii first)
model_list_rii<-tibble(type=c("nors", "adj_std", "int"),
                       models=c(model_nors_rii,model_rii, model_rii_region_int))
at2a<-map(1:nrow(model_list_rii), function(i){
  #i<-3
  type<-model_list_rii$type[[i]]
  model<-model_list_rii$models[[i]]
  fixed<-model %>% 
    tidy %>% 
    filter(effect=="fixed", term!="(Intercept)", !grepl("^region", term)) %>% 
    mutate(coef=estimate,
           lci=estimate-1.96*std.error,
           uci=estimate+1.96*std.error,
           term=sub("svi_std\\:", "", term)) %>% 
    select(term, coef, lci, uci) %>% 
    mutate_at(-1, exp)
  fixed<-fixed %>% 
    mutate(coef=paste0(
      round(coef, digits=2), " (",
      round(lci, digits=2), ";",
      round(uci, digits=2), ")")) %>% 
    select(term, coef) 
  if (type=="unadj"){
    fixed<-fixed %>% 
      add_row(term=c("pct_age1844","pct_age4564","pct_age65plus"), coef="")
  }
  if (type=="int"){
    fixed<-fixed %>% 
      add_row(term="regionSouth", coef="1 (Ref.)")
  } else {
    fixed<-fixed %>% 
      add_row(term=c("regionSouth", "regionMidwest", "regionWest", "regionNortheast"), coef="")
  }
  random<-model %>% tidy %>% 
    filter(effect=="ran_pars") %>% 
    mutate(term=case_when(
      grepl("sd__\\(Intercept", term) ~ "t00",
      grepl("cor", term) ~ "t01",
      grepl("sd__svi", term) ~ "t11",
      grepl("Observ", term) ~ "s2"),
      estimate=ifelse(term=="t01", estimate, estimate^2),
      coef=round(estimate, digits=4) %>% as.character) %>% 
    select(term, coef)
  if (type=="nors"){
    random<-random %>% 
      add_row(term=c("t01", "t11"), coef="")
  }
  all<-bind_rows(fixed, random) %>% 
    mutate(term=ifelse(grepl("svi", term), "svi", term),
           term=factor(term, 
                       levels=c("svi", "pct_age1844","pct_age4564","pct_age65plus",
                               "regionSouth","regionMidwest", "regionWest", "regionNortheast",
                                "t00" ,"t11", "t01", "s2",
                                "ll", "df"))) %>% 
    arrange(term)
  colnames(all)[2]<-paste0("rii_", type)
  all
})
# appendix table of coefficients (sii)
model_list_sii<-tibble(type=c("nors", "adj_std", "int"),
                       models=c(model_nors_sii,model_sii, model_sii_region_int))
at2b<-map(1:nrow(model_list_sii), function(i){
  #i<-3
  type<-model_list_sii$type[[i]]
  model<-model_list_sii$models[[i]]
  fixed<-model %>% 
    tidy %>% 
    filter(effect=="fixed", term!="(Intercept)", !grepl("^region", term)) %>% 
    mutate(coef=estimate,
           lci=estimate-1.96*std.error,
           uci=estimate+1.96*std.error,
           term=sub("svi_std\\:", "", term)) %>% 
    select(term, coef, lci, uci) 
  fixed<-fixed %>% 
    mutate(coef=paste0(
      round(coef, digits=1), " (",
      round(lci, digits=1), ";",
      round(uci, digits=1), ")")) %>% 
    select(term, coef)
  if (type=="unadj"){
    fixed<-fixed %>% 
      add_row(term=c("pct_age1844","pct_age4564","pct_age65plus"), coef="")
  }
  if (type=="int"){
    fixed<-fixed %>% 
      add_row(term="regionSouth", coef="1 (Ref.)")
  } else {
    fixed<-fixed %>% 
      add_row(term=c("regionSouth", "regionMidwest", "regionWest", "regionNortheast"), coef="")
  }
  random<-model %>% tidy %>% 
    filter(effect=="ran_pars") %>% 
    mutate(term=case_when(
      grepl("sd__\\(Intercept", term) ~ "t00",
      grepl("cor", term) ~ "t01",
      grepl("sd__svi", term) ~ "t11",
      grepl("Observ", term) ~ "s2"),
      estimate=ifelse(term=="t01", estimate, estimate^2),
      coef=round(estimate, digits=3) %>% as.character) %>% 
    select(term, coef)
  if (type=="nors"){
    random<-random %>% 
      add_row(term=c("t01", "t11"), coef="")
  }
  all<-bind_rows(fixed, random) %>% 
    mutate(term=ifelse(grepl("svi", term), "svi", term),
           term=factor(term, 
                       levels=c("svi", "pct_age1844","pct_age4564","pct_age65plus",
                                "regionSouth","regionMidwest", "regionWest", "regionNortheast",
                                "t00" ,"t11", "t01", "s2",
                                "ll", "df"))) %>% 
    arrange(term)
  colnames(all)[2]<-paste0("sii_", type)
  all
})
at2<-full_join(reduce(at2a, full_join),
               reduce(at2b, full_join))
# pcv of t11
(0.0140-0.0011)/0.0140
(35.425-9.511)/35.425


# SIXTH: domains
# 4 SVI components
all_vax_svi4<-full_join(all_vax %>% select(zcta, svi1_std:svi4_std) %>% 
            gather(component, value, -zcta),
          all_vax %>% select(city_f, state, city_state, zcta, total_pop, fully_vaccinated, fully_vaccinated_prop,
                             pct_age1844, pct_age4564, pct_age65plus, region))
# and for the 4 domains
city_slopes_components_models<-all_vax_svi4 %>% 
  group_by(city_state, component) %>% 
  group_modify(~{
    #.x<-all_vax %>% filter(city_state=="Philadelphia, PA")
    rii<-lm(log(fully_vaccinated_prop)~value+pct_age1844+pct_age4564+pct_age65plus, data=.x)
    sii<-lm((fully_vaccinated_prop)*100~value+pct_age1844+pct_age4564+pct_age65plus, data=.x)
    tibble(outcome=c("rii", "sii"),
      models=list(rii, sii))
  })
# get RII and SII (adjusted) for Table2
rii_sii_components<-city_slopes_components_models %>% 
  group_by(city_state, component) %>% 
  group_modify(~{
    #.x<-city_slopes_components_models %>% filter(city_state=="Houston, TX", component=="svi1_std")
    cbind(.x %>% filter(outcome=="rii") %>% 
            pull(models) %>% `[[`(1) %>% tidy %>% 
            filter(term=="value") %>% 
            mutate(rii=exp(estimate),
                   rii_lci=exp(estimate-1.96*std.error),
                   rii_uci=exp(estimate+1.96*std.error)) %>% 
            select(rii, rii_lci, rii_uci),
          .x %>% filter(outcome=="sii") %>% 
            pull(models) %>% `[[`(1) %>% tidy %>% 
            filter(term=="value") %>% 
            mutate(sii=(estimate),
                   sii_lci=(estimate-1.96*std.error),
                   sii_uci=(estimate+1.96*std.error)) %>% 
            select(sii, sii_lci, sii_uci))
  }) %>% 
  mutate(city_state=factor(city_state))
table3_riisii<-rii_sii_components %>% 
  mutate(sii=paste0(round(sii, digits=2), "\n(", 
                    round(sii_lci, digits=2), ";",
                    round(sii_uci, digits=2), ")")) %>% 
  mutate(rii=paste0(round(rii, digits=2), "\n(", 
                    round(rii_lci, digits=2), ";",
                    round(rii_uci, digits=2), ")")) %>% 
  select(city_state, rii, sii) %>% 
  pivot_wider(id_cols=city_state, names_from=component, values_from = rii:sii) %>% 
  mutate(city_state=factor(city_state, levels=c((all_vax %>% filter(!duplicated(city)) %>% arrange(state, city) %>% pull(city_state)), "Total**"))) %>% 
  arrange(city_state)
# missing total row: get from MLM models!

# then MLM equivalent for appendix
mlm_domains<-all_vax_svi4 %>% group_by(component) %>% 
  group_map(~{
    #.x<-all_vax_svi4 %>% filter(component=="svi1_std")
    model_rii<-lmer(log(fully_vaccinated_prop)~value+pct_age1844+pct_age4564+pct_age65plus+(value|city_state), data=.x, REML=T)
    model_sii<-lmer((fully_vaccinated_prop)*100~value+pct_age1844+pct_age4564+pct_age65plus+(value|city_state), data=.x, REML=T)
    model_nors_rii<-lmer(log(fully_vaccinated_prop)~value+pct_age1844+pct_age4564+pct_age65plus+(1|city_state), data=.x, REML=T)
    model_nors_sii<-lmer((fully_vaccinated_prop)*100~value+pct_age1844+pct_age4564+pct_age65plus+(1|city_state), data=.x, REML=T)
    
    random<-bind_rows(
      model_rii %>% tidy %>% 
      filter(effect=="ran_pars") %>% 
      mutate(term=case_when(
        grepl("sd__\\(Intercept", term) ~ "t00",
        grepl("cor", term) ~ "t01",
        grepl("sd__value", term) ~ "t11",
        grepl("Observ", term) ~ "s2"),
        estimate=ifelse(term=="t01", estimate, estimate^2),
        coef=round(estimate, digits=3) %>% as.character,
        outcome="rii") %>% 
      select(outcome, term, coef),
      model_sii %>% tidy %>% 
        filter(effect=="ran_pars") %>% 
        mutate(term=case_when(
          grepl("sd__\\(Intercept", term) ~ "t00",
          grepl("cor", term) ~ "t01",
          grepl("sd__value", term) ~ "t11",
          grepl("Observ", term) ~ "s2"),
          estimate=ifelse(term=="t01", estimate, estimate^2),
          coef=round(estimate, digits=3) %>% as.character,
          outcome="sii") %>% 
        select(outcome, term, coef)) %>% 
      mutate(component=.y$component)
    list(random=random, 
         model_rii=model_rii, model_sii=model_sii, 
         model_nors_rii=model_nors_rii, model_nors_sii=model_nors_sii, 
         component=.y$component)
  })
# mlm coefs for at4
at4<-map_dfr(mlm_domains, function(temp){
  #temp<-table3_results_all[[1]]
  component<-temp$component
  fixed_rii<-temp$model_rii %>% 
    tidy %>% 
    filter(effect=="fixed", term!="(Intercept)") %>% 
    mutate(coef=estimate,
           lci=estimate-1.96*std.error,
           uci=estimate+1.96*std.error) %>% 
    select(term, coef, lci, uci) %>% 
    mutate_at(-1, exp) %>% 
    mutate(coef=paste0(
      round(coef, digits=2), " (",
      round(lci, digits=2), ";",
      round(uci, digits=2), ")")) %>% 
    select(term, coef)
  fixed_sii<-temp$model_sii %>% 
    tidy %>% 
    filter(effect=="fixed", term!="(Intercept)") %>% 
    mutate(coef=estimate,
           lci=estimate-1.96*std.error,
           uci=estimate+1.96*std.error) %>% 
    select(term, coef, lci, uci) %>% 
    mutate_at(-1, ~.*100) %>% 
    mutate(coef=paste0(
      round(coef, digits=2), " (",
      round(lci, digits=2), ";",
      round(uci, digits=2), ")")) %>% 
    select(term, coef)
  bind_rows(fixed_rii %>% mutate(outcome="rii"), fixed_sii %>% mutate(outcome="sii")) %>% 
    mutate(component=component)
}) %>% 
  spread(outcome, coef)
at4<-full_join(at4 %>% select(-sii) %>% 
                 spread(component, rii) %>% 
                 rename_at(-1, ~paste0("rii_", .)),
               at4 %>% select(-rii) %>% 
                 spread(component, sii) %>% 
                 rename_at(-1, ~paste0("sii_", .))) %>% 
  mutate(term=factor(term, levels=c("value", "pct_age1844","pct_age4564","pct_age65plus"))) %>% 
  arrange(term)
at4b<-map_dfr(mlm_domains, function(xx) xx$random) %>% 
  spread(outcome, coef) 
at4b<-full_join(at4b %>% select(-sii) %>% 
                 spread(component, rii) %>% 
                 rename_at(-1, ~paste0("rii_", .)),
               at4b %>% select(-rii) %>% 
                 spread(component, sii) %>% 
                 rename_at(-1, ~paste0("sii_", .))) %>% 
  mutate(term=factor(term, levels=c("t00", "t11", "t01", "s2", "pval"))) %>% 
  arrange(term)
at4<-bind_rows(at4, at4b)

# at3: get ll and df and do LRT for each model comparison
model_list<-tibble(type=c(rep("rii", times=5),rep("sii", times=5)),
                   component=rep(paste0("svi", c("", 1:4)), times=2),
                   models=list(
                     c(model_nors_rii,model_rii),
                     c(mlm_domains[[1]]$model_nors_rii, mlm_domains[[1]]$model_rii),
                     c(mlm_domains[[2]]$model_nors_rii, mlm_domains[[2]]$model_rii),
                     c(mlm_domains[[3]]$model_nors_rii, mlm_domains[[3]]$model_rii),
                     c(mlm_domains[[4]]$model_nors_rii, mlm_domains[[4]]$model_rii),
                     c(model_nors_sii,model_sii),
                     c(mlm_domains[[1]]$model_nors_sii, mlm_domains[[1]]$model_sii),
                     c(mlm_domains[[2]]$model_nors_sii, mlm_domains[[2]]$model_sii),
                     c(mlm_domains[[3]]$model_nors_sii, mlm_domains[[3]]$model_sii),
                     c(mlm_domains[[4]]$model_nors_sii, mlm_domains[[4]]$model_sii)))
at3<-map_dfr(1:nrow(model_list), function(i){
  #i<-1
  type<-model_list$type[[i]]
  component<-model_list$component[[i]]
  model_ri<-model_list$models[[i]][[1]]
  model_rs<-model_list$models[[i]][[2]]
  ll<-logLik(model_ri)
  df<-attr(ll, "df") %>% as.character
  ll<-as.character(round(ll, digits=1))
  ri<-paste0("logLik=", ll, "; df=", df)
  ll<-logLik(model_rs)
  df<-attr(ll, "df") %>% as.character
  ll<-as.character(round(ll, digits=1))
  rs<-paste0("logLik=", ll, "; df=", df)
  pval<-anova(model_ri, model_rs) %>% tidy %>% filter(term=="model_rs") %>% pull(p.value) 
  pval<-ifelse(pval<0.001, "<0.001", as.character(round(pval, digits=3)))
  data.frame(model=paste0(type, ": ", component),
             ri=ri, rs=rs, pval=pval)
})

# sixth(v2): wrap up talbe 3 with mlm coef
table3<-table3_riisii %>% 
  bind_rows(
    map_dfr(mlm_domains, function(temp){
      #temp<-mlm_domains[[1]]
      model_rii<-temp$model_rii
      model_sii<-temp$model_sii
      component<-temp$component
      bind_rows(full_join(model_rii %>%
                            tidy %>% 
                            filter(term=="value") %>% 
                            mutate(rii=exp(estimate),
                                   rii_lci=exp(estimate-1.96*std.error),
                                   rii_uci=exp(estimate+1.96*std.error)) %>% 
                            mutate(rii=paste0(round(rii, digits=2), "\n(", 
                                              round(rii_lci, digits=2), ";",
                                              round(rii_uci, digits=2), ")")) %>% 
                            mutate(city_state="Total**") %>% 
                            select(city_state, rii),
                          model_sii %>%
                            tidy %>% 
                            filter(term=="value") %>% 
                            mutate(sii=(estimate),
                                   sii_lci=(estimate-1.96*std.error),
                                   sii_uci=(estimate+1.96*std.error)) %>% 
                            mutate(sii=paste0(round(sii, digits=2), "\n(", 
                                              round(sii_lci, digits=2), ";",
                                              round(sii_uci, digits=2), ")")) %>% 
                            mutate(city_state="Total**") %>% 
                            select(city_state, sii))) %>% 
        mutate(component=component)
    }) %>% 
      pivot_wider(id_cols=city_state, names_from=component, values_from=rii:sii)
  )


# save results
write_csv(table1 , file="results/table1.csv")
write_csv(table2, file="results/table2.csv")
write_csv(table3, file="results/Table3.csv")
write_csv(at2, file="results/AppendixTable2.csv")
write_csv(at3, file="results/AppendixTable3.csv")
write_csv(at4, file="results/AppendixTable4.csv")
ggsave("results/figure1.pdf", figure1, width=12, height=10)
ggsave("results/figure2.pdf", figure2, width=14, height=7.5)
ggsave("results/AppendixFigure1.pdf", af1, width=12, height=10)
ggsave("results/AppendixFigure2.pdf", af2, width=14, height=7.5)
ggsave("results/AppendixFigure3.pdf", af3, width=10, height=7.5)
ggsave("results/AppendixFigure4.pdf", af4, width=10, height=7.5)
ggsave("results/AppendixFigure5.pdf", af5, width=10, height=7.5)

