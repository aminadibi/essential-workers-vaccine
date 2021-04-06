library(Cairo)

source('./analysis/setup.R')

# parameter space
pars <- crossing(R=c(1.15, 1.3, 1.5), ve = c(0.6,0.75,0.9), vp = 0.9, scen=c(1,2))

# RUN (according to piecewise scenario)
res <- pars %>%  future_pmap_dfr(run_over_scen_2, .progress=TRUE)

############################
# FIGURE 1 (trajectories)
#############################
# Look at trajectories
trajA <- compare_sims(sim1 = filter(res, R==1.3 & scen==1 & ve==0.75), 
                             sim2=filter(res, R==1.3 & scen ==2 & ve==0.75),
                             name1=labels[1], name2=labels[2], startDate=startDate, 
                             textsize = 16)

# trajB <- compare_sims(sim1 = filter(res, R==1.3 & scen==3 & ve==0.75), 
#                              sim2=filter(res, R==1.3 & scen==4 & ve==0.75),
#                              name1=labels[3], name2=labels[4], startDate=startDate, 
#                              textsize = 16)

# Look at number vaccinated
gg_vax <- res %>% 
    filter(R==1.15) %>% # note: all R values will give approx. the same plot
    group_by(type)%>% 
    nest()%>%
    summarize(plot=map(data, display_prop_vax, startDate, type, textsize=16))


fig1a = ggarrange(gg_vax$plot[[1]]+
                    ggtitle(subtitle = 'A: 80+, 70-79, EW, 60-69, ...') +
                    scale_color_viridis(discrete = T)+
                    theme_ipsum_rc(grid="XY") + 
                    theme(axis.title.x = element_blank(),text=element_text(size=16))
                    ,
                  gg_vax$plot[[2]]+ggtitle('B: 80+, 70-79, 60-69, EW, 50-59, ...') +
                    scale_color_viridis(discrete = T)+
                    theme_ipsum_rc(grid="XY"),
                  ncol=2, nrow=1, common.legend=TRUE, legend="bottom")


# Arrange
fig1b = ggarrange(ggarrange(plotlist=trajA, nrow=2, ncol=2, widths = c(1,1,1,1),
                            common.legend=TRUE, legend="bottom"),
           #ggarrange(plotlist=trajB, nrow=1, ncol=4, widths = c(1,1,1,1)),
           nrow=1)
 
ggsave("figures/fig-trajectories.pdf", width = 45, height = 10, device = cairo_pdf)      

ggarrange(fig1a, fig1b, nrow=2,heights = c(1, 1.6))
ggsave("figures/fig-trajectoriesFull.pdf", width = 15, height = 10, device = cairo_pdf)      


##########################
# FIG 2 (bar plots)
###########################
tsize <- 16

#---SUMMARIZE
res2 <- res %>% 
    group_by(type, R, ve,vp) %>%
    nest() %>%
    summarize(cases=map_dbl(data, total_cases),
              hosp=map_dbl(data, total_hosp,hosp_efficacy=rep(vp, 9)),
              deaths=map_dbl(data, total_deaths),
              long = map_dbl(data, total_long, hosp_efficacy=rep(vp, 9)),
              t_turn = map_dbl(data, time_to_decr),
              cases_turn = map_dbl(data,cases_immunity),
              vax_immunity = map_dbl(data, vax_immunity)
)

R_vec <- c(1.15,1.3) # R vals to plot

g1 <- ggplot(filter(res2, R %in% R_vec), aes(x=ve, y=cases, group=type, fill=type))+
  geom_col(position='dodge', alpha=1)+ 
  #scale_fill_brewer(palette = "Dark2")+ 
  facet_wrap(~ R,  ncol=4,labeller=label_both)+
  #scale_color_brewer(palette = "Dark2")+
  theme(text=element_text(size=16))+
  theme(panel.spacing.x=unit(1.5, "lines"))+
  labs(x='Efficacy against Infection', y='Infections', fill='Strategy') +
            theme(text=element_text(size=tsize))+
            theme(panel.spacing.x=unit(1.5, "lines"),
                          axis.text.x = element_text(angle = 35,hjust = 1))+
  scale_x_continuous(breaks=c(0.6,0.75,0.9)) +
  scale_y_continuous(labels = scales::comma) +
  theme_ipsum_rc(grid="Y")

g2 <- ggplot(filter(res2, R %in% R_vec), aes(x=ve, y=hosp, group=type, fill=type))+
  geom_col(position='dodge',alpha=1)+ 
  #scale_fill_brewer(palette = "Dark2")+ 
  facet_wrap(~ R,  ncol=4,labeller=label_both)+
  #scale_color_brewer(palette = "Dark2")+
  theme(text=element_text(size=tsize))+
  theme(panel.spacing.x=unit(1.5, "lines"))+
  labs(x='Efficacy against Infection', y='Hospitalizations', fill='Strategy') +
            theme(text=element_text(size=16))+theme(panel.spacing.x=unit(1.5, "lines") ,
                          axis.text.x = element_text(angle = 35,hjust = 1))+
  scale_x_continuous(breaks=c(0.6,0.75,0.9)) + theme_ipsum_rc(grid="Y")

g3 <- ggplot(filter(res2, R %in% R_vec), aes(x=ve, y=deaths, group=type, fill=type))+
  geom_col(position='dodge',alpha=1)+ 
  #scale_fill_brewer(palette = "Dark2")+ 
  facet_wrap(~ R,  ncol=4, labeller=label_both)+
  #scale_color_brewer(palette = "Dark2")+
  theme(text=element_text(size=16))+
  theme(panel.spacing.x=unit(1.5, "lines"))+
  labs(x='Efficacy against Infection', y='Deaths', fill='Strategy') +
            theme(text=element_text(size=tsize))+theme(panel.spacing.x=unit(1.5, "lines") ,
                          axis.text.x = element_text(angle = 35,hjust = 1))+
  scale_x_continuous(breaks=c(0.6,0.75,0.9)) + theme_ipsum_rc(grid="Y")

g4 <- ggplot(filter(res2, R %in% R_vec), aes(x=ve, y=long, group=type, fill=type))+
  geom_col(position='dodge',alpha=1)+ 
  #scale_fill_brewer(palette = "Accent")+ 
  facet_wrap(~ R,  ncol=4,labeller=label_both)+
  #scale_color_brewer(palette = "Accent")+
  theme(text=element_text(size=16))+
  theme(panel.spacing.x=unit(1.5, "lines"))+
  labs(x='Efficacy against Infection', y='Long COVID', fill='Strategy') +
            theme(text=element_text(size=tsize))+theme(panel.spacing.x=unit(1.5, "lines") ,
                          axis.text.x = element_text(angle = 35,hjust = 1))+
  scale_x_continuous(breaks=c(0.6,0.75,0.9)) +
  theme_ipsum_rc(grid="Y") 

bars <- ggarrange(g1,g2,g3,g4, ncol=2, nrow=2, common.legend=TRUE, legend='bottom', align='v')

ggsave('figures/fig-barplots.pdf', width=14, height=10,  device = cairo_pdf)


### Personal Risk

oo <- compare_sims_data(sim1 = filter(res, R==1.3 & scen==1 & ve==0.75), 
                                               sim2=filter(res, R==1.3 & scen ==2 & ve==0.75),
                                               name1=labels[1], name2=labels[2], startDate=startDate, 
                                               textsize = 16)


oo %>% filter (age_band == "50-59" & scen == "B: 80+, 70-79, 60-69, EW, 50-59, ..." & 
                 date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 709300

oo %>% filter (age_band == "40-49" & scen == "B: 80+, 70-79, 60-69, EW, 50-59, ..." & 
                 date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 617410

oo %>% filter (age_band == "30-39" & scen == "B: 80+, 70-79, 60-69, EW, 50-59, ..." & 
                 date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 607340

oo %>% filter (age_band == "20-29" & scen == "B: 80+, 70-79, 60-69, EW, 50-59, ..." & 
                 date<ymd("2021-06-01") & date>ymd("2021-04-01")) %>%
  select(incid, newdeaths, hosp, long) %>% summarise_all(sum) / 590560

#####################
# QALYs and Cost
#####################
source('./analysis/qalys_cost.R')
