# process "legacy" catch data from Litzow & Urban 2009
# and run DFA

library(tidyverse)
library(MARSS)
theme_set(theme_bw())
# set colors
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## data processing -------------------------------------
#From 03_JoinLegacy_Current_SalmonCatchData.r
head(DatAll)

#remove non-catch data and remove species that can't be analyzed together (King Crab has no legacy, herring and shrimp have diff slopes between legacy and current)
dat=DatAll %>%
  select(-c(color.x, dataset.x,color.y, dataset.y)) %>%
  select(-c(KingCrab_mt,Pherring_mt,Shrimp_mt)) %>%
  filter (year<2021) #full time series isi 1956-2020

head(dat)

# sum by group and plot total catch
sum.catch <- dat %>%
  pivot_longer(cols = -year, names_to = "group", values_to = "catch") %>%
  group_by(group) %>%
  summarise(catch = sum(catch, na.rm = T))

# sort and plot
sum.catch$group <- reorder(sum.catch$group, desc(sum.catch$catch))

ggplot(sum.catch, aes(group, catch/10000)) +
  geom_bar(stat = "identity", fill = "dark grey", color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_blank()) +
  scale_y_continuous(breaks=c(0,1,5,10,20,50,100,200,300.400,500,800,1000)) +
  coord_trans(y = "pseudo_log") +
  labs(title = "Total catch 1956-2020",
       y = "Catch (10,000 t)")

# and cumulative catch
cumulative.catch <- sum.catch %>%
  arrange(desc(catch)) %>%
  mutate(prop.total = catch / sum(sum.catch$catch)) %>%
  mutate(cumulative = cumsum(prop.total))

# keep the groups making up 99% of the catch
keep <- cumulative.catch$group[cumulative.catch$cumulative < 0.99]

# restict DFA data to these groups
dfa.dat <- dat %>%
  select(all_of(keep))

# I think all NA should be 0 catch 
# make that change
change <- is.na(dfa.dat)

dfa.dat[change] <- 0

# divide by 1000s of t and fourth-root transform
dfa.dat <- (dfa.dat/1000)^0.25

# make some new exploratory plots

dfa.dat <- dfa.dat %>%
  mutate(year = 1956:2020) %>%
  pivot_longer(cols = -year)

# first look at time series
ggplot(dfa.dat, aes(year, value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~name)

# now distributions
ggplot(dfa.dat, aes(value)) +
  geom_histogram(bins = 12, fill = "grey", color = "black") +
  facet_wrap(~name, scales = "free")

# DFA model selection ---------------------------------------------
# set up data

dfa.dat <- dfa.dat %>%
  pivot_wider(names_from = name, values_from = value) %>% # this gives 1956:2020; use all
  select(-year) %>%
  t()
  

colnames(dfa.dat) <- 1956:2020

# set up forms of R matrices
levels.R = c("diagonal and equal",
             "diagonal and unequal",
             "equalvarcov",
             "unconstrained")
model.data = data.frame()

# changing convergence criterion to ensure convergence
cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)

# fit models & store results
for(R in levels.R) {
  for(m in 1:2) {  # allowing up to 2 trends
    dfa.model = list(A="zero", R=R, m=m)
    kemz = MARSS(dfa.dat, model=dfa.model,
                 form="dfa", z.score=TRUE, control=cntl.list)
    model.data = rbind(model.data,
                       data.frame(R=R,
                                  m=m,
                                  logLik=kemz$logLik,
                                  K=kemz$num.params,
                                  AICc=kemz$AICc,
                                  stringsAsFactors=FALSE))
    assign(paste("kemz", m, R, sep="."), kemz)
  } # end m loop
} # end R loop

# calculate delta-AICc scores, sort in descending order, and compare
model.data$dAICc <- model.data$AICc-min(model.data$AICc)
model.data <- model.data %>%
  arrange(dAICc)
model.data

# save model selection table
write.csv(model.data, "../Results/All catch dfa model selection table 1956-2020.csv",
          row.names = F)

## fit the best model --------------------------------------------------
model.list = list(A="zero", m=2, R="unconstrained") # best model for early era

# not sure that these changes to control list are needed for this best model, but using them again!
cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)

mod = MARSS(dfa.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# this model has convergence problems - plot to evaluate - same with "current" data

# and rotate the loadings
Z.est = coef(mod, type="matrix")$Z
H.inv = varimax(coef(mod, type="matrix")$Z)$rotmat
Z.rot = as.data.frame(Z.est %*% H.inv)

# reverse trend 2 to plot
# Z.rot[,2] <- -Z.rot[,2]

Z.rot$names <- rownames(dfa.dat)
Z.rot <- arrange(Z.rot, V1)
Z.rot <- gather(Z.rot[,c(1,2)])
Z.rot$names <- rownames(dfa.dat)
Z.rot$plot.names <- reorder(Z.rot$names, 1:length(Z.rot$names))

loadings1 <- ggplot(Z.rot, aes(plot.names, value, fill=key)) + geom_bar(stat="identity", position="dodge") +
  ylab("Loading") + xlab("") + ggtitle("1956-2020 current catch - unconstrained") + 
  scale_fill_manual(values=cb[2:3]) +
  theme(legend.position = c(0.8,0.2), legend.title=element_blank()) + geom_hline(yintercept = 0) +
  theme(axis.text.x  = element_text(angle=45, hjust=1, size=12)) 

# compare with loadings for 2nd-best model
model.list = list(A="zero", m=2, R="diagonal and unequal") # best model for early era
mod2 = MARSS(dfa.dat, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)

# this model converges easily

# and rotate the loadings
Z.est = coef(mod2, type="matrix")$Z
H.inv = varimax(coef(mod2, type="matrix")$Z)$rotmat
Z.rot = as.data.frame(Z.est %*% H.inv)

# reverse trend 2 to plot
# Z.rot[,2] <- -Z.rot[,2]

Z.rot$names <- rownames(dfa.dat)
Z.rot <- arrange(Z.rot, V1)
Z.rot <- gather(Z.rot[,c(1,2)])
Z.rot$names <- rownames(dfa.dat)
Z.rot$plot.names <- reorder(Z.rot$names, 1:length(Z.rot$names))

loadings2 <- ggplot(Z.rot, aes(plot.names, value, fill=key)) + geom_bar(stat="identity", position="dodge") +
  ylab("Loading") + xlab("") + ggtitle("1956-2020 current catch - diagonal and unequal") + 
  scale_fill_manual(values=cb[2:3]) +
  theme(legend.position = c(0.8,0.2), legend.title=element_blank()) + geom_hline(yintercept = 0) +
  theme(axis.text.x  = element_text(angle=45, hjust=1, size=12)) 

# save a fig combining loadings plots for each model
png("../Figs/All_dfa_mod1_mod2_loadings_plot.png", 
    width=5, height=8, units='in', res=300)

ggpubr::ggarrange(loadings1, loadings2, nrow = 2, ncol = 1)

dev.off()

# loadings are generally similar, especially for trend 1
# propose for now that we use model 2

# plot trends for mod2
# first rotate the trends!
H.inv = varimax(coef(mod2, type="matrix")$Z)$rotmat
trends.rot = solve(H.inv) %*% mod2$states

trends.plot <- data.frame(year = 1956:2020,
                          trend1 = trends.rot[1,],
                          trend2 = trends.rot[2,]) %>%
  pivot_longer(cols = -year)

ggplot(trends.plot, aes(year, value, color = name)) +
  geom_line() +
  scale_color_manual(values = cb[2:3]) +
  ggtitle("1956-2020 current catch - diagonal and unequal") +
  theme(axis.title.x = element_blank()) +
  geom_hline(yintercept = 0)

ggsave("../Figs/All_catch_trend_plot_diagonal_unequal.png", width = 5.5, height = 3, units = 'in')

