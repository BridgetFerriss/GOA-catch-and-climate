# process "legacy" catch data from Litzow & Urban 2009
# and save for dfa, etc.

library(tidyverse)

## data processing -------------------------------------
dat <- read.csv("./Data/legacy.goa.catch.csv")

head(dat)

# clean up - halibut and herring need to be combined between SE and rest of Gulf
# and limit to 1956 - 1990 (new data begin in 1991)

dat <- dat %>% 
  mutate(pacific.halibut = pacific.halibut + pacific.halibut.se) %>%
  select(-pacific.halibut.se) %>%
  mutate(pacific.herring = pacific.herring + pacific.herring.se) %>%
  select(-pacific.herring.se) %>%
  filter(year %in% 1956:1990)

# remove 'rockfish' as this is a sum of the other categories
dat <- dat %>%
  select(-rockfish)

# sum by group and plot total catch

sum.catch <- dat %>%
  pivot_longer(cols = -year, names_to = "group", values_to = "catch") %>%
  group_by(group) %>%
  summarise(catch = sum(catch, na.rm = T))

# remove ATF as no catch through 1990!
sum.catch <- sum.catch %>%
  filter(group != "arrowtooth.flounder")

# sort and plot
sum.catch$group <- reorder(sum.catch$group, desc(sum.catch$catch))

ggplot(sum.catch, aes(group, catch/10000)) +
  geom_bar(stat = "identity", fill = "dark grey", color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x = element_blank()) +
  scale_y_continuous(breaks=c(0,1,5,10,20,50,100,200)) +
  coord_trans(y = "pseudo_log") +
  labs(title = "Total catch 1956-1990",
       y = "10,000 t")

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
  mutate(year = 1956:1990) %>%
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

# save for analysis
write.csv(dfa.dat, "./data/legacy.dfa.dat.csv", row.names = F)
