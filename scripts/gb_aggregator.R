library(tidyverse)
library(cmdstanr)

parties <- c("Conservative Party","Labour Party","Liberal Democrats","Scottish National Party","Green Party","Reform UK","Other")

polls <- read_csv("https://filipvanlaenen.github.io/eopaod/gb-gbn-N.csv") %>%
  filter(str_detect(`Sample Size`, "[0-9]+")) %>%
  mutate(size = parse_number(`Sample Size`)) %>%
  select(`Polling Firm`,`Fieldwork Start`,`Fieldwork End`,size,
         `Conservative Party`,`Labour Party`,`Liberal Democrats`,`Scottish National Party`,`Green Party`,`Brexit Party`) %>%
  mutate(across(c(`Conservative Party`,`Labour Party`,`Liberal Democrats`,`Scottish National Party`,`Green Party`,`Brexit Party`),
                ~ifelse(.x != "Not Available",parse_number(.x),NA))) %>%
  rename(start = `Fieldwork Start`,
         end = `Fieldwork End`,
         pollster = `Polling Firm`,
         `Reform UK` = `Brexit Party`) %>%
  mutate(Other = 100 - `Conservative Party` - `Labour Party` - `Liberal Democrats` - `Scottish National Party` - `Green Party` - `Reform UK`) %>%
  mutate(across(parties, ~size*.x/100)) %>%
  mutate(across(all_of(parties), ~ifelse(is.na(.x),0,.x))) %>%
  mutate(across(all_of(parties), ~as.integer(round(.x)))) %>%
  filter(start >= as.Date("2019-12-12"),
         !is.na(pollster)) %>%
  mutate(t = interval(start = as.Date("2019-12-12"), end = start) %/% weeks(1) + 1)
  

data_list <- list(
  T = max(polls$t),
  J = length(parties),
  P = length(unique(polls$pollster)),
  N = nrow(polls),
  t = polls$t,
  vi = polls[,parties] %>% as.matrix,
  pollster = as_factor(polls$pollster) %>% as.numeric(),
  size = polls$size,
  vote0 = c(0.436, 0.321, 0.116, 0.039, 0.0261, 0.0201, 1 - sum(0.436, 0.321, 0.116, 0.039, 0.0261, 0.0201))
)

model <- cmdstan_model("models/multinomial_dirichlet_model.stan")

fit <- model$sample(
  data = data_list,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000
)

### Create polling aggregation plot

polls_long <- polls
polls_long[,parties] <- polls[,parties] / rowSums(polls[,parties])

polls_long %>%
  mutate(across(all_of(parties), ~ifelse(.x==0,NA,.x))) %>%
  pivot_longer(cols = all_of(parties), names_to = "party", values_to = "vote") %>%
  rename(date = start) %>%
  mutate(party = factor(party, levels = parties)) %>%
  filter(vote != -1) %>%
  select(date,party,vote) -> polls_long

fit$draws("vote", format = "matrix") %>%
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
  t() %>%
  as_tibble() %>%
  bind_cols(
    expand_grid(party = parties,
                t = 1:max(polls$t))
  ) %>%
  mutate(party = factor(party, levels = parties)) %>%
  mutate(date = weeks(t-1) + date("2019-12-12")) %>%
  ggplot(aes(x = date)) +
  geom_point(data = polls_long, aes(y = vote, color = party), alpha = 0.3) +
  geom_line(aes(y = `50%`, color = party)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`, fill = party), alpha = 0.3) +
  scale_color_manual(values = c("#2c88df","#d90f32","#f3a600","#fcf287","#3aa859","#42b6d1","grey")) +
  scale_fill_manual(values = c("#2c88df","#d90f32","#f3a600","#fcf287","#3aa859","#42b6d1","grey")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = NULL,
       color = "Party",
       fill = "Party",
       title = "Bayesian poll of polls for the United Kingdom",
       subtitle = "Polls courtesy of EuropeElects",
       caption = "@jwhandley17") +
  theme_light()
ggsave("graphs/poll_of_polls_gb.png",width=16,height=10)
