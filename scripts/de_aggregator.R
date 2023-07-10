library(tidyverse)
library(cmdstanr)

COUNTRY <- "de"

polls <- read_csv(paste("https://filipvanlaenen.github.io/eopaod/",COUNTRY,"-N.csv",sep="")) %>%
  filter(str_detect(`Sample Size`, "[0-9]+")) %>%
  mutate(size = parse_number(`Sample Size`)) %>%
  select(`Polling Firm`,`Fieldwork Start`,`Fieldwork End`,size,
         Union,SPD,`Alternative für Deutschland`,FDP,`DIE LINKE`,`BÜNDNIS 90/DIE GRÜNEN`,CDU,CSU,Other) %>%
  mutate(across(c(Union,SPD,`Alternative für Deutschland`,FDP,`DIE LINKE`,`BÜNDNIS 90/DIE GRÜNEN`,CDU,CSU,Other),
                ~ifelse(.x != "Not Available",parse_number(.x),NA))) %>%
  mutate(Union = ifelse(is.na(Union),CDU + CSU,Union)) %>%
  rename(AfD = `Alternative für Deutschland`,
         Green = `BÜNDNIS 90/DIE GRÜNEN`,
         Linke = `DIE LINKE`,
         start = `Fieldwork Start`,
         end = `Fieldwork End`,
         pollster = `Polling Firm`) %>%
  select(-CDU,-CSU) %>%
  mutate(Other = 100 - Union - SPD - AfD - FDP - Linke - Green) %>%
  mutate(across(c(Union,SPD,AfD,FDP,Linke,Green,Other), ~.x/100)) %>%
  filter(start >= as.Date("2017-09-24")) %>%
  mutate(t = interval(start = as.Date("2017-09-24"), end = start) %/% weeks(1) + 1)

data_list <- list(
  T = max(polls$t),
  J = 7,
  P = length(unique(polls$pollster)),
  N = nrow(polls),
  t = polls$t,
  vi = polls[,c("Union","SPD","AfD","FDP","Linke","Green","Other")],
  pollster = as_factor(polls$pollster) %>% as.numeric(),
  size = polls$size,
  vote0 = c(0.329, 0.205, 0.126, 0.107, 0.092, 0.089, 1 - sum(0.329, 0.205, 0.126, 0.107, 0.092, 0.089))
)

model <- cmdstan_model("models/poll_aggregator.stan")

fit <- model$sample(
  data = data_list,
  chains = 6,
  parallel_chains = 6,
  iter_warmup = 1000,
  iter_sampling = 1000
)

### Create polling aggregation plot

polls %>%
  pivot_longer(cols = c("Union","SPD","AfD","FDP","Linke","Green","Other"), names_to = "party", values_to = "vote") %>%
  rename(date = start) %>%
  mutate(party = factor(party, levels = c("Union","SPD","AfD","FDP","Linke","Green","Other"))) %>%
  select(date,party,vote) -> polls_long

fit$draws("vote", format = "matrix") %>%
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
  t() %>%
  as_tibble() %>%
  bind_cols(
    expand_grid(party = c("Union","SPD","AfD","FDP","Linke","Green","Other"),
                t = 1:max(polls$t))
  ) %>%
  mutate(party = factor(party, levels = c("Union","SPD","AfD","FDP","Linke","Green","Other"))) %>%
  mutate(date = weeks(t-1) + date("2017-09-24")) %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept = 0.05) +
  geom_point(data = polls_long, aes(y = vote, color = party), alpha = 0.3, size = 0.5) +
  geom_line(aes(y = `50%`, color = party)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`, fill = party), alpha = 0.3) +
  scale_color_manual(values = c("#000000","#e00f00","#369fe3","#fdec00","#b53474","#6da022","grey")) +
  scale_fill_manual(values = c("#000000","#e00f00","#369fe3","#fdec00","#b53474","#6da022","grey")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = NULL,
       color = "Party",
       fill = "Party",
       title = "Bayesian poll of polls for Germany",
       subtitle = "Polls courtesy of EuropeElects",
       caption = "@jwhandley17") +
  theme_light()
ggsave("graphs/poll_of_polls_de.png",width=16,height=10)
