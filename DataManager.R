### DB Wrangle
pacman::p_load("tidyverse","lubridate")


db %>%
  select(
    Catalogue, Item, Text, h_i = pre_n.k_i,
    User, h_u = h.u, y = Score,
    Day, Week, Month, Year,
    Likes, Dislikes,
    avg.x_h_i = pre_avg.k_i
    ) %>%
  filter(User != "[Anonymous]") %>%
  group_by(User) %>% mutate(k_u = n(),
                            avg.y_h_u = map_dbl(h_u, ~mean(y[h_u < .x]))) %>%
  group_by(Item) %>% mutate(k_i = n(),
                            h_i = h_i+1,
                            t_i = (Day - first(Day[h_i == 1])) %>%
                              as.integer()+1) -> db

###

db %>% group_by(Item,h_i) %>% 
  mutate(
    pol2 = map_dbl(h_i,
                        ~2*min(
                          (sum(y[h_i < .x]==0))/sum(h_i < .x),
                          (sum(y[h_i < .x]==10))/sum(h_i < .x))
  )) -> DB$db


# Userlist

db %>% filter() %>%
  group_by(User) %>%
  arrange(Day) %>%
  summarise(
    first_rev = first(Day),
    k_u = n(),
    avg.y = mean(y),
    sd.y = sd(y),
    pol_1 = sum(y %in% c(0,1))/n(),
    pol_2 = 2*min(
      sum(y == 0)/n(),
      sum(y == 10)/n())
  ) -> u_tbl

#

TLOU2 <- db %>%
  ungroup %>% filter(Item == "The Last of Us Part II (PlayStation 4)") %>%
  select(-c(Catalogue,Item,k_i))

TLOU2 %>%
  mutate(
    nchar = str_length(Text),
    bugged = str_detect(Text,"Expand\n")
  ) -> TLOU2

write_csv(TLOU2, "TLOU2.csv")
