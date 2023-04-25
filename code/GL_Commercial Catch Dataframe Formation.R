##code to import catch data if wanted/needed for future
erie_fish <- read_excel("data/commercial.xlsx", sheet = "Erie") %>%
  select(Year, Lake, Species, `Grand Total`) %>%
  filter(!is.na(`Grand Total`)) %>%
  group_by(Lake, Species) %>%
  summarise(total_catch_allyears = sum(`Grand Total`))
ontario_fish <- read_excel("data/commercial.xlsx", sheet = "Ontario") %>%
  select(Year, Lake, Species, `Grand Totals`) %>%
  filter(!is.na(`Grand Totals`)) %>%
  group_by(Lake, Species) %>%
  summarise(total_catch_allyears = sum(`Grand Totals`))
huron_fish <- read_excel("data/commercial.xlsx", sheet = "Huron") %>%
  select(Year, Lake, Species, `Grand Total`) %>%
  filter(!is.na(`Grand Total`)) %>%
  group_by(Lake, Species) %>%
  summarise(total_catch_allyears = sum(`Grand Total`))
superior_fish <- read_excel("data/commercial.xlsx", sheet = "Superior") %>%
  select(Year, Lake, Species, `Grand Total`) %>%
  filter(!is.na(`Grand Total`)) %>%
  group_by(Lake, Species) %>%
  summarise(total_catch_allyears = sum(`Grand Total`))

michigan_fish <- read_excel("data/commercial.xlsx", sheet = "Michigan") %>%
  select(Year, Lake, Species, `U.S. Total`) %>%
  filter(!is.na(`U.S. Total`)) %>%
  group_by(Lake, Species) %>%
  summarise(total_catch_allyears = sum(`U.S. Total`))

stclair_fish <- read_excel("data/commercial.xlsx", sheet = "St. Clair")%>%
  select(Year, Lake, Species, `Grand Total`) %>%
  filter(!is.na(`Grand Total`)) %>%
  group_by(Lake, Species) %>%
  summarise(total_catch_allyears = sum(`Grand Total`))

##Create df of all fish
gl_fish <- rbind(erie_fish, huron_fish, ontario_fish, superior_fish, michigan_fish, stclair_fish) %>%
  group_by(Species) %>%
  summarise(total_catch_alllakes = sum(total_catch_allyears))