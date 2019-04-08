
### Game of Thrones
### Sankey plot of survival
### By Jehan Gonsal

### Data sourced from: https://www.kaggle.com/mylesoneill/game-of-thrones

library(tidyverse)
library(ggalluvial)
library(ggimage)

df_battles <- read_csv("battles.csv")
df_deaths <- read_csv("character-deaths.csv")

df_deaths <- df_deaths %>% 
  filter(`Death Year` > 297 | is.na(`Death Year`))

df_deaths <- df_deaths %>%
  mutate(Allegiances = gsub("House ", "", Allegiances),
         Allegiances = trimws(Allegiances))

df_deaths <- df_deaths %>%
  mutate(Book_Death = case_when(`Book of Death` == 1 ~ "A Game of Thrones",
                                `Book of Death` == 2 ~ "A Clash of Kings",
                                `Book of Death` == 3 ~ "A Storm of Swords",
                                `Book of Death` == 4 ~ "A Feast for Crows",
                                `Book of Death` == 5 ~ "A Dance of Dragons",
                                is.na(`Book of Death`)~ "The Winds of Winter"),
         Book_Death = fct_relevel(Book_Death, c("A Game of Thrones",
                                                "A Clash of Kings",
                                                "A Storm of Swords",
                                                "A Feast for Crows",
                                                "A Dance of Dragons",
                                                "The Winds of Winter")))
# Get total character count for checking

total <- df_deaths %>%
  summarise(Deaths = n()) %>%
  as.numeric()

# Get our main houses

houses <- df_deaths %>% 
  group_by(Allegiances) %>%
  summarise(count = n()) %>%
  filter(!Allegiances %in% c("Night's Watch", "None", "Wildling")) %>%
  mutate(rank = rank(-count)) %>%
  arrange(rank) %>%
  select(Allegiances) %>%
  unlist()

## First chart

df_deaths %>%
  group_by(Book_Death) %>%
  summarise(Dead = n()) %>%
  mutate(Alive = sum(Dead) - cumsum(Dead)) %>%
  mutate(Dead = cumsum(Dead)) %>%
  filter(Book_Death != "The Winds of Winter") %>%
  gather(Event, Count, -Book_Death) %>%
  mutate(book_image = case_when(Book_Death == "A Game of Thrones" ~ "https://i.harperapps.com/hcanz/covers/9780006479888/y648.jpg",
                   Book_Death == "A Clash of Kings" ~ "https://images-na.ssl-images-amazon.com/images/I/91Nl6NuijHL.jpg",
                   Book_Death == "A Storm of Swords" ~ "https://images-na.ssl-images-amazon.com/images/I/91d-77kn-dL.jpg",
                   Book_Death == "A Feast for Crows" ~ "https://images-na.ssl-images-amazon.com/images/I/81MylCMYnVL.jpg",
                   Book_Death == "A Dance of Dragons" ~ "https://images-na.ssl-images-amazon.com/images/I/81e1rZDeBBL.jpg")) %>%
  ggplot(aes(fill  = Event, x = Book_Death, y = Count, stratum = Event, alluvium = Event, label = Event)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft",
            color = "darkgray") +
  geom_stratum() + 
  theme_classic() +
  #geom_image(aes(image = book_image), size = 0.1, y = 800) +
  scale_fill_manual(values = c("light blue", "salmon")) +
  labs(x = "Book", y = "Count", title = "Sankey Diagram of Game of Thrones Series' Deaths",
       subtitle = "Chart indicates cumulative deaths over time")

### Add houses
### Still a work in progress! :)

df_deaths %>%
  filter(Allegiances %in% houses) %>%
  group_by(Book_Death, Allegiances) %>%
  summarise(Dead = n()) %>%
  right_join(
    df_deaths %>%
      filter(Allegiances %in% houses) %>%
      select(Book_Death, Allegiances) %>%
      unique() %>%
      expand(Book_Death, Allegiances),
    by = c("Book_Death", "Allegiances")) %>%
  group_by(Allegiances) %>%
  mutate(Dead = ifelse(is.na(Dead), 0, Dead),
         Alive = sum(Dead) - cumsum(Dead),
         Dead = cumsum(Dead)) %>%
  filter(Book_Death != "The Winds of Winter") %>%
  gather(Event, Count, -Book_Death, -Allegiances) %>%
  mutate(key_events = case_when(Allegiances == "Targaryen" &
                                  Book_Death == "A Game of Thrones" &
                                  Event == "Alive" ~ "https://vignette.wikia.nocookie.net/gameofthrones/images/1/1c/Daenerys_and_dragon.jpg/revision/latest/scale-to-width-down/205?cb=20160718051855")) %>%
  ggplot(aes(fill  = Event, x = Book_Death, y = Count, stratum = Event, alluvium = Event, label = Event)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft",
            color = "darkgray") +
  geom_stratum() + 
  theme_classic() +
  facet_grid(Allegiances~.) +
  geom_image(aes(image = key_events), size = 0.2, na.rm = TRUE, by = "height") +
  scale_fill_manual(values = c("light blue", "salmon")) +
  labs(x = "Book", y = "Count", title = "Sankey Diagram of Game of Thrones Series' Deaths",
       subtitle = "Chart indicates cumulative deaths over time")


