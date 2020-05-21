library(tidyverse)
library(janitor)
library(knitr)

spotify_songs_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
unique(spotify_songs$track_popularity)

spotify_songs <- spotify_songs_raw %>% 
  mutate(
    mode = case_when(mode == 1 ~ "major",
                     mode == 0 ~ "minor"),
    track_album_release_date = as.Date(track_album_release_date),
    year = as.numeric(format(track_album_release_date,'%Y')),
    era = case_when(year <= 2000 ~ "under 00s era",
                    year > 2000 & year <= 2010 ~ "2010 era",
                    year > 2010 ~ "modern era")
  )

glimpse(spotify_songs)


spotify_songs %>%
  group_by(mode, playlist_genre) %>%
  count(mode, sort = TRUE) %>%
  ggplot() +
  geom_bar(aes(playlist_genre, n, fill=mode), position="fill", stat="identity") +
  geom_hline(aes(yintercept=0.5)) +
  coord_flip() +
  labs(
    x='Proportion', y='Playlist Genre'
  ) +
  theme_minimal()


spotify_songs %>%
  group_by(era) %>%
  count(era, sort = TRUE) %>%
  na.omit() %>%
  ggplot(aes(era, y=n, fill=era)) +
  geom_bar(stat="identity") +
  labs(x='Era', y='Count') +
  theme_minimal()


spotify_songs %>%
  group_by(playlist_genre, era) %>%
  na.omit() %>%
  ggplot() +
  geom_density(aes(tempo, fill=playlist_genre)) +
  facet_grid(playlist_genre~era) +
  scale_x_log10() +
  labs(x='Tempo') +
  theme_minimal() +
  theme(legend.position = 'none')


spotify_songs %>%
  select(track_name, track_artist, track_popularity) %>%
  arrange(-track_popularity) %>%
  distinct() %>%
  filter(track_popularity %in% c(97:100)) %>% 
  knitr::kable()


spotify_songs %>%
  group_by(playlist_genre, mode) %>%
  count(playlist_subgenre) %>%
  ggplot() +
  geom_bar(aes(reorder(playlist_subgenre, n), n, fill=mode), position = 'stack', stat='identity') +
  coord_flip() +
  facet_wrap(playlist_genre~., scales="free", nrow=3) +
  theme_minimal() +
  theme(legend.position = 'bottom')

spotify_songs %>% 
  summary %>% view()
