library(spotifyr)
library(dplyr)

Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')

access_token <- get_spotify_access_token()

arctic_monkeys <- get_artist_audio_features('arctic monkeys')

arctic_monkeys %>%
	select(track_name, track_number, album_name, album_release_date, danceability, energy, loudness, speechiness, duration_ms) %>%
	write.csv(file = "arctic_monkeys.csv")