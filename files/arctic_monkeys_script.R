library(spotifyr)
library(dplyr)

Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')

access_token <- get_spotify_access_token()

AM <- get_artist_audio_features('arctic monkeys')

AM %>%
	select(track_name, track_number, album_name, album_release_date, album_release_year, danceability, energy, loudness, speechiness, duration_ms, tempo, valence) %>%
	write.csv(file = "arctic_monkeys.csv")