library(spotifyr)
library(dplyr)

Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')

access_token <- get_spotify_access_token()

artist_list <- c("alt-j", "arctic monkeys", "lana del rey", "king gizzard & the lizzard wizard")

artist_tracks <- map(artist_list, get_artist_audio_features)

bind_rows(artist_tracks) %>%
	select(track_name, track_number, album_name, album_release_date, album_release_year, danceability, energy, loudness, duration_ms, tempo, valence) %>%
	write.csv(file = "spotify.csv")