#############################
#     Tweet per Line        #
#############################

library(remotes)
library(rtweet)
library(httpuv)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidytext)
library(tidyr)
library(readr)
library(tm)

# version 0.1.0
# 5/12/2020
# Authors: Damon C. Roberts

## Functions:
## Dependencies:    twitteR (for access to the Twitter API)
#                   remotes (for access to github repositories)
#                   rtweets (for access to the Twitter API)
#                   httpuv  (assists with converting JSON files to CSV)
#                   tidytext (useful for managing strings in a tibble)
#                   tidyverse (useful to bring in the many tidyverse packages to make clean data storage and management)
#                   ggplot2 (suggested to help with any graphical representation of data)
#                   dplyr (assists with tibble management/manipulation)
#                   lubridate (assists with conversion from JSON to tibble)
#                   tidyr (assists with clean data storage and management)
#                   readr (assists with reading CSVs)
#                   tm (used in text mining)

## Functions Included:
# (1) tweet_per_line

######################
#-(1) tweet_per_line-#
######################
#' Takes each dataframe for the tweets that you downloaded and removes unimportant information if you are only interested in who posted it and the text of  the tweet
#' Writes a txt file with each tweet per row

tweet_per_line <- function() {
dsenatemdtweets <- select(senatemdtweets, -c(user_id, status_id, created_at, screen_name, source, display_text_width, reply_to_status_id,
                                             reply_to_user_id, reply_to_screen_name, is_quote, is_retweet, favorite_count, retweet_count, quote_count,
                                             reply_count, hashtags, symbols, urls_url, urls_t.co, urls_expanded_url, media_url,
                                             media_t.co, media_expanded_url, media_type, ext_media_url, ext_media_t.co, ext_media_expanded_url,
                                             ext_media_type, mentions_user_id, mentions_screen_name, lang, quoted_status_id, quoted_text, quoted_created_at,
                                             quoted_source, quoted_favorite_count, quoted_retweet_count, quoted_user_id, quoted_screen_name, quoted_name,
                                             quoted_followers_count,quoted_friends_count,quoted_statuses_count,quoted_location,quoted_description,
                                             quoted_verified, retweet_status_id, retweet_text, retweet_created_at, retweet_source, retweet_favorite_count,
                                             retweet_retweet_count, retweet_user_id, retweet_screen_name, retweet_name, retweet_followers_count, retweet_friends_count,
                                             retweet_statuses_count, retweet_location, retweet_description, retweet_verified, place_url, place_name, place_full_name,
                                             place_type, country, country_code, geo_coords, coords_coords,bbox_coords, status_url, name, location,
                                             description, url, protected, followers_count, friends_count, listed_count, statuses_count, favourites_count,
                                             account_created_at, verified, profile_url, profile_expanded_url, account_lang, profile_banner_url, profile_background_url,
                                             profile_image_url, person))
write.table(dsenatemdtweets, file = "data/senatemd.txt", sep = "")
dsenatemrtweets <- select(senatemrtweets, -c(user_id, status_id, created_at, screen_name, source, display_text_width, reply_to_status_id,
                                             reply_to_user_id, reply_to_screen_name, is_quote, is_retweet, favorite_count, retweet_count, quote_count,
                                             reply_count, hashtags, symbols, urls_url, urls_t.co, urls_expanded_url, media_url,
                                             media_t.co, media_expanded_url, media_type, ext_media_url, ext_media_t.co, ext_media_expanded_url,
                                             ext_media_type, mentions_user_id, mentions_screen_name, lang, quoted_status_id, quoted_text, quoted_created_at,
                                             quoted_source, quoted_favorite_count, quoted_retweet_count, quoted_user_id, quoted_screen_name, quoted_name,
                                             quoted_followers_count,quoted_friends_count,quoted_statuses_count,quoted_location,quoted_description,
                                             quoted_verified, retweet_status_id, retweet_text, retweet_created_at, retweet_source, retweet_favorite_count,
                                             retweet_retweet_count, retweet_user_id, retweet_screen_name, retweet_name, retweet_followers_count, retweet_friends_count,
                                             retweet_statuses_count, retweet_location, retweet_description, retweet_verified, place_url, place_name, place_full_name,
                                             place_type, country, country_code, geo_coords, coords_coords,bbox_coords, status_url, name, location,
                                             description, url, protected, followers_count, friends_count, listed_count, statuses_count, favourites_count,
                                             account_created_at, verified, profile_url, profile_expanded_url, account_lang, profile_banner_url, profile_background_url,
                                             profile_image_url, person))
write.table(dsenatemrtweets, file = "data/senatemr.txt", sep = "")

dsenatefdtweets <- select(senatefdtweets, -c(user_id, status_id, created_at, screen_name, source, display_text_width, reply_to_status_id,
                                             reply_to_user_id, reply_to_screen_name, is_quote, is_retweet, favorite_count, retweet_count, quote_count,
                                             reply_count, hashtags, symbols, urls_url, urls_t.co, urls_expanded_url, media_url,
                                             media_t.co, media_expanded_url, media_type, ext_media_url, ext_media_t.co, ext_media_expanded_url,
                                             ext_media_type, mentions_user_id, mentions_screen_name, lang, quoted_status_id, quoted_text, quoted_created_at,
                                             quoted_source, quoted_favorite_count, quoted_retweet_count, quoted_user_id, quoted_screen_name, quoted_name,
                                             quoted_followers_count,quoted_friends_count,quoted_statuses_count,quoted_location,quoted_description,
                                             quoted_verified, retweet_status_id, retweet_text, retweet_created_at, retweet_source, retweet_favorite_count,
                                             retweet_retweet_count, retweet_user_id, retweet_screen_name, retweet_name, retweet_followers_count, retweet_friends_count,
                                             retweet_statuses_count, retweet_location, retweet_description, retweet_verified, place_url, place_name, place_full_name,
                                             place_type, country, country_code, geo_coords, coords_coords,bbox_coords, status_url, name, location,
                                             description, url, protected, followers_count, friends_count, listed_count, statuses_count, favourites_count,
                                             account_created_at, verified, profile_url, profile_expanded_url, account_lang, profile_banner_url, profile_background_url,
                                             profile_image_url, person))
write.table(dsenatefdtweets, file = "data/senatefd.txt", sep = "")

dsenatefrtweets <- select(senatefrtweets, -c(user_id, status_id, created_at, screen_name, source, display_text_width, reply_to_status_id,
                                             reply_to_user_id, reply_to_screen_name, is_quote, is_retweet, favorite_count, retweet_count, quote_count,
                                             reply_count, hashtags, symbols, urls_url, urls_t.co, urls_expanded_url, media_url,
                                             media_t.co, media_expanded_url, media_type, ext_media_url, ext_media_t.co, ext_media_expanded_url,
                                             ext_media_type, mentions_user_id, mentions_screen_name, lang, quoted_status_id, quoted_text, quoted_created_at,
                                             quoted_source, quoted_favorite_count, quoted_retweet_count, quoted_user_id, quoted_screen_name, quoted_name,
                                             quoted_followers_count,quoted_friends_count,quoted_statuses_count,quoted_location,quoted_description,
                                             quoted_verified, retweet_status_id, retweet_text, retweet_created_at, retweet_source, retweet_favorite_count,
                                             retweet_retweet_count, retweet_user_id, retweet_screen_name, retweet_name, retweet_followers_count, retweet_friends_count,
                                             retweet_statuses_count, retweet_location, retweet_description, retweet_verified, place_url, place_name, place_full_name,
                                             place_type, country, country_code, geo_coords, coords_coords,bbox_coords, status_url, name, location,
                                             description, url, protected, followers_count, friends_count, listed_count, statuses_count, favourites_count,
                                             account_created_at, verified, profile_url, profile_expanded_url, account_lang, profile_banner_url, profile_background_url,
                                             profile_image_url, person))
write.table(dsenatefrtweets, file = "data/senatefr.txt", sep = "")

dhormdtweets <- select(hormdtweets, -c(user_id, status_id, created_at, screen_name, source, display_text_width, reply_to_status_id,
                                       reply_to_user_id, reply_to_screen_name, is_quote, is_retweet, favorite_count, retweet_count, quote_count,
                                       reply_count, hashtags, symbols, urls_url, urls_t.co, urls_expanded_url, media_url,
                                       media_t.co, media_expanded_url, media_type, ext_media_url, ext_media_t.co, ext_media_expanded_url,
                                       ext_media_type, mentions_user_id, mentions_screen_name, lang, quoted_status_id, quoted_text, quoted_created_at,
                                       quoted_source, quoted_favorite_count, quoted_retweet_count, quoted_user_id, quoted_screen_name, quoted_name,
                                       quoted_followers_count,quoted_friends_count,quoted_statuses_count,quoted_location,quoted_description,
                                       quoted_verified, retweet_status_id, retweet_text, retweet_created_at, retweet_source, retweet_favorite_count,
                                       retweet_retweet_count, retweet_user_id, retweet_screen_name, retweet_name, retweet_followers_count, retweet_friends_count,
                                       retweet_statuses_count, retweet_location, retweet_description, retweet_verified, place_url, place_name, place_full_name,
                                       place_type, country, country_code, geo_coords, coords_coords,bbox_coords, status_url, name, location,
                                       description, url, protected, followers_count, friends_count, listed_count, statuses_count, favourites_count,
                                       account_created_at, verified, profile_url, profile_expanded_url, account_lang, profile_banner_url, profile_background_url,
                                       profile_image_url, person))
write.table(dhormdtweets, file = "data/hormd.txt", sep = "")

dhormrtweets <- select(hormrtweets, -c(user_id, status_id, created_at, screen_name, source, display_text_width, reply_to_status_id,
                                       reply_to_user_id, reply_to_screen_name, is_quote, is_retweet, favorite_count, retweet_count, quote_count,
                                       reply_count, hashtags, symbols, urls_url, urls_t.co, urls_expanded_url, media_url,
                                       media_t.co, media_expanded_url, media_type, ext_media_url, ext_media_t.co, ext_media_expanded_url,
                                       ext_media_type, mentions_user_id, mentions_screen_name, lang, quoted_status_id, quoted_text, quoted_created_at,
                                       quoted_source, quoted_favorite_count, quoted_retweet_count, quoted_user_id, quoted_screen_name, quoted_name,
                                       quoted_followers_count,quoted_friends_count,quoted_statuses_count,quoted_location,quoted_description,
                                       quoted_verified, retweet_status_id, retweet_text, retweet_created_at, retweet_source, retweet_favorite_count,
                                       retweet_retweet_count, retweet_user_id, retweet_screen_name, retweet_name, retweet_followers_count, retweet_friends_count,
                                       retweet_statuses_count, retweet_location, retweet_description, retweet_verified, place_url, place_name, place_full_name,
                                       place_type, country, country_code, geo_coords, coords_coords,bbox_coords, status_url, name, location,
                                       description, url, protected, followers_count, friends_count, listed_count, statuses_count, favourites_count,
                                       account_created_at, verified, profile_url, profile_expanded_url, account_lang, profile_banner_url, profile_background_url,
                                       profile_image_url, person))
write.table(dhormrtweets, file = "data/hormr.txt", sep = "")

dhorfdtweets <- select(horfdtweets, -c(user_id, status_id, created_at, screen_name, source, display_text_width, reply_to_status_id,
                                       reply_to_user_id, reply_to_screen_name, is_quote, is_retweet, favorite_count, retweet_count, quote_count,
                                       reply_count, hashtags, symbols, urls_url, urls_t.co, urls_expanded_url, media_url,
                                       media_t.co, media_expanded_url, media_type, ext_media_url, ext_media_t.co, ext_media_expanded_url,
                                       ext_media_type, mentions_user_id, mentions_screen_name, lang, quoted_status_id, quoted_text, quoted_created_at,
                                       quoted_source, quoted_favorite_count, quoted_retweet_count, quoted_user_id, quoted_screen_name, quoted_name,
                                       quoted_followers_count,quoted_friends_count,quoted_statuses_count,quoted_location,quoted_description,
                                       quoted_verified, retweet_status_id, retweet_text, retweet_created_at, retweet_source, retweet_favorite_count,
                                       retweet_retweet_count, retweet_user_id, retweet_screen_name, retweet_name, retweet_followers_count, retweet_friends_count,
                                       retweet_statuses_count, retweet_location, retweet_description, retweet_verified, place_url, place_name, place_full_name,
                                       place_type, country, country_code, geo_coords, coords_coords,bbox_coords, status_url, name, location,
                                       description, url, protected, followers_count, friends_count, listed_count, statuses_count, favourites_count,
                                       account_created_at, verified, profile_url, profile_expanded_url, account_lang, profile_banner_url, profile_background_url,
                                       profile_image_url, person))
write.table(dhorfdtweets, file = "data/horfd.txt", sep = "")

dhorfrtweets <- select(horfrtweets, -c(user_id, status_id, created_at, screen_name, source, display_text_width, reply_to_status_id,
                                       reply_to_user_id, reply_to_screen_name, is_quote, is_retweet, favorite_count, retweet_count, quote_count,
                                       reply_count, hashtags, symbols, urls_url, urls_t.co, urls_expanded_url, media_url,
                                       media_t.co, media_expanded_url, media_type, ext_media_url, ext_media_t.co, ext_media_expanded_url,
                                       ext_media_type, mentions_user_id, mentions_screen_name, lang, quoted_status_id, quoted_text, quoted_created_at,
                                       quoted_source, quoted_favorite_count, quoted_retweet_count, quoted_user_id, quoted_screen_name, quoted_name,
                                       quoted_followers_count,quoted_friends_count,quoted_statuses_count,quoted_location,quoted_description,
                                       quoted_verified, retweet_status_id, retweet_text, retweet_created_at, retweet_source, retweet_favorite_count,
                                       retweet_retweet_count, retweet_user_id, retweet_screen_name, retweet_name, retweet_followers_count, retweet_friends_count,
                                       retweet_statuses_count, retweet_location, retweet_description, retweet_verified, place_url, place_name, place_full_name,
                                       place_type, country, country_code, geo_coords, coords_coords,bbox_coords, status_url, name, location,
                                       description, url, protected, followers_count, friends_count, listed_count, statuses_count, favourites_count,
                                       account_created_at, verified, profile_url, profile_expanded_url, account_lang, profile_banner_url, profile_background_url,
                                       profile_image_url, person))
write.table(dhorfrtweets, file = "data/horfr.txt", sep = "")
}
