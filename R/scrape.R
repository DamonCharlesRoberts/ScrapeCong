##########################
# ScrapeCongress          #
##########################

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
# (1) authenticate()
# (2) senmaleD()
# (3) senfemD()
# (4) senfemR()
# (5) senmaleR()
# (6) horfemR()
# (7) horfemD()
# (8) hormaleR()
# (9) hormaleD()

library(rtweet)
library(tidytext)
library(dplyr)
library(tidytext)
library(readr)
library(tm)
library(utils)


#############################
#----(1) Keys-------#
#############################
#' RStudio Console prompts the user to enter their twitter credentials
#' Users should keep these keys handy when starting a new session of R and should run this function before the others.
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @export
keys <- function() {
api_key <- readline(prompt = "Enter Twitter API Key: ")
api_secret <- readline(prompt = "Enter Twitter API Secret: ")
access_token <- readline(prompt = "Enter Twitter Access Token: ")
access_token_secret <- readline(prompt = "Enter Twitter Token Secret: ")
app_name <- readline(prompt = "Enter Twitter APP Name (Exactly as it appears on Twitter): ")
token <- rtweet::create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_token_secret
)
print(keys())
}
#############################
#--------(1) senmaleD-------#
#############################
#' Grabs the 50 most recent tweets from Male Democrats in the Senate
#' Saves those tweets in individual csv (for each MC) and uses the UTF-8 file encoding for the CSV
#' Creates one dataframe to combine all of the tweets from each MC
#' With the one large dataframe, it creates a CSV to hold the 50 most recent tweets for each Male Democrat in the Senate
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @export

senmaleD <- function() {
  dougjones <- rtweet::get_timeline('@SenDougJones', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(dougjones, "data/dougjones.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
dougjonestweets <- readr::read_csv("data/dougjones.csv")
bennet <- rtweet::get_timeline('@SenatorBennet', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(bennet, "data/bennet.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
bennettweets <- readr::read_csv("data/bennet.csv")
blumenthal <- rtweet::get_timeline('@SenBlumenthal', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(blumenthal, "data/blumenthal.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
blumenthaltweets <- readr::read_csv("data/blumenthal.csv")
murphey <- rtweet::get_timeline('@SenMurphyOffice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(murphey, "data/murphy.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
murpheytweets <- readr::read_csv("data/murphy.csv")
chriscoons <- rtweet::get_timeline('@ChrisCoons', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(chriscoons, "data/chriscoons.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
chriscoonstweets <- readr::read_csv("data/chriscoons.csv")
cooper <- rtweet::get_timeline('@SenatorCarper', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(cooper, "data/cooper.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
coopertweets <- readr::read_csv("data/cooper.csv")
brianschatz <- rtweet::get_timeline('@SenBrianSchatz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(brianschatz, "data/brianschatz.csv")
brianschatztweets <- readr::read_csv("data/brianschatz.csv")
durbin <- rtweet::get_timeline('@SenatorDurbin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(durbin, "data/durbin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
durbintweets <- readr::read_csv("data/durbin.csv")
markey <- rtweet::get_timeline('@SenMarkey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(markey, "data/markey.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
markeytweets <- readr::read_csv("data/markey.csv")
cardin <- rtweet::get_timeline('@SenatorCardin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(cardin, "data/cardin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
cardintweets <- readr::read_csv("data/cardin.csv")
chrisvanhollen <- rtweet::get_timeline('@ChrisVanHollen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(chrisvanhollen, "data/chrisvanhollen.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
chrisvanhollentweets <- readr::read_csv("data/chrisvanhollen.csv")
garypeters <- rtweet::get_timeline('@SenGaryPeters', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(garypeters, "data/garypeters.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
garypeterstweets <- readr::read_csv("data/garypeters.csv")
booker <- rtweet::get_timeline('@SenBooker', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(booker, "data/booker.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
bookertweets <- readr::read_csv("data/booker.csv")
menedez <- rtweet::get_timeline('@SenatorMenendez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(menedez, "data/menedez.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
menedeztweets <- readr::read_csv("data/menedez.csv")
martinheinrich <- rtweet::get_timeline('@MartinHeinrich', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(martinheinrich, "data/martinheinrich.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
martinheinrichtweets <- readr::read_csv("data/martinheinrich.csv")
tomudall <- rtweet::get_timeline('@SenatorTomUdall', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(tomudall, "data/tomudall.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
tomudalltweets <- readr::read_csv("data/tomudall.csv")
schumer <- rtweet::get_timeline('@SenSchumer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(schumer, "data/schumer.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
schumertweets <- readr::read_csv("data/schumer.csv")
sherrodbrown <- rtweet::get_timeline('@SenSherrodBrown', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(sherrodbrown, "data/sherrodbrown.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
sherrodbrowntweets <- readr::read_csv("data/sherrodbrown.csv")
jeffmerkley <- rtweet::get_timeline('@SenJeffMerkley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(jeffmerkley, "data/jeffmerkley.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
jeffmerkleytweets <- readr::read_csv("data/jeffmerkley.csv")
ronwyden <- rtweet::get_timeline('@RonWyden', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(ronwyden, "data/ronwyden.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
ronwydentweets <- readr::read_csv("data/ronwyden.csv")
bobcasey <- rtweet::get_timeline('@SenBobCasey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(bobcasey, "data/bobcasey.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
bobcaseytweets <- readr::read_csv("data/bobcasey.csv")
toomey <- rtweet::get_timeline('@SenToomey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(toomey, "data/toomey.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
toomeytweets <- readr::read_csv("data/toomey.csv")
jackreed <- rtweet::get_timeline('@SenJackReed', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(jackreed, "data/jackreed.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
jackreedtweets <- readr::read_csv("data/jackreed.csv")
whitehouse <- rtweet::get_timeline('@SenWhitehouse', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(whitehouse, "data/whitehouse.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
whitehousetweets <- readr::read_csv("data/whitehouse.csv")
leahy <- rtweet::get_timeline('@SenatorLeahy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(leahy, "data/leahy.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
leahytweets <- readr::read_csv("data/leahy.csv")
sanders <- rtweet::get_timeline('@SenSanders', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(sanders, "data/sanders.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
sanderstweets <- readr::read_csv("data/sanders.csv")
warner <- rtweet::get_timeline('@MarkWarner', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(warner, "data/warner.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
warnertweets <- readr::read_csv("data/warner.csv")
joemanchin <- rtweet::get_timeline('@Sen_JoeManchin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(joemanchin, "data/joemanchin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
joemanchintweets <- readr::read_csv("data/joemanchin.csv")
senatemdtweets <- dplyr::bind_rows(dougjonestweets %>%
                              dplyr::mutate(person = "Doug Jones"),
                            bennettweets %>%
                              dplyr::mutate(person = "Bennet"),
                            blumenthaltweets %>%
                              dplyr::mutate(person = "Blumenthal"),
                            murpheytweets %>%
                              dplyr::mutate(person = "Murphey"),
                            chriscoonstweets %>%
                              dplyr::mutate(person = "Chris Coons"),
                            coopertweets %>%
                              dplyr::mutate(person = "Cooper"),
                            brianschatztweets %>%
                              dplyr::mutate(person = "Brian Schatz"),
                            durbintweets %>%
                              dplyr::mutate(person = "Durbin"),
                            markeytweets %>%
                              dplyr::mutate(person = "Markey"),
                            cardintweets %>%
                              dplyr::mutate(person = "Cardin"),
                            chrisvanhollentweets %>%
                              dplyr::mutate(person = "Chris Vanhollen"),
                            garypeterstweets %>%
                              dplyr::mutate(person = "Gary Peters"),
                            bookertweets %>%
                              dplyr::mutate(person = "Booker"),
                            menedeztweets %>%
                              dplyr::mutate(person = "Menedez"),
                            martinheinrichtweets %>%
                              dplyr::mutate(person = "Martin Heinrich"),
                            tomudalltweets %>%
                              dplyr::mutate(person = "Tom Udall"),
                            schumertweets %>%
                              dplyr::mutate(person = "Schumer"),
                            sherrodbrowntweets %>%
                              dplyr::mutate(person = "Sherrod Brown"),
                            jeffmerkleytweets %>%
                              dplyr::mutate(person = "Jeff Merkley"),
                            ronwydentweets %>%
                              dplyr::mutate(person = "Ron Wyden"),
                            bobcaseytweets %>%
                              dplyr::mutate(person = "Bob Casey"),
                            toomeytweets %>%
                              dplyr::mutate(person = "Toomey"),
                            jackreedtweets %>%
                              dplyr::mutate(person = "Jack Reed"),
                            whitehousetweets %>%
                              dplyr::mutate(person = "Whitehouse"),
                            leahytweets %>%
                              dplyr::mutate(person = "Leahy"),
                            sanderstweets %>%
                              dplyr::mutate(person = "Sanders"),
                            warnertweets %>%
                              dplyr::mutate(person = "Warner"),
                            joemanchintweets %>%
                              dplyr::mutate(person = "Joe Manchin"))

save_as_csv(senatemdtweets, "data/senatemdtweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
senatemdtweets <- readr::read_csv("data/senatemdtweets.csv")

if(nrow(senatemdtweets)>0) {
  message("Check your Data Folder. Function ran successfully")
}
}


#############################
#--------(2) senfemD-------#
#############################
#' Grabs the 50 most recent tweets from Female Democrats in the Senate
#' Saves those tweets in individual csv (for each MC) and uses the UTF-8 file encoding for the CSV
#' Creates one dataframe to combine all of the tweets from each MC
#' With the one large dataframe, it creates a CSV to hold the 50 most recent tweets for each Female Democrat in the Senate
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @export

senfemD <- function() {
  sinema <- rtweet::get_timeline('@SenatorSinema', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(sinema,"data/sinema.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
sinematweets <- readr::read_csv("data/sinema.csv")
feinstein <- rtweet::get_timeline('@SenFeinstein', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(feinstein,"data/feinstein.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
feinsteintweets <- readr::read_csv("data/feinstein.csv")
kamalaharris <- rtweet::get_timeline('@SenKamalaHarris', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(kamalaharris,"data/kamalaharris.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
kamalaharristweets <- readr::read_csv("data/kamalaharris.csv")
maziehirono <- rtweet::get_timeline('@maziehirono', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(maziehirono,"data/maziehirono.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
maziehironotweets <- readr::read_csv("data/maziehirono.csv")
duckworth <- rtweet::get_timeline('@SenDuckworth', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(duckworth,"data/duckworth.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
duckworthtweets <- readr::read_csv("data/duckworth.csv")
warren <- rtweet::get_timeline('@SenWarren', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(warren,"data/warren.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
warrentweets <- readr::read_csv("data/warren.csv")
stabenow <- rtweet::get_timeline('@SenStabenow', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(stabenow,"data/stabenow.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
stabenowtweets <- readr::read_csv("data/stabenow.csv")
amyklobuchar <- rtweet::get_timeline('@SenAmyKlobuchar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(amyklobuchar,"data/amyklobuchar.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
amyklobuchartweets <- readr::read_csv("data/amyklobuchar.csv")
tinasmith <- rtweet::get_timeline('@SenTinaSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(tinasmith,"data/tinasmith.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
tinasmithtweets <- readr::read_csv("data/tinasmith.csv")
hassan <- rtweet::get_timeline('@SenatorHassan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(hassan,"data/hassan.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
hassantweets <- readr::read_csv("data/hassan.csv")
shaheen <- rtweet::get_timeline('@SenatorShaheen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(shaheen,"data/shaheen.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
shaheentweets <- readr::read_csv("data/shaheen.csv")
cortezmasto <- rtweet::get_timeline('@SenCortezMasto', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(cortezmasto,"data/cortezmasto.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
cortezmastotweets <- readr::read_csv("data/cortezmasto.csv")
jackyrosen <- rtweet::get_timeline(' @SenJackyRosen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(jackyrosen,"data/jackyrosen.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
jackyrosentweets <- readr::read_csv("data/jackyrosen.csv")
gillibrand <- rtweet::get_timeline('@gillibrandny', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(gillibrand,"data/gillibrand.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
gillibrandtweets <- readr::read_csv("data/gillibrand.csv")
cantwell <- rtweet::get_timeline('@SenatorCantwell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(cantwell,"data/cantewell.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
cantwelltweets <- readr::read_csv("data/cantewell.csv")
pattymurray <- rtweet::get_timeline('@PattyMurray', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(pattymurray,"data/pattymurray.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
pattymurraytweets <- readr::read_csv("data/pattymurray.csv")
baldwin <- rtweet::get_timeline('@SenatorBaldwin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(baldwin,"data/baldwin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
baldwintweets <- readr::read_csv("data/baldwin.csv")
senatefdtweets <- dplyr::bind_rows(feinsteintweets %>%
                                dplyr::mutate(person = "Feinstein"),
                              kamalaharristweets %>%
                                dplyr::mutate(person = "Kamala Harris"),
                              maziehironotweets %>%
                                dplyr::mutate(person = "Mazie Hirono"),
                              duckworthtweets %>%
                                dplyr::mutate(person = "Duckworth"),
                              warrentweets %>%
                                dplyr::mutate(person = "Warren"),
                              stabenowtweets %>%
                                dplyr::mutate(person = "Stabenow"),
                              amyklobuchartweets %>%
                                dplyr::mutate(person = "Amy Klobuchar"),
                              tinasmithtweets %>%
                                dplyr::mutate(person = "Tina Smith"),
                              hassantweets %>%
                                dplyr::mutate(person = "Hassan"),
                              shaheentweets %>%
                                dplyr::mutate(person = "Shaheen"),
                              cortezmastotweets %>%
                                dplyr::mutate(person = "Cortez-Masto"),
                              jackyrosentweets %>%
                                dplyr::mutate(person = "Jacky Rosen"),
                              gillibrandtweets %>%
                                dplyr::mutate(person = "Gillibrand"),
                              cantwelltweets %>%
                                dplyr::mutate(person = "Cantwell"),
                              pattymurraytweets %>%
                                dplyr::mutate(person = "Patty Murray"),
                              baldwintweets %>%
                                dplyr::mutate(person = "Baldwin"))

save_as_csv(senatefdtweets, "data/senatefdtweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
senatefdtweets <- readr::read_csv("data/senatefdtweets.csv")
if(nrow(senatefdtweets)>0) {
  message("Check your Data Folder. Function ran successfully")
}
}

#############################
#--------(3) senfemR-------#
#############################
#' Grabs the 50 most recent tweets from Female Republicans in the Senate
#' Saves those tweets in individual csv (for each MC) and uses the UTF-8 file encoding for the CSV
#' Creates one dataframe to combine all of the tweets from each MC
#' With the one large dataframe, it creates a CSV to hold the 50 most recent tweets for each Female Republican in the Senate
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @export

senfemR <- function() {
  lisamurkowski<- rtweet::get_timeline('@lisamurkowski', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(lisamurkowski,"data/lisamurkowski.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
lisamurkowskitweets <- readr::read_csv("data/lisamurkowski.csv")
mcsally <- rtweet::get_timeline('@SenMcSallyAZ', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(mcsally,"data/mcsally.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
mcsallytweets <- readr::read_csv("data/mcsally.csv")
joniernst <- rtweet::get_timeline('@SenJoniErnst', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(joniernst,"data/joniernst.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
joniernsttweets <- readr::read_csv("data/joniernst.csv")
collins <- rtweet::get_timeline('@SenatorCollins', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(collins,"data/collins.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
collinstweets <- readr::read_csv("data/collins.csv")
hydesmith <- rtweet::get_timeline('@SenHydeSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(hydesmith,"data/hydesmith.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
hydesmithtweets <- readr::read_csv("data/hydesmith.csv")
fischer <- rtweet::get_timeline('@SenatorFischer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(fischer,"data/fischer.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
fischertweets <- readr::read_csv("data/fischer.csv")
marshablackburn <- rtweet::get_timeline('@MarshaBlackburn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(marshablackburn,"data/marshablackburn.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
marshablackburntweets <- readr::read_csv("data/marshablackburn.csv")
capito <- rtweet::get_timeline('@SenCapito', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(capito,"data/capito.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
capitotweets <- readr::read_csv("data/capito.csv")

senatefrtweets <- dplyr::bind_rows(lisamurkowskitweets %>%
                                dplyr::mutate(person = "Lisa Murkowski"),
                              mcsallytweets %>%
                                dplyr::mutate(person = "Martha McSally"),
                              joniernsttweets %>%
                                dplyr::mutate(person = "Joni Ernst"),
                              collinstweets %>%
                                dplyr::mutate(person = "Susan Collinis"),
                              hydesmithtweets %>%
                                dplyr::mutate(person = "Cindy Hyde-Smith"),
                              fischertweets %>%
                                dplyr::mutate(person = "Deb Fischer"),
                              marshablackburntweets %>%
                                dplyr::mutate(person = "Marsha Blackburn"),
                              capitotweets %>%
                                dplyr::mutate(person = "Shelley Moore Capito"))

save_as_csv(senatefrtweets, "data/senatefrtweets.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
senatefrtweets <- readr::read_csv("data/senatefrtweets.csv")
if(nrow(senatefrtweets)>0) {
  message("Check your Data Folder. Function ran successfully")
}
}


#############################
#--------(4) senmaleR-------#
#############################
#' Grabs the 50 most recent tweets from Male Republicans in the Senate
#' Saves those tweets in individual csv (for each MC) and uses the UTF-8 file encoding for the CSV
#' Creates one dataframe to combine all of the tweets from each MC
#' With the one large dataframe, it creates a CSV to hold the 50 most recent tweets for each Male Republican in the Senate
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @export

senmaleR <- function() {
  donsullivan <- rtweet::get_timeline('@SenDanSullivan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
shelby <- rtweet::get_timeline('@SenShelby', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johnboozman <- rtweet::get_timeline('@JohnBoozman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
tomcotton <- rtweet::get_timeline('@SenTomCotton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
corygardner <- rtweet::get_timeline('@SenCoryGardner', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
rubio <- rtweet::get_timeline('@SenRubioPress', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
rickscott<- rtweet::get_timeline('@SenRickScott', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
isakson <- rtweet::get_timeline('@SenatorIsakson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
davidperdue <- rtweet::get_timeline('@sendavidperdue', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
chuckgrassley <- rtweet::get_timeline('@ChuckGrassley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
mikecrapo <- rtweet::get_timeline('@MikeCrapo', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
risch <- rtweet::get_timeline('@SenatorRisch', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
braun<- rtweet::get_timeline('@SenatorBraun', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
toddyoung<- rtweet::get_timeline('@SenToddYoung', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
jerrymoran<- rtweet::get_timeline('@JerryMoran', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
patroberts<- rtweet::get_timeline('@SenPatRoberts', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
mitchmconnell<- rtweet::get_timeline('@SenateMajLdr', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
randpaul<- rtweet::get_timeline('@RandPaul', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
billcassidy<- rtweet::get_timeline('@SenBillCassidy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johnkennedy<- rtweet::get_timeline('@SenJohnKennedy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
royblunt<- rtweet::get_timeline('@RoyBlunt', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
hawley<- rtweet::get_timeline('@SenHawleyPress', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
wicker<- rtweet::get_timeline('@SenatorWicker', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
stevedaines<- rtweet::get_timeline('@SteveDaines', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
burr<- rtweet::get_timeline('@SenatorBurr', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
thomtillis<- rtweet::get_timeline('@SenThomTillis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
kevincramer<- rtweet::get_timeline('@@SenKevinCramer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johnhoeven<- rtweet::get_timeline('@SenJohnHoeven', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
sasse<- rtweet::get_timeline('@SenSasse', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
robportman<- rtweet::get_timeline('@senrobportman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
jiminhofe<- rtweet::get_timeline('@JimInhofe', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
lankford<- rtweet::get_timeline('@SenatorLankford', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
lindseygraham<- rtweet::get_timeline('@LindseyGrahamSC', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
timscott<- rtweet::get_timeline('@SenatorTimScott', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
rounds<- rtweet::get_timeline('@SenatorRounds', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johnthune<- rtweet::get_timeline('@SenJohnThune', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
alexander<- rtweet::get_timeline('@SenAlexander', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johncornyn<- rtweet::get_timeline('@JohnCornyn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
tedcruz<- rtweet::get_timeline('@SenTedCruz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
mikelee<- rtweet::get_timeline('@SenMikeLee', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
romney<- rtweet::get_timeline('@SenatorRomney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
ronjohnson<- rtweet::get_timeline('@SenRonJohnson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johnbarrasso<- rtweet::get_timeline('@SenJohnBarrasso', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
enzi<- rtweet::get_timeline('@SenatorEnzi', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)


save_as_csv(donsullivan,"data/donsullivan.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(shelby,"data/shelby.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(johnboozman,"data/johnboozman.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(tomcotton,"data/tomcotton.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(corygardner,"data/corygardner.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(rubio,"data/rubio.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(rickscott,"data/rickscott.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(isakson,"data/isakson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(davidperdue,"data/davidperdue.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(chuckgrassley,"data/chuckgrassley.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(mikecrapo,"data/mikecrapo.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(risch,"data/risch.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(braun,"data/braun.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(toddyoung,"data/toddyoung.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(jerrymoran,"data/jerrymoran.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(patroberts,"data/patroberts.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(mitchmconnell,"data/mitchmconnell.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(randpaul,"data/randpaul.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(billcassidy,"data/billcassidy.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(johnkennedy,"data/johnkennedy.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(royblunt,"data/royblunt.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(hawley,"data/hawley.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(wicker,"data/wicker.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(stevedaines,"data/stevedaines.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(burr,"data/burr.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(thomtillis,"data/thomtillis.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(kevincramer,"data/kevincramer.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(johnhoeven,"data/johnhoeven.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(sasse,"data/sasse.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(robportman,"data/robportman.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(jiminhofe,"data/jiminhofe.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(lankford,"data/lankford.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(lindseygraham,"data/lindseygraham.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(timscott,"data/timscott.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(rounds,"data/rounds.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(johnthune,"data/johnthune.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(alexander,"data/alexander.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(johncornyn,"data/johncornyn.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(tedcruz,"data/tedcruz.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(mikelee,"data/mikelee.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(romney,"data/romney.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(ronjohnson,"data/ronjohnson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(johnbarrasso,"data/johnbarrasso.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(enzi,"data/enzi.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")

donsullivantweets <- readr::read_csv("data/donsullivan.csv")
shelbytweets <- readr::read_csv("data/shelby.csv")
johnboozmantweets <- readr::read_csv("data/johnboozman.csv")
tomcottontweets <- readr::read_csv("data/tomcotton.csv")
corygardnertweets <- readr::read_csv("data/corygardner.csv")
rubiotweets <- readr::read_csv("data/rubio.csv")
rickscotttweets <- readr::read_csv("data/rickscott.csv")
isaksontweets <- readr::read_csv("data/isakson.csv")
davidperduetweets <- readr::read_csv("data/davidperdue.csv")
chuckgrassleytweets <- readr::read_csv("data/chuckgrassley.csv")
mikecrapotweets <- readr::read_csv("data/mikecrapo.csv")
rischtweets <- readr::read_csv("data/risch.csv")
brauntweets <- readr::read_csv("data/braun.csv")
toddyoungtweets <- readr::read_csv("data/toddyoung.csv")
jerrymorantweets <- readr::read_csv("data/jerrymoran.csv")
patrobertstweets <- readr::read_csv("data/patroberts.csv")
mitchmconnelltweets <- readr::read_csv("data/mitchmconnell.csv")
randpaultweets <- readr::read_csv("data/randpaul.csv")
billcassidytweets <- readr::read_csv("data/billcassidy.csv")
johnkennedytweets <- readr::read_csv("data/johnkennedy.csv")
royblunttweets <- readr::read_csv("data/royblunt.csv")
hawleytweets <- readr::read_csv("data/hawley.csv")
wickertweets <- readr::read_csv("data/wicker.csv")
stevedainestweets <- readr::read_csv("data/stevedaines.csv")
burrtweets <- readr::read_csv("data/burr.csv")
thomtillistweets <- readr::read_csv("data/thomtillis.csv")
kevincramertweets <- readr::read_csv("data/kevincramer.csv")
johnhoeventweets <- readr::read_csv("data/johnhoeven.csv")
sassetweets <- readr::read_csv("data/sasse.csv")
robportmantweets <- readr::read_csv("data/robportman.csv")
jiminhofetweets <- readr::read_csv("data/jiminhofe.csv")
lankfordtweets <- readr::read_csv("data/lankford.csv")
lindseygrahamtweets <- readr::read_csv("data/lindseygraham.csv")
timscotttweets <- readr::read_csv("data/timscott.csv")
roundstweets <- readr::read_csv("data/rounds.csv")
johnthunetweets <- readr::read_csv("data/johnthune.csv")
alexandertweets <- readr::read_csv("data/alexander.csv")
johncornyntweets <- readr::read_csv("data/johncornyn.csv")
tedcruztweets <- readr::read_csv("data/tedcruz.csv")
mikeleetweets <- readr::read_csv("data/mikelee.csv")
romneytweets <- readr::read_csv("data/romney.csv")
ronjohnsontweets <- readr::read_csv("data/ronjohnson.csv")
johnbarrassotweets <- readr::read_csv("data/johnbarrasso.csv")
enzitweets <- readr::read_csv("data/enzi.csv")

senatemrtweets <- dplyr::bind_rows(donsullivantweets %>%
                              dplyr::mutate(person = "Mike Lee"),
                            shelbytweets %>%
                              dplyr::mutate(person = "Shelby"),
                            johnboozmantweets %>%
                              dplyr::mutate(person = "John Boozman"),
                            tomcottontweets %>%
                              dplyr::mutate(person = "Tom Cotton"),
                            corygardnertweets %>%
                              dplyr::mutate(person = "Cory Gardner"),
                            rubiotweets %>%
                              dplyr::mutate(person = "Marco Rubio"),
                            rickscotttweets %>%
                              dplyr::mutate(person = "Rick Scott"),
                            isaksontweets %>%
                              dplyr::mutate(person = "Isakson"),
                            davidperduetweets %>%
                              dplyr::mutate(person = "David Perdue"),
                            chuckgrassleytweets %>%
                              dplyr::mutate(person = "Chuck ChuckGrassley"),
                            mikecrapotweets %>%
                              dplyr::mutate(person = "Mike Crapo"),
                            rischtweets %>%
                              dplyr::mutate(person = "Tim Risch"),
                            brauntweets %>%
                              dplyr::mutate(person = "Braun"),
                            toddyoungtweets %>%
                              dplyr::mutate(person = "Todd Young"),
                            jerrymorantweets %>%
                              dplyr::mutate(person = "Jerry Moran"),
                            patrobertstweets %>%
                              dplyr::mutate(person = "Pat Roberts"),
                            mitchmconnelltweets %>%
                              dplyr::mutate(person = "Mitch McConnell"),
                            randpaultweets %>%
                              dplyr::mutate(person = "Rand Paul"),
                            billcassidytweets %>%
                              dplyr::mutate(person = "Bill Cassidy"),
                            johnkennedytweets %>%
                              dplyr::mutate(person = "John Kennedy"),
                            royblunttweets %>%
                              dplyr::mutate(person = "Roy Blunt"),
                            hawleytweets %>%
                              dplyr::mutate(person = "Hawley"),
                            wickertweets %>%
                              dplyr::mutate(person = "Wicker"),
                            stevedainestweets %>%
                              dplyr::mutate(person = "Steven Daines"),
                            burrtweets %>%
                              dplyr::mutate(person = "Burr"),
                            thomtillistweets %>%
                              dplyr::mutate(person = "Thom Tillis"),
                            kevincramertweets %>%
                              dplyr::mutate(person = "Kevin Cramer"),
                            johnhoeventweets %>%
                              dplyr::mutate(person = "John Hoeven"),
                            sassetweets %>%
                              dplyr::mutate(person = "Sasse"),
                            robportmantweets %>%
                              dplyr::mutate(person = "Rob Portman"),
                            jiminhofetweets %>%
                              dplyr::mutate(person = "Jim Inhofe"),
                            lankfordtweets %>%
                              dplyr::mutate(person = "Lankford"),
                            lindseygrahamtweets %>%
                              dplyr::mutate(person = "Lindsey Graham"),
                            timscotttweets %>%
                              dplyr::mutate(person = "Tim Scott"),
                            roundstweets %>%
                              dplyr::mutate(person = "Rounds"),
                            johnthunetweets %>%
                              dplyr::mutate(person = "John Thune"),
                            alexandertweets %>%
                              dplyr::mutate(person = "Alexander"),
                            johncornyntweets %>%
                              dplyr::mutate(person = "John Cornyn"),
                            tedcruztweets %>%
                              dplyr::mutate(person = "Ted Cruz"),
                            mikeleetweets %>%
                              dplyr::mutate(person = "Mike Lee"),
                            romneytweets %>%
                              dplyr::mutate(person = "Mitt Romney"),
                            ronjohnsontweets %>%
                              dplyr::mutate(person = "Ron Johnson"),
                            johnbarrassotweets %>%
                              dplyr::mutate(person = "John Barrasso"),
                            enzitweets %>%
                              dplyr::mutate(person = "Enzi"))

save_as_csv(senatemrtweets, "data/senatemrtweets.csv",prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
senatemrtweets <- readr::read_csv("data/senatemrtweets.csv")
if(nrow(senatemrtweets)>0) {
  message("Check your Data Folder. Function ran successfully")
}
}


#############################
#--------(5) horfemR-------#
#############################
#' Grabs the 50 most recent tweets from Female Republicans in the House of Representatives
#' Saves those tweets in individual csv (for each MC) and uses the UTF-8 file encoding for the CSV
#' Creates one dataframe to combine all of the tweets from each MC
#' With the one large dataframe, it creates a CSV to hold the 50 most recent tweets for each Female Republican in the House of Representatives
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @export

horfemR <- function() {
  martharoby <- rtweet::get_timeline('@RepMarthaRoby', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  walorski <- rtweet::get_timeline('@RepWalorski', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  susanbrooks <- rtweet::get_timeline('@SusanWBrooks', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  annwagner <- rtweet::get_timeline('@RepAnnWagner', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hartzler <- rtweet::get_timeline('@RepHartzler', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  virginiafoxx <- rtweet::get_timeline('@virginiafoxx', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  armstrong <- rtweet::get_timeline('@RepArmstrongND', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stefanik <- rtweet::get_timeline('@RepStefanik', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cathymcmorris <- rtweet::get_timeline('@cathymcmorris', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lizcheney<- rtweet::get_timeline('@RepLizCheney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)


  save_as_csv(martharoby,"data/martharoby.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(walorski,"data/walorski.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(susanbrooks,"data/susanbrooks.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(annwagner,"data/annwagner.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(hartzler,"data/hartzler.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(virginiafoxx,"data/virginiafoxx.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(armstrong,"data/armstrong.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(stefanik,"data/stefanik.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(cathymcmorris,"data/cathymcmorris.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(lizcheney,"data/lizcheney.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")

  martharobytweets <- readr::read_csv("data/martharoby.csv")
  walorskitweets <- readr::read_csv("data/walorski.csv")
  susanbrookstweets <- readr::read_csv("data/susanbrooks.csv")
  annwagnertweets <- readr::read_csv("data/annwagner.csv")
  hartzlertweets <- readr::read_csv("data/hartzler.csv")
  virginiafoxxtweets <- readr::read_csv("data/virginiafoxx.csv")
  armstrongtweets <- readr::read_csv("data/armstrong.csv")
  stefaniktweets <- readr::read_csv("data/stefanik.csv")
  cathymcmorristweets <- readr::read_csv("data/cathymcmorris.csv")
  lizcheneytweets <- readr::read_csv("data/lizcheney.csv")
  horfrtweets <- dplyr::bind_rows(martharobytweets %>%
                             dplyr::mutate(person = "Martha Roby"),
                           walorskitweets %>%
                             dplyr::mutate(person = "Walorksi"),
                           susanbrookstweets %>%
                             dplyr::mutate(person = "Susan Brooks"),
                           annwagnertweets %>%
                             dplyr::mutate(person = "Ann Wagner"),
                           hartzlertweets %>%
                             dplyr::mutate(person = "Hartzler"),
                           virginiafoxxtweets %>%
                             dplyr::mutate(person = "Virginia Foxx"),
                           armstrongtweets %>%
                             dplyr::mutate(person = "Armstrong"),
                           stefaniktweets %>%
                             dplyr::mutate(person = "Stefanik"),
                           cathymcmorristweets %>%
                             dplyr::mutate(person = "Cathy McMorris"),
                           lizcheneytweets %>%
                             dplyr::mutate(person = "Liz Cheney"))
  save_as_csv(horfrtweets, "data/horfrtweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
  horfrtweets <- readr::read_csv("data/horfrtweets.csv")
  if(nrow(horfrtweets)>0) {
    message("Check your Data Folder. Function ran successfully")
  }
}

#############################
#--------(6) horfemD-------#
#############################
#' Grabs the 50 most recent tweets from Female Democrats in the House of Representatives
#' Saves those tweets in individual csv (for each MC) and uses the UTF-8 file encoding for the CSV
#' Creates one dataframe to combine all of the tweets from each MC
#' With the one large dataframe, it creates a CSV to hold the 50 most recent tweets for each Female Democrat in the House of Representatives
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @export

horfemD <- function() {
  terrisewell <- rtweet::get_timeline('@RepTerriSewell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kirkpatrick<- rtweet::get_timeline('@RepKirkpatrick', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dorismatsui<- rtweet::get_timeline('@DorisMatsui', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bera<- rtweet::get_timeline('@RepBera', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  pelosi <- rtweet::get_timeline('@SpeakerPelosi', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  barbaralee <- rtweet::get_timeline('@RepBarbaraLee', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  speier <- rtweet::get_timeline('@RepSpeier', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  annaeshoo <- rtweet::get_timeline('@RepAnnaEshoo', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  zoelofgren <- rtweet::get_timeline('@RepZoeLofgren', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  katiehill <- rtweet::get_timeline('@RepKatieHill', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brownley <- rtweet::get_timeline('@RepBrownley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  judychu <- rtweet::get_timeline('@RepJudyChu', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gracenapolitano <- rtweet::get_timeline('@gracenapolitano', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  normatorres <- rtweet::get_timeline('@NormaJTorres', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  karenbass <- rtweet::get_timeline('@RepKarenBass', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lindasanchez <- rtweet::get_timeline('@RepLindaSanchez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  roybalallard<- rtweet::get_timeline('@RepRoybalAllard', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  maxinewaters<- rtweet::get_timeline('@RepMaxineWaters', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  barragan<- rtweet::get_timeline('@RepBarragan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  katieporter <- rtweet::get_timeline('@RepKatiePorter', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  susandavis <- rtweet::get_timeline('@RepSusanDavis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dianadegette <- rtweet::get_timeline('@RepDianaDeGette', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rosadelauro<- rtweet::get_timeline('@rosadelauro', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jahanahayes<- rtweet::get_timeline('@RepJahanaHayes', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lbr<- rtweet::get_timeline('@RepLBR', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  eleanornorton<- rtweet::get_timeline('@EleanorNorton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stephmurphy<- rtweet::get_timeline('@RepStephMurphy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kcastor<- rtweet::get_timeline('@USRepKCastor', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hastings <- rtweet::get_timeline('@RepHastingsFL', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  loisfrankel <- rtweet::get_timeline('@RepLoisFrankel', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dwstweets <- rtweet::get_timeline('@RepDWStweets', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  wilson <- rtweet::get_timeline('@RepWilson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dmp <- rtweet::get_timeline('@RepDMP', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  shalala <- rtweet::get_timeline('@RepShalala', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tulsi <- rtweet::get_timeline('@TulsiPress', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  finkenauer <- rtweet::get_timeline('@RepFinkenauer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cindyaxne <- rtweet::get_timeline('@RepCindyAxne', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  robinkelly <- rtweet::get_timeline('@RepRobinKelly', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  janschakowsky <- rtweet::get_timeline('@janschakowsky', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  underwood <- rtweet::get_timeline('@RepUnderwood', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cheri <- rtweet::get_timeline('@RepCheri', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davids <- rtweet::get_timeline('@RepDavids', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  loritrahan <- rtweet::get_timeline('@RepLoriTrahan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kclark <- rtweet::get_timeline('@RepKClark', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  pressley <- rtweet::get_timeline('@RepPressley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  haleystevens <- rtweet::get_timeline('@RepHaleyStevens', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  debdingell <- rtweet::get_timeline('@RepDebDingell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rashida <- rtweet::get_timeline('@RepRashida', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lawrence <- rtweet::get_timeline('@RepLawrence', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  angiecraig <- rtweet::get_timeline('@RepAngieCraig', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  deanphillips <- rtweet::get_timeline('@RepDeanPhillips', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bettymccollum <- rtweet::get_timeline('@BettyMcCollum04', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ilhan<- rtweet::get_timeline('@Ilhan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lacyclay <- rtweet::get_timeline('@LacyClayMO1', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  adams <- rtweet::get_timeline('@RepAdams', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  anniekuster <- rtweet::get_timeline('@RepAnnieKuster', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bonnie<- rtweet::get_timeline('@RepBonnie', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  debhaaland <- rtweet::get_timeline('@RepDebHaaland', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  torressmall <- rtweet::get_timeline('@RepTorresSmall', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dinatitus<- rtweet::get_timeline('@repdinatitus', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  susielee <- rtweet::get_timeline('@RepSusieLee', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kathleenrice <- rtweet::get_timeline('@RepKathleenRice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  nydiavelazquez<- rtweet::get_timeline('@NydiaVelazquez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  yvetteclarke<- rtweet::get_timeline('@RepYvetteClarke', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  maloney<- rtweet::get_timeline('@RepMaloney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  aoc <- rtweet::get_timeline('@RepAOC', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  nitalowey <- rtweet::get_timeline('@NitaLowey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  beatty <- rtweet::get_timeline('@RepBeatty', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  marcykaptur <- rtweet::get_timeline('@RepMarcyKaptur', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  marciafudge<- rtweet::get_timeline('@RepMarciaFudge', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kendrahorn<- rtweet::get_timeline('@RepKendraHorn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bonamici <- rtweet::get_timeline('@RepBonamici', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mgs <- rtweet::get_timeline('@RepMGS', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dean <- rtweet::get_timeline('@RepDean', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  houlahan <- rtweet::get_timeline('@RepHoulahan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  susanwild <- rtweet::get_timeline('@RepSusanWild', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  fletcher <- rtweet::get_timeline('@RepFletcher', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  escobar <- rtweet::get_timeline('@RepEscobar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jacksonlee <- rtweet::get_timeline('@JacksonLeeTX18', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  sylviagarcia <- rtweet::get_timeline('@RepSylviaGarcia', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  elaineluria<- rtweet::get_timeline('@RepElaineLuria', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  spanberger<- rtweet::get_timeline('@RepSpanberger', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  wexton <- rtweet::get_timeline('@RepWexton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  delbene <- rtweet::get_timeline('@RepDelBene', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  herrerabeutler <- rtweet::get_timeline('@HerreraBeutler', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jayapal<- rtweet::get_timeline('@RepJayapal', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kimschrier <- rtweet::get_timeline('@RepKimSchrier', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gwenmoore <- rtweet::get_timeline('@RepGwenMoore', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)

  save_as_csv(terrisewell,"data/terrisewell.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kirkpatrick,"data/kirkpatrick.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(dorismatsui,"data/dorismatsui.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(bera,"data/bera.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(pelosi,"data/pelosi.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(barbaralee,"data/barbaralee.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(speier,"data/speier.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(annaeshoo,"data/annaeshoo.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(zoelofgren,"data/zoelofgren.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(katiehill,"data/katiehill.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(brownley,"data/brownley.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(judychu,"data/judychu.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(gracenapolitano,"data/gracenapolitano.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(normatorres,"data/normatorres.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(karenbass,"data/karenbass.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(lindasanchez,"data/lindasanchez.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(roybalallard,"data/roybalallard.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(maxinewaters,"data/maxinewaters.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(barragan,"data/barragan.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(katieporter,"data/katieporter.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(susandavis,"data/susandavis.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(dianadegette,"data/dianadegette.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(rosadelauro,"data/rosadelauro.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jahanahayes,"data/jahanahayes.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(lbr,"data/lbr.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(eleanornorton,"data/eleanornorton.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(stephmurphy,"data/stephmurphy.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kcastor,"data/kcastor.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(hastings,"data/hastings.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(loisfrankel,"data/loisfrankel.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(dwstweets,"data/dwstweets.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(wilson,"data/wilson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(dmp,"data/dmp.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(shalala,"data/shalala.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(tulsi,"data/tulsi.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(finkenauer,"data/finkenauer.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(cindyaxne,"data/cindyaxne.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(robinkelly,"data/robinkelly.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(janschakowsky,"data/janschakowsky.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(underwood,"data/underwood.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(cheri,"data/cheri.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(davids,"data/davids.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(loritrahan,"data/loritrahan.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kclark,"data/kclark.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(pressley,"data/pressley.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(haleystevens,"data/haleystevens.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(debdingell,"data/debdingell.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(rashida,"data/rashida.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(lawrence,"data/lawrence.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(angiecraig,"data/angiecraig.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(deanphillips,"data/deanphillips.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(bettymccollum,"data/bettymccollum.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(ilhan,"data/ilhan.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(lacyclay,"data/lacyclay.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(adams,"data/adams.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(anniekuster,"data/anniekuster.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(bonnie,"data/bonnie.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(debhaaland,"data/debhaaland.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(torressmall,"data/torressmall.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(dinatitus,"data/dinatitus.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(susielee,"data/susielee.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kathleenrice,"data/kathleenrice.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(nydiavelazquez,"data/nydiavelazquez.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(yvetteclarke,"data/yvetteclarke.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(maloney,"data/maloney.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(aoc,"data/aoc.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(nitalowey,"data/nitalowey.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(beatty,"data/beatty.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(marcykaptur,"data/marcykaptur.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(marciafudge,"data/marciafudge.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kendrahorn,"data/kendrahorn.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(bonamici,"data/bonamici.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mgs,"data/mgs.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(dean,"data/dean.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(houlahan,"data/houlahan.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(susanwild,"data/susanwild.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(fletcher,"data/fletcher.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(escobar,"data/escobar.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jacksonlee,"data/jacksonlee.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(sylviagarcia,"data/sylviagarcia.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(elaineluria,"data/elaineluria.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(spanberger,"data/spanberger.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(wexton,"data/wexton.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(delbene,"data/delbene.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(herrerabeutler,"data/herrerabeutler.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jayapal,"data/jayapal.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kimschrier,"data/kimschrier.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(gwenmoore,"data/gwenmoore.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")

  terrisewelltweets <- readr::read_csv("data/terrisewell.csv")
  kirkpatricktweets <- readr::read_csv("data/kirkpatrick.csv")
  dorismatsuitweets <- readr::read_csv("data/dorismatsui.csv")
  beratweets <- readr::read_csv("data/bera.csv")
  pelositweets <- readr::read_csv("data/pelosi.csv")
  barbaraleetweets <- readr::read_csv("data/barbaralee.csv")
  speiertweets <- readr::read_csv("data/speier.csv")
  annaeshootweets <- readr::read_csv("data/annaeshoo.csv")
  zoelofgrentweets <- readr::read_csv("data/zoelofgren.csv")
  katiehilltweets <- readr::read_csv("data/katiehill.csv")
  brownleytweets <- readr::read_csv("data/brownley.csv")
  judychutweets <- readr::read_csv("data/judychu.csv")
  gracenapolitanotweets <- readr::read_csv("data/gracenapolitano.csv")
  normatorrestweets <- readr::read_csv("data/normatorres.csv")
  karenbasstweets <- readr::read_csv("data/karenbass.csv")
  lindasancheztweets <- readr::read_csv("data/lindasanchez.csv")
  roybalallardtweets <- readr::read_csv("data/roybalallard.csv")
  maxinewaterstweets <- readr::read_csv("data/maxinewaters.csv")
  barragantweets <- readr::read_csv("data/barragan.csv")
  katieportertweets <- readr::read_csv("data/katieporter.csv")
  susandavistweets <- readr::read_csv("data/susandavis.csv")
  dianadegettetweets <- readr::read_csv("data/dianadegette.csv")
  rosadelaurotweets <- readr::read_csv("data/rosadelauro.csv")
  jahanahayestweets <- readr::read_csv("data/jahanahayes.csv")
  lbrtweets <- readr::read_csv("data/lbr.csv")
  eleanornortontweets <- readr::read_csv("data/eleanornorton.csv")
  stephmurphytweets <- readr::read_csv("data/stephmurphy.csv")
  kcastortweets <- readr::read_csv("data/kcastor.csv")
  hastingstweets <- readr::read_csv("data/hastings.csv")
  loisfrankeltweets <- readr::read_csv("data/loisfrankel.csv")
  dwsweetstweets <- readr::read_csv("data/dwstweets.csv")
  wilsontweets <- readr::read_csv("data/wilson.csv")
  dmptweets <-  readr::read_csv("data/dmp.csv")
  shalalatweets <- readr::read_csv("data/shalala.csv")
  tulsitweets <- readr::read_csv("data/tulsi.csv")
  finkenauertweets <- readr::read_csv("data/finkenauer.csv")
  cindyaxnetweets <- readr::read_csv("data/cindyaxne.csv")
  robinkellytweets <- readr::read_csv("data/robinkelly.csv")
  janschakowskytweets <- readr::read_csv("data/janschakowsky.csv")
  underwoodtweets <- readr::read_csv("data/underwood.csv")
  cheritweets <- readr::read_csv("data/cheri.csv")
  davidstweets <- readr::read_csv("data/davids.csv")
  loritrahantweets <- readr::read_csv("data/loritrahan.csv")
  kclarktweets <- readr::read_csv("data/kclark.csv")
  pressleytweets <- readr::read_csv("data/pressley.csv")
  haleystevenstweets <- readr::read_csv("data/haleystevens.csv")
  debdingelltweets <- readr::read_csv("data/debdingell.csv")
  rashidatweets <- readr::read_csv("data/rashida.csv")
  lawrencetweets <- readr::read_csv("data/lawrence.csv")
  angiecraigtweets <- readr::read_csv("data/angiecraig.csv")
  deanphillipstweets <- readr::read_csv("data/deanphillips.csv")
  bettymccollumtweets <- readr::read_csv("data/bettymccollum.csv")
  ilhantweets <- readr::read_csv("data/ilhan.csv")
  lacyclaytweets <- readr::read_csv("data/lacyclay.csv")
  adamstweets <- readr::read_csv("data/adams.csv")
  anniekustertweets <- readr::read_csv("data/anniekuster.csv")
  bonnietweets <- readr::read_csv("data/bonnie.csv")
  debhaalandtweets <- readr::read_csv("data/debhaaland.csv")
  torressmalltweets <- readr::read_csv("data/torressmall.csv")
  dinatitustweets <- readr::read_csv("data/dinatitus.csv")
  susieleetweets <- readr::read_csv("data/susielee.csv")
  kathleenricetweets <- readr::read_csv("data/kathleenrice.csv")
  nydiavelazqueztweets <- readr::read_csv("data/nydiavelazquez.csv")
  yvetteclarketweets <- readr::read_csv("data/yvetteclarke.csv")
  maloneytweets <- readr::read_csv("data/maloney.csv")
  aoctweets <- readr::read_csv("data/aoc.csv")
  nitaloweytweets <- readr::read_csv("data/nitalowey.csv")
  beattytweets <- readr::read_csv("data/beatty.csv")
  marcykapturtweets <- readr::read_csv("data/marcykaptur.csv")
  marciafudgetweets <- readr::read_csv("data/marciafudge.csv")
  kendrahorntweets <- readr::read_csv("data/kendrahorn.csv")
  bonamicitweets <- readr::read_csv("data/bonamici.csv")
  mgstweets <- readr::read_csv("data/mgs.csv")
  deantweets <- readr::read_csv("data/dean.csv")
  houlahantweets <- readr::read_csv("data/houlahan.csv")
  susanwildtweets <- readr::read_csv("data/susanwild.csv")
  fletchertweets <- readr::read_csv("data/fletcher.csv")
  escobartweets <- readr::read_csv("data/escobar.csv")
  jacksonleetweets <- readr::read_csv("data/jacksonlee.csv")
  sylviagarciatweets <- readr::read_csv("data/sylviagarcia.csv")
  elaineluriatweets <- readr::read_csv("data/elaineluria.csv")
  spanbergertweets <- readr::read_csv("data/spanberger.csv")
  wextontweets <- readr::read_csv("data/wexton.csv")
  delbenetweets <- readr::read_csv("data/delbene.csv")
  herrerabeutlertweets <- readr::read_csv("data/herrerabeutler.csv")
  jayapaltweets <- readr::read_csv("data/jayapal.csv")
  kimschriertweets <- readr::read_csv("data/kimschrier.csv")
  gwenmooretweets <- readr::read_csv("data/gwenmoore.csv")


  horfdtweets <- dplyr::bind_rows(terrisewelltweets %>%
                             dplyr::mutate(person = "Terri Swell"),
                           kirkpatricktweets %>%
                             dplyr::mutate(person = "Kirkpatrick"),
                           dorismatsuitweets %>%
                             dplyr::mutate(person = "Doris Matsui"),
                           beratweets %>%
                             dplyr::mutate(person = "Bera"),
                           pelositweets %>%
                             dplyr::mutate(person = "Nancy Pelosi"),
                           barbaraleetweets %>%
                             dplyr::mutate(person = "Barbara Lee"),
                           speiertweets %>%
                             dplyr::mutate(person = "Speier"),
                           annaeshootweets %>%
                             dplyr::mutate(person = "Anna Eshoo"),
                           zoelofgrentweets %>%
                             dplyr::mutate(person = "Zoe Lofgren"),
                           katiehilltweets %>%
                             dplyr::mutate(person = "Katie Hill"),
                           brownleytweets %>%
                             dplyr::mutate(person = "Brownley"),
                           judychutweets %>%
                             dplyr::mutate(person = "Judy Chu"),
                           gracenapolitanotweets %>%
                             dplyr::mutate(person = "Grace Napolitano"),
                           normatorrestweets %>%
                             dplyr::mutate(person = "Norma Torres"),
                           karenbasstweets %>%
                             dplyr::mutate(person = "Karen Bass"),
                           lindasancheztweets %>%
                             dplyr::mutate(person = "Linda Sanchez"),
                           roybalallardtweets %>%
                             dplyr::mutate(person = "Roybal Allard"),
                           maxinewaterstweets %>%
                             dplyr::mutate(person = "Maxine Waters"),
                           barragantweets %>%
                             dplyr::mutate(person = "Barragan"),
                           katieportertweets %>%
                             dplyr::mutate(person = "Katie Porter"),
                           susandavistweets %>%
                             dplyr::mutate(person = "Susan Davis"),
                           dianadegettetweets %>%
                             dplyr::mutate(person = "Diana Delgette"),
                           rosadelaurotweets %>%
                             dplyr::mutate(person = "Rosa Delauro"),
                           jahanahayestweets %>%
                             dplyr::mutate(person = "Jahana Hayes"),
                           lbrtweets %>%
                             dplyr::mutate(person = "LBR"),
                           eleanornortontweets %>%
                             dplyr::mutate(person = "Eleanor Norton"),
                           stephmurphytweets %>%
                             dplyr::mutate(person = "Steph Murphy"),
                           kcastortweets %>%
                             dplyr::mutate(person = "K Castor"),
                           hastingstweets %>%
                             dplyr::mutate(person = "Hastings"),
                           loisfrankeltweets %>%
                             dplyr::mutate(person = "Lois Frankel"),
                           dwsweetstweets %>%
                             dplyr::mutate(person = "DW Stweets"),
                           wilsontweets %>%
                             dplyr::mutate(person = "Wilson"),
                           dmptweets %>%
                             dplyr::mutate(person = "DMP"),
                           shalalatweets %>%
                             dplyr::mutate(person = "Shalala"),
                           tulsitweets %>%
                             dplyr::mutate(person = "Tulsi"),
                           finkenauertweets %>%
                             dplyr::mutate(person = "Finkenauer"),
                           cindyaxnetweets %>%
                             dplyr::mutate(person = "Cindy Axne"),
                           robinkellytweets %>%
                             dplyr::mutate(person = "Robin Kelly"),
                           janschakowskytweets %>%
                             dplyr::mutate(person = "Janschakowsky"),
                           underwoodtweets %>%
                             dplyr::mutate(person = "Underwood"),
                           cheritweets %>%
                             dplyr::mutate(person = "Cheri"),
                           davidstweets %>%
                             dplyr::mutate(person = "Davids"),
                           loritrahantweets %>%
                             dplyr::mutate(person = "Lori Trahan"),
                           kclarktweets %>%
                             dplyr::mutate(person = "K Clark"),
                           pressleytweets %>%
                             dplyr::mutate(person = "Pressley"),
                           haleystevenstweets %>%
                             dplyr::mutate(person = "Haley Stevens"),
                           debdingelltweets %>%
                             dplyr::mutate(person = "Deb Dingell"),
                           rashidatweets %>%
                             dplyr::mutate(person = "Rashida Talib"),
                           lawrencetweets %>%
                             dplyr::mutate(person = "Lawrence"),
                           angiecraigtweets %>%
                             dplyr::mutate(person = "Angie Craig"),
                           deanphillipstweets %>%
                             dplyr::mutate(person = "Dean Phillips"),
                           bettymccollumtweets %>%
                             dplyr::mutate(person = "Betty McCollum"),
                           ilhantweets %>%
                             dplyr::mutate(person = "Ilhan Omar"),
                           lacyclaytweets %>%
                             dplyr::mutate(person = "Lacy Clay"),
                           adamstweets %>%
                             dplyr::mutate(person = "Adams"),
                           anniekustertweets %>%
                             dplyr::mutate(person = "Annie Kuster"),
                           bonnietweets %>%
                             dplyr::mutate(person = "Bonnie"),
                           debhaalandtweets %>%
                             dplyr::mutate(person = "Deb Haaland"),
                           torressmalltweets %>%
                             dplyr::mutate(person = "Torres Small"),
                           dinatitustweets %>%
                             dplyr::mutate(person = "Dina Titus"),
                           susieleetweets %>%
                             dplyr::mutate(person = "Susie Lee"),
                           kathleenricetweets %>%
                             dplyr::mutate(person = "Kathleen Rice"),
                           nydiavelazqueztweets %>%
                             dplyr::mutate(person = "Nydia Velazquez"),
                           yvetteclarketweets %>%
                             dplyr::mutate(person = "Yvette Clarke"),
                           maloneytweets %>%
                             dplyr::mutate(person = "Maloney"),
                           aoctweets %>%
                             dplyr::mutate(person = "AOC"),
                           nitaloweytweets %>%
                             dplyr::mutate(person = "Nita Lowey"),
                           beattytweets %>%
                             dplyr::mutate(person = "Beatty"),
                           marcykapturtweets %>%
                             dplyr::mutate(person = "Marcy Kaptur"),
                           marciafudgetweets %>%
                             dplyr::mutate(person = "Marcia Fudge"),
                           kendrahorntweets %>%
                             dplyr::mutate(person = "Kendra Horn"),
                           bonamicitweets %>%
                             dplyr::mutate(person = "Bonamici"),
                           mgstweets %>%
                             dplyr::mutate(person = "MGS"),
                           deantweets %>%
                             dplyr::mutate(person = "Dean"),
                           houlahantweets %>%
                             dplyr::mutate(person = "Houlahan"),
                           susanwildtweets %>%
                             dplyr::mutate(person = "Susan Wild"),
                           fletchertweets %>%
                             dplyr::mutate(person = "Fletcher"),
                           escobartweets %>%
                             dplyr::mutate(person = "Escobar"),
                           jacksonleetweets %>%
                             dplyr::mutate(person = "Jackson Lee"),
                           sylviagarciatweets %>%
                             dplyr::mutate(person = "Sylvia Garcia"),
                           elaineluriatweets %>%
                             dplyr::mutate(person = "Elaine Luria"),
                           spanbergertweets %>%
                             dplyr::mutate(person = "Spanberger"),
                           wextontweets %>%
                             dplyr::mutate(person = "Wexton"),
                           delbenetweets %>%
                             dplyr::mutate(person = "Delbene"),
                           herrerabeutlertweets %>%
                             dplyr::mutate(person = "Herrera Beutler"),
                           jayapaltweets %>%
                             dplyr::mutate(person = "Jayapal"),
                           kimschriertweets %>%
                             dplyr::mutate(person = "Kim Schrier"),
                           gwenmooretweets %>%
                             dplyr::mutate(person = "Gwen Moore"))

  save_as_csv(horfdtweets, "data/horfdtweets.csv", prepend_ids = TRUE , na = "", fileEncoding = "UTF-8")
  horfdtweets <- readr::read_csv("data/horfdtweets.csv")
  if(nrow(horfdtweets)>0) {
    message("Check your Data Folder. Function ran successfully")
  }
}

#############################
#--------(7) hormaleR-------#
#############################
#' Grabs the 50 most recent tweets from Male Republicans in the House of Representatives
#' Saves those tweets in individual csv (for each MC) and uses the UTF-8 file encoding for the CSV
#' Creates one dataframe to combine all of the tweets from each MC
#' With the one large dataframe, it creates a CSV to hold the 50 most recent tweets for each Male Republican in the House of Representatives
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @export

hormaleR <- function() {
  donyoung <- rtweet::get_timeline('@repdonyoung', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  byrne <- rtweet::get_timeline('@RepByrne', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikerogers <- rtweet::get_timeline('@RepMikeRogersAL', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  robertaderholt <- rtweet::get_timeline('@Robert_Aderholt', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mobrooks <- rtweet::get_timeline('@RepMoBrooks', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  garypalmer <- rtweet::get_timeline('@USRepGaryPalmer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rickcrawford <- rtweet::get_timeline('@RepRickCrawford', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  frenchhill<- rtweet::get_timeline('@RepFrenchHill', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stevewomack <- rtweet::get_timeline('@rep_stevewomack', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  westerman <- rtweet::get_timeline('@RepWesterman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gosar <- rtweet::get_timeline('@RepGosar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andybiggs <- rtweet::get_timeline('@RepAndyBiggsAZ', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  david <- rtweet::get_timeline('@RepDavid', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dlesko <- rtweet::get_timeline('@RepDLesko', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lamalfa<- rtweet::get_timeline('@RepLaMalfa', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mcclintock <- rtweet::get_timeline('@RepMcClintock', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  paulcook <- rtweet::get_timeline('@RepPaulCook', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  devinnunes <- rtweet::get_timeline('@RepDevinNunes', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kevinmccarthy <- rtweet::get_timeline('@GOPLeader', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kencalvert<- rtweet::get_timeline('@KenCalvert', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tipton <- rtweet::get_timeline('@RepTipton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kenbuck <- rtweet::get_timeline('@RepKenBuck', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dlamborn <- rtweet::get_timeline('@RepDLamborn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mattgaetz <- rtweet::get_timeline('@RepMattGaetz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  nealdunn <- rtweet::get_timeline('@DrNealDunnFL2', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tedyoho <- rtweet::get_timeline('@RepTedYoho', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rutherford <- rtweet::get_timeline('@RepRutherfordFL', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  michaelwaltz <- rtweet::get_timeline('@RepMichaelWaltz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  billposey <- rtweet::get_timeline('@congbillposey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  webster <- rtweet::get_timeline('@RepWebster', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gusbilirakis <- rtweet::get_timeline('@RepGusBilirakis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  vernbuchanan<- rtweet::get_timeline('@VernBuchanan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gregsteube <- rtweet::get_timeline('@RepGregSteube', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brianmast <- rtweet::get_timeline('@RepBrianMast', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rooney <- rtweet::get_timeline('@RepRooney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mariodb <- rtweet::get_timeline('@MarioDB', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  buddycarter <- rtweet::get_timeline('@RepBuddyCarter', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  drewferguson <- rtweet::get_timeline('@RepDrewFerguson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  robwoodall<- rtweet::get_timeline('@RepRobWoodall', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  austinscott <- rtweet::get_timeline('@AustinScottGA08', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dougcollins <- rtweet::get_timeline('@RepDougCollins', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hice <- rtweet::get_timeline('@CongressmanHice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  loudermilk <- rtweet::get_timeline('@RepLoudermilk', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rickallen <- rtweet::get_timeline('@RepRickAllen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomgraves <- rtweet::get_timeline('@RepTomGraves', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  russfulcher <- rtweet::get_timeline('@RepRussFulcher', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikesimpson <- rtweet::get_timeline('@CongMikeSimpson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bost <- rtweet::get_timeline('@RepBost', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rodneydavis <- rtweet::get_timeline('@RodneyDavis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  shimkus <- rtweet::get_timeline('@RepShimkus', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kinzinger <- rtweet::get_timeline('@RepKinzinger', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lahood <- rtweet::get_timeline('@RepLaHood', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimbanks <- rtweet::get_timeline('@RepJimBanks', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimbaird <- rtweet::get_timeline('@RepJimBaird', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gregpence <- rtweet::get_timeline('@RepGregPence', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  larrybucshon <- rtweet::get_timeline('@RepLarryBucshon', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  trey<- rtweet::get_timeline('@RepTrey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ronestes <- rtweet::get_timeline('@RepRonEstes', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  guthrie <- rtweet::get_timeline('@RepGuthrie', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  thomasmassie <- rtweet::get_timeline('@RepThomasMassie', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  halrogers<- rtweet::get_timeline('@RepHalRogers', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andybarr <- rtweet::get_timeline('@RepAndyBarr', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stevescalise <- rtweet::get_timeline('@SteveScalise', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  clayhiggins <- rtweet::get_timeline('@RepClayHiggins', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikejohnson <- rtweet::get_timeline('@RepMikeJohnson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  abraham <- rtweet::get_timeline('@RepAbraham', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  garretgraves <- rtweet::get_timeline('@RepGarretGraves', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andyharris<- rtweet::get_timeline('@RepAndyHarrisMD', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jackbergman <- rtweet::get_timeline('@RepJackBergman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  huizenga <- rtweet::get_timeline('@RepHuizenga', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  justinamash <- rtweet::get_timeline('@justinamash', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  moolenaar<- rtweet::get_timeline('@RepMoolenaar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  fredupton <- rtweet::get_timeline('@RepFredUpton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  walberg <- rtweet::get_timeline('@RepWalberg', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  slotkin <- rtweet::get_timeline('@RepSlotkin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  paulmitchell <- rtweet::get_timeline('@RepPaulMitchell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hagedorn<- rtweet::get_timeline('@RepHagedorn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomemmer <- rtweet::get_timeline('@RepTomEmmer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  petestauber <- rtweet::get_timeline('@RepPeteStauber', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  blaine <- rtweet::get_timeline('@RepBlaine', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  samgraves<- rtweet::get_timeline('@RepSamGraves', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  long <- rtweet::get_timeline('@USRepLong', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jasonsmith <- rtweet::get_timeline('@RepJasonSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  trentkelly <- rtweet::get_timeline('@RepTrentKelly', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  michaelguest <- rtweet::get_timeline('@RepMichaelGuest', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  palazzo<- rtweet::get_timeline('@CongPalazzo', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  holding <- rtweet::get_timeline('@RepHolding', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  walterjones <- rtweet::get_timeline('@RepWalterJones', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  markwalker <- rtweet::get_timeline('@RepMarkWalker', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidrouzer <- rtweet::get_timeline('@RepDavidRouzer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  richhudson <- rtweet::get_timeline('@RepRichHudson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  patrickmchenry <- rtweet::get_timeline('@PatrickMcHenry', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  markmeadows <- rtweet::get_timeline('@RepMarkMeadows', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tedbudd <- rtweet::get_timeline('@RepTedBudd', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jefffortenberry <- rtweet::get_timeline('@JeffFortenberry', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  donbacon<- rtweet::get_timeline('@RepDonBacon', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  adriansmith <- rtweet::get_timeline('@RepAdrianSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jvd <- rtweet::get_timeline('@CongressmanJVD', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chrissmith <- rtweet::get_timeline('@RepChrisSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  markamodei <- rtweet::get_timeline('@MarkAmodeiNV2', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  leezeldin <- rtweet::get_timeline('@RepLeeZeldin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  peterking <- rtweet::get_timeline('@RepPeteKing', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomreed <- rtweet::get_timeline('@RepTomReed', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnkatko <- rtweet::get_timeline('@RepJohnKatko', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chriscollins <- rtweet::get_timeline('@RepChrisCollins', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stevechabot <- rtweet::get_timeline('@RepSteveChabot', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bradwenstrup<- rtweet::get_timeline('@RepBradWenstrup', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimjordan<- rtweet::get_timeline('@Jim_Jordan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  boblatta <- rtweet::get_timeline('@boblatta', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  billjohnson <- rtweet::get_timeline('@RepBillJohnson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bobgibbs <- rtweet::get_timeline('@RepBobGibbs', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  warrendavidson <- rtweet::get_timeline('@WarrenDavidson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  miketurner <- rtweet::get_timeline('@RepMikeTurner', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  balderson <- rtweet::get_timeline('@RepBalderson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davejoyce <- rtweet::get_timeline('@RepDaveJoyce', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stevestivers <- rtweet::get_timeline('@RepSteveStivers', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  agonzalez<- rtweet::get_timeline('@RepAGonzalez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kevinhern <- rtweet::get_timeline('@repkevinhern', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mullin <- rtweet::get_timeline('@RepMullin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  franklucas <- rtweet::get_timeline('@RepFrankLucas', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomcole <- rtweet::get_timeline('@TomColeOK04', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brianfitz <- rtweet::get_timeline('@RepBrianFitz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  meuser <- rtweet::get_timeline('@RepMeuser', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  scottperry <- rtweet::get_timeline('@RepScottPerry', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  smucker <- rtweet::get_timeline('@RepSmucker', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnjoyce <- rtweet::get_timeline('@RepJohnJoyce', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  greschenthaler <- rtweet::get_timeline('@GReschenthaler', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gt <- rtweet::get_timeline('@CongressmanGT', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikekelly <- rtweet::get_timeline('@MikeKellyPA', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joewilson <- rtweet::get_timeline('@RepJoeWilson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jeffduncan <- rtweet::get_timeline('@RepJeffDuncan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  timmons <- rtweet::get_timeline('@reptimmons', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ralphnorman <- rtweet::get_timeline('@RepRalphNorman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomrice <- rtweet::get_timeline('@RepTomRice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dustyjohnson <- rtweet::get_timeline('@RepDustyJohnson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  philroe <- rtweet::get_timeline('@DrPhilRoe', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  timburchett <- rtweet::get_timeline('@RepTimBurchett', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chuck <- rtweet::get_timeline('@RepChuck', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  desjarlais <- rtweet::get_timeline('@DesJarlaisTN04l', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnrose <- rtweet::get_timeline('@RepJohnRose', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  markgreen <- rtweet::get_timeline('@RepMarkGreen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidkustoff <- rtweet::get_timeline('@RepDavidKustoff', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  louiegohmert <- rtweet::get_timeline('@replouiegohmert', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dancrenshaw<- rtweet::get_timeline('@RepDanCrenshaw', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  vantaylor <- rtweet::get_timeline('@RepVanTaylor', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ratcliffe <- rtweet::get_timeline('@RepRatcliffe', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lancegooden <- rtweet::get_timeline('@RepLanceGooden', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ronwright <- rtweet::get_timeline('@RepRonWright', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kevinbrady <- rtweet::get_timeline('@RepKevinBrady', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mccaul <- rtweet::get_timeline('@RepMcCaul', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  conaway <- rtweet::get_timeline('@ConawayTX11', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kaygranger <- rtweet::get_timeline('@RepKayGranger', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mac <- rtweet::get_timeline('@MacTXPress', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  randy <- rtweet::get_timeline('@TXRandy14', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  billflores <- rtweet::get_timeline('@RepBillFlores', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  arrington <- rtweet::get_timeline('@RepArrington', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chiproy <- rtweet::get_timeline('@RepChipRoy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  peteolson <- rtweet::get_timeline('@RepPeteOlson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hurd <- rtweet::get_timeline('@HurdOnTheHill', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kenmarchant <- rtweet::get_timeline('@RepKenMarchant', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rwilliams <- rtweet::get_timeline('@RepRWilliams', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  michaelburgess <- rtweet::get_timeline('@michaelcburgess', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cloud<- rtweet::get_timeline('@RepCloudTX', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  carter <- rtweet::get_timeline('@JudgeCarter', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brianbabin <- rtweet::get_timeline('@RepBrianBabin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  robbishop <- rtweet::get_timeline('@RepRobBishop', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chrisstewart <- rtweet::get_timeline('@RepChrisStewart', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johncurtis <- rtweet::get_timeline('@RepJohnCurtis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  robwittman <- rtweet::get_timeline('@RobWittman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  riggleman <- rtweet::get_timeline('@RepRiggleman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bencline <- rtweet::get_timeline('@RepBenCline', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mgriffith <- rtweet::get_timeline('@RepMGriffith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  newhouse <- rtweet::get_timeline('@RepNewhouse', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bryansteil <- rtweet::get_timeline('@RepBryanSteil', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jim <- rtweet::get_timeline('@JimPressOffice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  grothman <- rtweet::get_timeline('@RepGrothman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  seanduffy <- rtweet::get_timeline('@RepSeanDuffy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gallagher <- rtweet::get_timeline('@RepGallagher', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mckinley <- rtweet::get_timeline('@RepMcKinley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  alexmooney <- rtweet::get_timeline('@RepAlexMooney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)




















  save_as_csv(donyoung,"data/donyoung.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(byrne,"data/byrne.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mikerogers,"data/mikerogers.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(robertaderholt,"data/robertaderholt.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mobrooks,"data/mobrooks.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(garypalmer,"data/garypalmer.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(rickcrawford,"data/rickcrawford.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(frenchhill,"data/frenchhill.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(stevewomack,"data/stevewomack.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(westerman,"data/westerman.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(gosar,"data/gosar.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(andybiggs,"data/andybiggs.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(david,"data/david.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(dlesko,"data/dlesko.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(lamalfa,"data/lamalfa.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mcclintock,"data/mcclintock.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(paulcook,"data/paulcook.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(devinnunes,"data/devinnunes.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kevinmccarthy,"data/kevinmccarthy.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kencalvert,"data/kencalvert.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(tipton,"data/tipton.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kenbuck,"data/kenbuck.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(dlamborn,"data/dlamborn.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mattgaetz,"data/mattgaetz.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(nealdunn,"data/nealdunn.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(tedyoho,"data/tedyoho.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(rutherford,"data/rutherford.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(michaelwaltz,"data/michaelwaltz.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(billposey,"data/billposey.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(webster,"data/webster.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(gusbilirakis,"data/gusbilirakis.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(vernbuchanan,"data/vernbuchanan.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(gregsteube,"data/gregsteube.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(brianmast,"data/brianmast.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(rooney,"data/rooney.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mariodb,"data/mariodb.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(buddycarter,"data/buddycarter.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(drewferguson,"data/drewferguson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(robwoodall,"data/robwoodall.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(austinscott,"data/austinscott.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(dougcollins,"data/dougcollins.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(hice,"data/hice.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(loudermilk,"data/loudermilk.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(rickallen,"data/rickallen.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(tomgraves,"data/tomgraves.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(russfulcher,"data/russfulcher.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mikesimpson,"data/mikesimpson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(bost,"data/bost.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(rodneydavis,"data/rodneydavis.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(shimkus,"data/shimkus.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kinzinger,"data/kinzinger.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(lahood,"data/lahood.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jimbanks,"data/jimbanks.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jimbaird,"data/jimbaird.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(gregpence,"data/gregpence.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(larrybucshon,"data/larrybucshon.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(trey,"data/trey.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(ronestes,"data/ronestes.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(guthrie,"data/guthrie.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(thomasmassie,"data/thomasmassie.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(halrogers,"data/halrogers.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(andybarr,"data/andybarr.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(stevescalise,"data/stevescalise.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(clayhiggins,"data/clayhiggins.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mikejohnson,"data/mikejohnson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(abraham,"data/abraham.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(garretgraves,"data/garretgraves.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(andyharris,"data/andyharris.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jackbergman,"data/jackbergman.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(huizenga,"data/huizenga.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(justinamash,"data/justinamash.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(moolenaar,"data/moolenaar.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(fredupton,"data/fredupton.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(walberg,"data/walberg.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(slotkin,"data/slotkin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(paulmitchell,"data/paulmitchell.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(hagedorn,"data/hagedorn.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(tomemmer,"data/tomemmer.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(petestauber,"data/petestauber.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(blaine,"data/blaine.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(samgraves,"data/samgraves.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(long,"data/long.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jasonsmith,"data/jasonsmith.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(trentkelly,"data/trentkelly.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(michaelguest,"data/michaelguest.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(palazzo,"data/palazzo.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(holding,"data/holding.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(walterjones,"data/walterjones.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(markwalker,"data/markwalker.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(davidrouzer,"data/davidrouzer.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(richhudson,"data/richhudson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(patrickmchenry,"data/patrickmchenry.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(markmeadows,"data/markmeadows.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(tedbudd,"data/tedbudd.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jefffortenberry,"data/jefffortenberry.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(donbacon,"data/donbacon.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(adriansmith,"data/adriansmith.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jvd,"data/jvd.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(chrissmith,"data/chrissmith.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(markamodei,"data/markamodei.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(leezeldin,"data/leezeldin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(peterking,"data/peterking.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(tomreed,"data/tomreed.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(johnkatko,"data/johnkatko.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(chriscollins,"data/chriscollins.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(stevechabot,"data/stevechabot.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(bradwenstrup,"data/bradwenstrup.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jimjordan,"data/jimjordan.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(boblatta,"data/boblatta.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(billjohnson,"data/billjohnson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(bobgibbs,"data/bobgibbs.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(warrendavidson,"data/warrendavidson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(miketurner,"data/miketurner.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(balderson,"data/balderson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(davejoyce,"data/davejoyce.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(stevestivers,"data/stevestivers.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(agonzalez,"data/agonzalez.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kevinhern,"data/kevinhern.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mullin,"data/mullin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(franklucas,"data/franklucas.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(tomcole,"data/tomcole.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(brianfitz,"data/brianfitz.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(meuser,"data/meuser.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(scottperry,"data/scottperry.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(smucker,"data/smucker.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(johnjoyce,"data/johnjoyce.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(greschenthaler,"data/greschenthaler.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(gt,"data/gt.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mikekelly,"data/mikekelly.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(joewilson,"data/joewilson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jeffduncan,"data/jeffduncan.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(timmons,"data/timmons.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(ralphnorman,"data/ralphnorman.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(tomrice,"data/tomrice.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(dustyjohnson,"data/dustyjohnson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(philroe,"data/philroe.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(timburchett,"data/timburchett.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(chuck,"data/chuck.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(desjarlais,"data/desjarlais.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(johnrose,"data/johnrose.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(markgreen,"data/markgreen.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(davidkustoff,"data/davidkustoff.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(louiegohmert,"data/louiegohmert.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(dancrenshaw,"data/dancrenshaw.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(vantaylor,"data/vantaylor.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(ratcliffe,"data/ratcliffe.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(lancegooden,"data/lancegooden.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(ronwright,"data/ronwright.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kevinbrady,"data/kevinbrady.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mccaul,"data/mccaul.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(conaway,"data/conaway.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kaygranger,"data/kaygranger.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mac,"data/mac.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(randy,"data/randy.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(billflores,"data/billflores.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(arrington,"data/arrington.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(chiproy,"data/chiproy.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(peteolson,"data/peteolson.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(hurd,"data/hurd.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(kenmarchant,"data/kenmarchant.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(rwilliams,"data/rwilliams.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(michaelburgess,"data/michaelburgess.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(cloud,"data/cloud.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(carter,"data/carter.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(brianbabin,"data/brianbabin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(robbishop,"data/robbishop.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(chrisstewart,"data/chrisstewart.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(johncurtis,"data/johncurtis.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(robwittman,"data/robwittman.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(riggleman,"data/riggleman.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(bencline,"data/bencline.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mgriffith,"data/mgriffith.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(newhouse,"data/newhouse.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(bryansteil,"data/bryansteil.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(jim,"data/jim.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(grothman,"data/grothman.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(seanduffy,"data/seanduffy.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(gallagher,"data/gallagher.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(mckinley,"data/mckinley.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(alexmooney,"data/alexmooney.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")



  donyoungtweets <- readr::read_csv("data/donyoung.csv")
  byrnetweets <- readr::read_csv("data/byrne.csv")
  mikerogerstweets <- readr::read_csv("data/mikerogers.csv")
  robertaderholttweets <- readr::read_csv("data/robertaderholt.csv")
  mobrookstweets <- readr::read_csv("data/mobrooks.csv")
  garypalmertweets <- readr::read_csv("data/garypalmer.csv")
  rickcrawfordtweets <- readr::read_csv("data/rickcrawford.csv")
  frenchhilltweets <- readr::read_csv("data/frenchhill.csv")
  stevewomacktweets <- readr::read_csv("data/stevewomack.csv")
  westermantweets <- readr::read_csv("data/westerman.csv")
  gosartweets <- readr::read_csv("data/gosar.csv")
  andybiggstweets <- readr::read_csv("data/andybiggs.csv")
  davidtweets <- readr::read_csv("data/david.csv")
  dleskotweets <- readr::read_csv("data/dlesko.csv")
  lamalfatweets <- readr::read_csv("data/lamalfa.csv")
  mcclintocktweets <- readr::read_csv("data/mcclintock.csv")
  paulcooktweets <- readr::read_csv("data/paulcook.csv")
  devinnunestweets <- readr::read_csv("data/devinnunes.csv")
  kevinmccarthytweets <- readr::read_csv("data/kevinmccarthy.csv")
  kencalverttweets <- readr::read_csv("data/kencalvert.csv")
  tiptontweets <- readr::read_csv("data/tipton.csv")
  kenbucktweets <- readr::read_csv("data/kenbuck.csv")
  dlamborntweets <- readr::read_csv("data/dlamborn.csv")
  mattgaetztweets <- readr::read_csv("data/mattgaetz.csv")
  nealdunntweets <- readr::read_csv("data/nealdunn.csv")
  tedyohotweets <- readr::read_csv("data/tedyoho.csv")
  rutherfordtweets <- readr::read_csv("data/rutherford.csv")
  michaelwaltztweets <- readr::read_csv("data/michaelwaltz.csv")
  billposeytweets <- readr::read_csv("data/billposey.csv")
  webstertweets <- readr::read_csv("data/webster.csv")
  gusbilirakistweets <- readr::read_csv("data/gusbilirakis.csv")
  vernbuchanantweets <- readr::read_csv("data/vernbuchanan.csv")
  gregsteubetweets <- readr::read_csv("data/gregsteube.csv")
  brianmasttweets <-  readr::read_csv("data/brianmast.csv")
  rooneytweets <- readr::read_csv("data/rooney.csv")
  mariodbtweets <-  readr::read_csv("data/mariodb.csv")
  buddycartertweets <- readr::read_csv("data/buddycarter.csv")
  drewfergusontweets <- readr::read_csv("data/drewferguson.csv")
  robwoodalltweets <- readr::read_csv("data/robwoodall.csv")
  austinscotttweets <- readr::read_csv("data/austinscott.csv")
  dougcollinstweets <- readr::read_csv("data/dougcollins.csv")
  hicetweets <- readr::read_csv("data/hice.csv")
  loudermilktweets <- readr::read_csv("data/loudermilk.csv")
  rickallentweets <- readr::read_csv("data/rickallen.csv")
  tomgravestweets <- readr::read_csv("data/tomgraves.csv")
  russfulchertweets <- readr::read_csv("data/russfulcher.csv")
  mikesimpsontweets <- readr::read_csv("data/mikesimpson.csv")
  bosttweets <- readr::read_csv("data/bost.csv")
  rodneydavistweets <- readr::read_csv("data/rodneydavis.csv")
  shimkustweets <- readr::read_csv("data/shimkus.csv")
  kinzingertweets <- readr::read_csv("data/kinzinger.csv")
  lahoodtweets <- readr::read_csv("data/lahood.csv")
  jimbankstweets <- readr::read_csv("data/jimbanks.csv")
  jimbairdtweets <- readr::read_csv("data/jimbaird.csv")
  gregpencetweets <- readr::read_csv("data/gregpence.csv")
  larrybucshontweets <- readr::read_csv("data/larrybucshon.csv")
  treytweets <- readr::read_csv("data/trey.csv")
  ronestestweets <- readr::read_csv("data/ronestes.csv")
  guthrietweets <- readr::read_csv("data/guthrie.csv")
  thomasmassietweets <- readr::read_csv("data/thomasmassie.csv")
  halrogerstweets <- readr::read_csv("data/halrogers.csv")
  andybarrtweets <- readr::read_csv("data/andybarr.csv")
  stevescalisetweets <- readr::read_csv("data/stevescalise.csv")
  clayhigginstweets <- readr::read_csv("data/clayhiggins.csv")
  mikejohnsontweets <- readr::read_csv("data/clayhiggins.csv")
  abrahamtweets <-  readr::read_csv("data/abraham.csv")
  garretgravestweets <- readr::read_csv("data/garretgraves.csv")
  andyharristweets <- readr::read_csv("data/andyharris.csv")
  jackbergmantweets <- readr::read_csv("data/jackbergman.csv")
  huizengatweets <- readr::read_csv("data/huizenga.csv")
  justinamashtweets <- readr::read_csv("data/justinamash.csv")
  moolenaartweets <- readr::read_csv("data/moolenaar.csv")
  freduptontweets <- readr::read_csv("data/fredupton.csv")
  walbergtweets <- readr::read_csv("data/walberg.csv")
  slotkintweets <- readr::read_csv("data/slotkin.csv")
  paulmitchelltweets <- readr::read_csv("data/paulmitchell.csv")
  hagedorntweets <- readr::read_csv("data/hagedorn.csv")
  tomemmertweets <- readr::read_csv("data/tomemmer.csv")
  petestaubertweets <- readr::read_csv("data/petestauber.csv")
  blainetweets <- readr::read_csv("data/blaine.csv")
  samgravestweets <- readr::read_csv("data/samgraves.csv")
  longtweets <- readr::read_csv("data/long.csv")
  jasonsmithtweets <- readr::read_csv("data/jasonsmith.csv")
  trentkellytweets <- readr::read_csv("data/trentkelly.csv")
  michaelguesttweets <- readr::read_csv("data/michaelguest.csv")
  palazzotweets <- readr::read_csv("data/palazzo.csv")
  holdingtweets <- readr::read_csv("data/holding.csv")
  walterjonestweets <- readr::read_csv("data/holding.csv")
  markwalkertweets <- readr::read_csv("data/markwalker.csv")
  davidrouzertweets <- readr::read_csv("data/davidrouzer.csv")
  richhudsontweets <- readr::read_csv("data/richhudson.csv")
  patrickmchenrytweets <- readr::read_csv("data/patrickmchenry.csv")
  markmeadowstweets <- readr::read_csv("data/markmeadows.csv")
  tedbuddtweets <-  readr::read_csv("data/tedbudd.csv")
  jefffortenberrytweets <- readr::read_csv("data/jefffortenberry.csv")
  donbacontweets <- readr::read_csv("data/donbacon.csv")
  adriansmithtweets <- readr::read_csv("data/adriansmith.csv")
  jvdtweets <- readr::read_csv("data/jvd.csv")
  chrissmithtweets <- readr::read_csv("data/chrissmith.csv")
  markamodeitweets <- readr::read_csv("data/markamodei.csv")
  leezeldintweets <- readr::read_csv("data/leezeldin.csv")
  peterkingtweets <- readr::read_csv("data/peterking.csv")
  tomreedtweets <- readr::read_csv("data/tomreed.csv")
  johnkatkotweets <- readr::read_csv("data/johnkatko.csv")
  chriscollinstweets <- readr::read_csv("data/chriscollins.csv")
  stevechabottweets <- readr::read_csv("data/stevechabot.csv")
  bradwenstruptweets <- readr::read_csv("data/bradwenstrup.csv")
  jimjordantweets <- readr::read_csv("data/jimjordan.csv")
  boblattatweets <- readr::read_csv("data/boblatta.csv")
  billjohnsontweets <- readr::read_csv("data/billjohnson.csv")
  bobgibbstweets <- readr::read_csv("data/bobgibbs.csv")
  warrendavidsontweets <- readr::read_csv("data/warrendavidson.csv")
  miketurnertweets <- readr::read_csv("data/miketurner.csv")
  baldersontweets <- readr::read_csv("data/balderson.csv")
  davejoycetweets <- readr::read_csv("data/davejoyce.csv")
  stevestiverstweets <- readr::read_csv("data/stevestivers.csv")
  agonzaleztweets <- readr::read_csv("data/agonzalez.csv")
  kevinherntweets <- readr::read_csv("data/kevinhern.csv")
  mullintweets <- readr::read_csv("data/mullin.csv")
  franklucastweets <- readr::read_csv("data/franklucas.csv")
  tomcoletweets <- readr::read_csv("data/tomcole.csv")
  brianfitztweets <- readr::read_csv("data/brianfitz.csv")
  meusertweets <- readr::read_csv("data/meuser.csv")
  scottperrytweets <- readr::read_csv("data/scottperry.csv")
  smuckertweets <- readr::read_csv("data/smucker.csv")
  johnjoycetweets <- readr::read_csv("data/johnjoyce.csv")
  greschenthalertweets <- readr::read_csv("data/greschenthaler.csv")
  gttweets <- readr::read_csv("data/gt.csv")
  mikekellytweets <- readr::read_csv("data/mikekelly.csv")
  joewilsontweets <- readr::read_csv("data/joewilson.csv")
  jeffduncantweets <-  readr::read_csv("data/jeffduncan.csv")
  timmonstweets <- readr::read_csv("data/timmons.csv")
  ralphnormantweets <- readr::read_csv("data/ralphnorman.csv")
  tomricetweets <- readr::read_csv("data/tomrice.csv")
  dustyjohnsontweets <- readr::read_csv("data/dustyjohnson.csv")
  philroetweets <- readr::read_csv("data/philroe.csv")
  timburchetttweets <- readr::read_csv("data/timburchett.csv")
  chucktweets <-  readr::read_csv("data/chuck.csv")
  desjarlaistweets <- readr::read_csv("data/desjarlais.csv")
  johnrosetweets <- readr::read_csv("data/johnrose.csv")
  markgreentweets <- readr::read_csv("data/markgreen.csv")
  davidkustofftweets <- readr::read_csv("data/davidkustoff.csv")
  louiegohmerttweets <- readr::read_csv("data/louiegohmert.csv")
  dancrenshawtweets <- readr::read_csv("data/dancrenshaw.csv")
  vantaylortweets <- readr::read_csv("data/vantaylor.csv")
  ratcliffetweets <- readr::read_csv("data/ratcliffe.csv")
  lancegoodentweets <- readr::read_csv("data/lancegooden.csv")
  ronwrighttweets <- readr::read_csv("data/ronwright.csv")
  kevinbradytweets <- readr::read_csv("data/kevinbrady.csv")
  mccaultweets <- readr::read_csv("data/mccaul.csv")
  conawaytweets <- readr::read_csv("data/conaway.csv")
  kaygrangertweets <- readr::read_csv("data/kaygranger.csv")
  mactweets <- readr::read_csv("data/mac.csv")
  randytweets <- readr::read_csv("data/randy.csv")
  billflorestweets <- readr::read_csv("data/billflores.csv")
  arringtontweets <- readr::read_csv("data/arrington.csv")
  chiproytweets <- readr::read_csv("data/chiproy.csv")
  peteolsontweets <- readr::read_csv("data/peteolson.csv")
  hurdtweets <- readr::read_csv("data/hurd.csv")
  kenmarchanttweets <- readr::read_csv("data/kenmarchant.csv")
  rwilliamstweets <- readr::read_csv("data/rwilliams.csv")
  michaelburgesstweets <- readr::read_csv("data/michaelburgess.csv")
  cloudtweets <- readr::read_csv("data/cloud.csv")
  cartertweets <- readr::read_csv("data/carter.csv")
  brianbabintweets <- readr::read_csv("data/brianbabin.csv")
  robbishoptweets <- readr::read_csv("data/robbishop.csv")
  chrisstewarttweets <- readr::read_csv("data/chrisstewart.csv")
  johncurtistweets <- readr::read_csv("data/johncurtis.csv")
  robwittmantweets <- readr::read_csv("data/robwittman.csv")
  rigglemantweets <- readr::read_csv("data/riggleman.csv")
  benclinetweets <- readr::read_csv("data/bencline.csv")
  mgriffithtweets <- readr::read_csv("data/mgriffith.csv")
  newhousetweets <- readr::read_csv("data/newhouse.csv")
  bryansteiltweets <- readr::read_csv("data/bryansteil.csv")
  jimtweets <- readr::read_csv("data/jim.csv")
  grothmantweets <- readr::read_csv("data/grothman.csv")
  seanduffytweets <- readr::read_csv("data/seanduffy.csv")
  gallaghertweets <- readr::read_csv("data/gallagher.csv")
  mckinleytweets <- readr::read_csv("data/mckinley.csv")
  alexmooneytweets <- readr::read_csv("data/alexmooney.csv")



  hormrtweets <- dplyr::bind_rows(donyoungtweets %>%
                             dplyr::mutate(person = "Don Young"),
                           byrnetweets %>%
                             dplyr::mutate(person = "Byrne"),
                           mikerogerstweets %>%
                             dplyr::mutate(person = "Mike Rogers"),
                           robertaderholttweets %>%
                             dplyr::mutate(person = "Robert Aderholt"),
                           mobrookstweets %>%
                             dplyr::mutate(person = "Mo Brooks"),
                           garypalmertweets %>%
                             dplyr::mutate(person = "Gary Palmer"),
                           rickcrawfordtweets %>%
                             dplyr::mutate(person = "Rick Crawford"),
                           frenchhilltweets %>%
                             dplyr::mutate(person = "French Hill"),
                           stevewomacktweets %>%
                             dplyr::mutate(person = "Steve Womack"),
                           westermantweets %>%
                             dplyr::mutate(person = "Westerman"),
                           gosartweets %>%
                             dplyr::mutate(person = "Gosar"),
                           andybiggstweets %>%
                             dplyr::mutate(person = "Andy Biggs"),
                           davidtweets %>%
                             dplyr::mutate(person = "David"),
                           dleskotweets %>%
                             dplyr::mutate(person = "D Lesko"),
                           lamalfatweets %>%
                             dplyr::mutate(person = "Lamalfa"),
                           mcclintocktweets %>%
                             dplyr::mutate(person = "McClintock"),
                           paulcooktweets %>%
                             dplyr::mutate(person = "Paul Cook"),
                           devinnunestweets %>%
                             dplyr::mutate(person = "Devin Nunes"),
                           kevinmccarthytweets %>%
                             dplyr::mutate(person = "Kevin McCarthy"),
                           kencalverttweets %>%
                             dplyr::mutate(person = "Ken Calvert"),
                           tiptontweets %>%
                             dplyr::mutate(person = "Tipton"),
                           kenbucktweets %>%
                             dplyr::mutate(person = "Ken Buck"),
                           dlamborntweets %>%
                             dplyr::mutate(person = "D Lamborn"),
                           mattgaetztweets %>%
                             dplyr::mutate(person = "Matt Gaetz"),
                           nealdunntweets %>%
                             dplyr::mutate(person = "Neal Dunn"),
                           tedyohotweets %>%
                             dplyr::mutate(person = "Ted Yoho"),
                           rutherfordtweets %>%
                             dplyr::mutate(person = "Rutherford"),
                           michaelwaltztweets %>%
                             dplyr::mutate(person = "Michael Waltz"),
                           billposeytweets %>%
                             dplyr::mutate(person = "Bill Posey"),
                           webstertweets %>%
                             dplyr::mutate(person = "Webster"),
                           gusbilirakistweets %>%
                             dplyr::mutate(person = "Gus Bilirakis"),
                           vernbuchanantweets %>%
                             dplyr::mutate(person = "Vern Buchanon"),
                           gregsteubetweets %>%
                             dplyr::mutate(person = "Greg Steube"),
                           brianmasttweets %>%
                             dplyr::mutate(person = "Brian Mast"),
                           rooneytweets %>%
                             dplyr::mutate(person = "Rooney"),
                           mariodbtweets %>%
                             dplyr::mutate(person = "Mario DB"),
                           buddycartertweets %>%
                             dplyr::mutate(person = "Buddy Carter"),
                           drewfergusontweets %>%
                             dplyr::mutate(person = "Drew Ferguson"),
                           robwoodalltweets %>%
                             dplyr::mutate(person = "Rob Woodall"),
                           austinscotttweets %>%
                             dplyr::mutate(person = "Austin Scott"),
                           dougcollinstweets %>%
                             dplyr::mutate(person = "Doug Collins"),
                           hicetweets %>%
                             dplyr::mutate(person = "Hice"),
                           loudermilktweets %>%
                             dplyr::mutate(person = "Loudermilk"),
                           rickallentweets %>%
                             dplyr::mutate(person = "Rick Allen"),
                           tomgravestweets %>%
                             dplyr::mutate(person = "Tom Graves"),
                           russfulchertweets %>%
                             dplyr::mutate(person = "Russ Fulcher"),
                           mikesimpsontweets %>%
                             dplyr::mutate(person = "Mike Simpson"),
                           bosttweets %>%
                             dplyr::mutate(person = "Bost"),
                           rodneydavistweets %>%
                             dplyr::mutate(person = "Rodney Davis"),
                           shimkustweets %>%
                             dplyr::mutate(person = "Shimkus"),
                           kinzingertweets %>%
                             dplyr::mutate(person = "Kinzinger"),
                           lahoodtweets %>%
                             dplyr::mutate(person = "Lahood"),
                           jimbankstweets %>%
                             dplyr::mutate(person = "Jim Banks"),
                           jimbairdtweets %>%
                             dplyr::mutate(person = "Jim Baird"),
                           gregpencetweets %>%
                             dplyr::mutate(person = "Greg Pence"),
                           larrybucshontweets %>%
                             dplyr::mutate(person = "Larry Bucschon"),
                           treytweets %>%
                             dplyr::mutate(person = "Trey"),
                           ronestestweets %>%
                             dplyr::mutate(person = "Ron Estes"),
                           guthrietweets %>%
                             dplyr::mutate(person = "Guthrie"),
                           thomasmassietweets %>%
                             dplyr::mutate(person = "Thomas Massie"),
                           halrogerstweets %>%
                             dplyr::mutate(person = "Hal Rogers"),
                           andybarrtweets %>%
                             dplyr::mutate(person = "Andy Barr"),
                           stevescalisetweets %>%
                             dplyr::mutate(person = "Steve Scalise"),
                           clayhigginstweets %>%
                             dplyr::mutate(person = "Clay Higgins"),
                           mikejohnsontweets %>%
                             dplyr::mutate(person = "Mike Johnson"),
                           abrahamtweets %>%
                             dplyr::mutate(person = "Abraham"),
                           garretgravestweets %>%
                             dplyr::mutate(person = "Garret Graves"),
                           andyharristweets %>%
                             dplyr::mutate(person = "Andy Harris"),
                           jackbergmantweets %>%
                             dplyr::mutate(person = "Jack Bergman"),
                           huizengatweets %>%
                             dplyr::mutate(person = "Huizenga"),
                           justinamashtweets %>%
                             dplyr::mutate(person = "Justin Amash"),
                           moolenaartweets %>%
                             dplyr::mutate(person = "Moolenaar"),
                           freduptontweets %>%
                             dplyr::mutate(person = "Fred Upton"),
                           walbergtweets %>%
                             dplyr::mutate(person = "Walberg"),
                           slotkintweets %>%
                             dplyr::mutate(person = "Slotkin"),
                           paulmitchelltweets %>%
                             dplyr::mutate(person = "Paul Mitchell"),
                           hagedorntweets %>%
                             dplyr::mutate(person = "Hagedorn"),
                           tomemmertweets %>%
                             dplyr::mutate(person = "Tom Emmer"),
                           petestaubertweets %>%
                             dplyr::mutate(person = "Peter Stauber"),
                           blainetweets %>%
                             dplyr::mutate(person = "Blaine"),
                           samgravestweets %>%
                             dplyr::mutate(person = "Sam Graves"),
                           longtweets %>%
                             dplyr::mutate(person = "Long"),
                           jasonsmithtweets %>%
                             dplyr::mutate(person = "Jason Smith"),
                           trentkellytweets %>%
                             dplyr::mutate(person = "Trent Kelly"),
                           michaelguesttweets %>%
                             dplyr::mutate(person = "Michael Guest"),
                           palazzotweets %>%
                             dplyr::mutate(person = "Palazzo"),
                           holdingtweets %>%
                             dplyr::mutate(person = "Holding"),
                           walterjonestweets %>%
                             dplyr::mutate(person = "Walter Jones"),
                           markwalkertweets %>%
                             dplyr::mutate(person = "Mark Walker"),
                           davidrouzertweets %>%
                             dplyr::mutate(person = "David Rouzer"),
                           richhudsontweets %>%
                             dplyr::mutate(person = "Rich Hudson"),
                           patrickmchenrytweets %>%
                             dplyr::mutate(person = "Patrick McHenry"),
                           markmeadowstweets %>%
                             dplyr::mutate(person = "Mark Meadows"),
                           tedbuddtweets %>%
                             dplyr::mutate(person = "Ted Budd"),
                           jefffortenberrytweets %>%
                             dplyr::mutate(person = "Jeff Fortenberry"),
                           donbacontweets %>%
                             dplyr::mutate(person = "Don Bacon"),
                           adriansmithtweets %>%
                             dplyr::mutate(person = "Adrian Smith"),
                           jvdtweets %>%
                             dplyr::mutate(person = "JVD"),
                           chrissmithtweets %>%
                             dplyr::mutate(person = "Chris Smith"),
                           markamodeitweets %>%
                             dplyr::mutate(person = "Mark Modei"),
                           leezeldintweets %>%
                             dplyr::mutate(person = "Lee Zeldin"),
                           peterkingtweets %>%
                             dplyr::mutate(person = "Peter King"),
                           tomreedtweets %>%
                             dplyr::mutate(person = "Tom Reed"),
                           johnkatkotweets %>%
                             dplyr::mutate(person = "John Katko"),
                           chriscollinstweets %>%
                             dplyr::mutate(person = "Chris Collins"),
                           stevechabottweets %>%
                             dplyr::mutate(person = "Steve Chabot"),
                           bradwenstruptweets %>%
                             dplyr::mutate(person = "Braden Strup"),
                           jimjordantweets %>%
                             dplyr::mutate(person = "Jim Jordan"),
                           boblattatweets %>%
                             dplyr::mutate(person = "Bob Latta"),
                           billjohnsontweets %>%
                             dplyr::mutate(person = "Bill Johnson"),
                           bobgibbstweets %>%
                             dplyr::mutate(person = "Bob Gibbs"),
                           warrendavidsontweets %>%
                             dplyr::mutate(person = "Warren Davidson"),
                           miketurnertweets %>%
                             dplyr::mutate(person = "Mike Turner"),
                           baldersontweets %>%
                             dplyr::mutate(person = "Balderson"),
                           davejoycetweets %>%
                             dplyr::mutate(person = "Dave Joycet"),
                           stevestiverstweets %>%
                             dplyr::mutate(person = "Steve Stivers"),
                           agonzaleztweets %>%
                             dplyr::mutate(person = "A Gonzalez"),
                           kevinherntweets %>%
                             dplyr::mutate(person = "Kevin Hern"),
                           mullintweets %>%
                             dplyr::mutate(person = "Mullin"),
                           franklucastweets %>%
                             dplyr::mutate(person = "Frank Lucas"),
                           tomcoletweets %>%
                             dplyr::mutate(person = "Tom Cole"),
                           brianfitztweets %>%
                             dplyr::mutate(person = "Brian Fitz"),
                           meusertweets %>%
                             dplyr::mutate(person = "Meuser"),
                           scottperrytweets %>%
                             dplyr::mutate(person = "Scott Perry"),
                           smuckertweets %>%
                             dplyr::mutate(person = "Smucker"),
                           johnjoycetweets %>%
                             dplyr::mutate(person = "John Joyce"),
                           greschenthalertweets %>%
                             dplyr::mutate(person = "Greschenthaler"),
                           gttweets %>%
                             dplyr::mutate(person = "GT"),
                           mikekellytweets %>%
                             dplyr::mutate(person = "Mike Kelly"),
                           joewilsontweets %>%
                             dplyr::mutate(person = "Joe Wilson"),
                           jeffduncantweets %>%
                             dplyr::mutate(person = "Jeff Duncan"),
                           timmonstweets %>%
                             dplyr::mutate(person = "Timmons"),
                           ralphnormantweets %>%
                             dplyr::mutate(person = "Ralph Norman"),
                           tomricetweets %>%
                             dplyr::mutate(person = "Tom Rice"),
                           dustyjohnsontweets %>%
                             dplyr::mutate(person = "Dusty Johnson"),
                           philroetweets %>%
                             dplyr::mutate(person = "Phil Roe"),
                           timburchetttweets %>%
                             dplyr::mutate(person = "Tim Burchett"),
                           chucktweets %>%
                             dplyr::mutate(person = "Chuck"),
                           desjarlaistweets %>%
                             dplyr::mutate(person = "Des Jarlais"),
                           johnrosetweets %>%
                             dplyr::mutate(person = "John Rose"),
                           markgreentweets %>%
                             dplyr::mutate(person = "Mark Green"),
                           davidkustofftweets %>%
                             dplyr::mutate(person = "David Kustoff"),
                           louiegohmerttweets %>%
                             dplyr::mutate(person = "Louie Gohmer"),
                           dancrenshawtweets %>%
                             dplyr::mutate(person = "Dan Crenshaw"),
                           vantaylortweets %>%
                             dplyr::mutate(person = "Van Taylor"),
                           ratcliffetweets %>%
                             dplyr::mutate(person = "Ratcliffe"),
                           lancegoodentweets %>%
                             dplyr::mutate(person = "Lance Gooden"),
                           ronwrighttweets %>%
                             dplyr::mutate(person = "Ron Wright"),
                           kevinbradytweets %>%
                             dplyr::mutate(person = "Kevin Brady"),
                           mccaultweets %>%
                             dplyr::mutate(person = "McCaul"),
                           conawaytweets %>%
                             dplyr::mutate(person = "Conaway"),
                           kaygrangertweets %>%
                             dplyr::mutate(person = "Kay Granger"),
                           mactweets %>%
                             dplyr::mutate(person = "Mac"),
                           randytweets %>%
                             dplyr::mutate(person = "Randy"),
                           billflorestweets %>%
                             dplyr::mutate(person = "Bill Flores"),
                           arringtontweets %>%
                             dplyr::mutate(person = "Arrington"),
                           chiproytweets %>%
                             dplyr::mutate(person = "Chip Roy"),
                           peteolsontweets %>%
                             dplyr::mutate(person = "Pete Olson"),
                           hurdtweets %>%
                             dplyr::mutate(person = "Hurd"),
                           kenmarchanttweets %>%
                             dplyr::mutate(person = "Ken Marchant"),
                           rwilliamstweets %>%
                             dplyr::mutate(person = "R Williams"),
                           michaelburgesstweets %>%
                             dplyr::mutate(person = "Michael Burgess"),
                           cloudtweets %>%
                             dplyr::mutate(person = "Cloud"),
                           cartertweets %>%
                             dplyr::mutate(person = "Carter"),
                           brianbabintweets %>%
                             dplyr::mutate(person = "Brian Babin"),
                           robbishoptweets %>%
                             dplyr::mutate(person = "Robbi Shop"),
                           chrisstewarttweets %>%
                             dplyr::mutate(person = "Chris Stewart"),
                           johncurtistweets %>%
                             dplyr::mutate(person = "John Curtis"),
                           robwittmantweets %>%
                             dplyr::mutate(person = "Rob Wittman"),
                           rigglemantweets %>%
                             dplyr::mutate(person = "Riggleman"),
                           benclinetweets %>%
                             dplyr::mutate(person = "Ben Cline"),
                           mgriffithtweets %>%
                             dplyr::mutate(person = "M Griffith"),
                           newhousetweets %>%
                             dplyr::mutate(person = "Newhouse"),
                           bryansteiltweets %>%
                             dplyr::mutate(person = "Bryan Steil"),
                           jimtweets %>%
                             dplyr::mutate(person = "Jim"),
                           grothmantweets %>%
                             dplyr::mutate(person = "Grothman"),
                           seanduffytweets %>%
                             dplyr::mutate(person = "Sean Duffy"),
                           gallaghertweets %>%
                             dplyr::mutate(person = "Gallagher"),
                           mckinleytweets %>%
                             dplyr::mutate(person = "McKinley"),
                           alexmooneytweets %>%
                             dplyr::mutate(person = "Alex Mooney"))

  save_as_csv(hormrtweets, "data/hormrtweets.csv", prepend_ids = TRUE , na = "", fileEncoding = "UTF-8")
  hormrtweets <- readr::read_csv("data/hormrtweets.csv")
  if(nrow(hormrtweets)>0) {
    message("Check your Data Folder. Function ran successfully")
  }
}

#############################
#--------(8) hormaleD-------#
#############################
#' Grabs the 50 most recent tweets from Male Democrats in the House of Representatives
#' Saves those tweets in individual csv (for each MC) and uses the UTF-8 file encoding for the CSV
#' Creates one dataframe to combine all of the tweets from each MC
#' With the one large dataframe, it creates a CSV to hold the 50 most recent tweets for each Male Democrat in the House of Representatives
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @export

hormaleD <- function() {
  ohalleran <- rtweet::get_timeline('@RepOHalleran', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  raulgrijalva<- rtweet::get_timeline('@RepRaulGrijalva', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rubengallego <- rtweet::get_timeline('@RepRubenGallego', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gregstanton<- rtweet::get_timeline('@RepGregStanton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  garamendi<- rtweet::get_timeline('@RepGaramendi', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  thompson<- rtweet::get_timeline('@RepThompson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mcnerney <- rtweet::get_timeline('@RepMcNerney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joshharder <- rtweet::get_timeline('@RepJoshHarder', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  desaulnier<- rtweet::get_timeline('@RepDeSaulnier', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  swalwell<- rtweet::get_timeline('@RepSwalwell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimcosta <- rtweet::get_timeline('@RepJimCosta', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rokhanna <- rtweet::get_timeline('@RepRoKhanna', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimmypanetta <- rtweet::get_timeline('@RepJimmyPanetta', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tjcox<- rtweet::get_timeline('@RepTjCox', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  carbajal <- rtweet::get_timeline('@RepCarbajal', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  adamschiff <- rtweet::get_timeline('@RepAdamSchiff', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cardenas<- rtweet::get_timeline('@RepCardenas', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bradsherman <- rtweet::get_timeline('@BradSherman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  peteaguilar<- rtweet::get_timeline('@RepPeteAguilar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tedlieu<- rtweet::get_timeline('@RepTedLieu', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimmygomez <- rtweet::get_timeline('@RepJimmyGomez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ruiz<- rtweet::get_timeline('@CongressmanRuiz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gilcisneros <- rtweet::get_timeline('@RepGilCisneros', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  marktakano<- rtweet::get_timeline('@RepMarkTakano', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  loucorrea<- rtweet::get_timeline('@RepLouCorrea', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lowenthal<- rtweet::get_timeline('@RepLowenthal', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  harley<- rtweet::get_timeline('@RepHarley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikelevin <- rtweet::get_timeline('@RepMikeLevin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hunter<- rtweet::get_timeline('@Rep_Hunter', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  juanvargas <- rtweet::get_timeline('@RepJuanVargas', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  scottpeters<- rtweet::get_timeline('@RepScottPeters', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joeneguse<- rtweet::get_timeline('@RepJoeNeguse', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jasoncrow<- rtweet::get_timeline('@RepJasonCrow', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  perlmutter<- rtweet::get_timeline('@RepPerlmutter', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnlarson<- rtweet::get_timeline('@RepJohnLarson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joecourtney<- rtweet::get_timeline('@RepJoeCourtney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jahimes<- rtweet::get_timeline('@jahimes', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  allawson <- rtweet::get_timeline('@RepAlLawsonJr', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  darrensoto <- rtweet::get_timeline('@RepDarrenSoto', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  valdemings<- rtweet::get_timeline('@RepValDemings', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  charliecrist<- rtweet::get_timeline('@RepCharlieCrist', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  teddeutch<- rtweet::get_timeline('@RepTedDeutch', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  sanfordbishop<- rtweet::get_timeline('@SanfordBishop', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hankjohnson<- rtweet::get_timeline('@RepHankJohnson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnlewis<- rtweet::get_timeline('@repjohnlewis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lucymcbath<- rtweet::get_timeline('@RepLucyMcBath', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidscott<- rtweet::get_timeline('@repdavidscott', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  edcase<- rtweet::get_timeline('@RepEdCase', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  daveloebsack <- rtweet::get_timeline('@daveloebsack', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bobbyrush<- rtweet::get_timeline('@RepBobbyRush', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lipinski<- rtweet::get_timeline('@RepLipinski', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chuygarcia <- rtweet::get_timeline('@RepChuyGarcia', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikequigley<- rtweet::get_timeline('@RepMikeQuigley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  casten<- rtweet::get_timeline('@RepCasten', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dannydavis <- rtweet::get_timeline('@RepDannyDavis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  raja<- rtweet::get_timeline('@CongressmanRaja', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  schneider <- rtweet::get_timeline('@RepSchneider', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  billfoster<- rtweet::get_timeline('@RepBillFoster', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  visclosky<- rtweet::get_timeline('@RepVisclosky', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andrecarson<- rtweet::get_timeline('@RepAndreCarson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnyarmuth<- rtweet::get_timeline('@RepJohnYarmuth', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  richmond<- rtweet::get_timeline('@RepRichmond', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chelliepingree <- rtweet::get_timeline('@chelliepingree', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  golden<- rtweet::get_timeline('@RepGolden', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dutch <- rtweet::get_timeline('@Call_Me_Dutch', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  sarbanes <- rtweet::get_timeline('@RepSarbanes', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  anthonybrown <- rtweet::get_timeline('@RepAnthonyBrown', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hoyer<- rtweet::get_timeline('@LeaderHoyer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidtrone<- rtweet::get_timeline('@RepDavidTrone', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cummings<- rtweet::get_timeline('@RepCummings', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  raskin <- rtweet::get_timeline('@RepRaskin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  richardneal <- rtweet::get_timeline('@RepRichardNeal', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mcgovern<- rtweet::get_timeline('@RepMcGovern', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joekennedy <- rtweet::get_timeline('@RepJoeKennedy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  moulton<- rtweet::get_timeline('@teammoulton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stephenlynch <- rtweet::get_timeline('@RepStephenLynch', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  keating<- rtweet::get_timeline('@USRepKeating', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dankildee <- rtweet::get_timeline('@RepDanKildee', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andylevin<- rtweet::get_timeline('@RepAndyLevin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cleaver<- rtweet::get_timeline('@repcleaver', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  benniethompson <- rtweet::get_timeline('@BennieGThompson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gkbutterfield<- rtweet::get_timeline('@GKButterfield', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidprice<- rtweet::get_timeline('@RepDavidEPrice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chrispappas<- rtweet::get_timeline('@RepChrisPappas', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  donaldnorcross<- rtweet::get_timeline('@DonaldNorcross', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andykim<- rtweet::get_timeline('@RepAndyKimNJ', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joshg <- rtweet::get_timeline('@RepJoshG', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  frankpallone <- rtweet::get_timeline('@FrankPallone', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  malinowski<- rtweet::get_timeline('@RepMalinowski', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  sires<- rtweet::get_timeline('@RepSires', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  billpascrell <- rtweet::get_timeline('@BillPascrell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  donaldpayne<- rtweet::get_timeline('@RepDonaldPayne', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  sherrill<- rtweet::get_timeline('@RepSherrill', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  benraylujan <- rtweet::get_timeline('@repbenraylujan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  horsford<- rtweet::get_timeline('@RepHorsford', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomsuozzi <- rtweet::get_timeline('@RepTomSuozzi', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gregorymeeks<- rtweet::get_timeline('@RepGregoryMeeks', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gracemeng<- rtweet::get_timeline('@RepGraceMeng', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jeffries<- rtweet::get_timeline('@RepJeffries', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jerrynadler <- rtweet::get_timeline('@RepJerryNadler', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  maxrose<- rtweet::get_timeline('@RepMaxRose', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  espaillat <- rtweet::get_timeline('@RepEspaillat', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joseserrano<- rtweet::get_timeline('@RepJoseSerrano', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  eliotengel<- rtweet::get_timeline('@RepEliotEngel', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  seanmaloney<- rtweet::get_timeline('@RepSeanMaloney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  delgado<- rtweet::get_timeline('@repdelgado', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  paultonko <- rtweet::get_timeline('@RepPaulTonko', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brindisi<- rtweet::get_timeline('@RepBrindisi', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joemorelle <- rtweet::get_timeline('@RepJoeMorelle', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brianhiggins<- rtweet::get_timeline('@RepBrianHiggins', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  timryan<- rtweet::get_timeline('@RepTimRyan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gregwalden <- rtweet::get_timeline('@repgregwalden', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  blumenauer<- rtweet::get_timeline('@repblumenauer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  peterdefazio<- rtweet::get_timeline('@RepPeterDeFazio', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  schrader<- rtweet::get_timeline('@RepSchrader', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  boyle <- rtweet::get_timeline('@CongBoyle', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dwightevans <- rtweet::get_timeline('@RepDwightEvans', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cartwright<- rtweet::get_timeline('@RepCartwright', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  conorlamb<- rtweet::get_timeline('@RepConorLamb', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikedoyle<- rtweet::get_timeline('@USRepMikeDoyle', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidcicilline<- rtweet::get_timeline('@davidcicilline', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimlangevin<- rtweet::get_timeline('@JimLangevin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cunningham<- rtweet::get_timeline('@RepCunningham', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  clyburn<- rtweet::get_timeline('@WhipClyburn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimcooper <- rtweet::get_timeline('@repjimcooper', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cohen<- rtweet::get_timeline('@RepCohen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  algreen <- rtweet::get_timeline('@RepAlGreen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gonzalez <- rtweet::get_timeline('@RepGonzalez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joaquincastro <- rtweet::get_timeline('@JoaquinCastrotx', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cuellar<- rtweet::get_timeline('@RepCuellar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ebj <- rtweet::get_timeline('@RepEBJ', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  colinallred <- rtweet::get_timeline('@RepColinAllred', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  veasey<- rtweet::get_timeline('@RepVeasey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  filemonvela <- rtweet::get_timeline('@RepFilemonVela', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lloyddoggett<- rtweet::get_timeline('@RepLloydDoggett', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  benmcadams<- rtweet::get_timeline('@RepBenMcAdams', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bobbyscott<- rtweet::get_timeline('@BobbyScott', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mceachin<- rtweet::get_timeline('@RepMcEachin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  donbeyer <- rtweet::get_timeline('@RepDonBeyer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gerryconnolly <- rtweet::get_timeline('@GerryConnolly', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  peterwelch<- rtweet::get_timeline('@PeterWelch', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ricklarsen<- rtweet::get_timeline('@RepRickLarsen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  derekkilmer<- rtweet::get_timeline('@RepDerekKilmer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  adamsmith<- rtweet::get_timeline('@RepAdamSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dennyheck<- rtweet::get_timeline('@RepDennyHeck', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  markpocan<- rtweet::get_timeline('@repmarkpocan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ronkind<- rtweet::get_timeline('@RepRonKind', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  carolmiller <- rtweet::get_timeline('@RepCarolMiller', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)











  save_as_csv(ohalleran,"data/ohalleran.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(raulgrijalva,"data/raulgrijalva.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(rubengallego,"data/rubengallego.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(gregstanton,"data/gregstanton.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(garamendi,"data/garamendi.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(thompson,"data/thompson.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(mcnerney,"data/mcnerney.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(joshharder,"data/joshharder.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(desaulnier,"data/desaulnier.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(swalwell,"data/swalwell.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(jimcosta,"data/jimcosta.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(rokhanna,"data/rokhanna.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(jimmypanetta,"data/jimmypanetta.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(tjcox,"data/tjcox.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(carbajal,"data/carbajal.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(adamschiff,"data/adamschiff.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(cardenas,"data/cardenas.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(bradsherman,"data/bradsherman.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(peteaguilar,"data/peteaguilar.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(tedlieu,"data/tedlieu.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(jimmygomez,"data/jimmygomez.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(ruiz,"data/ruiz.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(gilcisneros,"data/gilcisneros.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(marktakano,"data/marktakano.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(loucorrea,"data/loucorrea.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(lowenthal,"data/lowenthal.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(harley,"data/harley.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(mikelevin,"data/mikelevin.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(hunter,"data/hunter.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(juanvargas,"data/juanvargas.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(scottpeters,"data/scottpeters.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(joeneguse,"data/joeneguse.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(jasoncrow,"data/jasoncrow.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(perlmutter,"data/perlmutter.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(johnlarson,"data/johnlarson.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(joecourtney,"data/joecourtney.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(jahimes,"data/jahimes.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(allawson,"data/allawson.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(darrensoto,"data/darrensoto.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(valdemings,"data/valdemings.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(charliecrist,"data/charliecrist.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(teddeutch,"data/teddeutch.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(sanfordbishop,"data/sanfordbishop.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(hankjohnson,"data/hankjohnson.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(johnlewis,"data/johnlewis.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(lucymcbath,"data/lucymcbath.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(davidscott,"data/davidscott.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(edcase,"data/edcase.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(daveloebsack,"data/daveloebsack.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(bobbyrush,"data/bobbyrush.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(lipinski,"data/lipinski.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(chuygarcia,"data/chuygarcia.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(mikequigley,"data/mikequigley.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(casten,"data/casten.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(dannydavis,"data/dannydavis.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(raja,"data/raja.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(schneider,"data/schneider.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(billfoster,"data/billfoster.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(visclosky,"data/visclosky.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(andrecarson,"data/andrecarson.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(johnyarmuth,"data/johnyarmuth.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(richmond,"data/richmond.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(chelliepingree,"data/chelliepingree.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(golden,"data/golden.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(dutch,"data/dutch.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(sarbanes,"data/sarbanes.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(anthonybrown,"data/anthonybrown.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(hoyer,"data/hoyer.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(davidtrone,"data/davidtrone.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(cummings,"data/cummings.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(raskin,"data/raskin.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(richardneal,"data/richardneal.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(mcgovern,"data/mcgovern.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(joekennedy,"data/joekennedy.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(moulton,"data/moulton.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(stephenlynch,"data/stephenlynch.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(keating,"data/keating.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(dankildee,"data/dankildee.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(andylevin,"data/andylevin.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(cleaver,"data/cleaver.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(benniethompson,"data/benniethompson.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(gkbutterfield,"data/gkbutterfield.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(davidprice,"data/davidprice.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(chrispappas,"data/chrispappas.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(donaldnorcross,"data/donaldnorcross.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(andykim,"data/andykim.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(joshg,"data/joshg.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(frankpallone,"data/frankpallone.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(malinowski,"data/malinowski.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(sires,"data/sires.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(billpascrell,"data/billpascrell.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(donaldpayne,"data/donaldpayne.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(sherrill,"data/sherrill.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(benraylujan,"data/benraylujan.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(horsford,"data/horsford.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(tomsuozzi,"data/tomsuozzi.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(gregorymeeks,"data/gregorymeeks.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(gracemeng,"data/gracemeng.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(jeffries,"data/jeffries.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(jerrynadler,"data/jerrynadler.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(maxrose,"data/maxrose.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(espaillat,"data/espaillat.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(joseserrano,"data/joseserrano.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(eliotengel,"data/eliotengel.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(seanmaloney,"data/seanmaloney.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(delgado,"data/delgado.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(paultonko,"data/paultonko.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(brindisi,"data/brindisi.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(joemorelle,"data/joemorelle.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(brianhiggins,"data/brianhiggins.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(timryan,"data/timryan.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(gregwalden,"data/gregwalden.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(blumenauer,"data/blumenauer.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(peterdefazio,"data/peterdefazio.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(schrader,"data/schrader.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(boyle,"data/boyle.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(dwightevans,"data/dwightevans.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(cartwright,"data/cartwright.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(conorlamb,"data/conorlamb.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(mikedoyle,"data/mikedoyle.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(davidcicilline,"data/davidcicilline.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(jimlangevin,"data/jimlangevin.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(cunningham,"data/cunningham.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(clyburn,"data/clyburn.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(jimcooper,"data/jimcooper.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(cohen,"data/cohen.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(algreen,"data/algreen.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(gonzalez,"data/gonzalez.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(joaquincastro,"data/joaquincastro.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(cuellar,"data/cuellar.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(ebj,"data/ebj.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(colinallred,"data/colinallred.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(veasey,"data/veasey.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(filemonvela,"data/filemonvela.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(lloyddoggett,"data/lloyddoggett.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(benmcadams,"data/benmcadams.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(bobbyscott,"data/bobbyscott.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(mceachin,"data/mceachin.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(donbeyer,"data/donbeyer.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(gerryconnolly,"data/gerryconnolly.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(peterwelch,"data/peterwelch.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(ricklarsen,"data/ricklarsen.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(derekkilmer,"data/derekkilmer.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(adamsmith,"data/adamsmith.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(dennyheck,"data/dennyheck.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(markpocan,"data/markpocan.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(ronkind,"data/ronkind.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")
  save_as_csv(carolmiller,"data/carolmiller.csv",prepend_ids = TRUE, na="", fileEncoding = "UTF-8")


  ohallerantweets <- readr::read_csv("data/ohalleran.csv")
  raulgrijalvatweets <- readr::read_csv("data/raulgrijalva.csv")
  rubengallegotweets <- readr::read_csv("data/rubengallego.csv")
  gregstantontweets <- readr::read_csv("data/gregstanton.csv")
  garamendtweets <- readr::read_csv("data/garamendi.csv")
  thompsontweets <- readr::read_csv("data/thompson.csv")
  mcnerneytweets <- readr::read_csv("data/mcnerney.csv")
  joshhardertweets <- readr::read_csv("data/joshharder.csv")
  desaulniertweets <- readr::read_csv("data/desaulnier.csv")
  swalwelltweets <- readr::read_csv("data/swalwell.csv")
  jimcostatweets <- readr::read_csv("data/jimcosta.csv")
  rokhannatweets <- readr::read_csv("data/rokhanna.csv")
  jimmypanettatweets <- readr::read_csv("data/jimmypanetta.csv")
  tjcoxtweets <- readr::read_csv("data/tjcox.csv")
  carbajaltweets <- readr::read_csv("data/carbajal.csv")
  adamschifftweets <- readr::read_csv("data/adamschiff.csv")
  cardenastweets <- readr::read_csv("data/cardenas.csv")
  bradshermantweets <- readr::read_csv("data/bradsherman.csv")
  peteaguilartweets <- readr::read_csv("data/peteaguilar.csv")
  tedlieutweets <- readr::read_csv("data/tedlieu.csv")
  jimmygomeztweets <- readr::read_csv("data/jimmygomez.csv")
  ruiztweets <- readr::read_csv("data/ruiz.csv")
  gilcisnerostweets <- readr::read_csv("data/gilcisneros.csv")
  marktakanotweets <- readr::read_csv("data/marktakano.csv")
  loucorreatweets <- readr::read_csv("data/loucorrea.csv")
  lowenthaltweets <- readr::read_csv("data/lowenthal.csv")
  harleytweets <- readr::read_csv("data/harley.csv")
  mikelevintweets <- readr::read_csv("data/mikelevin.csv")
  huntertweets <- readr::read_csv("data/hunter.csv")
  juanvargastweets <- readr::read_csv("data/juanvargas.csv")
  scottpeterstweets <- readr::read_csv("data/scottpeters.csv")
  joenegusetweets <- readr::read_csv("data/joeneguse.csv")
  jasoncrowtweets <- readr::read_csv("data/jasoncrow.csv")
  perlmuttertweets <- readr::read_csv("data/perlmutter.csv")
  johnlarsontweets <- readr::read_csv("data/johnlarson.csv")
  joecourtneytweets <- readr::read_csv("data/joecourtney.csv")
  jahimestweets <- readr::read_csv("data/jahimes.csv")
  allawsontweets <- readr::read_csv("data/allawson.csv")
  darrensototweets <- readr::read_csv("data/darrensoto.csv")
  valdemingstweets <- readr::read_csv("data/valdemings.csv")
  charliecristtweets <- readr::read_csv("data/charliecrist.csv")
  teddeutchtweets <- readr::read_csv("data/teddeutch.csv")
  sanfordbishoptweets <- readr::read_csv("data/sanfordbishop.csv")
  hankjohnsontweets <- readr::read_csv("data/hankjohnson.csv")
  johnlewistweets <- readr::read_csv("data/johnlewis.csv")
  lucymcbathtweets <- readr::read_csv("data/lucymcbath.csv")
  davidscotttweets <- readr::read_csv("data/davidscott.csv")
  edcasetweets <- readr::read_csv("data/edcase.csv")
  daveloebsacktweets <- readr::read_csv("data/daveloebsack.csv")
  bobbyrushtweets <- readr::read_csv("data/bobbyrush.csv")
  lipinskitweets <- readr::read_csv("data/lipinski.csv")
  chuygarciatweets <- readr::read_csv("data/chuygarcia.csv")
  mikequigleytweets <- readr::read_csv("data/mikequigley.csv")
  castentweets <- readr::read_csv("data/casten.csv")
  dannydavistweets <- readr::read_csv("data/dannydavis.csv")
  rajatweets <- readr::read_csv("data/raja.csv")
  schneidertweets <- readr::read_csv("data/schneider.csv")
  billfostertweets <- readr::read_csv("data/billfoster.csv")
  viscloskytweets <- readr::read_csv("data/visclosky.csv")
  andrecarsontweets <- readr::read_csv("data/andrecarson.csv")
  johnyarmuthtweets <- readr::read_csv("data/johnyarmuth.csv")
  richmondtweets <- readr::read_csv("data/richmond.csv")
  chelliepingreetweets <- readr::read_csv("data/chelliepingree.csv")
  goldentweets <- readr::read_csv("data/golden.csv")
  dutchtweets <- readr::read_csv("data/dutch.csv")
  sarbanestweets <- readr::read_csv("data/sarbanes.csv")
  anthonybrowntweets <- readr::read_csv("data/anthonybrown.csv")
  hoyertweets <- readr::read_csv("data/hoyer.csv")
  davidtronetweets <- readr::read_csv("data/davidtrone.csv")
  cummingstweets <- readr::read_csv("data/cummings.csv")
  raskintweets <- readr::read_csv("data/raskin.csv")
  richardnealtweets <- readr::read_csv("data/richardneal.csv")
  mcgoverntweets <- readr::read_csv("data/mcgovern.csv")
  joekennedytweets <- readr::read_csv("data/joekennedy.csv")
  moultontweets <- readr::read_csv("data/moulton.csv")
  stephenlynchtweets <- readr::read_csv("data/stephenlynch.csv")
  keatingtweets <- readr::read_csv("data/keating.csv")
  dankildeetweets <- readr::read_csv("data/dankildee.csv")
  andylevintweets <- readr::read_csv("data/andylevin.csv")
  cleavertweets <- readr::read_csv("data/cleaver.csv")
  benniethompsontweets <- readr::read_csv("data/benniethompson.csv")
  gkbutterfieldtweets <- readr::read_csv("data/gkbutterfield.csv")
  davidpricetweets <- readr::read_csv("data/davidprice.csv")
  chrispappastweets <- readr::read_csv("data/chrispappas.csv")
  donaldnorcrosstweets <- readr::read_csv("data/donaldnorcross.csv")
  andykimtweets <- readr::read_csv("data/andykim.csv")
  joshgtweets <- readr::read_csv("data/joshg.csv")
  frankpallonetweets <- readr::read_csv("data/frankpallone.csv")
  malinowskitweets <- readr::read_csv("data/malinowski.csv")
  sirestweets <- readr::read_csv("data/sires.csv")
  billpascrelltweets <- readr::read_csv("data/billpascrell.csv")
  donaldpaynetweets <- readr::read_csv("data/donaldpayne.csv")
  sherrilltweets <- readr::read_csv("data/sherrill.csv")
  benraylujantweets <- readr::read_csv("data/benraylujan.csv")
  horsfordtweets <- readr::read_csv("data/horsford.csv")
  tomsuozzitweets <- readr::read_csv("data/tomsuozzi.csv")
  gregorymeekstweets <- readr::read_csv("data/gregorymeeks.csv")
  gracemengtweets <- readr::read_csv("data/gracemeng.csv")
  jeffriestweets <- readr::read_csv("data/jeffries.csv")
  jerrynadlertweets <- readr::read_csv("data/jerrynadler.csv")
  maxrosetweets <- readr::read_csv("data/maxrose.csv")
  espaillattweets <- readr::read_csv("data/espaillat.csv")
  joseserranotweets <- readr::read_csv("data/joseserrano.csv")
  eliotengeltweets <- readr::read_csv("data/eliotengel.csv")
  seanmaloneytweets <- readr::read_csv("data/seanmaloney.csv")
  delgadotweets <- readr::read_csv("data/delgado.csv")
  paultonkotweets <- readr::read_csv("data/paultonko.csv")
  brindisitweets <- readr::read_csv("data/brindisi.csv")
  joemorelletweets <- readr::read_csv("data/joemorelle.csv")
  brianhigginstweets <- readr::read_csv("data/brianhiggins.csv")
  timryantweets <- readr::read_csv("data/timryan.csv")
  gregwaldentweets <- readr::read_csv("data/gregwalden.csv")
  blumenauertweets <- readr::read_csv("data/blumenauer.csv")
  peterdefaziotweets <- readr::read_csv("data/peterdefazio.csv")
  schradertweets <- readr::read_csv("data/schrader.csv")
  boyletweets <- readr::read_csv("data/boyle.csv")
  dwightevanstweets <- readr::read_csv("data/dwightevans.csv")
  cartwrighttweets <- readr::read_csv("data/cartwright.csv")
  conorlambtweets <- readr::read_csv("data/conorlamb.csv")
  mikedoyletweets <- readr::read_csv("data/mikedoyle.csv")
  davidcicillinetweets <- readr::read_csv("data/davidcicilline.csv")
  jimlangevintweets <- readr::read_csv("data/jimlangevin.csv")
  cunninghamtweets <- readr::read_csv("data/cunningham.csv")
  clyburntweets <- readr::read_csv("data/clyburn.csv")
  jimcoopertweets <- readr::read_csv("data/jimcooper.csv")
  cohentweets <- readr::read_csv("data/cohen.csv")
  algreentweets <- readr::read_csv("data/algreen.csv")
  gonzaleztweets <- readr::read_csv("data/gonzalez.csv")
  joaquincastrotweets <- readr::read_csv("data/joaquincastro.csv")
  cuellartweets <- readr::read_csv("data/cuellar.csv")
  ebjtweets <- readr::read_csv("data/ebj.csv" )
  colinallredtweets <- readr::read_csv("data/colinallred.csv")
  veaseytweets <- readr::read_csv("data/veasey.csv")
  filemonvelatweets <- readr::read_csv("data/filemonvela.csv")
  lloyddoggetttweets <- readr::read_csv("data/lloyddoggett.csv")
  benmcadamstweets <- readr::read_csv("data/benmcadams.csv")
  bobbyscotttweets <- readr::read_csv("data/bobbyscott.csv")
  mceachintweets <- readr::read_csv("data/mceachin.csv")
  donbeyertweets <- readr::read_csv("data/donbeyer.csv")
  gerryconnollytweets <- readr::read_csv("data/gerryconnolly.csv")
  peterwelchtweets <- readr::read_csv("data/peterwelch.csv")
  ricklarsentweets <- readr::read_csv("data/ricklarsen.csv")
  derekkilmertweets <- readr::read_csv("data/derekkilmer.csv")
  adamsmithtweets <- readr::read_csv("data/adamsmith.csv")
  dennyhecktweets <- readr::read_csv("data/dennyheck.csv")
  markpocantweets <- readr::read_csv("data/markpocan.csv")
  ronkindtweets <- readr::read_csv("data/ronkind.csv")
  carolmillertweets <- readr::read_csv("data/carolmiller.csv")


  hormdtweets <- dplyr::bind_rows(ohallerantweets %>%
                             dplyr::mutate(person = "OHalleran"),
                           raulgrijalvatweets %>%
                             dplyr::mutate(person = "Raul Grijalva"),
                           rubengallegotweets %>%
                             dplyr::mutate(person = "Ruben Gallego"),
                           gregstantontweets %>%
                             dplyr::mutate(person = "Greg Stanton"),
                           garamendtweets %>%
                             dplyr::mutate(person = "Garamend"),
                           thompsontweets %>%
                             dplyr::mutate(person = "Thompson"),
                           mcnerneytweets %>%
                             dplyr::mutate(person = "McNerney"),
                           joshhardertweets %>%
                             dplyr::mutate(person = "Josh Hardert"),
                           desaulniertweets %>%
                             dplyr::mutate(person = "DeSaulnier"),
                           swalwelltweets %>%
                             dplyr::mutate(person = "Swalwell"),
                           jimcostatweets %>%
                             dplyr::mutate(person = "Jim Costa"),
                           rokhannatweets %>%
                             dplyr::mutate(person = "Rokhanna"),
                           jimmypanettatweets %>%
                             dplyr::mutate(person = "Jimmy Panetta"),
                           tjcoxtweets %>%
                             dplyr::mutate(person = "Tj Cox"),
                           carbajaltweets %>%
                             dplyr::mutate(person = "Carbajal"),
                           adamschifftweets %>%
                             dplyr::mutate(person = "Adam Schiff"),
                           cardenastweets %>%
                             dplyr::mutate(person = "Cardenas"),
                           bradshermantweets %>%
                             dplyr::mutate(person = "Brad Sherman"),
                           peteaguilartweets %>%
                             dplyr::mutate(person = "Pete Aguilar"),
                           tedlieutweets %>%
                             dplyr::mutate(person = "Ted Lieu"),
                           jimmygomeztweets %>%
                             dplyr::mutate(person = "Jimmy Gomez"),
                           ruiztweets %>%
                             dplyr::mutate(person = "Ruiz"),
                           gilcisnerostweets %>%
                             dplyr::mutate(person = "Gil Cisneros"),
                           marktakanotweets %>%
                             dplyr::mutate(person = "Mark Takano"),
                           loucorreatweets %>%
                             dplyr::mutate(person = "Lou Correa"),
                           lowenthaltweets %>%
                             dplyr::mutate(person = "Lownthal"),
                           harleytweets %>%
                             dplyr::mutate(person = "Harley"),
                           mikelevintweets %>%
                             dplyr::mutate(person = "Mike Levin"),
                           huntertweets %>%
                             dplyr::mutate(person = "Hunter"),
                           juanvargastweets %>%
                             dplyr::mutate(person = "Juan Vargas"),
                           scottpeterstweets %>%
                             dplyr::mutate(person = "Scott Peters"),
                           joenegusetweets %>%
                             dplyr::mutate(person = "Joe Neguse"),
                           jasoncrowtweets %>%
                             dplyr::mutate(person = "Jason Crow"),
                           perlmuttertweets %>%
                             dplyr::mutate(person = "Perlmutter"),
                           johnlarsontweets %>%
                             dplyr::mutate(person = "John Larson"),
                           joecourtneytweets %>%
                             dplyr::mutate(person = "Joe Courtney"),
                           jahimestweets %>%
                             dplyr::mutate(person = "Jahimes"),
                           allawsontweets %>%
                             dplyr::mutate(person = "Al Lawson"),
                           darrensototweets %>%
                             dplyr::mutate(person = "Darren Soto"),
                           valdemingstweets %>%
                             dplyr::mutate(person = "Val Demings"),
                           charliecristtweets %>%
                             dplyr::mutate(person = "Charlie Crist"),
                           teddeutchtweets %>%
                             dplyr::mutate(person = "Ted Deutch"),
                           sanfordbishoptweets %>%
                             dplyr::mutate(person = "Sanford Bishop"),
                           hankjohnsontweets %>%
                             dplyr::mutate(person = "Hank Johnson"),
                           johnlewistweets %>%
                             dplyr::mutate(person = "John Lewis"),
                           lucymcbathtweets %>%
                             dplyr::mutate(person = "Lucy McBath"),
                           davidscotttweets %>%
                             dplyr::mutate(person = "David Scott"),
                           edcasetweets %>%
                             dplyr::mutate(person = "Ed Case"),
                           daveloebsacktweets %>%
                             dplyr::mutate(person = "Dave Loebsack"),
                           bobbyrushtweets %>%
                             dplyr::mutate(person = "Bobby Rush"),
                           lipinskitweets %>%
                             dplyr::mutate(person = "Lipinski"),
                           chuygarciatweets %>%
                             dplyr::mutate(person = "Chuy Garcia"),
                           mikequigleytweets %>%
                             dplyr::mutate(person = "Mike Quigley"),
                           castentweets %>%
                             dplyr::mutate(person = "Casten"),
                           dannydavistweets %>%
                             dplyr::mutate(person = "Danny Davis"),
                           rajatweets %>%
                             dplyr::mutate(person = "Raja"),
                           schneidertweets %>%
                             dplyr::mutate(person = "Schneider"),
                           billfostertweets %>%
                             dplyr::mutate(person = "Bill Foster"),
                           viscloskytweets %>%
                             dplyr::mutate(person = "Visclosky"),
                           andrecarsontweets %>%
                             dplyr::mutate(person = "Andre Carson"),
                           johnyarmuthtweets %>%
                             dplyr::mutate(person = "Johny Armuth"),
                           richmondtweets %>%
                             dplyr::mutate(person = "Richmond"),
                           chelliepingreetweets %>%
                             dplyr::mutate(person = "Chellipingree"),
                           goldentweets %>%
                             dplyr::mutate(person = "Golden"),
                           dutchtweets %>%
                             dplyr::mutate(person = "Dutch"),
                           sarbanestweets %>%
                             dplyr::mutate(person = "Sarbanes"),
                           anthonybrowntweets %>%
                             dplyr::mutate(person = "Anthony Brown"),
                           hoyertweets %>%
                             dplyr::mutate(person = "Hoyer"),
                           davidtronetweets %>%
                             dplyr::mutate(person = "David Trone"),
                           cummingstweets %>%
                             dplyr::mutate(person = "Cummings"),
                           raskintweets %>%
                             dplyr::mutate(person = "Raskin"),
                           richardnealtweets %>%
                             dplyr::mutate(person = "Richard Neal"),
                           mcgoverntweets %>%
                             dplyr::mutate(person = "McGovern"),
                           joekennedytweets %>%
                             dplyr::mutate(person = "Joe Kennedy"),
                           moultontweets %>%
                             dplyr::mutate(person = "Moulton"),
                           stephenlynchtweets %>%
                             dplyr::mutate(person = "Stephen Lynch"),
                           keatingtweets %>%
                             dplyr::mutate(person = "Keating"),
                           dankildeetweets %>%
                             dplyr::mutate(person = "Dan Kildee"),
                           andylevintweets %>%
                             dplyr::mutate(person = "Andy Levin"),
                           cleavertweets %>%
                             dplyr::mutate(person = "Cleaver"),
                           benniethompsontweets %>%
                             dplyr::mutate(person = "Bennie Thompson"),
                           gkbutterfieldtweets %>%
                             dplyr::mutate(person = "GK Butterfield"),
                           davidpricetweets %>%
                             dplyr::mutate(person = "David Price"),
                           chrispappastweets %>%
                             dplyr::mutate(person = "Chris Pappas"),
                           donaldnorcrosstweets %>%
                             dplyr::mutate(person = "Donald Norcross"),
                           andykimtweets %>%
                             dplyr::mutate(person = "Andy Kim"),
                           joshgtweets %>%
                             dplyr::mutate(person = "Josh G"),
                           frankpallonetweets %>%
                             dplyr::mutate(person = "Frank Pallone"),
                           malinowskitweets %>%
                             dplyr::mutate(person = "Malinowski"),
                           sirestweets %>%
                             dplyr::mutate(person = "Sires"),
                           billpascrelltweets %>%
                             dplyr::mutate(person = "Bill Pascrell"),
                           donaldpaynetweets %>%
                             dplyr::mutate(person = "Donald Payne"),
                           sherrilltweets %>%
                             dplyr::mutate(person = "Sherrill"),
                           benraylujantweets %>%
                             dplyr::mutate(person = "Ben Raylujan"),
                           horsfordtweets %>%
                             dplyr::mutate(person = "Horsford"),
                           tomsuozzitweets %>%
                             dplyr::mutate(person = "Tom Suouzzi"),
                           gregorymeekstweets %>%
                             dplyr::mutate(person = "Gregory Meeks"),
                           gracemengtweets %>%
                             dplyr::mutate(person = "Grace Meng"),
                           jeffriestweets %>%
                             dplyr::mutate(person = "Jeffries"),
                           jerrynadlertweets %>%
                             dplyr::mutate(person = "Jerry Nadler"),
                           maxrosetweets %>%
                             dplyr::mutate(person = "Max Rose"),
                           espaillattweets %>%
                             dplyr::mutate(person = "Espailla"),
                           joseserranotweets %>%
                             dplyr::mutate(person = "Jose Serrano"),
                           eliotengeltweets %>%
                             dplyr::mutate(person = "Eli Otengel"),
                           seanmaloneytweets %>%
                             dplyr::mutate(person = "Sean Maloney"),
                           delgadotweets %>%
                             dplyr::mutate(person = "Delgado"),
                           paultonkotweets %>%
                             dplyr::mutate(person = "Paul Tonko"),
                           brindisitweets %>%
                             dplyr::mutate(person = "Brindisi"),
                           joemorelletweets %>%
                             dplyr::mutate(person = "Joe Morelle"),
                           brianhigginstweets %>%
                             dplyr::mutate(person = "Brian Higgins"),
                           timryantweets %>%
                             dplyr::mutate(person = "Tim Ryan"),
                           gregwaldentweets %>%
                             dplyr::mutate(person = "Greg Walden"),
                           blumenauertweets %>%
                             dplyr::mutate(person = "Blumenauer"),
                           peterdefaziotweets %>%
                             dplyr::mutate(person = "Peter DeFazio"),
                           schradertweets %>%
                             dplyr::mutate(person = "Schrader"),
                           boyletweets %>%
                             dplyr::mutate(person = "Boyle"),
                           dwightevanstweets %>%
                             dplyr::mutate(person = "Dwight Evans"),
                           cartwrighttweets %>%
                             dplyr::mutate(person = "Cartwright"),
                           conorlambtweets %>%
                             dplyr::mutate(person = "Conor Lamb"),
                           mikedoyletweets %>%
                             dplyr::mutate(person = "Mike Doyle"),
                           davidcicillinetweets %>%
                             dplyr::mutate(person = "David Cicilline"),
                           jimlangevintweets %>%
                             dplyr::mutate(person = "Jim Langevin"),
                           cunninghamtweets %>%
                             dplyr::mutate(person = "Cunningham"),
                           clyburntweets %>%
                             dplyr::mutate(person = "Clyburn"),
                           jimcoopertweets %>%
                             dplyr::mutate(person = "Jim Cooper"),
                           cohentweets %>%
                             dplyr::mutate(person = "Cohen"),
                           algreentweets %>%
                             dplyr::mutate(person = "Al Green"),
                           gonzaleztweets %>%
                             dplyr::mutate(person = "Gonzalez"),
                           joaquincastrotweets %>%
                             dplyr::mutate(person = "Joaquin Castro"),
                           cuellartweets %>%
                             dplyr::mutate(person = "Cuellar"),
                           ebjtweets %>%
                             dplyr::mutate(person = "EBJ"),
                           colinallredtweets %>%
                             dplyr::mutate(person = "Colin Allred"),
                           veaseytweets %>%
                             dplyr::mutate(person = "Veasey"),
                           filemonvelatweets %>%
                             dplyr::mutate(person = "File Monvela"),
                           lloyddoggetttweets %>%
                             dplyr::mutate(person = "Lloyd Doggett"),
                           benmcadamstweets %>%
                             dplyr::mutate(person = "Ben McAdams"),
                           bobbyscotttweets %>%
                             dplyr::mutate(person = "Bobby Scott"),
                           mceachintweets %>%
                             dplyr::mutate(person = "McEachin"),
                           donbeyertweets %>%
                             dplyr::mutate(person = "Don Beyer"),
                           gerryconnollytweets %>%
                             dplyr::mutate(person = "Gerry Connolly"),
                           peterwelchtweets %>%
                             dplyr::mutate(person = "Peter Welch"),
                           ricklarsentweets %>%
                             dplyr::mutate(person = "Rick Larson"),
                           derekkilmertweets %>%
                             dplyr::mutate(person = "Derek Kilmer"),
                           adamsmithtweets %>%
                             dplyr::mutate(person = "Adam Smith"),
                           dennyhecktweets %>%
                             dplyr::mutate(person = "Denny Heck"),
                           markpocantweets %>%
                             dplyr::mutate(person = "Mark Pocan"),
                           ronkindtweets %>%
                             dplyr::mutate(person = "Ron Kind"),
                           carolmillertweets %>%
                             dplyr::mutate(person = "Carol Miller"))

  save_as_csv(hormdtweets, "data/hormdtweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
  hormdtweets <- readr::read_csv("data/hormdtweets.csv")
  if(nrow(hormdtweets)>0) {
    message("Check your Data Folder. Function ran successfully")
  }
}

