##########################
# ScrapeCongress             #
##########################

library(twitteR)
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


#############################
## Senate Male Democrats    #
#############################



senmaleD <- function() {
  dougjones <- get_timeline('@SenDougJones', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(dougjones, "data/dougjones.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
dougjonestweets <- read_csv("data/dougjones.csv")
bennet <- get_timeline('@SenatorBennet', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(bennet, "data/bennet.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
bennettweets <- read_csv("data/bennet.csv")
blumenthal <- get_timeline('@SenBlumenthal', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(blumenthal, "data/blumenthal.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
blumenthaltweets <- read_csv("data/blumenthal.csv")
murphey <- get_timeline('@SenMurphyOffice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(murphey, "data/murphy.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
murpheytweets <- read_csv("data/murphy.csv")
chriscoons <- get_timeline('@ChrisCoons', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(chriscoons, "data/chriscoons.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
chriscoonstweets <- read_csv("data/chriscoons.csv")
cooper <- get_timeline('@SenatorCarper', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(cooper, "data/cooper.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
coopertweets <- read_csv("data/cooper.csv")
brianschatz <- get_timeline('@SenBrianSchatz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(brianschatz, "data/brianschatz.csv")
brianschatztweets <- read_csv("data/brianschatz.csv")
durbin <- get_timeline('@SenatorDurbin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(durbin, "data/durbin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
durbintweets <- read_csv("data/durbin.csv")
markey <- get_timeline('@SenMarkey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(markey, "data/markey.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
markeytweets <- read_csv("data/markey.csv")
cardin <- get_timeline('@SenatorCardin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(cardin, "data/cardin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
cardintweets <- read_csv("data/cardin.csv")
chrisvanhollen <- get_timeline('@ChrisVanHollen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(chrisvanhollen, "data/chrisvanhollen.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
chrisvanhollentweets <- read_csv("data/chrisvanhollen.csv")
garypeters <- get_timeline('@SenGaryPeters', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(garypeters, "data/garypeters.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
garypeterstweets <- read_csv("data/garypeters.csv")
booker <- get_timeline('@SenBooker', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(booker, "data/booker.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
bookertweets <- read_csv("data/booker.csv")
menedez <- get_timeline('@SenatorMenendez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(menedez, "data/menedez.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
menedeztweets <- read_csv("data/menedez.csv")
martinheinrich <- get_timeline('@MartinHeinrich', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(martinheinrich, "data/martinheinrich.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
martinheinrichtweets <- read_csv("data/martinheinrich.csv")
tomudall <- get_timeline('@SenatorTomUdall', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(tomudall, "data/tomudall.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
tomudalltweets <- read_csv("data/tomudall.csv")
schumer <- get_timeline('@SenSchumer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(schumer, "data/schumer.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
schumertweets <- read_csv("data/schumer.csv")
sherrodbrown <- get_timeline('@SenSherrodBrown', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(sherrodbrown, "data/sherrodbrown.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
sherrodbrowntweets <- read_csv("data/sherrodbrown.csv")
jeffmerkley <- get_timeline('@SenJeffMerkley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(jeffmerkley, "data/jeffmerkley.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
jeffmerkleytweets <- read_csv("data/jeffmerkley.csv")
ronwyden <- get_timeline('@RonWyden', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(ronwyden, "data/ronwyden.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
ronwydentweets <- read_csv("data/ronwyden.csv")
bobcasey <- get_timeline('@SenBobCasey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(bobcasey, "data/bobcasey.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
bobcaseytweets <- read_csv("data/bobcasey.csv")
toomey <- get_timeline('@SenToomey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(toomey, "data/toomey.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
toomeytweets <- read_csv("data/toomey.csv")
jackreed <- get_timeline('@SenJackReed', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(jackreed, "data/jackreed.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
jackreedtweets <- read_csv("data/jackreed.csv")
whitehouse <- get_timeline('@SenWhitehouse', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(whitehouse, "data/whitehouse.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
whitehousetweets <- read_csv("data/whitehouse.csv")
leahy <- get_timeline('@SenatorLeahy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(leahy, "data/leahy.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
leahytweets <- read_csv("data/leahy.csv")
sanders <- get_timeline('@SenSanders', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(sanders, "data/sanders.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
sanderstweets <- read_csv("data/sanders.csv")
warner <- get_timeline('@MarkWarner', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(warner, "data/warner.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
warnertweets <- read_csv("data/warner.csv")
joemanchin <- get_timeline('@Sen_JoeManchin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(joemanchin, "data/joemanchin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
joemanchintweets <- read_csv("data/joemanchin.csv")
senatemdtweets <- bind_rows(dougjonestweets %>%
                              mutate(person = "Doug Jones"),
                            bennettweets %>%
                              mutate(person = "Bennet"),
                            blumenthaltweets %>%
                              mutate(person = "Blumenthal"),
                            murpheytweets %>%
                              mutate(person = "Murphey"),
                            chriscoonstweets %>%
                              mutate(person = "Chris Coons"),
                            coopertweets %>%
                              mutate(person = "Cooper"),
                            brianschatztweets %>%
                              mutate(person = "Brian Schatz"),
                            durbintweets %>%
                              mutate(person = "Durbin"),
                            markeytweets %>%
                              mutate(person = "Markey"),
                            cardintweets %>%
                              mutate(person = "Cardin"),
                            chrisvanhollentweets %>%
                              mutate(person = "Chris Vanhollen"),
                            garypeterstweets %>%
                              mutate(person = "Gary Peters"),
                            bookertweets %>%
                              mutate(person = "Booker"),
                            menedeztweets %>%
                              mutate(person = "Menedez"),
                            martinheinrichtweets %>%
                              mutate(person = "Martin Heinrich"),
                            tomudalltweets %>%
                              mutate(person = "Tom Udall"),
                            schumertweets %>%
                              mutate(person = "Schumer"),
                            sherrodbrowntweets %>%
                              mutate(person = "Sherrod Brown"),
                            jeffmerkleytweets %>%
                              mutate(person = "Jeff Merkley"),
                            ronwydentweets %>%
                              mutate(person = "Ron Wyden"),
                            bobcaseytweets %>%
                              mutate(person = "Bob Casey"),
                            toomeytweets %>%
                              mutate(person = "Toomey"),
                            jackreedtweets %>%
                              mutate(person = "Jack Reed"),
                            whitehousetweets %>%
                              mutate(person = "Whitehouse"),
                            leahytweets %>%
                              mutate(person = "Leahy"),
                            sanderstweets %>%
                              mutate(person = "Sanders"),
                            warnertweets %>%
                              mutate(person = "Warner"),
                            joemanchintweets %>%
                              mutate(person = "Joe Manchin"))

save_as_csv(senatemdtweets, "data/senatemdtweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
senatemdtweets <- read_csv("data/senatemdtweets.csv")
}




#############################
## Senate Female Democrats  #
#############################


senfemD <- function() {
  sinema <- get_timeline('@SenatorSinema', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(sinema,"data/sinema.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
sinematweets <- read_csv("data/sinema.csv")
feinstein <- get_timeline('@SenFeinstein', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(feinstein,"data/feinstein.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
feinsteintweets <- read_csv("data/feinstein.csv")
kamalaharris <- get_timeline('@SenKamalaHarris', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(kamalaharris,"data/kamalaharris.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
kamalaharristweets <- read_csv("data/kamalaharris.csv")
maziehirono <- get_timeline('@maziehirono', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(maziehirono,"data/maziehirono.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
maziehironotweets <- read_csv("data/maziehirono.csv")
duckworth <- get_timeline('@SenDuckworth', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(duckworth,"data/duckworth.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
duckworthtweets <- read_csv("data/duckworth.csv")
warren <- get_timeline('@SenWarren', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(warren,"data/warren.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
warrentweets <- read_csv("data/warren.csv")
stabenow <- get_timeline('@SenStabenow', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(stabenow,"data/stabenow.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
stabenowtweets <- read_csv("data/stabenow.csv")
amyklobuchar <- get_timeline('@SenAmyKlobuchar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(amyklobuchar,"data/amyklobuchar.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
amyklobuchartweets <- read_csv("data/amyklobuchar.csv")
tinasmith <- get_timeline('@SenTinaSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(tinasmith,"data/tinasmith.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
tinasmithtweets <- read_csv("data/tinasmith.csv")
hassan <- get_timeline('@SenatorHassan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(hassan,"data/hassan.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
hassantweets <- read_csv("data/hassan.csv")
shaheen <- get_timeline('@SenatorShaheen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(shaheen,"data/shaheen.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
shaheentweets <- read_csv("data/shaheen.csv")
cortezmasto <- get_timeline('@SenCortezMasto', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(cortezmasto,"data/cortezmasto.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
cortezmastotweets <- read_csv("data/cortezmasto.csv")
jackyrosen <- get_timeline(' @SenJackyRosen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(jackyrosen,"data/jackyrosen.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
jackyrosentweets <- read_csv("data/jackyrosen.csv")
gillibrand <- get_timeline('@gillibrandny', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(gillibrand,"data/gillibrand.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
gillibrandtweets <- read_csv("data/gillibrand.csv")
cantwell <- get_timeline('@SenatorCantwell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(cantwell,"data/cantewell.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
cantwelltweets <- read_csv("data/cantewell.csv")
pattymurray <- get_timeline('@PattyMurray', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(pattymurray,"data/pattymurray.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
pattymurraytweets <- read_csv("data/pattymurray.csv")
baldwin <- get_timeline('@SenatorBaldwin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(baldwin,"data/baldwin.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
baldwintweets <- read_csv("data/baldwin.csv")
senatefdtweets <- bind_rows(feinsteintweets %>%
                                mutate(person = "Feinstein"),
                              kamalaharristweets %>%
                                mutate(person = "Kamala Harris"),
                              maziehironotweets %>%
                                mutate(person = "Mazie Hirono"),
                              duckworthtweets %>%
                                mutate(person = "Duckworth"),
                              warrentweets %>%
                                mutate(person = "Warren"),
                              stabenowtweets %>%
                                mutate(person = "Stabenow"),
                              amyklobuchartweets %>%
                                mutate(person = "Amy Klobuchar"),
                              tinasmithtweets %>%
                                mutate(person = "Tina Smith"),
                              hassantweets %>%
                                mutate(person = "Hassan"),
                              shaheentweets %>%
                                mutate(person = "Shaheen"),
                              cortezmastotweets %>%
                                mutate(person = "Cortez-Masto"),
                              jackyrosentweets %>%
                                mutate(person = "Jacky Rosen"),
                              gillibrandtweets %>%
                                mutate(person = "Gillibrand"),
                              cantwelltweets %>%
                                mutate(person = "Cantwell"),
                              pattymurraytweets %>%
                                mutate(person = "Patty Murray"),
                              baldwintweets %>%
                                mutate(person = "Baldwin"))

save_as_csv(senatefdtweets, "data/senatefdtweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
senatefdtweets <- read_csv("data/senatefdtweets.csv")
}

#############################
## Senate Female Republicans#
#############################

senfemR <- function() {
  lisamurkowski<- get_timeline('@lisamurkowski', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(lisamurkowski,"data/lisamurkowski.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
lisamurkowskitweets <- read_csv("data/lisamurkowski.csv")
mcsally <- get_timeline('@SenMcSallyAZ', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(mcsally,"data/mcsally.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
mcsallytweets <- read_csv("data/mcsally.csv")
joniernst <- get_timeline('@SenJoniErnst', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(joniernst,"data/joniernst.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
joniernsttweets <- read_csv("data/joniernst.csv")
collins <- get_timeline('@SenatorCollins', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(collins,"data/collins.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
collinstweets <- read_csv("data/collins.csv")
hydesmith <- get_timeline('@SenHydeSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(hydesmith,"data/hydesmith.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
hydesmithtweets <- read_csv("data/hydesmith.csv")
fischer <- get_timeline('@SenatorFischer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(fischer,"data/fischer.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
fischertweets <- read_csv("data/fischer.csv")
marshablackburn <- get_timeline('@MarshaBlackburn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(marshablackburn,"data/marshablackburn.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
marshablackburntweets <- read_csv("data/marshablackburn.csv")
capito <- get_timeline('@SenCapito', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
save_as_csv(capito,"data/capito.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
capitotweets <- read_csv("data/capito.csv")

senatefrtweets <- bind_rows(lisamurkowskitweets %>%
                                mutate(person = "Lisa Murkowski"),
                              mcsallytweets %>%
                                mutate(person = "Martha McSally"),
                              joniernsttweets %>%
                                mutate(person = "Joni Ernst"),
                              collinstweets %>%
                                mutate(person = "Susan Collinis"),
                              hydesmithtweets %>%
                                mutate(person = "Cindy Hyde-Smith"),
                              fischertweets %>%
                                mutate(person = "Deb Fischer"),
                              marshablackburntweets %>%
                                mutate(person = "Marsha Blackburn"),
                              capitotweets %>%
                                mutate(person = "Shelley Moore Capito"))

save_as_csv(senatefrtweets, "data/senatefrtweets.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
senatefrtweets <- read_csv("data/senatefrtweets.csv")
}


#############################
## Senate Male Republicans  #
#############################


senmaleR <- function() {
  donsullivan <- get_timeline('@SenDanSullivan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
shelby <- get_timeline('@SenShelby', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johnboozman <- get_timeline('@JohnBoozman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
tomcotton <- get_timeline('@SenTomCotton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
corygardner <- get_timeline('@SenCoryGardner', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
rubio <- get_timeline('@SenRubioPress', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
rickscott<- get_timeline('@SenRickScott', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
isakson <- get_timeline('@SenatorIsakson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
davidperdue <- get_timeline('@sendavidperdue', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
chuckgrassley <- get_timeline('@ChuckGrassley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
mikecrapo <- get_timeline('@MikeCrapo', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
risch <- get_timeline('@SenatorRisch', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
braun<- get_timeline('@SenatorBraun', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
toddyoung<- get_timeline('@SenToddYoung', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
jerrymoran<- get_timeline('@JerryMoran', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
patroberts<- get_timeline('@SenPatRoberts', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
mitchmconnell<- get_timeline('@SenateMajLdr', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
randpaul<- get_timeline('@RandPaul', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
billcassidy<- get_timeline('@SenBillCassidy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johnkennedy<- get_timeline('@SenJohnKennedy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
royblunt<- get_timeline('@RoyBlunt', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
hawley<- get_timeline('@SenHawleyPress', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
wicker<- get_timeline('@SenatorWicker', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
stevedaines<- get_timeline('@SteveDaines', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
burr<- get_timeline('@SenatorBurr', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
thomtillis<- get_timeline('@SenThomTillis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
kevincramer<- get_timeline('@@SenKevinCramer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johnhoeven<- get_timeline('@SenJohnHoeven', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
sasse<- get_timeline('@SenSasse', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
robportman<- get_timeline('@senrobportman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
jiminhofe<- get_timeline('@JimInhofe', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
lankford<- get_timeline('@SenatorLankford', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
lindseygraham<- get_timeline('@LindseyGrahamSC', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
timscott<- get_timeline('@SenatorTimScott', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
rounds<- get_timeline('@SenatorRounds', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johnthune<- get_timeline('@SenJohnThune', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
alexander<- get_timeline('@SenAlexander', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johncornyn<- get_timeline('@JohnCornyn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
tedcruz<- get_timeline('@SenTedCruz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
mikelee<- get_timeline('@SenMikeLee', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
romney<- get_timeline('@SenatorRomney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
ronjohnson<- get_timeline('@SenRonJohnson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
johnbarrasso<- get_timeline('@SenJohnBarrasso', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
enzi<- get_timeline('@SenatorEnzi', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)


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

donsullivantweets <- read_csv("data/donsullivan.csv")
shelbytweets <- read_csv("data/shelby.csv")
johnboozmantweets <- read_csv("data/johnboozman.csv")
tomcottontweets <- read_csv("data/tomcotton.csv")
corygardnertweets <- read_csv("data/corygardner.csv")
rubiotweets <- read_csv("data/rubio.csv")
rickscotttweets <- read_csv("data/rickscott.csv")
isaksontweets <- read_csv("data/isakson.csv")
davidperduetweets <- read_csv("data/davidperdue.csv")
chuckgrassleytweets <- read_csv("data/chuckgrassley.csv")
mikecrapotweets <- read_csv("data/mikecrapo.csv")
rischtweets <- read_csv("data/risch.csv")
brauntweets <- read_csv("data/braun.csv")
toddyoungtweets <- read_csv("data/toddyoung.csv")
jerrymorantweets <- read_csv("data/jerrymoran.csv")
patrobertstweets <- read_csv("data/patroberts.csv")
mitchmconnelltweets <- read_csv("data/mitchmconnell.csv")
randpaultweets <- read_csv("data/randpaul.csv")
billcassidytweets <- read_csv("data/billcassidy.csv")
johnkennedytweets <- read_csv("data/johnkennedy.csv")
royblunttweets <- read_csv("data/royblunt.csv")
hawleytweets <- read_csv("data/hawley.csv")
wickertweets <- read_csv("data/wicker.csv")
stevedainestweets <- read_csv("data/stevedaines.csv")
burrtweets <- read_csv("data/burr.csv")
thomtillistweets <- read_csv("data/thomtillis.csv")
kevincramertweets <- read_csv("data/kevincramer.csv")
johnhoeventweets <- read_csv("data/johnhoeven.csv")
sassetweets <- read_csv("data/sasse.csv")
robportmantweets <- read_csv("data/robportman.csv")
jiminhofetweets <- read_csv("data/jiminhofe.csv")
lankfordtweets <- read_csv("data/lankford.csv")
lindseygrahamtweets <- read_csv("data/lindseygraham.csv")
timscotttweets <- read_csv("data/timscott.csv")
roundstweets <- read_csv("data/rounds.csv")
johnthunetweets <- read_csv("data/johnthune.csv")
alexandertweets <- read_csv("data/alexander.csv")
johncornyntweets <- read_csv("data/johncornyn.csv")
tedcruztweets <- read_csv("data/tedcruz.csv")
mikeleetweets <- read_csv("data/mikelee.csv")
romneytweets <- read_csv("data/romney.csv")
ronjohnsontweets <- read_csv("data/ronjohnson.csv")
johnbarrassotweets <- read_csv("data/johnbarrasso.csv")
enzitweets <- read_csv("data/enzi.csv")

senatemrtweets <- bind_rows(donsullivantweets %>%
                              mutate(person = "Mike Lee"),
                            shelbytweets %>%
                              mutate(person = "Shelby"),
                            johnboozmantweets %>%
                              mutate(person = "John Boozman"),
                            tomcottontweets %>%
                              mutate(person = "Tom Cotton"),
                            corygardnertweets %>%
                              mutate(person = "Cory Gardner"),
                            rubiotweets %>%
                              mutate(person = "Marco Rubio"),
                            rickscotttweets %>%
                              mutate(person = "Rick Scott"),
                            isaksontweets %>%
                              mutate(person = "Isakson"),
                            davidperduetweets %>%
                              mutate(person = "David Perdue"),
                            chuckgrassleytweets %>%
                              mutate(person = "Chuck ChuckGrassley"),
                            mikecrapotweets %>%
                              mutate(person = "Mike Crapo"),
                            rischtweets %>%
                              mutate(person = "Tim Risch"),
                            brauntweets %>%
                              mutate(person = "Braun"),
                            toddyoungtweets %>%
                              mutate(person = "Todd Young"),
                            jerrymorantweets %>%
                              mutate(person = "Jerry Moran"),
                            patrobertstweets %>%
                              mutate(person = "Pat Roberts"),
                            mitchmconnelltweets %>%
                              mutate(person = "Mitch McConnell"),
                            randpaultweets %>%
                              mutate(person = "Rand Paul"),
                            billcassidytweets %>%
                              mutate(person = "Bill Cassidy"),
                            johnkennedytweets %>%
                              mutate(person = "John Kennedy"),
                            royblunttweets %>%
                              mutate(person = "Roy Blunt"),
                            hawleytweets %>%
                              mutate(person = "Hawley"),
                            wickertweets %>%
                              mutate(person = "Wicker"),
                            stevedainestweets %>%
                              mutate(person = "Steven Daines"),
                            burrtweets %>%
                              mutate(person = "Burr"),
                            thomtillistweets %>%
                              mutate(person = "Thom Tillis"),
                            kevincramertweets %>%
                              mutate(person = "Kevin Cramer"),
                            johnhoeventweets %>%
                              mutate(person = "John Hoeven"),
                            sassetweets %>%
                              mutate(person = "Sasse"),
                            robportmantweets %>%
                              mutate(person = "Rob Portman"),
                            jiminhofetweets %>%
                              mutate(person = "Jim Inhofe"),
                            lankfordtweets %>%
                              mutate(person = "Lankford"),
                            lindseygrahamtweets %>%
                              mutate(person = "Lindsey Graham"),
                            timscotttweets %>%
                              mutate(person = "Tim Scott"),
                            roundstweets %>%
                              mutate(person = "Rounds"),
                            johnthunetweets %>%
                              mutate(person = "John Thune"),
                            alexandertweets %>%
                              mutate(person = "Alexander"),
                            johncornyntweets %>%
                              mutate(person = "John Cornyn"),
                            tedcruztweets %>%
                              mutate(person = "Ted Cruz"),
                            mikeleetweets %>%
                              mutate(person = "Mike Lee"),
                            romneytweets %>%
                              mutate(person = "Mitt Romney"),
                            ronjohnsontweets %>%
                              mutate(person = "Ron Johnson"),
                            johnbarrassotweets %>%
                              mutate(person = "John Barrasso"),
                            enzitweets %>%
                              mutate(person = "Enzi"))

save_as_csv(senatemrtweets, "data/senatemrtweets.csv",prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
senatemrtweets <- read_csv("data/senatemrtweets.csv")
}


#### House of Representatives ######

horfemR <- function() {
  martharoby <- get_timeline('@RepMarthaRoby', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  walorski <- get_timeline('@RepWalorski', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  susanbrooks <- get_timeline('@SusanWBrooks', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  annwagner <- get_timeline('@RepAnnWagner', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hartzler <- get_timeline('@RepHartzler', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  virginiafoxx <- get_timeline('@virginiafoxx', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  armstrong <- get_timeline('@RepArmstrongND', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stefanik <- get_timeline('@RepStefanik', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cathymcmorris <- get_timeline('@cathymcmorris', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lizcheney<- get_timeline('@RepLizCheney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)


  save_as_csv(martharoby,"data/martharoby.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(walorski,"data/walorski.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(susanbrooks,"data/susanbrooks.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(annwagner,"data/annwagner.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(hartzler,"data/hartzler.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(virginiafoxx,"data/virginiafoxx.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(stefanik,"data/stefanik.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(cathymcmorris,"data/cathymcmorris.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")
  save_as_csv(lizcheney,"data/lizcheney.csv", prepend_ids =  TRUE, na = "", fileEncoding = "UTF-8")

  martharobytweets <- read_csv("data/martharoby.csv")
  walorskitweets <- read_csv("data/walorski.csv")
  susanbrookstweets <- read_csv("data/susanbrooks.csv")
  annwagnertweets <- read_csv("data/annwagner.csv")
  hartzlertweets <- read_csv("data/hartzler.csv")
  virginiafoxxtweets <- read_csv("data/virginiafoxx.csv")
  armstrongtweets <- read_csv("data/armstrong.csv")
  stefaniktweets <- read_csv("data/stefanik.csv")
  cathymcmorristweets <- read_csv("data/cathymcmorris.csv")
  lizcheneytweets <- read_csv("data/lizcheney.csv")
  horfrtweets <- bind_rows(martharobytweets %>%
                             mutate(person = "Martha Roby"),
                           walorskitweets %>%
                             mutate(person = "Walorksi"),
                           susanbrookstweets %>%
                             mutate(person = "Susan Brooks"),
                           annwagnertweets %>%
                             mutate(person = "Ann Wagner"),
                           hartzlertweets %>%
                             mutate(person = "Hartzler"),
                           virginiafoxxtweets %>%
                             mutate(person = "Virginia Foxx"),
                           armstrongtweets %>%
                             mutate(person = "Armstrong"),
                           stefaniktweets %>%
                             mutate(person = "Stefanik"),
                           cathymcmorristweets %>%
                             mutate(person = "Cathy McMorris"),
                           lizcheneytweets %>%
                             mutate(person = "Liz Cheney"))
  save_as_csv(horfrtweets, "data/horfrtweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
  horfrtweets <- read_csv("data/horfrtweets.csv")
}


horfemD <- function() {
  terrisewell <- get_timeline('@RepTerriSewell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kirkpatrick<- get_timeline('@RepKirkpatrick', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dorismatsui<- get_timeline('@DorisMatsui', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bera<- get_timeline('@RepBera', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  pelosi <- get_timeline('@SpeakerPelosi', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  barbaralee <- get_timeline('@RepBarbaraLee', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  speier <- get_timeline('@RepSpeier', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  annaeshoo <- get_timeline('@RepAnnaEshoo', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  zoelofgren <- get_timeline('@RepZoeLofgren', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  katiehill <- get_timeline('@RepKatieHill', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brownley <- get_timeline('@RepBrownley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  judychu <- get_timeline('@RepJudyChu', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gracenapolitano <- get_timeline('@gracenapolitano', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  normatorres <- get_timeline('@NormaJTorres', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  karenbass <- get_timeline('@RepKarenBass', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lindasanchez <- get_timeline('@RepLindaSanchez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  roybalallard<- get_timeline('@RepRoybalAllard', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  maxinewaters<- get_timeline('@RepMaxineWaters', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  barragan<- get_timeline('@RepBarragan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  katieporter <- get_timeline('@RepKatiePorter', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  susandavis <- get_timeline('@RepSusanDavis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dianadegette <- get_timeline('@RepDianaDeGette', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rosadelauro<- get_timeline('@rosadelauro', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jahanahayes<- get_timeline('@RepJahanaHayes', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lbr<- get_timeline('@RepLBR', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  eleanornorton<- get_timeline('@EleanorNorton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stephmurphy<- get_timeline('@RepStephMurphy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kcastor<- get_timeline('@USRepKCastor', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hastings <- get_timeline('@RepHastingsFL', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  loisfrankel <- get_timeline('@RepLoisFrankel', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dwstweets <- get_timeline('@RepDWStweets', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  wilson <- get_timeline('@RepWilson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dmp <- get_timeline('@RepDMP', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  shalala <- get_timeline('@RepShalala', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tulsi <- get_timeline('@TulsiPress', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  finkenauer <- get_timeline('@RepFinkenauer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cindyaxne <- get_timeline('@RepCindyAxne', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  robinkelly <- get_timeline('@RepRobinKelly', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  janschakowsky <- get_timeline('@janschakowsky', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  underwood <- get_timeline('@RepUnderwood', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cheri <- get_timeline('@RepCheri', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davids <- get_timeline('@RepDavids', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  loritrahan <- get_timeline('@RepLoriTrahan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kclark <- get_timeline('@RepKClark', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  pressley <- get_timeline('@RepPressley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  haleystevens <- get_timeline('@RepHaleyStevens', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  debdingell <- get_timeline('@RepDebDingell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rashida <- get_timeline('@RepRashida', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lawrence <- get_timeline('@RepLawrence', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  angiecraig <- get_timeline('@RepAngieCraig', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  deanphillips <- get_timeline('@RepDeanPhillips', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bettymccollum <- get_timeline('@BettyMcCollum04', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ilhan<- get_timeline('@Ilhan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lacyclay <- get_timeline('@LacyClayMO1', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  adams <- get_timeline('@RepAdams', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  anniekuster <- get_timeline('@RepAnnieKuster', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bonnie<- get_timeline('@RepBonnie', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  debhaaland <- get_timeline('@RepDebHaaland', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  torressmall <- get_timeline('@RepTorresSmall', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dinatitus<- get_timeline('@repdinatitus', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  susielee <- get_timeline('@RepSusieLee', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kathleenrice <- get_timeline('@RepKathleenRice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  nydiavelazquez<- get_timeline('@NydiaVelazquez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  yvetteclarke<- get_timeline('@RepYvetteClarke', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  maloney<- get_timeline('@RepMaloney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  aoc <- get_timeline('@RepAOC', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  nitalowey <- get_timeline('@NitaLowey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  beatty <- get_timeline('@RepBeatty', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  marcykaptur <- get_timeline('@RepMarcyKaptur', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  marciafudge<- get_timeline('@RepMarciaFudge', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kendrahorn<- get_timeline('@RepKendraHorn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bonamici <- get_timeline('@RepBonamici', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mgs <- get_timeline('@RepMGS', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dean <- get_timeline('@RepDean', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  houlahan <- get_timeline('@RepHoulahan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  susanwild <- get_timeline('@RepSusanWild', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  fletcher <- get_timeline('@RepFletcher', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  escobar <- get_timeline('@RepEscobar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jacksonlee <- get_timeline('@JacksonLeeTX18', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  sylviagarcia <- get_timeline('@RepSylviaGarcia', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  elaineluria<- get_timeline('@RepElaineLuria', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  spanberger<- get_timeline('@RepSpanberger', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  wexton <- get_timeline('@RepWexton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  delbene <- get_timeline('@RepDelBene', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  herrerabeutler <- get_timeline('@HerreraBeutler', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jayapal<- get_timeline('@RepJayapal', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kimschrier <- get_timeline('@RepKimSchrier', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gwenmoore <- get_timeline('@RepGwenMoore', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)

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

  terrisewelltweets <- read_csv("data/terrisewell.csv")
  kirkpatricktweets <- read_csv("data/kirkpatrick.csv")
  dorismatsuitweets <- read_csv("data/dorismatsui.csv")
  beratweets <- read_csv("data/bera.csv")
  pelositweets <- read_csv("data/pelosi.csv")
  barbaraleetweets <- read_csv("data/barbaralee.csv")
  speiertweets <- read_csv("data/speier.csv")
  annaeshootweets <- read_csv("data/annaeshoo.csv")
  zoelofgrentweets <- read_csv("data/zoelofgren.csv")
  katiehilltweets <- read_csv("data/katiehill.csv")
  brownleytweets <- read_csv("data/brownley.csv")
  judychutweets <- read_csv("data/judychu.csv")
  gracenapolitanotweets <- read_csv("data/gracenapolitano.csv")
  normatorrestweets <- read_csv("data/normatorres.csv")
  karenbasstweets <- read_csv("data/karenbass.csv")
  lindasancheztweets <- read_csv("data/lindasanchez.csv")
  roybalallardtweets <- read_csv("data/roybalallard.csv")
  maxinewaterstweets <- read_csv("data/maxinewaters.csv")
  barragantweets <- read_csv("data/barragan.csv")
  katieportertweets <- read_csv("data/katieporter.csv")
  susandavistweets <- read_csv("data/susandavis.csv")
  dianadegettetweets <- read_csv("data/dianadegette.csv")
  rosadelaurotweets <- read_csv("data/rosadelauro.csv")
  jahanahayestweets <- read_csv("data/jahanahayes.csv")
  lbrtweets <- read_csv("data/lbr.csv")
  eleanornortontweets <- read_csv("data/eleanornorton.csv")
  stephmurphytweets <- read_csv("data/stephmurphy.csv")
  kcastortweets <- read_csv("data/kcastor.csv")
  hastingstweets <- read_csv("data/hastings.csv")
  loisfrankeltweets <- read_csv("data/loisfrankel.csv")
  dwsweetstweets <- read_csv("data/dwstweets.csv")
  wilsontweets <- read_csv("data/wilson.csv")
  dmptweets <-  read_csv("data/dmp.csv")
  shalalatweets <- read_csv("data/shalala.csv")
  tulsitweets <- read_csv("data/tulsi.csv")
  finkenauertweets <- read_csv("data/finkenauer.csv")
  cindyaxnetweets <- read_csv("data/cindyaxne.csv")
  robinkellytweets <- read_csv("data/robinkelly.csv")
  janschakowskytweets <- read_csv("data/janschakowsky.csv")
  underwoodtweets <- read_csv("data/underwood.csv")
  cheritweets <- read_csv("data/cheri.csv")
  davidstweets <- read_csv("data/davids.csv")
  loritrahantweets <- read_csv("data/loritrahan.csv")
  kclarktweets <- read_csv("data/kclark.csv")
  pressleytweets <- read_csv("data/pressley.csv")
  haleystevenstweets <- read_csv("data/haleystevens.csv")
  debdingelltweets <- read_csv("data/debdingell.csv")
  rashidatweets <- read_csv("data/rashida.csv")
  lawrencetweets <- read_csv("data/lawrence.csv")
  angiecraigtweets <- read_csv("data/angiecraig.csv")
  deanphillipstweets <- read_csv("data/deanphillips.csv")
  bettymccollumtweets <- read_csv("data/bettymccollum.csv")
  ilhantweets <- read_csv("data/ilhan.csv")
  lacyclaytweets <- read_csv("data/lacyclay.csv")
  adamstweets <- read_csv("data/adams.csv")
  anniekustertweets <- read_csv("data/anniekuster.csv")
  bonnietweets <- read_csv("data/bonnie.csv")
  debhaalandtweets <- read_csv("data/debhaaland.csv")
  torressmalltweets <- read_csv("data/torressmall.csv")
  dinatitustweets <- read_csv("data/dinatitus.csv")
  susieleetweets <- read_csv("data/susielee.csv")
  kathleenricetweets <- read_csv("data/kathleenrice.csv")
  nydiavelazqueztweets <- read_csv("data/nydiavelazquez.csv")
  yvetteclarketweets <- read_csv("data/yvetteclarke.csv")
  maloneytweets <- read_csv("data/maloney.csv")
  aoctweets <- read_csv("data/aoc.csv")
  nitaloweytweets <- read_csv("data/nitalowey.csv")
  beattytweets <- read_csv("data/beatty.csv")
  marcykapturtweets <- read_csv("data/marcykaptur.csv")
  marciafudgetweets <- read_csv("data/marciafudge.csv")
  kendrahorntweets <- read_csv("data/kendrahorn.csv")
  bonamicitweets <- read_csv("data/bonamici.csv")
  mgstweets <- read_csv("data/mgs.csv")
  deantweets <- read_csv("data/dean.csv")
  houlahantweets <- read_csv("data/houlahan.csv")
  susanwildtweets <- read_csv("data/susanwild.csv")
  fletchertweets <- read_csv("data/fletcher.csv")
  escobartweets <- read_csv("data/escobar.csv")
  jacksonleetweets <- read_csv("data/jacksonlee.csv")
  sylviagarciatweets <- read_csv("data/sylviagarcia.csv")
  elaineluriatweets <- read_csv("data/elaineluria.csv")
  spanbergertweets <- read_csv("data/spanberger.csv")
  wextontweets <- read_csv("data/wexton.csv")
  delbenetweets <- read_csv("data/delbene.csv")
  herrerabeutlertweets <- read_csv("data/herrerabeutler.csv")
  jayapaltweets <- read_csv("data/jayapal.csv")
  kimschriertweets <- read_csv("data/kimschrier.csv")
  gwenmooretweets <- read_csv("data/gwenmoore.csv")


  horfdtweets <- bind_rows(terrisewelltweets %>%
                             mutate(person = "Terri Swell"),
                           kirkpatricktweets %>%
                             mutate(person = "Kirkpatrick"),
                           dorismatsuitweets %>%
                             mutate(person = "Doris Matsui"),
                           beratweets %>%
                             mutate(person = "Bera"),
                           pelositweets %>%
                             mutate(person = "Nancy Pelosi"),
                           barbaraleetweets %>%
                             mutate(person = "Barbara Lee"),
                           speiertweets %>%
                             mutate(person = "Speier"),
                           annaeshootweets %>%
                             mutate(person = "Anna Eshoo"),
                           zoelofgrentweets %>%
                             mutate(person = "Zoe Lofgren"),
                           katiehilltweets %>%
                             mutate(person = "Katie Hill"),
                           brownleytweets %>%
                             mutate(person = "Brownley"),
                           judychutweets %>%
                             mutate(person = "Judy Chu"),
                           gracenapolitanotweets %>%
                             mutate(person = "Grace Napolitano"),
                           normatorrestweets %>%
                             mutate(person = "Norma Torres"),
                           karenbasstweets %>%
                             mutate(person = "Karen Bass"),
                           lindasancheztweets %>%
                             mutate(person = "Linda Sanchez"),
                           roybalallardtweets %>%
                             mutate(person = "Roybal Allard"),
                           maxinewaterstweets %>%
                             mutate(person = "Maxine Waters"),
                           barragantweets %>%
                             mutate(person = "Barragan"),
                           katieportertweets %>%
                             mutate(person = "Katie Porter"),
                           susandavistweets %>%
                             mutate(person = "Susan Davis"),
                           dianadegettetweets %>%
                             mutate(person = "Diana Delgette"),
                           rosadelaurotweets %>%
                             mutate(person = "Rosa Delauro"),
                           jahanahayestweets %>%
                             mutate(person = "Jahana Hayes"),
                           lbrtweets %>%
                             mutate(person = "LBR"),
                           eleanornortontweets %>%
                             mutate(person = "Eleanor Norton"),
                           stephmurphytweets %>%
                             mutate(person = "Steph Murphy"),
                           kcastortweets %>%
                             mutate(person = "K Castor"),
                           hastingstweets %>%
                             mutate(person = "Hastings"),
                           loisfrankeltweets %>%
                             mutate(person = "Lois Frankel"),
                           dwsweetstweets %>%
                             mutate(person = "DW Stweets"),
                           wilsontweets %>%
                             mutate(person = "Wilson"),
                           dmptweets %>%
                             mutate(person = "DMP"),
                           shalalatweets %>%
                             mutate(person = "Shalala"),
                           tulsitweets %>%
                             mutate(person = "Tulsi"),
                           finkenauertweets %>%
                             mutate(person = "Finkenauer"),
                           cindyaxnetweets %>%
                             mutate(person = "Cindy Axne"),
                           robinkellytweets %>%
                             mutate(person = "Robin Kelly"),
                           janschakowskytweets %>%
                             mutate(person = "Janschakowsky"),
                           underwoodtweets %>%
                             mutate(person = "Underwood"),
                           cheritweets %>%
                             mutate(person = "Cheri"),
                           davidstweets %>%
                             mutate(person = "Davids"),
                           loritrahantweets %>%
                             mutate(person = "Lori Trahan"),
                           kclarktweets %>%
                             mutate(person = "K Clark"),
                           pressleytweets %>%
                             mutate(person = "Pressley"),
                           haleystevenstweets %>%
                             mutate(person = "Haley Stevens"),
                           debdingelltweets %>%
                             mutate(person = "Deb Dingell"),
                           rashidatweets %>%
                             mutate(person = "Rashida Talib"),
                           lawrencetweets %>%
                             mutate(person = "Lawrence"),
                           angiecraigtweets %>%
                             mutate(person = "Angie Craig"),
                           deanphillipstweets %>%
                             mutate(person = "Dean Phillips"),
                           bettymccollumtweets %>%
                             mutate(person = "Betty McCollum"),
                           ilhantweets %>%
                             mutate(person = "Ilhan Omar"),
                           lacyclaytweets %>%
                             mutate(person = "Lacy Clay"),
                           adamstweets %>%
                             mutate(person = "Adams"),
                           anniekustertweets %>%
                             mutate(person = "Annie Kuster"),
                           bonnietweets %>%
                             mutate(person = "Bonnie"),
                           debhaalandtweets %>%
                             mutate(person = "Deb Haaland"),
                           torressmalltweets %>%
                             mutate(person = "Torres Small"),
                           dinatitustweets %>%
                             mutate(person = "Dina Titus"),
                           susieleetweets %>%
                             mutate(person = "Susie Lee"),
                           kathleenricetweets %>%
                             mutate(person = "Kathleen Rice"),
                           nydiavelazqueztweets %>%
                             mutate(person = "Nydia Velazquez"),
                           yvetteclarketweets %>%
                             mutate(person = "Yvette Clarke"),
                           maloneytweets %>%
                             mutate(person = "Maloney"),
                           aoctweets %>%
                             mutate(person = "AOC"),
                           nitaloweytweets %>%
                             mutate(person = "Nita Lowey"),
                           beattytweets %>%
                             mutate(person = "Beatty"),
                           marcykapturtweets %>%
                             mutate(person = "Marcy Kaptur"),
                           marciafudgetweets %>%
                             mutate(person = "Marcia Fudge"),
                           kendrahorntweets %>%
                             mutate(person = "Kendra Horn"),
                           bonamicitweets %>%
                             mutate(person = "Bonamici"),
                           mgstweets %>%
                             mutate(person = "MGS"),
                           deantweets %>%
                             mutate(person = "Dean"),
                           houlahantweets %>%
                             mutate(person = "Houlahan"),
                           susanwildtweets %>%
                             mutate(person = "Susan Wild"),
                           fletchertweets %>%
                             mutate(person = "Fletcher"),
                           escobartweets %>%
                             mutate(person = "Escobar"),
                           jacksonleetweets %>%
                             mutate(person = "Jackson Lee"),
                           sylviagarciatweets %>%
                             mutate(person = "Sylvia Garcia"),
                           elaineluriatweets %>%
                             mutate(person = "Elaine Luria"),
                           spanbergertweets %>%
                             mutate(person = "Spanberger"),
                           wextontweets %>%
                             mutate(person = "Wexton"),
                           delbenetweets %>%
                             mutate(person = "Delbene"),
                           herrerabeutlertweets %>%
                             mutate(person = "Herrera Beutler"),
                           jayapaltweets %>%
                             mutate(person = "Jayapal"),
                           kimschriertweets %>%
                             mutate(person = "Kim Schrier"),
                           gwenmooretweets %>%
                             mutate(person = "Gwen Moore"))

  save_as_csv(horfdtweets, "data/horfdtweets.csv", prepend_ids = TRUE , na = "", fileEncoding = "UTF-8")
  horfdtweets <- read_csv("data/horfdtweets.csv")
}


hormaleR <- function() {
  donyoung <- get_timeline('@repdonyoung', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  byrne <- get_timeline('@RepByrne', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikerogers <- get_timeline('@RepMikeRogersAL', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  robertaderholt <- get_timeline('@Robert_Aderholt', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mobrooks <- get_timeline('@RepMoBrooks', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  garypalmer <- get_timeline('@USRepGaryPalmer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rickcrawford <- get_timeline('@RepRickCrawford', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  frenchhill<- get_timeline('@RepFrenchHill', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stevewomack <- get_timeline('@rep_stevewomack', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  westerman <- get_timeline('@RepWesterman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gosar <- get_timeline('@RepGosar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andybiggs <- get_timeline('@RepAndyBiggsAZ', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  david <- get_timeline('@RepDavid', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dlesko <- get_timeline('@RepDLesko', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lamalfa<- get_timeline('@RepLaMalfa', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mcclintock <- get_timeline('@RepMcClintock', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  paulcook <- get_timeline('@RepPaulCook', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  devinnunes <- get_timeline('@RepDevinNunes', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kevinmccarthy <- get_timeline('@GOPLeader', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kencalvert<- get_timeline('@KenCalvert', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tipton <- get_timeline('@RepTipton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kenbuck <- get_timeline('@RepKenBuck', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dlamborn <- get_timeline('@RepDLamborn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mattgaetz <- get_timeline('@RepMattGaetz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  nealdunn <- get_timeline('@DrNealDunnFL2', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tedyoho <- get_timeline('@RepTedYoho', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rutherford <- get_timeline('@RepRutherfordFL', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  michaelwaltz <- get_timeline('@RepMichaelWaltz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  billposey <- get_timeline('@congbillposey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  webster <- get_timeline('@RepWebster', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gusbilirakis <- get_timeline('@RepGusBilirakis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  vernbuchanan<- get_timeline('@VernBuchanan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gregsteube <- get_timeline('@RepGregSteube', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brianmast <- get_timeline('@RepBrianMast', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rooney <- get_timeline('@RepRooney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mariodb <- get_timeline('@MarioDB', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  buddycarter <- get_timeline('@RepBuddyCarter', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  drewferguson <- get_timeline('@RepDrewFerguson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  robwoodall<- get_timeline('@RepRobWoodall', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  austinscott <- get_timeline('@AustinScottGA08', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dougcollins <- get_timeline('@RepDougCollins', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hice <- get_timeline('@CongressmanHice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  loudermilk <- get_timeline('@RepLoudermilk', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rickallen <- get_timeline('@RepRickAllen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomgraves <- get_timeline('@RepTomGraves', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  russfulcher <- get_timeline('@RepRussFulcher', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikesimpson <- get_timeline('@CongMikeSimpson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bost <- get_timeline('@RepBost', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rodneydavis <- get_timeline('@RodneyDavis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  shimkus <- get_timeline('@RepShimkus', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kinzinger <- get_timeline('@RepKinzinger', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lahood <- get_timeline('@RepLaHood', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimbanks <- get_timeline('@RepJimBanks', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimbaird <- get_timeline('@RepJimBaird', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gregpence <- get_timeline('@RepGregPence', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  larrybucshon <- get_timeline('@RepLarryBucshon', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  trey<- get_timeline('@RepTrey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ronestes <- get_timeline('@RepRonEstes', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  guthrie <- get_timeline('@RepGuthrie', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  thomasmassie <- get_timeline('@RepThomasMassie', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  halrogers<- get_timeline('@RepHalRogers', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andybarr <- get_timeline('@RepAndyBarr', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stevescalise <- get_timeline('@SteveScalise', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  clayhiggins <- get_timeline('@RepClayHiggins', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikejohnson <- get_timeline('@RepMikeJohnson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  abraham <- get_timeline('@RepAbraham', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  garretgraves <- get_timeline('@RepGarretGraves', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andyharris<- get_timeline('@RepAndyHarrisMD', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jackbergman <- get_timeline('@RepJackBergman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  huizenga <- get_timeline('@RepHuizenga', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  justinamash <- get_timeline('@justinamash', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  moolenaar<- get_timeline('@RepMoolenaar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  fredupton <- get_timeline('@RepFredUpton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  walberg <- get_timeline('@RepWalberg', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  slotkin <- get_timeline('@RepSlotkin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  paulmitchell <- get_timeline('@RepPaulMitchell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hagedorn<- get_timeline('@RepHagedorn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomemmer <- get_timeline('@RepTomEmmer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  petestauber <- get_timeline('@RepPeteStauber', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  blaine <- get_timeline('@RepBlaine', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  samgraves<- get_timeline('@RepSamGraves', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  long <- get_timeline('@USRepLong', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jasonsmith <- get_timeline('@RepJasonSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  trentkelly <- get_timeline('@RepTrentKelly', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  michaelguest <- get_timeline('@RepMichaelGuest', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  palazzo<- get_timeline('@CongPalazzo', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  holding <- get_timeline('@RepHolding', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  walterjones <- get_timeline('@RepWalterJones', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  markwalker <- get_timeline('@RepMarkWalker', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidrouzer <- get_timeline('@RepDavidRouzer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  richhudson <- get_timeline('@RepRichHudson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  patrickmchenry <- get_timeline('@PatrickMcHenry', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  markmeadows <- get_timeline('@RepMarkMeadows', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tedbudd <- get_timeline('@RepTedBudd', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jefffortenberry <- get_timeline('@JeffFortenberry', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  donbacon<- get_timeline('@RepDonBacon', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  adriansmith <- get_timeline('@RepAdrianSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jvd <- get_timeline('@CongressmanJVD', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chrissmith <- get_timeline('@RepChrisSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  markamodei <- get_timeline('@MarkAmodeiNV2', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  leezeldin <- get_timeline('@RepLeeZeldin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  peterking <- get_timeline('@RepPeteKing', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomreed <- get_timeline('@RepTomReed', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnkatko <- get_timeline('@RepJohnKatko', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chriscollins <- get_timeline('@RepChrisCollins', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stevechabot <- get_timeline('@RepSteveChabot', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bradwenstrup<- get_timeline('@RepBradWenstrup', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimjordan<- get_timeline('@Jim_Jordan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  boblatta <- get_timeline('@boblatta', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  billjohnson <- get_timeline('@RepBillJohnson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bobgibbs <- get_timeline('@RepBobGibbs', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  warrendavidson <- get_timeline('@WarrenDavidson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  miketurner <- get_timeline('@RepMikeTurner', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  balderson <- get_timeline('@RepBalderson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davejoyce <- get_timeline('@RepDaveJoyce', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stevestivers <- get_timeline('@RepSteveStivers', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  agonzalez<- get_timeline('@RepAGonzalez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kevinhern <- get_timeline('@repkevinhern', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mullin <- get_timeline('@RepMullin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  franklucas <- get_timeline('@RepFrankLucas', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomcole <- get_timeline('@TomColeOK04', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brianfitz <- get_timeline('@RepBrianFitz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  meuser <- get_timeline('@RepMeuser', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  scottperry <- get_timeline('@RepScottPerry', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  smucker <- get_timeline('@RepSmucker', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnjoyce <- get_timeline('@RepJohnJoyce', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  greschenthaler <- get_timeline('@GReschenthaler', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gt <- get_timeline('@CongressmanGT', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikekelly <- get_timeline('@MikeKellyPA', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joewilson <- get_timeline('@RepJoeWilson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jeffduncan <- get_timeline('@RepJeffDuncan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  timmons <- get_timeline('@reptimmons', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ralphnorman <- get_timeline('@RepRalphNorman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomrice <- get_timeline('@RepTomRice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dustyjohnson <- get_timeline('@RepDustyJohnson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  philroe <- get_timeline('@DrPhilRoe', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  timburchett <- get_timeline('@RepTimBurchett', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chuck <- get_timeline('@RepChuck', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  desjarlais <- get_timeline('@DesJarlaisTN04l', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnrose <- get_timeline('@RepJohnRose', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  markgreen <- get_timeline('@RepMarkGreen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidkustoff <- get_timeline('@RepDavidKustoff', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  louiegohmert <- get_timeline('@replouiegohmert', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dancrenshaw<- get_timeline('@RepDanCrenshaw', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  vantaylor <- get_timeline('@RepVanTaylor', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ratcliffe <- get_timeline('@RepRatcliffe', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lancegooden <- get_timeline('@RepLanceGooden', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ronwright <- get_timeline('@RepRonWright', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kevinbrady <- get_timeline('@RepKevinBrady', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mccaul <- get_timeline('@RepMcCaul', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  conaway <- get_timeline('@ConawayTX11', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kaygranger <- get_timeline('@RepKayGranger', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mac <- get_timeline('@MacTXPress', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  randy <- get_timeline('@TXRandy14', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  billflores <- get_timeline('@RepBillFlores', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  arrington <- get_timeline('@RepArrington', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chiproy <- get_timeline('@RepChipRoy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  peteolson <- get_timeline('@RepPeteOlson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hurd <- get_timeline('@HurdOnTheHill', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  kenmarchant <- get_timeline('@RepKenMarchant', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rwilliams <- get_timeline('@RepRWilliams', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  michaelburgess <- get_timeline('@michaelcburgess', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cloud<- get_timeline('@RepCloudTX', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  carter <- get_timeline('@JudgeCarter', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brianbabin <- get_timeline('@RepBrianBabin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  robbishop <- get_timeline('@RepRobBishop', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chrisstewart <- get_timeline('@RepChrisStewart', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johncurtis <- get_timeline('@RepJohnCurtis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  robwittman <- get_timeline('@RobWittman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  riggleman <- get_timeline('@RepRiggleman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bencline <- get_timeline('@RepBenCline', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mgriffith <- get_timeline('@RepMGriffith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  newhouse <- get_timeline('@RepNewhouse', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bryansteil <- get_timeline('@RepBryanSteil', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jim <- get_timeline('@JimPressOffice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  grothman <- get_timeline('@RepGrothman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  seanduffy <- get_timeline('@RepSeanDuffy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gallagher <- get_timeline('@RepGallagher', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mckinley <- get_timeline('@RepMcKinley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  alexmooney <- get_timeline('@RepAlexMooney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)




















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



  donyoungtweets <- read_csv("data/donyoung.csv")
  byrnetweets <- read_csv("data/byrne.csv")
  mikerogerstweets <- read_csv("data/mikerogers.csv")
  robertaderholttweets <- read_csv("data/robertaderholt.csv")
  mobrookstweets <- read_csv("data/mobrooks.csv")
  garypalmertweets <- read_csv("data/garypalmer.csv")
  rickcrawfordtweets <- read_csv("data/rickcrawford.csv")
  frenchhilltweets <- read_csv("data/frenchhill.csv")
  stevewomacktweets <- read_csv("data/stevewomack.csv")
  westermantweets <- read_csv("data/westerman.csv")
  gosartweets <- read_csv("data/gosar.csv")
  andybiggstweets <- read_csv("data/andybiggs.csv")
  davidtweets <- read_csv("data/david.csv")
  dleskotweets <- read_csv("data/dlesko.csv")
  lamalfatweets <- read_csv("data/lamalfa.csv")
  mcclintocktweets <- read_csv("data/mcclintock.csv")
  paulcooktweets <- read_csv("data/paulcook.csv")
  devinnunestweets <- read_csv("data/devinnunes.csv")
  kevinmccarthytweets <- read_csv("data/kevinmccarthy.csv")
  kencalverttweets <- read_csv("data/kencalvert.csv")
  tiptontweets <- read_csv("data/tipton.csv")
  kenbucktweets <- read_csv("data/kenbuck.csv")
  dlamborntweets <- read_csv("data/dlamborn.csv")
  mattgaetztweets <- read_csv("data/mattgaetz.csv")
  nealdunntweets <- read_csv("data/nealdunn.csv")
  tedyohotweets <- read_csv("data/tedyoho.csv")
  rutherfordtweets <- read_csv("data/rutherford.csv")
  michaelwaltztweets <- read_csv("data/michaelwaltz.csv")
  billposeytweets <- read_csv("data/billposey.csv")
  webstertweets <- read_csv("data/webster.csv")
  gusbilirakistweets <- read_csv("data/gusbilirakis.csv")
  vernbuchanantweets <- read_csv("data/vernbuchanan.csv")
  gregsteubetweets <- read_csv("data/gregsteube.csv")
  brianmasttweets <-  read_csv("data/brianmast.csv")
  rooneytweets <- read_csv("data/rooney.csv")
  mariodbtweets <-  read_csv("data/mariodb.csv")
  buddycartertweets <- read_csv("data/buddycarter.csv")
  drewfergusontweets <- read_csv("data/drewferguson.csv")
  robwoodalltweets <- read_csv("data/robwoodall.csv")
  austinscotttweets <- read_csv("data/austinscott.csv")
  dougcollinstweets <- read_csv("data/dougcollins.csv")
  hicetweets <- read_csv("data/hice.csv")
  loudermilktweets <- read_csv("data/loudermilk.csv")
  rickallentweets <- read_csv("data/rickallen.csv")
  tomgravestweets <- read_csv("data/tomgraves.csv")
  russfulchertweets <- read_csv("data/russfulcher.csv")
  mikesimpsontweets <- read_csv("data/mikesimpson.csv")
  bosttweets <- read_csv("data/bost.csv")
  rodneydavistweets <- read_csv("data/rodneydavis.csv")
  shimkustweets <- read_csv("data/shimkus.csv")
  kinzingertweets <- read_csv("data/kinzinger.csv")
  lahoodtweets <- read_csv("data/lahood.csv")
  jimbankstweets <- read_csv("data/jimbanks.csv")
  jimbairdtweets <- read_csv("data/jimbaird.csv")
  gregpencetweets <- read_csv("data/gregpence.csv")
  larrybucshontweets <- read_csv("data/larrybucshon.csv")
  treytweets <- read_csv("data/trey.csv")
  ronestestweets <- read_csv("data/ronestes.csv")
  guthrietweets <- read_csv("data/guthrie.csv")
  thomasmassietweets <- read_csv("data/thomasmassie.csv")
  halrogerstweets <- read_csv("data/halrogers.csv")
  andybarrtweets <- read_csv("data/andybarr.csv")
  stevescalisetweets <- read_csv("data/stevescalise.csv")
  clayhigginstweets <- read_csv("data/clayhiggins.csv")
  mikejohnsontweets <- read_csv("data/clayhiggins.csv")
  abrahamtweets <-  read_csv("data/abraham.csv")
  garretgravestweets <- read_csv("data/garretgraves.csv")
  andyharristweets <- read_csv("data/andyharris.csv")
  jackbergmantweets <- read_csv("data/jackbergman.csv")
  huizengatweets <- read_csv("data/huizenga.csv")
  justinamashtweets <- read_csv("data/justinamash.csv")
  moolenaartweets <- read_csv("data/moolenaar.csv")
  freduptontweets <- read_csv("data/fredupton.csv")
  walbergtweets <- read_csv("data/walberg.csv")
  slotkintweets <- read_csv("data/slotkin.csv")
  paulmitchelltweets <- read_csv("data/paulmitchell.csv")
  hagedorntweets <- read_csv("data/hagedorn.csv")
  tomemmertweets <- read_csv("data/tomemmer.csv")
  petestaubertweets <- read_csv("data/petestauber.csv")
  blainetweets <- read_csv("data/blaine.csv")
  samgravestweets <- read_csv("data/samgraves.csv")
  longtweets <- read_csv("data/long.csv")
  jasonsmithtweets <- read_csv("data/jasonsmith.csv")
  trentkellytweets <- read_csv("data/trentkelly.csv")
  michaelguesttweets <- read_csv("data/michaelguest.csv")
  palazzotweets <- read_csv("data/palazzo.csv")
  holdingtweets <- read_csv("data/holding.csv")
  walterjonestweets <- read_csv("data/holding.csv")
  markwalkertweets <- read_csv("data/markwalker.csv")
  davidrouzertweets <- read_csv("data/davidrouzer.csv")
  richhudsontweets <- read_csv("data/richhudson.csv")
  patrickmchenrytweets <- read_csv("data/patrickmchenry.csv")
  markmeadowstweets <- read_csv("data/markmeadows.csv")
  tedbuddtweets <-  read_csv("data/tedbudd.csv")
  jefffortenberrytweets <- read_csv("data/jefffortenberry.csv")
  donbacontweets <- read_csv("data/donbacon.csv")
  adriansmithtweets <- read_csv("data/adriansmith.csv")
  jvdtweets <- read_csv("data/jvd.csv")
  chrissmithtweets <- read_csv("data/chrissmith.csv")
  markamodeitweets <- read_csv("data/markamodei.csv")
  leezeldintweets <- read_csv("data/leezeldin.csv")
  peterkingtweets <- read_csv("data/peterking.csv")
  tomreedtweets <- read_csv("data/tomreed.csv")
  johnkatkotweets <- read_csv("data/johnkatko.csv")
  chriscollinstweets <- read_csv("data/chriscollins.csv")
  stevechabottweets <- read_csv("data/stevechabot.csv")
  bradwenstruptweets <- read_csv("data/bradwenstrup.csv")
  jimjordantweets <- read_csv("data/jimjordan.csv")
  boblattatweets <- read_csv("data/boblatta.csv")
  billjohnsontweets <- read_csv("data/billjohnson.csv")
  bobgibbstweets <- read_csv("data/bobgibbs.csv")
  warrendavidsontweets <- read_csv("data/warrendavidson.csv")
  miketurnertweets <- read_csv("data/miketurner.csv")
  baldersontweets <- read_csv("data/balderson.csv")
  davejoycetweets <- read_csv("data/davejoyce.csv")
  stevestiverstweets <- read_csv("data/stevestivers.csv")
  agonzaleztweets <- read_csv("data/agonzalez.csv")
  kevinherntweets <- read_csv("data/kevinhern.csv")
  mullintweets <- read_csv("data/mullin.csv")
  franklucastweets <- read_csv("data/franklucas.csv")
  tomcoletweets <- read_csv("data/tomcole.csv")
  brianfitztweets <- read_csv("data/brianfitz.csv")
  meusertweets <- read_csv("data/meuser.csv")
  scottperrytweets <- read_csv("data/scottperry.csv")
  smuckertweets <- read_csv("data/smucker.csv")
  johnjoycetweets <- read_csv("data/johnjoyce.csv")
  greschenthalertweets <- read_csv("data/greschenthaler.csv")
  gttweets <- read_csv("data/gt.csv")
  mikekellytweets <- read_csv("data/mikekelly.csv")
  joewilsontweets <- read_csv("data/joewilson.csv")
  jeffduncantweets <-  read_csv("data/jeffduncan.csv")
  timmonstweets <- read_csv("data/timmons.csv")
  ralphnormantweets <- read_csv("data/ralphnorman.csv")
  tomricetweets <- read_csv("data/tomrice.csv")
  dustyjohnsontweets <- read_csv("data/dustyjohnson.csv")
  philroetweets <- read_csv("data/philroe.csv")
  timburchetttweets <- read_csv("data/timburchett.csv")
  chucktweets <-  read_csv("data/chuck.csv")
  desjarlaistweets <- read_csv("data/desjarlais.csv")
  johnrosetweets <- read_csv("data/johnrose.csv")
  markgreentweets <- read_csv("data/markgreen.csv")
  davidkustofftweets <- read_csv("data/davidkustoff.csv")
  louiegohmerttweets <- read_csv("data/louiegohmert.csv")
  dancrenshawtweets <- read_csv("data/dancrenshaw.csv")
  vantaylortweets <- read_csv("data/vantaylor.csv")
  ratcliffetweets <- read_csv("data/ratcliffe.csv")
  lancegoodentweets <- read_csv("data/lancegooden.csv")
  ronwrighttweets <- read_csv("data/ronwright.csv")
  kevinbradytweets <- read_csv("data/kevinbrady.csv")
  mccaultweets <- read_csv("data/mccaul.csv")
  conawaytweets <- read_csv("data/conaway.csv")
  kaygrangertweets <- read_csv("data/kaygranger.csv")
  mactweets <- read_csv("data/mac.csv")
  randytweets <- read_csv("data/randy.csv")
  billflorestweets <- read_csv("data/billflores.csv")
  arringtontweets <- read_csv("data/arrington.csv")
  chiproytweets <- read_csv("data/chiproy.csv")
  peteolsontweets <- read_csv("data/peteolson.csv")
  hurdtweets <- read_csv("data/hurd.csv")
  kenmarchanttweets <- read_csv("data/kenmarchant.csv")
  rwilliamstweets <- read_csv("data/rwilliams.csv")
  michaelburgesstweets <- read_csv("data/michaelburgess.csv")
  cloudtweets <- read_csv("data/cloud.csv")
  cartertweets <- read_csv("data/carter.csv")
  brianbabintweets <- read_csv("data/brianbabin.csv")
  robbishoptweets <- read_csv("data/robbishop.csv")
  chrisstewarttweets <- read_csv("data/chrisstewart.csv")
  johncurtistweets <- read_csv("data/johncurtis.csv")
  robwittmantweets <- read_csv("data/robwittman.csv")
  rigglemantweets <- read_csv("data/riggleman.csv")
  benclinetweets <- read_csv("data/bencline.csv")
  mgriffithtweets <- read_csv("data/mgriffith.csv")
  newhousetweets <- read_csv("data/newhouse.csv")
  bryansteiltweets <- read_csv("data/bryansteil.csv")
  jimtweets <- read_csv("data/jim.csv")
  grothmantweets <- read_csv("data/grothman.csv")
  seanduffytweets <- read_csv("data/seanduffy.csv")
  gallaghertweets <- read_csv("data/gallagher.csv")
  mckinleytweets <- read_csv("data/mckinley.csv")
  alexmooneytweets <- read_csv("data/alexmooney.csv")



  hormrtweets <- bind_rows(donyoungtweets %>%
                             mutate(person = "Don Young"),
                           byrnetweets %>%
                             mutate(person = "Byrne"),
                           mikerogerstweets %>%
                             mutate(person = "Mike Rogers"),
                           robertaderholttweets %>%
                             mutate(person = "Robert Aderholt"),
                           mobrookstweets %>%
                             mutate(person = "Mo Brooks"),
                           garypalmertweets %>%
                             mutate(person = "Gary Palmer"),
                           rickcrawfordtweets %>%
                             mutate(person = "Rick Crawford"),
                           frenchhilltweets %>%
                             mutate(person = "French Hill"),
                           stevewomacktweets %>%
                             mutate(person = "Steve Womack"),
                           westermantweets %>%
                             mutate(person = "Westerman"),
                           gosartweets %>%
                             mutate(person = "Gosar"),
                           andybiggstweets %>%
                             mutate(person = "Andy Biggs"),
                           davidtweets %>%
                             mutate(person = "David"),
                           dleskotweets %>%
                             mutate(person = "D Lesko"),
                           lamalfatweets %>%
                             mutate(person = "Lamalfa"),
                           mcclintocktweets %>%
                             mutate(person = "McClintock"),
                           paulcooktweets %>%
                             mutate(person = "Paul Cook"),
                           devinnunestweets %>%
                             mutate(person = "Devin Nunes"),
                           kevinmccarthytweets %>%
                             mutate(person = "Kevin McCarthy"),
                           kencalverttweets %>%
                             mutate(person = "Ken Calvert"),
                           tiptontweets %>%
                             mutate(person = "Tipton"),
                           kenbucktweets %>%
                             mutate(person = "Ken Buck"),
                           dlamborntweets %>%
                             mutate(person = "D Lamborn"),
                           mattgaetztweets %>%
                             mutate(person = "Matt Gaetz"),
                           nealdunntweets %>%
                             mutate(person = "Neal Dunn"),
                           tedyohotweets %>%
                             mutate(person = "Ted Yoho"),
                           rutherfordtweets %>%
                             mutate(person = "Rutherford"),
                           michaelwaltztweets %>%
                             mutate(person = "Michael Waltz"),
                           billposeytweets %>%
                             mutate(person = "Bill Posey"),
                           webstertweets %>%
                             mutate(person = "Webster"),
                           gusbilirakistweets %>%
                             mutate(person = "Gus Bilirakis"),
                           vernbuchanantweets %>%
                             mutate(person = "Vern Buchanon"),
                           gregsteubetweets %>%
                             mutate(person = "Greg Steube"),
                           brianmasttweets %>%
                             mutate(person = "Brian Mast"),
                           rooneytweets %>%
                             mutate(person = "Rooney"),
                           mariodbtweets %>%
                             mutate(person = "Mario DB"),
                           buddycartertweets %>%
                             mutate(person = "Buddy Carter"),
                           drewfergusontweets %>%
                             mutate(person = "Drew Ferguson"),
                           robwoodalltweets %>%
                             mutate(person = "Rob Woodall"),
                           austinscotttweets %>%
                             mutate(person = "Austin Scott"),
                           dougcollinstweets %>%
                             mutate(person = "Doug Collins"),
                           hicetweets %>%
                             mutate(person = "Hice"),
                           loudermilktweets %>%
                             mutate(person = "Loudermilk"),
                           rickallentweets %>%
                             mutate(person = "Rick Allen"),
                           tomgravestweets %>%
                             mutate(person = "Tom Graves"),
                           russfulchertweets %>%
                             mutate(person = "Russ Fulcher"),
                           mikesimpsontweets %>%
                             mutate(person = "Mike Simpson"),
                           bosttweets %>%
                             mutate(person = "Bost"),
                           rodneydavistweets %>%
                             mutate(person = "Rodney Davis"),
                           shimkustweets %>%
                             mutate(person = "Shimkus"),
                           kinzingertweets %>%
                             mutate(person = "Kinzinger"),
                           lahoodtweets %>%
                             mutate(person = "Lahood"),
                           jimbankstweets %>%
                             mutate(person = "Jim Banks"),
                           jimbairdtweets %>%
                             mutate(person = "Jim Baird"),
                           gregpencetweets %>%
                             mutate(person = "Greg Pence"),
                           larrybucshontweets %>%
                             mutate(person = "Larry Bucschon"),
                           treytweets %>%
                             mutate(person = "Trey"),
                           ronestestweets %>%
                             mutate(person = "Ron Estes"),
                           guthrietweets %>%
                             mutate(person = "Guthrie"),
                           thomasmassietweets %>%
                             mutate(person = "Thomas Massie"),
                           halrogerstweets %>%
                             mutate(person = "Hal Rogers"),
                           andybarrtweets %>%
                             mutate(person = "Andy Barr"),
                           stevescalisetweets %>%
                             mutate(person = "Steve Scalise"),
                           clayhigginstweets %>%
                             mutate(person = "Clay Higgins"),
                           mikejohnsontweets %>%
                             mutate(person = "Mike Johnson"),
                           abrahamtweets %>%
                             mutate(person = "Abraham"),
                           garretgravestweets %>%
                             mutate(person = "Garret Graves"),
                           andyharristweets %>%
                             mutate(person = "Andy Harris"),
                           jackbergmantweets %>%
                             mutate(person = "Jack Bergman"),
                           huizengatweets %>%
                             mutate(person = "Huizenga"),
                           justinamashtweets %>%
                             mutate(person = "Justin Amash"),
                           moolenaartweets %>%
                             mutate(person = "Moolenaar"),
                           freduptontweets %>%
                             mutate(person = "Fred Upton"),
                           walbergtweets %>%
                             mutate(person = "Walberg"),
                           slotkintweets %>%
                             mutate(person = "Slotkin"),
                           paulmitchelltweets %>%
                             mutate(person = "Paul Mitchell"),
                           hagedorntweets %>%
                             mutate(person = "Hagedorn"),
                           tomemmertweets %>%
                             mutate(person = "Tom Emmer"),
                           petestaubertweets %>%
                             mutate(person = "Peter Stauber"),
                           blainetweets %>%
                             mutate(person = "Blaine"),
                           samgravestweets %>%
                             mutate(person = "Sam Graves"),
                           longtweets %>%
                             mutate(person = "Long"),
                           jasonsmithtweets %>%
                             mutate(person = "Jason Smith"),
                           trentkellytweets %>%
                             mutate(person = "Trent Kelly"),
                           michaelguesttweets %>%
                             mutate(person = "Michael Guest"),
                           palazzotweets %>%
                             mutate(person = "Palazzo"),
                           holdingtweets %>%
                             mutate(person = "Holding"),
                           walterjonestweets %>%
                             mutate(person = "Walter Jones"),
                           markwalkertweets %>%
                             mutate(person = "Mark Walker"),
                           davidrouzertweets %>%
                             mutate(person = "David Rouzer"),
                           richhudsontweets %>%
                             mutate(person = "Rich Hudson"),
                           patrickmchenrytweets %>%
                             mutate(person = "Patrick McHenry"),
                           markmeadowstweets %>%
                             mutate(person = "Mark Meadows"),
                           tedbuddtweets %>%
                             mutate(person = "Ted Budd"),
                           jefffortenberrytweets %>%
                             mutate(person = "Jeff Fortenberry"),
                           donbacontweets %>%
                             mutate(person = "Don Bacon"),
                           adriansmithtweets %>%
                             mutate(person = "Adrian Smith"),
                           jvdtweets %>%
                             mutate(person = "JVD"),
                           chrissmithtweets %>%
                             mutate(person = "Chris Smith"),
                           markamodeitweets %>%
                             mutate(person = "Mark Modei"),
                           leezeldintweets %>%
                             mutate(person = "Lee Zeldin"),
                           peterkingtweets %>%
                             mutate(person = "Peter King"),
                           tomreedtweets %>%
                             mutate(person = "Tom Reed"),
                           johnkatkotweets %>%
                             mutate(person = "John Katko"),
                           chriscollinstweets %>%
                             mutate(person = "Chris Collins"),
                           stevechabottweets %>%
                             mutate(person = "Steve Chabot"),
                           bradwenstruptweets %>%
                             mutate(person = "Braden Strup"),
                           jimjordantweets %>%
                             mutate(person = "Jim Jordan"),
                           boblattatweets %>%
                             mutate(person = "Bob Latta"),
                           billjohnsontweets %>%
                             mutate(person = "Bill Johnson"),
                           bobgibbstweets %>%
                             mutate(person = "Bob Gibbs"),
                           warrendavidsontweets %>%
                             mutate(person = "Warren Davidson"),
                           miketurnertweets %>%
                             mutate(person = "Mike Turner"),
                           baldersontweets %>%
                             mutate(person = "Balderson"),
                           davejoycetweets %>%
                             mutate(person = "Dave Joycet"),
                           stevestiverstweets %>%
                             mutate(person = "Steve Stivers"),
                           agonzaleztweets %>%
                             mutate(person = "A Gonzalez"),
                           kevinherntweets %>%
                             mutate(person = "Kevin Hern"),
                           mullintweets %>%
                             mutate(person = "Mullin"),
                           franklucastweets %>%
                             mutate(person = "Frank Lucas"),
                           tomcoletweets %>%
                             mutate(person = "Tom Cole"),
                           brianfitztweets %>%
                             mutate(person = "Brian Fitz"),
                           meusertweets %>%
                             mutate(person = "Meuser"),
                           scottperrytweets %>%
                             mutate(person = "Scott Perry"),
                           smuckertweets %>%
                             mutate(person = "Smucker"),
                           johnjoycetweets %>%
                             mutate(person = "John Joyce"),
                           greschenthalertweets %>%
                             mutate(person = "Greschenthaler"),
                           gttweets %>%
                             mutate(person = "GT"),
                           mikekellytweets %>%
                             mutate(person = "Mike Kelly"),
                           joewilsontweets %>%
                             mutate(person = "Joe Wilson"),
                           jeffduncantweets %>%
                             mutate(person = "Jeff Duncan"),
                           timmonstweets %>%
                             mutate(person = "Timmons"),
                           ralphnormantweets %>%
                             mutate(person = "Ralph Norman"),
                           tomricetweets %>%
                             mutate(person = "Tom Rice"),
                           dustyjohnsontweets %>%
                             mutate(person = "Dusty Johnson"),
                           philroetweets %>%
                             mutate(person = "Phil Roe"),
                           timburchetttweets %>%
                             mutate(person = "Tim Burchett"),
                           chucktweets %>%
                             mutate(person = "Chuck"),
                           desjarlaistweets %>%
                             mutate(person = "Des Jarlais"),
                           johnrosetweets %>%
                             mutate(person = "John Rose"),
                           markgreentweets %>%
                             mutate(person = "Mark Green"),
                           davidkustofftweets %>%
                             mutate(person = "David Kustoff"),
                           louiegohmerttweets %>%
                             mutate(person = "Louie Gohmer"),
                           dancrenshawtweets %>%
                             mutate(person = "Dan Crenshaw"),
                           vantaylortweets %>%
                             mutate(person = "Van Taylor"),
                           ratcliffetweets %>%
                             mutate(person = "Ratcliffe"),
                           lancegoodentweets %>%
                             mutate(person = "Lance Gooden"),
                           ronwrighttweets %>%
                             mutate(person = "Ron Wright"),
                           kevinbradytweets %>%
                             mutate(person = "Kevin Brady"),
                           mccaultweets %>%
                             mutate(person = "McCaul"),
                           conawaytweets %>%
                             mutate(person = "Conaway"),
                           kaygrangertweets %>%
                             mutate(person = "Kay Granger"),
                           mactweets %>%
                             mutate(person = "Mac"),
                           randytweets %>%
                             mutate(person = "Randy"),
                           billflorestweets %>%
                             mutate(person = "Bill Flores"),
                           arringtontweets %>%
                             mutate(person = "Arrington"),
                           chiproytweets %>%
                             mutate(person = "Chip Roy"),
                           peteolsontweets %>%
                             mutate(person = "Pete Olson"),
                           hurdtweets %>%
                             mutate(person = "Hurd"),
                           kenmarchanttweets %>%
                             mutate(person = "Ken Marchant"),
                           rwilliamstweets %>%
                             mutate(person = "R Williams"),
                           michaelburgesstweets %>%
                             mutate(person = "Michael Burgess"),
                           cloudtweets %>%
                             mutate(person = "Cloud"),
                           cartertweets %>%
                             mutate(person = "Carter"),
                           brianbabintweets %>%
                             mutate(person = "Brian Babin"),
                           robbishoptweets %>%
                             mutate(person = "Robbi Shop"),
                           chrisstewarttweets %>%
                             mutate(person = "Chris Stewart"),
                           johncurtistweets %>%
                             mutate(person = "John Curtis"),
                           robwittmantweets %>%
                             mutate(person = "Rob Wittman"),
                           rigglemantweets %>%
                             mutate(person = "Riggleman"),
                           benclinetweets %>%
                             mutate(person = "Ben Cline"),
                           mgriffithtweets %>%
                             mutate(person = "M Griffith"),
                           newhousetweets %>%
                             mutate(person = "Newhouse"),
                           bryansteiltweets %>%
                             mutate(person = "Bryan Steil"),
                           jimtweets %>%
                             mutate(person = "Jim"),
                           grothmantweets %>%
                             mutate(person = "Grothman"),
                           seanduffytweets %>%
                             mutate(person = "Sean Duffy"),
                           gallaghertweets %>%
                             mutate(person = "Gallagher"),
                           mckinleytweets %>%
                             mutate(person = "McKinley"),
                           alexmooneytweets %>%
                             mutate(person = "Alex Mooney"))

  save_as_csv(hormrtweets, "data/hormrtweets.csv", prepend_ids = TRUE , na = "", fileEncoding = "UTF-8")
  hormrtweets <- read_csv("data/hormrtweets.csv")
}

hormaleD <- function() {
  ohalleran <- get_timeline('@RepOHalleran', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  raulgrijalva<- get_timeline('@RepRaulGrijalva', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rubengallego <- get_timeline('@RepRubenGallego', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gregstanton<- get_timeline('@RepGregStanton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  garamendi<- get_timeline('@RepGaramendi', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  thompson<- get_timeline('@RepThompson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mcnerney <- get_timeline('@RepMcNerney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joshharder <- get_timeline('@RepJoshHarder', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  desaulnier<- get_timeline('@RepDeSaulnier', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  swalwell<- get_timeline('@RepSwalwell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimcosta <- get_timeline('@RepJimCosta', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  rokhanna <- get_timeline('@RepRoKhanna', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimmypanetta <- get_timeline('@RepJimmyPanetta', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tjcox<- get_timeline('@RepTjCox', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  carbajal <- get_timeline('@RepCarbajal', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  adamschiff <- get_timeline('@RepAdamSchiff', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cardenas<- get_timeline('@RepCardenas', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bradsherman <- get_timeline('@BradSherman', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  peteaguilar<- get_timeline('@RepPeteAguilar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tedlieu<- get_timeline('@RepTedLieu', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimmygomez <- get_timeline('@RepJimmyGomez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ruiz<- get_timeline('@CongressmanRuiz', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gilcisneros <- get_timeline('@RepGilCisneros', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  marktakano<- get_timeline('@RepMarkTakano', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  loucorrea<- get_timeline('@RepLouCorrea', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lowenthal<- get_timeline('@RepLowenthal', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  harley<- get_timeline('@RepHarley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikelevin <- get_timeline('@RepMikeLevin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hunter<- get_timeline('@Rep_Hunter', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  juanvargas <- get_timeline('@RepJuanVargas', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  scottpeters<- get_timeline('@RepScottPeters', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joeneguse<- get_timeline('@RepJoeNeguse', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jasoncrow<- get_timeline('@RepJasonCrow', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  perlmutter<- get_timeline('@RepPerlmutter', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnlarson<- get_timeline('@RepJohnLarson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joecourtney<- get_timeline('@RepJoeCourtney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jahimes<- get_timeline('@jahimes', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  allawson <- get_timeline('@RepAlLawsonJr', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  darrensoto <- get_timeline('@RepDarrenSoto', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  valdemings<- get_timeline('@RepValDemings', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  charliecrist<- get_timeline('@RepCharlieCrist', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  teddeutch<- get_timeline('@RepTedDeutch', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  sanfordbishop<- get_timeline('@SanfordBishop', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hankjohnson<- get_timeline('@RepHankJohnson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnlewis<- get_timeline('@repjohnlewis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lucymcbath<- get_timeline('@RepLucyMcBath', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidscott<- get_timeline('@repdavidscott', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  edcase<- get_timeline('@RepEdCase', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  daveloebsack <- get_timeline('@daveloebsack', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bobbyrush<- get_timeline('@RepBobbyRush', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lipinski<- get_timeline('@RepLipinski', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chuygarcia <- get_timeline('@RepChuyGarcia', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikequigley<- get_timeline('@RepMikeQuigley', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  casten<- get_timeline('@RepCasten', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dannydavis <- get_timeline('@RepDannyDavis', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  raja<- get_timeline('@CongressmanRaja', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  schneider <- get_timeline('@RepSchneider', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  billfoster<- get_timeline('@RepBillFoster', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  visclosky<- get_timeline('@RepVisclosky', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andrecarson<- get_timeline('@RepAndreCarson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  johnyarmuth<- get_timeline('@RepJohnYarmuth', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  richmond<- get_timeline('@RepRichmond', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chelliepingree <- get_timeline('@chelliepingree', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  golden<- get_timeline('@RepGolden', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dutch <- get_timeline('@Call_Me_Dutch', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  sarbanes <- get_timeline('@RepSarbanes', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  anthonybrown <- get_timeline('@RepAnthonyBrown', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  hoyer<- get_timeline('@LeaderHoyer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidtrone<- get_timeline('@RepDavidTrone', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cummings<- get_timeline('@RepCummings', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  raskin <- get_timeline('@RepRaskin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  richardneal <- get_timeline('@RepRichardNeal', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mcgovern<- get_timeline('@RepMcGovern', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joekennedy <- get_timeline('@RepJoeKennedy', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  moulton<- get_timeline('@teammoulton', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  stephenlynch <- get_timeline('@RepStephenLynch', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  keating<- get_timeline('@USRepKeating', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dankildee <- get_timeline('@RepDanKildee', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andylevin<- get_timeline('@RepAndyLevin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cleaver<- get_timeline('@repcleaver', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  benniethompson <- get_timeline('@BennieGThompson', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gkbutterfield<- get_timeline('@GKButterfield', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidprice<- get_timeline('@RepDavidEPrice', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  chrispappas<- get_timeline('@RepChrisPappas', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  donaldnorcross<- get_timeline('@DonaldNorcross', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  andykim<- get_timeline('@RepAndyKimNJ', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joshg <- get_timeline('@RepJoshG', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  frankpallone <- get_timeline('@FrankPallone', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  malinowski<- get_timeline('@RepMalinowski', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  sires<- get_timeline('@RepSires', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  billpascrell <- get_timeline('@BillPascrell', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  donaldpayne<- get_timeline('@RepDonaldPayne', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  sherrill<- get_timeline('@RepSherrill', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  benraylujan <- get_timeline('@repbenraylujan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  horsford<- get_timeline('@RepHorsford', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  tomsuozzi <- get_timeline('@RepTomSuozzi', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gregorymeeks<- get_timeline('@RepGregoryMeeks', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gracemeng<- get_timeline('@RepGraceMeng', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jeffries<- get_timeline('@RepJeffries', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jerrynadler <- get_timeline('@RepJerryNadler', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  maxrose<- get_timeline('@RepMaxRose', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  espaillat <- get_timeline('@RepEspaillat', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joseserrano<- get_timeline('@RepJoseSerrano', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  eliotengel<- get_timeline('@RepEliotEngel', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  seanmaloney<- get_timeline('@RepSeanMaloney', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  delgado<- get_timeline('@repdelgado', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  paultonko <- get_timeline('@RepPaulTonko', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brindisi<- get_timeline('@RepBrindisi', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joemorelle <- get_timeline('@RepJoeMorelle', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  brianhiggins<- get_timeline('@RepBrianHiggins', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  timryan<- get_timeline('@RepTimRyan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gregwalden <- get_timeline('@repgregwalden', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  blumenauer<- get_timeline('@repblumenauer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  peterdefazio<- get_timeline('@RepPeterDeFazio', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  schrader<- get_timeline('@RepSchrader', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  boyle <- get_timeline('@CongBoyle', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dwightevans <- get_timeline('@RepDwightEvans', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cartwright<- get_timeline('@RepCartwright', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  conorlamb<- get_timeline('@RepConorLamb', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mikedoyle<- get_timeline('@USRepMikeDoyle', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  davidcicilline<- get_timeline('@davidcicilline', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimlangevin<- get_timeline('@JimLangevin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cunningham<- get_timeline('@RepCunningham', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  clyburn<- get_timeline('@WhipClyburn', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  jimcooper <- get_timeline('@repjimcooper', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cohen<- get_timeline('@RepCohen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  algreen <- get_timeline('@RepAlGreen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gonzalez <- get_timeline('@RepGonzalez', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  joaquincastro <- get_timeline('@JoaquinCastrotx', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  cuellar<- get_timeline('@RepCuellar', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ebj <- get_timeline('@RepEBJ', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  colinallred <- get_timeline('@RepColinAllred', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  veasey<- get_timeline('@RepVeasey', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  filemonvela <- get_timeline('@RepFilemonVela', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  lloyddoggett<- get_timeline('@RepLloydDoggett', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  benmcadams<- get_timeline('@RepBenMcAdams', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  bobbyscott<- get_timeline('@BobbyScott', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  mceachin<- get_timeline('@RepMcEachin', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  donbeyer <- get_timeline('@RepDonBeyer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  gerryconnolly <- get_timeline('@GerryConnolly', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  peterwelch<- get_timeline('@PeterWelch', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ricklarsen<- get_timeline('@RepRickLarsen', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  derekkilmer<- get_timeline('@RepDerekKilmer', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  adamsmith<- get_timeline('@RepAdamSmith', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  dennyheck<- get_timeline('@RepDennyHeck', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  markpocan<- get_timeline('@repmarkpocan', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  ronkind<- get_timeline('@RepRonKind', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)
  carolmiller <- get_timeline('@RepCarolMiller', n = 50, max_id = NULL, home = FALSE, parse = TRUE, check = FALSE, token = token, include_rts = FALSE)











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


  ohallerantweets <- read_csv("data/ohalleran.csv")
  raulgrijalvatweets <- read_csv("data/raulgrijalva.csv")
  rubengallegotweets <- read_csv("data/rubengallego.csv")
  gregstantontweets <- read_csv("data/gregstanton.csv")
  garamendtweets <- read_csv("data/garamendi.csv")
  thompsontweets <- read_csv("data/thompson.csv")
  mcnerneytweets <- read_csv("data/mcnerney.csv")
  joshhardertweets <- read_csv("data/joshharder.csv")
  desaulniertweets <- read_csv("data/desaulnier.csv")
  swalwelltweets <- read_csv("data/swalwell.csv")
  jimcostatweets <- read_csv("data/jimcosta.csv")
  rokhannatweets <- read_csv("data/rokhanna.csv")
  jimmypanettatweets <- read_csv("data/jimmypanetta.csv")
  tjcoxtweets <- read_csv("data/tjcox.csv")
  carbajaltweets <- read_csv("data/carbajal.csv")
  adamschifftweets <- read_csv("data/adamschiff.csv")
  cardenastweets <- read_csv("data/cardenas.csv")
  bradshermantweets <- read_csv("data/bradsherman.csv")
  peteaguilartweets <- read_csv("data/peteaguilar.csv")
  tedlieutweets <- read_csv("data/tedlieu.csv")
  jimmygomeztweets <- read_csv("data/jimmygomez.csv")
  ruiztweets <- read_csv("data/ruiz.csv")
  gilcisnerostweets <- read_csv("data/gilcisneros.csv")
  marktakanotweets <- read_csv("data/marktakano.csv")
  loucorreatweets <- read_csv("data/loucorrea.csv")
  lowenthaltweets <- read_csv("data/lowenthal.csv")
  harleytweets <- read_csv("data/harley.csv")
  mikelevintweets <- read_csv("data/mikelevin.csv")
  huntertweets <- read_csv("data/hunter.csv")
  juanvargastweets <- read_csv("data/juanvargas.csv")
  scottpeterstweets <- read_csv("data/scottpeters.csv")
  joenegusetweets <- read_csv("data/joeneguse.csv")
  jasoncrowtweets <- read_csv("data/jasoncrow.csv")
  perlmuttertweets <- read_csv("data/perlmutter.csv")
  johnlarsontweets <- read_csv("data/johnlarson.csv")
  joecourtneytweets <- read_csv("data/joecourtney.csv")
  jahimestweets <- read_csv("data/jahimes.csv")
  allawsontweets <- read_csv("data/allawson.csv")
  darrensototweets <- read_csv("data/darrensoto.csv")
  valdemingstweets <- read_csv("data/valdemings.csv")
  charliecristtweets <- read_csv("data/charliecrist.csv")
  teddeutchtweets <- read_csv("data/teddeutch.csv")
  sanfordbishoptweets <- read_csv("data/sanfordbishop.csv")
  hankjohnsontweets <- read_csv("data/hankjohnson.csv")
  johnlewistweets <- read_csv("data/johnlewis.csv")
  lucymcbathtweets <- read_csv("data/lucymcbath.csv")
  davidscotttweets <- read_csv("data/davidscott.csv")
  edcasetweets <- read_csv("data/edcase.csv")
  daveloebsacktweets <- read_csv("data/daveloebsack.csv")
  bobbyrushtweets <- read_csv("data/bobbyrush.csv")
  lipinskitweets <- read_csv("data/lipinski.csv")
  chuygarciatweets <- read_csv("data/chuygarcia.csv")
  mikequigleytweets <- read_csv("data/mikequigley.csv")
  castentweets <- read_csv("data/casten.csv")
  dannydavistweets <- read_csv("data/dannydavis.csv")
  rajatweets <- read_csv("data/raja.csv")
  schneidertweets <- read_csv("data/schneider.csv")
  billfostertweets <- read_csv("data/billfoster.csv")
  viscloskytweets <- read_csv("data/visclosky.csv")
  andrecarsontweets <- read_csv("data/andrecarson.csv")
  johnyarmuthtweets <- read_csv("data/johnyarmuth.csv")
  richmondtweets <- read_csv("data/richmond.csv")
  chelliepingreetweets <- read_csv("data/chelliepingree.csv")
  goldentweets <- read_csv("data/golden.csv")
  dutchtweets <- read_csv("data/dutch.csv")
  sarbanestweets <- read_csv("data/sarbanes.csv")
  anthonybrowntweets <- read_csv("data/anthonybrown.csv")
  hoyertweets <- read_csv("data/hoyer.csv")
  davidtronetweets <- read_csv("data/davidtrone.csv")
  cummingstweets <- read_csv("data/cummings.csv")
  raskintweets <- read_csv("data/raskin.csv")
  richardnealtweets <- read_csv("data/richardneal.csv")
  mcgoverntweets <- read_csv("data/mcgovern.csv")
  joekennedytweets <- read_csv("data/joekennedy.csv")
  moultontweets <- read_csv("data/moulton.csv")
  stephenlynchtweets <- read_csv("data/stephenlynch.csv")
  keatingtweets <- read_csv("data/keating.csv")
  dankildeetweets <- read_csv("data/dankildee.csv")
  andylevintweets <- read_csv("data/andylevin.csv")
  cleavertweets <- read_csv("data/cleaver.csv")
  benniethompsontweets <- read_csv("data/benniethompson.csv")
  gkbutterfieldtweets <- read_csv("data/gkbutterfield.csv")
  davidpricetweets <- read_csv("data/davidprice.csv")
  chrispappastweets <- read_csv("data/chrispappas.csv")
  donaldnorcrosstweets <- read_csv("data/donaldnorcross.csv")
  andykimtweets <- read_csv("data/andykim.csv")
  joshgtweets <- read_csv("data/joshg.csv")
  frankpallonetweets <- read_csv("data/frankpallone.csv")
  malinowskitweets <- read_csv("data/malinowski.csv")
  sirestweets <- read_csv("data/sires.csv")
  billpascrelltweets <- read_csv("data/billpascrell.csv")
  donaldpaynetweets <- read_csv("data/donaldpayne.csv")
  sherrilltweets <- read_csv("data/sherrill.csv")
  benraylujantweets <- read_csv("data/benraylujan.csv")
  horsfordtweets <- read_csv("data/horsford.csv")
  tomsuozzitweets <- read_csv("data/tomsuozzi.csv")
  gregorymeekstweets <- read_csv("data/gregorymeeks.csv")
  gracemengtweets <- read_csv("data/gracemeng.csv")
  jeffriestweets <- read_csv("data/jeffries.csv")
  jerrynadlertweets <- read_csv("data/jerrynadler.csv")
  maxrosetweets <- read_csv("data/maxrose.csv")
  espaillattweets <- read_csv("data/espaillat.csv")
  joseserranotweets <- read_csv("data/joseserrano.csv")
  eliotengeltweets <- read_csv("data/eliotengel.csv")
  seanmaloneytweets <- read_csv("data/seanmaloney.csv")
  delgadotweets <- read_csv("data/delgado.csv")
  paultonkotweets <- read_csv("data/paultonko.csv")
  brindisitweets <- read_csv("data/brindisi.csv")
  joemorelletweets <- read_csv("data/joemorelle.csv")
  brianhigginstweets <- read_csv("data/brianhiggins.csv")
  timryantweets <- read_csv("data/timryan.csv")
  gregwaldentweets <- read_csv("data/gregwalden.csv")
  blumenauertweets <- read_csv("data/blumenauer.csv")
  peterdefaziotweets <- read_csv("data/peterdefazio.csv")
  schradertweets <- read_csv("data/schrader.csv")
  boyletweets <- read_csv("data/boyle.csv")
  dwightevanstweets <- read_csv("data/dwightevans.csv")
  cartwrighttweets <- read_csv("data/cartwright.csv")
  conorlambtweets <- read_csv("data/conorlamb.csv")
  mikedoyletweets <- read_csv("data/mikedoyle.csv")
  davidcicillinetweets <- read_csv("data/davidcicilline.csv")
  jimlangevintweets <- read_csv("data/jimlangevin.csv")
  cunninghamtweets <- read_csv("data/cunningham.csv")
  clyburntweets <- read_csv("data/clyburn.csv")
  jimcoopertweets <- read_csv("data/jimcooper.csv")
  cohentweets <- read_csv("data/cohen.csv")
  algreentweets <- read_csv("data/algreen.csv")
  gonzaleztweets <- read_csv("data/gonzalez.csv")
  joaquincastrotweets <- read_csv("data/joaquincastro.csv")
  cuellartweets <- read_csv("data/cuellar.csv")
  ebjtweets <- read_csv("data/ebj.csv" )
  colinallredtweets <- read_csv("data/colinallred.csv")
  veaseytweets <- read_csv("data/veasey.csv")
  filemonvelatweets <- read_csv("data/filemonvela.csv")
  lloyddoggetttweets <- read_csv("data/lloyddoggett.csv")
  benmcadamstweets <- read_csv("data/benmcadams.csv")
  bobbyscotttweets <- read_csv("data/bobbyscott.csv")
  mceachintweets <- read_csv("data/mceachin.csv")
  donbeyertweets <- read_csv("data/donbeyer.csv")
  gerryconnollytweets <- read_csv("data/gerryconnolly.csv")
  peterwelchtweets <- read_csv("data/peterwelch.csv")
  ricklarsentweets <- read_csv("data/ricklarsen.csv")
  derekkilmertweets <- read_csv("data/derekkilmer.csv")
  adamsmithtweets <- read_csv("data/adamsmith.csv")
  dennyhecktweets <- read_csv("data/dennyheck.csv")
  markpocantweets <- read_csv("data/markpocan.csv")
  ronkindtweets <- read_csv("data/ronkind.csv")
  carolmillertweets <- read_csv("data/carolmiller.csv")


  hormdtweets <- bind_rows(ohallerantweets %>%
                             mutate(person = "OHalleran"),
                           raulgrijalvatweets %>%
                             mutate(person = "Raul Grijalva"),
                           rubengallegotweets %>%
                             mutate(person = "Ruben Gallego"),
                           gregstantontweets %>%
                             mutate(person = "Greg Stanton"),
                           garamendtweets %>%
                             mutate(person = "Garamend"),
                           thompsontweets %>%
                             mutate(person = "Thompson"),
                           mcnerneytweets %>%
                             mutate(person = "McNerney"),
                           joshhardertweets %>%
                             mutate(person = "Josh Hardert"),
                           desaulniertweets %>%
                             mutate(person = "DeSaulnier"),
                           swalwelltweets %>%
                             mutate(person = "Swalwell"),
                           jimcostatweets %>%
                             mutate(person = "Jim Costa"),
                           rokhannatweets %>%
                             mutate(person = "Rokhanna"),
                           jimmypanettatweets %>%
                             mutate(person = "Jimmy Panetta"),
                           tjcoxtweets %>%
                             mutate(person = "Tj Cox"),
                           carbajaltweets %>%
                             mutate(person = "Carbajal"),
                           adamschifftweets %>%
                             mutate(person = "Adam Schiff"),
                           cardenastweets %>%
                             mutate(person = "Cardenas"),
                           bradshermantweets %>%
                             mutate(person = "Brad Sherman"),
                           peteaguilartweets %>%
                             mutate(person = "Pete Aguilar"),
                           tedlieutweets %>%
                             mutate(person = "Ted Lieu"),
                           jimmygomeztweets %>%
                             mutate(person = "Jimmy Gomez"),
                           ruiztweets %>%
                             mutate(person = "Ruiz"),
                           gilcisnerostweets %>%
                             mutate(person = "Gil Cisneros"),
                           marktakanotweets %>%
                             mutate(person = "Mark Takano"),
                           loucorreatweets %>%
                             mutate(person = "Lou Correa"),
                           lowenthaltweets %>%
                             mutate(person = "Lownthal"),
                           harleytweets %>%
                             mutate(person = "Harley"),
                           mikelevintweets %>%
                             mutate(person = "Mike Levin"),
                           huntertweets %>%
                             mutate(person = "Hunter"),
                           juanvargastweets %>%
                             mutate(person = "Juan Vargas"),
                           scottpeterstweets %>%
                             mutate(person = "Scott Peters"),
                           joenegusetweets %>%
                             mutate(person = "Joe Neguse"),
                           jasoncrowtweets %>%
                             mutate(person = "Jason Crow"),
                           perlmuttertweets %>%
                             mutate(person = "Perlmutter"),
                           johnlarsontweets %>%
                             mutate(person = "John Larson"),
                           joecourtneytweets %>%
                             mutate(person = "Joe Courtney"),
                           jahimestweets %>%
                             mutate(person = "Jahimes"),
                           allawsontweets %>%
                             mutate(person = "Al Lawson"),
                           darrensototweets %>%
                             mutate(person = "Darren Soto"),
                           valdemingstweets %>%
                             mutate(person = "Val Demings"),
                           charliecristtweets %>%
                             mutate(person = "Charlie Crist"),
                           teddeutchtweets %>%
                             mutate(person = "Ted Deutch"),
                           sanfordbishoptweets %>%
                             mutate(person = "Sanford Bishop"),
                           hankjohnsontweets %>%
                             mutate(person = "Hank Johnson"),
                           johnlewistweets %>%
                             mutate(person = "John Lewis"),
                           lucymcbathtweets %>%
                             mutate(person = "Lucy McBath"),
                           davidscotttweets %>%
                             mutate(person = "David Scott"),
                           edcasetweets %>%
                             mutate(person = "Ed Case"),
                           daveloebsacktweets %>%
                             mutate(person = "Dave Loebsack"),
                           bobbyrushtweets %>%
                             mutate(person = "Bobby Rush"),
                           lipinskitweets %>%
                             mutate(person = "Lipinski"),
                           chuygarciatweets %>%
                             mutate(person = "Chuy Garcia"),
                           mikequigleytweets %>%
                             mutate(person = "Mike Quigley"),
                           castentweets %>%
                             mutate(person = "Casten"),
                           dannydavistweets %>%
                             mutate(person = "Danny Davis"),
                           rajatweets %>%
                             mutate(person = "Raja"),
                           schneidertweets %>%
                             mutate(person = "Schneider"),
                           billfostertweets %>%
                             mutate(person = "Bill Foster"),
                           viscloskytweets %>%
                             mutate(person = "Visclosky"),
                           andrecarsontweets %>%
                             mutate(person = "Andre Carson"),
                           johnyarmuthtweets %>%
                             mutate(person = "Johny Armuth"),
                           richmondtweets %>%
                             mutate(person = "Richmond"),
                           chelliepingreetweets %>%
                             mutate(person = "Chellipingree"),
                           goldentweets %>%
                             mutate(person = "Golden"),
                           dutchtweets %>%
                             mutate(person = "Dutch"),
                           sarbanestweets %>%
                             mutate(person = "Sarbanes"),
                           anthonybrowntweets %>%
                             mutate(person = "Anthony Brown"),
                           hoyertweets %>%
                             mutate(person = "Hoyer"),
                           davidtronetweets %>%
                             mutate(person = "David Trone"),
                           cummingstweets %>%
                             mutate(person = "Cummings"),
                           raskintweets %>%
                             mutate(person = "Raskin"),
                           richardnealtweets %>%
                             mutate(person = "Richard Neal"),
                           mcgoverntweets %>%
                             mutate(person = "McGovern"),
                           joekennedytweets %>%
                             mutate(person = "Joe Kennedy"),
                           moultontweets %>%
                             mutate(person = "Moulton"),
                           stephenlynchtweets %>%
                             mutate(person = "Stephen Lynch"),
                           keatingtweets %>%
                             mutate(person = "Keating"),
                           dankildeetweets %>%
                             mutate(person = "Dan Kildee"),
                           andylevintweets %>%
                             mutate(person = "Andy Levin"),
                           cleavertweets %>%
                             mutate(person = "Cleaver"),
                           benniethompsontweets %>%
                             mutate(person = "Bennie Thompson"),
                           gkbutterfieldtweets %>%
                             mutate(person = "GK Butterfield"),
                           davidpricetweets %>%
                             mutate(person = "David Price"),
                           chrispappastweets %>%
                             mutate(person = "Chris Pappas"),
                           donaldnorcrosstweets %>%
                             mutate(person = "Donald Norcross"),
                           andykimtweets %>%
                             mutate(person = "Andy Kim"),
                           joshgtweets %>%
                             mutate(person = "Josh G"),
                           frankpallonetweets %>%
                             mutate(person = "Frank Pallone"),
                           malinowskitweets %>%
                             mutate(person = "Malinowski"),
                           sirestweets %>%
                             mutate(person = "Sires"),
                           billpascrelltweets %>%
                             mutate(person = "Bill Pascrell"),
                           donaldpaynetweets %>%
                             mutate(person = "Donald Payne"),
                           sherrilltweets %>%
                             mutate(person = "Sherrill"),
                           benraylujantweets %>%
                             mutate(person = "Ben Raylujan"),
                           horsfordtweets %>%
                             mutate(person = "Horsford"),
                           tomsuozzitweets %>%
                             mutate(person = "Tom Suouzzi"),
                           gregorymeekstweets %>%
                             mutate(person = "Gregory Meeks"),
                           gracemengtweets %>%
                             mutate(person = "Grace Meng"),
                           jeffriestweets %>%
                             mutate(person = "Jeffries"),
                           jerrynadlertweets %>%
                             mutate(person = "Jerry Nadler"),
                           maxrosetweets %>%
                             mutate(person = "Max Rose"),
                           espaillattweets %>%
                             mutate(person = "Espailla"),
                           joseserranotweets %>%
                             mutate(person = "Jose Serrano"),
                           eliotengeltweets %>%
                             mutate(person = "Eli Otengel"),
                           seanmaloneytweets %>%
                             mutate(person = "Sean Maloney"),
                           delgadotweets %>%
                             mutate(person = "Delgado"),
                           paultonkotweets %>%
                             mutate(person = "Paul Tonko"),
                           brindisitweets %>%
                             mutate(person = "Brindisi"),
                           joemorelletweets %>%
                             mutate(person = "Joe Morelle"),
                           brianhigginstweets %>%
                             mutate(person = "Brian Higgins"),
                           timryantweets %>%
                             mutate(person = "Tim Ryan"),
                           gregwaldentweets %>%
                             mutate(person = "Greg Walden"),
                           blumenauertweets %>%
                             mutate(person = "Blumenauer"),
                           peterdefaziotweets %>%
                             mutate(person = "Peter DeFazio"),
                           schradertweets %>%
                             mutate(person = "Schrader"),
                           boyletweets %>%
                             mutate(person = "Boyle"),
                           dwightevanstweets %>%
                             mutate(person = "Dwight Evans"),
                           cartwrighttweets %>%
                             mutate(person = "Cartwright"),
                           conorlambtweets %>%
                             mutate(person = "Conor Lamb"),
                           mikedoyletweets %>%
                             mutate(person = "Mike Doyle"),
                           davidcicillinetweets %>%
                             mutate(person = "David Cicilline"),
                           jimlangevintweets %>%
                             mutate(person = "Jim Langevin"),
                           cunninghamtweets %>%
                             mutate(person = "Cunningham"),
                           clyburntweets %>%
                             mutate(person = "Clyburn"),
                           jimcoopertweets %>%
                             mutate(person = "Jim Cooper"),
                           cohentweets %>%
                             mutate(person = "Cohen"),
                           algreentweets %>%
                             mutate(person = "Al Green"),
                           gonzaleztweets %>%
                             mutate(person = "Gonzalez"),
                           joaquincastrotweets %>%
                             mutate(person = "Joaquin Castro"),
                           cuellartweets %>%
                             mutate(person = "Cuellar"),
                           ebjtweets %>%
                             mutate(person = "EBJ"),
                           colinallredtweets %>%
                             mutate(person = "Colin Allred"),
                           veaseytweets %>%
                             mutate(person = "Veasey"),
                           filemonvelatweets %>%
                             mutate(person = "File Monvela"),
                           lloyddoggetttweets %>%
                             mutate(person = "Lloyd Doggett"),
                           benmcadamstweets %>%
                             mutate(person = "Ben McAdams"),
                           bobbyscotttweets %>%
                             mutate(person = "Bobby Scott"),
                           mceachintweets %>%
                             mutate(person = "McEachin"),
                           donbeyertweets %>%
                             mutate(person = "Don Beyer"),
                           gerryconnollytweets %>%
                             mutate(person = "Gerry Connolly"),
                           peterwelchtweets %>%
                             mutate(person = "Peter Welch"),
                           ricklarsentweets %>%
                             mutate(person = "Rick Larson"),
                           derekkilmertweets %>%
                             mutate(person = "Derek Kilmer"),
                           adamsmithtweets %>%
                             mutate(person = "Adam Smith"),
                           dennyhecktweets %>%
                             mutate(person = "Denny Heck"),
                           markpocantweets %>%
                             mutate(person = "Mark Pocan"),
                           ronkindtweets %>%
                             mutate(person = "Ron Kind"),
                           carolmillertweets %>%
                             mutate(person = "Carol Miller"))

  save_as_csv(hormdtweets, "data/hormdtweets.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
  hormdtweets <- read_csv("data/hormdtweets.csv")
}

