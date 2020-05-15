##########################
# ScrapeCongress          #
##########################

=======
library(rtweet)
library(tidytext)
library(dplyr)
library(tidytext)
library(readr)
library(tm)
library(utils)

# version 0.1.0
# 5/12/2020
# Authors: Damon C. Roberts


## Functions:
## Dependencies:
#                   twitteR (uses userTimeline() function to access tweets)      
#                   rtweets (uses save_as_csv() function to make quick use of json files)
#                   tidytext (useful for managing strings in a tibble)
#                   dplyr (assists with tibble management/manipulation)
#                   readr (assists with reading CSVs)
#                   tm (used in text mining)

## Functions Included:
# (1) senmaleD()
# (2) senfemD()
# (3) senfemR()
# (4) senmaleR()
# (5) horfemR()
# (6) horfemD()
# (7) hormaleR()
# (8) hormaleD()


library(rtweet)
library(tidytext)
library(dplyr)
library(readr)
library(tm)
library(utils)
library(remotes)


#############################
#--------(1) senmaleD-------#
#############################
#' Grabs the 50 most recent tweets from Male Democrats in the Senate
#' Saves those tweets in individual csv (for each MC) and uses the UTF-8 file encoding for the CSV
#' Creates one dataframe to combine all of the tweets from each MC
#' With the one large dataframe, it creates a CSV to hold the 50 most recent tweets for each Male Democrat in the Senate
#' @import twitteR
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @import remotes
#' @export

senmaleD <- function() {
  dougjones <- twitteR::userTimeline('@SenDougJones', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dougjonesDF <- twitteR::twListToDF(dougjones)
  write.csv(dougjonesDF, "data/dougjones.csv")
  dougjonestweets <- readr::read_csv("data/dougjones.csv")
  bennet <- twitteR::userTimeline('@SenatorBennet', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bennetDF <- twitteR::twListToDF(bennet)
  write.csv(bennetDF,"data/bennet.csv")
  bennettweets <- readr::read_csv("data/bennet.csv")
  blumenthal <- twitteR::userTimeline('@SenBlumenthal', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  blumenthalDF <- twitteR::twListToDF(blumenthal)
  write.csv(blumenthalDF,"data/blumenthal.csv")
  blumenthaltweets <- readr::read_csv("data/blumenthal.csv")
  murphey <- twitteR::userTimeline('@SenMurphyOffice', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  murpheyDF <- twitteR::twListToDF(murphey)
  write.csv(murpheyDF, "data/murphy.csv")
  murpheytweets <- readr::read_csv("data/murphy.csv")
  chriscoons <- twitteR::userTimeline('@ChrisCoons', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  chriscoonsDF <- twitteR::twListToDF(chriscoons)
  write.csv(chriscoonsDF, "data/chriscoons.csv")
  chriscoonstweets <- readr::read_csv("data/chriscoons.csv")
  cooper <- twitteR::userTimeline('@SenatorCarper', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cooperDF <- twitteR::twListToDF(cooper)
  write.csv(cooperDF, "data/cooper.csv")
  coopertweets <- readr::read_csv("data/cooper.csv")
  brianschatz <- twitteR::userTimeline('@SenBrianSchatz', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  brianschatzDF <- twitteR::twListToDF(brianschatz)
  write.csv(brianschatzDF, "data/brianschatz.csv")
  brianschatztweets <- readr::read_csv("data/brianschatz.csv")
  durbin <- twitteR::userTimeline('@SenatorDurbin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  durbinDF <- twitteR::twListToDF(durbin)
  write.csv(durbinDF, "data/durbin.csv")
  durbintweets <- readr::read_csv("data/durbin.csv")
  markey <- twitteR::userTimeline('@SenMarkey', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  markeyDF <- twitteR::twListToDF(markey)
  write.csv(markeyDF, "data/markey.csv")
  markeytweets <- readr::read_csv("data/markey.csv")
  cardin <- twitteR::userTimeline('@SenatorCardin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cardinDF <- twitteR::twListToDF(cardin)
  write.csv(cardinDF, "data/cardin.csv")
  cardintweets <- readr::read_csv("data/cardin.csv")
  chrisvanhollen <- twitteR::userTimeline('@ChrisVanHollen', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  chrisvanhollenDF <- twitteR::twListToDF(chrisvanhollen)
  write.csv(chrisvanhollenDF, "data/chrisvanhollen.csv")
  chrisvanhollentweets <- readr::read_csv("data/chrisvanhollen.csv")
  garypeters <- twitteR::userTimeline('@SenGaryPeters', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  garypetersDF <- twitteR::twListToDF(garypeters)
  write.csv(garypetersDF, "data/garypeters.csv")
  garypeterstweets <- readr::read_csv("data/garypeters.csv")
  booker <- twitteR::userTimeline('@SenBooker', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bookerDF <- twitteR::twListToDF(booker)
  write.csv(bookerDF, "data/booker.csv")
  bookertweets <- readr::read_csv("data/booker.csv")
  menendez <- twitteR::userTimeline('@SenatorMenendez', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  menendezDF <- twitteR::twListToDF(menendez)
  write.csv(menendezDF, "data/menendez.csv")
  menendeztweets <- readr::read_csv("data/menendez.csv")
  martinheinrich <- twitteR::userTimeline('@MartinHeinrich', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  martinheinrichDF <- twitteR::twListToDF(martinheinrich)
  write.csv(martinheinrichDF, "data/martinheinrich.csv")
  martinheinrichtweets <- readr::read_csv("data/martinheinrich.csv")
  tomudall <- twitteR::userTimeline('@SenatorTomUdall', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tomudallDF <- twitteR::twListToDF(tomudall)
  write.csv(tomudallDF, "data/tomudall.csv")
  tomudalltweets <- readr::read_csv("data/tomudall.csv")
  schumer <- twitteR::userTimeline('@SenSchumer', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  schumerDF <- twitteR::twListToDF(schumer)
  write.csv(schumerDF, "data/schumer.csv")
  schumertweets <- readr::read_csv("data/schumer.csv")
  sherrodbrown <- twitteR::userTimeline('@SenSherrodBrown', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  sherrodbrownDF <- twitteR::twListToDF(sherrodbrown)
  write.csv(sherrodbrownDF, "data/sherrodbrown.csv")
  sherrodbrowntweets <- readr::read_csv("data/sherrodbrown.csv")
  jeffmerkley <- twitteR::userTimeline('@SenJeffMerkley', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jeffmerkleyDF <- twitteR::twListToDF(jeffmerkley)
  write.csv(jeffmerkleyDF, "data/jeffmerkley.csv")
  jeffmerkleytweets <- readr::read_csv("data/jeffmerkley.csv")
  ronwyden <- twitteR::userTimeline('@RonWyden', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  ronwydenDF <- twitteR::twListToDF(ronwyden)
  write.csv(ronwydenDF, "data/ronwyden.csv")
  ronwydentweets <- readr::read_csv("data/ronwyden.csv")
  bobcasey <- twitteR::userTimeline('@SenBobCasey', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bobcaseyDF <- twitteR::twListToDF(bobcasey)
  write.csv(bobcaseyDF, "data/bobcasey.csv")
  bobcaseytweets <- readr::read_csv("data/bobcasey.csv")
  toomey <- twitteR::userTimeline('@SenToomey', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  toomeyDF <- twitteR::twListToDF(toomey)
  write.csv(toomeyDF, "data/toomey.csv")
  toomeytweets <- readr::read_csv("data/toomey.csv")
  jackreed <- twitteR::userTimeline('@SenJackReed', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jackreedDF <- twitteR::twListToDF(jackreed)
  write.csv(jackreedDF, "data/jackreed.csv")
  jackreedtweets <- readr::read_csv("data/jackreed.csv")
  whitehouse <- twitteR::userTimeline('@SenWhitehouse', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  whitehouseDF <- twitteR::twListToDF(whitehouse)
  write.csv(whitehouseDF, "data/whitehouse.csv")
  whitehousetweets <- readr::read_csv("data/whitehouse.csv")
  leahy <- twitteR::userTimeline('@SenatorLeahy', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  leahyDF <- twitteR::twListToDF(leahy)
  write.csv(leahyDF, "data/leahy.csv")
  leahytweets <- readr::read_csv("data/leahy.csv")
  sanders <- twitteR::userTimeline('@SenSanders', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  sandersDF <- twitteR::twListToDF(sanders)
  write.csv(sandersDF, "data/sanders.csv")
  sanderstweets <- readr::read_csv("data/sanders.csv")
  warner <- twitteR::userTimeline('@MarkWarner', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  warnerDF <- twitteR::twListToDF(warner)
  write.csv(warnerDF, "data/warner.csv")
  warnertweets <- readr::read_csv("data/warner.csv")
  joemanchin <- twitteR::userTimeline('@Sen_JoeManchin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  joemanchinDF <- twitteR::twListToDF(joemanchin)
  write.csv(joemanchinDF, "data/joemanchin.csv")
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
                                     martokentweets %>%
                                       dplyr::mutate(person = "Martoken"),
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
  
  senatemdDF <- twitteR::twListToDF(senatemdtweets)
  write.csv(senatemdDF, "data/senatemdtweets.csv")
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
#' @import twitteR
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @import remotes
#' @export

senfemD <- function() {
  sinema <- twitteR::userTimeline('@SenatorSinema', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  sinemaDF <- twitteR::twListToDF(sinema)
  write.csv(sinemaDF,"data/sinema.csv")
  sinematweets <- readr::read_csv("data/sinema.csv")
  feinstein <- twitteR::userTimeline('@SenFeinstein', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  feinsteinDF <- twitteR::twListToDF(feinstein)
  write.csv(feinsteinDF,"data/feinstein.csv")
  feinsteintweets <- readr::read_csv("data/feinstein.csv")
  kamalaharris <- twitteR::userTimeline('@SenKamalaHarris', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kamalaharrisDF <- twitteR::twListToDF(kamalaharris)
  write.csv(kamalaharrisDF,"data/kamalaharris.csv")
  kamalaharristweets <- readr::read_csv("data/kamalaharris.csv")
  maziehirono <- twitteR::userTimeline('@maziehirono', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  maziehironoDF <- twitteR::twListToDF(maziehirono)
  write.csv(maziehironoDF,"data/maziehirono.csv")
  maziehironotweets <- readr::read_csv("data/maziehirono.csv")
  duckworth <- twitteR::userTimeline('@SenDuckworth', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  duckworthDF <- twitteR::twListToDF(duckworth)
  write.csv(duckworthDF,"data/duckworth.csv")
  duckworthtweets <- readr::read_csv("data/duckworth.csv")
  warren <- twitteR::userTimeline('@SenWarren', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  warrenDF <- twitteR::twListToDF(warren)
  write.csv(warrenDF,"data/warren.csv")
  warrentweets <- readr::read_csv("data/warren.csv")
  stabenow <- twitteR::userTimeline('@SenStabenow', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  stabenowDF <- twitteR::twListToDF(stabenow)
  write.csv(stabenowDF,"data/stabenow.csv")
  stabenowtweets <- readr::read_csv("data/stabenow.csv")
  amyklobuchar <- twitteR::userTimeline('@SenAmyKlobuchar', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  amyklobucharDF <- twitteR::twListToDF(amyklobuchar)
  write.csv(amyklobucharDF,"data/amyklobuchar.csv")
  amyklobuchartweets <- readr::read_csv("data/amyklobuchar.csv")
  tinasmith <- twitteR::userTimeline('@SenTinaSmith', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tinasmithDF <- twitteR::twListToDF(tinasmith)
  write.csv(tinasmithDF,"data/tinasmith.csv")
  tinasmithtweets <- readr::read_csv("data/tinasmith.csv")
  hassan <- twitteR::userTimeline('@SenatorHassan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  hassanDF <- twitteR::twListToDF(hassan)
  write.csv(hassanDF,"data/hassan.csv")
  hassantweets <- readr::read_csv("data/hassan.csv")
  shaheen <- twitteR::userTimeline('@SenatorShaheen', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  shaheenDF <- twitteR::twListToDF(shaheen)
  write.csv(shaheenDF,"data/shaheen.csv")
  shaheentweets <- readr::read_csv("data/shaheen.csv")
  cortezmasto <- twitteR::userTimeline('@SenCortezMasto', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cortezmastoDF <- twitteR::twListToDF(cortezmasto)
  write.csv(cortezmastoDF,"data/cortezmasto.csv")
  cortezmastotweets <- readr::read_csv("data/cortezmasto.csv")
  jackyrosen <- twitteR::userTimeline(' @SenJackyRosen', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jackyrosenDF <- twitteR::twListToDF(jackyrosen)
  write.csv(jackyrosenDF,"data/jackyrosen.csv")
  jackyrosentweets <- readr::read_csv("data/jackyrosen.csv")
  gillibrand <- twitteR::userTimeline('@gillibrandny', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gillibrandDF <- twitteR::twListToDF(gillibrand)
  write.csv(gillibrandDF,"data/gillibrand.csv")
  gillibrandtweets <- readr::read_csv("data/gillibrand.csv")
  cantwell <- twitteR::userTimeline('@SenatorCantwell', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cantwellDF <- twitteR::twListToDF(cantwell)
  write.csv(cantwellDF,"data/cantewell.csv")
  cantwelltweets <- readr::read_csv("data/cantewell.csv")
  pattymurray <- twitteR::userTimeline('@PattyMurray', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  pattymurrayDF <- twitteR::twListToDF(pattymurray)
  write.csv(pattymurrayDF,"data/pattymurray.csv")
  pattymurraytweets <- readr::read_csv("data/pattymurray.csv")
  baldwin <- twitteR::userTimeline('@SenatorBaldwin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  baldwinDF <- twitteR::twListToDF(baldwin)
  write.csv(baldwinDF,"data/baldwin.csv")
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
  
  senatefdDF <- twitteR::twListToDF(senatefdtweets)
  write.csv(senatefdDF, "data/senatefdtweets.csv")
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
#' @import twitteR
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @import remotes
#' @export

senfemR <- function() {
  lisamurkowski<- twitteR::userTimeline('@lisamurkowski', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lisamurkowskiDF <- twitteR::twListToDF(lisamurkowski)
  write.csv(lisamurkowskiDF,"data/lisamurkowski.csv")
  lisamurkowskitweets <- readr::read_csv("data/lisamurkowski.csv")
  mcsally <- twitteR::userTimeline('@SenMcSallyAZ', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mcsallyDF <- twitteR::twListToDF(mcsally)
  write.csv(mcsallyDF,"data/mcsally.csv")
  mcsallytweets <- readr::read_csv("data/mcsally.csv")
  joniernst <- twitteR::userTimeline('@SenJoniErnst', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  joniernstDF <- twitteR::twListToDF(joniernst)
  write.csv(joniernstDF,"data/joniernst.csv")
  joniernsttweets <- readr::read_csv("data/joniernst.csv")
  collins <- twitteR::userTimeline('@SenatorCollins', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  collinsDF <- twitteR::twListToDF(collins)
  write.csv(collinsDF,"data/collins.csv")
  collinstweets <- readr::read_csv("data/collins.csv")
  hydesmith <- twitteR::userTimeline('@SenHydeSmith', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  hydesmithDF <- twitteR::twListToDF(hydesmith)
  write.csv(hydesmithDF,"data/hydesmith.csv")
  hydesmithtweets <- readr::read_csv("data/hydesmith.csv")
  fischer <- twitteR::userTimeline('@SenatorFischer', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  fischerDF <- twitteR::twListToDF(fischer)
  write.csv(fischerDF,"data/fischer.csv")
  fischertweets <- readr::read_csv("data/fischer.csv")
  marshablackburn <- twitteR::userTimeline('@MarshaBlackburn', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  marshablackburnDF <- twitteR::twListToDF(marshablackburn)
  write.csv(marshablackburnDF,"data/marshablackburn.csv")
  marshablackburntweets <- readr::read_csv("data/marshablackburn.csv")
  capito <- twitteR::userTimeline('@SenCapito', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  capitoDF <- twitteR::twListToDF(capito)
  write.csv(capitoDF,"data/capito.csv")
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
  
  senatefrDF <- twitteR::twListToDF(senatefrtweets)
  write.csv(senatefrDF, "data/senatefrtweets.csv")
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
#' @import twitteR
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @import remotes
#' @export

senmaleR <- function() {
  donsullivan <- twitteR::userTimeline('@SenDanSullivan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  shelby <- twitteR::userTimeline('@SenShelby', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johnboozman <- twitteR::userTimeline('@JohnBoozman', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tomcotton <- twitteR::userTimeline('@SenTomCotton', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  corygardner <- twitteR::userTimeline('@SenCoryGardner', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rubio <- twitteR::userTimeline('@SenRubioPress', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rickscott<- twitteR::userTimeline('@SenRickScott', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  isakson <- twitteR::userTimeline('@SenatorIsakson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  davidperdue <- twitteR::userTimeline('@sendavidperdue', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  chuckgrassley <- twitteR::userTimeline('@ChuckGrassley', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mikecrapo <- twitteR::userTimeline('@MikeCrapo', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  risch <- twitteR::userTimeline('@SenatorRisch', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  braun<- twitteR::userTimeline('@SenatorBraun', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  toddyoung<- twitteR::userTimeline('@SenToddYoung', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jerrymoran<- twitteR::userTimeline('@JerryMoran', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  patroberts<- twitteR::userTimeline('@SenPatRoberts', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mitchmconnell<- twitteR::userTimeline('@SenateMajLdr', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  randpaul<- twitteR::userTimeline('@RandPaul', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  billcassidy<- twitteR::userTimeline('@SenBillCassidy', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johnkennedy<- twitteR::userTimeline('@SenJohnKennedy', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  royblunt<- twitteR::userTimeline('@RoyBlunt', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  hawley<- twitteR::userTimeline('@SenHawleyPress', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  wicker<- twitteR::userTimeline('@SenatorWicker', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  stevedaines<- twitteR::userTimeline('@SteveDaines', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  burr<- twitteR::userTimeline('@SenatorBurr', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  thomtillis<- twitteR::userTimeline('@SenThomTillis', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kevincramer<- twitteR::userTimeline('@@SenKevinCramer', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johnhoeven<- twitteR::userTimeline('@SenJohnHoeven', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  sasse<- twitteR::userTimeline('@SenSasse', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  robportman<- twitteR::userTimeline('@senrobportman', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jiminhofe<- twitteR::userTimeline('@JimInhofe', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lankford<- twitteR::userTimeline('@SenatorLankford', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lindseygraham<- twitteR::userTimeline('@LindseyGrahamSC', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  timscott<- twitteR::userTimeline('@SenatorTimScott', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rounds<- twitteR::userTimeline('@SenatorRounds', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johnthune<- twitteR::userTimeline('@SenJohnThune', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  alexander<- twitteR::userTimeline('@SenAlexander', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johncornyn<- twitteR::userTimeline('@JohnCornyn', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tedcruz<- twitteR::userTimeline('@SenTedCruz', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mikelee<- twitteR::userTimeline('@SenMikeLee', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  romney<- twitteR::userTimeline('@SenatorRomney', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  ronjohnson<- twitteR::userTimeline('@SenRonJohnson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johnbarrasso<- twitteR::userTimeline('@SenJohnBarrasso', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  enzi<- twitteR::userTimeline('@SenatorEnzi', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  
  
  donsullivanDF <- twitteR::twListToDF(donsullivan)
  write.csv(donsullivanDF,"data/donsullivan.csv")
  shelbyDF <- twitteR::twListToDF(shelby)
  write.csv(shelbyDF,"data/shelby.csv")
  johnboozmanDF <- twitteR::twListToDF(johnboozman)
  write.csv(johnboozmanDF,"data/johnboozman.csv")
  tomcottonDF <- twitteR::twListToDF(tomcotton)
  write.csv(tomcottonDF,"data/tomcotton.csv")
  corygardnerDF <- twitteR::twListToDF(corygardner)
  write.csv(corygardnerDF,"data/corygardner.csv")
  rubioDF <- twitteR::twListToDF(rubio)
  write.csv(rubioDF,"data/rubio.csv")
  rickscottDF <- twitteR::twListToDF(rickscott)
  write.csv(rickscottDF,"data/rickscott.csv")
  isaksonDF <- twitteR::twListToDF(isakson)
  write.csv(isaksonDF,"data/isakson.csv")
  davidperdueDF <- twitteR::twListToDF(davidperdue)
  write.csv(davidperdueDF,"data/davidperdue.csv")
  chuckgrassleyDF <- twitteR::twListToDF(chuckgrassley)
  write.csv(chuckgrassleyDF,"data/chuckgrassley.csv")
  mikecrapoDF <- twitteR::twListToDF(mikecrapo)
  write.csv(mikecrapoDF,"data/mikecrapo.csv")
  rischDF <- twitteR::twListToDF(risch)
  write.csv(rischDF,"data/risch.csv")
  braunDF <- twitteR::twListToDF(braun)
  write.csv(braunDF,"data/braun.csv")
  toddyoungDF <- twitteR::twListToDF(toddyoung)
  write.csv(toddyoungDF,"data/toddyoung.csv")
  jerrymoranDF <- twitteR::twListToDF(jerrymoran)
  write.csv(jerrymoranDF,"data/jerrymoran.csv")
  patrobertsDF <- twitteR::twListToDF(patroberts)
  write.csv(patrobertsDF,"data/patroberts.csv")
  mitchmconnellDF <- twitteR::twListToDF(mitchmconnell)
  write.csv(mitchmconnellDF,"data/mitchmconnell.csv")
  randpaulDF <- twitteR::twListToDF(randpaul)
  write.csv(randpaulDF,"data/randpaul.csv")
  billcassidyDF <- twitteR::twListToDF(billcassidy)
  write.csv(billcassidyDF,"data/billcassidy.csv")
  johnkennedyDF <- twitteR::twListToDF(johnkennedy)
  write.csv(johnkennedyDF,"data/johnkennedy.csv")
  roybluntDF <- twitteR::twListToDF(royblunt)
  write.csv(roybluntDF,"data/royblunt.csv")
  hawleyDF <- twitteR::twListToDF(hawley)
  write.csv(hawleyDF,"data/hawley.csv")
  wickerDF <- twitteR::twListToDF(wicker)
  write.csv(wickerDF,"data/wicker.csv")
  stevedainesDF <- twitteR::twListToDF(stevedaines)
  write.csv(stevedainesDF,"data/stevedaines.csv")
  burrDF <- twitteR::twListToDF(burr)
  write.csv(burrDF,"data/burr.csv")
  thomtillisDF <- twitteR::twListToDF(thomtillis)
  write.csv(thomtillisDF,"data/thomtillis.csv")
  kevincramerDF <- twitteR::twListToDF(kevincrame)
  write.csv(kevincramerDF,"data/kevincramer.csv")
  johnhoevenDF <- twitteR::twListToDF(johnhoeven)
  write.csv(johnhoevenDF,"data/johnhoeven.csv")
  sasseDF <- twitteR::twListToDF(sasse)
  write.csv(sasseDF,"data/sasse.csv")
  robportmanDF <- twitteR::twListToDF(robportman)
  write.csv(robportmanDF,"data/robportman.csv")
  jiminhofeDF <- twitteR::twListToDF(jiminhofe)
  write.csv(jiminhofeDF,"data/jiminhofe.csv")
  lankfordDF <- twitteR::twListToDF(lankford)
  write.csv(lankfordDF,"data/lankford.csv")
  lindseygrahamDF <- twitteR::twListToDF(lindseygraham)
  write.csv(lindseygrahamDF,"data/lindseygraham.csv")
  timscottDF <- twitteR::twListToDF(timscott)
  write.csv(timscottDF,"data/timscott.csv")
  roundsDF <- twitteR::twListToDF(rounds)
  write.csv(roundsDF,"data/rounds.csv")
  johnthuneDF <- twitteR::twListToDF(johnthune)
  write.csv(johnthuneDF,"data/johnthune.csv")
  alexanderDF <- twitteR::twListToDF(alexander)
  write.csv(alexanderDF,"data/alexander.csv")
  johncornynDF <- twitteR::twListToDF(johncornyn)
  write.csv(johncornynDF,"data/johncornyn.csv")
  tedcruzDF <- twitteR::twListToDF(tedcruz)
  write.csv(tedcruzDF,"data/tedcruz.csv")
  mikeleeDF <- twitteR::twListToDF(mikelee)
  write.csv(mikeleeDF,"data/mikelee.csv")
  romneyDF <- twitteR::twListToDF(romney)
  write.csv(romneyDF,"data/romney.csv")
  ronjohnsonDF <- twitteR::twListToDF(ronjohnson)
  write.csv(ronjohnsonDF,"data/ronjohnson.csv")
  johnbarrassoDF <- twitteR::twListToDF(johnbarrasso)
  write.csv(johnbarrassoDF,"data/johnbarrasso.csv")
  enziDF <- twitteR::twListToDF(enzi)
  write.csv(enziDF,"data/enzi.csv")
  
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
  
  senatemrDF <- twitteR::twListToDF(senatemrtweets)
  write.csv(senatemrDF, "data/senatemrtweets.csv")
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
#' @import twitteR
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @import remotes
#' @export

horfemR <- function() {
  martharoby <- twitteR::userTimeline('@RepMarthaRoby', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  walorski <- twitteR::userTimeline('@RepWalorski', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  susanbrooks <- twitteR::userTimeline('@SusanWBrooks', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  annwagner <- twitteR::userTimeline('@RepAnnWagner', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  hartzler <- twitteR::userTimeline('@RepHartzler', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  virginiafoxx <- twitteR::userTimeline('@virginiafoxx', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  armstrong <- twitteR::userTimeline('@RepArmstrongND', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  stefanik <- twitteR::userTimeline('@RepStefanik', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cathymcmorris <- twitteR::userTimeline('@cathymcmorris', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lizcheney<- twitteR::userTimeline('@RepLizCheney', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  
  
  martharobyDF <- twitteR::twListToDF(martharoby)
  write.csv(martharobyDF,"data/martharoby.csv")
  walorskiDF <- twitteR::twListToDF(walorski)
  write.csv(walorskiDF,"data/walorski.csv")
  susanbrooksDF <- twitteR::twListToDF(susanbrooks)
  write.csv(susanbrooksDF,"data/susanbrooks.csv")
  annwagnerDF <- twitteR::twListToDF(annwagner)
  write.csv(annwagnerDF,"data/annwagner.csv")
  hartzlerDF <- twitteR::twListToDF(hartzler)
  write.csv(hartzlerDF,"data/hartzler.csv")
  virginiafoxxDF <- twitteR::twListToDF(virginiafoxx)
  write.csv(virginiafoxxDF,"data/virginiafoxx.csv")
  armstrongDF <- twitteR::twListToDF(armstrong)
  write.csv(armstrongDF,"data/armstrong.csv")
  stefanikDF <- twitteR::twListToDF(stefanik)
  write.csv(stefanikDF,"data/stefanik.csv")
  cathymcmorrisDF <- twitteR::twListToDF(cathymcmorris)
  write.csv(cathymcmorrisDF,"data/cathymcmorris.csv")
  lizcheneyDF <- twitteR::twListToDF(lizcheney)
  write.csv(lizcheneyDF,"data/lizcheney.csv")
  
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
  horfrDF <- twitteR::twListToDF(horfrtweets)
  write.csv(horfrDF, "data/horfrtweets.csv")
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
#' @import twitteR
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @import remotes
#' @export

horfemD <- function() {
  terrisewell <- twitteR::userTimeline('@RepTerriSewell', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kirkpatrick<- twitteR::userTimeline('@RepKirkpatrick', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dorismatsui<- twitteR::userTimeline('@DorisMatsui', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bera<- twitteR::userTimeline('@RepBera', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  pelosi <- twitteR::userTimeline('@SpeakerPelosi', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  barbaralee <- twitteR::userTimeline('@RepBarbaraLee', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  speier <- twitteR::userTimeline('@RepSpeier', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  annaeshoo <- twitteR::userTimeline('@RepAnnaEshoo', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  zoelofgren <- twitteR::userTimeline('@RepZoeLofgren', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  katiehill <- twitteR::userTimeline('@RepKatieHill', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  brownley <- twitteR::userTimeline('@RepBrownley', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  judychu <- twitteR::userTimeline('@RepJudyChu', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gracenapolitano <- twitteR::userTimeline('@gracenapolitano', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  normatorres <- twitteR::userTimeline('@NormaJTorres', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  karenbass <- twitteR::userTimeline('@RepKarenBass', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lindasanchez <- twitteR::userTimeline('@RepLindaSanchez', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  roybalallard<- twitteR::userTimeline('@RepRoybalAllard', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  maxinewaters<- twitteR::userTimeline('@RepMaxineWaters', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  barragan<- twitteR::userTimeline('@RepBarragan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  katieporter <- twitteR::userTimeline('@RepKatiePorter', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  susandavis <- twitteR::userTimeline('@RepSusanDavis', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dianadegette <- twitteR::userTimeline('@RepDianaDeGette', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rosadelauro<- twitteR::userTimeline('@rosadelauro', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jahanahayes<- twitteR::userTimeline('@RepJahanaHayes', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lbr<- twitteR::userTimeline('@RepLBR', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  eleanornorton<- twitteR::userTimeline('@EleanorNorton', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  stephmurphy<- twitteR::userTimeline('@RepStephMurphy', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kcastor<- twitteR::userTimeline('@USRepKCastor', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  hastings <- twitteR::userTimeline('@RepHastingsFL', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  loisfrankel <- twitteR::userTimeline('@RepLoisFrankel', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dwstweets <- twitteR::userTimeline('@RepDWStweets', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  wilson <- twitteR::userTimeline('@RepWilson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dmp <- twitteR::userTimeline('@RepDMP', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  shalala <- twitteR::userTimeline('@RepShalala', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tulsi <- twitteR::userTimeline('@TulsiPress', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  finkenauer <- twitteR::userTimeline('@RepFinkenauer', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cindyaxne <- twitteR::userTimeline('@RepCindyAxne', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  robinkelly <- twitteR::userTimeline('@RepRobinKelly', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  janschakowsky <- twitteR::userTimeline('@janschakowsky', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  underwood <- twitteR::userTimeline('@RepUnderwood', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cheri <- twitteR::userTimeline('@RepCheri', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  davids <- twitteR::userTimeline('@RepDavids', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  loritrahan <- twitteR::userTimeline('@RepLoriTrahan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kclark <- twitteR::userTimeline('@RepKClark', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  pressley <- twitteR::userTimeline('@RepPressley', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  haleystevens <- twitteR::userTimeline('@RepHaleyStevens', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  debdingell <- twitteR::userTimeline('@RepDebDingell', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rashida <- twitteR::userTimeline('@RepRashida', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lawrence <- twitteR::userTimeline('@RepLawrence', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  angiecraig <- twitteR::userTimeline('@RepAngieCraig', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  deanphillips <- twitteR::userTimeline('@RepDeanPhillips', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bettymccollum <- twitteR::userTimeline('@BettyMcCollum04', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  ilhan<- twitteR::userTimeline('@Ilhan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lacyclay <- twitteR::userTimeline('@LacyClayMO1', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  adams <- twitteR::userTimeline('@RepAdams', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  anniekuster <- twitteR::userTimeline('@RepAnnieKuster', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bonnie<- twitteR::userTimeline('@RepBonnie', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  debhaaland <- twitteR::userTimeline('@RepDebHaaland', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  torressmall <- twitteR::userTimeline('@RepTorresSmall', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dinatitus<- twitteR::userTimeline('@repdinatitus', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  susielee <- twitteR::userTimeline('@RepSusieLee', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kathleenrice <- twitteR::userTimeline('@RepKathleenRice', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  nydiavelazquez<- twitteR::userTimeline('@NydiaVelazquez', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  yvetteclarke<- twitteR::userTimeline('@RepYvetteClarke', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  maloney<- twitteR::userTimeline('@RepMaloney', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  aoc <- twitteR::userTimeline('@RepAOC', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  nitalowey <- twitteR::userTimeline('@NitaLowey', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  beatty <- twitteR::userTimeline('@RepBeatty', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  marcykaptur <- twitteR::userTimeline('@RepMarcyKaptur', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  marciafudge<- twitteR::userTimeline('@RepMarciaFudge', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kendrahorn<- twitteR::userTimeline('@RepKendraHorn', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bonamici <- twitteR::userTimeline('@RepBonamici', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mgs <- twitteR::userTimeline('@RepMGS', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dean <- twitteR::userTimeline('@RepDean', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  houlahan <- twitteR::userTimeline('@RepHoulahan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  susanwild <- twitteR::userTimeline('@RepSusanWild', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  fletcher <- twitteR::userTimeline('@RepFletcher', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  escobar <- twitteR::userTimeline('@RepEscobar', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jacksonlee <- twitteR::userTimeline('@JacksonLeeTX18', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  sylviagarcia <- twitteR::userTimeline('@RepSylviaGarcia', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  elaineluria<- twitteR::userTimeline('@RepElaineLuria', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  spanberger<- twitteR::userTimeline('@RepSpanberger', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  wexton <- twitteR::userTimeline('@RepWexton', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  delbene <- twitteR::userTimeline('@RepDelBene', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  herrerabeutler <- twitteR::userTimeline('@HerreraBeutler', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jayapal<- twitteR::userTimeline('@RepJayapal', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kimschrier <- twitteR::userTimeline('@RepKimSchrier', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gwenmoore <- twitteR::userTimeline('@RepGwenMoore', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  
  terrisewellDF <- twitteR::twListToDF(terrisewell)
  write.csv(terrisewellDF,"data/terrisewell.csv")
  kirkpatrickDF <- twitteR::twListToDF(kirkpatrick)
  write.csv(kirkpatrickDF,"data/kirkpatrick.csv")
  dorismatsuiDF <- twitteR::twListToDF(dorismatsui)
  write.csv(dorismatsuiDF,"data/dorismatsui.csv")
  beraDF <- twitteR::twListToDF(bera)
  write.csv(beraDF,"data/bera.csv")
  pelosiDF <- twitteR::twListToDF(pelosi)
  write.csv(pelosiDF,"data/pelosi.csv")
  barbaraleeDF <- twitteR::twListToDF(barbaralee)
  write.csv(barbaraleedf,"data/barbaralee.csv")
  speierDF <- twitteR::twListToDF(speier)
  write.csv(speierDF,"data/speier.csv")
  annaeshooDF <- twitteR::twListToDF(annaeshoo)
  write.csv(annaeshooDF,"data/annaeshoo.csv")
  zoelofgrenDF <- twitteR::twListToDF(zoelofgren)
  write.csv(zoelofgrenDF,"data/zoelofgren.csv")
  katiehillDF <- twitteR::twListToDF(katiehill)
  write.csv(katiehillDF,"data/katiehill.csv")
  brownleyDF <- twitteR::twListToDF(brownley)
  write.csv(brownleyDF,"data/brownley.csv")
  judychuDF <- twitteR::twListToDF(judychu)
  write.csv(judychuDF,"data/judychu.csv")
  gracenapolitanoDF <- twitteR::twListToDF(gracenapolitano)
  write.csv(gracenapolitanoDF,"data/gracenapolitano.csv")
  normatorresDF <- twitteR::twListToDF(normatorres)
  write.csv(normatorresDF,"data/normatorres.csv")
  karenbassDF <- twitteR::twListToDF(karenbass)
  write.csv(karenbassDF,"data/karenbass.csv")
  lindasanchezDF <- twitteR::twListToDF(lindasanchez)
  write.csv(lindasanchezDF,"data/lindasanchez.csv")
  roybalallardDF <- twitteR::twListToDF(roybalallard)
  write.csv(roybalallardDF,"data/roybalallard.csv")
  maxinewatersDF <- twitteR::twListToDF(maxinewaters)
  write.csv(maxinewatersDF,"data/maxinewaters.csv")
  barraganDF <- twitteR::twListToDF(barragan)
  write.csv(barraganDF,"data/barragan.csv")
  katieporterDF <- twitteR::twListToDF(katieporter)
  write.csv(katieporterDF,"data/katieporter.csv")
  susandavisDF <- twitteR::twListToDF(susandavis)
  write.csv(susandavisDF,"data/susandavis.csv")
  dianadegetteDF <- twitteR::twListToDF(dianadegette)
  write.csv(dianadegetteDF,"data/dianadegette.csv")
  rosadelauroDF <- twitteR::twListToDF(rosadelauro)
  write.csv(rosadelauroDF,"data/rosadelauro.csv")
  jahanahayesDF <- twitteR::twListToDF(jahanahayes)
  write.csv(jahanahayesDF,"data/jahanahayes.csv")
  lbrDF <- twitteR::twListToDF(lbr)
  write.csv(lbrDF,"data/lbr.csv")
  eleanornortonDF <- twitteR::twListToDF(eleanornorton)
  write.csv(eleanornortonDF,"data/eleanornorton.csv")
  stephmurphyDF <- twitteR::twListToDF(stephmurphy)
  write.csv(stephmurphyDF,"data/stephmurphy.csv")
  kcastorDF <- twitteR::twListToDF(kcastor)
  write.csv(kcastorDF,"data/kcastor.csv")
  hastingsDF <- twitteR::twListToDF(hastings)
  write.csv(hastingsDF,"data/hastings.csv")
  loisfrankelDF <- twitteR::twListToDF(loisfrankel)
  write.csv(loisfrankelDF,"data/loisfrankel.csv")
  dwstweetsDF <- twitteR::twListToDF(dwstweets)
  write.csv(dwstweetsDF,"data/dwstweets.csv")
  wilsonDF <- twitteR::twListToDF(wilson)
  write.csv(wilsonDF,"data/wilson.csv")
  dmpDF <- twitteR::twListToDF(dmp)
  write.csv(dmpDF,"data/dmp.csv")
  shalalaDF <- twitteR::twListToDF(shalala)
  write.csv(shalalaDF,"data/shalala.csv")
  tulsiDF <- twitteR::twListToDF(tulsi)
  write.csv(tulsiDF,"data/tulsi.csv")
  finkenauerDF <- twitteR::twListToDF(finkenauer)
  write.csv(finkenauerDF,"data/finkenauer.csv")
  cindyaxneDF <- twitteR::twListToDF(cindyaxne)
  write.csv(cindyaxneDF,"data/cindyaxne.csv")
  robinkellyDF <- twitteR::twListToDF(robinkelly)
  write.csv(robinkellyDF,"data/robinkelly.csv")
  janschakowskyDF <- twitteR::twListToDF(janschakowsky)
  write.csv(janschakowskyDF,"data/janschakowsky.csv")
  underwoodDF <- twitteR::twListToDF(underwood)
  write.csv(underwoodDF,"data/underwood.csv")
  cheriDF <- twitteR::twListToDF(cheri)
  write.csv(cheriDF,"data/cheri.csv")
  davidsDF <- twitteR::twListToDF(davids)
  write.csv(davidsDF,"data/davids.csv")
  loritrahanDF <- twitteR::twListToDF(loritrahan)
  write.csv(loritrahanDF, "data/loritrahan.csv")
  kclarkDF <- twitteR::twListToDF(kclark)
  write.csv(kclarkDF,"data/kclark.csv")
  pressleyDF <- twitteR::twListToDF(pressley)
  write.csv(pressleyDF,"data/pressley.csv")
  haleystevensDF <- twitteR::twListToDF(haleystevens)
  write.csv(haleystevensDF,"data/haleystevens.csv")
  debdingellDF <- twitteR::twListToDF(debdingell)
  write.csv(debdingellDF,"data/debdingell.csv")
  rashidaDF <- twitteR::twListToDF(rashida)
  write.csv(rashidaDF,"data/rashida.csv")
  lawrenceDF <- twitteR::twListToDF(lawrence)
  write.csv(lawrenceDF,"data/lawrence.csv")
  anniecraigDF <- twitteR::twListToDF(angiecraig)
  write.csv(anniecraigDF,"data/angiecraig.csv")
  deanphillipsDF <- twitteR::twListToDF(deanphillips)
  write.csv(deanphillipsDF,"data/deanphillips.csv")
  bettymccollumDF <- twitteR::twListToDF(bettymccollum)
  write.csv(bettymccollumDF,"data/bettymccollum.csv")
  ilhanDF <- twitteR::twListToDF(ilhan)
  write.csv(ilhanDF,"data/ilhan.csv")
  lacyclayDF <- twitteR::twListToDF(lacyclay)
  write.csv(lacyclayDF,"data/lacyclay.csv")
  adamsDF <- twitteR::twListToDF(adams)
  write.csv(adamsDF,"data/adams.csv")
  anniekusterDF <- twitteR::twListToDF(anniekuster)
  write.csv(anniekusterDF,"data/anniekuster.csv")
  bonnieDF <- twitteR::twListToDF(bonnie)
  write.csv(bonnieDF,"data/bonnie.csv")
  debhaalandDF <- twitteR::twListToDF(debhaaland)
  write.csv(debhaalandDF,"data/debhaaland.csv")
  torressmallDF <- twitteR::twListToDF(torressmall)
  write.csv(torressmallDF,"data/torressmall.csv")
  dinatitusDF <- twitteR::twListToDF(dinatitus)
  write.csv(dinatitusDF,"data/dinatitus.csv")
  susieleeDF <- twitteR::twListToDF(susielee)
  write.csv(susieleeDF,"data/susielee.csv")
  kathleenriceDF <- twitteR::twListToDF(kathleenrice)
  write.csv(kathleenriceDF,"data/kathleenrice.csv")
  nydiavelazquezDF <- twitteR::twListToDF(nydiavelazquez)
  write.csv(nydiavelazquezDF,"data/nydiavelazquez.csv")
  yvetteclarkeDF <- twitteR::twListToDF(yvetteclarke)
  write.csv(yvetteclarkeDF,"data/yvetteclarke.csv")
  maloneyDF <- twitteR::twListToDF(maloney)
  write.csv(maloneyDF,"data/maloney.csv")
  aocDF <- twitteR::twListToDF(aoc)
  write.csv(aocDF,"data/aoc.csv")
  nitaloweyDF <- twitteR::twListToDF(nitalowey)
  write.csv(nitaloweyDF,"data/nitalowey.csv")
  beattyDF <- twitteR::twListToDF(beatty)
  write.csv(beattyDF,"data/beatty.csv")
  marcykapturDF <- twitteR::twListToDF(marcykaptur)
  write.csv(marcykapturDF,"data/marcykaptur.csv")
  marciafudgeDF <- twitteR::twListToDF(marciafudge)
  write.csv(marciafudgeDF,"data/marciafudge.csv")
  kendrahornDF <- twitteR::twListToDF(kendrahorn)
  write.csv(kendrahornDF,"data/kendrahorn.csv")
  bonamiciDF <- twitteR::twListToDF(bonamici)
  write.csv(bonamiciDF,"data/bonamici.csv")
  mgsDF <- twitteR::twListToDF(mgs)
  write.csv(mgsDF,"data/mgs.csv")
  deanDF <- twitteR::twListToDF(dean)
  write.csv(deanDF,"data/dean.csv")
  houlahanDF <- twitteR::twListToDF(houlahan)
  write.csv(houlahanDF,"data/houlahan.csv")
  susanwildDF <- twitteR::twListToDF(susanwild)
  write.csv(susanwildDF,"data/susanwild.csv")
  fletcherDF <- twitteR::twListToDF(fletcher)
  write.csv(fletcherDF,"data/fletcher.csv")
  escobarDF <- twitteR::twListToDF(escobar)
  write.csv(escobarDF,"data/escobar.csv")
  jacksonleeDF <- twitteR::twListToDF(jacksonlee)
  write.csv(jacksonleeDF,"data/jacksonlee.csv")
  sylviagarciaDF <- twitteR::twListToDF(sylviagarcia)
  write.csv(sylviagarciaDF,"data/sylviagarcia.csv")
  elaineluriaDF <- twitteR::twListToDF(elaineluria)
  write.csv(elaineluriaDF,"data/elaineluria.csv")
  spanbergerDF <- twitteR::twListToDF(spanberger)
  write.csv(spanbergerDF,"data/spanberger.csv")
  wextonDF <- twitteR::twListToDF(wexton)
  write.csv(wextonDF,"data/wexton.csv")
  delbeneDF <- twitteR::twListToDF(delbene)
  write.csv(delbeneDF,"data/delbene.csv")
  herrerabeutlerDF <- twitteR::twListToDF(herrerabeutler)
  write.csv(herrerabeutlerDF,"data/herrerabeutler.csv")
  jayapalDF <- twitteR::twListToDF(jayapal)
  write.csv(jayapalDF,"data/jayapal.csv")
  kimschrierDF <- twitteR::twListToDF(kimschrier)
  write.csv(kimschrierDF,"data/kimschrier.csv")
  gwenmooreDF <- twitteR::twListToDF(gwenmoore)
  write.csv(gwenmooreDF,"data/gwenmoore.csv")
  
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
  
  horfdDF <- twitteR::twListToDF(horfdtweets)
  write.csv(horfdDf, "data/horfdtweets.csv")
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
#' @import twitteR
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @import remotes
#' @export

hormaleR <- function() {
  donyoung <- twitteR::userTimeline('@repdonyoung', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  byrne <- twitteR::userTimeline('@RepByrne', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mikerogers <- twitteR::userTimeline('@RepMikeRogersAL', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  robertaderholt <- twitteR::userTimeline('@Robert_Aderholt', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mobrooks <- twitteR::userTimeline('@RepMoBrooks', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  garypalmer <- twitteR::userTimeline('@USRepGaryPalmer', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rickcrawford <- twitteR::userTimeline('@RepRickCrawford', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  frenchhill<- twitteR::userTimeline('@RepFrenchHill', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  stevewomack <- twitteR::userTimeline('@rep_stevewomack', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  westerman <- twitteR::userTimeline('@RepWesterman', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gosar <- twitteR::userTimeline('@RepGosar', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  andybiggs <- twitteR::userTimeline('@RepAndyBiggsAZ', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  david <- twitteR::userTimeline('@RepDavid', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dlesko <- twitteR::userTimeline('@RepDLesko', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lamalfa<- twitteR::userTimeline('@RepLaMalfa', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mcclintock <- twitteR::userTimeline('@RepMcClintock', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  paulcook <- twitteR::userTimeline('@RepPaulCook', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  devinnunes <- twitteR::userTimeline('@RepDevinNunes', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kevinmccarthy <- twitteR::userTimeline('@GOPLeader', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kencalvert<- twitteR::userTimeline('@KenCalvert', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tipton <- twitteR::userTimeline('@RepTipton', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kenbuck <- twitteR::userTimeline('@RepKenBuck', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dlamborn <- twitteR::userTimeline('@RepDLamborn', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mattgaetz <- twitteR::userTimeline('@RepMattGaetz', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  nealdunn <- twitteR::userTimeline('@DrNealDunnFL2', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tedyoho <- twitteR::userTimeline('@RepTedYoho', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rutherford <- twitteR::userTimeline('@RepRutherfordFL', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  michaelwaltz <- twitteR::userTimeline('@RepMichaelWaltz', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  billposey <- twitteR::userTimeline('@congbillposey', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  webster <- twitteR::userTimeline('@RepWebster', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gusbilirakis <- twitteR::userTimeline('@RepGusBilirakis', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  vernbuchanan<- twitteR::userTimeline('@VernBuchanan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gregsteube <- twitteR::userTimeline('@RepGregSteube', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  brianmast <- twitteR::userTimeline('@RepBrianMast', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rooney <- twitteR::userTimeline('@RepRooney', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mariodb <- twitteR::userTimeline('@MarioDB', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  buddycarter <- twitteR::userTimeline('@RepBuddyCarter', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  drewferguson <- twitteR::userTimeline('@RepDrewFerguson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  robwoodall<- twitteR::userTimeline('@RepRobWoodall', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  austinscott <- twitteR::userTimeline('@AustinScottGA08', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dougcollins <- twitteR::userTimeline('@RepDougCollins', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  hice <- twitteR::userTimeline('@CongressmanHice', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  loudermilk <- twitteR::userTimeline('@RepLoudermilk', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rickallen <- twitteR::userTimeline('@RepRickAllen', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tomgraves <- twitteR::userTimeline('@RepTomGraves', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  russfulcher <- twitteR::userTimeline('@RepRussFulcher', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mikesimpson <- twitteR::userTimeline('@CongMikeSimpson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bost <- twitteR::userTimeline('@RepBost', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rodneydavis <- twitteR::userTimeline('@RodneyDavis', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  shimkus <- twitteR::userTimeline('@RepShimkus', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kinzinger <- twitteR::userTimeline('@RepKinzinger', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lahood <- twitteR::userTimeline('@RepLaHood', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jimbanks <- twitteR::userTimeline('@RepJimBanks', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jimbaird <- twitteR::userTimeline('@RepJimBaird', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gregpence <- twitteR::userTimeline('@RepGregPence', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  larrybucshon <- twitteR::userTimeline('@RepLarryBucshon', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  trey<- twitteR::userTimeline('@RepTrey', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  ronestes <- twitteR::userTimeline('@RepRonEstes', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  guthrie <- twitteR::userTimeline('@RepGuthrie', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  thomasmassie <- twitteR::userTimeline('@RepThomasMassie', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  halrogers<- twitteR::userTimeline('@RepHalRogers', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  andybarr <- twitteR::userTimeline('@RepAndyBarr', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  stevescalise <- twitteR::userTimeline('@SteveScalise', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  clayhiggins <- twitteR::userTimeline('@RepClayHiggins', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mikejohnson <- twitteR::userTimeline('@RepMikeJohnson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  abraham <- twitteR::userTimeline('@RepAbraham', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  garretgraves <- twitteR::userTimeline('@RepGarretGraves', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  andyharris<- twitteR::userTimeline('@RepAndyHarrisMD', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jackbergman <- twitteR::userTimeline('@RepJackBergman', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  huizenga <- twitteR::userTimeline('@RepHuizenga', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  justinamash <- twitteR::userTimeline('@justinamash', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  moolenaar<- twitteR::userTimeline('@RepMoolenaar', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  fredupton <- twitteR::userTimeline('@RepFredUpton', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  walberg <- twitteR::userTimeline('@RepWalberg', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  slotkin <- twitteR::userTimeline('@RepSlotkin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  paulmitchell <- twitteR::userTimeline('@RepPaulMitchell', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  hagedorn<- twitteR::userTimeline('@RepHagedorn', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tomemmer <- twitteR::userTimeline('@RepTomEmmer', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  petestauber <- twitteR::userTimeline('@RepPeteStauber', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  blaine <- twitteR::userTimeline('@RepBlaine', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  samgraves<- twitteR::userTimeline('@RepSamGraves', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  long <- twitteR::userTimeline('@USRepLong', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jasonsmith <- twitteR::userTimeline('@RepJasonSmith', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  trentkelly <- twitteR::userTimeline('@RepTrentKelly', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  michaelguest <- twitteR::userTimeline('@RepMichaelGuest', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  palazzo<- twitteR::userTimeline('@CongPalazzo', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  holding <- twitteR::userTimeline('@RepHolding', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  walterjones <- twitteR::userTimeline('@RepWalterJones', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  markwalker <- twitteR::userTimeline('@RepMarkWalker', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  davidrouzer <- twitteR::userTimeline('@RepDavidRouzer', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  richhudson <- twitteR::userTimeline('@RepRichHudson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  patrickmchenry <- twitteR::userTimeline('@PatrickMcHenry', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  markmeadows <- twitteR::userTimeline('@RepMarkMeadows', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tedbudd <- twitteR::userTimeline('@RepTedBudd', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jefffortenberry <- twitteR::userTimeline('@JeffFortenberry', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  donbacon<- twitteR::userTimeline('@RepDonBacon', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  adriansmith <- twitteR::userTimeline('@RepAdrianSmith', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jvd <- twitteR::userTimeline('@CongressmanJVD', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  chrissmith <- twitteR::userTimeline('@RepChrisSmith', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  markamodei <- twitteR::userTimeline('@MarkAmodeiNV2', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  leezeldin <- twitteR::userTimeline('@RepLeeZeldin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  peterking <- twitteR::userTimeline('@RepPeteKing', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tomreed <- twitteR::userTimeline('@RepTomReed', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johnkatko <- twitteR::userTimeline('@RepJohnKatko', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  chriscollins <- twitteR::userTimeline('@RepChrisCollins', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  stevechabot <- twitteR::userTimeline('@RepSteveChabot', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bradwenstrup<- twitteR::userTimeline('@RepBradWenstrup', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jimjordan<- twitteR::userTimeline('@Jim_Jordan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  boblatta <- twitteR::userTimeline('@boblatta', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  billjohnson <- twitteR::userTimeline('@RepBillJohnson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bobgibbs <- twitteR::userTimeline('@RepBobGibbs', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  warrendavidson <- twitteR::userTimeline('@WarrenDavidson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  miketurner <- twitteR::userTimeline('@RepMikeTurner', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  balderson <- twitteR::userTimeline('@RepBalderson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  davejoyce <- twitteR::userTimeline('@RepDaveJoyce', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  stevestivers <- twitteR::userTimeline('@RepSteveStivers', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  agonzalez<- twitteR::userTimeline('@RepAGonzalez', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kevinhern <- twitteR::userTimeline('@repkevinhern', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mullin <- twitteR::userTimeline('@RepMullin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  franklucas <- twitteR::userTimeline('@RepFrankLucas', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tomcole <- twitteR::userTimeline('@TomColeOK04', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  brianfitz <- twitteR::userTimeline('@RepBrianFitz', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  meuser <- twitteR::userTimeline('@RepMeuser', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  scottperry <- twitteR::userTimeline('@RepScottPerry', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  smucker <- twitteR::userTimeline('@RepSmucker', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johnjoyce <- twitteR::userTimeline('@RepJohnJoyce', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  greschenthaler <- twitteR::userTimeline('@GReschenthaler', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gt <- twitteR::userTimeline('@CongressmanGT', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mikekelly <- twitteR::userTimeline('@MikeKellyPA', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  joewilson <- twitteR::userTimeline('@RepJoeWilson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jeffduncan <- twitteR::userTimeline('@RepJeffDuncan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  timmons <- twitteR::userTimeline('@reptimmons', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  ralphnorman <- twitteR::userTimeline('@RepRalphNorman', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tomrice <- twitteR::userTimeline('@RepTomRice', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dustyjohnson <- twitteR::userTimeline('@RepDustyJohnson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  philroe <- twitteR::userTimeline('@DrPhilRoe', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  timburchett <- twitteR::userTimeline('@RepTimBurchett', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  chuck <- twitteR::userTimeline('@RepChuck', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  desjarlais <- twitteR::userTimeline('@DesJarlaisTN04l', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johnrose <- twitteR::userTimeline('@RepJohnRose', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  markgreen <- twitteR::userTimeline('@RepMarkGreen', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  davidkustoff <- twitteR::userTimeline('@RepDavidKustoff', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  louiegohmert <- twitteR::userTimeline('@replouiegohmert', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dancrenshaw<- twitteR::userTimeline('@RepDanCrenshaw', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  vantaylor <- twitteR::userTimeline('@RepVanTaylor', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  ratcliffe <- twitteR::userTimeline('@RepRatcliffe', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lancegooden <- twitteR::userTimeline('@RepLanceGooden', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  ronwright <- twitteR::userTimeline('@RepRonWright', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kevinbrady <- twitteR::userTimeline('@RepKevinBrady', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mccaul <- twitteR::userTimeline('@RepMcCaul', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  conaway <- twitteR::userTimeline('@ConawayTX11', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kaygranger <- twitteR::userTimeline('@RepKayGranger', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mac <- twitteR::userTimeline('@MacTXPress', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  randy <- twitteR::userTimeline('@TXRandy14', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  billflores <- twitteR::userTimeline('@RepBillFlores', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  arrington <- twitteR::userTimeline('@RepArrington', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  chiproy <- twitteR::userTimeline('@RepChipRoy', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  peteolson <- twitteR::userTimeline('@RepPeteOlson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  hurd <- twitteR::userTimeline('@HurdOnTheHill', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  kenmarchant <- twitteR::userTimeline('@RepKenMarchant', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rwilliams <- twitteR::userTimeline('@RepRWilliams', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  michaelburgess <- twitteR::userTimeline('@michaelcburgess', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cloud<- twitteR::userTimeline('@RepCloudTX', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  carter <- twitteR::userTimeline('@JudgeCarter', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  brianbabin <- twitteR::userTimeline('@RepBrianBabin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  robbishop <- twitteR::userTimeline('@RepRobBishop', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  chrisstewart <- twitteR::userTimeline('@RepChrisStewart', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johncurtis <- twitteR::userTimeline('@RepJohnCurtis', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  robwittman <- twitteR::userTimeline('@RobWittman', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  riggleman <- twitteR::userTimeline('@RepRiggleman', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bencline <- twitteR::userTimeline('@RepBenCline', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mgriffith <- twitteR::userTimeline('@RepMGriffith', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  newhouse <- twitteR::userTimeline('@RepNewhouse', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bryansteil <- twitteR::userTimeline('@RepBryanSteil', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jim <- twitteR::userTimeline('@JimPressOffice', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  grothman <- twitteR::userTimeline('@RepGrothman', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  seanduffy <- twitteR::userTimeline('@RepSeanDuffy', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gallagher <- twitteR::userTimeline('@RepGallagher', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mckinley <- twitteR::userTimeline('@RepMcKinley', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  alexmooney <- twitteR::userTimeline('@RepAlexMooney', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  donyoungDF <- twitteR::twListToDF(donyoung)
  write.csv(donyoungDF,"data/donyoung.csv")
  byrneDF <- twitteR::twListToDF(byrne)
  write.csv(byrneDF,"data/byrne.csv")
  mikerogersDF <- twitteR::twListToDF(mikerogers)
  write.csv(mikerogersDF,"data/mikerogers.csv")
  robertaderholtDF <- twitteR::twListToDF(robertaderholt)
  write.csv(robertaderholtDF,"data/robertaderholt.csv")
  mobrooksDF <- twitteR::twListToDF(mobrooks)
  write.csv(mobrooksDF,"data/mobrooks.csv")
  garypalmerDF <- twitteR::twListToDF(garypalmer)
  write.csv(garypalmerDF,"data/garypalmer.csv")
  rickcrawfordDF <- twitteR::twListToDF(rickcrawford)
  write.csv(rickcrawfordDF,"data/rickcrawford.csv")
  frenchillDF <- twitteR::twListToDF(frenchhill)
  write.csv(frenchillDF,"data/frenchhill.csv")
  stevewomackDF <- twitteR::twListToDF(stevewomack)
  write.csv(stevewomackDF,"data/stevewomack.csv")
  westermanDF <- twitteR::twListToDF(westerman)
  write.csv(westermanDF,"data/westerman.csv")
  gosarDF <- twitteR::twListToDF(gosar)
  write.csv(gosarDF,"data/gosar.csv")
  andybiggsDF <- twitteR::twListToDF(andybiggs)
  write.csv(andybiggsDF,"data/andybiggs.csv")
  davidDF <- twitteR::twListToDF(david)
  write.csv(davidDF,"data/david.csv")
  deleskoDF <- twitteR::twListToDF(dlesko)
  write.csv(deleskoDF,"data/dlesko.csv")
  lamalfaDF <- twitteR::twListToDF(lamalfa)
  write.csv(lamalfaDF,"data/lamalfa.csv")
  mcclintockDF <- twitteR::twListToDF(mcclintock)
  write.csv(mcclintockDF,"data/mcclintock.csv")
  paulcookDF <- twitteR::twListToDF(paulcook)
  write.csv(paulcookDF,"data/paulcook.csv")
  devinnunesDF <- twitteR::twListToDF(devinnunes)
  write.csv(devinnunesDF,"data/devinnunes.csv")
  kevinmccarthyDF <- twitteR::twListToDF(kevinmccarthy)
  write.csv(kevinmccarthyDF,"data/kevinmccarthy.csv")
  kencalvertDF <- twitteR::twListToDF(kencalvert)
  write.csv(kencalvertDF,"data/kencalvert.csv")
  tiptonDF <- twitteR::twListToDF(tipton)
  write.csv(tiptonDF,"data/tipton.csv")
  kenbuckDF <- twitteR::twListToDF(kenbuck)
  write.csv(kenbuckDF,"data/kenbuck.csv")
  dlambornDF <- twitteR::twListToDF(dlamborn)
  write.csv(dlambornDF,"data/dlamborn.csv")
  mattgaetzDF <- twitteR::twListToDF(mattgaetz)
  write.csv(mattgaetzDF,"data/mattgaetz.csv")
  nealdunnDF <- twitteR::twListToDF(nealdunn)
  write.csv(nealdunnDF,"data/nealdunn.csv")
  tedyohoDF <- twitteR::twListToDF(tedyoho)
  write.csv(tedyohoDF,"data/tedyoho.csv")
  rutherfordDF <- twitteR::twListToDF(rutherford)
  write.csv(rutherfordDF,"data/rutherford.csv")
  michaelwaltzDF <- twitteR::twListToDF(michaelwaltz)
  write.csv(michaelwaltzDF,"data/michaelwaltz.csv")
  billposeyDF <- twitteR::twListToDF(billposey)
  write.csv(billposeyDF,"data/billposey.csv")
  websterDF <- twitteR::twListToDF(webster)
  write.csv(websterDF,"data/webster.csv")
  gusbilirakisDF <- twitteR::twListToDF(gusbilirakis)
  write.csv(gusbilirakisDF,"data/gusbilirakis.csv")
  vernbuchananDF <- twitteR::twListToDF(vernbuchanan)
  write.csv(vernbuchananDF,"data/vernbuchanan.csv")
  gregsteubeDF <- twitteR::twListToDF(gregsteube)
  write.csv(gregsteubeDF,"data/gregsteube.csv")
  brianmastDF <- twitteR::twListToDF(brianmast)
  write.csv(brianmastDF,"data/brianmast.csv")
  rooneyDF <- twitteR::twListToDF(rooney)
  write.csv(rooneyDF,"data/rooney.csv")
  mariodbDF <- twitteR::twListToDF(mariodb)
  write.csv(mariodbDF,"data/mariodb.csv")
  buddycarterDF <- twitteR::twListToDF(buddycarter)
  write.csv(buddycarterDF,"data/buddycarter.csv")
  drewfergusonDF <- twitteR::twListToDF(drewferguson)
  write.csv(drewfergusonDF,"data/drewferguson.csv")
  robwoodallDF <- twitteR::twListToDF(robwoodall)
  write.csv(robwoodallDF,"data/robwoodall.csv")
  austinscottDF <- twitteR::twListToDF(austinscott)
  write.csv(austinscottDF,"data/austinscott.csv")
  dougcollinsDF <- twitteR::twListToDF(dougcollins)
  write.csv(dougcollinsDF,"data/dougcollins.csv")
  hiceDF <- twitteR::twListToDF(hice)
  write.csv(hiceDF,"data/hice.csv")
  loudermilkDF <- twitteR::twListToDF(loudermilk)
  write.csv(loudermilkDF,"data/loudermilk.csv")
  rickallenDF <- twitteR::twListToDF(rickallen)
  read.csv(rickallenDF,"data/rickallen.csv")
  tomgravesDF <- twitteR::twListToDF(tomgraves)
  write.csv(tomgravesDF,"data/tomgraves.csv")
  russfulcherDF <- twitteR::twListToDF(russfulcher)
  write.csv(russfulcherDF,"data/russfulcher.csv")
  mikesimpsonDF <- twitteR::twListToDF(mikesimpson)
  write.csv(mikesimpsonDF,"data/mikesimpson.csv")
  bostDF <- twitteR::twListToDF(bost)
  write.csv(bostDF,"data/bost.csv")
  rodneydavisDF <- twitteR::twListToDF(rodneydavis)
  write.csv(rodneydavisDF,"data/rodneydavis.csv")
  shimkusDF <- twitteR::twListToDF(shimkus)
  write.csv(shimkusDF,"data/shimkus.csv")
  kinzingerDF <- twitteR::twListToDF(kinzinger)
  write.csv(kinzingerDF, "data/kinzinger.csv")
  lahoodDF <- twitteR::twListToDF(lahood)
  write.csv(lahoodDF,"data/lahood.csv")
  jimbanksDF <- twitteR::twListToDF(jimbanks)
  write.csv(jimbanksDF,"data/jimbanks.csv")
  jimbairdDF <- twitteR::twListToDF(jimbaird)
  write.csv(jimbairdDF,"data/jimbaird.csv")
  gregpenceDF <- twitteR::twListToDF(gregpence)
  write.csv(gregpenceDF,"data/gregpence.csv")
  larrybucshononDF <- twitteR::twListToDF(larrybucshon)
  write.csv(larrybucshonDF,"data/larrybucshon.csv")
  treyDF <- twitteR::twListToDF(trey)
  write.csv(treyDF,"data/trey.csv")
  ronestesDF <- twitteR::twListToDF(ronestes)
  write.csv(ronestesDF,"data/ronestes.csv")
  guthrieDF <- twitteR::twListToDF(guthrie)
  write.csv(guthrieDF,"data/guthrie.csv")
  thomasmassieDF <- twitteR::twListToDF(thomasmassie)
  write.csv(thomasmassieDF,"data/thomasmassie.csv")
  halrogersDF <- twitteR::twListToDF(halrogers)
  write.csv(halrogersDF,"data/halrogers.csv")
  andybarrDF <- twitteR::twListToDF(andybarr)
  write.csv(andybarrDF,"data/andybarr.csv")
  stevescaliseDF <- twitteR::twListToDF(stevescalise)
  write.csv(stevescaliseDF,"data/stevescalise.csv")
  clayhigginsDF <- twitteR::twListToDF(clayhiggins)
  write.csv(clayhigginsDF,"data/clayhiggins.csv")
  mikejohnsonDF <- twitteR::twListToDF(mikejohnson)
  write.csv(mikejohnsonDF,"data/mikejohnson.csv")
  abrahamDF <- twitteR::twListToDF(abraham)
  write.csv(abrahamDF,"data/abraham.csv")
  garretgravesDF <- twitteR::twListToDF(garretgraves)
  write.csv(garretgravesDF,"data/garretgraves.csv")
  andyharrisDF <- twitteR::twListToDF(andyharris)
  write.csv(andyharrisDF,"data/andyharris.csv")
  jackbergmanDF <- twitteR::twListToDF(jackbergman)
  write.csv(jackbergmanDF,"data/jackbergman.csv")
  huizengaDF <- twitteR::twListToDF(huizenga)
  write.csv(huizengaDF,"data/huizenga.csv")
  justinamashDF <- twitteR::twListToDF(justinamash)
  write.csv(justinamashDF,"data/justinamash.csv")
  moolenaarDF <- twitteR::twListToDF(moolenaar)
  write.csv(moolenaarDF,"data/moolenaar.csv")
  freduptonDF <- twitteR::twListToDF(fredupton)
  write.csv(freduptonDF,"data/fredupton.csv")
  walbergDF <- twitteR::twListToDF(walberg)
  write.csv(walbergDF,"data/walberg.csv")
  slotkinDF <- twitteR::twListToDF(slotkin)
  write.csv(slotkinDF,"data/slotkin.csv")
  paulmitchellDF <- twitteR::twListToDF(paulmitchell)
  write.csv(paulmitchellDF,"data/paulmitchell.csv")
  hagedornDF <- twitteR::twListToDF(hagedorn)
  write.csv(hagedornDF,"data/hagedorn.csv")
  tomemmerDF <- twitteR::twListToDF(tomemmer)
  write.csv(tomemmerDF,"data/tomemmer.csv")
  petestauberDF <- twitteR::twListToDF(petestauber)
  write.csv(petestauberDF,"data/petestauber.csv")
  blaineDF <- twitteR::twListToDF(blaine)
  write.csv(blaineDF,"data/blaine.csv")
  samgravesDF <- twitteR::twListToDF(samgraves)
  write.csv(samgravesDF,"data/samgraves.csv")
  longDF <- twitteR::twListToDF(long)
  write.csv(longDF,"data/long.csv")
  jasonsmithDF <- twitteR::twListToDF(jasonsmith)
  write.csv(jasonsmithDF,"data/jasonsmith.csv")
  trentkellyDF <- twitteR::twListToDF(trentkelly)
  write.csv(trentkellyDF,"data/trentkelly.csv")
  michaelguestDF <- twitteR::twListToDF(michaelguest)
  write.csv(michaelguestDF,"data/michaelguest.csv")
  palazzoDF <- twitteR::twListToDF(palazzo)
  write.csv(palazzoDF,"data/palazzo.csv")
  holdingDF <- twitteR::twListToDF(holding)
  write.csv(holdingDF,"data/holding.csv")
  walterjonesDF <- twitteR::twListToDF(walterjones)
  write.csv(walterjonesDF,"data/walterjones.csv")
  markwalkerDF <- twitteR::twListToDF(markwalker)
  write.csv(markwalkerDF,"data/markwalker.csv")
  davidrouzerDF <- twitteR::twListToDF(davidrouzer)
  write.csv(davidrouzer,"data/davidrouzer.csv")
  richhudsonDF <- twitteR::twListToDF(richhudson)
  write.csv(richhudsonDF,"data/richhudson.csv")
  patrickmchenryDF <- twitteR::twListToDF(patrickmchenry)
  write.csv(patrickmchenryDF,"data/patrickmchenry.csv")
  markmeadowsDF <- twitteR::twListToDF(markmeadows)
  write.csv(markmeadowsDF,"data/markmeadows.csv")
  tedbuddDF <- twitteR::twListToDF(tedbudd)
  write.csv(tedbuddDF,"data/tedbudd.csv")
  jefffortenberryDF <- twitteR::twListToDF(jefffortenberry)
  write.csv(jefffortenberryDF,"data/jefffortenberry.csv")
  donbaconDF <- twitteR::twListToDF(donbacon)
  write.csv(donbaconDF,"data/donbacon.csv")
  adriansmithDF <- twitteR::twListToDF(adriansmith)
  write.csv(adriansmithDF,"data/adriansmith.csv")
  jvdDF <- twitteR::twListToDF(jvd)
  write.csv(jvdDF,"data/jvd.csv")
  chrissmithDF <- twitteR::twListToDF(chrissmith)
  write.csv(chrissmithDF,"data/chrissmith.csv")
  markmodeiDF <- twitteR::twListToDF(markamodei)
  write.csv(markmodeiDF,"data/markamodei.csv")
  leezeldinDF <- twitteR::twListToDF(leezeldin)
  write.csv(leezeldinDF,"data/leezeldin.csv")
  peterkingDF <- twitteR::twListToDF(peterking)
  write.csv(peterkingDF,"data/peterking.csv")
  tomreedDF <- twitteR::twListToDF(tomreed)
  write.csv(tomreedDF,"data/tomreed.csv")
  johnkatkoDF <- twitteR::twListToDF(johnkatko)
  write.csv(johnkatkoDF,"data/johnkatko.csv")
  chriscollinsDF <- twitteR::twListToDF(chriscollins)
  write.csv(chriscollinsDF,"data/chriscollins.csv")
  stevechabotDF <- twitteR::twListToDF(stevechabot)
  write.csv(stevechabotDF,"data/stevechabot.csv")
  bradwenstrupDF <- twitteR::twListToDF(bradwenstrup)
  write.csv(bradwenstrupDF,"data/bradwenstrup.csv")
  jimjordanDF <- twitteR::twListToDF(jimjordan)
  write.csv(jimjordanDF,"data/jimjordan.csv")
  boblattaDF <- twitteR::twListToDF(boblatta)
  write.csv(boblattaDF,"data/boblatta.csv")
  billjohnsonDF <- twitteR::twListToDF(billjohnson)
  write.csv(billjohnsonDF,"data/billjohnson.csv")
  bobgibbsDF <- twitteR::twListToDF(bobgibbs)
  write.csv(bobgibbsDF,"data/bobgibbs.csv")
  warrendavidsonDF <- twitteR::twListToDF(warrendavidson)
  write.csv(warrendavidsonDF,"data/warrendavidson.csv")
  miketurnerDF <- twitteR::twListToDF(miketurner)
  write.csv(miketurnerDF,"data/miketurner.csv")
  baldersonDF <- twitteR::twListToDF(balderson)
  write.csv(baldersonDF,"data/balderson.csv")
  davejoyceDF <- twitteR::twListToDF(davejoyce)
  write.csv(davejoyceDF,"data/davejoyce.csv")
  stevestiversDF <- twitteR::twListToDF(stevestivers)
  write.csv(stevestiversDF,"data/stevestivers.csv")
  agonzalezDF <- twitteR::twListToDF(agonzalez)
  write.csv(agonzalezDF,"data/agonzalez.csv")
  kevinhernDF <- twitteR::twListToDF(kevinhern)
  write.csv(kevinhernDF,"data/kevinhern.csv")
  mullinDF <- twitteR::twListToDF(mullin)
  write.csv(mullinDF,"data/mullin.csv")
  franklucasDF <- twitteR::twListToDF(franklucas)
  write.csv(franklucasDF,"data/franklucas.csv")
  tomcoleDF <- twitteR::twListToDF(tomcole)
  write.csv(tomcoleDF,"data/tomcole.csv")
  brianfitzDF <- twitteR::twListToDF(brianfitz)
  write.csv(brianfitzDF,"data/brianfitz.csv")
  meuserDF <- twitteR::twListToDF(meuser)
  write.csv(meuserDF,"data/meuser.csv")
  scottperryDF <- twitteR::twListToDF(scottperry)
  write.csv(scottperryDF,"data/scottperry.csv")
  smuckerDF <- twitteR::twListToDF(smucker)
  write.csv(smuckerDF,"data/smucker.csv")
  johnjoyceDF <- twitteR::twListToDF(johnjoyce)
  write.csv(johnjoyceDF,"data/johnjoyce.csv")
  greschentalerDF <- twitteR::twListToDF(greschenthaler)
  write.csv(greschenthalerDF,"data/greschenthaler.csv")
  gtDF <- twitteR::twListToDF(gt)
  write.csv(gtDF, "data/gt.csv")
  mikekellyDF <- twitteR::twListToDF(mikekelly)
  write.csv(mikekellyDF,"data/mikekelly.csv")
  joewilsonDF <- twitteR::twListToDF(joewilson)
  write.csv(joewilsonDF,"data/joewilson.csv")
  jeffduncanDF <- twitteR::twListToDF(jeffduncan)
  write.csv(jeffduncanDF,"data/jeffduncan.csv")
  timmonsDF <- twitteR::twListToDF(timmons)
  write.csv(timmonsDF,"data/timmons.csv")
  ralphnormanDF <- twitteR::twListToDF(ralphnorman)
  write.csv(ralphnormanDF,"data/ralphnorman.csv")
  tomriceDF <- twitteR::twListToDF(tomrice)
  write.csv(tomriceDF,"data/tomrice.csv")
  dustyjohnsonDF <- twitteR::twListToDF(dustyjohnson)
  write.csv(dustyjohnsonDF,"data/dustyjohnson.csv")
  philroeDF <- twitteR::twListToDF(philroe)
  write.csv(philroeDF,"data/philroe.csv")
  timburchettDF <- twitteR::twListToDF(timburchett)
  write.csv(timburchettDF,"data/timburchett.csv")
  chuckDF <- twitteR::twListToDF(chuck)
  write.csv(chuckDF,"data/chuck.csv")
  dejarlaisDF <- twitteR::twListToDF(desjarlais)
  write.csv(dejarlaisDF,"data/desjarlais.csv")
  johnroseDF <- twitteR::twListToDF(johnrose)
  write.csv(johnroseDF,"data/johnrose.csv")
  markgreenDF <- twitteR::twListToDF(markgreen)
  write.csv(markgreenDF,"data/markgreen.csv")
  davidkustoffDF <- twitteR::twListToDF(davidkustoff)
  write.csv(davidkustoffDF,"data/davidkustoff.csv")
  louiegohmertDF <- twitteR::twListToDF(louiegohmert)
  write.csv(louiegohmertDF,"data/louiegohmert.csv")
  dancrenshawDF <- twitteR::twListToDF(dancrenshaw)
  write.csv(dancrenshawDF,"data/dancrenshaw.csv")
  vantaylorDF <- twitteR::twListToDF(vantaylor)
  write.csv(vantaylorDF,"data/vantaylor.csv")
  ratcliffeDF <- twitteR::twListToDF(ratcliffe)
  write.csv(ratcliffeDF,"data/ratcliffe.csv")
  lancegoodenDF <- twitteR::twListToDF(lancegooden)
  write.csv(lancegoodenDF,"data/lancegooden.csv")
  ronwrightDF <- twitteR::twListToDF(ronwright)
  write.csv(ronwrightDF,"data/ronwright.csv")
  kevinbradyDF <- twitteR::twListToDF(kevinbrady)
  write.csv(kevinbradyDF,"data/kevinbrady.csv")
  mccaulDF <- twitteR::twListToDF(mccaul)
  write.csv(mccaulDF,"data/mccaul.csv")
  conawayDF <- twitteR::twListToDF(conaway)
  write.csv(conawayDF,"data/conaway.csv")
  kaygrangerDF <- twitteR::twListToDF(kaygranger)
  write.csv(kaygrangerDF,"data/kaygranger.csv")
  macDF <- twitteR::twListToDF(mac)
  write.csv(macDF,"data/mac.csv")
  randyDF <- twitteR::twListToDF(randy)
  write.csv(randyDF,"data/randy.csv")
  billfloresDF <- twitteR::twListToDF(billflores)
  write.csv(billfloresDF,"data/billflores.csv")
  arringtonDF <- twitteR::twListToDF(arrington)
  write.csv(arringtonDF,"data/arrington.csv")
  chiproyDF <- twitteR::twListToDF(chiproy)
  write.csv(chiproy,"data/chiproy.csv")
  peteolsonDF <- twitteR::twListToDF(peteolson)
  write.csv(peteolsonDF,"data/peteolson.csv")
  hurdDF <- twitteR::twListToDF(hurd)
  write.csv(hurdDF,"data/hurd.csv")
  kenmarchantDF <- twitteR::twListToDF(kenmarchant)
  write.csv(kenmarchantDF,"data/kenmarchant.csv")
  rwilliamsDF <- twitteR::twListToDF(rwilliams)
  write.csv(rwilliamsDF,"data/rwilliams.csv")
  michaelburgessDF <- twitteR::twListToDF(michaelburgess)
  write.csv(michaelburgessDF,"data/michaelburgess.csv")
  cloudDF <- twitteR::twListToDF(cloud)
  write.csv(cloudDF,"data/cloud.csv")
  carterDF <- twitteR::twListToDF(carter)
  write.csv(carterDF,"data/carter.csv")
  brianbabinDF <- twitteR::twListToDF(brianbabin)
  write.csv(brianbabinDF,"data/brianbabin.csv")
  robbishopDF <- twitteR::twListToDF(robbishop)
  write.csv(robbishopDF,"data/robbishop.csv")
  chrisstewartDF <- twitteR::twListToDF(chrisstewart)
  write.csv(chrisstewartDF,"data/chrisstewart.csv")
  johncurtisDF <- twitteR::twListToDF(johncurtis)
  write.csv(johncurtisDF,"data/johncurtis.csv")
  robwittmanDF <- twitteR::twListToDF(robwittman)
  write.csv(robwittmanDF,"data/robwittman.csv")
  rigglemanDF <- twitteR::twListToDF(riggleman)
  write.csv(rigglemanDF,"data/riggleman.csv")
  benclineDF <- twitteR::twListToDF(bencline)
  write.csv(benclineDF,"data/bencline.csv")
  mgriffithDF <- twitteR::twListToDF(mgriffith)
  write.csv(mgriffithDF,"data/mgriffith.csv")
  newhouseDF <- twitteR::twListToDF(newhouse)
  write.csv(newhouseDF,"data/newhouse.csv")
  bryansteilDF <- twitteR::twListToDF(bryansteil)
  write.csv(bryansteilDF,"data/bryansteil.csv")
  jimDF <- twitteR::twListToDF(jim)
  write.csv(jimDF,"data/jim.csv")
  grothmanDF <- twitteR::twListToDF(grothman)
  write.csv(grothmanDF,"data/grothman.csv")
  seanduffyDF <- twitteR::twListToDF(seanduffy)
  write.csv(seanduffyDF,"data/seanduffy.csv")
  gallagherDF <- twitteR::twListToDF(gallagher)
  write.csv(gallagherDF,"data/gallagher.csv")
  mckinleyDF <- twitteR::twListToDF(mckinley)
  write.csv(mckinleyDF,"data/mckinley.csv")
  alexmooneyDF <- twitteR::twListToDF(alexmooney)
  write.csv(alexmooneyDF,"data/alexmooney.csv")
  
  
  
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
  
  hormrDF <- twitteR::twListToDF(hormrtweets)
  write.csv(hormrDF, "data/hormrtweets.csv")
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
#' @import twitteR
#' @import rtweet
#' @import dplyr
#' @import readr
#' @import utils
#' @import tm
#' @import tidytext
#' @import remotes
#' @export

hormaleD <- function() {
  ohalleran <- twitteR::userTimeline('@RepOHalleran', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  raulgrijalva<- twitteR::userTimeline('@RepRaulGrijalva', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rubengallego <- twitteR::userTimeline('@RepRubenGallego', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gregstanton<- twitteR::userTimeline('@RepGregStanton', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  garamendi<- twitteR::userTimeline('@RepGaramendi', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  thompson<- twitteR::userTimeline('@RepThompson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mcnerney <- twitteR::userTimeline('@RepMcNerney', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  joshharder <- twitteR::userTimeline('@RepJoshHarder', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  desaulnier<- twitteR::userTimeline('@RepDeSaulnier', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  swalwell<- twitteR::userTimeline('@RepSwalwell', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jimcosta <- twitteR::userTimeline('@RepJimCosta', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  rokhanna <- twitteR::userTimeline('@RepRoKhanna', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jimmypanetta <- twitteR::userTimeline('@RepJimmyPanetta', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tjcox<- twitteR::userTimeline('@RepTjCox', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  carbajal <- twitteR::userTimeline('@RepCarbajal', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  adamschiff <- twitteR::userTimeline('@RepAdamSchiff', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cardenas<- twitteR::userTimeline('@RepCardenas', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bradsherman <- twitteR::userTimeline('@BradSherman', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  peteaguilar<- twitteR::userTimeline('@RepPeteAguilar', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tedlieu<- twitteR::userTimeline('@RepTedLieu', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jimmygomez <- twitteR::userTimeline('@RepJimmyGomez', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  ruiz<- twitteR::userTimeline('@CongressmanRuiz', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gilcisneros <- twitteR::userTimeline('@RepGilCisneros', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  marktakano<- twitteR::userTimeline('@RepMarkTakano', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  loucorrea<- twitteR::userTimeline('@RepLouCorrea', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lowenthal<- twitteR::userTimeline('@RepLowenthal', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  harley<- twitteR::userTimeline('@RepHarley', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mikelevin <- twitteR::userTimeline('@RepMikeLevin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  hunter<- twitteR::userTimeline('@Rep_Hunter', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  juanvargas <- twitteR::userTimeline('@RepJuanVargas', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  scottpeters<- twitteR::userTimeline('@RepScottPeters', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  joeneguse<- twitteR::userTimeline('@RepJoeNeguse', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jasoncrow<- twitteR::userTimeline('@RepJasonCrow', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  perlmutter<- twitteR::userTimeline('@RepPerlmutter', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johnlarson<- twitteR::userTimeline('@RepJohnLarson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  joecourtney<- twitteR::userTimeline('@RepJoeCourtney', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jahimes<- twitteR::userTimeline('@jahimes', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  allawson <- twitteR::userTimeline('@RepAlLawsonJr', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  darrensoto <- twitteR::userTimeline('@RepDarrenSoto', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  valdemings<- twitteR::userTimeline('@RepValDemings', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  charliecrist<- twitteR::userTimeline('@RepCharlieCrist', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  teddeutch<- twitteR::userTimeline('@RepTedDeutch', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  sanfordbishop<- twitteR::userTimeline('@SanfordBishop', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  hankjohnson<- twitteR::userTimeline('@RepHankJohnson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johnlewis<- twitteR::userTimeline('@repjohnlewis', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lucymcbath<- twitteR::userTimeline('@RepLucyMcBath', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  davidscott<- twitteR::userTimeline('@repdavidscott', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  edcase<- twitteR::userTimeline('@RepEdCase', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  daveloebsack <- twitteR::userTimeline('@daveloebsack', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bobbyrush<- twitteR::userTimeline('@RepBobbyRush', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lipinski<- twitteR::userTimeline('@RepLipinski', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  chuygarcia <- twitteR::userTimeline('@RepChuyGarcia', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mikequigley<- twitteR::userTimeline('@RepMikeQuigley', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  casten<- twitteR::userTimeline('@RepCasten', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dannydavis <- twitteR::userTimeline('@RepDannyDavis', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  raja<- twitteR::userTimeline('@CongressmanRaja', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  schneider <- twitteR::userTimeline('@RepSchneider', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  billfoster<- twitteR::userTimeline('@RepBillFoster', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  visclosky<- twitteR::userTimeline('@RepVisclosky', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  andrecarson<- twitteR::userTimeline('@RepAndreCarson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  johnyarmuth<- twitteR::userTimeline('@RepJohnYarmuth', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  richmond<- twitteR::userTimeline('@RepRichmond', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  chelliepingree <- twitteR::userTimeline('@chelliepingree', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  golden<- twitteR::userTimeline('@RepGolden', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dutch <- twitteR::userTimeline('@Call_Me_Dutch', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  sarbanes <- twitteR::userTimeline('@RepSarbanes', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  anthonybrown <- twitteR::userTimeline('@RepAnthonyBrown', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  hoyer<- twitteR::userTimeline('@LeaderHoyer', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  davidtrone<- twitteR::userTimeline('@RepDavidTrone', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cummings<- twitteR::userTimeline('@RepCummings', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  raskin <- twitteR::userTimeline('@RepRaskin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  richardneal <- twitteR::userTimeline('@RepRichardNeal', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mcgovern<- twitteR::userTimeline('@RepMcGovern', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  joekennedy <- twitteR::userTimeline('@RepJoeKennedy', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  moulton<- twitteR::userTimeline('@teammoulton', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  stephenlynch <- twitteR::userTimeline('@RepStephenLynch', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  keating<- twitteR::userTimeline('@USRepKeating', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dankildee <- twitteR::userTimeline('@RepDanKildee', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  andylevin<- twitteR::userTimeline('@RepAndyLevin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cleaver<- twitteR::userTimeline('@repcleaver', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  benniethompson <- twitteR::userTimeline('@BennieGThompson', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gkbutterfield<- twitteR::userTimeline('@GKButterfield', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  davidprice<- twitteR::userTimeline('@RepDavidEPrice', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  chrispappas<- twitteR::userTimeline('@RepChrisPappas', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  donaldnorcross<- twitteR::userTimeline('@DonaldNorcross', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  andykim<- twitteR::userTimeline('@RepAndyKimNJ', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  joshg <- twitteR::userTimeline('@RepJoshG', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  frankpallone <- twitteR::userTimeline('@FrankPallone', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  malinowski<- twitteR::userTimeline('@RepMalinowski', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  sires<- twitteR::userTimeline('@RepSires', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  billpascrell <- twitteR::userTimeline('@BillPascrell', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  donaldpayne<- twitteR::userTimeline('@RepDonaldPayne', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  sherrill<- twitteR::userTimeline('@RepSherrill', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  benraylujan <- twitteR::userTimeline('@repbenraylujan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  horsford<- twitteR::userTimeline('@RepHorsford', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  tomsuozzi <- twitteR::userTimeline('@RepTomSuozzi', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gregorymeeks<- twitteR::userTimeline('@RepGregoryMeeks', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gracemeng<- twitteR::userTimeline('@RepGraceMeng', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jeffries<- twitteR::userTimeline('@RepJeffries', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jerrynadler <- twitteR::userTimeline('@RepJerryNadler', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  maxrose<- twitteR::userTimeline('@RepMaxRose', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  espaillat <- twitteR::userTimeline('@RepEspaillat', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  joseserrano<- twitteR::userTimeline('@RepJoseSerrano', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  eliotengel<- twitteR::userTimeline('@RepEliotEngel', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  seanmaloney<- twitteR::userTimeline('@RepSeanMaloney', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  delgado<- twitteR::userTimeline('@repdelgado', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  paultonko <- twitteR::userTimeline('@RepPaulTonko', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  brindisi<- twitteR::userTimeline('@RepBrindisi', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  joemorelle <- twitteR::userTimeline('@RepJoeMorelle', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  brianhiggins<- twitteR::userTimeline('@RepBrianHiggins', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  timryan<- twitteR::userTimeline('@RepTimRyan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gregwalden <- twitteR::userTimeline('@repgregwalden', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  blumenauer<- twitteR::userTimeline('@repblumenauer', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  peterdefazio<- twitteR::userTimeline('@RepPeterDeFazio', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  schrader<- twitteR::userTimeline('@RepSchrader', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  boyle <- twitteR::userTimeline('@CongBoyle', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dwightevans <- twitteR::userTimeline('@RepDwightEvans', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cartwright<- twitteR::userTimeline('@RepCartwright', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  conorlamb<- twitteR::userTimeline('@RepConorLamb', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mikedoyle<- twitteR::userTimeline('@USRepMikeDoyle', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  davidcicilline<- twitteR::userTimeline('@davidcicilline', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jimlangevin<- twitteR::userTimeline('@JimLangevin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cunningham<- twitteR::userTimeline('@RepCunningham', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  clyburn<- twitteR::userTimeline('@WhipClyburn', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  jimcooper <- twitteR::userTimeline('@repjimcooper', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cohen<- twitteR::userTimeline('@RepCohen', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  algreen <- twitteR::userTimeline('@RepAlGreen', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gonzalez <- twitteR::userTimeline('@RepGonzalez', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  joaquincastro <- twitteR::userTimeline('@JoaquinCastrotx', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  cuellar<- twitteR::userTimeline('@RepCuellar', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  ebj <- twitteR::userTimeline('@RepEBJ', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  colinallred <- twitteR::userTimeline('@RepColinAllred', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  veasey<- twitteR::userTimeline('@RepVeasey', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  filemonvela <- twitteR::userTimeline('@RepFilemonVela', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  lloyddoggett<- twitteR::userTimeline('@RepLloydDoggett', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  benmcadams<- twitteR::userTimeline('@RepBenMcAdams', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  bobbyscott<- twitteR::userTimeline('@BobbyScott', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  mceachin<- twitteR::userTimeline('@RepMcEachin', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  donbeyer <- twitteR::userTimeline('@RepDonBeyer', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  gerryconnolly <- twitteR::userTimeline('@GerryConnolly', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  peterwelch<- twitteR::userTimeline('@PeterWelch', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  ricklarsen<- twitteR::userTimeline('@RepRickLarsen', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  derekkilmer<- twitteR::userTimeline('@RepDerekKilmer', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  adamsmith<- twitteR::userTimeline('@RepAdamSmith', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  dennyheck<- twitteR::userTimeline('@RepDennyHeck', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  markpocan<- twitteR::userTimeline('@repmarkpocan', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  ronkind<- twitteR::userTimeline('@RepRonKind', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  carolmiller <- twitteR::userTimeline('@RepCarolMiller', n = 50, maxID=NULL, includeRts=FALSE, excludeReplies=TRUE)
  
  
  
  
  
  
  
  
  
  
  
  ohalleranDF <- twitteR::twListToDF(ohalleran)
  write.csv(ohalleranDF,"data/ohalleran.csv")
  raulgrijalvaDF <- twitteR::twListToDF(raulgrijalva)
  write.csv(raulgrijalvaDF,"data/raulgrijalva.csv")
  rubengallegoDF <- twitteR::twListToDF(rubengallego)
  write.csv(rubengallegoDF,"data/rubengallego.csv")
  gregstantonDF <- twitteR::twListToDF(gregstanton)
  write.csv(gregstantonDF,"data/gregstanton.csv")
  garamendiDF <- twitteR::twListToDF(garamendi)
  write.csv(garamendiDF,"data/garamendi.csv")
  thompsonDF <- twitteR::twListToDF(thompson)
  write.csv(thompsonDF,"data/thompson.csv")
  mcnerneyDF <- twitteR::twListToDF(mcnerney)
  write.csv(mcnerneyDF,"data/mcnerney.csv")
  joshharderDF <- twitteR::twListToDF(joshharder)
  write.csv(joshharderDF,"data/joshharder.csv")
  desaulnierDF <- twitteR::twListToDF(desaulnier)
  write.csv(desaulnierDF,"data/desaulnier.csv")
  swalwellDF <- twitteR::twListToDF(swalwell)
  write.csv(swalwellDF,"data/swalwell.csv")
  jimcostaDF <- twitteR::twListToDF(jimcosta)
  write.csv(jimcostaDF,"data/jimcosta.csv")
  rokhannaDF <- twitteR::twListToDF(rokhanna)
  write.csv(rokhannaDF,"data/rokhanna.csv")
  jimmypanettaDF <- twitteR::twListToDF(jimmypanetta)
  write.csv(jimmypanettaDF,"data/jimmypanetta.csv")
  tjcoxDF <- twitteR::twListToDF(tjcox)
  write.csv(tjcoxDF,"data/tjcox.csv")
  carbajalDF <- twitteR::twListToDF(carbajal)
  write.csv(carbahal,"data/carbajal.csv")
  adamschiffDF <- twitteR::twListToDF(adamschiff)
  write.csv(adamschiffDF,"data/adamschiff.csv")
  cardenasDF <- twitteR::twListToDF(cardenas)
  write.csv(cardenasDF,"data/cardenas.csv")
  bradshermanDF <- twitteR::twListToDF(bradsherman)
  write.csv(bradshermanDF,"data/bradsherman.csv")
  peteaguilarDF <- twitteR::twListToDF(peteaguilar)
  write.csv(peteaguilarDF,"data/peteaguilar.csv")
  tedlieuDF <- twitteR::twListToDF(tedlieu)
  write.csv(tedlieuDF,"data/tedlieu.csv")
  jimmygomezDF <- twitteR::twListToDF(jimmygomez)
  write.csv(jimmygomezDF,"data/jimmygomez.csv")
  ruizDF <- twitteR::twListToDF(ruiz)
  write.csv(ruizDF,"data/ruiz.csv")
  gilcisnerosDF <- twitteR::twListToDF(gilcisneros)
  write.csv(gilcisnerosDF,"data/gilcisneros.csv")
  marktakanoDF <- twitteR::twListToDF(marktakano)
  write.csv(marktakanoDF,"data/marktakano.csv")
  loucorreaDF <- twitteR::twListToDF(loucorrea)
  write.csv(loucorreaDF,"data/loucorrea.csv")
  lowenthalDF <- twitteR::twListToDF(lowenthal)
  write.csv(lowenthalDF,"data/lowenthal.csv")
  harleyDF <- twitteR::twListToDF(harley)
  write.csv(harleyDF,"data/harley.csv")
  mikelevinDF <- twitteR::twListToDF(mikelevin)
  write.csv(mikelevinDF,"data/mikelevin.csv")
  hunterDF <- twitteR::twListToDF(hunter)
  write.csv(hunterDF,"data/hunter.csv")
  juanvargasDF <- twitteR::twListToDF(juanvargas)
  write.csv(juanvargasDF,"data/juanvargas.csv")
  scottpetersDF <- twitteR::twListToDF(scottpeters)
  write.csv(scottpetersDF,"data/scottpeters.csv")
  joeneguseDF <- twitteR::twListToDF(joeneguse)
  write.csv(joeneguseDF,"data/joeneguse.csv")
  jasoncrowDF <- twitteR::twListToDF(jasoncrow)
  write.csv(jasoncrowDF,"data/jasoncrow.csv")
  perlmutterDF <- twitteR::twListToDF(perlmutter)
  write.csv(perlmutterDF,"data/perlmutter.csv")
  johnlarsonDF <- twitteR::twListToDF(johnlarson)
  write.csv(johnlarsonDF,"data/johnlarson.csv")
  joecourtneyDF <- twitteR::twListToDF(joecourtney)
  write.csv(joecourtneyDF,"data/joecourtney.csv")
  jahimesDF <- twitteR::twListToDF(jahimes)
  write.csv(jahimesDF,"data/jahimes.csv")
  allawsonDF <- twitteR::twListToDF(allawson)
  write.csv(allawsonDF,"data/allawson.csv")
  darrensotoDF <- twitteR::twListToDF(darrensoto)
  write.csv(darrensotoDF,"data/darrensoto.csv")
  valdemingsDF <- twitteR::twListToDF(valdemings)
  write.csv(valdemingsDF,"data/valdemings.csv")
  charliecristDF <- twitteR::twListToDF(charliecrist)
  write.csv(charliecristDF, "data/charliecrist.csv")
  teddeutchDF <- twitteR::twListToDF(teddeutch)
  write.csv(teddeutchDF,"data/teddeutch.csv")
  sanfordbishopDF <- twitteR::twListToDF(sanfordbishop)
  write.csv(sanfordbishopDF,"data/sanfordbishop.csv")
  hankjohnsonDF <- twitteR::twListToDF(hankjohnson)
  write.csv(hankjohnsonDF,"data/hankjohnson.csv")
  johnlewisDF <- twitteR::twListToDF(johnlewis)
  write.csv(johnlewisDF,"data/johnlewis.csv")
  lucymcbathDF <- twitteR::twListToDF(lucymcbath)
  write.csv(lucymcbathDF,"data/lucymcbath.csv")
  davidscottDF <- twitteR::twListToDF(davidscott)
  write.csv(davidscottDF,"data/davidscott.csv")
  edcaseDF <- twitteR::twListToDF(edcase)
  write.csv(edcaseDF,"data/edcase.csv")
  daveloebsackDF <- twitteR::twListToDF(daveloebsack)
  write.csv(daveloebsackDF,"data/daveloebsack.csv")
  bobbyrushDF <- twitteR::twListToDF(bobbyrush)
  write.csv(bobbyrushDF,"data/bobbyrush.csv")
  lipinskiDF <- twitteR::twListToDF(lipinski)
  write.csv(lipinskiDF,"data/lipinski.csv")
  chuygarciaDF <- twitteR::twListToDF(chuygarcia)
  write.csv(chuygarciaDF,"data/chuygarcia.csv")
  mikequigleyDF <- twitteR::twListToDF(mikequigley)
  write.csv(mikequigleyDF,"data/mikequigley.csv")
  castenDF <- twitteR::twListToDF(casten)
  write.csv(castenDF,"data/casten.csv")
  dannydavisDF <- twitteR::twListToDF(dannydavis)
  write.csv(dannydavisDF,"data/dannydavis.csv")
  rajaDF <- twitteR::twListToDF(raja)
  write.csv(rajaDF,"data/raja.csv")
  schneiderDF <- twitteR::twListToDF(schneider)
  write.csv(schneiderDF,"data/schneider.csv")
  billfosterDF <- twitteR::twListToDF(billfoster)
  write.csv(billfosterDF,"data/billfoster.csv")
  viscloskyDF <- twitteR::twListToDF(visclosky)
  write.csv(viscloskyDF,"data/visclosky.csv")
  andrecarsonDF <- twitteR::twListToDF(andrecarson)
  write.csv(andrecarsonDF,"data/andrecarson.csv")
  johnyarmuthDF <- twitteR::twListToDF(johnyarmuth)
  write.csv(johnyarmuthDF,"data/johnyarmuth.csv")
  richmondDF <- twitteR::twListToDF(richmond)
  write.csv(richmondDF,"data/richmond.csv")
  chelliepingreeDF <- twitteR::twListToDF(chelliepingree)
  write.csv(chelliepingreeDF,"data/chelliepingree.csv")
  goldenDF <- twitteR::twListToDF(golden)
  write.csv(goldenDF,"data/golden.csv")
  dutchDF <- twitteR::twListToDF(dutch)
  write.csv(dutchDF,"data/dutch.csv")
  sarbanesDF <- twitteR::twListToDF(sarbanes)
  write.csv(sarbanesDF,"data/sarbanes.csv")
  anthonybrownDF <- twitteR::twListToDF(anthonybrown)
  write.csv(anthonybrownDF,"data/anthonybrown.csv")
  hoyerDF <- twitteR::twListToDF(hoyer)
  write.csv(hoyerDF,"data/hoyer.csv")
  davidtroneDF <- twitteR::twListToDF(davidtrone)
  write.csv(davidtroneDF,"data/davidtrone.csv")
  cummingsDF <- twitteR::twListToDF(cummings)
  write.csv(cummingsDF,"data/cummings.csv")
  raskinDF <- twitteR::twListToDF(raskin)
  write.csv(raskinDF,"data/raskin.csv")
  richardnealDF <- twitteR::twListToDF(richardneal)
  write.csv(richardnealDF,"data/richardneal.csv")
  mcgovernDF <- twitteR::twListToDF(mcgovern)
  write.csv(mcgovernDF,"data/mcgovern.csv")
  joekennedyDF <- twitteR::twListToDF(joekennedy)
  write.csv(joekennedyDF,"data/joekennedy.csv")
  moultonDF <- twitteR::twListToDF(moulton)
  write.csv(moultonDF,"data/moulton.csv")
  stephenlynchDF <- twitteR::twListToDF(stephenlynch)
  write.csv(stephenlynchDF,"data/stephenlynch.csv")
  keatingDF <- twitteR::twListToDF(keating)
  write.csv(keatingDF,"data/keating.csv")
  dankildeeDF <- twitteR::twListToDF(dankildee)
  write.csv(dankildeeDF,"data/dankildee.csv")
  andylevinDF <- twitteR::twListToDF(andylevin)
  write.csv(andylevinDF,"data/andylevin.csv")
  cleaverDF <- twitteR::twListToDF(cleaver)
  write.csv(cleaverDF,"data/cleaver.csv")
  benniethompsonDF <- twitteR::twListToDF(benniethompson)
  write.csv(benniethompsonDF,"data/benniethompson.csv")
  gkbutterfieldDF <- twitteR::twListToDF(gkbutterfield)
  write.csv(gkbutterfieldDF,"data/gkbutterfield.csv")
  davidpriceDF <- twitteR::twListToDF(davidprice)
  write.csv(davidpriceDF,"data/davidprice.csv")
  chrispappasDF <- twitteR::twListToDF(chrispappas)
  write.csv(chrispappasDF,"data/chrispappas.csv")
  donaldnorcrossDF <- twitteR::twListToDF(donaldnorcross)
  write.csv(donaldnorcrossDF,"data/donaldnorcross.csv")
  andykimDF <- twitteR::twListToDF(andykim)
  write.csv(andykimDF,"data/andykim.csv")
  joshgDF <- twitteR::twListToDF(joshg)
  write.csv(joshgDF,"data/joshg.csv")
  frankpalloneDF <- twitteR::twListToDF(frankpallone)
  write.csv(frankpalloneDF,"data/frankpallone.csv")
  malinowskiDF <- twitteR::twListToDF(malinowski)
  write.csv(malinowskiDF,"data/malinowski.csv")
  siresDF <- twitteR::twListToDF(sires)
  write.csv(siresDF,"data/sires.csv")
  billpascrellDF <- twitteR::twListToDF(billpascrell)
  write.csv(billpascrellDF,"data/billpascrell.csv")
  donaldpayneDF <- twitteR::twListToDF(donaldpayne)
  write.csv(donaldpayneDF,"data/donaldpayne.csv")
  sherrillDF <- twitteR::twListToDF(sherrill)
  write.csv(sherrillDF,"data/sherrill.csv")
  benraylujanDF <- twitteR::twListToDF(benraylujan)
  write.csv(benraylujanDF,"data/benraylujan.csv")
  horsfordDF <- twitteR::twListToDF(horsford)
  write.csv(horsfordDF,"data/horsford.csv")
  tomuozziDF <- twitteR::twListToDF(tomsuozzi)
  write.csv(tomuozziDF,"data/tomsuozzi.csv")
  gregorymeeksDF <- twitteR::twListToDF(gregorymeeks)
  write.csv(gregorymeeksDF,"data/gregorymeeks.csv")
  gracemengDF <- twitteR::twListToDF(gracemeng)
  write.csv(gracemengDF,"data/gracemeng.csv")
  jeffriesDF <- twitteR::twListToDF(jeffries)
  write.csv(jeffriesDF,"data/jeffries.csv")
  jerrynadlerDF <- twitteR::twListToDF(jerrynadler)
  write.csv(jerrynadlerDF,"data/jerrynadler.csv")
  maxroseDF <- twitteR::twListToDF(maxrose)
  write.csv(maxroseDF,"data/maxrose.csv")
  espillatDF <- twitteR::twListToDF(espaillat)
  write.csv(espillatDF,"data/espaillat.csv")
  joseserranoDF <- twitteR::twListToDF(joseserrano)
  write.csv(joseserranoDF,"data/joseserrano.csv")
  eliotengelDF <- twitteR::twListToDF(eliotengel)
  write.csv(eliotengelDF,"data/eliotengel.csv")
  seanmaloneyDF <- twitteR::twListToDF(seanmaloney)
  write.csv(seanmaloneyDF,"data/seanmaloney.csv")
  delgadoDF <- twitteR::twListToDF(delgado)
  write.csv(delgadoDF,"data/delgado.csv")
  paultonkoDF <- twitteR::twListToDF(paultonko)
  write.csv(paultonkoDF,"data/paultonko.csv")
  brindisiDF <- twitteR::twListToDF(brindisi)
  write.csv(brindisiDF,"data/brindisi.csv")
  joemorelleDF <- twitteR::twListToDF(joemorelle)
  write.csv(joemorelleDF,"data/joemorelle.csv")
  brianhigginsDF <- twitteR::twListToDF(brianhiggins)
  write.csv(brianhigginsDF,"data/brianhiggins.csv")
  timryanDF <- twitteR::twListToDF(timryan)
  write.csv(timryanDF,"data/timryan.csv")
  gregwaldenDF <- twitteR::twListToDF(gregwalden)
  write.csv(gregwaldenDF,"data/gregwalden.csv")
  blumenauerDF <- twitteR::twListToDF(blumenauer)
  write.csv(blumenauerDF,"data/blumenauer.csv")
  peterdefazioDF <- twitteR::twListToDF(peterdefazio)
  write.csv(peterdefazioDF,"data/peterdefazio.csv")
  schraderDF <- twitteR::twListToDF(schrader)
  write.csv(schraderDF,"data/schrader.csv")
  boyleDF <- twitteR::twListToDF(boyle)
  write.csv(boyleDF,"data/boyle.csv")
  dwightevansDF <- twitteR::twListToDF(dwightevans)
  write.csv(dwightevansDF,"data/dwightevans.csv")
  cartwrightDF <- twitteR::twListToDF(cartwright)
  write.csv(cartwrightDF,"data/cartwright.csv")
  conorlambDF <- twitteR::twListToDF(conorlamb)
  write.csv(conorlambDF,"data/conorlamb.csv")
  mikedoyleDF <- twitteR::twListToDF(mikedoyle)
  write.csv(mikedoyleDF,"data/mikedoyle.csv")
  davidcicillineDF <- twitteR::twListToDF(davidcicilline)
  write.csv(davidcicillineDF,"data/davidcicilline.csv")
  jimlangevinDF <- twitteR::twListToDF(jimlangevin)
  write.csv(jimlangevinDF,"data/jimlangevin.csv")
  cunninghamDF <- twitteR::twListToDF(cunningham)
  write.csv(cunninghamDF,"data/cunningham.csv")
  clyburnDF <- twitteR::twListToDF(clyburn)
  write.csv(clyburnDF,"data/clyburn.csv")
  jimcooperDF <- twitteR::twListToDF(jimcooper)
  write.csv(jimcooperDF,"data/jimcooper.csv")
  cohenDF <- twitteR::twListToDF(cohen)
  write.csv(cohenDF,"data/cohen.csv")
  algreenDF <- twitteR::twListToDF(algreen)
  write.csv(algreenDF,"data/algreen.csv")
  gonzalezDF <- twitteR::twListToDF(gonzalez)
  write.csv(gonzalezDF,"data/gonzalez.csv")
  joaquincastroDF <- twitteR::twListToDF(joaquincastro)
  write.csv(joaquincastroDF,"data/joaquincastro.csv")
  cuellarDF <- twitteR::twListToDF(cuellar)
  write.csv(cuellarDF,"data/cuellar.csv")
  ebjDF <- twitteR::twListToDF(ebj)
  write.csv(ebjDF,"data/ebj.csv")
  colinallredDF <- twitteR::twListToDF(colinallred)
  write.csv(colinallredDF,"data/colinallred.csv")
  veaseyDF <- twitteR::twListToDF(veasey)
  write.csv(veaseyDF,"data/veasey.csv")
  filemonvelaDF <- twitteR::twListToDF(filemonvela)
  write.csv(filemonvelaDF,"data/filemonvela.csv")
  lloyddoggettDF <- twitteR::twListToDF(lloyddoggett)
  write.csv(lloyddoggettDF,"data/lloyddoggett.csv")
  benmcadamsDF <- twitteR::twListToDF(benmcadams)
  write.csv(benmcadamsDF,"data/benmcadams.csv")
  bobbyscottDF <- twitteR::twListToDF(bobbyscott)
  write.csv(bobbyscottDF,"data/bobbyscott.csv")
  mceachinDF <- twitteR::twListToDF(mceachin)
  write.csv(mceachinDF,"data/mceachin.csv")
  donbeyerDF <- twitteR::twListToDF(donbeyer)
  write.csv(donbeyerDF,"data/donbeyer.csv")
  gerryconnollyDF <- twitteR::twListToDF(gerryconnolly)
  write.csv(gerryconnollyDF,"data/gerryconnolly.csv")
  peterwelchDF <- twitteR::twListToDF(peterwelch)
  write.csv(peterwelchDF,"data/peterwelch.csv")
  ricklarsenDF <- twitteR::twListToDF(ricklarsen)
  write.csv(ricklarsenDF,"data/ricklarsen.csv")
  derekkilmerDF <- twitteR::twListToDF(derekkilmer)
  write.csv(derekkilmerDF,"data/derekkilmer.csv")
  adamsmithDF <- twitteR::twListToDF(adamsmith)
  write.csv(adamsmithDF,"data/adamsmith.csv")
  dennyheckDF <- twitteR::twListToDF(dennyheck)
  write.csv(dennyheckDF,"data/dennyheck.csv")
  markpocanDF <- twitteR::twListToDF(markpocan)
  write.csv(markpocanDF,"data/markpocan.csv")
  ronkindDF <- twitteR::twListToDF(ronkind)
  write.csv(ronkindDF,"data/ronkind.csv")
  carolmillerDF <- twitteR::twListToDF(carolmiller)
  write.csv(carolmillerDF,"data/carolmiller.csv")
  
  
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
  
  hormdDF <- twitteR::twListToDF(hormdtweets)
  write.csv(hormdDF, "data/hormdtweets.csv")
  hormdtweets <- readr::read_csv("data/hormdtweets.csv")
  if(nrow(hormdtweets)>0) {
    message("Check your Data Folder. Function ran successfully")
  }
}


