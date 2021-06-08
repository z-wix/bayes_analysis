Smash Data Analysis
================
Zack Wixom

![Super Smash
Logo](/Project/Figures/Smash/super-smash-bros-ultimate.PNG)

``` r
# Load Packages
library(tidyverse)
library(tidytext)
library(textdata)
library(rvest)
```

So my first attempt to get data on Fortnite players to understand
earnings and victory royales didn’t work. My API didn’t have sufficient
data for the players I wanted to analyze. So I am going to try to go a
different approach with Super Smash Bros.

I think that I can build a model to understand the relationship between
character stats and their win rates. I want to see if frame data,
weight, speed, etc, will increase or decrease chance of winning a match
against another character. Ideally I will be able to pit two characters
up against each other and see which one wins.

## Collecting Data

### Frame Data

In order to do this I need two sources of data: Character data and Match
data. I know of a [website](https://ultimateframedata.com/) that has
frame data for every character and I know of at least one data set of
match results data. I am not sure if I can get more but the more the
better so I can see the match ups for every character so I can make some
kind of comparison between the characters. Not sure if this is possible
but I will see.

``` r
## Web scrapping for Frame Data
tables <- read_html("https://ultimateframedata.com/stats.php") %>%
  html_nodes("table")

# # Practice table webscrape
# air_acceleration <- tables %>% 
#   .[1] %>%
#   html_table(fill = TRUE)

# Loop to grab all data tables
for (i in 1:14) {
  x <- tables %>% 
  .[i] %>%
  html_table(fill = TRUE)
  
  write_csv(as.data.frame(x), here::here("Project", "Data", paste("smash", i, ".csv", sep = ""))
}
```

Every Data table has a identifier, `Character`, variable. This will be
used to join together the datasets. However, there are some variable
names that are repeated but need to be fixed before joining together.
For example, every dataset comes with the ranking of the characters for
that statistic. So that will need to be changed so that we can
differentiate before each table’s ranking.

I also need to clean up the `Character` variable a bit. Some of the
tables have echo fighters together and some have them separate. The
final dataset will have them separate since people choose one echo
fighter over the other for various reasons (i.e. Dark Samus over Samus,
Daisy over Peach, etc.).

``` r
# Load in Air Acceleration Data >>> Needed to define variables better to avoid confusion
acceleration <- read.csv(here::here("Project", "Data", "smash1.csv")) %>%
  rename(
    rank_accel = Rank,
    base_accel = Base,
    additional_accel = Additional,
    max_accel = Max
  )

# acceleration <- read.csv(here::here("Project", "Data", "smash1_acceleration.csv"))

# Fix Names
acceleration$Character[acceleration$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
acceleration$Character[acceleration$Character == "DK"] <- "Donkey Kong"
acceleration$Character[acceleration$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
acceleration$Character[acceleration$Character == "DK"] <- "Donkey Kong"
acceleration$Character[acceleration$Character == "P.T. [Charizard]"] <- "Charizard"
acceleration$Character[acceleration$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
acceleration$Character[acceleration$Character == "P.T. [Squirtle]"] <- "Squirtle"
acceleration$Character[acceleration$Character == "PAC-MAN"] <- "Pac-Man"
acceleration$Character[acceleration$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
acceleration$Character[acceleration$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
acceleration$Character[acceleration$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
acceleration$Character[acceleration$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
acceleration$Character[acceleration$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
acceleration$Character[acceleration$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
acceleration$Character[acceleration$Character == "Dedede"] <- "King Dedede"
acceleration$Character[acceleration$Character == "Metaknight"] <- "Meta Knight"
acceleration$Character[acceleration$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
acceleration$Character[acceleration$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
acceleration <- acceleration %>% 
  separate_rows(Character, sep = "/")

# Write Data Locally
write_csv(acceleration, here::here("Project", "Data", "smash1_acceleration.csv"))

# Load in Air Speed Data
airspeed <- read.csv(here::here("Project", "Data", "smash2.csv")) %>% 
  rename(
    rank_airspeed = Rank
  )

# airspeed <- read.csv(here::here("Project", "Data", "smash2_airspeed.csv"))

# Fix Names
airspeed$Character[airspeed$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
airspeed$Character[airspeed$Character == "DK"] <- "Donkey Kong"
airspeed$Character[airspeed$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
airspeed$Character[airspeed$Character == "DK"] <- "Donkey Kong"
airspeed$Character[airspeed$Character == "P.T. [Charizard]"] <- "Charizard"
airspeed$Character[airspeed$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
airspeed$Character[airspeed$Character == "P.T. [Squirtle]"] <- "Squirtle"
airspeed$Character[airspeed$Character == "PAC-MAN"] <- "Pac-Man"
airspeed$Character[airspeed$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
airspeed$Character[airspeed$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
airspeed$Character[airspeed$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
airspeed$Character[airspeed$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
airspeed$Character[airspeed$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
airspeed$Character[airspeed$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
airspeed$Character[airspeed$Character == "Dedede"] <- "King Dedede"
airspeed$Character[airspeed$Character == "Metaknight"] <- "Meta Knight"
airspeed$Character[airspeed$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
airspeed$Character[airspeed$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
airspeed <- airspeed %>% 
  separate_rows(Character, sep = "/")

# Write data locally
write_csv(airspeed, here::here("Project", "Data", "smash2_airspeed.csv"))

# Load in Fall Speed Data
fallspeed <- read.csv(here::here("Project", "Data", "smash3.csv")) %>% 
  rename(
    rank_fallspeed = Rank
  )

# fallspeed <- read.csv(here::here("Project", "Data", "smash3_fallspeed.csv"))

# Fix Names
fallspeed$Character[fallspeed$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
fallspeed$Character[fallspeed$Character == "DK"] <- "Donkey Kong"
fallspeed$Character[fallspeed$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
fallspeed$Character[fallspeed$Character == "DK"] <- "Donkey Kong"
fallspeed$Character[fallspeed$Character == "P.T. [Charizard]"] <- "Charizard"
fallspeed$Character[fallspeed$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
fallspeed$Character[fallspeed$Character == "P.T. [Squirtle]"] <- "Squirtle"
fallspeed$Character[fallspeed$Character == "PAC-MAN"] <- "Pac-Man"
fallspeed$Character[fallspeed$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
fallspeed$Character[fallspeed$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
fallspeed$Character[fallspeed$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
fallspeed$Character[fallspeed$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
fallspeed$Character[fallspeed$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
fallspeed$Character[fallspeed$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
fallspeed$Character[fallspeed$Character == "Dedede"] <- "King Dedede"
fallspeed$Character[fallspeed$Character == "Metaknight"] <- "Meta Knight"
fallspeed$Character[fallspeed$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
fallspeed$Character[fallspeed$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
fallspeed <- fallspeed %>% 
  separate_rows(Character, sep = "/")

# Write Data Locally
write_csv(fallspeed, here::here("Project", "Data", "smash3_fallspeed.csv"))

# Load in Gravity Data
gravity <- read.csv(here::here("Project", "Data", "smash4.csv"))

# gravity <- read.csv(here::here("Project", "Data", "smash4_gravity.csv"))

# Fix Names
gravity$Character[gravity$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
gravity$Character[gravity$Character == "DK"] <- "Donkey Kong"
gravity$Character[gravity$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
gravity$Character[gravity$Character == "DK"] <- "Donkey Kong"
gravity$Character[gravity$Character == "P.T. [Charizard]"] <- "Charizard"
gravity$Character[gravity$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
gravity$Character[gravity$Character == "P.T. [Squirtle]"] <- "Squirtle"
gravity$Character[gravity$Character == "PAC-MAN"] <- "Pac-Man"
gravity$Character[gravity$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
gravity$Character[gravity$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
gravity$Character[gravity$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
gravity$Character[gravity$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
gravity$Character[gravity$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
gravity$Character[gravity$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
gravity$Character[gravity$Character == "Dedede"] <- "King Dedede"
gravity$Character[gravity$Character == "Metaknight"] <- "Meta Knight"
gravity$Character[gravity$Character =="Mii Swordfighter"] <- "Mii Sword Fighter"
gravity$Character[gravity$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
gravity <- gravity %>% 
  separate_rows(Character, sep = "/")

# Write Data Locally
write_csv(gravity, here::here("Project", "Data", "smash4_gravity.csv"))

# Load in Jump Height Data
jumpheight <- read.csv(here::here("Project", "Data", "smash5.csv")) %>% 
  rename(
    shorthop_height = Short.Hop,
    fullhop_height = Full.Hop,
    airjump_height = Air.Jump
  )

# jumpheight <- read.csv(here::here("Project", "Data", "smash5_jumpheight.csv"))

# Fix Names
jumpheight$Character[jumpheight$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
jumpheight$Character[jumpheight$Character == "DK"] <- "Donkey Kong"
jumpheight$Character[jumpheight$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
jumpheight$Character[jumpheight$Character == "DK"] <- "Donkey Kong"
jumpheight$Character[jumpheight$Character == "P.T. [Charizard]"] <- "Charizard"
jumpheight$Character[jumpheight$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
jumpheight$Character[jumpheight$Character == "P.T. [Squirtle]"] <- "Squirtle"
jumpheight$Character[jumpheight$Character == "PAC-MAN"] <- "Pac-Man"
jumpheight$Character[jumpheight$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
jumpheight$Character[jumpheight$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
jumpheight$Character[jumpheight$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
jumpheight$Character[jumpheight$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
jumpheight$Character[jumpheight$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
jumpheight$Character[jumpheight$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
jumpheight$Character[jumpheight$Character == "Dedede"] <- "King Dedede"
jumpheight$Character[jumpheight$Character == "Metaknight"] <- "Meta Knight"
jumpheight$Character[jumpheight$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
jumpheight$Character[jumpheight$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
jumpheight <- jumpheight %>% 
  separate_rows(Character, sep = "/")

# Write Data Locally
write_csv(jumpheight, here::here("Project", "Data", "smash5_jumpheight.csv"))

# Load in Jump Duration Data
jumpduration <- read.csv(here::here("Project", "Data", "smash6.csv")) %>% 
  rename(
    shorthop_dur = Short.Hop,
    fullhop_dur = Full.Hop
  )

# jumpduration <- read.csv(here::here("Project", "Data", "smash6_jumpduration.csv"))

# Fix Names
jumpduration$Character[jumpduration$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
jumpduration$Character[jumpduration$Character == "DK"] <- "Donkey Kong"
jumpduration$Character[jumpduration$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
jumpduration$Character[jumpduration$Character == "DK"] <- "Donkey Kong"
jumpduration$Character[jumpduration$Character == "P.T. [Charizard]"] <- "Charizard"
jumpduration$Character[jumpduration$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
jumpduration$Character[jumpduration$Character == "P.T. [Squirtle]"] <- "Squirtle"
jumpduration$Character[jumpduration$Character == "PAC-MAN"] <- "Pac-Man"
jumpduration$Character[jumpduration$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
jumpduration$Character[jumpduration$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
jumpduration$Character[jumpduration$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
jumpduration$Character[jumpduration$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
jumpduration$Character[jumpduration$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
jumpduration$Character[jumpduration$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
jumpduration$Character[jumpduration$Character == "Dedede"] <- "King Dedede"
jumpduration$Character[jumpduration$Character == "Metaknight"] <- "Meta Knight"
jumpduration$Character[jumpduration$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
jumpduration$Character[jumpduration$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
jumpduration <- jumpduration %>% 
  separate_rows(Character, sep = "/")

# Write data locally
write_csv(jumpduration, here::here("Project", "Data", "smash6_jumpduration.csv"))

# Load in Weight Data
weight <- read.csv(here::here("Project", "Data", "smash7.csv")) %>% 
  rename(
    rank_weight = Rank
  )

# weight <- read.csv(here::here("Project", "Data", "smash7_weight.csv"))

# Fix Names
weight$Character[weight$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
weight$Character[weight$Character == "DK"] <- "Donkey Kong"
weight$Character[weight$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
weight$Character[weight$Character == "DK"] <- "Donkey Kong"
weight$Character[weight$Character == "P.T. [Charizard]"] <- "Charizard"
weight$Character[weight$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
weight$Character[weight$Character == "P.T. [Squirtle]"] <- "Squirtle"
weight$Character[weight$Character == "PAC-MAN"] <- "Pac-Man"
weight$Character[weight$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
weight$Character[weight$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
weight$Character[weight$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
weight$Character[weight$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
weight$Character[weight$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
weight$Character[weight$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
weight$Character[weight$Character == "Dedede"] <- "King Dedede"
weight$Character[weight$Character == "Metaknight"] <- "Meta Knight"
weight$Character[weight$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
weight$Character[weight$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
weight <- weight %>% 
  separate_rows(Character, sep = "/")

# Write data locally
write_csv(weight, here::here("Project", "Data", "smash7_weight.csv"))

# Load in Landing Data
landing <- read.csv(here::here("Project", "Data", "smash8.csv"))

# landing <- read.csv(here::here("Project", "Data", "smash8_landing.csv"))

# Fix Names
landing$Character[landing$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
landing$Character[landing$Character == "DK"] <- "Donkey Kong"
landing$Character[landing$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
landing$Character[landing$Character == "DK"] <- "Donkey Kong"
landing$Character[landing$Character == "P.T. [Charizard]"] <- "Charizard"
landing$Character[landing$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
landing$Character[landing$Character == "P.T. [Squirtle]"] <- "Squirtle"
landing$Character[landing$Character == "PAC-MAN"] <- "Pac-Man"
landing$Character[landing$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
landing$Character[landing$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
landing$Character[landing$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
landing$Character[landing$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
landing$Character[landing$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
landing$Character[landing$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
landing$Character[landing$Character == "Dedede"] <- "King Dedede"
landing$Character[landing$Character == "Metaknight"] <- "Meta Knight"
landing$Character[landing$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
landing$Character[landing$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
landing <- landing %>% 
  separate_rows(Character, sep = "/")

# Write data locally
write_csv(landing, here::here("Project", "Data", "smash8_landing.csv"))

# Load in Walk Speed Data
walkspeed <- read.csv(here::here("Project", "Data", "smash9.csv")) %>% 
  rename(
    rank_walkspeed = Rank
  )

# walkspeed <- read.csv(here::here("Project", "Data", "smash9_walkspeed.csv"))

# Fix Names
walkspeed$Character[walkspeed$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
walkspeed$Character[walkspeed$Character == "DK"] <- "Donkey Kong"
walkspeed$Character[walkspeed$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
walkspeed$Character[walkspeed$Character == "DK"] <- "Donkey Kong"
walkspeed$Character[walkspeed$Character == "P.T. [Charizard]"] <- "Charizard"
walkspeed$Character[walkspeed$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
walkspeed$Character[walkspeed$Character == "P.T. [Squirtle]"] <- "Squirtle"
walkspeed$Character[walkspeed$Character == "PAC-MAN"] <- "Pac-Man"
walkspeed$Character[walkspeed$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
walkspeed$Character[walkspeed$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
walkspeed$Character[walkspeed$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
walkspeed$Character[walkspeed$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
walkspeed$Character[walkspeed$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
walkspeed$Character[walkspeed$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
walkspeed$Character[walkspeed$Character == "Dedede"] <- "King Dedede"
walkspeed$Character[walkspeed$Character == "Metaknight"] <- "Meta Knight"
walkspeed$Character[walkspeed$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
walkspeed$Character[walkspeed$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
walkspeed <- walkspeed %>% 
  separate_rows(Character, sep = "/")

# Write data locally
write_csv(walkspeed, here::here("Project", "Data", "smash9_walkspeed.csv"))

# Load in Dash and Run Data
dashrun <- read.csv(here::here("Project", "Data", "smash10.csv"))

# dashrun <- read.csv(here::here("Project", "Data", "smash10_dashrun.csv"))

# Fix Names
dashrun$Character[dashrun$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
dashrun$Character[dashrun$Character == "DK"] <- "Donkey Kong"
dashrun$Character[dashrun$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
dashrun$Character[dashrun$Character == "DK"] <- "Donkey Kong"
dashrun$Character[dashrun$Character == "P.T. [Charizard]"] <- "Charizard"
dashrun$Character[dashrun$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
dashrun$Character[dashrun$Character == "P.T. [Squirtle]"] <- "Squirtle"
dashrun$Character[dashrun$Character == "PAC-MAN"] <- "Pac-Man"
dashrun$Character[dashrun$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
dashrun$Character[dashrun$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
dashrun$Character[dashrun$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
dashrun$Character[dashrun$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
dashrun$Character[dashrun$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
dashrun$Character[dashrun$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
dashrun$Character[dashrun$Character == "Dedede"] <- "King Dedede"
dashrun$Character[dashrun$Character == "Metaknight"] <- "Meta Knight"
dashrun$Character[dashrun$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
dashrun$Character[dashrun$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
dashrun <- dashrun %>% 
  separate_rows(Character, sep = "/")

# Write data locally
write_csv(dashrun, here::here("Project", "Data", "smash10_dashrun.csv"))

# Load in Dash Turn Around Data
dashturn <- read.csv(here::here("Project", "Data", "smash11.csv")) %>% 
  rename(
    Fast.Initial.Dash = Fast..Initial.Dash.,
    Normal.Buffer.Window = Normal..Buffer.Window.,
    Slow.Full.Dash = Slow..Full.Dash.
  )

# dashturn <- read.csv(here::here("Project", "Data", "smash11_dashturnaround.csv"))

# Fix Names
dashturn$Character[dashturn$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
dashturn$Character[dashturn$Character == "DK"] <- "Donkey Kong"
dashturn$Character[dashturn$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
dashturn$Character[dashturn$Character == "DK"] <- "Donkey Kong"
dashturn$Character[dashturn$Character == "P.T. [Charizard]"] <- "Charizard"
dashturn$Character[dashturn$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
dashturn$Character[dashturn$Character == "P.T. [Squirtle]"] <- "Squirtle"
dashturn$Character[dashturn$Character == "PAC-MAN"] <- "Pac-Man"
dashturn$Character[dashturn$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
dashturn$Character[dashturn$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
dashturn$Character[dashturn$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
dashturn$Character[dashturn$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
dashturn$Character[dashturn$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
dashturn$Character[dashturn$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
dashturn$Character[dashturn$Character == "Dedede"] <- "King Dedede"
dashturn$Character[dashturn$Character == "Metaknight"] <- "Meta Knight"
dashturn$Character[dashturn$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
dashturn$Character[dashturn$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
dashturn <- dashturn %>% 
  separate_rows(Character, sep = "/")

# Write data locally
write_csv(dashturn, here::here("Project", "Data", "smash11_dashturnaround.csv"))

# Load in Grab Data
grab <- read.csv(here::here("Project", "Data", "smash12.csv"))

# grab <- read.csv(here::here("Project", "Data", "smash12_grab.csv"))

# Fix Names
grab$Character[grab$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
grab$Character[grab$Character == "DK"] <- "Donkey Kong"
grab$Character[grab$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
grab$Character[grab$Character == "DK"] <- "Donkey Kong"
grab$Character[grab$Character == "P.T. [Charizard]"] <- "Charizard"
grab$Character[grab$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
grab$Character[grab$Character == "P.T. [Squirtle]"] <- "Squirtle"
grab$Character[grab$Character == "PAC-MAN"] <- "Pac-Man"
grab$Character[grab$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
grab$Character[grab$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
grab$Character[grab$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
grab$Character[grab$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
grab$Character[grab$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
grab$Character[grab$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
grab$Character[grab$Character == "Dedede"] <- "King Dedede"
grab$Character[grab$Character == "Metaknight"] <- "Meta Knight"
grab$Character[grab$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
grab$Character[grab$Character   == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
grab <- grab %>% 
  separate_rows(Character, sep = "/")

# Write data locally
write_csv(grab, here::here("Project", "Data", "smash12_grab.csv"))

## Lots of changes for Ledge Stats
# Load in LEdge Stats Data
ledge <- read.csv(here::here("Project", "Data", "smash13.csv"))

# ledge <- read.csv(here::here("Project", "Data", "smash13_ledge.csv"))

# Fix Names
ledge$Character[ledge$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
ledge$Character[ledge$Character == "DK"] <- "Donkey Kong"
ledge$Character[ledge$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
ledge$Character[ledge$Character == "DK"] <- "Donkey Kong"
ledge$Character[ledge$Character == "P.T. [Charizard]"] <- "Charizard"
ledge$Character[ledge$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
ledge$Character[ledge$Character == "P.T. [Squirtle]"] <- "Squirtle"
ledge$Character[ledge$Character == "PAC-MAN"] <- "Pac-Man"
ledge$Character[ledge$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
ledge$Character[ledge$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
ledge$Character[ledge$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
ledge$Character[ledge$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
ledge$Character[ledge$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
ledge$Character[ledge$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
ledge$Character[ledge$Character == "Dedede"] <- "King Dedede"
ledge$Character[ledge$Character == "Metaknight"] <- "Meta Knight"
ledge$Character[ledge$Character =="Mii Swordfighter"] <- "Mii Sword Fighter"
ledge$Character[ledge$Character  == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
ledge <- ledge %>% 
  separate_rows(Character, sep = "/")

# Write data locally
write_csv(ledge, here::here("Project", "Data", "smash13_ledge.csv"))

# Load in Out of Shield Data
outofshield <- read.csv(here::here("Project", "Data", "smash14.csv"))

# outofshield <- read.csv(here::here("Project", "Data", "smash14_outofshield.csv"))

# Fix Names
outofshield$Character[outofshield$Character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
outofshield$Character[outofshield$Character == "DK"] <- "Donkey Kong"
outofshield$Character[outofshield$Character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
outofshield$Character[outofshield$Character == "DK"] <- "Donkey Kong"
outofshield$Character[outofshield$Character == "P.T. [Charizard]"] <- "Charizard"
outofshield$Character[outofshield$Character == "P.T. [Ivysaur]"] <- "Ivysaur"
outofshield$Character[outofshield$Character == "P.T. [Squirtle]"] <- "Squirtle"
outofshield$Character[outofshield$Character == "PAC-MAN"] <- "Pac-Man"
outofshield$Character[outofshield$Character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
outofshield$Character[outofshield$Character == "Pokemon Trainer (Charizard)"] <- "Charizard"
outofshield$Character[outofshield$Character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
outofshield$Character[outofshield$Character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
outofshield$Character[outofshield$Character == "Olimar (r/b/y)"] <- "Olimar (rby)"
outofshield$Character[outofshield$Character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
outofshield$Character[outofshield$Character == "Dedede"] <- "King Dedede"
outofshield$Character[outofshield$Character == "Metaknight"] <- "Meta Knight"
outofshield$Character[outofshield$Character == "Mii Swordfighter"] <- "Mii Sword Fighter"
outofshield$Character[outofshield$Character == "R.O.B."] <- "R.O.B"

# Separate Echo Fighters
outofshield <- outofshield %>% 
  separate_rows(Character, sep = "/")

# Write data locally
write_csv(outofshield, here::here("Project", "Data", "smash14_outofshield.csv"))
```

Now to join all the datasets together. As well as clean some of the
`Character` names since it is not consistent across `frame_data` and
`tier_data`

``` r
acceleration <- read.csv(here::here("Project", "Data", "smash1_acceleration.csv"))
acceleration$Character[acceleration$Character == "Nana"] <- "Ice Climbers"
acceleration$Character[acceleration$Character == "Popo"] <- "Ice Climbers"

airspeed <- read.csv(here::here("Project", "Data", "smash2_airspeed.csv"))

fallspeed <- read.csv(here::here("Project", "Data", "smash3_fallspeed.csv"))

gravity <- read.csv(here::here("Project", "Data", "smash4_gravity.csv"))

jumpheight <- read.csv(here::here("Project", "Data", "smash5_jumpheight.csv"))
jumpheight$Character[jumpheight$Character == "Rosalina & Luma"] <- "Rosalina"

jumpduration <- read.csv(here::here("Project", "Data", "smash6_jumpduration.csv"))

weight <- read.csv(here::here("Project", "Data", "smash7_weight.csv"))

landing <- read.csv(here::here("Project", "Data", "smash8_landing.csv"))

walkspeed <- read.csv(here::here("Project", "Data", "smash9_walkspeed.csv"))
walkspeed$Character[walkspeed$Character == "Nana"] <- "Ice Climbers"
walkspeed$Character[walkspeed$Character == "Popo"] <- "Ice Climbers"


dashrun <- read.csv(here::here("Project", "Data", "smash10_dashrun.csv"))
dashrun$Character[dashrun$Character == "Rosalina & Luma"] <- "Rosalina"
dashrun$Character[dashrun$Character == "Ice Climbers (partner)"] <- "Ice Climbers"
dashrun$Character[dashrun$Character == "Ice Climbers (leader)"] <- "Ice Climbers"

dashturn <- read.csv(here::here("Project", "Data", "smash11_dashturnaround.csv"))
dashturn$Character[dashturn$Character == "Rosalina & Luma"] <- "Rosalina"
dashturn$Character[dashturn$Character == "Pokemon Trainer (All)"] <- "Pokemon Trainer"

grab <- read.csv(here::here("Project", "Data", "smash12_grab.csv"))
grab$Character[grab$Character == "Rosalina & Luma"] <- "Rosalina"

ledge <- read.csv(here::here("Project", "Data", "smash13_ledge.csv"))
outofshield <- read.csv(here::here("Project", "Data", "smash14_outofshield.csv"))
outofshield$Character[outofshield$Character == "Rosalina & Luma"] <- "Rosalina"
```

``` r
frame_data <- acceleration %>% 
  full_join(airspeed, by = "Character") %>% # Terry, Joker, Min Min
  full_join(fallspeed, by = "Character") %>%  # Sephiroth 
  full_join(gravity, by = "Character") %>% # Pyra, Mythra
  full_join(jumpheight, by = "Character") %>%
  full_join(jumpduration, by = "Character") %>% 
  full_join(weight, by = "Character") %>% 
  full_join(landing, by = "Character") %>% 
  full_join(walkspeed, by = "Character") %>% 
  full_join(dashrun, by = "Character") %>% 
  left_join(dashturn, by = "Character") %>% 
  left_join(grab, by = "Character") %>% 
  full_join(ledge, by = "Character") %>% 
  left_join(outofshield, by = "Character")
```

Now with all the frame data joined we can write it locally.

``` r
# Check Names
unique(frame_data$Character[order(frame_data$Character)])
```

    ##  [1] "Banjo & Kazooie"   "Bayonetta"         "Bowser"           
    ##  [4] "Bowser Jr."        "Byleth"            "Captain Falcon"   
    ##  [7] "Charizard"         "Chrom"             "Cloud"            
    ## [10] "Corrin"            "Daisy"             "Dark Pit"         
    ## [13] "Dark Samus"        "Diddy Kong"        "Donkey Kong"      
    ## [16] "Dr. Mario"         "Duck Hunt"         "Falco"            
    ## [19] "Fox"               "Ganondorf"         "Greninja"         
    ## [22] "Hero"              "Ice Climbers"      "Ike"              
    ## [25] "Incineroar"        "Inkling"           "Isabelle"         
    ## [28] "Ivysaur"           "Jigglypuff"        "Joker"            
    ## [31] "Ken"               "King Dedede"       "King K. Rool"     
    ## [34] "Kirby"             "Link"              "Little Mac"       
    ## [37] "Lucario"           "Lucas"             "Lucina"           
    ## [40] "Luigi"             "Mario"             "Marth"            
    ## [43] "Mega Man"          "Meta Knight"       "Mewtwo"           
    ## [46] "Mii Brawler"       "Mii Gunner"        "Mii Sword Fighter"
    ## [49] "Min Min"           "Mr. Game & Watch"  "Mythra"           
    ## [52] "Ness"              "Olimar"            "Pac-Man"          
    ## [55] "Palutena"          "Peach"             "Pichu"            
    ## [58] "Pikachu"           "Piranha Plant"     "Pit"              
    ## [61] "Pyra"              "R.O.B"             "Richter"          
    ## [64] "Ridley"            "Robin"             "Rosalina"         
    ## [67] "Roy"               "Ryu"               "Samus"            
    ## [70] "Sephiroth"         "Sheik"             "Shulk"            
    ## [73] "Simon"             "Snake"             "Sonic"            
    ## [76] "Squirtle"          "Steve"             "Terry"            
    ## [79] "Toon Link"         "Villager"          "Wario"            
    ## [82] "Wii Fit Trainer"   "Wolf"              "Yoshi"            
    ## [85] "Young Link"        "Zelda"             "Zero Suit Samus"

``` r
# Write Data
write_csv(frame_data, here::here("Project", "Data", "frame_data.csv"))
```

### Character Usage Data

I have found some data on the usage of the characters. Here is the
[website](https://www.eventhubs.com/stats/ssbu/) I web-scrapped the data
from This shows a tier list and the player usage. I think even though
this might not be a completely comprehensive list it does show how some
characters are chosen over others. Since there are certain characters
that are used more and I want to know if there is any underlying frame
data that is causing these choices.

I also need to clean the `character` variable names so it matches the
`frame_data`.

``` r
# Webscraping
tables <- read_html("https://www.eventhubs.com/stats/ssbu/") %>%
  html_nodes("#tiers1")

# Practice table webscrape
tier_list <- tables %>%
  .[1] %>%
  html_table(fill = TRUE)

tier_data <- as.data.frame(tier_list) %>% 
  select(-X.) %>% 
  rename(
    character = Character.name,
    score = Score,
    total_players = Total.players,
    per_played = Percentage.played,
    string1 = X1S,
    string2 = X2S,
    string3 = X3S
  ) %>% 
  separate_rows(character, sep = " / ")

# Look at all names in alphabetical order to see what ones are spelled differently
unique(tier_data$character[order(tier_data$character)])
```

    ##  [1] "Banjo-Kazooie"      "Bayonetta"          "Bowser"            
    ##  [4] "Bowser Jr."         "Byleth"             "Captain Falcon"    
    ##  [7] "Chrom"              "Cloud"              "Corrin"            
    ## [10] "Daisy"              "Dark Pit"           "Dark Samus"        
    ## [13] "Diddy Kong"         "Donkey Kong"        "Dr. Mario"         
    ## [16] "Duck Hunt"          "Falco"              "Fox"               
    ## [19] "Ganondorf"          "Greninja"           "Hero"              
    ## [22] "Ice Climbers"       "Ike"                "Incineroar"        
    ## [25] "Inkling"            "Isabelle"           "Jigglypuff"        
    ## [28] "Joker"              "Ken"                "King Dedede"       
    ## [31] "King K. Rool"       "Kirby"              "Link"              
    ## [34] "Little Mac"         "Lucario"            "Lucas"             
    ## [37] "Lucina"             "Luigi"              "Mario"             
    ## [40] "Marth"              "Mega Man"           "Meta Knight"       
    ## [43] "Mewtwo"             "Mii Brawler"        "Mii Gunner"        
    ## [46] "Mii Swordfighter"   "Min Min"            "Mr. Game and Watch"
    ## [49] "Mythra"             "Ness"               "Olimar"            
    ## [52] "Pac-Man"            "Palutena"           "Peach"             
    ## [55] "Pichu"              "Pikachu"            "Piranha Plant"     
    ## [58] "Pit"                "Pokemon Trainer"    "Pyra"              
    ## [61] "R.O.B."             "Richter"            "Ridley"            
    ## [64] "Robin"              "Rosalina"           "Roy"               
    ## [67] "Ryu"                "Samus"              "Sephiroth"         
    ## [70] "Sheik"              "Shulk"              "Simon"             
    ## [73] "Snake"              "Sonic"              "Steve"             
    ## [76] "Terry"              "Toon Link"          "Villager"          
    ## [79] "Wario"              "Wii Fit Trainer"    "Wolf"              
    ## [82] "Yoshi"              "Young Link"         "Zelda"             
    ## [85] "Zero Suit Samus"

``` r
# Fix names
tier_data$character[tier_data$character == "Banjo-Kazooie"] <- "Banjo & Kazooie"
tier_data$character[tier_data$character == "DK"] <- "Donkey Kong"
tier_data$character[tier_data$character == "Mr. Game and Watch"] <- "Mr. Game & Watch"
tier_data$character[tier_data$character == "P.T. [Charizard]"] <- "Charizard"
tier_data$character[tier_data$character == "P.T. [Ivysaur]"] <- "Ivysaur"
tier_data$character[tier_data$character == "P.T. [Squirtle]"] <- "Squirtle"
tier_data$character[tier_data$character == "PAC-MAN"] <- "Pac-Man"
tier_data$character[tier_data$character == "Pokemon Trainer (Ivysaur)"] <- "Ivysaur"
tier_data$character[tier_data$character == "Pokemon Trainer (Charizard)"] <- "Charizard"
tier_data$character[tier_data$character == "Pokemon Trainer (Squirtle)"] <- "Squirtle"
tier_data$character[tier_data$character == "Olimar (r/b/y +2)"] <- "Olimar (rby +2)"
tier_data$character[tier_data$character == "Olimar (r/b/y)"] <- "Olimar (rby)"
tier_data$character[tier_data$character == "Olimar (r/b/y +1)"] <- "Olimar (rby +1)"
tier_data$character[tier_data$character == "Dedede"] <- "King Dedede"
tier_data$character[tier_data$character == "Metaknight"] <- "Meta Knight"
tier_data$character[tier_data$character == "Mii Swordfighter"] <- "Mii Sword Fighter"
tier_data$character[tier_data$character == "R.O.B."] <- "R.O.B"

# Double check names
unique(tier_data$character[order(tier_data$character)])
```

    ##  [1] "Banjo & Kazooie"   "Bayonetta"         "Bowser"           
    ##  [4] "Bowser Jr."        "Byleth"            "Captain Falcon"   
    ##  [7] "Chrom"             "Cloud"             "Corrin"           
    ## [10] "Daisy"             "Dark Pit"          "Dark Samus"       
    ## [13] "Diddy Kong"        "Donkey Kong"       "Dr. Mario"        
    ## [16] "Duck Hunt"         "Falco"             "Fox"              
    ## [19] "Ganondorf"         "Greninja"          "Hero"             
    ## [22] "Ice Climbers"      "Ike"               "Incineroar"       
    ## [25] "Inkling"           "Isabelle"          "Jigglypuff"       
    ## [28] "Joker"             "Ken"               "King Dedede"      
    ## [31] "King K. Rool"      "Kirby"             "Link"             
    ## [34] "Little Mac"        "Lucario"           "Lucas"            
    ## [37] "Lucina"            "Luigi"             "Mario"            
    ## [40] "Marth"             "Mega Man"          "Meta Knight"      
    ## [43] "Mewtwo"            "Mii Brawler"       "Mii Gunner"       
    ## [46] "Mii Sword Fighter" "Min Min"           "Mr. Game & Watch" 
    ## [49] "Mythra"            "Ness"              "Olimar"           
    ## [52] "Pac-Man"           "Palutena"          "Peach"            
    ## [55] "Pichu"             "Pikachu"           "Piranha Plant"    
    ## [58] "Pit"               "Pokemon Trainer"   "Pyra"             
    ## [61] "R.O.B"             "Richter"           "Ridley"           
    ## [64] "Robin"             "Rosalina"          "Roy"              
    ## [67] "Ryu"               "Samus"             "Sephiroth"        
    ## [70] "Sheik"             "Shulk"             "Simon"            
    ## [73] "Snake"             "Sonic"             "Steve"            
    ## [76] "Terry"             "Toon Link"         "Villager"         
    ## [79] "Wario"             "Wii Fit Trainer"   "Wolf"             
    ## [82] "Yoshi"             "Young Link"        "Zelda"            
    ## [85] "Zero Suit Samus"

``` r
write_csv(tier_data, here::here("Project", "Data", "tier_data.csv"))
```

## Merging Data

Now that I have `frame_data` and `tier_data` I can merge them based on
the character name and then the `per_played` will be my outcome variable
with the frame data being the explanatory variables. I am going to use
`full_join` again. I need to fix the issue with the echo fighters being
combined on the `tier_data` but separate in the `frame_data`.

``` r
# Read in Data
frame_data <- read_csv(here::here("Project", "Data", "frame_data.csv"))
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_character(),
    ##   Air.speed = col_double(),
    ##   Regular.Fall = col_double(),
    ##   Fast.Fall = col_double(),
    ##   Gravity = col_double(),
    ##   shorthop_dur = col_double(),
    ##   fullhop_dur = col_double(),
    ##   SH.Fast.Fall = col_double(),
    ##   FH.Fast.Fall = col_double(),
    ##   Weight = col_double(),
    ##   Hard.Land = col_double(),
    ##   Soft.Land..Universal. = col_double(),
    ##   Walk.Speed = col_double(),
    ##   Initial.Dash = col_double(),
    ##   Run.Speed = col_double(),
    ##   X.2 = col_double(),
    ##   X.3 = col_double()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
tier_data <- read_csv(here::here("Project", "Data", "tier_data.csv"))
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   character = col_character(),
    ##   score = col_double(),
    ##   total_players = col_double(),
    ##   per_played = col_double(),
    ##   string1 = col_double(),
    ##   string2 = col_double(),
    ##   string3 = col_double()
    ## )

``` r
# Join Datasets
smash_data <- tier_data %>% 
  right_join(frame_data, by = c("character" = "Character"))

unique(smash_data$character[order(smash_data$character)])
```

    ##  [1] "Banjo & Kazooie"   "Bayonetta"         "Bowser"           
    ##  [4] "Bowser Jr."        "Byleth"            "Captain Falcon"   
    ##  [7] "Charizard"         "Chrom"             "Cloud"            
    ## [10] "Corrin"            "Daisy"             "Dark Pit"         
    ## [13] "Dark Samus"        "Diddy Kong"        "Donkey Kong"      
    ## [16] "Dr. Mario"         "Duck Hunt"         "Falco"            
    ## [19] "Fox"               "Ganondorf"         "Greninja"         
    ## [22] "Hero"              "Ice Climbers"      "Ike"              
    ## [25] "Incineroar"        "Inkling"           "Isabelle"         
    ## [28] "Ivysaur"           "Jigglypuff"        "Joker"            
    ## [31] "Ken"               "King Dedede"       "King K. Rool"     
    ## [34] "Kirby"             "Link"              "Little Mac"       
    ## [37] "Lucario"           "Lucas"             "Lucina"           
    ## [40] "Luigi"             "Mario"             "Marth"            
    ## [43] "Mega Man"          "Meta Knight"       "Mewtwo"           
    ## [46] "Mii Brawler"       "Mii Gunner"        "Mii Sword Fighter"
    ## [49] "Min Min"           "Mr. Game & Watch"  "Mythra"           
    ## [52] "Ness"              "Olimar"            "Pac-Man"          
    ## [55] "Palutena"          "Peach"             "Pichu"            
    ## [58] "Pikachu"           "Piranha Plant"     "Pit"              
    ## [61] "Pyra"              "R.O.B"             "Richter"          
    ## [64] "Ridley"            "Robin"             "Rosalina"         
    ## [67] "Roy"               "Ryu"               "Samus"            
    ## [70] "Sephiroth"         "Sheik"             "Shulk"            
    ## [73] "Simon"             "Snake"             "Sonic"            
    ## [76] "Squirtle"          "Steve"             "Terry"            
    ## [79] "Toon Link"         "Villager"          "Wario"            
    ## [82] "Wii Fit Trainer"   "Wolf"              "Yoshi"            
    ## [85] "Young Link"        "Zelda"             "Zero Suit Samus"

``` r
# Write Data Locally
write_csv(smash_data, here::here("Project", "Data", "smash_data.csv"))
```

Data cleaning is pretty much done. There are a few weird instances of
character name difference that I don’t care about at this moment. Mostly
with the Pokemon Trainer since I have frame data on each pokemon
respectively but tier data on just Pokemon Trainer as a single
character.

Now I can move onto starting some modeling in `02_smash_analysis.Rmd`
where I will look at the variables and start a simple model.

### Failed Data Gathering

Originally I wanted to access match results to use as my outcome
variable. However I was unable to do this and here is some of the code
of my attempts to connect to a database I found on github.

Now that I have the frame data collected. I can get the Match data which
will tell me the wins and losses of each character. My goal is to create
a model that will determine if a character wins or losses. Maybe a win
rate or discrete explanatory variable we will see.

``` r
# library(DBI)
# library(odbc)
# # Create an ephemeral in-memory RSQLite database
# con <- dbConnect(RSQLite::SQLite(), dbname = "ultimate_player_database.db")
# 
# dbListTables(con)
# 
# con <- dbConnect(RSQLite::SQLite(), dbname = "ultimate_player_database.db")
# 
# con <- dbConnect(
#   RPostgreSQL::PostgreSQL(), "ultimate_player_database.db",
# )
# 
# dbListTables(con)
# 
# ## Connect to an existing database
# test <- dbConnect(RSQLite::SQLite(), 'https://github.com/smashdata/ThePlayerDatabase/releases/download/v2021.02.05/ultimate_player_database.db')
# 
# dbListTables(test)
# 
# file.exists("https://github.com/smashdata/ThePlayerDatabase/releases/download/v2021.02.05/ultimate_player_database.db")
# 
# file.info("https://github.com/smashdata/ThePlayerDatabase/releases/download/v2021.02.05/ultimate_player_database.db")
# 
# con <- dbConnect(odbc(), "ultimate_player_database.db")
```

``` r
# library(RSQLite)
# temp <- tempfile()
# # download.file("https://github.com/smashdata/ThePlayerDatabase/releases/download/v2021.02.05/ultimate_player_database.db", temp)
# db <- dbConnect(RSQLite::SQLite(), dbname=temp)
# dbListTables(db)
```

``` r
# # Packages
# library(tidyverse)
# library(httr)
# library(jsonlite)
#
# # # Save API Key to Private Folder
# # saveRDS(smash_key, here::here("Private", "smash_key.rds"))
# 
# # Read in API Key
# api_key <- readRDS(here::here("Private", "smash_key.rds"))
# 
# # Url to API
# url <- "https://api.smash.gg/gql/alpha"
# 
# resp <- httr::GET(url,
#           add_headers("Bearer" = api_key))
# 
# httr::GET(url,
#           add_headers("Bearer" = api_key))
# 
# # Check for any errors
# http_error(resp) 
```
