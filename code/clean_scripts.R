

# Cleaning Scripts --------------------------------------------------------

library(readr)


filenames <- list.files(pattern = "season*")

for (i in 1:1) {
  # Head of for-loop
  assign(paste0("season", i),                                   # Read and store data frames
         read_csv(
           paste0(
             "G:/My Drive/Hobbies/popculture_datavis/gilmoregirls/gilmore_girls/season",
             i,
             ".csv"
           )
         ))
}
d.season1 <- season1[!is.na(season1$script),]

d.season1 <- d.season1 %>%
  mutate(row_id = seq(1, nrow(d.season1))) %>%
  mutate(episode = ifelse(substr(script, 1, 2) == "1.", substr(script, 3, 4), episode),
         script = ifelse(substr(script, 1, 2) == "1.", NA, script))

d.season1 <- d.season1[, c(3, 1:2)]

d.season1 <- d.season1 %>%
  fill(episode, .direction = "down") %>%
  mutate(episode = as.numeric(episode))

#cleaned so character name mistakes are corrected, location details and whether its dialogue, stage direction or credits, etc.

#credits were manually enetered for missing episodes

d.season1 <- d.season1 %>%
  mutate(character = ifelse(grepl(":", script), toupper(sub(":.*", "", script)), NA)) %>%
  mutate(
    character = ifelse(
      grepl("[(]", character),
      str_trim(toupper(sub(
        "[(].*", "", character
      ))),
      str_trim(character)
    ),
    character = ifelse(
      character %in% c(
        "LOLERAI",
        "LOLERALI",
        "LOREAI",
        "LOREALI",
        "LORELA",
        "LORELAI",
        "LORELAIL",
        "LORLEAI",
        "LORLELAI"
      ),
      "LORELAI",
      character
    ),
    character = ifelse(character %in% c("LORY", "ORY"),  "RORY", character),
    character = ifelse(character %in% c("BABETE"),  "BABETTE", character),
    character = ifelse(character %in% c("CASHIERORY"),  "CASHIER", character),
    character = ifelse(character %in% c("CHRISTOHPER"),  "CHRISTOPHER", character),
    character = ifelse(character %in% c("EMIL"),  "EMILY", character),
    character = ifelse(character %in% c("LUK"),  "LUKE", character),
    character = ifelse(character %in% c("RICHRAD"),  "RICHARD", character),
    character = ifelse(character %in% c("RACHELORELAI"),  "RACHEL", character),
    character = ifelse(character %in% c("IRATE MOTHERORY"),  "IRATE MOTHER", character),
    character = ifelse(character %in% c("SOOKEI", "SOOKI"),  "SOOKIE", character),
    character = ifelse(character %in% c("MAX", "MR. MEDINA"),  "MAX MEDINA", character),
    character = ifelse(character %in% c("TRISTIN"),  "TRISTAN", character),
    character = ifelse(
      character %in% c("", "[LORELAI RORY",  "1.08 - LOVE AND W*RORY"),
      NA,
      character
    ),
    character = ifelse(
      character == "WRITTEN BY", NA , character
    ),
    script = ifelse(
      script %in% c("F.D. » Transcripts » G » Gilmore Girls",
                    "© 2000-2022 Forever Dreaming. All rights reserved.",
                    "Gumballs 25 cents"), NA, script
      ),
    script = ifelse(
      is.na(character) & grepl("written by", script, ignore.case = "TRUE") |
      is.na(character) & grepl("story by", script, ignore.case = "TRUE") |
      is.na(character) & grepl("transcript by", script, ignore.case = "TRUE") |
      is.na(character) & grepl("teleplay by", script, ignore.case = "TRUE") |
      is.na(character) & grepl("THE END", script, ignore.case = "TRUE") |
      is.na(character) & grepl("directed by", script, ignore.case = "TRUE"),
      NA, script
    ),
    location_det = ifelse(grepl("open in", script, ignore.case = "TRUE") |
                          grepl("open at", script, ignore.case = "TRUE") , 
                          script, NA),
    location_det = ifelse(
      grepl("cut to", script, ignore.case = "TRUE") &
        !grepl("LORELAI:", script, ignore.case = "TRUE"),
      script,
      location_det
    ),
    location_det = ifelse(
      grepl("pan to", script, ignore.case = "TRUE") &
        is.na(character),
      script,
      location_det
    ),
    type = ifelse(!is.na(location_det),
                  'location', NA
                  ),
    type = ifelse(grepl(":", script) &
                    is.na(location_det), 'dialogue', type),
    type = ifelse(
      grepl("opening credits", script, ignore.case = "TRUE"),
      'credits',
      type
    ),
    type = ifelse(
      grepl("opening sequence", script, ignore.case = "TRUE"),
      'credits',
      type
    ),
    type = ifelse(
      is.na(location_det), 'stage direction',type)
    )



d.season1 <- d.season1 %>% drop_na(script)

#do this once


locations <- d.season1 %>%
  subset(type == 'location')

d.season1 <- d.season1 %>%
  mutate(
    location_det = gsub('cut to ', '',location_det, ignore.case = "TRUE"),
    location_det = str_trim(toupper(location_det)),
    location_det = ifelse(
        grepl("stars hollow", location_det, ignore.case = "TRUE") |
        grepl("pan to rory and dean", location_det, ignore.case = "TRUE") |
        grepl("town center", location_det, ignore.case = "TRUE") |
        grepl("street", location_det, ignore.case = "TRUE") |
        grepl("sidewalk", location_det, ignore.case = "TRUE") |
        grepl("front of a school", location_det, ignore.case = "TRUE") |
        grepl("gazebo", location_det, ignore.case = "TRUE") |
        grepl("Streelight ", location_det, ignore.case = "TRUE") |
        grepl("Center of Town", location_det, ignore.case = "TRUE") |
        grepl("festival", location_det, ignore.case = "TRUE") |
        grepl("Alley", location_det, ignore.case = "TRUE") |
        grepl("Bus Stop", location_det, ignore.case = "TRUE") |
        grepl("Rummage Sale", location_det, ignore.case = "TRUE") |
        grepl("on her way", location_det, ignore.case = "TRUE") |
        grepl("softball", location_det, ignore.case = "TRUE") |
        grepl("lane and todd", location_det, ignore.case = "TRUE") |
        grepl("walking", location_det, ignore.case = "TRUE") |
        grepl("outside", location_det, ignore.case = "TRUE"),
      'Stars Hollow - Exterior',
      location_det
    ),
    location_det = ifelse(
      grepl("lorelai\'s", location_det, ignore.case = "TRUE") |
      grepl("lorelais", location_det, ignore.case = "TRUE") |
      grepl("later the save", location_det, ignore.case = "TRUE") |
      grepl("front door", location_det, ignore.case = "TRUE") |
      grepl("morning", location_det, ignore.case = "TRUE") |
      grepl("lorelai", location_det, ignore.case = "TRUE") |
      grepl("rory and lane getting ready", location_det, ignore.case = "TRUE") |
      grepl("kitchen", location_det, ignore.case = "TRUE") |
      grepl("living room", location_det, ignore.case = "TRUE") |
      grepl("gilmore house", location_det, ignore.case = "TRUE") |
      grepl("Rory\'s room", location_det, ignore.case = "TRUE") |
      grepl("Porch", location_det, ignore.case = "TRUE") |
      grepl("Gilmore Living Room", location_det, ignore.case = "TRUE") |
      grepl("LORELAI GETTING HOME", location_det, ignore.case = "TRUE") |
      grepl("LORELAI MAKING A DRESS E", location_det, ignore.case = "TRUE") |
      grepl("LORELAI SITTING AT THE KITCHEN", location_det, ignore.case = "TRUE") |
      grepl("front hallway", location_det, ignore.case = "TRUE") |
      grepl("LORELAI COMING DOWN THE STAIRS", location_det, ignore.case = "TRUE") |
      grepl("Rory\'s bedroom", location_det, ignore.case = "TRUE"),
      'Lorelai\'s House',
      location_det
    ),
    location_det = ifelse(
      grepl("luke\'s", location_det, ignore.case = "TRUE") |
      grepl("inside", location_det, ignore.case = "TRUE") |
      grepl("storage room", location_det, ignore.case = "TRUE") |
      grepl("diner", location_det, ignore.case = "TRUE"),
      'Luke\'s Diner' ,
      location_det
    ),
    location_det = ifelse(
      grepl("babette\'s", location_det, ignore.case = "TRUE") |
        grepl("wake", location_det, ignore.case = "TRUE") |
        grepl("babette goes over to sit", location_det, ignore.case = "TRUE"),
      'Babette\'s House' ,
      location_det
    ),
    location_det = ifelse(
      grepl("EMILY AND RICHARD\'S", location_det, ignore.case = "TRUE") |
        grepl("grandma\'s house", location_det, ignore.case = "TRUE") |
        grepl("study", location_det, ignore.case = "TRUE") |
        grepl("upstairs", location_det, ignore.case = "TRUE") |
        grepl("basement", location_det, ignore.case = "TRUE") |
        grepl("later that", location_det, ignore.case = "TRUE") |
        grepl("emily's kitchen", location_det, ignore.case = "TRUE") |
        grepl("the party", location_det, ignore.case = "TRUE") |
        grepl("Emily Directing ", location_det, ignore.case = "TRUE") |
        grepl("Emily Saying Goodnight", location_det, ignore.case = "TRUE") |
        grepl("Richard Looking Up", location_det, ignore.case = "TRUE") |
        grepl("Richard Standing With Some", location_det, ignore.case = "TRUE") |
        grepl("richard is reading", location_det, ignore.case = "TRUE") |
        grepl("dining room", location_det, ignore.case = "TRUE") |
        grepl("front hall", location_det, ignore.case = "TRUE") |
        grepl("ELDER GILMORE RESIDENCE", location_det, ignore.case = "TRUE"),
      'Emily and Richard\'s House',
      location_det
    ),
    location_det = ifelse(
      grepl("Steam Room", location_det, ignore.case = "TRUE") |
        grepl("golf", location_det, ignore.case = "TRUE") |
        grepl("club", location_det, ignore.case = "TRUE"),
      'Country Club' ,
      location_det
    ),
    location_det = ifelse(
      grepl("lorelai\'s jeep", location_det, ignore.case = "TRUE") |
      grepl("driving", location_det, ignore.case = "TRUE") |
      grepl("inside truck", location_det, ignore.case = "TRUE") |
      grepl("car", location_det, ignore.case = "TRUE"),
      'CAR',
      location_det
    ),

    location_det = ifelse(
      grepl("Department Store", location_det, ignore.case = "TRUE"),
      'Mall',
      location_det
    ),
    location_det = ifelse(
      grepl("stars hollow high", location_det, ignore.case = "TRUE"),
      'Stars Hollow High',
      location_det
    ),
    location_det = ifelse(
      grepl("bookstore", location_det, ignore.case = "TRUE"),
      'Bookstore',
      location_det
    ),
    location_det = ifelse(
      grepl("hartford", location_det, ignore.case = "TRUE")|
      grepl("tea", location_det, ignore.case = "TRUE"),
      'hartford',
      location_det
    ),
    location_det = ifelse(
      grepl("madeline's", location_det, ignore.case = "TRUE"),
      'Madeline\'s House',
      location_det
    ),
    location_det = ifelse(
      grepl("chilton", location_det, ignore.case = "TRUE")|
      grepl("college", location_det, ignore.case = "TRUE")|
      grepl("hallway", location_det, ignore.case = "TRUE")|
      grepl("Mr. Medina's Class", location_det, ignore.case = "TRUE")|
      grepl("classrooms", location_det, ignore.case = "TRUE")|
      grepl("headmaster", location_det, ignore.case = "TRUE")|
      grepl("Cafeteria", location_det, ignore.case = "TRUE")|
      grepl("The Dance", location_det, ignore.case = "TRUE")|
      grepl("outside chilton", location_det, ignore.case = "TRUE"),
      'Chilton',
      location_det
    ),
    location_det = ifelse(
      grepl("independence inn", location_det, ignore.case = "TRUE") |
      grepl("michel and guy", location_det, ignore.case = "TRUE") |
      grepl("inn", location_det, ignore.case = "TRUE"),
      'The Independence Inn',
      location_det
    ),
    location_det = ifelse(
      grepl("NY", location_det, ignore.case = "TRUE") |
      grepl("concert", location_det, ignore.case = "TRUE") |
      grepl("Rory and Paris Waiting", location_det, ignore.case = "TRUE") |
      grepl("apartment building", location_det, ignore.case = "TRUE"),
      'New York',
      location_det
    ),
    location_det = ifelse(
      grepl("SOOKIE\'S", location_det, ignore.case = "TRUE"),
      'Sookie\'s House',
      location_det
    ),
    location_det = ifelse(
      grepl("Pan To Rory Coming Out Of Room", location_det, ignore.case = "TRUE") |
      grepl("hospital", location_det, ignore.case = "TRUE"),
      'Hospital',
      location_det
    ),
    location_det = ifelse(
      grepl("Market", location_det, ignore.case = "TRUE"),
      'Doose\'s Market',
      location_det
    ),
    location_det = ifelse(
      grepl("Kim household", location_det, ignore.case = "TRUE")|
      grepl("Mrs. Kim", location_det, ignore.case = "TRUE") |
      grepl("Lane's Room", location_det, ignore.case = "TRUE") |
      grepl("Antique", location_det, ignore.case = "TRUE"),
      'Kim Household & Kim\'s Antiques',
      location_det
    ),
    location_det = ifelse(
      grepl("miss patty\'s", location_det, ignore.case = "TRUE"),
      'Miss Patty\'s',
      location_det
    ),
    location_det = ifelse(
      grepl("Max's", location_det, ignore.case = "TRUE") |
      grepl("Max Medina's Apartment", location_det, ignore.case = "TRUE") | 
      grepl("Max's Apartment", location_det, ignore.case = "TRUE"),
      'MAX\'S APARTMENT',
      location_det
    )
    )
 
d.season1$location_det <- str_to_title(d.season1$location_det)

sort(unique(d.season1$location_det))

d.season1 <- d.season1 %>%
  fill(location_det, .direction = "down")
