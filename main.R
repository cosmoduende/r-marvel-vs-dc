
# REQUIRED LIBRARIES

library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(gridExtra)
library(wesanderson)
library(pander)
library(plotly)

# READ DATASETS

infoCharacters <- read.csv("dataset_shdb/heroesInformation.csv", na.strings = c("-", "-99"))
infoPowers <- read.csv("dataset_shdb/superHeroPowers.csv")
infoStats <- read.csv("dataset_shdb/charactersStats.csv", na.strings = "")

# SUBSET FOR MARVEL AND DC

colnames(infoCharacters)[colnames(infoCharacters) == "name"] <- "Name"
marvelDcInfo <- infoCharacters[(infoCharacters$Publisher == "Marvel Comics" | infoCharacters$Publisher == "DC Comics"), ]

# REMOVE NAME DUPLICATES AND SELECT COLUMNS 

marvelDcInfo <- marvelDcInfo[!duplicated(marvelDcInfo$Name), ]
marvelDcInfo <- marvelDcInfo %>%
  select(Name, Gender, Race, Publisher) 

# JOIN DATASETS

marvelDcStatsInfo <- join(marvelDcInfo, infoStats, by = "Name", type = "inner")

colnames(infoPowers)[colnames(infoPowers) == "hero_names"] <- "Name"
fullMarvelDc <- join(marvelDcStatsInfo, infoPowers, by = "Name", type = "inner")

# TRANSFORM INTO A SINGLE COLUMN SUPER POWER

marvelDc <- melt(fullMarvelDc, id = c("Name", "Gender", "Race", "Publisher", "Alignment", "Intelligence", 
                                         "Strength", "Speed", "Durability", "Power", "Combat", "Total"))

colnames(marvelDc)[colnames(marvelDc) == "variable"] <- "SuperPower"

marvelDc <- marvelDc %>%
  filter(value == "True") %>%
  select(-value) 

# CONVERT CATEGORICAL COLUMNS TO FACTORS

marvelDc$Name <- as.factor(marvelDc$Name)
marvelDc$Gender <- as.factor(marvelDc$Gender)
marvelDc$Race <- as.factor(marvelDc$Race)
marvelDc$Publisher <- as.factor(marvelDc$Publisher)
marvelDc$Alignment <- as.factor(marvelDc$Alignment)
marvelDc$SuperPower <- as.factor(marvelDc$SuperPower)

# CUSTOM PALETTES

dcMarvelPalette <- c("#0476F2", "#EC1E24")
goodBadPalette <- c("#E58700", "#0EBF7D", "#C99902")

# DATA OBSERVATIONS IN EACH PUBLISHER

ggplot(marvelDc, aes(x = Publisher, fill = Publisher)) + 
  geom_bar(stat = "count", aes(fill = Publisher)) +
  labs(x = "Publisher", y = "No. of Characters", title = "DC and Marvel Characters", subtitle = "No. of Characters each publisher") +
  geom_label(stat = "count", aes(label = ..count..)) +
  guides(fill = FALSE) +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

# MALES AND FEMALES IN DC AND MARVEL

marvelDcGender <- marvelDc %>%
  filter(!is.na(Gender)) %>%
  group_by(Gender) %>%
  dplyr::count(Publisher) %>%
  select(Gender, Publisher, Count = n)

ggplot(marvelDcGender, aes(x = Gender, y = Count)) +
  geom_bar(stat = "identity", aes(fill = Publisher)) +
  labs(x = "Gender", y = "No. of Characters", title = "DC and Marvel Characters", subtitle = "Gender comparison") +
  geom_label(stat = "identity", aes(label = Count)) +
  facet_wrap(~Publisher) +
  guides(fill = FALSE) +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

# RACE DIFFERENCE BETWEEN MARVEL AND DC

DCRace <- marvelDc %>%
  filter(!is.na(Race)) %>%
  filter(Publisher == "DC Comics") %>%
  group_by(Race) %>%
  dplyr::count(Race) %>%
  select(Race, Count = n) %>%
  arrange(-Count)

DCRace <- ggplot(DCRace[1:15, ], aes(x = reorder(Race, Count), y = Count)) + 
  geom_bar(stat = "identity", aes(fill = Race)) +
  labs(x = "Race", y = "No. of Characters", title = "DC Character Races", subtitle= "Top 15 races") +
  geom_label(stat = "identity", aes(label = Count)) +
  coord_flip() +
  guides(fill = FALSE, alpha = FALSE) +
  theme_bw()

marvelRace <- marvelDc %>%
  filter(!is.na(Race)) %>%
  filter(Publisher == "Marvel Comics") %>%
  group_by(Race) %>%
  dplyr::count(Race) %>%
  select(Race, Count = n) %>%
  arrange(-Count)

marvelRace <- ggplot(marvelRace[1:15, ], aes(x = reorder(Race, Count), y = Count)) + 
  geom_bar(stat = "identity", aes(fill = Race)) +
  geom_label(stat = "identity", aes(label = Count)) +
  labs(x = "Race", y = "No. of Characters", title = "Marvel Character Races", subtitle= "Top 15 races") +
  coord_flip() +
  guides(fill = FALSE, alpha = FALSE) +
  theme_bw()

grid.arrange(DCRace, marvelRace, ncol = 2)

# HEROES AND VILLIANS IN MARVEL AND DC

marvelDcAlignment <- marvelDc %>%
  filter(!is.na(Alignment)) %>%
  group_by(Alignment) %>%
  dplyr::count(Publisher) %>%
  select(Alignment, Publisher, Count = n)

ggplot(marvelDcAlignment, aes(x = Alignment, y = Count)) +
  geom_bar(stat = "identity", aes(fill = Publisher)) +
  labs(x = "Alignment", y = "No. of Characters", title = "DC and Marvel Alignment", subtitle= "Heroes, Villains and Neutral comparison") +
  guides(fill = FALSE) +
  geom_label(stat = "identity", aes(label = Count)) +
  facet_wrap(~Publisher) +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

# BOX PLOT COMPARISON SKILLS MARVEL AND DC

### INTELLIGENCE
boxIntel <- ggplot(marvelDc, aes(x = Publisher, y = Intelligence, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "", title = "DC and Marvel Characters", subtitle = "Comparison of Intelligence") +
  guides(fill = FALSE) +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

### STRENGTH
boxStrength <- ggplot(marvelDc, aes(x = Publisher, y = Strength, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "", title = "DC and Marvel Characters", subtitle = "Comparison of Strength") +
  guides(fill = FALSE) +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

### SPEED
boxSpeed <- ggplot(marvelDc, aes(x = Publisher, y = Speed, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "", title = "DC and Marvel Characters", subtitle = "Comparison of Speed") +
  guides(fill = FALSE) +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

### DURABILITY
boxDurability <- ggplot(marvelDc, aes(x = Publisher, y = Durability, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "", title = "DC and Marvel Characters", subtitle = "Comparison of Durability") +
  guides(fill = FALSE) +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

### POWER
boxPower <- ggplot(marvelDc, aes(x = Publisher, y = Power, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "", title = "DC and Marvel Characters", subtitle = "Comparison of Power") +
  guides(fill = FALSE) +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

### COMBAT
boxCombat <- ggplot(marvelDc, aes(x = Publisher, y = Combat, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "", title = "DC and Marvel Characters", subtitle = "Comparison of Combat") +
  guides(fill = FALSE) +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

grid.arrange(boxIntel, boxStrength, boxSpeed, boxDurability, boxPower, boxCombat,  ncol = 2)

# TOP CHARACTERS HIGHEST INTELLIGENCE MARVEL AND DC

marvelDcIntel <- marvelDc %>%
  group_by(Name, Publisher) %>%
  distinct(Intelligence) %>%
  select(Name, Intelligence) %>%
  arrange(-Intelligence)

ggplot(marvelDcIntel[1:30, ], aes(x = reorder(Name, Intelligence), y = Intelligence)) + 
  geom_bar(stat = "identity", aes(fill = Publisher)) +
  ylim(0, 120) +
  labs(x = "Character", y = "Intelligence", title = "Top 30 Marvel and DC Characters", subtitle = "With Highest Intelligence") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

# TOP CHARACTERS HIGHEST STRENGTH MARVEL AND DC

marvelDcStrength <- marvelDc %>%
  group_by(Name, Publisher) %>%
  distinct(Strength) %>%
  select(Name, Strength) %>%
  arrange(-Strength)

ggplot(marvelDcStrength[1:30, ], aes(x = reorder(Name, Strength), y = Strength)) + 
  geom_bar(stat = "identity", aes(fill = Publisher)) +
  labs(x = "Character", y = "Strength", title = "Top 30 Marvel and DC Characters", subtitle = "With Highest Strength") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

# TOP CHARACTERS HIGHEST SPEED MARVEL AND DC

marvelDcSpeed <- marvelDc %>%
  group_by(Name, Publisher) %>%
  distinct(Speed) %>%
  select(Name, Speed) %>%
  arrange(-Speed)

ggplot(marvelDcSpeed[1:30, ], aes(x = reorder(Name, Speed), y = Speed)) + 
  geom_bar(stat = "identity", aes(fill = Publisher)) +
  labs(x = "Character", y = "Speed", title = "Top 30 Marvel and DC Characters", subtitle = "With Highest Speed") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

# TOP CHARACTERS HIGHEST DURABILITY MARVEL AND DC

marvelDcDur <- marvelDc %>%
  group_by(Name, Publisher) %>%
  distinct(Durability) %>%
  select(Name, Durability) %>%
  arrange(-Durability)

ggplot(marvelDcDur[1:30, ], aes(x = reorder(Name, Durability), y = Durability)) + 
  geom_bar(stat = "identity", aes(fill = Publisher)) +
  ylim(0, 120) +
  labs(x = "Character", y = "Durability", title = "Top 30 Marvel and DC Characters", subtitle = "With Highest Durability") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

# TOP CHARACTERS HIGHEST POWER MARVEL AND DC

marvelDcPow <- marvelDc %>%
  group_by(Name, Publisher) %>%
  distinct(Power) %>%
  select(Name, Power) %>%
  arrange(-Power)

ggplot(marvelDcPow[1:30, ], aes(x = reorder(Name, Power), y = Power)) + 
  geom_bar(stat = "identity", aes(fill = Publisher)) +
  labs(x = "Character", y = "Power", title = "Top 30 Marvel and DC Characters", subtitle = "With Highest Power") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

# TOP CHARACTERS HIGHEST COMBAT SKILLS MARVEL AND DC

marvelDcCombat <- marvelDc %>%
  group_by(Name, Publisher) %>%
  distinct(Combat) %>%
  select(Name, Combat) %>%
  arrange(-Combat)

ggplot(marvelDcCombat[1:30, ], aes(x = reorder(Name, Combat), y = Combat)) + 
  geom_bar(stat = "identity", aes(fill = Publisher)) +
  labs(x = "Character", y = "Combat", title = "Top 30 Marvel and DC Characters", subtitle = "With Highest Combat Skills") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)

# BOXPLOT OVERALL COMPARISION MARVEL AND DC

ggplot(marvelDc, aes(x = Publisher, y = Total, fill = Publisher)) + 
  geom_boxplot() +
  labs(x = "Publisher", title = "DC and Marvel Characters", subtitle =  "Most Powerful Characters Comparison (Sum of abilities)") +
  theme_bw() + 
  scale_fill_manual(values=dcMarvelPalette)

# TOP POWERFUL CHARACTERS MARVEL AND DC

dcTotal <- marvelDc %>%
  filter(Publisher == "DC Comics") %>%
  group_by(Name, Alignment) %>%
  distinct(Total) %>%
  select(Name, Total) %>%
  arrange(-Total)

dcTotal <- ggplot(dcTotal[1:20, ], aes(x = reorder(Name, Total), y = Total)) + 
  geom_bar(stat = "identity", aes(fill = Alignment)) +
  labs(x = "Character", y = "Total", title = "Top 20 DC Characters", subtitle = "Most Powerful Heroes or Villains") +
  coord_flip() +
  theme_bw() + 
  scale_fill_manual(values=goodBadPalette, labels = c("villain", "hero", "neutral"))

marvelTotal <- marvelDc %>%
  filter(Publisher == "Marvel Comics") %>%
  group_by(Name, Alignment) %>%
  distinct(Total) %>%
  select(Name, Total) %>%
  arrange(-Total)

marvelTotal <- ggplot(marvelTotal[1:20, ], aes(x = reorder(Name, Total), y = Total)) + 
  geom_bar(stat = "identity", aes(fill = Alignment)) +
  labs(x = "Character", y = "Total", title = "Top 20 Marvel Characters", subtitle = "Most Powerful Heroes or Villains") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=goodBadPalette, labels = c("villain", "hero", "neutral"))

grid.arrange(dcTotal, marvelTotal, ncol = 2)

# TOP SUPERPOWERS MARVEL AND DC

dcSuperP <- marvelDc %>%
  filter(Publisher == "DC Comics") %>%
  group_by(SuperPower) %>%
  dplyr::count(SuperPower) %>%
  select(SuperPower, Count = n) %>%
  arrange(-Count)

dcSuperP <- ggplot(dcSuperP[1:20, ], aes(x = reorder(SuperPower, Count), y = Count)) + 
  geom_bar(stat = "identity", aes(fill = SuperPower)) +
  geom_label(stat = "identity", aes(label = Count)) +
  labs(x = "Superpower", y = "No. of Characters", title = "DC Comics", subtitle = "Top 20 Superpowers") +
  guides(fill = FALSE) +
  coord_flip() +
  theme_bw()

marvelSuperP <- marvelDc %>%
  filter(Publisher == "Marvel Comics") %>%
  group_by(SuperPower) %>%
  dplyr::count(SuperPower) %>%
  select(SuperPower, Count = n) %>%
  arrange(-Count)

marvelSuperP <- ggplot(marvelSuperP[1:20, ], aes(x = reorder(SuperPower, Count), y = Count)) + 
  geom_bar(stat = "identity", aes(fill = SuperPower)) +
  geom_label(stat = "identity", aes(label = Count)) +
  labs(x = "Superpower", y = "No. of Characters", title = "Marvel Comics", subtitle = "Top 20 Superpowers") +
  guides(fill = FALSE) +
  coord_flip() +
  theme_bw()

grid.arrange(dcSuperP, marvelSuperP, ncol = 2)


# TOP CHARACTERS HIGHEST No. OF SUPERPOWERS  

marvelDcSuperP <- marvelDc %>%
  group_by(Name, Publisher) %>%
  dplyr::count(Name) %>%
  select(Name, Count = n) %>%
  arrange(-Count)

ggplot(marvelDcSuperP[1:25, ], aes(x = reorder(Name, Count), y = Count)) + 
  geom_bar(stat = "identity", aes(fill = Publisher)) +
  labs(x = "Character", y = "No. of superpowers", title = "Top 25 Marvel and DC Characters", subtitle = "With Highest No. of Superpowers") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=dcMarvelPalette)
