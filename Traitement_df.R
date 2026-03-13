library(dplyr)
library(rvest)
library(stringr)
library(purrr)
#CODE DE CREATION DU DATA FRAME
##### 

#readLines('./donnees/player_valuations.csv')
player_value = read.csv('app/data/player_latest_market_value.csv',sep=',')

#readLines('./donnees/player_profiles.csv')
player_name = read.csv('app/data/player_profiles.csv',sep=',')

#merge entre les infos du joueur et les valeurs marchandes, on merge par l'id du joueur (player_id).
player_value = merge(player_name,player_value, by = "player_id")

player_value$name <- gsub(" \\(.*\\)", "", player_value$player_name)

#readLines('./donnees/players_data-2024_2025.csv')
player_stat = read.csv('app/data/players_data-2024_2025.csv',sep=',')


#rennomage de la colonne name pour merge ensuite les deux datasets
player_stat$name = player_stat$Player

df_players <- merge(player_stat, player_value, by = "name")


#Dedoublonnage pour ne garder que la valeur marchande la plus récente pour chaque joueur.

df_players$date <- as.Date(df_players$date_unix, format = "%Y-%m-%d")

df_sorted <- df_players[order(df_players$name, df_players$date, decreasing = TRUE), ]

df_clean <- df_sorted[!duplicated(df_sorted$name), ]
df_players <- df_clean

df_players$Player = NULL 


# ON va se concentrer seulement sur les joueurs evoluant en LIGUE 1 
df_players <- df_players[df_players$Comp == 'fr Ligue 1',]

# on ne prends que les valeurs apès le 17/05/2025, qui représente la fin du championnat, les données vont jusqu'en septembre 2025 donc on a pas besoin de les limiter dans le temps 
df_players <- df_players[df_players$date >= as.Date("2025-05-17"),]

# Nous avons remarquer que des joueurs peuvent avoir plusieurs postes (variable Poste). Nous considérons que leurs poste principal est le premier affiché dans la variable "Poste"

df_players$Pos <- as.factor(df_players$Pos)

levels(df_players$Pos)
levels(df_players$Pos)[c(2,3)] = "DF"
levels(df_players$Pos)
levels(df_players$Pos)[c(3,4)] = "FW"
levels(df_players$Pos)
levels(df_players$Pos)[c(5,6)] = "MF"
levels(df_players$Pos)

# on remplace les NA par des 0 dans la variable Save.
df_players$Save.[is.na(df_players$Save.)] <- 0

# Nous avons 301 variables qu'on va garder
# On ecrit le df_players dans un csv 'dataset_stat_large'

write.csv(df_players, "app/data/dataset_stat_large.csv", row.names = FALSE)




