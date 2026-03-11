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


player_value = merge(player_name,player_value, by = "player_id")

player_value$name <- gsub(" \\(.*\\)", "", player_value$player_name)
# et merce chat

#readLines('./donnees/players_data-2024_2025.csv')
player_stat = read.csv('app/data/players_data-2024_2025.csv',sep=',')


#rennomage de la colonne name pour merge ensuite les deux datasets
player_stat$name = player_stat$Player

df_players <- merge(player_stat, player_value, by = "name")


#Dedoublonnage pour ne garder que la valeur marchande la plus récente

df_players$date <- as.Date(df_players$date_unix, format = "%Y-%m-%d")

df_sorted <- df_players[order(df_players$name, df_players$date, decreasing = TRUE), ]

df_clean <- df_sorted[!duplicated(df_sorted$name), ]
df_players <- df_clean

df_players$Player = NULL 


# ON va se concentrer seulement sur les joueurs evoluant en LIGUE 1 
df_players <- df_players[df_players$Comp == 'fr Ligue 1',]

# Parmis les 292 variables nous avons décidé de garder seulement les variables qui nous semblent pertinentes. 
#NOus allons garder : Le nom du joueur (name)
#l'age du joueur (Age)
# la nationalité du joueur (Nation)
# le poste du joueur (Pos)
#le nom de son équipe (squad)
# le nombre de match joué (MP)
#Nombre de minutes joués (Min)
# Nombre de passe décisives et de buts (GA)
# NOmbre de But  et de passé décisives attendus hors pénalty (npxG)
# passe clés (KP)
# Tacles et interceptions (Tkl+Int) 
# Pourcentage d'arrêts effectués (Save%)
# et evidemment la variable à expliquer la valeur marchande value

df <- df_players[, c("date","name", "Age", "Nation", "Pos", "Squad", "MP", 
                     "Min", "G.A", "xG.xAG", "KP", "Tkl.Int", "Save.", 
                     "value")]

# on ne prends que les valeurs apès le 17/05/2025, la fin du championnat, les données vont jusqu'en septembre 2025 donc on a pas besoin de les limiter dans le temps 
df <- df[df$date >= as.Date("2025-05-17"),]

#ON transforme en facteurs les variables qualitatives ('Nation','Pos','Squad')

df$Nation <- as.factor(df$Nation)
df$Pos    <- as.factor(df$Pos)
df$Squad  <- as.factor(df$Squad)

# Nous avons remarquer que des joueurs peuvent avoir plusieurs postes (variable Poste). Nous considérons que leurs poste principal est le premier affiché dans la variable "Poste"

levels(df$Pos)
levels(df$Pos)[c(2,3)] = "DF"
levels(df$Pos)
levels(df$Pos)[c(3,4)] = "FW"
levels(df$Pos)
levels(df$Pos)[c(5,6)] = "MF"
levels(df$Pos)

# on remplace les NA par des 0 dans la variable Save.
df$Save.[is.na(df$Save.)] <- 0

#CREATION DU CSV 'df.csv' qu'on utilisera dans le rendu, ca evitera de devoir executer tout le code à chaque fois pour obtenir le df.

#write.csv(df, "df.csv", row.names = FALSE)

# Scraping AJAX sur Foot Mercato
url <- "https://www.footmercato.net/player_overviews/tournaments/2825525736518882482/general?itemsPerPage=900&enablePlayerRating=1"

# On essaie de récupérer le tableau (avec un tryCatch pour éviter que l'app plante si le site est inaccessible)
  page <- read_html(url)
  
  df_fm <- page %>%
    html_element("#generalTable") %>%
    html_table()
  
  # Nettoyage basique : on s'assure d'avoir des noms de colonnes propres 
  # (Il faudra adapter "Joueur" selon le nom réel de la colonne sur le site)
  # Exemple de fusion avec ton dataframe principal (left_join)
  # On suppose que la colonne s'appelle "Joueur" sur FootMercato et "name" dans ton df
  
  df <- df %>%
    left_join(df_fm, by = c("name" = "Joueur"))
  
  # Nettoyage et fusion (On suppose que la colonne s'appelle "Joueur" sur FootMercato)
  df_fm <- unique(df_fm)
  if("Joueur" %in% names(df_fm)) {
    df <- df %>% left_join(df_fm, by = c("name" = "Joueur"))
  }
  


##### 
# Importer le dataframe final présent dans les dossier


#df <-read.csv(file = 'app/data/df.csv',stringsAsFactors = T,sep = ',',dec = '.')

