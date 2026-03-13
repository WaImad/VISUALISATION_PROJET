# --- ui.R ---
library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(DT)

# Définition du thème ( A modifier si vous voulez)
theme_ligue1 <- bs_theme(
  bg = "#07111F",        # Bleu nuit très profond (Fond de l'application)
  fg = "#F8F9FA",        # Texte en blanc cassé pour une bonne lisibilité
  primary = "#085fff",   # Le fameux Bleu Cyan électrique du nouveau logo L1 !
  secondary = "#085fff", # Bleu marine pour les cartes et bordures
  success = "#00C853",   # Vert vif pour les stats positives
  danger = "#FF1744",    # Rouge vif pour les alertes
  base_font = font_google("Montserrat"),    # Police moderne et sportive
  heading_font = font_google("Montserrat")  # Police moderne et sportive
)


page_navbar(

  theme = theme_ligue1,
  title = tags$span(
    tags$img(
      src = "https://ligue1.com/images/Logo_Ligue1.webp", 
      height = "35px", 
      style = "margin-right: 10px;"
    ), 
    "Scouting Dashboard | L1 24-25"
  ),
  # BARRE LATÉRALE (Filtres)
  sidebar = sidebar(
    width = 300,
    h4("Filtres"),
    
    selectInput("squad", "Équipe :", choices = "Chargement..."),
    checkboxGroupInput("position", "Poste :", choices = "Chargement..."),
    sliderInput("min_minutes", "Minutes jouées minimum :", min = 0, max = 3500, value = 500)
  ),
  
  nav_panel(
    title = "Contexte & Données",
    icon = bs_icon("info-circle"),
    
    layout_columns(
      col_widths = 12, # Prend toute la largeur
      
      # Carte 1 : L'introduction
      card(
        card_header("⚽ Introduction au Projet", class = "bg-primary text-white"),
        card_body(
          p("Bienvenue sur ce tableau de bord interactif dédié à l'analyse des performances des joueurs de Ligue 1 pour la saison 2024-2025."),
          tags$img(
            src = "https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExeXlpZ2dsZ29hN2M0NXFkaXlnMXNla2VzanRsbnphanFpOG83MTYxbiZlcD12MV9naWZzX3NlYXJjaCZjdD1n/KWvok8lP2noNDRTpXw/giphy.gif", 
            width = "28%"
          ),
          p("L'objectif de cette application est de croiser les statistiques sportives individuelles (temps de jeu, efficacité offensive, solidité défensive) avec la valeur marchande estimée des joueurs. Cet outil permet ainsi d'identifier les profils sur-performants, d'évaluer l'apport réel d'un joueur par rapport à son prix, et de fournir une aide visuelle au recrutement (Scouting).")
        )
      ),
      
      # Carte 2 : Le dictionnaire de données
      card(
        card_header("📚 Dictionnaire des Données & Sources", class = "bg-primary text-dark"),
        card_body(
          HTML('
            <div class="table-responsive" style="max-height: 600px; overflow-y: auto;">
              <table class="table table-hover table-sm align-middle text-light">
                <thead class="sticky-top bg-dark text-info">
                  <tr>
                    <th>Variable</th>
                    <th>Définition/explication</th>
                    <th>Type / Unité</th>
                    <th>Source</th>
                  </tr>
                </thead>
                <tbody>
                  <tr class="table-secondary text-dark"><th colspan="4">ℹ️ Informations de Base</th></tr>
                  <tr><td><b>Player</b></td><td>Nom complet du joueur</td><td>Texte</td><td><a href="https://www.kaggle.com/datasets/hubertsidorowicz/football-players-stats-2024-2025" target="_blank">Kaggle (Football Players Stats 24/25)</a></td></tr>
                  <tr><td><b>Nation</b></td><td>Nationalité sportive</td><td>Texte</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Pos</b></td><td>Poste (FW, MF, DF, GK)</td><td>Catégorie</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Squad</b></td><td>Club d\'appartenance</td><td>Texte</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Comp</b></td><td>Championnat (ex: Ligue 1)</td><td>Texte</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Age</b></td><td>Âge du joueur</td><td>Numérique (Années)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Height</b></td><td><b>Taille du joueur</b></td><td>Numérique (centimètres)</td><td>Kaggle (5.7M+ Records)</td></tr>
                  <tr><td><b>Foot</b></td><td><b>Pied fort du joueur</b></td><td>Texte</td><td>Kaggle (5.7M+ Records)</td></tr>
                  <tr><td><b>Born</b></td><td>Année de naissance</td><td>Numérique (Année)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Player Agent Name</b></td><td><b>Agent du joueur</b></td><td>Texte</td><td>Kaggle (5.7M+ Records)</td></tr>
                  <tr><td><b>value</b></td><td><b>Valeur marchande estimée du joueur</b></td><td>Numérique (€)</td><td><a href="https://www.kaggle.com/datasets/xfkzujqjvx97n/football-datasets" target="_blank">Kaggle (5.7M+ Records)</a></td></tr>

                  <tr class="table-secondary text-dark"><th colspan="4">⏱️ Temps de Jeu & Apparitions</th></tr>
                  <tr><td><b>MP</b></td><td>Matchs joués (Apparitions)</td><td>Numérique (Nb)</td><td><a href="https://www.kaggle.com/datasets/hubertsidorowicz/football-players-stats-2024-2025" target="_blank">Kaggle (Football Players Stats 24/25)</a></td></tr>
                  <tr><td><b>Starts</b></td><td>Titularisations (Matchs commencés)</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Min</b></td><td>Minutes totales jouées</td><td>Numérique (Minutes)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>90s</b></td><td>Équivalent de matchs entiers joués (Minutes / 90)</td><td>Numérique (Ratio)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>

                  <tr class="table-secondary text-dark"><th colspan="4">⚔️ Statistiques Offensives</th></tr>
                  <tr><td><b>Gls</b></td><td>Buts marqués</td><td>Numérique (Nb)</td><td><a href="https://www.kaggle.com/datasets/hubertsidorowicz/football-players-stats-2024-2025" target="_blank">Kaggle (Football Players Stats 24/25)</a>)</td></tr>
                  <tr><td><b>Ast</b></td><td>Passes décisives</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>G+A</b></td><td>Somme des Buts et Passes décisives</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>xG</b></td><td>Expected Goals (Probabilité de marquer)</td><td>Numérique (Ratio)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>xAG</b></td><td>Expected Assisted Goals (Probabilité de passe décisive)</td><td>Numérique (Ratio)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>npxG</b></td><td>Expected Goals hors pénaltys</td><td>Numérique (Ratio)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>G-PK</b></td><td>Buts marqués en excluant les pénaltys</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Sh/90</b></td><td>Nombre de tirs par match</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>SoT/90</b></td><td>Nombre de tirs cadrés par match</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>G/Sh</b></td><td>Nombre de buts par tir (efficacité) </td><td>Numérique (Ration)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  

                  <tr class="table-secondary text-dark"><th colspan="4">🛡️ Statistiques Défensives</th></tr>
                  <tr><td><b>Tkl</b></td><td>Tacles tentés</td><td>Numérique (Nb)</td><td><a href="https://www.kaggle.com/datasets/hubertsidorowicz/football-players-stats-2024-2025" target="_blank">Kaggle (Football Players Stats 24/25)</a></td></tr>
                  <tr><td><b>TklW</b></td><td>Tacles réussis (Possession récupérée)</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Blocks</b></td><td>Tirs ou passes bloqués</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Int</b></td><td>Interceptions de passes adverses</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Tkl+Int</b></td><td>Total combiné des tacles et interceptions</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Clr</b></td><td>Dégagements défensifs</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Err</b></td><td>Erreurs directes menant à un tir ou but</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>

                  <tr class="table-secondary text-dark"><th colspan="4">🎯 Passes & Créativité</th></tr>
                  <tr><td><b>PrgP</b></td><td>Passes progressives (vers l\'avant)</td><td>Numérique (Nb)</td><td><a href="https://www.kaggle.com/datasets/hubertsidorowicz/football-players-stats-2024-2025" target="_blank">Kaggle (Football Players Stats 24/25)</a></td></tr>
                  <tr><td><b>PrgC</b></td><td>Conduites de balle progressives vers l\'avant</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>KP</b></td><td>Passes clés (menant à un tir)</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Cmp%_stats_passing</b></td><td>Pourcentage de passes réussies</td><td>Numérique (%)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Ast_stats_passing</b></td><td>Passes décisives (Depuis les stats de passes)</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>xA</b></td><td>Expected Assists (Passes décisives attendues)</td><td>Numérique (Ratio)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>PPA</b></td><td>Passes réussies dans la surface de réparation adverse</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>

                  <tr class="table-secondary text-dark"><th colspan="4">🧤 Statistiques des Gardiens</th></tr>
                  <tr><td><b>GA</b></td><td>Buts encaissés</td><td>Numérique (Nb)</td><td><a href="https://www.kaggle.com/datasets/hubertsidorowicz/football-players-stats-2024-2025" target="_blank">Kaggle (Football Players Stats 24/25)</a></td></tr>
                  <tr><td><b>Saves</b></td><td>Arrêts réalisés</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Save%</b></td><td>Pourcentage de tirs arrêtés</td><td>Numérique (%)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>CS</b></td><td>Clean sheets (Matchs sans encaisser)</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>CS%</b></td><td>Pourcentage de matchs en Clean sheet</td><td>Numérique (%)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>PKA</b></td><td>Pénaltys subis</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>PKsv</b></td><td>Pénaltys arrêtés</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>

                  <tr class="table-secondary text-dark"><th colspan="4">⚽ Possession & Contrôle de Balle</th></tr>
                  <tr><td><b>Touches</b></td><td>Nombre total de ballons touchés</td><td>Numérique (Nb)</td><td><a href="https://www.kaggle.com/datasets/hubertsidorowicz/football-players-stats-2024-2025" target="_blank">Kaggle (Football Players Stats 24/25)</a></td></tr>
                  <tr><td><b>Carries</b></td><td>Nombre total de conduites de balle</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>PrgR</b></td><td>Courses progressives (Conduites percutantes)</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Mis</b></td><td>Mauvais contrôles (Faute technique)</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Dis</b></td><td>Pertes de balle (Dépossédé)</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>

                  <tr class="table-secondary text-dark"><th colspan="4">📊 Statistiques Diverses</th></tr>
                  <tr><td><b>CrdY</b></td><td>Cartons jaunes reçus</td><td>Numérique (Nb)</td><td><a href="https://www.kaggle.com/datasets/hubertsidorowicz/football-players-stats-2024-2025" target="_blank">Kaggle (Football Players Stats 24/25)</a></td></tr>
                  <tr><td><b>CrdR</b></td><td>Cartons rouges reçus</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>PKwon</b></td><td>Pénaltys obtenus</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>PKcon</b></td><td>Pénaltys concédés</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Recov</b></td><td>Ballons récupérés</td><td>Numérique (Nb)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                
                </tbody>
              </table>
            </div>
          ')
        )
    )
    )
  ),
  # --- ONGLET 1 : ANALYSE DES PERFORMANCES ---
  nav_panel(
    title = "Analyse des Performances",
      
     
      
      # CONTENU PRINCIPAL
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Joueurs analysés",
          value = textOutput("val_joueurs"),
          showcase = bs_icon("people-fill"),
          theme = "primary"
        ),
        value_box(
          title = "Meilleur Buteur/Passeur (G.A)",
          value = textOutput("val_meilleur_ga"),
          p(textOutput("nom_meilleur_ga")),
          showcase = uiOutput("img_meilleur_ga"), 
          theme = "success"
        ),
        value_box(
          title = "Valeur moyenne de l'effectif",
          value = textOutput("val_moyenne"),
          showcase = bs_icon("cash-coin"),
          theme = "info"
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("💥 Efficacité : G.A vs Expected (xG.xAG)", class = "bg-primary text-dark"),
          plotlyOutput("plot_efficiency", height = "350px"),
          htmlOutput("commentaire_efficiency")
        ),
        
        card(
          card_header("💵 Top 10 : Valeurs Marchandes", class = "bg-primary text-dark"),
          plotlyOutput("plot_value", height = "350px")
        )
      ),
    layout_columns(
      col_widths = c(6,6),
      card(
        card_header("⏱️ Top 10 : Joueurs les plus utilisés (Temps de jeu)", class = "bg-primary text-dark"),
        plotlyOutput("plot_timeplay", height = "350px")
      ),
      card(
        card_header("🌍 Top 10 : Nationalités les plus représentées", class = "bg-primary text-dark"),
        plotlyOutput("plot_nationalities", height = "350px")
      )
    )
  ),
  
  # --- ONGLET 2 : BASE DE DONNÉES ---
  nav_panel(
    title = "Explorateur de Données",
    card(
      card_header("Données brutes filtrables"),
      DTOutput("table_data")
    )
  ),
  
nav_panel(
  title = "Analyse Globale du championnat",
  card(
    card_header("Valeur Marchande selon la Tranche d'Âge"),
    plotOutput("plot_age_value", height = "350px")
  )
)
)