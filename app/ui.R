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
          p("L'objectif de cette application est de croiser les statistiques sportives individuelles (temps de jeu, efficacité offensive, solidité défensive) avec la valeur marchande estimée des joueurs. Cet outil permet ainsi d'identifier les profils sur-performants, d'évaluer l'apport réel d'un joueur par rapport à son prix, et de fournir une aide visuelle au recrutement (Scouting).")
        )
      ),
      
      # Carte 2 : Le dictionnaire de données
      card(
        card_header("📚 Dictionnaire des Données & Sources", class = "bg-primary text-white"),
        card_body(
          HTML('
            <div class="table-responsive">
              <table class="table table-striped table-hover table-sm align-middle">
                <thead class="table-dark">
                  <tr>
                    <th>Variable</th>
                    <th>Définition</th>
                    <th>Type / Unité</th>
                    <th>Source</th>
                  </tr>
                </thead>
                <tbody>
                  <tr><td><b>name</b></td><td>Identifiant unique du joueur (Nom complet)</td><td>Nominale (Character)</td><td><a href="https://www.kaggle.com/datasets/hubertsidorowicz/football-players-stats-2024-2025" target="_blank">Kaggle (Football Players Stats 24/25)</a></td></tr>
                  <tr><td><b>Age</b></td><td>Âge du joueur au moment de la collecte</td><td>Numérique</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Nation</b></td><td>Nationalité sportive du joueur</td><td>Nominale (Factor)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Pos</b></td><td>Position principale (FW: Attaquant, MF: Milieu, DF: Défenseur, GK: Gardien)</td><td>Nominale (Factor)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Squad</b></td><td>Club d\'appartenance (Ligue 1)</td><td>Nominale (Factor)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>MP</b></td><td>Matches Played. Nombre de rencontres disputées</td><td>Numérique</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Min</b></td><td>Minutes. Temps de jeu effectif total sur la saison</td><td>Numérique (Minutes)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>G.A</b></td><td>Goals + Assists. Cumul des buts marqués et passes décisives</td><td>Numérique</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>xG.xAG</b></td><td>Non-Penalty Expected Goals + Expected Assisted Goals. Espérance de buts et passes décisives par 90 minutes. Mesure la dangerosité moyenne.</td><td>Numérique (Ratio)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>KP</b></td><td>Key Passes. Passes menant directement à un tir</td><td>Numérique</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Tkl.Int</b></td><td>Tackles + Interceptions. Actions défensives réussies</td><td>Numérique</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr><td><b>Save.</b></td><td>Save Percentage. Taux d\'arrêts (Gardiens uniquement)</td><td>Numérique (%)</td><td>Kaggle (Football Players Stats 24/25)</td></tr>
                  <tr class="table-info"><td><b>value</b></td><td><b>Variable Cible (Y)</b>. Valeur marchande estimée</td><td>Numérique (Euros €)</td><td><a href="https://www.kaggle.com/datasets/xfkzujqjvx97n/football-datasets" target="_blank">Kaggle (5.7M+ Records)</a></td></tr>
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
          showcase = bs_icon("star-fill"),
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
          card_header("Efficacité : G.A vs Expected (xG.xAG)", class = "bg-light"),
          plotOutput("plot_efficiency", height = "350px")
        ),
        card(
          card_header("Top 10 : Valeurs Marchandes", class = "bg-light"),
          plotlyOutput("plot_value", height = "350px")
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