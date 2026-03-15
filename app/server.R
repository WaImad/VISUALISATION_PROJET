# --- server.R ---
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(scales)

# chargement des données
df <- read.csv("data/dataset_stat_large.csv", stringsAsFactors = FALSE)

#Modification des données
df$age_class <- cut(df$Age, 4)

df <- df |>
  mutate(Poste_FR = case_when(
    Pos == "GK" ~ "Gardien",
    Pos == "DF" ~ "Défenseur",
    Pos == "MF" ~ "Milieu",
    Pos == "FW" ~ "Attaquant",
    TRUE ~ Pos 
  ))

df$value2 <- format(df$value, scientific = FALSE)

df <- df |>
  mutate(foot = case_when(
    foot == "right" ~ "Droit",
    foot == "left" ~ "Gauche",
    TRUE ~ foot 
  ))

# Server
function(input, output, session) {
  
  #BARRE LATÉRALE
  observeEvent(input$mes_onglets, {
    if (input$mes_onglets == "onglet_contexte") {
      toggle_sidebar(id = "ma_sidebar", open = FALSE)
    } else {
      toggle_sidebar(id = "ma_sidebar", open = TRUE)
    }
  })
  
  # Menus déroulants présent dans la sidebar 
  observe({
    updateSelectInput(session, "squad", choices = c("Toutes", sort(unique(df$Squad))))
    updateSelectizeInput(session, "position", choices = c("Tous", unique(df$Poste_FR)), selected = "Tous" )
    updateSliderInput(session, "min_minutes", max = max(df$Min, na.rm = TRUE))
  })
  
  #Création d'un data frame filtré en fonction des éléments choisis dans les menus de la barre latérale
  data_filtered <- reactive({
    req(input$position, input$squad) 
    
    # filtre minutes
    data <- df |> filter(Min >= input$min_minutes)
    
    # filtre équipe
    if (input$squad != "Toutes") {
      data <- data |> filter(Squad == input$squad)
    }
    
    # filtre poste
    if (input$position != "Tous") {
      data <- data |> filter(Poste_FR == input$position) 
    }
    
    return(data)
  })
  
  # Récupération des données lié à un joueur selectionné
  data_j1 <- reactive({
    req(input$joueur1)
    df |> filter(name == input$joueur1) |> slice(1) 
  })
  
  # MISE À JOUR DU MENU DU JOUEUR 1
  observe({
    # On récupère les données filtrées selon l'équipe, le poste et les minutes
    df_f <- data_filtered()
    
    # On récupère la liste des noms valides, s'il n'y en a pas, alors ça n'affiche rien
    if (is.null(df_f) || nrow(df_f) == 0) {
      noms_finale <- character(0)
    } else {
      noms_finale <- sort(unique(df_f$name))
    }
    
    # On mémorise le joueur actuel pour ne pas l'effacer si on bouge le curseur des minutes
    joueur_actuel <- isolate(input$joueur1) 
    
    #On regarde si après avoir modifié des paramètres, certains joueurs peuvent encore correspondrent au caractéristiques
    selection <- if (!is.null(joueur_actuel) && joueur_actuel %in% noms_finale) {
      joueur_actuel
    } else {
      ""
    }
    
    # création du menu déroualant avec tous les joueurs correspondant 
    updateSelectizeInput(
      session = session,
      inputId = "joueur1",
      choices = c("Choisir un joueur" = "", noms_finale),
      selected = selection,
      server = TRUE
    )
  })
  
  
  # On refait une selection de joueur qui sera utile dans le cadre de la comparaison 
  output$choix_equipe_j2 <- renderUI({
    req(input$activer_comp)
    req(input$joueur1)
    selectInput("squad_j2", "Équipe du Joueur 2 :", 
                choices = c("Toutes", sort(unique(df$Squad))))
  })
  
  # 2. MENU : Choix du nom du Joueur 2 (FILTRÉ PAR L'ÉQUIPE !)
  output$select_joueur2 <- renderUI({
    req(input$activer_comp, input$joueur1, input$squad_j2) 
    
        poste_j1 <- df |> filter(name == input$joueur1) |> pull(Poste_FR) |> first()
    
    # On garde les joueurs du même poste, mais on enlève le Joueur 1 de la liste
    df_comp <- df |> 
      filter(Poste_FR == poste_j1) |> 
      filter(name != input$joueur1)
    
    if (input$squad_j2 != "Toutes") {
      df_comp <- df_comp |> filter(Squad == input$squad_j2)
    }
    
    # On extrait les noms restants
    joueurs_comp <- df_comp |> pull(name) |> unique() |> sort()
    
    selectizeInput(
      inputId = "joueur2", 
      label = paste("Nom du joueur (", poste_j1, ") :"), 
      choices = c("Choisir un joueur" = "", joueurs_comp),
      options = list(placeholder = 'Tapez un nom...')
    )
  })
  
  # Affichage du profil d'un joueur (utilisé dans l'onglet comparaison)
  output$profil_joueur1 <- renderUI({
    req(input$joueur1)
    infos <- df |> filter(name == input$joueur1) |> slice(1)
    url_image <- infos$player_image_url
    
    div(
      class = "text-center", 
      img(src = url_image, style = "border-radius: 0%; border: 3px solid #2C3E50; width: 120px;"),
      h2(infos$name),
      h4(infos$Squad, style = "color: #7f8c8d;"),
      hr(), 
      fluidRow(
        column(4, p(strong("Poste : "), infos$Poste_FR)),
        column(4, p(strong("Âge : "), infos$Age, " ans")),
        column(4, p(strong("Taille : "), infos$height, " cm"))
      ),
      fluidRow(""),
      
      # --- DEUXIÈME LIGNE (Total = 12) ---
      fluidRow(
        column(3, p(strong("Pied fort : "), infos$foot)),
        column(3, p(strong("Valeur : "), paste0(infos$value2, " €"))), 
        column(3, p(strong("Fin contrat : "), infos$contract_expires)),
        column(3, p(strong("Nationalité : "), infos$Nation))
      )
  )
  })
  
  # Affichage du profil d'un deuxième joueur (utilisé dans l'onglet comparaison)
  output$profil_joueur2 <- renderUI({
    req(input$activer_comp, input$joueur2)
    infos_j2 <- df |> filter(name == input$joueur2) |> slice(1)
    url_image_j2 <- infos_j2$player_image_url      
    
      div(
        class = "text-center",
        img(src = url_image_j2, style = "border-radius: 0%; border: 3px solid #e74c3c; width: 120px;"), 
        h2(infos_j2$name),
        h4(infos_j2$Squad, style = "color: #7f8c8d;"),
        hr(),
        
        # --- PREMIÈRE LIGNE (Total = 12) ---
        fluidRow(
          column(4, p(strong("Poste : "), infos_j2$Poste_FR)),
          column(4, p(strong("Âge : "), infos_j2$Age, " ans")),
          column(4, p(strong("Taille : "), infos_j2$height, " cm"))
        ),
        
        # --- DEUXIÈME LIGNE (Total = 12) ---
        fluidRow(
          column(3, p(strong("Pied fort : "), infos_j2$foot)),
          column(3, p(strong("Valeur : "), paste0(infos_j2$value2, " €"))), 
          column(3, p(strong("Fin contrat : "), infos_j2$contract_expires)),
          column(3, p(strong("Nationalité : "), infos_j2$Nation)
        )
      )
      )
  })
  
  # Mise en page de l'onglet stat individuelle et comparaison
  output$layout_profils <- renderUI({
    req(input$joueur1) 
    if (input$activer_comp) {
      card(
        class = "shadow-sm mb-4 border-top border-dark border-3",
        card_header("Fiches d'identité", class = "bg-primary text-dark"),
        card_body(
          fluidRow(
            column(width = 6, class = "border-end", uiOutput("profil_joueur1")),
            column(width = 6, uiOutput("profil_joueur2"))
          )
        )
      )
    } else {
      card(
        class = "shadow-sm mb-4 border-top border-dark border-3",
        card_header("Fiche d'identité", class = "bg-primary text-dark"),
        card_body(
          fluidRow(
            column(width = 12, uiOutput("profil_joueur1"))
          )
        )
      )
    }
  })

    output$stats_saison_j1 <- renderUI({
    req(input$joueur1)
    infos <- df |> filter(name == input$joueur1) |> slice(1)
    card(
      class = "shadow-sm mb-3", 
      card_header("Bilan de la saison", class = "bg-primary text-dark"),
      card_body(
        fluidRow(
          column(4, class = "text-center", p("Matchs joués", class = "text-muted mb-1"), h4(infos$MP, class = "text-primary fw-bold mb-0")),
          column(4, class = "text-center border-start border-end", p("Titulaire", class = "text-muted mb-1"), h4(infos$Starts, class = "text-primary fw-bold mb-0")),
          column(4, class = "text-center", p("Temps de jeu", class = "text-muted mb-1"), h4(paste0(infos$Min_stats_playing_time, " min"), class = "text-primary fw-bold mb-0"))
        )
      )
    )
  })
  
  output$stats_saison_j2 <- renderUI({
    req(input$activer_comp, input$joueur2)
    infos_j2 <- df |> filter(name == input$joueur2) |> slice(1)
    if(nrow(infos_j2) == 0) return(NULL)
    
    card(
      class = "shadow-sm mb-3", 
      card_header("Bilan de la saison", class = "bg-primary text-dark"),
      card_body(
        fluidRow(
          column(4, class = "text-center", p("Matchs joués", class = "text-muted mb-1 small"), h4(infos_j2$MP, class = "text-danger fw-bold mb-0")),
          column(4, class = "text-center border-start border-end", p("Titulaire", class = "text-muted mb-1 small"), h4(infos_j2$Starts, class = "text-danger fw-bold mb-0")),
          column(4, class = "text-center", p("Temps de jeu", class = "text-muted mb-1 small"), h4(paste0(infos_j2$Min_stats_playing_time, " min"), class = "text-danger fw-bold mb-0"))
        )
      )
    )
  })
  
  output$layout_stats <- renderUI({
    validate(
      need(input$joueur1 != "", "Veuillez sélectionner un joueur dans le menu de gauche pour afficher cette analyse.")
    )
    req(input$joueur1)
    if (input$activer_comp) {
      fluidRow(
        column(width = 6, uiOutput("stats_saison_j1")),
        column(width = 6, uiOutput("stats_saison_j2"))
      )
    } else {
      fluidRow(
        column(width = 12, uiOutput("stats_saison_j1"))
      )
    }
  })
  
  # Graphique toile d'araignée (Radar Chart)
  output$radar_chart <- renderPlotly({
    
    req(input$joueur1)
    
    infos_j1 <- df |> filter(name == input$joueur1) |> slice(1)
    poste <- infos_j1$Pos 
    
    # 1. Sélection des statistiques en fonction du poste
    if (grepl("Attaquant", poste) || grepl("FW", poste)) {
      stats_radar <- c("Buts" = "Gls", "Passes D." = "Ast", "xG" = "xG", "Tirs par match" = "Sh.90", "Tirs Cadrés par match" = "SoT.90")
      titre <- "Profil Offensif (Stats par 90 min)"
      
    } else if (grepl("Milieu", poste) || grepl("MF", poste)) {
      stats_radar <- c("Passes R." = "Cmp", "Passes Prg." = "PrgP", "Passes D." = "Ast", "Courses Prg." = "PrgC", "Tacles" = "Tkl", "Intercep." = "Int")
      titre <- "Profil Création / Relais (Stats par 90 min)"
      
    } else if (grepl("Défenseur", poste) || grepl("DF", poste)) {
      stats_radar <- c("Tacles" = "Tkl", "Intercep." = "Int", "Dégagements" = "Clr", "Contres" = "Blocks", "Passes R." = "Cmp")
      titre <- "Profil Défensif (Stats par 90 min)"
      
    } else if (grepl("Gardien", poste) || grepl("GK", poste)) {
      stats_radar <- c("Arrêts" = "Saves", "% Arrêts" = "Save.", "Clean Sheets" = "CS", "Passes R." = "Cmp", "Temps Jeu" = "Min")
      titre <- "Profil Gardien (Stats par 90 min)"
      
    } else {
      stats_radar <- c("Buts" = "Gls", "Passes D." = "Ast", "Passes R." = "Cmp", "Tacles" = "Tkl", "Temps Jeu" = "Min") 
      titre <- "Profil Général (Stats par 90 min)"
    }
    
    # Liste des variables à NE PAS ramener sur 90 min (pourcentages, temps de jeu ou stats déjà en /90)
    exceptions_p90 <- c("Sh.90", "SoT.90", "Save.", "Min", "CS%")
    
    couleur_j1 <- "#2C3E50" 
    couleur_j2 <- "#e74c3c" 
    
    # 2. Fonction pour calculer les valeurs P90 et Normalisées pour le radar
    calculer_radar <- function(joueur_data, df_complet) {
      val_norm <- numeric(length(stats_radar))
      text_hover <- character(length(stats_radar))
      
      # Filtre anti-valeurs aberrantes (joueurs ayant au moins 270 min)
      df_filtre <- df_complet |> filter(Min >= 270)
      
      for (i in seq_along(stats_radar)) {
        col <- stats_radar[i]
        nom_stat <- names(stats_radar)[i]
        
        val_brute <- joueur_data[[col]]
        minutes <- joueur_data[["Min"]]
        
        # SÉCURITÉ : Gérer les cas où Min est NA ou 0
        if (is.na(minutes) || minutes == 0) minutes <- 1
        
        # A. Calcul de la valeur réelle (P90 ou Brut)
        if (col %in% exceptions_p90) {
          val_reelle <- val_brute
          suffixe <- ""
        } else {
          val_reelle <- (val_brute / minutes) * 90
          suffixe <- " /90m"
        }
        
        # B. Calcul du Maximum du championnat pour l'échelle (P90 ou Brut)
        if (col %in% exceptions_p90) {
          max_val <- max(df_filtre[[col]], na.rm = TRUE)
        } else {
          max_val <- max((df_filtre[[col]] / df_filtre[["Min"]]) * 90, na.rm = TRUE)
        }
        
        # C. Normalisation (De 0 à 1) pour l'affichage graphique
        if (is.na(val_reelle) || is.na(max_val) || max_val <= 0) {
          val_norm[i] <- 0
          val_reelle <- 0
        } else {
          val_norm[i] <- min(val_reelle / max_val, 1) # min(..., 1) empêche de dépasser le radar
        }
        
        # D. Création du texte personnalisé pour l'infobulle
        text_hover[i] <- paste0("<b>", nom_stat, "</b> : ", round(val_reelle, 2), suffixe)
      }
      
      return(list(norm = val_norm, text = text_hover))
    }
    
    # 3. Extraction pour le Joueur 1
    donnees_j1 <- calculer_radar(infos_j1, df)
    
    # Initialisation du graphique (Joueur 1)
    p <- plot_ly(
      type = 'scatterpolar',
      r = c(donnees_j1$norm, donnees_j1$norm[1]), # Boucle pour fermer le polygone
      theta = c(names(stats_radar), names(stats_radar)[1]),
      text = c(donnees_j1$text, donnees_j1$text[1]), # Textes personnalisés
      hoverinfo = 'text', # IMPORTANT : Force Plotly à n'afficher que notre texte !
      fill = 'toself',
      name = infos_j1$name,
      line = list(color = couleur_j1),
      fillcolor = 'rgba(44, 62, 80, 0.5)' # Bleu/Gris transparent basé sur ta couleur_j1
    )
    
    # 4. Superposition si le Joueur 2 est activé
    if (input$activer_comp && !is.null(input$joueur2) && input$joueur2 != "") {
      infos_j2 <- df |> filter(name == input$joueur2) |> slice(1)
      donnees_j2 <- calculer_radar(infos_j2, df)
      
      p <- p |> add_trace(
        r = c(donnees_j2$norm, donnees_j2$norm[1]),
        theta = c(names(stats_radar), names(stats_radar)[1]),
        text = c(donnees_j2$text, donnees_j2$text[1]),
        hoverinfo = 'text',
        fill = 'toself',
        name = infos_j2$name,
        line = list(color = couleur_j2),
        fillcolor = 'rgba(231, 76, 60, 0.5)' # Rouge transparent
      )
    }
    
    # 5. Mise en forme finale
    p <- p |> layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 1),
          showticklabels = FALSE # Cache les chiffres de 0 à 1 inutiles
        )
      ),
      title = list(text = paste("<b>", titre, "</b>"), x = 0.5),
      showlegend = TRUE,
      margin = list(t = 80, b = 40),
      paper_bgcolor = 'rgba(0,0,0,0)', 
      plot_bgcolor  = 'rgba(0,0,0,0)',
      font = list(color = "#F8F9FA")
    )
    
    p
  })
  
  # Deuxième grpahique de l'ongelt comparaison et stat individuelle
  output$choix_vars_scatter <- renderUI({
    req(input$joueur1)
    poste <- df |> filter(name == input$joueur1) |> pull(Poste_FR) |> first()
    
    if (grepl("Attaquant", poste) || grepl("FW", poste)) {
      choix <- c("Buts" = "Gls", "Passes Décisives" = "Ast", "Tirs" = "Sh", "Tirs Cadrés" = "SoT", "Expected Goals" = "xG", "Minutes" = "Min")
    } else if (grepl("Milieu", poste) || grepl("MF", poste)) {
      choix <- c("Passes réussies" = "Cmp", "Passes progressives" = "PrgP", "Passes Décisives" = "Ast", "Tacles" = "Tkl", "Minutes" = "Min")
    } else if (grepl("Défenseur", poste) || grepl("DF", poste)) {
      choix <- c("Tacles" = "Tkl", "Interceptions" = "Int", "Dégagements" = "Clr", "Contres" = "Blocks", "Minutes" = "Min")
    } else if (grepl("Gardien", poste) || grepl("GK", poste)) {
      choix <- c("Arrêts" = "Saves", "% Arrêts" = "Save.", "Clean Sheets" = "CS", "Buts encaissés" = "GA", "Minutes" = "Min_stats_keeper")
    } else {
      choix <- c("Buts" = "Gls", "Passes" = "Cmp", "Tacles" = "Tkl", "Minutes" = "Min")
    }
    
    fluidRow(
      column(6, selectInput("var_x", "Statistique en bas (Axe Horizontal) :", choices = choix, selected = choix[1])),
      column(6, selectInput("var_y", "Statistique à gauche (Axe Vertical) :", choices = choix, selected = choix[2]))
    )
  })
  
  output$scatter_plot <- renderPlotly({
    # On vérifie que le joueur est sélectionné, sinon on affiche un message propre
    
    req(input$joueur1, input$var_x, input$var_y)
    infos_j1 <- df |> filter(name == input$joueur1) |> slice(1)
    poste <- infos_j1$Poste_FR
    joueurs_meme_poste <- df |> filter(Poste_FR == poste)
    
    p <- plot_ly() |>
      add_trace(
        data = joueurs_meme_poste,
        x = as.formula(paste0("~", input$var_x)),
        y = as.formula(paste0("~", input$var_y)),
        type = 'scatter', mode = 'markers',
        marker = list(color = 'rgba(200, 200, 200, 0.6)', size = 8, line = list(color = 'white', width = 0.5)),
        text = ~paste("<b>", name, "</b><br>", Squad),
        hoverinfo = 'text', name = 'Reste du championnat'
      ) |>
      add_trace(
        data = infos_j1,
        x = as.formula(paste0("~", input$var_x)),
        y = as.formula(paste0("~", input$var_y)),
        type = 'scatter', mode = 'markers',
        marker = list(color = '#2C3E50', size = 16, line = list(color = 'white', width = 2)), 
        text = ~paste("<b>", name, "</b>"), hoverinfo = 'text', name = infos_j1$name
      )
    
    if (input$activer_comp && !is.null(input$joueur2) && input$joueur2 != "") {
      infos_j2 <- df |> filter(name == input$joueur2) |> slice(1)
      p <- p |> add_trace(
        data = infos_j2,
        x = as.formula(paste0("~", input$var_x)),
        y = as.formula(paste0("~", input$var_y)),
        type = 'scatter', mode = 'markers',
        marker = list(color = '#e74c3c', size = 16, line = list(color = 'white', width = 2)), 
        text = ~paste("<b>", name, "</b>"), hoverinfo = 'text', name = infos_j2$name
      )
    }
    
    p |> layout(
      xaxis = list(title = input$var_x, zeroline = FALSE),
      yaxis = list(title = input$var_y, zeroline = FALSE),
      hovermode = 'closest',
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center")
      
    )
  })
  # GRAPHIQUE PAR CENTILE DE L'ONGLET COMPARAISON ET STAT INDIVIDUELLE
  
  output$explication_centiles <- renderUI({
    HTML("<div style='font-size: 14px; color: #bdc3c7; text-align: center; background-color: #1A2E44; padding: 10px; border-radius: 8px;'>
          💡 <b>Comment lire ce graphique ?</b> Le centile indique le pourcentage de joueurs du même poste qui font moins bien.<br>
          <i>Exemple : Une barre verte au <b>85ème centile</b> signifie que le joueur est meilleur que 85% des autres joueurs de la ligue à son poste.</i>
          </div>")
  })
  
  # --- 2. FONCTION POUR GÉNÉRER UN GRAPHIQUE CENTILE ---
  
  generer_graphique_centile <- function(nom_joueur) {
    
    if (is.null(nom_joueur) || nom_joueur == "") return(NULL)
    
    infos_j <- df |> filter(name == nom_joueur) |> slice(1)
    if (nrow(infos_j) == 0) return(NULL)
    poste <- infos_j$Poste_FR
    
    # On filtre les joueurs du même poste (+ de 270 min) pour éviter les valeurs aberrantes et les joueurs avec très peu de temps de jeu
    df_poste <- df |> filter(Poste_FR == poste & Min >= 270)
    if (!(nom_joueur %in% df_poste$name)) {
      df_poste <- bind_rows(df_poste, infos_j)
    }
    
    if (poste == "Attaquant") {
      stats <- c("Buts" = "Gls", "Passes D." = "Ast", "xG" = "xG", "Tirs /90" = "Sh.90", "Tirs Cadrés /90" = "SoT.90")
    } else if (poste == "Milieu") {
      stats <- c("Passes R." = "Cmp", "Passes Prg." = "PrgP", "Passes D." = "Ast", "Courses Prg." = "PrgC", "Tacles" = "Tkl", "Intercep." = "Int")
    } else if (poste == "Défenseur") {
      stats <- c("Tacles" = "Tkl", "Intercep." = "Int", "Dégagements" = "Clr", "Contres" = "Blocks", "Passes R." = "Cmp")
    } else if (poste == "Gardien") {
      stats <- c("Arrêts" = "Saves", "% Arrêts" = "Save.", "Clean Sheets" = "CS", "Passes R." = "Cmp", "Temps Jeu" = "Min")
    } else {
      stats <- c("Buts" = "Gls", "Passes D." = "Ast", "Passes R." = "Cmp", "Tacles" = "Tkl", "Temps Jeu" = "Min") 
    }
    
    exceptions_p90 <- c("Sh.90", "SoT.90", "Save.", "Min", "CS%")
    resultats <- data.frame(Statistique = names(stats), Centile = numeric(length(stats)), Valeur = numeric(length(stats)), stringsAsFactors = FALSE)
    
    for (i in seq_along(stats)) {
      col <- stats[i]
      if (col %in% exceptions_p90) {
        valeurs_groupe <- df_poste[[col]]
        valeur_joueur <- infos_j[[col]]
        suffixe <- ""
      } else {
        valeurs_groupe <- (df_poste[[col]] / df_poste[["Min"]]) * 90
        valeur_joueur <- (infos_j[[col]] / infos_j[["Min"]]) * 90
        suffixe <- " /90m"
      }
      
      valeurs_groupe[is.na(valeurs_groupe)] <- 0
      if (is.na(valeur_joueur)) valeur_joueur <- 0
      
      centile <- round(ecdf(valeurs_groupe)(valeur_joueur) * 100)
      resultats$Centile[i] <- centile
      resultats$Valeur[i] <- round(valeur_joueur, 2)
      resultats$Texte_Hover[i] <- paste0("<b>", nom_joueur, "</b><br>", names(stats)[i], "<br>Centile : <b>", centile, "</b><br>Valeur : ", round(valeur_joueur, 2), suffixe)
    }
    
    resultats$Statistique <- factor(resultats$Statistique, levels = rev(names(stats)))
    
    # Création du graphique avec le nom du joueur en titre
    p <- ggplot(resultats, aes(x = Statistique, y = Centile, fill = Centile, text = Texte_Hover)) +
      geom_bar(stat = "identity", width = 0.6) +
      coord_flip() +
      scale_fill_gradient2(low = "#e74c3c", mid = "#f1c40f", high = "#2ecc71", midpoint = 50, limits = c(0, 100)) +
      scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      labs(x = "", y = "Centile", title = nom_joueur) + # Le titre différencie les joueurs
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(color = "#F8F9FA", face = "bold", hjust = 0.5), # Titre en blanc
        text = element_text(color = "#F8F9FA"),
        axis.text = element_text(color = "#F8F9FA", size = 11, face = "bold"),
        panel.grid.major.x = element_line(color = "#1A2E44", linetype = "dashed"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        autosize = TRUE, # <-- 1. FORCE PLOTLY À RECALCULER SA TAILLE
        paper_bgcolor = 'rgba(0,0,0,0)', 
        plot_bgcolor = 'rgba(0,0,0,0)', 
        hoverlabel = list(bgcolor = "#07111F", font = list(color = "white")),
        margin = list(t = 40, l = 10, r = 10) # Un peu de marge sur les côtés
      )
  }
  
  # --- 3. CRÉATION DES DEUX GRAPHIQUES ---
  # On donne simplement les noms à notre nouvelle fonction magique !
  output$plot_centile_j1 <- renderPlotly({ generer_graphique_centile(input$joueur1) })
  
  output$plot_centile_j2 <- renderPlotly({ 
    req(input$activer_comp)
    generer_graphique_centile(input$joueur2) 
  })
  
  # --- 4. GESTION DE L'AFFICHAGE (1 ou 2 COLONNES) ---
  output$layout_percentiles <- renderUI({
    req(input$joueur1)
    
    if (input$activer_comp && !is.null(input$joueur2) && input$joueur2 != "") {
      # 2 JOUEURS : On ajoute les titres HTML en haut de chaque colonne
      fluidRow(
        column(width = 6, 
               h4(input$joueur1, class = "text-center text-info fw-bold mb-3"), # TITRE J1 (Bleu clair)
               div(style = "width: 100%; max-width: 100%; overflow: hidden;", 
                   plotlyOutput("plot_centile_j1", height = "350px"))
        ),
        column(width = 6, 
               h4(input$joueur2, class = "text-center text-danger fw-bold mb-3"), # TITRE J2 (Rouge)
               div(style = "width: 100%; max-width: 100%; overflow: hidden;", 
                   plotlyOutput("plot_centile_j2", height = "350px"))
        )
      )
    } else {
      # 1 JOUEUR : Titre HTML centré sur toute la largeur
      fluidRow(
        column(width = 12, 
               h4(input$joueur1, class = "text-center text-info fw-bold mb-3"), # TITRE J1
               div(style = "width: 100%; max-width: 100%; overflow: hidden;", 
                   plotlyOutput("plot_centile_j1", height = "400px"))
        )
      )
    }
  })
  
  # PRÉPATION DES GRPAHIQUES UTILISÉS DANS L'ONGLET DETECTION DE POTENTIELS
  output$val_joueurs <- renderText({ nrow(data_filtered()) })
  
  output$val_meilleur_ga <- renderText({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return("N/A")
    max(df_f$G.A, na.rm = TRUE)
  })
  
  output$nom_meilleur_ga <- renderText({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return("")
    df_f |> filter(G.A == max(G.A, na.rm = TRUE)) |> pull(name) |> first()
  })
  
  output$val_moyenne <- renderText({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return("N/A")
    paste0(round(mean(df_f$value, na.rm = TRUE) / 1e6, 2), " M€")
  })
  
  output$img_meilleur_ga <- renderUI({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(bs_icon("star-fill")) 
    
    best_player <- df_f |> filter(G.A == max(G.A, na.rm = TRUE)) |> slice(1) 
    url_image <- best_player$player_image_url
    
    if(is.null(url_image) || is.na(url_image) || url_image == "") {
      return(bs_icon("star-fill"))
    }
    tags$img(src = url_image, style = "max-height: 100px; width: auto; border-radius: 20px; object-fit: cover; box-shadow: 0 4px 8px rgba(0,0,0,0.2);")
  })
  
  # GRAPHIQUES GLOBAUX 
  
  # Graphique 1 avec commentaire 
  output$plot_efficiency <- renderPlotly({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
    p <- ggplot(df_f, aes(x = (xG + xAG), y = G.A, color = Poste_FR, 
                          text = paste0("<b>", name, "</b><br>Club : ", Squad, "<br>Temps de jeu : ", Min, " min<br>---------------------<br>G+A (Réel) : <b>", G.A, "</b><br>xG+xAG (Attendu) : <b>", (xG + xAG), "</b><br>Différence : ", round(G.A - (xG + xAG), 2)))) +
      geom_point(size = 3, alpha = 0.8) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#FF1744") +
      labs(x = "Expected Goals + Expected Assists (xG + xAG)", y = "Buts + Passes Décisives réels (G.A)", color = "Poste") +
      theme_minimal() + 
      theme(legend.position = "bottom", text = element_text(color = "#F8F9FA"), axis.text = element_text(color = "#F8F9FA"), panel.grid.major = element_line(color = "#1A2E44"), panel.grid.minor = element_blank())
    
    ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h", x = 0, y = -0.2), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor  = 'rgba(0,0,0,0)', hoverlabel = list(bgcolor = "#1A2E44", font = list(color = "white")))
  })
  
  output$commentaire_efficiency <- renderUI({ 
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(HTML("Aucun joueur ne correspond aux critères de filtrage."))
    
    top_efficiency <- df_f |> mutate(efficiency = G.A - (xG + xAG)) |> arrange(desc(efficiency)) |> head(1)
    
    texte_html <- paste0(
      "<div style='font-size: 13px; line-height: 1; text-align: justify;'>", 
      "Les joueurs au dessus de la droite sont ceux qui sont très efficaces, ils marquent plus de buts qu'espéré.<br>",
      "Au contraire les joueurs en dessous de la droite sont des joueurs en sous-efficacité, ils marquent moins de buts qu'attendus.<br><br>",
      "Le joueur le plus efficient est <b>", top_efficiency$name, "</b> avec une sur-performance d'actions décisives <b>", 
      round(top_efficiency$efficiency, 2), "</b> (G.A : ", top_efficiency$G.A, " vs xG+xAG : ", round((top_efficiency$xG + top_efficiency$xAG), 2), ").",
      "</div>" 
    )
    HTML(texte_html) 
  })
  
  #Graphique 2 Valeurs marchandes en fonction du poste
  output$plot_value <- renderPlotly({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
    top_10 <- df_f |> arrange(desc(value)) |> head(10)
    p <- ggplot(top_10, aes(x = reorder(name, value), y = value, fill = Poste_FR,
                            text = paste("Joueur :", name, "<br>Valeur :", round(value / 1e6, 2), "M€<br>Club :", Squad))) +
      geom_bar(stat = "identity") +
      coord_flip() + 
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M€")) +
      labs(x = "", y = "Valeur Marchande", fill = "Poste") +
      theme_minimal() + 
      theme(legend.position = "bottom", text = element_text(color = "#F8F9FA"), axis.text = element_text(color = "#F8F9FA"), panel.grid.major.x = element_line(color = "#1A2E44"), panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h", x = 0, y = -0.2), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor  = 'rgba(0,0,0,0)', hoverlabel = list(bgcolor = "#1A2E44", font = list(color = "white")))
  })
  
  #Graphique 3 Temps de jeu
  output$plot_timeplay <- renderPlotly({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
    top_10_min <- df_f |> arrange(desc(Min)) |> head(10)
    p <- ggplot(top_10_min, aes(x = reorder(name, Min), y = Min, fill = Poste_FR,
                                text = paste0("Club : ", Squad, "<br>Âge : ", Age, " ans<br>---------------------<br>Minutes jouées : <b>", Min, " min</b><br>Matchs joués (MP) : ", MP, "<br>Moyenne : <b>", round(Min / MP, 1), "</b> min/match")
    )) +
      geom_bar(stat = "identity") +
      coord_flip() + 
      labs(x = "", y = "Minutes jouées (Min)", fill = "Poste") +
      theme_minimal() + 
      theme(legend.position = "bottom", text = element_text(color = "#F8F9FA"), axis.text = element_text(color = "#F8F9FA"), panel.grid.major.x = element_line(color = "#1A2E44"), panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h", x = 0, y = -0.2), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor  = 'rgba(0,0,0,0)', hoverlabel = list(bgcolor = "#1A2E44", font = list(color = "white")))
  })
  
  #Graphique 4 avec les nations présentes dans une équipe
  output$plot_nationalities <- renderPlotly({
    df_f <- data_filtered()
    if(nrow(df_f) == 0 || !"Nation" %in% names(df_f)) return(NULL)
    
    top_nations <- df_f |> count(Nation, name = "Nombre") |> arrange(desc(Nombre)) |> head(10)
    
    p <- ggplot(top_nations, aes(x = reorder(Nation, Nombre), y = Nombre, fill = Nation, text = paste0("<b>", Nation, "</b><br>Joueurs : <b>", Nombre, "</b>"))) +
      geom_bar(stat = "identity", show.legend = FALSE) + 
      coord_flip() + 
      labs(x = "", y = "Nombre de joueurs") +
      theme_minimal() + 
      theme(text = element_text(color = "#F8F9FA"), axis.text = element_text(color = "#F8F9FA"), panel.grid.major.x = element_line(color = "#1A2E44"), panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text") |> hide_legend() |> layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor  = 'rgba(0,0,0,0)', hoverlabel = list(bgcolor = "#1A2E44", font = list(color = "white")))
  })
  
  # GRAPHIQUE ANCIENNE VERSION 
  output$plot_age_value <- renderPlot({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
    ggplot(df_f, aes(x = age_class, y = value / 1e6, fill = age_class)) +
      geom_boxplot(outlier.colour = "red", outlier.alpha = 0.6) +
      scale_fill_brewer(palette = "Blues") +
      labs(title = "Valeur Marchande selon la Tranche d'Âge", subtitle = "Cycle de vie économique d'un joueur", x = "Tranche d'âge", y = "Valeur Marchande (en Millions d'€)", fill = "Âge") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "none")
  })
  
  #GRAPHIQUE UTILISÉ POUR L'ONGLET EXPLORATEUR DE JOUEUR
  output$table_data <- renderDT({
    data_filtered() |>
      select(name, Age, Poste_FR, Squad, Min, G.A, xG.xAG, Tkl.Int, value) |>
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Joueur", "Âge", "Poste", "Équipe", "Minutes", "G+A", "xG+xAG", "Tacles/Int", "Valeur (€)")
      ) |>
      formatCurrency("value", currency = "€", interval = 3, mark = " ", digits = 0)
  })
  
  #GRAPHIQUE UTILISÉ DANS L'ANALYSE GLOBALE DU CHAMPIONNATS
  output$plot_global_final <- renderPlotly({
    df_f <- data_filtered()
    validate(need(nrow(df_f) > 1, "Pas assez de données."))
    
    fmt_num <- function(x) format(round(x), big.mark = " ", scientific = FALSE)
    
    creer_classes_auto <- function(df, col_name, label_nom) {
      vec <- df[[col_name]]
      nb_uniques <- length(unique(vec))
      nb_groupes <- min(4, nb_uniques)
      
      if(nb_groupes < 2) return(factor(rep("Groupe Unique", nrow(df))))
      
      seuils <- round(unique(quantile(vec, probs = seq(0, 1, length.out = nb_groupes + 1), na.rm = TRUE)))
      classes <- cut(vec, breaks = seuils, include.lowest = TRUE)
      
      n_table <- table(classes)
      levels_old <- levels(classes)
      
      new_labels <- sapply(levels_old, function(lvl) {
        count <- n_table[lvl]
        nums_str <- gsub("\\[|\\]|\\(|\\)", "", lvl)
        nums <- as.numeric(unlist(strsplit(nums_str, ",")))
        
        if(length(nums) < 2) return(paste0(lvl, "\n(n=", count, ")"))
        if(grepl("Prix", label_nom)) {
          v1 <- format(nums[1]/1e6, scientific = FALSE, drop0trailing = TRUE)
          v2 <- format(nums[2]/1e6, scientific = FALSE, drop0trailing = TRUE)
          range_clean <- paste0(v1, " à ", v2, " M€")
        } else {
          v1 <- format(round(nums[1]), big.mark = " ", scientific = FALSE)
          v2 <- format(round(nums[2]), big.mark = " ", scientific = FALSE)
          range_clean <- paste0(v1, " à ", v2)
        }
        paste0(label_nom, ":\n", range_clean, "\n(n=", count, ")")
      })
      factor(classes, levels = levels_old, labels = new_labels)
    }
    if (input$global_group == "tranche_age") {
      df_f$group_var <- creer_classes_auto(df_f, "Age", "Âges")
      
    } else if (input$global_group == "Squad") {
      df_f$group_var <- as.factor(df_f$Squad)
      counts <- table(df_f$group_var)
      levels(df_f$group_var) <- paste0(levels(df_f$group_var), "\n(n=", counts[levels(df_f$group_var)], ")")
      
    } else if (input$global_group == "cat_ga") {
      df_f$group_var <- creer_classes_auto(df_f, "G.A", "Buts + Passes")
      
    } else if (input$global_group == "cat_val") {
      df_f$group_var <- creer_classes_auto(df_f, "value", "Prix")
      
    } else if (input$global_group == "cat_min") {
      df_f$group_var <- creer_classes_auto(df_f, "Min", "Minutes")
      
    } else {
      df_f$group_var <- factor(rep(paste0("Sélection actuelle\n(n=", nrow(df_f), ")"), nrow(df_f)))
    }
    
    noms_y <- c("value" = "Valeur Marchande (€)", "G.A" = "Buts + Passes Décisives", 
                "xG.xAG" = "Actions attendues (xG+xAG)", "Min" = "Minutes jouées")
    
    p <- ggplot(df_f, aes(x = group_var, y = .data[[input$global_y]], fill = group_var, text = name)) +
      geom_boxplot(alpha = 0.7, outlier.colour = "red") +
      geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
      labs(x = "", y = noms_y[input$global_y]) +
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      theme_minimal() +
      theme(
        legend.position = "none", 
        # 1. On passe tout le texte en blanc/gris clair
        text = element_text(color = "#F8F9FA"),
        axis.text.x = element_text(size = 9, color = "#F8F9FA"),
        axis.text.y = element_text(color = "#F8F9FA"),
        # 2. On adapte la couleur de la grille au thème sombre
        panel.grid.major = element_line(color = "#1A2E44"),
        panel.grid.minor = element_blank()
      )
    
    # 3. On rend le fond 100% transparent avec Plotly
    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)', 
        plot_bgcolor  = 'rgba(0,0,0,0)', 
        hoverlabel = list(bgcolor = "#07111F", font = list(color = "white"))
      )
  })
  
  output$table_data <- renderDT({
    data_filtered() |>
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        colnames = names(df)
      ) |>
      formatCurrency("value", currency = "€", interval = 3, mark = " ", digits = 0)
  })
}

