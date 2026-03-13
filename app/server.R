# --- server.R ---
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(scales)

# 1. CHARGEMENT DES DONNÉES (au démarrage du serveur)
df <- read.csv("data/dataset_stat_large.csv", stringsAsFactors = FALSE)
df$age_class <- cut(df$Age, 4)

df <- df %>%
  mutate(Poste_FR = case_when(
    Pos == "GK" ~ "Gardien",
    Pos == "DF" ~ "Défenseur",
    Pos == "MF" ~ "Milieu",
    Pos == "FW" ~ "Attaquant",
    TRUE ~ Pos 
  ))

df <- df %>%
  mutate(foot = case_when(
    foot == "right" ~ "Droit",
    foot == "left" ~ "Gauche",
    TRUE ~ foot # SÉCURITÉ : Garde la valeur d'origine si ce n'est ni right ni left (ex: "both" ou NA)
  ))

function(input, output, session) {
  
  observeEvent(input$mes_onglets, {
    if (input$mes_onglets == "onglet_contexte") {
      # Rétracte (ferme) la sidebar sur la page Contexte
      toggle_sidebar(id = "ma_sidebar", open = FALSE)
    } else {
      toggle_sidebar(id = "ma_sidebar", open = TRUE)
    }
  })
  
  # 2. MISE À JOUR DYNAMIQUE DES FILTRES DE BASE
  observe({
    updateSelectInput(session, "squad", choices = c("Toutes", sort(unique(df$Squad))))
    
    updateSelectizeInput(session, "position", choices = unique(df$Poste_FR), selected = unique(df$Poste_FR))
    
    updateSliderInput(session, "min_minutes", max = max(df$Min, na.rm = TRUE))
  })
  
  data_filtered <- reactive({
    req(input$position, input$squad) 
    
    data <- df %>%
      filter(Poste_FR %in% input$position) %>%
      filter(Min >= input$min_minutes) 
    
    if (input$squad != "Toutes") {
      data <- data %>% filter(Squad == input$squad)
    }
    
    return(data)
  })
  
  # 3. CRÉATION DES DATASETS RÉACTIFS
  data_j1 <- reactive({
    req(input$joueur1)
    df |> filter(name == input$joueur1) |> slice(1) 
  })
  
  # MISE À JOUR DU MENU DU JOUEUR 1
  observe({
    # CORRECTION 3 : On va chercher les noms dans data_filtered() et non plus dans df !
    df_f <- data_filtered()
    
    # Sécurité au cas où le tableau serait vide à cause des filtres
    if (nrow(df_f) == 0) {
      noms_finale <- character(0)
    } else {
      noms_finale <- sort(unique(df_f$name))
    }
    
    updateSelectizeInput(
      session = session,
      inputId = "joueur1",
      choices = c("Choisir un joueur" = "", noms_finale),
      server = TRUE
    )
  })

  
  
  # =========================================================================
  # TOUS LES OUTPUTS (DÉSORMAIS BIEN SORTIS DE L'OBSERVE)
  # =========================================================================
  
  # --- MENUS DE COMPARAISON ---
  # Choix du filtre équipe que'on va appliquer appliquer au joueur 2 (si comparaison activée)
  # --- MENUS DE COMPARAISON ---
  
  # 1. MENU : Choix de l'équipe du Joueur 2
  output$choix_equipe_j2 <- renderUI({
    req(input$activer_comp)
    req(input$joueur1)
    selectInput("squad_j2", "Équipe du Joueur 2 :", 
                choices = c("Toutes", sort(unique(df$Squad))))
  })
  
  # 2. MENU : Choix du nom du Joueur 2 (FILTRÉ PAR L'ÉQUIPE !)
  output$select_joueur2 <- renderUI({
    # On attend que le joueur 1 ET l'équipe 2 soient bien chargés
    req(input$activer_comp, input$joueur1, input$squad_j2) 
    
    # On récupère le poste du J1 (en français)
    poste_j1 <- df %>% filter(name == input$joueur1) %>% pull(Poste_FR) %>% first()
    
    # On garde les joueurs du même poste, mais on enlève le Joueur 1 de la liste
    df_comp <- df %>% 
      filter(Poste_FR == poste_j1) %>% 
      filter(name != input$joueur1)
    
    # LE FILTRE MAGIQUE : Si on a choisi une équipe spécifique, on filtre le tableau !
    if (input$squad_j2 != "Toutes") {
      df_comp <- df_comp %>% filter(Squad == input$squad_j2)
    }
    
    # On extrait les noms restants
    joueurs_comp <- df_comp %>% pull(name) %>% unique() %>% sort()
    
    # On crée le menu déroulant
    selectizeInput(
      inputId = "joueur2", 
      label = paste("Nom du joueur (", poste_j1, ") :"), 
      choices = c("Choisir un joueur" = "", joueurs_comp),
      options = list(placeholder = 'Tapez un nom...')
    )
  })
  
  # --- PROFILS DES JOUEURS ---
  output$profil_joueur1 <- renderUI({
    req(input$joueur1)
    infos <- df %>% filter(name == input$joueur1) %>% slice(1)
    url_image <- infos$player_image_url
    
    div(
      class = "text-center", 
      img(src = url_image, style = "border-radius: 0%; border: 3px solid #2C3E50; width: 120px;"),
      h2(infos$name),
      h4(infos$Squad, style = "color: #7f8c8d;"),
      hr(), 
      fluidRow(
        column(2, p(strong("Âge : "), infos$Age, " ans")),
        column(2, p(strong("Taille : "), infos$height, " cm")),
        column(3, p(strong("Pied fort : "), infos$foot)),
        column(2, p(strong("Valeur : "), paste0(infos$value , " €"))),
        column(3, p(strong("Fin contrat : "), infos$contract_expires))
      )
    )
  })
  
  output$profil_joueur2 <- renderUI({
    req(input$activer_comp, input$joueur2)
    infos_j2 <- df %>% filter(name == input$joueur2) %>% slice(1)
    url_image_j2 <- infos_j2$player_image_url      
    
    div(
      class = "text-center",
      img(src = url_image_j2, style = "border-radius: 0%; border: 3px solid #e74c3c; width: 120px;"), 
      h2(infos_j2$name),
      h4(infos_j2$Squad, style = "color: #7f8c8d;"),
      hr(),
      fluidRow(
        column(2, p(strong("Âge : "), infos_j2$Age, " ans")),
        column(2, p(strong("Taille : "), infos_j2$height, " cm")),
        column(3, p(strong("Pied fort : "), infos_j2$foot)),
        column(2, p(strong("Valeur : "), paste0(infos_j2$value, " €"))),
        column(3, p(strong("Fin contrat : "), infos_j2$contract_expires))
      )
    )
  })
  
  # --- MISE EN PAGE DES CARTES ---
  output$layout_profils <- renderUI({
    req(input$joueur1) 
    if (input$activer_comp) {
      card(
        class = "shadow-sm mb-4 border-top border-dark border-3",
        card_header("👤 Fiches d'identité", class = "bg-primary text-dark"),
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
        card_header("👤 Fiche d'identité", class = "bg-primary text-dark"),
        card_body(
          fluidRow(
            column(width = 12, uiOutput("profil_joueur1"))
          )
        )
      )
    }
  })
  
  # --- STATS DE LA SAISON ---
  output$stats_saison_j1 <- renderUI({
    req(input$joueur1)
    infos <- df %>% filter(name == input$joueur1) %>% slice(1)
    card(
      class = "shadow-sm mb-3", 
      card_header("📊 Bilan de la saison", class = "bg-primary text-dark"),
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
    infos_j2 <- df %>% filter(name == input$joueur2) %>% slice(1)
    if(nrow(infos_j2) == 0) return(NULL)
    
    card(
      class = "shadow-sm mb-3", 
      card_header("📊 Bilan de la saison", class = "bg-primary text-dark"),
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
      need(input$joueur1 != "", "Veuillez sélectionner un joueur dans le menu de gauche pour afficher cette analyse. ⚽")
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
  
  # --- GRAPHIQUE RADAR ---
  output$radar_chart <- renderPlotly({
    req(input$joueur1)
    infos <- df %>% filter(name == input$joueur1) %>% slice(1)
    poste <- infos$Poste_FR 
    
    if (grepl("Attaquant", poste) || grepl("FW", poste)) {
      stats_radar <- c("Gls", "Ast", "xG", "Sh", "SoT")
      titre <- "Comparaison Offensive"
    } else if (grepl("Milieu", poste) || grepl("MF", poste)) {
      stats_radar <- c("Cmp", "PrgP", "Ast", "PrgC", "Tkl")
      titre <- "Comparaison Création / Relais"
    } else if (grepl("Défenseur", poste) || grepl("DF", poste)) {
      stats_radar <- c("Tkl", "Int", "Clr", "Blocks", "Cmp")
      titre <- "Comparaison Défensive"
    } else if (grepl("Gardien", poste) || grepl("GK", poste)) {
      stats_radar <- c("Saves", "Save.", "CS", "Cmp", "Min")
      titre <- "Comparaison Gardiens"
    } else {
      stats_radar <- c("Gls", "Ast", "Cmp", "Tkl", "Min") 
      titre <- "Comparaison Générale"
    }
    
    couleur_j1 <- "#2C3E50" 
    couleur_j2 <- "#e74c3c" 
    
    valeurs_j1 <- sapply(stats_radar, function(col) {
      val <- infos[[col]]
      max_val <- max(df[[col]], na.rm = TRUE)
      if (is.na(max_val) || max_val == 0) return(0) else return(val / max_val)
    })
    
    p <- plot_ly(
      type = 'scatterpolar',
      r = c(valeurs_j1, valeurs_j1[1]), 
      theta = c(stats_radar, stats_radar[1]),
      fill = 'toself',
      name = infos$name,
      line = list(color = couleur_j1),
      fillcolor = paste0(couleur_j1, "40")
    )
    
    if (input$activer_comp && !is.null(input$joueur2) && input$joueur2 != "") {
      infos_j2 <- df %>% filter(name == input$joueur2) %>% slice(1)
      valeurs_j2 <- sapply(stats_radar, function(col) {
        val <- infos_j2[[col]]
        max_val <- max(df[[col]], na.rm = TRUE)
        if (is.na(max_val) || max_val == 0) return(0) else return(val / max_val)
      })
      
      p <- p %>% add_trace(
        r = c(valeurs_j2, valeurs_j2[1]),
        theta = c(stats_radar, stats_radar[1]),
        fill = 'toself',
        name = infos_j2$name,
        line = list(color = couleur_j2),
        fillcolor = paste0(couleur_j2, "40") 
      )
    }
    
    p %>% layout(
      title = list(text = paste("<b>", titre, "</b>"), x = 0.5),
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0, 1), showticklabels = FALSE)
      ),
      showlegend = TRUE, 
      margin = list(t = 50)
    )
  })
  
  # --- NUAGE DE POINTS (SCATTER) ---
  output$choix_vars_scatter <- renderUI({
    req(input$joueur1)
    poste <- df %>% filter(name == input$joueur1) %>% pull(Poste_FR) %>% first()
    
    if (grepl("Attaquant", poste) || grepl("FW", poste)) {
      choix <- c("Buts" = "Gls", "Passes Décisives" = "Ast", "Tirs" = "Sh", "Tirs Cadrés" = "SoT", "Expected Goals" = "xG", "Minutes" = "minutes")
    } else if (grepl("Milieu", poste) || grepl("MF", poste)) {
      choix <- c("Passes réussies" = "Cmp", "Passes progressives" = "PrgP", "Passes Décisives" = "Ast", "Tacles" = "Tkl", "Minutes" = "minutes")
    } else if (grepl("Défenseur", poste) || grepl("DF", poste)) {
      choix <- c("Tacles" = "Tkl", "Interceptions" = "Int", "Dégagements" = "Clr", "Contres" = "Blocks", "Minutes" = "minutes")
    } else if (grepl("Gardien", poste) || grepl("GK", poste)) {
      choix <- c("Arrêts" = "arrets", "% Arrêts" = "pct_arrets", "Clean Sheets" = "clean_sheets", "Buts encaissés" = "buts_encaisses", "Minutes" = "minutes")
    } else {
      choix <- c("Buts" = "Gls", "Passes" = "Cmp", "Tacles" = "Tkl", "Minutes" = "minutes")
    }
    
    fluidRow(
      column(6, selectInput("var_x", "📊 Statistique en bas (Axe Horizontal) :", choices = choix, selected = choix[1])),
      column(6, selectInput("var_y", "📈 Statistique à gauche (Axe Vertical) :", choices = choix, selected = choix[2]))
    )
  })
  
  output$scatter_plot <- renderPlotly({
    # On vérifie que le joueur est sélectionné, sinon on affiche un message propre
    
    req(input$joueur1, input$var_x, input$var_y)
    infos_j1 <- df %>% filter(name == input$joueur1) %>% slice(1)
    poste <- infos_j1$Poste_FR
    joueurs_meme_poste <- df %>% filter(Poste_FR == poste)
    
    p <- plot_ly() %>%
      add_trace(
        data = joueurs_meme_poste,
        x = as.formula(paste0("~", input$var_x)),
        y = as.formula(paste0("~", input$var_y)),
        type = 'scatter', mode = 'markers',
        marker = list(color = 'rgba(200, 200, 200, 0.6)', size = 8, line = list(color = 'white', width = 0.5)),
        text = ~paste("<b>", name, "</b><br>", Squad),
        hoverinfo = 'text', name = 'Reste du championnat'
      ) %>%
      add_trace(
        data = infos_j1,
        x = as.formula(paste0("~", input$var_x)),
        y = as.formula(paste0("~", input$var_y)),
        type = 'scatter', mode = 'markers',
        marker = list(color = '#2C3E50', size = 16, line = list(color = 'white', width = 2)), 
        text = ~paste("<b>", name, "</b>"), hoverinfo = 'text', name = infos_j1$name
      )
    
    if (input$activer_comp && !is.null(input$joueur2) && input$joueur2 != "") {
      infos_j2 <- df %>% filter(name == input$joueur2) %>% slice(1)
      p <- p %>% add_trace(
        data = infos_j2,
        x = as.formula(paste0("~", input$var_x)),
        y = as.formula(paste0("~", input$var_y)),
        type = 'scatter', mode = 'markers',
        marker = list(color = '#e74c3c', size = 16, line = list(color = 'white', width = 2)), 
        text = ~paste("<b>", name, "</b>"), hoverinfo = 'text', name = infos_j2$name
      )
    }
    
    p %>% layout(
      xaxis = list(title = input$var_x, zeroline = FALSE),
      yaxis = list(title = input$var_y, zeroline = FALSE),
      hovermode = 'closest',
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, y = -0.15, xanchor = "center") 
    )
  })
  
  # --- INDICATEURS (KPIs) ---
  output$val_joueurs <- renderText({ nrow(data_filtered()) })
  
  output$val_meilleur_ga <- renderText({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return("N/A")
    max(df_f$G.A, na.rm = TRUE)
  })
  
  output$nom_meilleur_ga <- renderText({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return("")
    df_f %>% filter(G.A == max(G.A, na.rm = TRUE)) %>% pull(name) %>% first()
  })
  
  output$val_moyenne <- renderText({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return("N/A")
    paste0(round(mean(df_f$value, na.rm = TRUE) / 1e6, 2), " M€")
  })
  
  output$img_meilleur_ga <- renderUI({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(bs_icon("star-fill")) 
    
    best_player <- df_f %>% filter(G.A == max(G.A, na.rm = TRUE)) %>% slice(1) 
    url_image <- best_player$player_image_url
    
    if(is.null(url_image) || is.na(url_image) || url_image == "") {
      return(bs_icon("star-fill"))
    }
    tags$img(src = url_image, style = "max-height: 100px; width: auto; border-radius: 20px; object-fit: cover; box-shadow: 0 4px 8px rgba(0,0,0,0.2);")
  })
  
  # --- GRAPHIQUES GLOBAUX ---
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
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y = -0.2), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor  = 'rgba(0,0,0,0)', hoverlabel = list(bgcolor = "#1A2E44", font = list(color = "white")))
  })
  
  output$commentaire_efficiency <- renderUI({ 
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(HTML("Aucun joueur ne correspond aux critères de filtrage."))
    
    top_efficiency <- df_f %>% mutate(efficiency = G.A - (xG + xAG)) %>% arrange(desc(efficiency)) %>% head(1)
    
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
  
  output$plot_value <- renderPlotly({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
    top_10 <- df_f %>% arrange(desc(value)) %>% head(10)
    p <- ggplot(top_10, aes(x = reorder(name, value), y = value, fill = Poste_FR,
                            text = paste("Joueur :", name, "<br>Valeur :", round(value / 1e6, 2), "M€<br>Club :", Squad))) +
      geom_bar(stat = "identity") +
      coord_flip() + 
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M€")) +
      labs(x = "", y = "Valeur Marchande", fill = "Poste") +
      theme_minimal() + 
      theme(legend.position = "bottom", text = element_text(color = "#F8F9FA"), axis.text = element_text(color = "#F8F9FA"), panel.grid.major.x = element_line(color = "#1A2E44"), panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y = -0.2), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor  = 'rgba(0,0,0,0)', hoverlabel = list(bgcolor = "#1A2E44", font = list(color = "white")))
  })
  
  output$plot_timeplay <- renderPlotly({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
    top_10_min <- df_f %>% arrange(desc(Min)) %>% head(10)
    p <- ggplot(top_10_min, aes(x = reorder(name, Min), y = Min, fill = Poste_FR,
                                text = paste0("Club : ", Squad, "<br>Âge : ", Age, " ans<br>---------------------<br>Minutes jouées : <b>", Min, " min</b><br>Matchs joués (MP) : ", MP, "<br>Moyenne : <b>", round(Min / MP, 1), "</b> min/match")
    )) +
      geom_bar(stat = "identity") +
      coord_flip() + 
      labs(x = "", y = "Minutes jouées (Min)", fill = "Poste") +
      theme_minimal() + 
      theme(legend.position = "bottom", text = element_text(color = "#F8F9FA"), axis.text = element_text(color = "#F8F9FA"), panel.grid.major.x = element_line(color = "#1A2E44"), panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y = -0.2), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor  = 'rgba(0,0,0,0)', hoverlabel = list(bgcolor = "#1A2E44", font = list(color = "white")))
  })
  
  output$plot_nationalities <- renderPlotly({
    df_f <- data_filtered()
    if(nrow(df_f) == 0 || !"Nation" %in% names(df_f)) return(NULL)
    
    top_nations <- df_f %>% count(Nation, name = "Nombre") %>% arrange(desc(Nombre)) %>% head(10)
    
    p <- ggplot(top_nations, aes(x = reorder(Nation, Nombre), y = Nombre, fill = Nation, text = paste0("<b>", Nation, "</b><br>Joueurs : <b>", Nombre, "</b>"))) +
      geom_bar(stat = "identity", show.legend = FALSE) + 
      coord_flip() + 
      labs(x = "", y = "Nombre de joueurs") +
      theme_minimal() + 
      theme(text = element_text(color = "#F8F9FA"), axis.text = element_text(color = "#F8F9FA"), panel.grid.major.x = element_line(color = "#1A2E44"), panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text") %>% hide_legend() %>% layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor  = 'rgba(0,0,0,0)', hoverlabel = list(bgcolor = "#1A2E44", font = list(color = "white")))
  })
  
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
  
  output$table_data <- renderDT({
    data_filtered() %>%
      select(name, Age, Poste_FR, Squad, Min, G.A, xG.xAG, Tkl.Int, value) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Joueur", "Âge", "Poste", "Équipe", "Minutes", "G+A", "xG+xAG", "Tacles/Int", "Valeur (€)")
      ) %>%
      formatCurrency("value", currency = "€", interval = 3, mark = " ", digits = 0)
  })
}