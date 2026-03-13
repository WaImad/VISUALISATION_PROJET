# --- server.R ---
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(scales)

# 1. CHARGEMENT DES DONNÉES (au démarrage du serveur)
df <- read.csv("data/dataset_stat_large.csv", stringsAsFactors = FALSE)
df$age_class <- cut(df$Age,4)

function(input, output, session) {
  
  # 2. MISE À JOUR DYNAMIQUE DE L'INTERFACE
  # On envoie les vraies données de df aux filtres de ui.R
  observe({
    updateSelectInput(session, "squad", choices = c("Toutes", sort(unique(df$Squad))))
    updateCheckboxGroupInput(session, "position", choices = unique(df$Pos), selected = unique(df$Pos))
    updateSliderInput(session, "min_minutes", max = max(df$Min, na.rm = TRUE))
  })
  
  # 3. CRÉATION DU DATASET FILTRÉ
  data_filtered <- reactive({
    # req() s'assure que les filtres sont bien mis à jour avant de faire les calculs
    req(input$position, input$squad) 
    
    data <- df %>%
      filter(Pos %in% input$position) %>%
      filter(Min >= input$min_minutes)
    
    if (input$squad != "Toutes") {
      data <- data %>% filter(Squad == input$squad)
    }
    
    return(data)
  })
  
  # --- 4. KPIs ---
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
  
  # --- 5. GRAPHIQUES ---
  # -- GRAPHIQUE XG vs XG + XAG
  output$plot_efficiency <- renderPlotly({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
    p <- ggplot(df_f, aes(x = (xG + xAG), y = G.A, color = Pos, 
                          # l'infobulle personnalisée (en HTML)
                          text = paste0(
                            "<b>", name, "</b><br>",
                            "Club : ", Squad, "<br>",
                            "Temps de jeu : ", Min, " min<br>",
                            "---------------------<br>",
                            "G+A (Réel) : <b>", G.A, "</b><br>",
                            "xG+xAG (Attendu) : <b>", (xG + xAG), "</b><br>",
                            "Différence (Sur-performance) : ", round(G.A - (xG + xAG), 2)
                          ))) +
      geom_point(size = 3, alpha = 0.8) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#FF1744") +
      labs(x = "Expected Goals + Expected Assists (xG + xAG)", 
           y = "Buts + Passes Décisives réels (G.A)", color = "Poste") +
      theme_minimal() + 
      theme(
        legend.position = "bottom",
        text = element_text(color = "#F8F9FA"),   
        axis.text = element_text(color = "#F8F9FA"),  
        panel.grid.major = element_line(color = "#1A2E44"),
        panel.grid.minor = element_blank()
      )
    

    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.2), # Légende horizontale en bas
        paper_bgcolor = 'rgba(0,0,0,0)', # Fond transparent
        plot_bgcolor  = 'rgba(0,0,0,0)', # Fond transparent
        hoverlabel = list(bgcolor = "#1A2E44", font = list(color = "white")) # Couleur de l'infobulle
      )
  })
  
  output$commentaire_efficiency <- renderText({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return("Aucun joueur ne correspond aux critères de filtrage.")
    
    top_efficiency <- df_f %>%
      mutate(efficiency = G.A - (xG + xAG)) %>%
      arrange(desc(efficiency)) %>%
      head(1)
    
    paste0(
      "Les joueurs au dessus de la droite sont ceux qui sont très efficace, ils marquent plus de buts qu'espérer.
      Au contraire les joueurs en dessous de la droite sont des joueurs en sous-efficacité, ils marquent moins de but qu'attendus ",
      "Le joueur le plus efficient est <b>", top_efficiency$name, "</b> avec une sur-performance de <b>", 
      round(top_efficiency$efficiency, 2), "</b> (G.A : ", top_efficiency$G.A, " vs xG+xAG : ", round(top_efficiency$xG + top_efficiency$xAG, 2), ")."
    )
  })
  
  # -- GRAPHIQUE TOP 10 VALUE
  output$plot_value <- renderPlotly({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
    top_10 <- df_f %>% arrange(desc(value)) %>% head(10)
    
    p <- ggplot(top_10, aes(x = reorder(name, value), y = value, fill = Pos,
                            # Ajout du texte personnalisé pour l'infobulle (tooltip)
                            text = paste("Joueur :", name, "<br>Valeur :", round(value / 1e6, 2), "M€","Club :", Squad))) +
      geom_bar(stat = "identity") +
      coord_flip() + 
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M€")) +
      labs(x = "", y = "Valeur Marchande", fill = "Poste") +
      theme_minimal() + 
      theme(
        legend.position = "bottom",
        text = element_text(color = "#F8F9FA"),
        axis.text = element_text(color = "#F8F9FA"),
        panel.grid.major.x = element_line(color = "#1A2E44"),
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.2),
        paper_bgcolor = 'rgba(0,0,0,0)', 
        plot_bgcolor  = 'rgba(0,0,0,0)',
        hoverlabel = list(bgcolor = "#1A2E44", font = list(color = "white"))
      )
  
  })
  
  
  # -- GRAPHIQUE TEMPS DE JEU
  output$plot_timeplay <- renderPlotly({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
    # On isole le Top 10 des joueurs avec le plus de minutes (Min)
    top_10_min <- df_f %>% 
      arrange(desc(Min)) %>% 
      head(10)
    
    # Création du graphique
    p <- ggplot(top_10_min, aes(
      x = reorder(name, Min), 
      y = Min, 
      fill = Pos,
      # Infobulle HTML personnalisée
      text = paste0(
        "Club : ", Squad, "<br>",
        "Âge : ", Age, " ans<br>",
        "---------------------<br>",
        "Minutes jouées : <b>", Min, " min</b><br>",
        "Matchs joués (MP) : ", MP, "<br>",
        "Moyenne : <b>", round(Min / MP, 1), "</b> min/match"
      )
    )) +
      geom_bar(stat = "identity") +
      coord_flip() + 
      labs(x = "", y = "Minutes jouées (Min)", fill = "Poste") +
      theme_minimal() + 
      theme(
        legend.position = "bottom",
        text = element_text(color = "#F8F9FA"),
        axis.text = element_text(color = "#F8F9FA"),
        panel.grid.major.x = element_line(color = "#1A2E44"),
        panel.grid.major.y = element_blank()
      )
    
    # Conversion en graphique interactif Plotly
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.2),
        paper_bgcolor = 'rgba(0,0,0,0)', 
        plot_bgcolor  = 'rgba(0,0,0,0)',
        hoverlabel = list(bgcolor = "#1A2E44", font = list(color = "white"))
      )
  })
  
  output$plot_age_value <- renderPlot({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
      ggplot(df_f, aes(x = age_class, y = value / 1000000, fill = age_class)) +
      geom_boxplot(outlier.colour = "red", outlier.alpha = 0.6) +
      scale_fill_brewer(palette = "Blues") +
      labs(
        title = "Valeur Marchande selon la Tranche d'Âge",
        subtitle = "Cycle de vie économique d'un joueur",
        x = "Tranche d'âge",
        y = "Valeur Marchande (en Millions d'€)",
        fill = "Âge"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none"
      )
  })
  
  

  # --- 6. TABLEAU INTERACTIF ---
  output$table_data <- renderDT({
    data_filtered() %>%
      select(name, Age, Pos, Squad, Min, G.A, xG.xAG, Tkl.Int, value) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        colnames = c("Joueur", "Âge", "Poste", "Équipe", "Minutes", "G+A", "xG+xAG", "Tacles/Int", "Valeur (€)")
      ) %>%
      formatCurrency("value", currency = "€", interval = 3, mark = " ", digits = 0)
  })
}