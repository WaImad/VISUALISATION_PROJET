# --- server.R ---
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(scales)

# 1. CHARGEMENT DES DONNÉES (au démarrage du serveur)
df <- read.csv("data/df.csv", stringsAsFactors = FALSE)

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
  output$plot_efficiency <- renderPlot({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
    ggplot(df_f, aes(x = xG.xAG, y = G.A, color = Pos)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_text(data = df_f %>% filter(G.A >= quantile(G.A, 0.9, na.rm=TRUE)), 
                aes(label = name), vjust = -1, size = 3, check_overlap = TRUE) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      labs(x = "Expected Goals + Expected Assists (xG.xAG)", 
           y = "Buts + Passes Décisives réels (G.A)", color = "Poste") +
      theme_minimal() + theme(legend.position = "bottom")
  })
  
  output$plot_value <- renderPlot({
    df_f <- data_filtered()
    if(nrow(df_f) == 0) return(NULL)
    
    top_10 <- df_f %>% arrange(desc(value)) %>% head(10)
    
    ggplot(top_10, aes(x = reorder(name, value), y = value, fill = Pos)) +
      geom_bar(stat = "identity") +
      coord_flip() + 
      geom_text(aes(label = paste0(round(value / 1e6, 2), " M€")), hjust = -0.1, size = 3) +
      scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M€")) +
      labs(x = "", y = "Valeur Marchande", fill = "Poste") +
      theme_minimal() + theme(legend.position = "bottom")
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