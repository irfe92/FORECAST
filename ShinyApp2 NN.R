
# Developpement d'une application web pour la visaulisation des analyses et des prédicitions des séries temporelles 
# de la consommation éléctrique

######Avant de lancer  shinyApp2.R il faut lancer:
# les scripts suivants (base_finale, les scripts synthèses par pays)
# Chargement des données résultent des modèles ARIMA et GAM (IRINA)

# gam_model_list <-get(load("F:/Projet/forecast3/Irina/gam_model_list.rdata"))
# dated_gam_model_list <-get(load("F:/Projet/forecast3/Irina/dated_gam_model_list.rdata"))

# arima_model_list <-get(load("F:/Projet/forecast3/Irina/arima_model_list.rdata"))
# dated_arima_model_list <-get(load("F:/Projet/forecast3/Irina/dated_arima_model_list.rdata"))



########Application de visualisation shiny

library(shiny)
library(shinydashboard)
library(plotly)
library(forecast)
library(DT)

## partie client (utilisatuer) ##

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard - Prévision de la consommation électrique",
                  titleWidth = 800),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #0092ff;
                              font-family: Courier;
                              }
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #0092ff;
                              }'))),
fluidRow(
      column(width = 8,
             
             box(plotlyOutput("forecast_plots"),
                 width = NULL),
             br(),
             box(plotOutput("checkresid_plots"),
                 width = NULL)),
     br(),
      h4("Choix du pays"),      
      column(width = 4,
             box(selectInput("pays", "",
                             c("Pays Bas"="NL",
                               "Belgique" = "BE",
                               "France" = "FR",
                               "Espagne" = "ES",
                               "Angleterre" = "UK",
                               "Allemagne" = "DE"
                             )),
                 width=NULL),
             h4("Choix du modéle de prédiction"),
             box(selectInput("forecast", "",
                             c("XGboost"="XGBOOST",
                                "Random Forest" = "RF",
                                "GAM SP" = "GAM_SP",
                                "SVR" = "svr",
                                "Réseau de neurones" = "NN", 
                                "ARIMA" = "ARIMA",
                                "GAM" = "GAM"
                               )),
                 width=NULL)
              ,
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             box(plotlyOutput("MSE_plots"),
                  width=NULL)

      )
    )
)
)

## Partie serveur ##
server <- function(input, output) {
  
# Graphique de comparaison conso réelle et prédite
output$forecast_plots <- renderPlotly({
  
if (input$pays == "NL") {
        if (input$forecast == "XGBOOST") {
          ggplot() +
            geom_line(aes(x = don.test_NL$Date, y =don.test_NL$Y, colour = 'Réel')) +
            geom_line(aes(x = don.test_NL$Date, y = pred_XGB_NL, colour = 'Prédiction')) +
            scale_color_manual(values = c(
              'Réel' = 'blue',
              'Prédiction' = 'red')) +
            labs(color = 'XGBOOST')+
           # ggtitle('XGboost') +
            xlab('date') +
            ylab('conso')+
            theme_bw()+
            theme(legend.position="top")
        } else if (input$forecast == "RF") {
          ggplot() +
            geom_line(aes(x = don.test_NL$Date, y =don.test_NL$Y, colour = 'Réel')) +
            geom_line(aes(x = don.test_NL$Date, y = pred_RF_NL, colour = 'Prédiction')) +
            scale_color_manual(values = c(
              'Réel' = 'blue',
              'Prédiction' = 'red')) +
            labs(color = 'Random forest')+
            # ggtitle('Random forest Regression') +
            xlab('date') +
            ylab('conso')+
            theme_bw()+
            theme(legend.position="top")
          
        } else if (input$forecast == "GAM_SP") {
         
           ggplot() +
            geom_line(aes(x = don.test_NL$Date, y =Y.test_NL, colour = 'Réel')) +
            geom_line(aes(x = don.test_NL$Date, y = pred_GAM_NL_SP$fit, colour = 'Prédiction')) +
            scale_color_manual(values = c(
              'Réel' = 'blue',
              'Prédiction' = 'red')) +
            labs(color = 'GAM SP')+
            # ggtitle('GAM SP') +
            xlab('date') +
            ylab('conso')+
            theme_bw()+
            theme(legend.position="top")
        
        } else if (input$forecast == "svr") {
          ggplot() +
            geom_line(aes(x = don.test_NL$Date, y =Y.test_NL, colour = 'Réel')) +
            geom_line(aes(x = don.test_NL$Date, y = pred_SVR_NL, colour = 'Prédiction')) +
            scale_color_manual(values = c(
              'Réel' = 'blue',
              'Prédiction' = 'red')) +
            labs(color = 'SVR')+
            # ggtitle('SVR') +
            xlab('date') +
            ylab('conso')+
            theme_bw()+
            theme(legend.position="top")
          
          } else if (input$forecast == "NN") {
            ggplot() +
              geom_line(aes(x = don.test_NL$Date, y =Y.test_NL, colour = 'Réel')) +
              geom_line(aes(x = don.test_NL$Date, y = pred_NN_NL, colour = 'Prédiction')) +
              scale_color_manual(values = c(
                'Réel' = 'blue',
                'Prédiction' = 'red')) +
              labs(color = 'Réseau de neurones')+
              # ggtitle('Réseau de neurones') +
              xlab('date') +
              ylab('conso')+
              theme_bw()+
              theme(legend.position="top")
          
          } else if (input$forecast == "ARIMA") {
          ggplot() +
            geom_line(aes(x = dated_arima_model_list$NL$Date, y =dated_arima_model_list$NL$Conso_test, colour = 'Réel')) +
            geom_line(aes(x = dated_arima_model_list$NL$Date, y = dated_arima_model_list$NL$Fitted, colour = 'Prédiction')) +
            scale_color_manual(values = c(
              'Réel' = 'blue',
              'Prédiction' = 'red')) +
            labs(color = 'ARIMA')+
            # ggtitle('ARIMA') +
            xlab('date') +
            ylab('conso')+
            theme_bw()+
            theme(legend.position="top")
          
        } else if (input$forecast == "GAM") {
          ggplot() +
            geom_line(aes(x = dated_gam_model_list$NL$Date, y =dated_gam_model_list$NL$Conso_test, colour = 'Réel')) +
            geom_line(aes(x = dated_gam_model_list$NL$Date, y = dated_gam_model_list$NL$Fitted, colour = 'Prédiction')) +
            scale_color_manual(values = c(
              'Réel' = 'blue',
              'Prédiction' = 'red')) +
            labs(color = 'GAM IRINA')+
            # ggtitle('ARIMA') +
            xlab('date') +
            ylab('conso')+
            theme_bw()+
            theme(legend.position="top")
        }
  else {}
  }
else if (input$pays == "BE") {
  if (input$forecast == "XGBOOST") {
    ggplot() +
      geom_line(aes(x = don.test_BE$Date, y =don.test_BE$Y, colour = 'Réel')) +
      geom_line(aes(x = don.test_BE$Date, y = pred_XGB_BE, colour = 'Prédiction')) +
      scale_color_manual(values = c(
        'Réel' = 'blue',
        'Prédiction' = 'red')) +
      labs(color = 'XGBOOST')+
      # ggtitle('XGboost') +
      xlab('date') +
      ylab('conso')+
      theme_bw()+
      theme(legend.position="top")
  } else if (input$forecast == "RF") {
    ggplot() +
      geom_line(aes(x = don.test_BE$Date, y =don.test_BE$Y, colour = 'Réel')) +
      geom_line(aes(x = don.test_BE$Date, y = pred_RF_BE, colour = 'Prédiction')) +
      scale_color_manual(values = c(
        'Réel' = 'blue',
        'Prédiction' = 'red')) +
      labs(color = 'Random forest')+
      # ggtitle('Random forest Regression') +
      xlab('date') +
      ylab('conso')+
      theme_bw()+
      theme(legend.position="top")
    
  } else if (input$forecast == "GAM_SP") {
    
    ggplot() +
      geom_line(aes(x = don.test_BE$Date, y =Y.test_BE, colour = 'Réel')) +
      geom_line(aes(x = don.test_BE$Date, y = pred_GAM_BE_SP$fit, colour = 'Prédiction')) +
      scale_color_manual(values = c(
        'Réel' = 'blue',
        'Prédiction' = 'red')) +
      labs(color = 'GAM SP')+
      # ggtitle('GAM SP') +
      xlab('date') +
      ylab('conso')+
      theme_bw()+
      theme(legend.position="top")
    
  } else if (input$forecast == "svr") {
    ggplot() +
      geom_line(aes(x = don.test_BE$Date, y =Y.test_BE, colour = 'Réel')) +
      geom_line(aes(x = don.test_BE$Date, y = pred_SVR_BE, colour = 'Prédiction')) +
      scale_color_manual(values = c(
        'Réel' = 'blue',
        'Prédiction' = 'red')) +
      labs(color = 'SVR')+
      # ggtitle('SVR') +
      xlab('date') +
      ylab('conso')+
      theme_bw()+
      theme(legend.position="top")
    
  } else if (input$forecast == "NN") {
    ggplot() +
      geom_line(aes(x = don.test_BE$Date, y =Y.test_BE, colour = 'Réel')) +
      geom_line(aes(x = don.test_BE$Date, y = pred_NN_BE, colour = 'Prédiction')) +
      scale_color_manual(values = c(
        'Réel' = 'blue',
        'Prédiction' = 'red')) +
      labs(color = 'Réseau de neurones')+
      # ggtitle('Réseau de neurones') +
      xlab('date') +
      ylab('conso')+
      theme_bw()+
      theme(legend.position="top")
    
  } else if (input$forecast == "ARIMA") {
    ggplot() +
      geom_line(aes(x = dated_arima_model_list$BE$Date, y =dated_arima_model_list$BE$Conso_test, colour = 'Réel')) +
      geom_line(aes(x = dated_arima_model_list$BE$Date, y = dated_arima_model_list$BE$Fitted, colour = 'Prédiction')) +
      scale_color_manual(values = c(
        'Réel' = 'blue',
        'Prédiction' = 'red')) +
      labs(color = 'ARIMA')+
      # ggtitle('ARIMA') +
      xlab('date') +
      ylab('conso')+
      theme_bw()+
      theme(legend.position="top")
    
  } else if (input$forecast == "GAM") {
    ggplot() +
      geom_line(aes(x = dated_gam_model_list$BE$Date, y =dated_gam_model_list$BE$Conso_test, colour = 'Réel')) +
      geom_line(aes(x = dated_gam_model_list$BE$Date, y = dated_gam_model_list$BE$Fitted, colour = 'Prédiction')) +
      scale_color_manual(values = c(
        'Réel' = 'blue',
        'Prédiction' = 'red')) +
      labs(color = 'GAM IRINA')+
      # ggtitle('ARIMA') +
      xlab('date') +
      ylab('conso')+
      theme_bw()+
      theme(legend.position="top")
  }
  else {}
}
  else if (input$pays == "FR") {
    if (input$forecast == "XGBOOST") {
      ggplot() +
        geom_line(aes(x = don.test_FR$Date, y =don.test_FR$Y, colour = 'Réel')) +
        geom_line(aes(x = don.test_FR$Date, y = pred_XGB_FR, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'XGBOOST')+
        # ggtitle('XGboost') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
    } else if (input$forecast == "RF") {
      ggplot() +
        geom_line(aes(x = don.test_FR$Date, y =don.test_FR$Y, colour = 'Réel')) +
        geom_line(aes(x = don.test_FR$Date, y = pred_RF_FR, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'Random forest')+
        # ggtitle('Random forest Regression') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "GAM_SP") {
      
      ggplot() +
        geom_line(aes(x = don.test_FR$Date, y =Y.test_FR, colour = 'Réel')) +
        geom_line(aes(x = don.test_FR$Date, y = pred_GAM_FR_SP$fit, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'GAM SP')+
        # ggtitle('GAM SP') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "svr") {
      ggplot() +
        geom_line(aes(x = don.test_FR$Date, y =Y.test_FR, colour = 'Réel')) +
        geom_line(aes(x = don.test_FR$Date, y = pred_SVR_FR, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'SVR')+
        # ggtitle('SVR') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "NN") {
      ggplot() +
        geom_line(aes(x = don.test_FR$Date, y =Y.test_FR, colour = 'Réel')) +
        geom_line(aes(x = don.test_FR$Date, y = pred_NN_FR, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'Réseau de neurones')+
        # ggtitle('Réseau de neurones') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "ARIMA") {
      ggplot() +
        geom_line(aes(x = dated_arima_model_list$FR$Date, y =dated_arima_model_list$FR$Conso_test, colour = 'Réel')) +
        geom_line(aes(x = dated_arima_model_list$FR$Date, y = dated_arima_model_list$FR$Fitted, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'ARIMA')+
        # ggtitle('ARIMA') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "GAM") {
      ggplot() +
        geom_line(aes(x = dated_gam_model_list$FR$Date, y =dated_gam_model_list$FR$Conso_test, colour = 'Réel')) +
        geom_line(aes(x = dated_gam_model_list$FR$Date, y = dated_gam_model_list$FR$Fitted, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'GAM IRINA')+
        # ggtitle('ARIMA') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
    }
    else {}
  }
  else if (input$pays == "ES") {
    if (input$forecast == "XGBOOST") {
      ggplot() +
        geom_line(aes(x = don.test_ES$Date, y =don.test_ES$Y, colour = 'Réel')) +
        geom_line(aes(x = don.test_ES$Date, y = pred_XGB_ES, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'XGBOOST')+
        # ggtitle('XGboost') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
    } else if (input$forecast == "RF") {
      ggplot() +
        geom_line(aes(x = don.test_ES$Date, y =don.test_ES$Y, colour = 'Réel')) +
        geom_line(aes(x = don.test_ES$Date, y = pred_RF_ES, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'Random forest')+
        # ggtitle('Random forest Regression') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "GAM_SP") {
      
      ggplot() +
        geom_line(aes(x = don.test_ES$Date, y =Y.test_ES, colour = 'Réel')) +
        geom_line(aes(x = don.test_ES$Date, y = pred_GAM_ES_SP$fit, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'GAM SP')+
        # ggtitle('GAM SP') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "svr") {
      ggplot() +
        geom_line(aes(x = don.test_ES$Date, y =Y.test_ES, colour = 'Réel')) +
        geom_line(aes(x = don.test_ES$Date, y = pred_SVR_ES, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'SVR')+
        # ggtitle('SVR') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "NN") {
      ggplot() +
        geom_line(aes(x = don.test_ES$Date, y =Y.test_ES, colour = 'Réel')) +
        geom_line(aes(x = don.test_ES$Date, y = pred_NN_ES, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'Réseau de neurones')+
        # ggtitle('Réseau de neurones') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "ARIMA") {
      ggplot() +
        geom_line(aes(x = dated_arima_model_list$ES$Date, y =dated_arima_model_list$ES$Conso_test, colour = 'Réel')) +
        geom_line(aes(x = dated_arima_model_list$ES$Date, y = dated_arima_model_list$ES$Fitted, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'ARIMA')+
        # ggtitle('ARIMA') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "GAM") {
      ggplot() +
        geom_line(aes(x = dated_gam_model_list$ES$Date, y =dated_gam_model_list$ES$Conso_test, colour = 'Réel')) +
        geom_line(aes(x = dated_gam_model_list$ES$Date, y = dated_gam_model_list$ES$Fitted, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'GAM IRINA')+
        # ggtitle('ARIMA') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
    }
    else {}
  }
  else if (input$pays == "UK") {
    if (input$forecast == "XGBOOST") {
      ggplot() +
        geom_line(aes(x = don.test_UK$Date, y =don.test_UK$Y, colour = 'Réel')) +
        geom_line(aes(x = don.test_UK$Date, y = pred_XGB_UK, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'XGBOOST')+
        # ggtitle('XGboost') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
    } else if (input$forecast == "RF") {
      ggplot() +
        geom_line(aes(x = don.test_UK$Date, y =don.test_UK$Y, colour = 'Réel')) +
        geom_line(aes(x = don.test_UK$Date, y = pred_RF_UK, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'Random forest')+
        # ggtitle('Random forest Regression') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "GAM_SP") {
      
      ggplot() +
        geom_line(aes(x = don.test_UK$Date, y =Y.test_UK, colour = 'Réel')) +
        geom_line(aes(x = don.test_UK$Date, y = pred_GAM_UK_SP$fit, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'GAM SP')+
        # ggtitle('GAM SP') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "svr") {
      ggplot() +
        geom_line(aes(x = don.test_UK$Date, y =Y.test_UK, colour = 'Réel')) +
        geom_line(aes(x = don.test_UK$Date, y = pred_SVR_UK, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'SVR')+
        # ggtitle('SVR') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "NN") {
      ggplot() +
        geom_line(aes(x = don.test_UK$Date, y =Y.test_UK, colour = 'Réel')) +
        geom_line(aes(x = don.test_UK$Date, y = pred_NN_UK, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'Réseau de neurones')+
        # ggtitle('Réseau de neurones') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "ARIMA") {
      ggplot() +
        geom_line(aes(x = dated_arima_model_list$UK$Date, y =dated_arima_model_list$UK$Conso_test, colour = 'Réel')) +
        geom_line(aes(x = dated_arima_model_list$UK$Date, y = dated_arima_model_list$UK$Fitted, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'ARIMA')+
        # ggtitle('ARIMA') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "GAM") {
      ggplot() +
        geom_line(aes(x = dated_gam_model_list$UK$Date, y =dated_gam_model_list$UK$Conso_test, colour = 'Réel')) +
        geom_line(aes(x = dated_gam_model_list$UK$Date, y = dated_gam_model_list$UK$Fitted, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'GAM IRINA')+
        # ggtitle('ARIMA') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
    }
    else {}
  }
  else if (input$pays == "DE") {
    if (input$forecast == "XGBOOST") {
      ggplot() +
        geom_line(aes(x = don.test_DE$Date, y =don.test_DE$Y, colour = 'Réel')) +
        geom_line(aes(x = don.test_DE$Date, y = pred_XGB_DE, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'XGBOOST')+
        # ggtitle('XGboost') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
    } else if (input$forecast == "RF") {
      ggplot() +
        geom_line(aes(x = don.test_DE$Date, y =don.test_DE$Y, colour = 'Réel')) +
        geom_line(aes(x = don.test_DE$Date, y = pred_RF_DE, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'Random forest')+
        # ggtitle('Random forest Regression') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "GAM_SP") {
      
      ggplot() +
        geom_line(aes(x = don.test_DE$Date, y =Y.test_DE, colour = 'Réel')) +
        geom_line(aes(x = don.test_DE$Date, y = pred_GAM_DE_SP$fit, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'GAM SP')+
        # ggtitle('GAM SP') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "svr") {
      ggplot() +
        geom_line(aes(x = don.test_DE$Date, y =Y.test_DE, colour = 'Réel')) +
        geom_line(aes(x = don.test_DE$Date, y = pred_SVR_DE, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'SVR')+
        # ggtitle('SVR') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "NN") {
      ggplot() +
        geom_line(aes(x = don.test_DE$Date, y =Y.test_DE, colour = 'Réel')) +
        geom_line(aes(x = don.test_DE$Date, y = pred_NN_DE, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'Réseau de neurones')+
        # ggtitle('Réseau de neurones') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "ARIMA") {
      ggplot() +
        geom_line(aes(x = dated_arima_model_list$DE$Date, y =dated_arima_model_list$DE$Conso_test, colour = 'Réel')) +
        geom_line(aes(x = dated_arima_model_list$DE$Date, y = dated_arima_model_list$DE$Fitted, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'ARIMA')+
        # ggtitle('ARIMA') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
      
    } else if (input$forecast == "GAM") {
      ggplot() +
        geom_line(aes(x = dated_gam_model_list$DE$Date, y =dated_gam_model_list$DE$Conso_test, colour = 'Réel')) +
        geom_line(aes(x = dated_gam_model_list$DE$Date, y = dated_gam_model_list$DE$Fitted, colour = 'Prédiction')) +
        scale_color_manual(values = c(
          'Réel' = 'blue',
          'Prédiction' = 'red')) +
        labs(color = 'GAM IRINA')+
        # ggtitle('ARIMA') +
        xlab('date') +
        ylab('conso')+
        theme_bw()+
        theme(legend.position="top")
    }
    else {}
  }
  
  })
# Affichage des residus 
output$checkresid_plots <- renderPlot({
if (input$pays == "NL") {
        if (input$forecast == "RF") {
            checkresiduals(res_RF_NL)
          }
        else if  (input$forecast == "XGBOOST") {
          checkresiduals(Y.test_NL-pred_XGB_NL)
        }
        else if (input$forecast == "GAM_SP") {
          checkresiduals(GAM_NL_SP)
          }
        else if (input$forecast == "svr") {
            checkresiduals(res_SVR_NL)
        }
        else if (input$forecast == "NN") {
          checkresiduals(res_NN_NL[,1]) 
        }  
        else if (input$forecast == "ARIMA") {
          checkresiduals(dated_arima_model_list$NL$Residuals)
        } 
       else if (input$forecast == "GAM") {
         checkresiduals(dated_gam_model_list$NL$Residuals)
       }  

else{}
}
else if (input$pays == "BE") {
          if (input$forecast == "RF") {
            checkresiduals(res_RF_BE)
          }
          else if  (input$forecast == "XGBOOST") {
            checkresiduals(Y.test_BE-pred_XGB_BE)
          }
          else if (input$forecast == "GAM_SP") {
            checkresiduals(GAM_BE_SP)
          }
          else if (input$forecast == "svr") {
            checkresiduals(res_SVR_BE)
          }
          else if (input$forecast == "NN") {
            checkresiduals(res_NN_BE[,1]) 
          }  
          else if (input$forecast == "ARIMA") {
            checkresiduals(dated_arima_model_list$BE$Residuals)
          } 
          else if (input$forecast == "GAM") {
            checkresiduals(dated_gam_model_list$BE$Residuals)
          }  
          
          else{}
      }
        else if (input$pays == "FR") {
          if (input$forecast == "RF") {
            checkresiduals(res_RF_FR)
          }
          else if  (input$forecast == "XGBOOST") {
            checkresiduals(Y.test_FR-pred_XGB_FR)
          }
          else if (input$forecast == "GAM_SP") {
            checkresiduals(GAM_FR_SP)
          }
          else if (input$forecast == "svr") {
            checkresiduals(res_SVR_FR)
          }
          else if (input$forecast == "NN") {
            checkresiduals(res_NN_FR[,1]) 
          }  
          else if (input$forecast == "ARIMA") {
            checkresiduals(dated_arima_model_list$FR$Residuals)
          } 
          else if (input$forecast == "GAM") {
            checkresiduals(dated_gam_model_list$FR$Residuals)
          }  
          
          else{}
        }
          else if (input$pays == "ES") {
            if (input$forecast == "RF") {
              checkresiduals(res_RF_ES)
            }
            else if  (input$forecast == "XGBOOST") {
              checkresiduals(Y.test_ES-pred_XGB_ES)
            }
            else if (input$forecast == "GAM_SP") {
              checkresiduals(GAM_ES_SP)
            }
            else if (input$forecast == "svr") {
              checkresiduals(res_SVR_ES)
            }
            else if (input$forecast == "NN") {
              checkresiduals(res_NN_ES[,1]) 
            }  
            else if (input$forecast == "ARIMA") {
              checkresiduals(dated_arima_model_list$ES$Residuals)
            } 
            else if (input$forecast == "GAM") {
              checkresiduals(dated_gam_model_list$ES$Residuals)
            }  
            
            else{}
          }
          else if (input$pays == "UK") {
            if (input$forecast == "RF") {
              checkresiduals(res_RF_UK)
            }
            else if  (input$forecast == "XGBOOST") {
              checkresiduals(Y.test_UK-pred_XGB_UK)
            }
            else if (input$forecast == "GAM_SP") {
              checkresiduals(GAM_UK_SP)
            }
            else if (input$forecast == "svr") {
              checkresiduals(res_SVR_UK)
            }
            else if (input$forecast == "NN") {
              checkresiduals(res_NN_UK[,1]) 
            }  
            else if (input$forecast == "ARIMA") {
              checkresiduals(dated_arima_model_list$UK$Residuals)
            } 
            else if (input$forecast == "GAM") {
              checkresiduals(dated_gam_model_list$UK$Residuals)
            }  
            
            else{}
          }
          else if (input$pays == "DE") {
            if (input$forecast == "RF") {
              checkresiduals(res_RF_DE)
            }
            else if  (input$forecast == "XGBOOST") {
              checkresiduals(Y.test_DE-pred_XGB_DE)
            }
            else if (input$forecast == "GAM_SP") {
              checkresiduals(GAM_DE_SP)
            }
            else if (input$forecast == "svr") {
              checkresiduals(res_SVR_DE)
            }
            else if (input$forecast == "NN") {
              checkresiduals(res_NN_DE[,1]) 
            }  
            else if (input$forecast == "ARIMA") {
              checkresiduals(dated_arima_model_list$DE$Residuals)
            } 
            else if (input$forecast == "GAM") {
              checkresiduals(dated_gam_model_list$DE$Residuals)
            }  
            
            else{}
          }
  })


# graphe des MSE

output$MSE_plots <- renderPlotly({
if (input$pays == "NL") {
  
    ##MSE des modèles (hors ARIMA et GAM Irina) 
    x_shiny <- c("RL", "RLI","multi", "P2" ,"SP", "GAM" ,"RF", "NN", "XGB","SVR")
    y_shiny <- c(MSE_RL_NL, MSE_RLI_NL, MSE_RLI_NL_multi, MSE_RLI_NL_P2, MSE_SP_NL, MSE_GAM_NL_SP, MSE_RF_NL, MSE_NN_NL, MSE_XGB_NL, MSE_SVR_NL)
    data_shiny <- data.frame(x_shiny, y_shiny)

    ##MAPE ARIMA et GAM Irina
    x_gam_shiny <- c("FR", "BE","DE", "NL" ,"UK", "ES")
    y_gam_shiny <- c(0.04205393, 0.04068602, 0.05740425, 0.04031844, 0.08889125, 0.03120817)
    gam_shiny <- data.frame(x_gam_shiny, y_gam_shiny)
  
        if (input$forecast == "XGBOOST") {
        library(plotly)
          plot_ly(data_shiny, x = ~x_shiny[x_shiny!="XGB"], y = ~y_shiny[y_shiny!=MSE_XGB_NL], name = 'Autres Modèles', type = 'bar',
                  marker = list(color = 
                                  c('rgba(145,239,51,1)'))) %>%
            layout(title = "MSE des modèles utilisés",
                   xaxis = list(title = "Modèles"),
                   yaxis = list(title = "Valeur MSE"))%>%
            add_bars(data_shiny, x = ~x_shiny[x_shiny=="XGB"], y = ~y_shiny[y_shiny==MSE_XGB_NL], name = 'XGBOOST', type = 'bar',
                     marker = list(color = 
                                     c('rgba(224,0,0,1')))
          
          }
        else if (input$forecast == "RF") {
          
          plot_ly(data_shiny, x = ~x_shiny[x_shiny!="RF"], y = ~y_shiny[y_shiny!=MSE_RF_NL], name = 'Autres Modèles', type = 'bar',
                  marker = list(color = 
                                  c('rgba(145,239,51,1)'))) %>%
            layout(title = "MSE des modèles utilisés",
                   xaxis = list(title = "Modèles"),
                   yaxis = list(title = "Valeur MSE"))%>%
            add_bars(data_shiny, x = ~x_shiny[x_shiny=="RF"], y = ~y_shiny[y_shiny==MSE_RF_NL], name = 'RF', type = 'bar',
                     marker = list(color = 
                                     c('rgba(224,0,0,1')))
            
          }
          else if (input$forecast == "GAM_SP") {
            plot_ly(data_shiny, x = ~x_shiny[x_shiny!="GAM"], y = ~y_shiny[y_shiny!=MSE_GAM_NL_SP], name = 'Autres Modèles', type = 'bar',
                    marker = list(color = 
                                    c('rgba(145,239,51,1)'))) %>%
              layout(title = "MSE des modèles utilisés",
                     xaxis = list(title = "Modèles"),
                     yaxis = list(title = "Valeur MSE"))%>%
              add_bars(data_shiny, x = ~x_shiny[x_shiny=="GAM"], y = ~y_shiny[y_shiny==MSE_GAM_NL_SP], name = 'GAM', type = 'bar',
                       marker = list(color = 
                                       c('rgba(224,0,0,1')))
          }
          else if (input$forecast == "svr") {
            
            plot_ly(data_shiny, x = ~x_shiny[x_shiny!="SVR"], y = ~y_shiny[y_shiny!=MSE_SVR_NL], name = 'Autres Modèles', type = 'bar',
                    marker = list(color = 
                                    c('rgba(145,239,51,1)'))) %>%
              layout(title = "MSE des modèles utilisés",
                     xaxis = list(title = "Modèles"),
                     yaxis = list(title = "Valeur MSE"))%>%
              add_bars(data_shiny, x = ~x_shiny[x_shiny=="SVR"], y = ~y_shiny[y_shiny==MSE_SVR_NL], name = 'SVR', type = 'bar',
                       marker = list(color = 
                                       c('rgba(224,0,0,1')))
          }
          else if (input$forecast == "NN") {
            
            plot_ly(data_shiny, x = ~x_shiny[x_shiny!="NN"], y = ~y_shiny[y_shiny!=MSE_NN_NL], name = 'Autres Modèles', type = 'bar',
                    marker = list(color = 
                                    c('rgba(145,239,51,1)'))) %>%
              layout(title = "MSE des modèles utilisés",
                     xaxis = list(title = "Modèles"),
                     yaxis = list(title = "Valeur MSE"))%>%
              add_bars(data_shiny, x = ~x_shiny[x_shiny=="NN"], y = ~y_shiny[y_shiny==MSE_NN_NL], name = 'NN', type = 'bar',
                       marker = list(color = 
                                       c('rgba(224,0,0,1')))
          }
          else if (input$forecast == "ARIMA") {
            
            plot_ly(arima_shiny, x = ~x_arima_shiny[x_arima_shiny!="NL"], y = ~y_arima_shiny[y_arima_shiny!=3.921389], name = 'Autres Pays', type = 'bar',
                    marker = list(color = 
                                    c('rgba(145,239,51,1)'))) %>%
              layout(title = "MAPE des Pays étudiées",
                     xaxis = list(title = "Pays"),
                     yaxis = list(title = "Valeur MAPE"))%>%
              add_bars(arima_shiny, x = ~x_arima_shiny[x_arima_shiny=="NL"], y = ~y_arima_shiny[y_arima_shiny==3.921389], name = 'Pays Bas', type = 'bar',
                       marker = list(color = 
                                       c('rgba(224,0,0,1')))
          }
        else if (input$forecast == "GAM") {
          
          plot_ly(gam_shiny, x = ~x_gam_shiny[x_gam_shiny!="NL"], y = ~y_gam_shiny[y_gam_shiny!=0.04031844], name = 'Autres Pays', type = 'bar',
                  marker = list(color = 
                                  c('rgba(145,239,51,1)'))) %>%
            layout(title = "MAPE des Pays étudiées",
                   xaxis = list(title = "Pays"),
                   yaxis = list(title = "Valeur MAPE"))%>%
            add_bars(gam_shiny, x = ~x_gam_shiny[x_gam_shiny=="NL"], y = ~y_gam_shiny[y_gam_shiny==0.04031844], name = 'Pays Bas', type = 'bar',
                     marker = list(color = 
                                     c('rgba(224,0,0,1')))
        }
    else{}
}
else if (input$pays == "BE") {
  
  ##MSE des modèles (hors ARIMA et GAM Irina) 
  x_shiny <- c("RL", "RLI","multi", "P2" ,"SP", "GAM" ,"RF", "NN", "XGB","SVR")
  y_shiny <- c(MSE_RL_BE, MSE_RLI_BE, MSE_RLI_BE_multi, MSE_RLI_BE_P2, MSE_SP_BE, MSE_GAM_BE_SP, MSE_RF_BE, MSE_NN_BE, MSE_XGB_BE, MSE_SVR_BE)
  data_shiny <- data.frame(x_shiny, y_shiny)
  
  ##MAPE ARIMA et GAM Irina
  x_gam_shiny <- c("FR", "BE","DE", "NL" ,"UK", "ES")
  y_gam_shiny <- c(0.04205393, 0.04068602, 0.05740425, 0.04031844, 0.08889125, 0.03120817)
  gam_shiny <- data.frame(x_gam_shiny, y_gam_shiny)
  
  x_arima_shiny <- c("FR", "BE","DE", "NL" ,"UK", "ES")
  y_arima_shiny <- c(3.573427, 3.719255, 5.133311, 3.921389, 6.662305, 2.520841)
  arima_shiny <- data.frame(x_arima_shiny, y_arima_shiny)
  
  if (input$forecast == "XGBOOST") {
    library(plotly)
    plot_ly(data_shiny, x = ~x_shiny[x_shiny!="XGB"], y = ~y_shiny[y_shiny!=MSE_XGB_BE], name = 'Autres Modèles', type = 'bar',
            marker = list(color = 
                            c('rgba(145,239,51,1)'))) %>%
      layout(title = "MSE des modèles utilisés",
             xaxis = list(title = "Modèles"),
             yaxis = list(title = "Valeur MSE"))%>%
      add_bars(data_shiny, x = ~x_shiny[x_shiny=="XGB"], y = ~y_shiny[y_shiny==MSE_XGB_BE], name = 'XGBOOST', type = 'bar',
               marker = list(color = 
                               c('rgba(224,0,0,1')))
    
  }
  else if (input$forecast == "RF") {
    
    plot_ly(data_shiny, x = ~x_shiny[x_shiny!="RF"], y = ~y_shiny[y_shiny!=MSE_RF_BE], name = 'Autres Modèles', type = 'bar',
            marker = list(color = 
                            c('rgba(145,239,51,1)'))) %>%
      layout(title = "MSE des modèles utilisés",
             xaxis = list(title = "Modèles"),
             yaxis = list(title = "Valeur MSE"))%>%
      add_bars(data_shiny, x = ~x_shiny[x_shiny=="RF"], y = ~y_shiny[y_shiny==MSE_RF_BE], name = 'RF', type = 'bar',
               marker = list(color = 
                               c('rgba(224,0,0,1')))
    
  }
  else if (input$forecast == "GAM_SP") {
    plot_ly(data_shiny, x = ~x_shiny[x_shiny!="GAM"], y = ~y_shiny[y_shiny!=MSE_GAM_BE_SP], name = 'Autres Modèles', type = 'bar',
            marker = list(color = 
                            c('rgba(145,239,51,1)'))) %>%
      layout(title = "MSE des modèles utilisés",
             xaxis = list(title = "Modèles"),
             yaxis = list(title = "Valeur MSE"))%>%
      add_bars(data_shiny, x = ~x_shiny[x_shiny=="GAM"], y = ~y_shiny[y_shiny==MSE_GAM_BE_SP], name = 'GAM', type = 'bar',
               marker = list(color = 
                               c('rgba(224,0,0,1')))
  }
  else if (input$forecast == "svr") {
    
    plot_ly(data_shiny, x = ~x_shiny[x_shiny!="SVR"], y = ~y_shiny[y_shiny!=MSE_SVR_BE], name = 'Autres Modèles', type = 'bar',
            marker = list(color = 
                            c('rgba(145,239,51,1)'))) %>%
      layout(title = "MSE des modèles utilisés",
             xaxis = list(title = "Modèles"),
             yaxis = list(title = "Valeur MSE"))%>%
      add_bars(data_shiny, x = ~x_shiny[x_shiny=="SVR"], y = ~y_shiny[y_shiny==MSE_SVR_BE], name = 'SVR', type = 'bar',
               marker = list(color = 
                               c('rgba(224,0,0,1')))
  }
  else if (input$forecast == "NN") {
    
    plot_ly(data_shiny, x = ~x_shiny[x_shiny!="NN"], y = ~y_shiny[y_shiny!=MSE_NN_BE], name = 'Autres Modèles', type = 'bar',
            marker = list(color = 
                            c('rgba(145,239,51,1)'))) %>%
      layout(title = "MSE des modèles utilisés",
             xaxis = list(title = "Modèles"),
             yaxis = list(title = "Valeur MSE"))%>%
      add_bars(data_shiny, x = ~x_shiny[x_shiny=="NN"], y = ~y_shiny[y_shiny==MSE_NN_BE], name = 'NN', type = 'bar',
               marker = list(color = 
                               c('rgba(224,0,0,1')))
  }
  else if (input$forecast == "ARIMA") {
    
    plot_ly(arima_shiny, x = ~x_arima_shiny[x_arima_shiny!="BE"], y = ~y_arima_shiny[y_arima_shiny!=3.719255], name = 'Autres Pays', type = 'bar',
            marker = list(color = 
                            c('rgba(145,239,51,1)'))) %>%
      layout(title = "MAPE des Pays étudiées",
             xaxis = list(title = "Pays"),
             yaxis = list(title = "Valeur MAPE"))%>%
      add_bars(arima_shiny, x = ~x_arima_shiny[x_arima_shiny=="BE"], y = ~y_arima_shiny[y_arima_shiny==3.719255], name = 'Belgique', type = 'bar',
               marker = list(color = 
                               c('rgba(224,0,0,1')))
  }
  else if (input$forecast == "GAM") {
    
    plot_ly(gam_shiny, x = ~x_gam_shiny[x_gam_shiny!="BE"], y = ~y_gam_shiny[y_gam_shiny!=0.04068602], name = 'Autres Pays', type = 'bar',
            marker = list(color = 
                            c('rgba(145,239,51,1)'))) %>%
      layout(title = "MAPE des Pays étudiées",
             xaxis = list(title = "Pays"),
             yaxis = list(title = "Valeur MAPE"))%>%
      add_bars(gam_shiny, x = ~x_gam_shiny[x_gam_shiny=="BE"], y = ~y_gam_shiny[y_gam_shiny==0.04068602], name = 'Belgique', type = 'bar',
               marker = list(color = 
                               c('rgba(224,0,0,1')))
  }
  else{}
}
  else if (input$pays == "FR") {
    
    ##MSE des modèles (hors ARIMA et GAM Irina) 
    x_shiny <- c("RL", "RLI","multi", "P2" ,"SP", "GAM" ,"RF", "NN", "XGB","SVR")
    y_shiny <- c(MSE_RL_FR, MSE_RLI_FR, MSE_RLI_FR_multi, MSE_RLI_FR_P2, MSE_SP_FR, MSE_GAM_FR_SP, MSE_RF_FR, MSE_NN_FR, MSE_XGB_FR, MSE_SVR_FR)
    data_shiny <- data.frame(x_shiny, y_shiny)
    
    ##MAPE ARIMA et GAM Irina
    x_gam_shiny <- c("FR", "BE","DE", "NL" ,"UK", "ES")
    y_gam_shiny <- c(0.04205393, 0.04068602, 0.05740425, 0.04031844, 0.08889125, 0.03120817)
    gam_shiny <- data.frame(x_gam_shiny, y_gam_shiny)
    
    x_arima_shiny <- c("FR", "BE","DE", "NL" ,"UK", "ES")
    y_arima_shiny <- c(3.573427, 3.719255, 5.133311, 3.921389, 6.662305, 2.520841)
    arima_shiny <- data.frame(x_arima_shiny, y_arima_shiny)
    
    if (input$forecast == "XGBOOST") {
      library(plotly)
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="XGB"], y = ~y_shiny[y_shiny!=MSE_XGB_FR], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="XGB"], y = ~y_shiny[y_shiny==MSE_XGB_FR], name = 'XGBOOST', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
      
    }
    else if (input$forecast == "RF") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="RF"], y = ~y_shiny[y_shiny!=MSE_RF_FR], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="RF"], y = ~y_shiny[y_shiny==MSE_RF_FR], name = 'RF', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
      
    }
    else if (input$forecast == "GAM_SP") {
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="GAM"], y = ~y_shiny[y_shiny!=MSE_GAM_FR_SP], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="GAM"], y = ~y_shiny[y_shiny==MSE_GAM_FR_SP], name = 'GAM', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "svr") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="SVR"], y = ~y_shiny[y_shiny!=MSE_SVR_FR], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="SVR"], y = ~y_shiny[y_shiny==MSE_SVR_FR], name = 'SVR', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "NN") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="NN"], y = ~y_shiny[y_shiny!=MSE_NN_FR], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="NN"], y = ~y_shiny[y_shiny==MSE_NN_FR], name = 'NN', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "ARIMA") {
      
      plot_ly(arima_shiny, x = ~x_arima_shiny[x_arima_shiny!="FR"], y = ~y_arima_shiny[y_arima_shiny!=3.573427], name = 'Autres Pays', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MAPE des Pays étudiées",
               xaxis = list(title = "Pays"),
               yaxis = list(title = "Valeur MAPE"))%>%
        add_bars(arima_shiny, x = ~x_arima_shiny[x_arima_shiny=="FR"], y = ~y_arima_shiny[y_arima_shiny==3.573427], name = 'France', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "GAM") {
      
      plot_ly(gam_shiny, x = ~x_gam_shiny[x_gam_shiny!="FR"], y = ~y_gam_shiny[y_gam_shiny!=0.04205393], name = 'Autres Pays', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MAPE des Pays étudiées",
               xaxis = list(title = "Pays"),
               yaxis = list(title = "Valeur MAPE"))%>%
        add_bars(gam_shiny, x = ~x_gam_shiny[x_gam_shiny=="FR"], y = ~y_gam_shiny[y_gam_shiny==0.04205393], name = 'France', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else{}
  }
  else if (input$pays == "ES") {
    
    ##MSE des modèles (hors ARIMA et GAM Irina) 
    x_shiny <- c("RL", "RLI","multi", "P2" ,"SP", "GAM" ,"RF", "NN", "XGB","SVR")
    y_shiny <- c(MSE_RL_ES, MSE_RLI_ES, MSE_RLI_ES_multi, MSE_RLI_ES_P2, MSE_SP_ES, MSE_GAM_ES_SP, MSE_RF_ES, MSE_NN_ES, MSE_XGB_ES, MSE_SVR_ES)
    data_shiny <- data.frame(x_shiny, y_shiny)
    
    ##MAPE ARIMA et GAM Irina
    x_gam_shiny <- c("FR", "ES","DE", "NL" ,"UK", "ES")
    y_gam_shiny <- c(0.04205393, 0.04068602, 0.05740425, 0.04031844, 0.08889125, 0.03120817)
    gam_shiny <- data.frame(x_gam_shiny, y_gam_shiny)
    
    x_arima_shiny <- c("FR", "ES","DE", "NL" ,"UK", "ES")
    y_arima_shiny <- c(3.573427, 3.719255, 5.133311, 3.921389, 6.662305, 2.520841)
    arima_shiny <- data.frame(x_arima_shiny, y_arima_shiny)
    
    if (input$forecast == "XGBOOST") {
      library(plotly)
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="XGB"], y = ~y_shiny[y_shiny!=MSE_XGB_ES], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="XGB"], y = ~y_shiny[y_shiny==MSE_XGB_ES], name = 'XGBOOST', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
      
    }
    else if (input$forecast == "RF") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="RF"], y = ~y_shiny[y_shiny!=MSE_RF_ES], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="RF"], y = ~y_shiny[y_shiny==MSE_RF_ES], name = 'RF', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
      
    }
    else if (input$forecast == "GAM_SP") {
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="GAM"], y = ~y_shiny[y_shiny!=MSE_GAM_ES_SP], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="GAM"], y = ~y_shiny[y_shiny==MSE_GAM_ES_SP], name = 'GAM', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "svr") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="SVR"], y = ~y_shiny[y_shiny!=MSE_SVR_ES], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="SVR"], y = ~y_shiny[y_shiny==MSE_SVR_ES], name = 'SVR', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "NN") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="NN"], y = ~y_shiny[y_shiny!=MSE_NN_ES], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="NN"], y = ~y_shiny[y_shiny==MSE_NN_ES], name = 'NN', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "ARIMA") {
      
      plot_ly(arima_shiny, x = ~x_arima_shiny[x_arima_shiny!="ES"], y = ~y_arima_shiny[y_arima_shiny!=3.719255], name = 'Autres Pays', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MAPE des Pays étudiées",
               xaxis = list(title = "Pays"),
               yaxis = list(title = "Valeur MAPE"))%>%
        add_bars(arima_shiny, x = ~x_arima_shiny[x_arima_shiny=="ES"], y = ~y_arima_shiny[y_arima_shiny==3.719255], name = 'Espagne', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "GAM") {
      
      plot_ly(gam_shiny, x = ~x_gam_shiny[x_gam_shiny!="ES"], y = ~y_gam_shiny[y_gam_shiny!=0.04068602], name = 'Autres Pays', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MAPE des Pays étudiées",
               xaxis = list(title = "Pays"),
               yaxis = list(title = "Valeur MAPE"))%>%
        add_bars(gam_shiny, x = ~x_gam_shiny[x_gam_shiny=="ES"], y = ~y_gam_shiny[y_gam_shiny==0.04068602], name = 'Espagne', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else{}
  }
  else if (input$pays == "UK") {
    
    ##MSE des modèles (hors ARIMA et GAM Irina) 
    x_shiny <- c("RL", "RLI","multi", "P2" ,"SP", "GAM" ,"RF", "NN", "XGB","SVR")
    y_shiny <- c(MSE_RL_UK, MSE_RLI_UK, MSE_RLI_UK_multi, MSE_RLI_UK_P2, MSE_SP_UK, MSE_GAM_UK_SP, MSE_RF_UK, MSE_NN_UK, MSE_XGB_UK, MSE_SVR_UK)
    data_shiny <- data.frame(x_shiny, y_shiny)
    
    ##MAPE ARIMA et GAM Irina
    x_gam_shiny <- c("FR", "ES","DE", "NL" ,"UK", "ES")
    y_gam_shiny <- c(0.04205393, 0.04068602, 0.05740425, 0.04031844, 0.08889125, 0.03120817)
    gam_shiny <- data.frame(x_gam_shiny, y_gam_shiny)
    
    x_arima_shiny <- c("FR", "ES","DE", "NL" ,"UK", "ES")
    y_arima_shiny <- c(3.573427, 3.719255, 5.133311, 3.921389, 6.662305, 2.520841)
    arima_shiny <- data.frame(x_arima_shiny, y_arima_shiny)
    
    if (input$forecast == "XGBOOST") {
      library(plotly)
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="XGB"], y = ~y_shiny[y_shiny!=MSE_XGB_UK], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="XGB"], y = ~y_shiny[y_shiny==MSE_XGB_UK], name = 'XGBOOST', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
      
    }
    else if (input$forecast == "RF") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="RF"], y = ~y_shiny[y_shiny!=MSE_RF_UK], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="RF"], y = ~y_shiny[y_shiny==MSE_RF_UK], name = 'RF', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
      
    }
    else if (input$forecast == "GAM_SP") {
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="GAM"], y = ~y_shiny[y_shiny!=MSE_GAM_UK_SP], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="GAM"], y = ~y_shiny[y_shiny==MSE_GAM_UK_SP], name = 'GAM', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "svr") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="SVR"], y = ~y_shiny[y_shiny!=MSE_SVR_UK], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="SVR"], y = ~y_shiny[y_shiny==MSE_SVR_UK], name = 'SVR', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "NN") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="NN"], y = ~y_shiny[y_shiny!=MSE_NN_UK], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="NN"], y = ~y_shiny[y_shiny==MSE_NN_UK], name = 'NN', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "ARIMA") {
      
      plot_ly(arima_shiny, x = ~x_arima_shiny[x_arima_shiny!="UK"], y = ~y_arima_shiny[y_arima_shiny!=6.662305], name = 'Autres Pays', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MAPE des Pays étudiées",
               xaxis = list(title = "Pays"),
               yaxis = list(title = "Valeur MAPE"))%>%
        add_bars(arima_shiny, x = ~x_arima_shiny[x_arima_shiny=="UK"], y = ~y_arima_shiny[y_arima_shiny==6.662305], name = 'Angleterre', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "GAM") {
      
      plot_ly(gam_shiny, x = ~x_gam_shiny[x_gam_shiny!="UK"], y = ~y_gam_shiny[y_gam_shiny!=0.08889125], name = 'Autres Pays', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MAPE des Pays étudiées",
               xaxis = list(title = "Pays"),
               yaxis = list(title = "Valeur MAPE"))%>%
        add_bars(gam_shiny, x = ~x_gam_shiny[x_gam_shiny=="UK"], y = ~y_gam_shiny[y_gam_shiny==0.08889125], name = 'Angleterre', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else{}
  }

  else if (input$pays == "DE") {
    
    ##MSE des modèles (hors ARIMA et GAM Irina) 
    x_shiny <- c("RL", "RLI","multi", "P2" ,"SP", "GAM" ,"RF", "NN", "XGB","SVR")
    y_shiny <- c(MSE_RL_DE, MSE_RLI_DE, MSE_RLI_DE_multi, MSE_RLI_DE_P2, MSE_SP_DE, MSE_GAM_DE_SP, MSE_RF_DE, MSE_NN_DE, MSE_XGB_DE, MSE_SVR_DE)
    data_shiny <- data.frame(x_shiny, y_shiny)
    
    ##MAPE ARIMA et GAM Irina
    x_gam_shiny <- c("FR", "BE","DE", "NL" ,"UK", "ES")
    y_gam_shiny <- c(0.04205393, 0.04068602, 0.05740425, 0.04031844, 0.08889125, 0.03120817)
    gam_shiny <- data.frame(x_gam_shiny, y_gam_shiny)
    
    x_arima_shiny <- c("FR", "BE","DE", "NL" ,"UK", "ES")
    y_arima_shiny <- c(3.573427, 3.719255, 5.133311, 3.921389, 6.662305, 2.520841)
    arima_shiny <- data.frame(x_arima_shiny, y_arima_shiny)
    
    if (input$forecast == "XGBOOST") {
      library(plotly)
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="XGB"], y = ~y_shiny[y_shiny!=MSE_XGB_DE], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="XGB"], y = ~y_shiny[y_shiny==MSE_XGB_DE], name = 'XGBOOST', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
      
    }
    else if (input$forecast == "RF") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="RF"], y = ~y_shiny[y_shiny!=MSE_RF_DE], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="RF"], y = ~y_shiny[y_shiny==MSE_RF_DE], name = 'RF', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
      
    }
    else if (input$forecast == "GAM_SP") {
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="GAM"], y = ~y_shiny[y_shiny!=MSE_GAM_DE_SP], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="GAM"], y = ~y_shiny[y_shiny==MSE_GAM_DE_SP], name = 'GAM', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "svr") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="SVR"], y = ~y_shiny[y_shiny!=MSE_SVR_DE], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="SVR"], y = ~y_shiny[y_shiny==MSE_SVR_DE], name = 'SVR', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "NN") {
      
      plot_ly(data_shiny, x = ~x_shiny[x_shiny!="NN"], y = ~y_shiny[y_shiny!=MSE_NN_DE], name = 'Autres Modèles', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MSE des modèles utilisés",
               xaxis = list(title = "Modèles"),
               yaxis = list(title = "Valeur MSE"))%>%
        add_bars(data_shiny, x = ~x_shiny[x_shiny=="NN"], y = ~y_shiny[y_shiny==MSE_NN_DE], name = 'NN', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "ARIMA") {
      
      plot_ly(arima_shiny, x = ~x_arima_shiny[x_arima_shiny!="DE"], y = ~y_arima_shiny[y_arima_shiny!=3.719255], name = 'Autres Pays', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MAPE des Pays étudiées",
               xaxis = list(title = "Pays"),
               yaxis = list(title = "Valeur MAPE"))%>%
        add_bars(arima_shiny, x = ~x_arima_shiny[x_arima_shiny=="DE"], y = ~y_arima_shiny[y_arima_shiny==3.719255], name = 'Allemagne', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else if (input$forecast == "GAM") {
      
      plot_ly(gam_shiny, x = ~x_gam_shiny[x_gam_shiny!="DE"], y = ~y_gam_shiny[y_gam_shiny!=0.05740425], name = 'Autres Pays', type = 'bar',
              marker = list(color = 
                              c('rgba(145,239,51,1)'))) %>%
        layout(title = "MAPE des Pays étudiées",
               xaxis = list(title = "Pays"),
               yaxis = list(title = "Valeur MAPE"))%>%
        add_bars(gam_shiny, x = ~x_gam_shiny[x_gam_shiny=="DE"], y = ~y_gam_shiny[y_gam_shiny==0.05740425], name = 'Allemagne', type = 'bar',
                 marker = list(color = 
                                 c('rgba(224,0,0,1')))
    }
    else{}
  }
  
  
  
    }
)


# output$summaryRF = DT::renderDataTable({
#     if (input$forecast == "RF") {
#       summaryRF <- summary(rf_BE)
#       t(summaryRF)
#     }
#     })

}

shinyApp(ui, server)
