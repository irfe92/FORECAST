#
# Developpement d'une application web pour la visaulisation des analyses et des prédicitions des séries temporelles 
# de la consommation éléctrique

#
# Developpement d'une application web pour la visaulisation des analyses et des prédicitions des séries temporelles 
# de la consommation éléctrique
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

# Create test set
#sp500_test <- window(sp500, 2015, c(2015, 12))

#fit_RF <- merge(test$Date, rf_BE_pred)
# fit_BC <- readRDS(here("models", 'box_cox.rds'))
# fit_net <- readRDS(here("models", 'neural_net.rds'))
# fit_meanf <- readRDS(here("models", 'meanf.rds'))
# fit_naive <- readRDS(here("models", 'naive.rds'))
# fit_snaive <- readRDS(here("models", 'snaive.rds'))
# fit_ets <- readRDS(here("models", 'ets.rds'))

## app.R ##

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard - Prévision de la consommation d'électricité",
                  titleWidth = 700),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #006b6f;
                              font-family: Courier;
                              }
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #00868B;
                              }'))),
    # mainPanel(
    #   plotlyOutput("forecast_plots_RF", height = "900")
    # )
fluidRow(
      column(width = 8,
             box(plotlyOutput("forecast_plots"),
                 width = NULL),
             box(plotOutput("checkresid_plots"),
                 width = NULL)),
      
      column(width = 4,
             box(selectInput("forecast", "Choix du modéle de prédiction:",
                             c("Random Forest" = "RF",
                                "GAM" = "GAM"
                               #,
                               # "ARIMA" = "ARIMA",
                               # "Mean Forecasting" = "fit_meanf",
                               # "Naive Forecasting" = "fit_naive",
                               # "Seasonal Naive Forecasting" = "fit_snaive",
                               # "Neural Networks" = "fit_net"
                             )),
                 width=NULL)
              ,

             box(verbatimTextOutput('test_print'),
                 width=NULL),
             box(plotOutput("MSE_plots"),
                  width=NULL)

      )
    )
)
)
server <- function(input, output) {
  
# Graphique de comparaison conso réelle et prédite
output$forecast_plots <- renderPlotly({
     if (input$forecast == "RF") {
       ggplot() +
         geom_line(aes(x = don.test$Date, y = Y.test),
                   colour = 'red') +
         geom_line(aes(x = don.test$Date, y = RF_NL_fin_pred),
                   colour = 'blue') +
         ggtitle('Random Forest Regression, en bleu prédiction') +
         xlab('date') +
         ylab('conso')
     } else if (input$forecast == "GAM") {
       ggplot() +
         geom_line(aes(x = don.test$Date, y = Y.test),
                   colour = 'red') +
         geom_line(aes(x = don.test$Date, y = pred_GAM_NL$fit),
                   colour = 'yellow') +
         ggtitle('GAM, en jaune prédiction') +
         xlab('date') +
         ylab('conso')
      } 
   
  } )
# Affichage des residus
output$checkresid_plots <- renderPlot({
    if (input$forecast == "RF") {
      checkresiduals(res_RF_NL)
    }
    else if (input$forecast == "GAM") {
      checkresiduals(GAM_NL)
    }
  
  })


# graphe des MSE

output$MSE_plots <- renderPlot({
  if (input$forecast == "RF") {
    barplot(MSE_NL_tot, xlab="modèles", ylab="MSE", main="MSE des modèles",las=0)
    axis(1, labels=c("RL", "RLI","multi", "P2" ,"Poly" ,"SPLINE", "GAM", "RF", "SVR", "NN"), at = graph)
  }
  else if (input$forecast == "GAM") {
    barplot(MSE_NL_tot, xlab="modèles", ylab="MSE", main="MSE des modèles",las=0)
    axis(1, labels=c("RL", "RLI","multi", "P2" ,"Poly" ,"SPLINE", "GAM", "RF", "SVR", "NN"), at = graph)
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
