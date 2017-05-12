library(shiny)
library(rworldmap)
library(dplyr)

raw <- read.csv("data.csv", stringsAsFactors = FALSE)
data <- raw %>%
  mutate(A = as.numeric(A == 1 | B == 1)) %>%
  mutate(C = as.numeric(C == 1 | D == 1)) %>%
  mutate(E = as.numeric(E == 1 | C == 1 | `F` == 1)) %>%
  mutate(`F` = as.numeric(`F` == 1 | C == 1 | E == 1)) %>%
  mutate(H = as.numeric(H == 1 | C == 1)) %>%
  mutate(J = as.numeric(J == 1 | C == 1)) %>%
  mutate(K = as.numeric(K == 1 | C == 1)) %>%
  mutate(L = as.numeric(L == 1 | C == 1)) %>%
  mutate(N = as.numeric(N == 1 | C == 1)) %>%
  mutate(O = as.numeric(O == 1 | C == 1)) %>%
  mutate( A = A / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( B = B / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( C = C / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( D = D / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( E = E / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( `F` = `F` / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( G = G / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( H = H / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( I = I / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( J = J / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( K = K / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( L = L / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( M = M / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( N = N / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O)) %>%
  mutate( O = O / (A + B + C + D + E + `F` + G + H + I + J + K + L + M + N + O))

distance <- dist(data %>% select(-country))

shinyServer(function(input, output) {
  
  output$worldmap <- renderPlot({
    fit <- hclust(distance, method = input$method)  
    data$cluster <- cutree(fit, k = input$clusters)
    plot_data <- joinCountryData2Map(data,
                                     joinCode = "NAME",
                                     nameJoinColumn = "country",
                                     nameCountryColumn = "country",
                                     verbose = TRUE)
    mapCountryData(plot_data,
                   nameColumnToPlot = "cluster",
                   catMethod = "categorical",
                   colourPalette = "diverging",
                   addLegend = FALSE,
                   mapTitle = "Countries likely to have similar electrical soket configurations")
  })
  
})