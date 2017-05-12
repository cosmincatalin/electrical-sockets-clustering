library(shiny)

htmlTemplate("index.html",
             clusterSize = sliderInput("clusters", label = "Clusters", min = 2, max = 15, value = 5),
             clusterMethod = selectInput("method", "Clustering method:", c("Complete" = "complete",
                                                                           "Average" = "average",
                                                                           "Single" = "single",
                                                                           "Centroid" = "centroid"))
)
