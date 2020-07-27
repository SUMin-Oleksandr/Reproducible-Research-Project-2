sapply(c("ggplot2", "R.utils", "dplyr", "grid"), 
       function(x) { 
           if(!require(x, character.only = TRUE)) install.packages(x) 
           library(x, character.only = TRUE)
           TRUE })

multiplot <- function(..., cols = 1) {
    plots <- list(...)
    numPlots = length(plots)
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                     ncol = cols, nrow = ceiling(numPlots / cols))
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
    }
}

if(!file.exists("data.csv")) {
    download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                  destfile = "data.csv.bz2")
    bunzip2("data.csv.bz2")
}

data <- read.csv("data.csv", stringsAsFactors = FALSE)

HealthDamage <- data %>% select(EVTYPE, FATALITIES, INJURIES)
EconomicDamage <- data %>% select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

dict <- c("B" = 10^9,
          "M" = 10^6,
          "K" = 10^3,
          "-" = 0)

EconomicDamage$PROPDMGEXP[!(EconomicDamage$PROPDMGEXP %in% c("B", "M", "K"))] <- "-"
EconomicDamage$PROPDMG <- EconomicDamage$PROPDMG * unname(dict[EconomicDamage$PROPDMGEXP])
EconomicDamage$CROPDMGEXP[!(EconomicDamage$CROPDMGEXP %in% c("B", "M", "K"))] <- "-"
EconomicDamage$CROPDMG <- EconomicDamage$CROPDMG * unname(dict[EconomicDamage$CROPDMGEXP])
EconomicDamage[, c("PROPDMGEXP", "CROPDMGEXP")] <- list(NULL)

total_FATALITIES <- aggregate(FATALITIES ~ EVTYPE, HealthDamage, sum)
total_FATALITIES <- total_FATALITIES[order(-total_FATALITIES$FATALITIES), ][c(1:25), ]
total_INJURIES <- aggregate(INJURIES ~ EVTYPE, HealthDamage, sum)
total_INJURIES <- total_INJURIES[order(-total_INJURIES$INJURIES), ][c(1:25), ]

total_EconomicDamage <- aggregate(PROPDMG ~ EVTYPE, EconomicDamage, sum)
total_EconomicDamage$CROPDMG <- aggregate(CROPDMG ~ EVTYPE, EconomicDamage, sum)$CROPDMG
total_EconomicDamage$Total <- total_EconomicDamage$PROPDMG + total_EconomicDamage$CROPDMG

plot1 <- ggplot(total_FATALITIES, aes(EVTYPE, FATALITIES)) +
    geom_col(fill = "#770000") +
    labs(x = "Event Type", y = "Number of Fatalities",
         title = "Total Number of Fatalities by Event Type") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10), 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))

plot2 <- ggplot(total_INJURIES, aes(EVTYPE, INJURIES)) +
    geom_col(fill = "#FFEE00") +
    labs(x = "Event Type", y = "Number of Injuries",
         title = "Total Number of Injuries by Event Type") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10), 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))

figure1 <- multiplot(plot1, plot2, cols = 2)

plot3 <- ggplot(total_EconomicDamage[order(-total_EconomicDamage$PROPDMG), ][c(1:25), ], aes(EVTYPE, PROPDMG / 10^9)) +
    geom_col() +
    labs(x = "Event Type", y = "Cost in PROP (Billion Dollars)",
         title = "Total Cost in PROP by Event Type (Billion Dollars)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 8), 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))    

plot4 <- ggplot(total_EconomicDamage[order(-total_EconomicDamage$CROPDMG), ][c(1:25), ], aes(EVTYPE, CROPDMG / 10^9)) +
    geom_col() +
    labs(x = "Event Type", y = "Cost in CROP (Billion Dollars)",
         title = "Total Cost in CROP by Event Type (Billion Dollars)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 8), 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7)) 

figure2 <- multiplot(plot3, plot4, cols = 2)

figure3 <- ggplot(total_EconomicDamage[order(-total_EconomicDamage$Total), ][c(1:25), ], aes(EVTYPE, Total / 10^9)) +
    geom_col() +
    labs(x = "Event Type", y = "Cost in PROP & CROP (Billion Dollars)",
         title = "Total Cost in PROP & CROP by Event Type (Billion Dollars)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10), 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))
