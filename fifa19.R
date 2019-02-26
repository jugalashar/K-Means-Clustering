rm(list = ls(all= TRUE))

fifa19 <- read.csv("C:/Users/pc/Desktop/data.csv")

#Uploading libraries that will be used throughout this analysis.
library(tidyverse)
#install.packages("janitor")
#install.packages("stringr")
library(stringr)
library(janitor)
library(caret)
library(e1071)
#install.packages("plotly")
library(plotly)

#Overall summary of the original dataset to quickly check how data is distributed and check for NA 
#values, etc.
summary(fifa19)

#Deleting rows that contain NA values. 
fifa19 <- fifa19 %>%
      na.omit()

#Checking data types of all columns in the data set
str(fifa19)

#Excluding features from the original dataset that are not going to help me in my analysis.
removeUrl <- fifa19 %>%
        select(-c(ID,Photo,Flag,Club.Logo,Release.Clause))


#Extracting only the Transfer Value and Wage of players in digits.
TransferValue <- str_extract(removeUrl$Value, "\\d+\\.*\\d*")
Wages  <- str_extract(removeUrl$Wage, "\\d+\\.*\\d*")
TransferValue <- as.numeric(TransferValue)
Wages <- as.numeric(Wages)

#Creating a function to standardize Wage to Transfer Value unit.
fn <- function(x){
  x <- x/10^3
  return(x)
} 

standardizeWages <- sapply(Wages, fn)
Wages <- standardizeWages

removeUrl <- cbind(removeUrl,TransferValue)
removeUrl <- cbind(removeUrl, Wages)

#Removing the original Transfer value and Wage features having character datatype
fifa19 <- removeUrl %>%
  select(-c(Value, Wage))

#Positions have some string values, which arent required. Eliminating those and converting data
#type to numeric.
fifa19$CB <- gsub("\\+.*","",fifa19$CB)
fifa19$CB <- as.numeric(fifa19$CB)

fifa19$RW <- gsub("\\+.*","",fifa19$RW)
fifa19$RW <- as.numeric(fifa19$RW)

#Some rows where the Position column has 'GK' have blanks. Therefore removing those too.
fifa19 <- fifa19 %>%
  filter(Position!='GK')

#Adding an additional column that depicts the scope of improvement of a player.
TopClubs <- fifa19 %>%
              mutate(Scope = Potential - Overall)
    
#subsetting dataset to specific needs for analysis.
 TopClubs <- TopClubs[, -c(1)]
 TopClubs <- TopClubs %>%
            filter(CB > 75 | RW > 75)
 
 #selecting only numeric features.
 topNumeric <- TopClubs %>%
              select_if(is.numeric)
 
 topNumeric <- topNumeric %>%
            select(- c(starts_with("GK"), Jersey.Number, Special))
 
 #scaling all neumeric attributes.
 scaledAttributes <- scale(topNumeric)
 
 wss <- 0
 # For 1 to 30 cluster centers
 for (j in 1:30) {
   km.out <- kmeans(scaledAttributes, centers = j, nstart = 20)
   # Save total within sum of squares to wss variable
   wss[j] <- km.out$tot.withinss
 }
 
 wss_df <- data.frame(num_cluster = 1:30, wgss = wss)
 
 #plotting elbow point to decide no.of clusters to use.
 ggplot(data = wss_df, aes(x=num_cluster, y= wgss)) + 
   geom_line(color = "grey", size = 2) + 
   geom_point(color = "blue", size = 2) +
   geom_curve(x=14, xend=8, y=300000, yend= 290500, arrow = arrow(length = unit(0.2,"cm")), size =1, colour = "purple") +
   geom_text(label = "k = 7\noptimal level", x=14, y= 290000, colour = "purple") +
   labs(title = "Using Seven Clusters To Group Players", subtitle = "Selecting the point where the elbow 'bends'")
 
 wisc.km <- kmeans(scale(topNumeric), centers = 7, nstart = 20)
 
 #Adding distance metric of clusters to the subsetted data frame used for analysis.
 topNumeric <- topNumeric %>%
            mutate(Cluster = wisc.km$cluster)
 
 TopClubs <- cbind(TopClubs, topNumeric$Cluster)
 
 #Making changes to Position column to avoid complexity.
 TopClubs$Position <- str_replace(TopClubs$Position, "LCB|RCB", "CB")
 TopClubs$Position <- as.factor(TopClubs$Position)
 
 #selecting necessary variables for performing cluster analysis.
 clusterAnalysis <- TopClubs %>%
   select(Name, Club, Age, Position, Overall, `topNumeric$Cluster`, TransferValue, Scope)
 
 #Making a table that stores details of how data is organized into the 7 clusters.
 table(clusterAnalysis$`topNumeric$Cluster`, clusterAnalysis$TransferValue)
 
 
 cluster_arranged <- clusterAnalysis %>%
   mutate(Cluster = as.character(clusterAnalysis$`topNumeric$Cluster`)) %>%
   arrange(desc(Overall)) %>% 
   group_by(Cluster)
 
 #Visualizing the K-means cluster through GGplot and plotly libraries.
 p  <- clusterAnalysis %>%
   mutate(Cluster = paste("Cluster: ", clusterAnalysis$`topNumeric$Cluster`, sep = "")) %>%
   arrange(desc(Overall)) %>% 
   group_by(Cluster) %>%
   slice(1:10) %>%
   mutate(Under27 = ifelse(Age < 27, "Yes", "No")) %>%
   ggplot(aes(x= Scope, y= TransferValue)) +
   geom_point(position = "jitter", shape = 21, color = "black", size = 2, aes(text = paste(Name, Position, Overall), fill = Under27)) +
   scale_fill_manual(values = c("red", "green")) +
   ggtitle("Player's overall rating plotted against their value for the 10 highest rated players in each cluster") +
   facet_wrap(~ factor(Cluster), scales = "free", ncol = 2) +
   theme(legend.position = "none", strip.text = element_text(face = "bold", size = 12))
 
 # plot output
 ggplotly(p, height = 700, width = 900)
              



