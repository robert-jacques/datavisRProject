spotify <- read.csv("dataset.csv")                                                  # Load dataset.

library(tidyverse)

summary(spotify)                                                                    

                                                                  
spotify <- spotify %>%
  select(-X, -track_id, -artists, -album_name,
         -track_name, -explicit, -track_genre)                                      # Select variables of interest.


sum(is.na(spotify))                                                                 # Check for missing values (NA)

spotify <- na.omit(spotify)

spotify <- spotify %>%
  mutate(across(.cols = -mode,
                .fns = ~ifelse(. == 0, mean(.x[.x != 0],
                                            na.rm = TRUE), .)))                     # Replace zero values in each column
                                                                                    # with mean value for that column
                                                                                    # once zero values removed (EXCLUDING
                                                                                    # "mode" as binary).


ggplot(spotify, aes(x = danceability, y = energy)) +
  geom_point(size=0.5)

ggplot(spotify, aes(x = danceability, y = loudness)) +
  geom_point(size=0.5)

ggplot(spotify, aes(x = danceability, speechiness)) +
  geom_point(size=0.5)

ggplot(spotify, aes(x = danceability, acousticness)) +
  geom_point(size=0.5)

ggplot(spotify, aes(x = danceability, y = instrumentalness)) +
  geom_point(size=0.5)

ggplot(spotify, aes(x = danceability, y = liveness)) +
  geom_point(size=0.5)

ggplot(spotify, aes(x = danceability, y = valence)) +
  geom_point(size=0.5)

ggplot(spotify, aes(x = danceability, y = tempo)) +
  geom_point(size=0.5)


library(reshape2)
library(RColorBrewer)

spotify_cor_matrix <- cor(spotify)

spotify_cor_matrix_melted <- melt(spotify_cor_matrix)

ggplot(spotify_cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Correlation Heatmap - Spotify Tracks Dataset",
       subtitle = "Danceability vs. Musical Features",
       x = "", y = "",
       caption = "Spotify Tracks Dataset",
       fill = "Correlation") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 10, face = "bold"))


install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(spotify_cor_matrix, method = "circle", 
           type = "lower", 
           title = "Correlation Matrix of Musical Characteristics", 
           ggtheme = theme_minimal())


spotify_categorised <- spotify %>%                              # Continuous danceability, valence and mod variables categorised.
  mutate(danceability_category = case_when(
    danceability <= 0.333 ~ "Low",
    danceability > 0.333 & danceability <= 0.666 ~ "Moderate",
    danceability > 0.666 ~ "High"),
    valence_category = case_when(
      valence <= 0.333 ~ "Low",
      valence > 0.333 & valence <= 0.666 ~ "Moderate",
      valence > 0.666 ~ "High"),
    mode_category = case_when(
      mode == 1 ~ "Major Key",
      mode == 0 ~ "Minor Key")) %>%
  mutate(danceability_category = factor(danceability_category, levels = c("High", "Moderate", "Low")),
         valence_category = factor(valence_category, levels = c("High", "Moderate", "Low")),
         mode_category = factor(mode_category, levels = c("Major Key", "Minor Key")))







library(ggalluvial)

spotify_categorised %>% group_by(danceability_category, valence_category, mode_category) %>% summarise(count=n()) %>%
  ggplot(aes(y = count, axis1 = danceability_category, axis2 = valence_category, axis3 = mode_category)) +
  geom_alluvium(aes(fill = danceability_category)) +
  geom_stratum() +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Danceability", "Valence", "Mode"))+
  theme(panel.background = element_blank(), axis.line.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Alluvial Plot - Spotify Tracks Dataset",
       subtitle = "The relationship between Danceability, Valence and Mode",
       caption = "Spotify Tracks Dataset",
       x = "Musical Characteristic",
       y = "",
       fill = "Danceability")



ggplot(spotify_categorised, aes(x = valence, y = danceability)) +
  geom_violin(aes(fill = valence), show.legend = FALSE) +  # Violin plot for each time signature
  labs(title = "Danceability vs Time Signature",
       x = "Time Signature",
       y = "Danceability") +
  theme_minimal()
















library(GGally)

ggparcoord(spotify_categorised, columns=c(2:3, 5:10), groupColumn = 1) +
  labs(colour = "Tempo",
       x = "Musical Characteristic",
       y = "Value",
       title ="Parallel coordinates of Musical Characteristic data by Tempo",
       caption = "Spotify Tracks Dataset")



# PRINCIPAL COMPONENT ANALYSIS

spotify_numeric_data <- spotify %>%
  select(-danceability)

spotify_pca <- prcomp(spotify_numeric_data)

spotify_pca <- data.frame(PC1 = spotify_pca$x[, 1],
                          PC2 = spotify_pca$x[, 2])

ggplot(spotify_pca, aes(x = PC1, y = PC2)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Principal Component Analysis",
       subtitle = "Spotify Tracks Dataset",
       caption = "Spotify Tracks Dataset")


spotify_pca <- prcomp(spotify_numeric_data, scale = TRUE)

spotify_pca <- data.frame(PC1 = spotify_pca$x[, 1],
                          PC2 = spotify_pca$x[, 2])


ggplot(spotify_pca, aes(x = PC1, y = PC2)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Principal Component Analysis - Scaled",
       subtitle = "Spotify Tracks Dataset",
       caption = "Spotify Tracks Dataset")


spotify_pca <- prcomp(spotify_numeric_data)

spotify_loading <- data.frame(dimensions = colnames(spotify_numeric_data),
                              PC1 = spotify_pca$rotation[, 1],
                              PC2 = spotify_pca$rotation[, 2])

ggplot(spotify_loading, aes(x = PC1, y = PC2, label = dimensions)) +
  geom_text()


