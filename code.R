install.packages(c("tidyverse", "scales", "viridis", "patchwork"))                  # Install packages.


library(tidyverse)                                                                  # Load packages.
library(scales)
library(viridis)
library(patchwork)



spotify <- read.csv("https://raw.githubusercontent.com/robert-jacques/datavisRProject/refs/heads/main/data/data.csv")

                                                                                    # Load dataset.

###############
# DATA CLEANING
###############


head(spotify)                                                                       # Check structure of dataset.

tail(spotify)

str(spotify)


summary(spotify)                                                                    # Check summary statistics for each variable.


sum(duplicated(spotify))                                                            # Check for duplicate rows in dataset.


sum(is.na(spotify))                                                                 # Check for missing values (NA) (No NA values
                                                                                    # in dataset so no further action required).


spotify %>%                                                                         # Check for zero values.
  summarise(across(c(popularity, duration_ms, danceability, energy,   
                     loudness, speechiness, acousticness,
                     instrumentalness, liveness, valence,
                     tempo, time_signature),
                   ~ sum(. == 0)))


spotify <- spotify %>%                                                              # Replace zero values in each column with
  mutate(across(c(popularity, duration_ms, danceability, energy,                    # mean value for that column once zero
                  loudness, speechiness, acousticness,                              # values removed, other than Mode which
                  instrumentalness, liveness, valence,                              # is binary and Key and Time Signature
                  tempo),                                                           # which are categorical.
                ~ ifelse(. == 0, mean(.[. != 0]), .)))


spotify <- spotify %>%                                                              # Replace zero values in Time Signature
  mutate(time_signature = ifelse(time_signature == 0,                               # with the mode for that column as
                                 as.numeric(names(sort(table(time_signature),       # categorical (zero values in Key and
                                                       decreasing = TRUE))[1]),     # Mode are not missing values).
                                 time_signature))


spotify %>%                                                                         # Check that zero values now replaced.
  summarise(across(c(popularity, duration_ms, danceability, energy,
                     loudness, speechiness, acousticness,
                     instrumentalness, liveness, valence,
                     tempo, time_signature),
                   ~ sum(. == 0)))


summary(spotify)



#####################
# DATA PRE-PROCESSING
#####################


spotify_audio_features <- spotify %>%                                               # Select variables of interest (Valence and
  select(danceability, energy, loudness, valence, tempo)                            # key audio features).


filter_outliers_using_iqr <- function(df, columns_to_clean) {
  df %>%
    mutate(across(all_of(columns_to_clean), ~ {
      Q1 <- quantile(., 0.25, na.rm = TRUE)                                         # Calculate Q1, Q3, and IQR for each
      Q3 <- quantile(., 0.75, na.rm = TRUE)                                         # specified column.
      IQR_value <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR_value                                           # Define lower and upper bounds for outliers.
      upper_bound <- Q3 + 1.5 * IQR_value
      
      ifelse(. >= lower_bound & . <= upper_bound, ., NA)
    }))
}

audio_features_with_outliers <- c("danceability", "energy", "loudness",             # Define audio features with outliers.
                                  "valence", "tempo")


spotify_audio_features <- spotify_audio_features %>%                                # Replace outliers with NA values.
  filter_outliers_using_iqr(audio_features_with_outliers) %>%
  drop_na()


spotify_audio_features <- spotify_audio_features %>%                                # Normalise all numeric variables to 0-1 scale.
  mutate(                                                                           
    across(where(is.numeric),
           ~ rescale(.)))                                                      


spotify_audio_features_cat <- spotify_audio_features %>%                            # Continuous Valence variable categorised as
  mutate(                                                                           # Negative, Neutral, Positive. 3 categories 
    valence = case_when(                                                            # of equal size created for ease of
      valence <= 0.333 ~ "Negative",                                                # interpretation.
      valence > 0.333 & valence <= 0.666 ~ "Neutral",
      valence > 0.666 ~ "Positive"),
    valence = factor(valence,                                                       # New categories converted into factors with
                     levels = c("Negative", "Neutral", "Positive")))                # specified levels.


summary(spotify_audio_features_cat)                                                 # Summarise pre-processed data as check.



#########
# PLOT 1.
#########


caption_text_01 <- paste("PLOT 1. KEY TAKEAWAYS:",
                         "This distribution suggests that positive tracks are consistently more danceable",
                         "while negative tracks tend to be less danceable, but with greater variation.",
                         sep = "\n")                                                # paste() function used for caption text
                                                                                    # due to length!

spotify_audio_features_cat_plot_01 <- ggplot(spotify_audio_features_cat,            # Plot violin plots with overlayed box plots. 
                                             aes(x = valence, y = danceability,
                                                 fill = valence)) +
  geom_violin(colour = "black", linewidth = 0.2, alpha = 0.5) +
  geom_boxplot(width = 0.2, colour = "black", linewidth = 0.2, alpha = 0.4,
               outlier.size = 2, outlier.colour = "darkorange", outlier.alpha = 0.2) +
  labs(title = "Plot 1.",
       subtitle = "Distribution of Danceability across different Valence categories",
       caption = caption_text_01,
       x = "Valence", y = "Danceability") +
  scale_fill_viridis_d(option = "plasma", direction = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0),
        plot.caption.position = "plot",
        axis.title.x = element_text(size = 12, vjust = -1),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        axis.text.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold", hjust = 0.41),
        legend.title.position = "top",
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "right",
        legend.box = "horizontal",
        legend.box.margin = margin(b = 39, l = 10),
        legend.key.spacing.x = unit(0.8, "cm"),
        plot.margin = margin(20, 10, 0, 10)) +
  guides(fill = guide_legend(title = "Valence"))



#########
# PLOT 2.
#########


spotify_audio_features <- spotify %>%                                               # Select variables of interest (Valence and
  select(danceability, energy, loudness, valence, tempo)                            # key audio features - need original
                                                                                    # continuous Valence variable for correlation).

spotify_audio_features_cor_matrix <- cor(spotify_audio_features)                    # Correlation matrix of audio features.


spotify_audio_features_cor_matrix_melted <- spotify_audio_features_cor_matrix %>%   # Reshape data into long format for plotting
  as_tibble(rownames = "Var1") %>%                                                  # of correlation heatmap.
  pivot_longer(cols = -Var1,
               names_to = "Var2",
               values_to = "value")


caption_text_02 <- paste("PLOT 2. KEY TAKEAWAYS:",
                         "The matrix shows that the strongest relationships exist between energy and",
                         "loudness, valence and danceability, and valence and loudness, suggesting that",
                         "valence and energy influence the danceability and loudness of tracks.",
                         sep = "\n")                                                # paste() function used for caption text
                                                                                    # due to length!

spotify_audio_features_cat_plot_02 <- ggplot(spotify_audio_features_cor_matrix_melted,   
                                             aes(x = Var1, y = Var2,                     
                                                 fill = spotify_audio_features_cor_matrix_melted$value)) +
  geom_tile(colour = "black", linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       limits = c(-0.07, 1),
                       labels = label_number(accuracy = 0.001),
                       guide = guide_colorbar(ticks = "both",
                                              tickcolour = "black",
                                              tickwidth = 0.2,
                                              barwidth = 16,
                                              frame.colour = "black",
                                              frame.linewidth = 0.2)) +
  geom_text(aes(label = format(round(spotify_audio_features_cor_matrix_melted$value, 3), nsmall = 3)),                       
            colour = ifelse(spotify_audio_features_cor_matrix_melted$value > 0.4, "white", "black"), size = 4) +               
  labs(title = "Plot 2.",                                                           # Annotate heatmap with rounded correlation
       subtitle = "Correlation Matrix of Key Audio Features and Valence",           # values.
       caption = caption_text_02,
       x = "Audio Features", y = "Audio Features",
       fill = "Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1),
        plot.caption.position = "panel",
        axis.title.x = element_text(size = 12, vjust = -1),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        axis.text.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.title.position = "top",
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "right",
        legend.box = "horizontal",
        legend.box.margin = margin(b = 30, r = 15),
        legend.key.spacing.x = unit(0.8, "cm"),
        legend.key.width = unit(4, "cm"),
        plot.margin = margin(20, 10, 0, 10)) +
  guides(fill = guide_colorbar(ticks = TRUE, ticks.colour = "black",
                               barwidth = 16, frame.colour = "black",
                               frame.linewidth = 0.2))



#########
# PLOT 3.
#########


spotify_audio_features_cat_long <- spotify_audio_features_cat %>%                   # Reshape data into long format for plotting
  pivot_longer(cols = c(danceability, energy, loudness, tempo),                     # of density distribution plots.  
               names_to = "audio_feature",
               values_to = "value")


caption_text_03 <- paste("PLOT 3. KEY TAKEAWAYS:",
                         "The density distributions show that positive tracks tend to have higher",
                         "danceability, energy and loudness, while tempo is less affected by valence.",
                         sep = "\n")                                                # paste() function used for caption text
                                                                                    # due to length!

spotify_audio_features_cat_plot_03 <- ggplot(spotify_audio_features_cat_long,       # Plot faceted density distribution plots.
                                             aes(x = value, fill = valence)) +
  geom_density(colour = "black", linewidth = 0.2, alpha = 0.5) +
  facet_wrap(~ audio_feature, scales = "fixed", ncol = 2) +
  scale_fill_viridis_d(direction = 1) +
  labs(title = "Plot 3.",
       subtitle = "Density Distributions of Audio Features by Valence",
       caption = caption_text_03,
       x = "Value", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0),
        plot.caption.position = "plot",
        axis.title.x = element_text(size = 12, vjust = -1),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        axis.text.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold", hjust = 0.41),
        legend.title.position = "top",
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "right",
        legend.box = "horizontal",
        legend.box.margin = margin(b = 30, r = 23),
        legend.key.spacing.x = unit(0.8, "cm"),
        plot.margin = margin(20, 10, 0, 10)) +
  guides(fill = guide_legend(title = "Valence"))



#########
# PLOT 4.
#########


variance_audio_features <- spotify_audio_features_cat %>%                           # Create tibble with calculated variance 
  group_by(valence) %>%                                                             # for each key audio feature.
  summarise(danceability = var(danceability),
            energy = var(energy),
            loudness = var(loudness),
            tempo = var(tempo))


variance_audio_features_long <- variance_audio_features %>%                         # Reshape data into long format for plotting
  pivot_longer(cols = c(danceability, energy, loudness, tempo),                     # of variance bar charts.
               names_to = "audio_feature", values_to = "variance_value")


caption_text_04 <- paste("PLOT 4. KEY TAKEAWAYS:",
                         "The bar charts indicate that energy exhibits the highest variance in tracks",
                         "with negative valence. As valence increases variance in energy decreases,",
                         "suggesting that more positive tracks have more consistent energy levels.",
                         sep = "\n")                                                # paste() function used for caption text
                                                                                    # due to length!

spotify_audio_features_cat_plot_04 <- ggplot(variance_audio_features_long,          # Plot variance bar charts.
                                             aes(x = audio_feature, y = variance_value,
                                                 fill = valence)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           colour = "black", linewidth = 0.2, alpha = 0.7,
           show.legend = TRUE) +
  scale_fill_viridis_d(option = "inferno", direction = 1) +
  geom_text(aes(label = format(round(variance_value, 3), nsmall = 3)),
            position = position_dodge(width = 0.8),
            size = 4, hjust = 0.5, vjust = -0.7) +
  coord_cartesian(ylim = c(0, 0.08)) +
  labs(title = "Plot 4.",
       subtitle = "Variance of Audio Features by Valence category",
       caption = caption_text_04,
       x = "Audio Feature", y = "Variance",
       fill = "Valence") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1),
        plot.caption.position = "panel",
        axis.title.x = element_text(size = 12, vjust = -1),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        axis.text.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold", hjust = 0.41),
        legend.title.position = "top",
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.justification = "right",
        legend.box = "horizontal",
        legend.box.margin = margin(b = 30, r = 15),
        legend.key.spacing.x = unit(1, "cm"),
        plot.margin = margin(20, 10, 0, 10)) +
  guides(fill = guide_legend(title = "Valence"))
    


#################
# COMPOSITE PLOT.
#################


caption_text_sum <- paste("Valence is a measure of musical positiveness and is a measurable indicator of the mood of a track. Tracks which",
                         "sound positive score higher and those which sound negative score lower.    Source: Spotify Tracks Dataset",
                         sep = "\n")                                                # paste() function used for caption text
                                                                                    # due to length!

spotify_audio_features_cat_plot_sum <- (spotify_audio_features_cat_plot_01 |        # Plot composite visualisation of plots 1-4.  
                                         spotify_audio_features_cat_plot_02) /
  (spotify_audio_features_cat_plot_03 | spotify_audio_features_cat_plot_04) +
  plot_annotation(title = "How do Key Audio Features vary across difference categories of Valence?",
                  subtitle = "Investigating relationships in the Spotify Tracks Dataset",
                  caption = caption_text_sum,
                  theme = theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
                                plot.subtitle = element_text(size = 18, face = "bold.italic", hjust = 0.5),
                                plot.caption = element_text(size = 10, hjust = 0.5, vjust = 0),
                                plot.caption.position = "panel")) +
  theme(axis.title.x = element_text(size = 12, vjust = -1),
        axis.title.y = element_text(size = 12, vjust = 1.5),
        plot.margin = margin(10, 10, 0, 10))


spotify_audio_features_cat_plot_sum