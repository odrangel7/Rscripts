###Created by Oscar Daniel Rangel Huerta
####11/15/2024
#####Script for creating boxplots from mixomics analysis, when there are blocks of data 
###This is an example for the project BIORICA-10

# Load necessary libraries
library(ggplot2)
library(reshape2)

####For creating individual boxplots for each variable
boxplot(HOMA ~ mb_metadata$Grupo, data = mb_biochemistry)
boxplot(Adiponectina ~ mb_metadata$Grupo, data = mb_inflammation)
boxplot(C18.1 ~ mb_metadata$Grupo, data = mb_ac)
boxplot(mb_dieta$DULCES_SNACKS_REFRESCOS_FRUTA_ALMIBAR_out ~ mb_metadata$Grupo)
boxplot(mb_gc$Glutamic.acid ~ mb_metadata$Grupo)
boxplot(mb_ac$C5.M.DC ~ mb_metadata$Grupo, data = mb_ac)
boxplot(mb_lcqtof$LPC.16.0.sn2 ~ mb_metadata$Grupo)

###In case that it is necessary to plot all the variables from each block, this is the code
# Assuming mb_biochemistry is your dataframe and Grupo is in mb_metadata
# Combine mb_metadata and mb_biochemistry for easy plotting
mb_combined <- cbind(mb_metadata[, 1, drop = FALSE], mb_biochemistry)
mb_combined_ac <- cbind(mb_metadata[, 1, drop = FALSE], mb_ac)
mb_combined_gc <- cbind(mb_metadata[, 1, drop = FALSE], mb_gc)
mb_combined_lcqtof <- cbind(mb_metadata[, 1, drop = FALSE], mb_lcqtof)
mb_combined_lcms <- cbind(mb_metadata[, 1, drop = FALSE], mb_lcms)
mb_combined_inf <- cbind(mb_metadata[, 1, drop = FALSE], mb_inflammation)

# Melt the dataframe to long format. Here is relevant to set up the variable that we want to use for grouping
###in this case is "Grupo" but it can be a different one e.g. sex
mb_melted <- melt(mb_combined, id.vars = "Grupo")
melted_ac <- melt(mb_combined_ac, id.vars = "Grupo")
melted_gc <- melt(mb_combined_gc, id.vars = "Grupo")
melted_lcqtof <- melt(mb_combined_lcqtof, id.vars = "Grupo")
melted_lcms <- melt(mb_combined_lcms, id.vars = "Grupo")
melted_inf <- melt(mb_combined_inf, id.vars = "Grupo")

####We create an object that contains the boxplot for each variable in the block
# Create the boxplots using ggplot
p <- ggplot(mb_melted, aes(x = Grupo, y = value, group = Grupo)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() +
  labs(y = NULL, x = "Grupo") +  # Set the y-axis label to NULL
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank())  # Remove y-axis text

p_ac <- ggplot(melted_ac, aes(x = Grupo, y = value, group = Grupo)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() +
  labs(y = NULL, x = "Grupo") +  # Set the y-axis label to NULL
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank())  # Remove y-axis text

p_gc <- ggplot(melted_gc, aes(x = Grupo, y = value, group = Grupo)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() +
  labs(y = NULL, x = "Grupo") +  # Set the y-axis label to NULL
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank())  # Remove y-axis text

p_lcqtof <- ggplot(melted_lcqtof, aes(x = Grupo, y = value, group = Grupo)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() +
  labs(y = NULL, x = "Grupo") +  # Set the y-axis label to NULL
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank())  # Remove y-axis text


p_lcms <- ggplot(melted_lcms, aes(x = Grupo, y = value, group = Grupo)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() +
  labs(y = NULL, x = "Grupo") +  # Set the y-axis label to NULL
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank())  # Remove y-axis text


p_inf <- ggplot(melted_inf, aes(x = Grupo, y = value, group = Grupo)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw() +
  labs(y = NULL, x = "Grupo") +  # Set the y-axis label to NULL
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_blank())  # Remove y-axis text


# Display the plot
print(p)
print(p_ac)
print(p_gc)
print(p_lcqtof)
print(p_lcms)
print(p_inf)
