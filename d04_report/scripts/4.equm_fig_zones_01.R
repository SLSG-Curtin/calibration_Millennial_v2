library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

#days
eqm_993 <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/optim/sitebysite/VR_new_benchmark_sim_ts_0613_year.rds")
new_all_sim_outs <- eqm_993
new_all_sim_outs$days <- ave(new_all_sim_outs$site, new_all_sim_outs$site, FUN = seq_along)
all_sim_outs_plot <- new_all_sim_outs[,c(2:9)]
all_sim_outs_plot$days <- as.numeric(all_sim_outs_plot$days)

all_sim_outs_plot <- all_sim_outs_plot[,c(1:5,7:8)]

days_sim_outs_plot_top <- all_sim_outs_plot[which(all_sim_outs_plot$site %in% samples_rangelandtop$site),]
days_sim_outs_plot_east <- all_sim_outs_plot[which(all_sim_outs_plot$site %in% samples_rangelandeast$site),]
days_sim_outs_plot_west <- all_sim_outs_plot[which(all_sim_outs_plot$site %in% samples_rangelandwest$site),]

#days
eqm_993 <- readRDS("/media/DATADRIVE1/Model/Millennial/calibration_mm2/d03_output/sitebysite_benchmark_pars_outs_ode_100y_0526_ts_sim.rds")
new_all_sim_outs <- eqm_993
new_all_sim_outs$years <- ave(new_all_sim_outs$site, new_all_sim_outs$site, FUN = seq_along)
all_sim_outs_plot <- new_all_sim_outs[,c(2:9)]
all_sim_outs_plot$years <- as.numeric(all_sim_outs_plot$years)

all_sim_outs_plot <- all_sim_outs_plot[,c(1:5,7:8)]

years_sim_outs_plot_top <- all_sim_outs_plot[which(all_sim_outs_plot$site %in% samples_rangelandtop$site),]
years_sim_outs_plot_east <- all_sim_outs_plot[which(all_sim_outs_plot$site %in% samples_rangelandeast$site),]
years_sim_outs_plot_west <- all_sim_outs_plot[which(all_sim_outs_plot$site %in% samples_rangelandwest$site),]



plot_timeseries_with_means <- function(data, time_col, site_col) {
  # Prepare data for plotting
  plot_data <- data %>%
    select(all_of(c(time_col, site_col, c("POM","AGG","MAOM")))) %>%
    pivot_longer(cols = all_of(c("POM","AGG","MAOM")), 
                 names_to = "variable", 
                 values_to = "value")
  
  # Calculate means for each time point and variable
  mean_data <- plot_data %>%
    group_by(!!sym(time_col), variable) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')
  
  # Create individual plots for each variable
  plots <- list()
  
  for (col in c("POM","AGG","MAOM")) {
    # Filter data for current variable
    current_data <- plot_data %>% filter(variable == col)
    current_mean <- mean_data %>% filter(variable == col)
    
    # Create plot
    p <- ggplot() +
      # Individual site lines (lighter, thinner)
      geom_line(data = current_data, 
                aes(x = !!sym(time_col), y = value, color = as.factor(!!sym(site_col))), 
                alpha = 0.6, size = 0.5) +
      # Mean line (bold, black)
      geom_line(data = current_mean, 
                aes(x = !!sym(time_col), y = mean_value), 
                color = "black", size = 1.5) +
      labs(title = paste("Time Series for", col),
           x = if (col == c("POM","AGG","MAOM")[length(c("POM","AGG","MAOM"))]) "Time" else "",
           y = col) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        #legend.position = if (col == value_cols[1]) "right" else "none"
      ) +
      scale_color_discrete(name = "Site")
    
    plots[[col]] <- p
  }
  
  # Arrange plots vertically
  grid.arrange(grobs = plots, ncol = 1)
}


plot_timeseries_with_means(all_sim_outs_plot_top, "months", "site")

# Example usage:
# Assuming your data frame is called 'df' with columns:
# - 'timestamp' for time
# - 'site_id' for site identification
# - 'large_col1', 'large_col2', 'large_col3' for the three columns with larger values

# plot_timeseries_with_means(
#   data = df,
#   time_col = "timestamp",
#   value_cols = c("large_col1", "large_col2", "large_col3"),
#   site_col = "site_id"
# )

df <- site_GA_bot154[,c(1:6)]
df <- site_NSW001534[,c(1:6)]
df <- site_GA_bot736[,c(1:6)]

#df[, c(1:6)] <- lapply(df[, c(1:6)], function(x){x/100})

df_long <- df %>% gather(key = "variable", value = "value", -time)

df_long$variable <- factor(df_long$variable, levels=c("POM", "MAOM","AGG","MIC","LMWC"))

df_long_mod <- df_long %>%
  bind_rows(
    tibble(
      time     = c(NA, NA),      # NA or any time points; won't plot lines, just sets scale
      value    = c(0, 4.5),    # desired min and max
      variable = "POM"           # must match the MAOC label
    ),
    tibble(
      time     = c(NA, NA),       # NA or any time points; won't plot lines, just sets scale
      value    = c(9, 54),       # desired min and max
      variable = "MAOM"           # must match the MAOC label
    ),
    tibble(
      time     = c(NA, NA),       # NA or any time points; won't plot lines, just sets scale
      value    = c(1, 18),         # desired min and max
      variable = "AGG"            # must match the AGG label
    ),
    tibble(
      time     = c(NA, NA),       # NA or any time points; won't plot lines, just sets scale
      value    = c(0.04, 0.30),      # desired min and max,1.5e-3
      variable = "MIC"            # must match the MIC label
    ),
    tibble(
      time     = c(NA, NA),       # NA or any time points; won't plot lines, just sets scale
      value    = c(0.01, 2.00),      # desired min and max,1.5e-2
      variable = "LMWC"            # must match the MIC label
    )
  )

df_long_mod$variable <- factor(df_long_mod$variable, levels=c("POM", "MAOM","AGG","MIC","LMWC"))

#df_long_mod$time <- df_long_mod$months

#df_long_mod_filter <- df_long_mod[which(df_long_mod$variable %in% c("POM", "MAOM","AGG")),]
p_GA_bot736 <- ggplot() +
  # Main time series (always solid)
  geom_line(data = df_long_mod,
            aes(x = time, y = value, color = variable),
            size = 1, linetype = "solid") +
  # Color scale for variables (kept as before)
  scale_color_manual(
    values = c(
      "POM"  = "#F39B7FFF", 
      "MAOM" = "#3C5488FF",
      "AGG"  = "#00A087FF",
      "MIC"  = "#4DBBD5FF",
      "LMWC" = "#E64B35FF"),
    guide = "none"  # Remove color legend
  ) +
  #scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1, strip.position = "right") +
  #scale_y_continuous(labels = label_scientific(digits = 1)) +
  theme_minimal() +
  theme(
    # Now show the legend (change the position as needed)
    legend.position = c(0.5, 0.95),
    #legend.justification = c(0.5, 1),
    panel.spacing = unit(0, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  ) +
  labs(x = "Time (month)", y = expression("Fractions, (MgC ha"^{-1}*")"))

p_GA_bot736


p_NSW001534 <- ggplot() +
  # Main time series (always solid)
  geom_line(data = df_long_mod,
            aes(x = time, y = value, color = variable),
            size = 1, linetype = "solid") +
  # Color scale for variables (kept as before)
  scale_color_manual(
    values = c(
      "POM"  = "#F39B7FFF", 
      "MAOM" = "#3C5488FF",
      "AGG"  = "#00A087FF",
      "MIC"  = "#4DBBD5FF",
      "LMWC" = "#E64B35FF"),
    guide = "none"  # Remove color legend
  ) +
  #scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1, strip.position = "right") +
  #scale_y_continuous(labels = label_scientific(digits = 1)) +
  theme_minimal() +
  theme(
    # Now show the legend (change the position as needed)
    legend.position = c(0.5, 0.95),
    #legend.justification = c(0.5, 1),
    panel.spacing = unit(0, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  ) +
  labs(x = "Time (month)", y = expression("Fractions, (MgC ha"^{-1}*")"))

p_NSW001534



p_NSW002903 <- ggplot() +
  # Main time series (always solid)
  geom_line(data = df_long_mod,
            aes(x = time, y = value, color = variable),
            size = 1, linetype = "solid") +
  # Color scale for variables (kept as before)
  scale_color_manual(
    values = c(
      "POM"  = "#F39B7FFF", 
      "MAOM" = "#3C5488FF",
      "AGG"  = "#00A087FF",
      "MIC"  = "#4DBBD5FF",
      "LMWC" = "#E64B35FF"),
    guide = "none"  # Remove color legend
  ) +
  #scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1, strip.position = "right") +
  #scale_y_continuous(labels = label_scientific(digits = 1)) +
  theme_minimal() +
  theme(
    # Now show the legend (change the position as needed)
    legend.position = c(0.5, 0.95),
    #legend.justification = c(0.5, 1),
    panel.spacing = unit(0, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  ) +
  labs(x = "Time (month)", y = expression("Fractions, (MgC ha"^{-1}*")"))

p_NSW002903

p_GA_bot154 <- ggplot() +
  # Main time series (always solid)
  geom_line(data = df_long_mod,
            aes(x = time, y = value, color = variable),
            size = 1, linetype = "solid") +
  # Color scale for variables (kept as before)
  scale_color_manual(
    values = c(
      "POM"  = "#F39B7FFF", 
      "MAOM" = "#3C5488FF",
      "AGG"  = "#00A087FF",
      "MIC"  = "#4DBBD5FF",
      "LMWC" = "#E64B35FF"),
    guide = "none"  # Remove color legend
  ) +
  #scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1, strip.position = "right") +
  #scale_y_continuous(labels = label_scientific(digits = 1)) +
  theme_minimal() +
  theme(
    # Now show the legend (change the position as needed)
    legend.position = c(0.5, 0.95),
    #legend.justification = c(0.5, 1),
    panel.spacing = unit(0, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  ) +
  labs(x = "Time (month)", y = expression("Fractions, (MgC ha"^{-1}*")"))

df_long_mod_154  <- df_long_mod
df_long_mod_1534 <- df_long_mod
df_long_mod_736  <- df_long_mod

df_long_mod_154$site <- "Tropical"
df_long_mod_1534$site <- "Temperate"
df_long_mod_736$site <- "Arid"

df_long_mod_three <- rbind(df_long_mod_154,df_long_mod_1534,df_long_mod_736)

df_long_mod_three$variable <- factor(df_long_mod_three$variable, levels = c("MAOM","AGG","POM","MIC","LMWC"))

p_three_sites <- ggplot() +
  # Main time series (always solid)
  geom_line(data = df_long_mod_three,
            aes(x = time, y = value, color = variable),
            size = 1, linetype = "solid") +
  # Color scale for variables (kept as before)
  scale_color_manual(
    values = c(
      "POM"  = "#F39B7FFF", 
      "MAOM" = "#3C5488FF",
      "AGG"  = "#00A087FF",
      "MIC"  = "#4DBBD5FF",
      "LMWC" = "#E64B35FF"),
    guide = "none"  # Remove color legend
  ) +
  # Facet by both site and variable
  facet_grid(variable ~ site, scales = "free_y", 
             labeller = labeller(site = label_both)) +
  theme_minimal() +
  theme(
    # Legend positioning
    legend.position = c(0.5, 0.95),
    panel.spacing = unit(0.2, "cm"),  # Add some spacing between panels
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    strip.background = element_blank(),
    # Strip text formatting for both variable and site labels
    strip.text.y = element_text(hjust = 0.5, face = "bold", size = 14),  # Variable labels
    strip.text.x = element_text(hjust = 0.5, face = "bold", size = 14),  # Site labels
    axis.text = element_text(face = "bold", size = 12),  # Slightly smaller for readability
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  ) +
  labs(x = "Time (year)", y = expression("C fractions, (MgC ha"^{-1}*")"))


p_three_sites

plot_timeseries_with_means <- function(data, time_col, site_col) {
  # Prepare data for plotting
  plot_data <- data %>%
    select(all_of(c(time_col, site_col, c("POM","AGG","MAOM")))) %>%
    pivot_longer(cols = all_of(c("POM","AGG","MAOM")), 
                 names_to = "variable", 
                 values_to = "value")
  
  # Calculate means for each time point and variable
  mean_data <- plot_data %>%
    group_by(!!sym(time_col), variable) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')
  
  # Create individual plots for each variable
  plots <- list()
  
  for (col in c("POM","AGG","MAOM")) {
    # Filter data for current variable
    current_data <- plot_data %>% filter(variable == col)
    current_mean <- mean_data %>% filter(variable == col)
    
    # Create plot
    p <- ggplot() +
      # Individual site lines (lighter, thinner)
      geom_line(data = current_data, 
                aes(x = !!sym(time_col), y = value, color = as.factor(!!sym(site_col))), 
                alpha = 0.6, size = 0.5) +
      # Mean line (bold, black)
      geom_line(data = current_mean, 
                aes(x = !!sym(time_col), y = mean_value), 
                color = "black", size = 1.5) +
      labs(title = paste("Time Series for", col),
           x = if (col == c("POM","AGG","MAOM")[length(c("POM","AGG","MAOM"))]) "Time" else "",
           y = col) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold")
      ) +
      scale_color_discrete(name = "Site")
    
    plots[[col]] <- p
  }
  
  # Arrange plots vertically
  grid.arrange(grobs = plots, ncol = 1)
}


plot_timeseries_faceted <- function(data, time_col, value_cols, site_col) {
  # Prepare data
  plot_data <- data %>%
    select(all_of(c(time_col, site_col, value_cols))) %>%
    pivot_longer(cols = all_of(value_cols), 
                 names_to = "variable", 
                 values_to = "value")
  # Calculate means
  mean_data <- plot_data %>%
    group_by(!!sym(time_col), variable) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')
  
  #mean_data$variable <- as.factor(mean_data$variable)
  variable_colors <- c(
    "POM"  = "#F39B7FFF", 
    "MAOM" = "#3C5488FF",
    "AGG"  = "#00A087FF"
  )
  # Create faceted plot
  ggplot() +
    # Individual site lines
    geom_line(data = plot_data, 
              aes(x = !!sym(time_col), y = value, color = as.factor(!!sym(site_col))), 
              alpha = 0.6, size = 0.5, linetype = "solid") +
    # Mean lines
    geom_line(data = mean_data, 
              aes(x = !!sym(time_col), y = mean_value), 
              color = "black", size = 1.5) +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, strip.position = "right") +
    labs(title = "",
         x = "Time",
         y = "Value") +
    theme_minimal() +
    theme(
      panel.spacing = unit(0, "cm"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "white", colour = "grey50"),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.text = element_text(face = "bold", size = 14),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    ) +
    scale_color_manual(
      values = variable_colors,
      guide = "none"  # Remove color legend
    )+
    labs(x = "Time (year)", y = expression("Fractions, (MgC ha"^{-1}*")"))
}

plot_timeseries_faceted(df, "months", c("POM","AGG","MAOM"),"site")


plot_timeseries_faceted <- function(data, time_col, value_cols, site_col) {
  # Prepare data
  plot_data <- data %>%
    select(all_of(c(time_col, site_col, value_cols))) %>%
    pivot_longer(cols = all_of(value_cols), 
                 names_to = "variable", 
                 values_to = "value")
  
  plot_data$variable <- factor(plot_data$variable,levels=c("POM","AGG","MAOM","MIC","LMWC"))
  
  # Calculate means
  mean_data <- plot_data %>%
    group_by(!!sym(time_col), variable) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')
  
  mean_data$variable <- factor(mean_data$variable,levels=c("POM","AGG","MAOM","MIC","LMWC"))
  
  variable_colors <- c(
    "POM"  = "#F39B7FFF", 
    "MAOM" = "#3C5488FF",
    "AGG"  = "#00A087FF",
    "MIC"  = "#4DBBD5FF",
    "LMWC" = "#E64B35FF"
  )
  
  # Create faceted plot
  p1 <- ggplot() +
    # Individual site lines - keep as gray/neutral
    geom_line(data = plot_data, 
              aes(x = !!sym(time_col), y = value, group = !!sym(site_col)), 
              color = "gray60", alpha = 0.6, size = 0.5, linetype = "solid") +
    # Mean lines - colored by variable
    geom_line(data = mean_data, 
              aes(x = !!sym(time_col), y = mean_value, color = variable), 
              size = 1.5) +
    facet_wrap(~ variable, scales = "free_y", ncol = 1, strip.position = "right") +
    labs(title = "",
         x = "Time",
         y = "Value") +
    theme_minimal() +
    theme(
      panel.spacing = unit(0, "cm"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "white", colour = "grey50"),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.text = element_text(face = "bold", size = 14),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    ) +
    scale_color_manual(
      values = variable_colors,
      guide = "none"  # Remove color legend
    ) +
    labs(x = "Time (year)", y = expression("Fractions, (MgC ha"^{-1}*")"))
}

df <- all_sim_outs_plot


df[, c(1:5)] <- lapply(df[, c(1:5)], function(x){x/100})



plot_timeseries_faceted(df, "months", c("POM","AGG","MAOM","MIC","LMWC"),"site")

#top
df <- all_sim_outs_plot_top

df[, c(1:5)] <- lapply(df[, c(1:5)], function(x){x/100})

plot_timeseries_faceted(df, "months", c("POM","AGG","MAOM","MIC","LMWC"),"site")

#east
df <- all_sim_outs_plot_east

df[, c(1:5)] <- lapply(df[, c(1:5)], function(x){x/100})

plot_timeseries_faceted(df, "months", c("POM","AGG","MAOM","MIC","LMWC"),"site")


#top
df <- all_sim_outs_plot_west

df[, c(1:5)] <- lapply(df[, c(1:5)], function(x){x/100})

plot_timeseries_faceted(df, "months", c("POM","AGG","MAOM","MIC","LMWC"),"site")


# Function to prepare and combine the three datasets
prepare_combined_data <- function() {
  # Prepare top dataset
  df_top <- all_sim_outs_plot_top
  df_top[, c(1:5)] <- lapply(df_top[, c(1:5)], function(x){x/100})
  df_top$dataset <- "Tropical"
  
  # Prepare east dataset
  df_east <- all_sim_outs_plot_east
  df_east[, c(1:5)] <- lapply(df_east[, c(1:5)], function(x){x/100})
  df_east$dataset <- "Temperate"
  
  # Prepare west dataset
  df_west <- all_sim_outs_plot_west
  df_west[, c(1:5)] <- lapply(df_west[, c(1:5)], function(x){x/100})
  df_west$dataset <- "Arid"
  
  # Combine all datasets
  combined_data <- bind_rows(df_top, df_east, df_west)
  
  return(combined_data)
}

# Modified function for combined plotting with dataset facets
plot_timeseries_combined_faceted <- function(data, time_col, value_cols, site_col, dataset_col) {
  # Prepare data
  plot_data <- data %>%
    select(all_of(c(time_col, site_col, dataset_col, value_cols))) %>%
    pivot_longer(cols = all_of(value_cols), 
                 names_to = "variable", 
                 values_to = "value")
  
  plot_data$variable <- factor(plot_data$variable, levels = c("MAOM","AGG","POM","MIC","LMWC"))
  
  plot_data[[dataset_col]] <- factor(plot_data[[dataset_col]], levels = c("Tropical", "Temperate", "Arid"))
  
  # Calculate means by dataset and variable
  mean_data <- plot_data %>%
    group_by(!!sym(time_col), !!sym(dataset_col), variable) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')
  
  mean_data$variable <- factor(mean_data$variable, levels = c("MAOM","AGG","POM","MIC","LMWC"))
  mean_data[[dataset_col]] <- factor(mean_data[[dataset_col]], levels = c("Tropical", "Temperate", "Arid"))
  
  #variable_colors <- c(
  #  "POM"  = "#F39B7FFF", 
  #  "MAOM" = "#3C5488FF",
  #  "AGG"  = "#00A087FF",
  #  "MIC"  = "#4DBBD5FF",
  #  "LMWC" = "#E64B35FF"
  #)
  
  variable_colors <- c(
    "MAOM" = "#3C5488FF",
    "AGG"  = "#00A087FF",
    "POM"  = "#F39B7FFF",
    "MIC"  = "#4DBBD5FF",
    "LMWC" = "#E64B35FF"
  )
  
  # Create faceted plot with both variable and dataset facets
  ggplot() +
    # Individual site lines - keep as gray/neutral
    geom_line(data = plot_data, 
              aes(x = !!sym(time_col), y = value, group = !!sym(site_col)), 
              color = "gray60", alpha = 0.6, size = 0.5, linetype = "solid") +
    # Mean lines - colored by variable
    geom_line(data = mean_data, 
              aes(x = !!sym(time_col), y = mean_value, color = variable), 
              size = 1.5) +
    # Facet by variable (rows) and dataset (columns)
    facet_grid(variable ~ dataset, scales = "free_y") +
    labs(title = "",
         x = "Time (year)", 
         y = expression("Fractions, (MgC ha"^{-1}*")")) +
    theme_minimal() +
    theme(
      panel.spacing = unit(0.2, "cm"),  # Add some spacing between panels
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "white", colour = "grey50"),
      strip.background = element_blank(),
      # Strip text for variables (right side)
      strip.text.y = element_text(hjust = 0.5, face = "bold", size = 14),
      # Strip text for datasets (top)
      strip.text.x = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text = element_text(face = "bold", size = 12),  # Slightly smaller for readability
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    ) +
    scale_color_manual(
      values = variable_colors,
      guide = "none"  # Remove color legend
    )
}


#revise one

plot_timeseries_combined_faceted <- function(data, time_col, value_cols, site_col, dataset_col) {
  # Prepare data
  plot_data <- data %>%
    select(all_of(c(time_col, site_col, dataset_col, value_cols))) %>%
    pivot_longer(cols = all_of(value_cols), 
                 names_to = "variable", 
                 values_to = "value")
  
  plot_data$variable <- factor(plot_data$variable, levels = c("MAOM","AGG","POM","MIC","LMWC"))
  #plot_data$months <- with(plot_data,log2(months))
  #plot_data$value <- with(plot_data,log(value))
  plot_data[[dataset_col]] <- factor(plot_data[[dataset_col]], levels = c("Tropical", "Temperate", "Arid"))
  
  # Calculate means by dataset and variable
  
  mean_data <- plot_data %>%
    group_by(!!sym(time_col), !!sym(dataset_col), variable) %>%
    summarise(
      mean_value = median(value, na.rm = TRUE),
      q05 = quantile(value, 0.05, na.rm = TRUE),  # 5th percentile
      q95 = quantile(value, 0.95, na.rm = TRUE),  # 95th percentile
      se = sd(value, na.rm = TRUE) / sqrt(n()),
      ci_lower = mean_value - 1.96 * se,  # 95% confidence interval
      ci_upper = mean_value + 1.96 * se,
      .groups = 'drop'
    )
  
  mean_data$variable <- factor(mean_data$variable, levels = c("MAOM","AGG","POM","MIC","LMWC"))
  mean_data[[dataset_col]] <- factor(mean_data[[dataset_col]], levels = c("Tropical", "Temperate", "Arid"))
  
  #variable_colors <- c(
  #  "POM"  = "#F39B7FFF", 
  #  "MAOM" = "#3C5488FF",
  #  "AGG"  = "#00A087FF",
  #  "MIC"  = "#4DBBD5FF",
  #  "LMWC" = "#E64B35FF"
  #)
  
  variable_colors <- c(
    "MAOM" = "#3C5488FF",
    "AGG"  = "#00A087FF",
    "POM"  = "#F39B7FFF",
    "MIC"  = "#4DBBD5FF",
    "LMWC" = "#E64B35FF"
  )
  
  # Create faceted plot with both variable and dataset facets
  ggplot() +
    # Individual site lines - keep as gray/neutral
    geom_line(data = plot_data, 
              aes(x = !!sym(time_col), y = value, group = !!sym(site_col)), 
              color = "gray80", alpha = 0.5, size = 0.3, linetype = "solid") +
    # Mean lines - colored by variable
    # Confidence intervals - filled ribbons
    #geom_ribbon(data = mean_data,
    #            aes(x = !!sym(time_col), ymin = ci_lower, ymax = ci_upper, fill = variable),
    #            alpha = 0.5,color = NA) +
    geom_ribbon(data = mean_data,
                aes(x = !!sym(time_col), ymin = q05, ymax = q95, fill = variable),
                alpha = 0.3, size = 0.5, linetype = "dashed") +
    geom_line(data = mean_data, 
              aes(x = !!sym(time_col), y = mean_value, color = variable), 
              size = 0.6, linetype = "solid") +
    # Facet by variable (rows) and dataset (columns)
    facet_grid(variable ~ dataset, scales = "free_y") +
    labs(title = "",
         x = "Time (day)", 
         y = expression("Fractions, (MgC ha"^{-1}*")")) +
    theme_minimal() +
    theme(
      panel.spacing = unit(0.2, "cm"),  # Add some spacing between panels
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "white", colour = "grey50"),
      strip.background = element_blank(),
      # Strip text for variables (right side)
      strip.text.y = element_text(hjust = 0.5, face = "bold", size = 14),
      # Strip text for datasets (top)
      strip.text.x = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text = element_text(face = "bold", size = 12),  # Slightly smaller for readability
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    ) +
    scale_color_manual(
      values = variable_colors,
      guide = "none"  # Remove color legend
    ) +
    scale_fill_manual(
      values = variable_colors,
      guide = "none"  # Remove fill legend
    )
}

plot_timeseries_combined_faceted_update <- function(data, time_col, value_cols, site_col, dataset_col) {
  #Prepare data
  plot_data <- data %>%
    select(all_of(c(time_col, site_col, dataset_col, value_cols))) %>%
    pivot_longer(cols = all_of(value_cols), 
                 names_to = "variable", 
                 values_to = "value")
  
  plot_data$variable <- factor(plot_data$variable, levels = c("MAOM","AGG","POM","MIC","LMWC"))
  plot_data[[dataset_col]] <- factor(plot_data[[dataset_col]], levels = c("Tropical", "Temperate", "Arid"))
  
  # Calculate means by dataset and variable
  mean_data <- plot_data %>%
    group_by(!!sym(time_col), !!sym(dataset_col), variable) %>%
    summarise(
      mean_value = median(value, na.rm = TRUE),
      q05 = quantile(value, 0.05, na.rm = TRUE),  # 5th percentile
      q95 = quantile(value, 0.95, na.rm = TRUE),  # 95th percentile
      se = sd(value, na.rm = TRUE) / sqrt(n()),
      ci_lower = mean_value - 1.96 * se,  # 95% confidence interval
      ci_upper = mean_value + 1.96 * se,
      .groups = 'drop'
    )
  
  mean_data$variable <- factor(mean_data$variable, levels = c("MAOM","AGG","POM","MIC","LMWC"))
  mean_data[[dataset_col]] <- factor(mean_data[[dataset_col]], levels = c("Tropical", "Temperate", "Arid"))
  
  variable_colors <- c(
    "MAOM" = "#3C5488FF",
    "AGG"  = "#00A087FF",
    "POM"  = "#F39B7FFF",
    "MIC"  = "#4DBBD5FF",
    "LMWC" = "#E64B35FF"
  )
  
  # Create faceted plot with both variable and dataset facets
  pplot <- ggplot() +
    # Individual site lines - lighter and thinner for background
    geom_line(data = plot_data, 
              aes(x = !!sym(time_col), y = value, group = !!sym(site_col)), 
              color = "gray85", alpha = 0.4, size = 0.3, linetype = "solid") +
    # Confidence intervals - dashed ribbon borders with subtle fill
    geom_ribbon(data = mean_data,
                aes(x = !!sym(time_col), ymin = q05, ymax = q95, fill = variable),
                alpha = 0.25, color = "gray70", size = 0.4, linetype = "dashed") +
    # Mean lines - slightly thicker and more prominent
    geom_line(data = mean_data, 
              aes(x = !!sym(time_col), y = mean_value, color = variable), 
              size = 0.65, linetype = "solid") +
    # Facet by variable (rows) and dataset (columns)
    facet_grid(variable ~ dataset, scales = "free_y") +
    labs(title = "",
         x = ifelse(time_col == "days", "Time (day)", "Time (year)"), 
         y = expression("Fractions, (MgC ha"^{-1}*")")) +
    theme_minimal() +
    theme(
      panel.spacing = unit(0.2, "cm"),  # Add some spacing between panels
      panel.grid.major.y = element_line(color = "gray92", size = 0.3),  # Very light grid lines
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray92", size = 0.3),  # Light x-axis grid
      panel.grid.minor.x = element_blank(),
      panel.background = element_rect(fill = "white", colour = "gray75", size = 0.5),
      strip.background = element_blank(),
      # Strip text for variables (right side)
      strip.text.y = element_text(hjust = 0.5, face = "bold", size = 14),
      # Strip text for datasets (top)
      strip.text.x = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text = element_text(face = "bold", size = 12),  # Slightly smaller for readability
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    ) +
    scale_color_manual(
      values = variable_colors,
      guide = "none"  # Remove color legend
    ) +
    scale_fill_manual(
      values = variable_colors,
      guide = "none"  # Remove fill legend
    )
  
  return(pplot)
}

# Execute the combined plot
combined_data <- prepare_combined_data()
# time_col, value_cols, site_col, dataset_col
plot_timeseries_combined_faceted(combined_data, "days", c("POM","AGG","MAOM","MIC","LMWC"), "site", "dataset")
plot_timeseries_combined_faceted_update(combined_data, "days", c("POM","AGG","MAOM","MIC","LMWC"), "site", "dataset")


################################################################################

plot_timeseries_combined_faceted_update <- function(data, time_col, value_cols, site_col, dataset_col, y_limits) {
  #Prepare data
  plot_data <- data %>%
    select(all_of(c(time_col, site_col, dataset_col, value_cols))) %>%
    pivot_longer(cols = all_of(value_cols), 
                 names_to = "variable", 
                 values_to = "value")
  
  plot_data$variable <- factor(plot_data$variable, levels = c("MAOM","AGG","POM","MIC","LMWC"))
  plot_data[[dataset_col]] <- factor(plot_data[[dataset_col]], levels = c("Tropical", "Temperate", "Arid"))
  
  # Calculate means by dataset and variable
  mean_data <- plot_data %>%
    group_by(!!sym(time_col), !!sym(dataset_col), variable) %>%
    summarise(
      mean_value = median(value, na.rm = TRUE),
      q05 = quantile(value, 0.05, na.rm = TRUE),  # 5th percentile
      q95 = quantile(value, 0.95, na.rm = TRUE),  # 95th percentile
      se = sd(value, na.rm = TRUE) / sqrt(n()),
      ci_lower = mean_value - 1.96 * se,  # 95% confidence interval
      ci_upper = mean_value + 1.96 * se,
      .groups = 'drop'
    )
  
  mean_data$variable <- factor(mean_data$variable, levels = c("MAOM","AGG","POM","MIC","LMWC"))
  mean_data[[dataset_col]] <- factor(mean_data[[dataset_col]], levels = c("Tropical", "Temperate", "Arid"))
  
  variable_colors <- c(
    "MAOM" = "#3C5488FF",
    "AGG"  = "#00A087FF",
    "POM"  = "#F39B7FFF",
    "MIC"  = "#4DBBD5FF",
    "LMWC" = "#E64B35FF"
  )
  
  # Create faceted plot with both variable and dataset facets
  pplot <- ggplot() +
    # Individual site lines - lighter and thinner for background
    geom_line(data = plot_data, 
              aes(x = !!sym(time_col), y = value, group = !!sym(site_col)), 
              color = "gray85", alpha = 0.4, size = 0.3, linetype = "solid") +
    # Confidence intervals - dashed ribbon borders with subtle fill
    geom_ribbon(data = mean_data,
                aes(x = !!sym(time_col), ymin = q05, ymax = q95, fill = variable),
                alpha = 0.25, color = "gray70", size = 0.4, linetype = "dashed") +
    # Mean lines - slightly thicker and more prominent
    geom_line(data = mean_data, 
              aes(x = !!sym(time_col), y = mean_value, color = variable), 
              size = 0.65, linetype = "solid") +
    # Facet by variable (rows) and dataset (columns)
    facet_grid(variable ~ dataset, scales = "free") +
    labs(title = "",
         x = ifelse(time_col == "days", "Time (day)", "Time (year)"), 
         y = expression("Fractions, (MgC ha"^{-1}*")")) +
    theme_minimal() +
    theme(
      panel.spacing = unit(0.2, "cm"),  # Add some spacing between panels
      panel.grid.major.y = element_line(color = "gray92", size = 0.3),  # Very light grid lines
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray92", size = 0.3),  # Light x-axis grid
      panel.grid.minor.x = element_blank(),
      panel.background = element_rect(fill = "white", colour = "gray75", size = 0.5),
      strip.background = element_blank(),
      # Strip text for variables (right side)
      strip.text.y = element_text(hjust = 0.5, face = "bold", size = 14),
      # Strip text for datasets (top)
      strip.text.x = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text = element_text(face = "bold", size = 12),  # Slightly smaller for readability
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    ) +
    scale_color_manual(
      values = variable_colors,
      guide = "none"  # Remove color legend
    ) +
    scale_fill_manual(
      values = variable_colors,
      guide = "none"  # Remove fill legend
    )
  
  #if (requireNamespace("ggh4x", quietly = TRUE)) {
  #  pplot <- pplot + ggh4x::facetted_pos_scales(
  #    y = lapply(names(y_limits), function(var) {
  #      scale_y_continuous(limits = y_limits[[var]])
  #    })
  #  )
  #}
  return(pplot)
}

prepare_combined_data_days <- function() {
  # Prepare top dataset
  df_top <- days_sim_outs_plot_top
  df_top[, c(1:5)] <- lapply(df_top[, c(1:5)], function(x){x/100})
  df_top$dataset <- "Tropical"
  
  # Prepare east dataset
  df_east <- days_sim_outs_plot_east
  df_east[, c(1:5)] <- lapply(df_east[, c(1:5)], function(x){x/100})
  df_east$dataset <- "Temperate"
  
  # Prepare west dataset
  df_west <- days_sim_outs_plot_west
  df_west[, c(1:5)] <- lapply(df_west[, c(1:5)], function(x){x/100})
  df_west$dataset <- "Arid"
  
  # Combine all datasets
  combined_data <- bind_rows(df_top, df_east, df_west)
  
  return(combined_data)
}

prepare_combined_data_years <- function() {
  # Prepare top dataset
  df_top <- years_sim_outs_plot_top
  df_top[, c(1:5)] <- lapply(df_top[, c(1:5)], function(x){x/100})
  df_top$dataset <- "Tropical"
  
  # Prepare east dataset
  df_east <- years_sim_outs_plot_east
  df_east[, c(1:5)] <- lapply(df_east[, c(1:5)], function(x){x/100})
  df_east$dataset <- "Temperate"
  
  # Prepare west dataset
  df_west <- years_sim_outs_plot_west
  df_west[, c(1:5)] <- lapply(df_west[, c(1:5)], function(x){x/100})
  df_west$dataset <- "Arid"
  
  # Combine all datasets
  combined_data <- bind_rows(df_top, df_east, df_west)
  
  return(combined_data)
}

combined_days_data  <- prepare_combined_data_days()
combined_years_data <- prepare_combined_data_years()

calculate_combined_y_limits <- function(data_list, value_cols) {
  all_values <- c()
  
  for (data in data_list) {
    values <- unlist(data[, value_cols])
    all_values <- c(all_values, values[!is.na(values)])
  }
  
  # Calculate overall range with some padding
  y_min <- min(all_values, na.rm = TRUE)
  y_max <- max(all_values, na.rm = TRUE)
  y_range <- y_max - y_min
  
  # Add 5% padding on each side
  return(c(y_min - 0.05 * y_range, y_max + 0.05 * y_range))
}

calculate_variable_y_limits <- function(data_list, value_cols) {
  y_limits <- list()
  
  for (col in value_cols) {
    all_values <- c()
    
    for (data in data_list) {
      values <- data[[col]]
      all_values <- c(all_values, values[!is.na(values)])
    }
    
    # Calculate range for this variable with some padding
    y_min <- min(all_values, na.rm = TRUE)
    y_max <- max(all_values, na.rm = TRUE)
    y_range <- y_max - y_min
    
    # Add 5% padding on each side
    y_limits[[col]] <- c(y_min - 0.05 * y_range, y_max + 0.05 * y_range)
  }
  
  return(y_limits)
}
value_cols <- c("MAOM","AGG","POM","MIC","LMWC")
#shared_y_limits <- calculate_combined_y_limits(list(combined_days_data, combined_years_data), value_cols)
variable_y_limits <- calculate_variable_y_limits(list(combined_days_data, combined_years_data), value_cols)


plots_days <- plot_timeseries_combined_faceted_update(combined_days_data, "days", c("POM","AGG","MAOM","MIC","LMWC"), "site", "dataset",variable_y_limits)
plots_years <- plot_timeseries_combined_faceted_update(combined_years_data, "years", c("POM","AGG","MAOM","MIC","LMWC"), "site", "dataset",variable_y_limits)

#plot_days <- plot_timeseries_combined_faceted_update(combined_days_data, "days", c("POM","AGG","MAOM","MIC","LMWC"), "site", "dataset")
#plot_years <- plot_timeseries_combined_faceted_update(combined_years_data, "years", c("POM","AGG","MAOM","MIC","LMWC"), "site", "dataset")
plots_years

grid.arrange(plots_days, plots_years, ncol = 2, widths = c(1, 2))
