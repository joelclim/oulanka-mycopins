suppressMessages( 
    {
        library(dplyr)
        library(DT)
        library(ggplot2)
        library(gt)
        library(iNEXT)
        library(IRdisplay) 
        library(labdsv)
        library(patchwork)
        library(readr)
        library(scales)
        library(stringr)
        library(tibble)
        library(tidyr)
        library(vegan)
        library(viridis)
    }
)

DATA_DIR = "../data"
OULANKA_FILE = paste(DATA_DIR, "/oulanka_mycopins.csv", sep="")
COMMUNITY_FILE = paste(DATA_DIR, "/oulanka_mycopins_community.csv", sep="")
ENVIRONMENT_FILE = paste(DATA_DIR, "/oulanka_mycopins_environment.csv", sep="")
ORGANISMS_FILE = paste(DATA_DIR, "/oulanka_mycopins_organisms.csv", sep="")
SAMPLE_CLUSTERS_FILE = paste(DATA_DIR, "/oulanka_mycopins_sample_clusters.csv", sep="")

get_community_data <- function() {
  return (read_csv(COMMUNITY_FILE, show_col_types = FALSE,
      locale = locale(encoding = "UTF-8")))
}


get_environment_data <- function() {
  return (read_csv(ENVIRONMENT_FILE, show_col_types = FALSE,
      locale = locale(encoding = "UTF-8")))
}


get_data <- function() {
    environment_df <- get_environment_data()
    community_df <- get_community_data()
    oulanka_df <- cbind(environment_df, community_df)
    return (oulanka_df)
}

get_environment_variables <- function(exclude_list = c("Tag", "Date.Collected")) {
  mycopins.env <- get_environment_data()

  env_vars <- names(mycopins.env)
  env_vars <- env_vars[!env_vars %in% exclude_list]

  return (env_vars)
}


get_organisms_data <- function() {
  return (read_csv(ORGANISMS_FILE, show_col_types = FALSE,
      locale = locale(encoding = "UTF-8")))
}

                 
get_saproxylic_community_data <- function(genus=FALSE, selected = c()) {
    community_df <- get_community_data()
    organisms_df <- get_organisms_data()
    saproxylic_organisms <- 
      organisms_df %>%
        filter (is_saproxylic == 'yes')

    saproxylic_community_df <- community_df[names(community_df) %in% saproxylic_organisms$organism]
    if (!genus) {
        if (length(selected) > 0) {
            return (saproxylic_community_df[names(saproxylic_community_df) %in% selected])
        }
        return (saproxylic_community_df)
    }

    return (get_genus_saproxylic_community_data(saproxylic_community_df, selected))
}

get_genus_saproxylic_community_data <- function(saproxylic_community_df, selected) {
    # Take the species names
    species_names <- names(saproxylic_community_df)
    # Take the genus names from the species names
    genus_names <- sub(" .*", "", species_names)
    
    # Tranpose to sum counts column-wise
    t_saproxylic_community_df <- as.data.frame(t(saproxylic_community_df))
    
    # Add the Genus name column for grouping
    t_saproxylic_community_df$Genus <- genus_names
    
    # Sum the counts of the same genus name
    t_saproxylic_community_df <- 
      t_saproxylic_community_df %>%
        group_by(Genus) %>%
        summarise(across(1:last_col(), sum))
    
    # Take the reduced genus names
    genus_names <- t_saproxylic_community_df$Genus

    # Ready to restore the data frame. Remove the genus column 
    t_saproxylic_community_df <-
        t_saproxylic_community_df %>%
            select(-Genus)
    
    # Restore the data frame. Transpose the transposed.
    saproxylic_community_df <- as.data.frame(t(t_saproxylic_community_df), row.names = NULL)
    
    # Use the reduced genus names as the column names
    names(saproxylic_community_df) <- genus_names

    # Clear the row names
    rownames(saproxylic_community_df) <- NULL
    
    if (length(selected) > 0) {
        return (saproxylic_community_df[names(saproxylic_community_df) %in% selected])
    }
    
    return (saproxylic_community_df)
}
                 
get_sample_clusters_data <- function() {
  return (read_csv(SAMPLE_CLUSTERS_FILE, show_col_types = FALSE,
      locale = locale(encoding = "UTF-8")))
}
