clean_dataset <- function(community, environment) {
    clean_community <- as.data.frame(community)
    clean_environment <- as.data.frame(environment)

    rownames(clean_community) <- environment$Sample.Number
    rownames(clean_environment) <- environment$Sample.Number
    
    samples_with_zero_abundance <- rownames(clean_community)[rowSums(clean_community) == 0]

    clean_community = clean_community[
        !rownames(clean_community) %in% samples_with_zero_abundance, , drop = FALSE
    ]
    
    missing_samples <- setdiff(
        rownames(clean_community),
        rownames(clean_environment)
    )
    
    if (length(missing_samples) > 0) {
        stop(
            "Samples missing from environment: ",
            paste(missing_samples, collapse = ", ")
        )
    }
    
    clean_environment <- clean_environment[
        rownames(clean_community),
        ,
        drop = FALSE
    ]    
    
    return (list(
        community = clean_community,
        environment = clean_environment
    ))
}


# Overload as.data.frame to convert summary.simper to a single data frame
as.data.frame.summary.simper <- function(simper_summary, row.names = NULL, optional = FALSE, ...) {
    return (
        imap_dfr(simper_summary, function(x, contrast) {
        # Taxon names are stored as row names
        x <- x %>%
            rownames_to_column("Taxon")
    
        # Determine the two transects in each contrast
        groups <- str_split(contrast, "_", simplify = TRUE)
    
        x %>%
            mutate(
                Contrast = paste0(groups[1], "–", groups[2]),
                Group_1 = groups[1],
                Group_2 = groups[2],
    
                # Individual contribution to total dissimilarity
                Contribution_pct =
                    average / sum(average, na.rm = TRUE) * 100,
    
                # Cumulative SIMPER contribution
                Cumulative_pct = cumsum * 100,
    
                # Which transect has the greater mean abundance?
                Higher_in = case_when(
                    ava > avb ~ Group_1,
                    avb > ava ~ Group_2,
                    TRUE ~ "Equal"
                ),
    
                Significant = case_when(
                    is.na(p) ~ FALSE,
                    # p-value threshold of 0.05
                    p <= 0.05 ~ TRUE,
                    TRUE ~ FALSE
                )
            )
    }))
}


run_ordination <- function(configuration, rel_prop = FALSE) {
    suppressMessages(suppressWarnings({
        ordination = list()
        ordination$configuration <- configuration
        ordination$configuration$community_hellinger <- decostand(configuration$community,
                                                                  method = "hellinger")

        community <- configuration$community
        if (rel_prop) {
            community <- ordination$configuration$community_hellinger
        }
        # Calculate ecological distance
        ordination$dist <- vegdist(community, configuration$method)
        # Compute dissimilarities
        ordination$mds <- metaMDS(ordination$dist, 
                                  try = 1000, 
                                  distance = configuration$method, 
                                  k = 2)
        
        # Hypothesis Test (PERMANOVA)
        formula <- reformulate(configuration$variable, response = "ordination$dist")
        ordination$permanova <- adonis2(formula, data = configuration$environment)
    
        # Diagnostic Hypothesis Test (Dispersion)
        ordination$dispersion_model <- betadisper(ordination$dist, 
                                       configuration$environment[[configuration$variable]])
        ordination$dispersion <- permutest(ordination$dispersion_model, 
                                           permutations = 999)
        # Which pairs of group means are statistically significantly different
        ordination$dispersion_tukey_hsd <- TukeyHSD(ordination$dispersion_model)
    
        # Hypothesis Test (ANOSIM)
        ordination$anosim <- anosim(ordination$dist, 
                                    configuration$environment[[configuration$variable]])
        
        # Hypothesis Test results
        ordination$hypothesis_test <- list(
            "permanova.p_value" = ordination$permanova["Model", "Pr(>F)"],
            "permanova.r_squared" = ordination$permanova["Model", "R2"],
            "betadisper.p_value" = ordination$dispersion$tab["Groups", "Pr(>F)"],
            "anosim.p_value" = ordination$anosim$signif,
            "anosim.r_statistic" = ordination$anosim$statistic
        )
        
        # Fit an environmental vector on the ordination plot
        ordination$vector <- envfit(ordination$mds, 
                                    configuration$environment[configuration$vector], 
                                    perm = 1000, 
                                    arrows = TRUE)
    }))
    
    return(ordination)
}



nmds_plot <- function(options) {    
    plot <- ordiplot(options$mds, 
                     type = options$graph_type, 
                     xlim = options$xlim, 
                     ylim = options$ylim)
    if ("ellipse" == options$draw) {
      ordiellipse(options$mds, 
                  groups = options$groups,
                  draw = "polygon", 
                  col = options$colors, 
                  label = options$draw_label)
    
    } else if ("hull" == options$draw) {
      ordihull(options$mds, 
               groups = options$groups,
               draw = "polygon", 
               col = options$colors, 
               label = options$draw_label)
    }
    
    if (options$show_sites) {
      orditorp(options$mds, 
               display = "sites", 
               cex = 0.78, 
               air = 0.01)
    }
    if (options$show_species) {
      orditorp(options$mds, 
               display = "species", 
               cex = 0.50, 
               col = "red", 
               air = 0.01)
    }
}