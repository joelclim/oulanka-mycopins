make_pie_chart <- function(data, aes_y, aes_transect, text_label) {
    ggplot(
        data,
        aes(x = 1, y = aes_y, fill = aes_transect)
    ) +
    # Pie slices
    geom_col(width = 1, color = "white", linewidth = 0.7) +
    # Center labels within slices
    geom_text(
        aes(label = text_label),
        position = position_stack(vjust = 0.5),
        hjust = 0.5,
        vjust = 0.5,
        color = "white",
        size = 5,
        fontface = "bold",
        lineheight = 0.9
    ) +
    # Convert bar chart to pie chart
    coord_polar(theta = "y", start = 0, clip = "off") +
    # Consistent color scale
    scale_fill_viridis_d(
        option = "viridis",
        begin = 0.1,
        end = 0.9,
        drop = FALSE
    ) +
    # Remove axes and background
    theme_void()
}

display_gt_html <- function(gt_html) {
    gt_html %>%
    as_raw_html(inline_css = TRUE) %>%
    display_html()
}