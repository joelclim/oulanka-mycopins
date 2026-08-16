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


make_gginext_labels_and_themes <- function(title, subtitle, caption) {
    list(
        labs(
            title = str_wrap(title_text, width = 125),
            subtitle = str_wrap(subtitle_text, width = 100),
            caption = str_wrap(caption_text, width = 125)
        ),
        theme(
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 14, color = "grey30", margin = margin(b=30)),
            strip.text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(1.25, "lines"),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 11),
            legend.position = "right",
            plot.caption = element_text(hjust = 0.5, size = 11, margin = margin(t=30)),
        )
    )
}


display_gt_html <- function(gt_html) {
    gt_html %>%
    as_raw_html(inline_css = TRUE) %>%
    display_html()
}