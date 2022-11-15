# Function to create WQ, nutrient, and chlorophyll figures for the publication

library(tidyverse)
library(multcomp)
library(emmeans)
library(rlang)


pub_figure_plotter <- function(df_data, # dataframe containing the dataset
                               param, # response variable
                               y_label, # label for y-axis (response variable and units)
                               fct_grp, # grouping for plot (either Region or Season)
                               model, # linear model of response variable versus predictor factors
                               plt_title, # label for the plot title
                               log_trans = FALSE, # is the response variable log transformed?
                               print_plt = TRUE) {

  # Set local variables to NULL
  . <- NULL

  # Convert fct_grp to a character string
  fct_grp_chr <- rlang::as_name(rlang::ensym(fct_grp))

  # Define factor order for Season, Region, and Drought variables
  lvs_season <- c("Winter", "Spring", "Summer", "Fall")
  lvs_region <- c("Suisun Marsh", "Suisun Bay", "Confluence", "SouthCentral", "North")
  lvs_drought <- c("D", "N", "W")

  # Prepare df_data for plotting
  df_data_c <- df_data %>%
    # Remove NA values in param
    tidyr::drop_na({{ param }}) %>%
    # Apply factor orders
    dplyr::mutate(
      Season = factor(.data$Season, levels = lvs_season),
      Region = factor(.data$Region, levels = lvs_region),
      Drought = factor(.data$Drought, levels = lvs_drought)
    )

  # Run emmeans Tukey post-hoc for all pairwise comparisons between Drought
    # classifications for each fct_grp
  emm_tuk <- emmeans::emmeans(model, as.formula(paste0("pairwise ~ Drought|", fct_grp_chr)))

  # Add significance grouping letters from the Tukey post-hoc results and
    # calculate min and max values to determine vertical positioning
  emm_tuk_c <- emm_tuk$emmeans %>%
    multcomp::cld(sort = FALSE, Letters = letters) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(.group = stringr::str_remove_all(.data$.group, fixed(" "))) %>%
    dplyr::left_join(
      df_data_c %>%
        dplyr::group_by(.data$Drought, {{ fct_grp }}) %>%
        dplyr::summarize(
          max_val = max({{ param }}),
          min_val = min({{ param }}),
          .groups = "drop"
        ),
      by = c("Drought", fct_grp_chr)
    ) %>%
    # Back transform log-transformed results if log_trans = TRUE
    {if (log_trans == TRUE) {
      dplyr::mutate(., dplyr::across(c(emmean, lower.CL, upper.CL), exp))
    } else {
      .
    }} %>%
    dplyr::mutate(
      # Determine vertical positioning of letters
      max_val = dplyr::if_else(.data$upper.CL > .data$max_val, .data$upper.CL, .data$max_val),
      y_pos = .data$max_val + (.data$max_val - .data$min_val) / 10,
      # Apply factor order to Drought
      Drought = factor(.data$Drought, levels = lvs_drought)
    ) %>%
    # Apply factor order to fct_grp
    {if (fct_grp_chr == "Season") {
      dplyr::mutate(., "{{fct_grp}}" := factor({{ fct_grp }}, levels = lvs_season))
    } else if (fct_grp_chr == "Region") {
      dplyr::mutate(., "{{fct_grp}}" := factor({{ fct_grp }}, levels = lvs_region))
    }}

  # Create boxplot showing Tukey post-hoc results for the fct_grp (Region or
    # Season)
  plt <- emm_tuk_c %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$Drought,
        y = .data$emmean,
        ymin = .data$lower.CL,
        ymax = .data$upper.CL,
        label = .data$.group
      )
    ) +
    ggplot2::geom_boxplot(
      data = df_data_c,
      ggplot2::aes(x = .data$Drought, y = {{ param }}),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_crossbar(color = "grey82", fill = "grey", alpha = 0.7, size = 0.1) +
    ggplot2::geom_point(color = "red") +
    ggplot2::geom_text(ggplot2::aes(y = y_pos)) +
    ggplot2::facet_wrap(ggplot2::vars({{ fct_grp }}), scales = "free_y", nrow = 1) +
    ggplot2::ylab(y_label) +
    ggplot2::ggtitle(plt_title) +
    ggplot2::theme_bw()

  # Calculate effect sizes between D and W
  diffs <- emm_tuk_c %>%
    dplyr::filter(.data$Drought %in% c("D", "W")) %>%
    dplyr::select(Drought, {{ fct_grp }}, emmean) %>%
    tidyr::pivot_wider(names_from = Drought, values_from = emmean) %>%
    dplyr::mutate(difference = D - W) %>%
    dplyr::arrange({{ fct_grp }})

  # Format contrasts from emmeans
  contrasts <- tibble::as_tibble(emm_tuk$contrasts) %>%
    # Apply factor order to fct_grp
    {if (fct_grp_chr == "Season") {
      dplyr::mutate(., "{{fct_grp}}" := factor({{ fct_grp }}, levels = lvs_season))
    } else if (fct_grp_chr == "Region") {
      dplyr::mutate(., "{{fct_grp}}" := factor({{ fct_grp }}, levels = lvs_region))
    }} %>%
    dplyr::arrange({{ fct_grp }})

  if (print_plt == TRUE) print(plt)

  invisible(
    tibble::lst(
      plt,
      diffs,
      contrasts
    )
  )
}

