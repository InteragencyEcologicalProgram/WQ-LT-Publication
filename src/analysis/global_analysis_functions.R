# WQ-LT Drought Publication
# Purpose: Global functions to be used across analyses for the WQ-LT drought publication
# Authors: Sam Bashevkin, Dave Bosworth
# Contacts: Sam.Bashevkin@Waterboards.ca.gov; David.Bosworth@water.ca.gov

library(magrittr)
library(patchwork)


# Create model diagnostic plots to check assumptions
model_plotter <- function(model, data, parameter) {
  data <- data %>%
    dplyr::filter(!is.na(.data[[parameter]])) %>%
    dplyr::mutate(
      Residuals = stats::resid(model),
      Fitted = stats::predict(model)
    )

  units <- dplyr::case_when(
    parameter == "Temperature" ~ " (°C)",
    parameter == "Salinity_l" ~ " (log)",
    parameter == "Secchi_l" ~ " (log)",
    parameter == "DissAmmonia_l" ~ " (log)",
    parameter == "DissNitrateNitrite_l" ~ " (log)",
    parameter == "DissOrthophos_l" ~ " (log)",
    parameter == "Chlorophyll_l" ~ " (log)"
  )

  parameter_label <- dplyr::case_when(
    parameter == "Secchi_l" ~ "secchi depth",
    parameter == "Salinity_l" ~ "salinity",
    parameter == "Temperature" ~ "temperature",
    parameter == "DissAmmonia_l" ~ "ammonia",
    parameter == "DissNitrateNitrite_l" ~ "nitrate",
    parameter == "DissOrthophos_l" ~ "phosphate",
    parameter == "Chlorophyll_l" ~ "chlorophyll"
  )

  p_hist <- ggplot2::ggplot(data, ggplot2::aes(x = .data$Residuals)) +
    ggplot2::geom_histogram() +
    ggplot2::xlab(paste0("Residuals", units)) +
    ggplot2::theme_bw()

  p_res_fit <- ggplot2::ggplot(data, ggplot2::aes(x = .data$Residuals, y = .data$Fitted)) +
    ggplot2::geom_point() +
    ggplot2::ylab(paste0("Predicted ", parameter_label, units)) +
    ggplot2::xlab(paste0("Residuals", units)) +
    ggplot2::theme_bw()

  p_obs_fit <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[parameter]], y = .data$Fitted)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::ylab(paste0("Predicted ", parameter_label, units)) +
    ggplot2::xlab(paste0("Observed ", parameter_label, units)) +
    ggplot2::theme_bw()

  out <-
    (p_hist + patchwork::plot_layout(ncol = 1)) +
    (p_res_fit + p_obs_fit + patchwork::plot_layout(ncol = 2)) +
    patchwork::plot_layout(nrow = 2, widths = c(1, 0.5, 0.5))

  return(out)
}


# Calculate Partial R2 for the Drought main effect. Derived from this post:
# https://stats.stackexchange.com/questions/64010/importance-of-predictors-in-multiple-regression-partial-r2-vs-standardized
partial.r2 <- function(ANOVA) {
  factors <- paste0("Drought", c("", ":Season", ":Region"))
  r2 <- sum(ANOVA[factors, "Sum Sq"]) / (sum(ANOVA[factors, "Sum Sq"]) + ANOVA["Residuals", "Sum Sq"])
  return(r2)
}


# Create WQ, nutrient, and chlorophyll figures for the publication
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
  lvs_region <- c("Suisun Marsh", "Suisun Bay", "Confluence", "South-Central Delta", "North Delta")
  lvs_drought <- c("D", "N", "W")

  # Prepare df_data for plotting
  df_data_c <- df_data %>%
    # Remove NA values in param
    tidyr::drop_na({{ param }}) %>%
    # Apply factor orders
    dplyr::mutate(
      # Make sure that we're using region names "South-Central Delta" and "North Delta"
      Region = dplyr::case_when(
        Region == "SouthCentral" ~ "South-Central Delta",
        Region == "North" ~ "North Delta",
        TRUE ~ .data$Region
      ),
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
    dplyr::mutate(.group = stringr::str_remove_all(.data$.group, stringr::fixed(" "))) %>%
    dplyr::left_join(
      df_data_c %>%
        dplyr::group_by({{ fct_grp }}) %>%
        dplyr::summarize(
          max_val = max({{ param }}),
          min_val = min({{ param }}),
          .groups = "drop"
        ),
      by = c(fct_grp_chr)
    ) %>%
    # Back transform log-transformed results if log_trans = TRUE
    {if (log_trans == TRUE) {
      dplyr::mutate(., dplyr::across(c(emmean, lower.CL, upper.CL), exp))
    } else {
      .
    }} %>%
    # Determine vertical positioning of letters
    dplyr::mutate(
      max_val = dplyr::if_else(.data$upper.CL > .data$max_val, .data$upper.CL, .data$max_val)
    ) %>%
    dplyr::group_by({{ fct_grp }}) %>%
    dplyr::mutate(max_val = max(.data$max_val)) %>%
    ungroup() %>%
    dplyr::mutate(y_pos = .data$max_val + (.data$max_val - .data$min_val) / 10) %>%
    # Apply factor order to Drought
    dplyr::mutate(Drought = factor(.data$Drought, levels = lvs_drought)) %>%
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
    ggplot2::geom_crossbar(color = "grey82", fill = "grey", alpha = 0.7, linewidth = 0.1) +
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

# Combine and format plots created from pub_figure_plotter
# ls_plt is a list of the plot objects to combine, ordered from top to bottom
# Changes x-axis label of bottom-most plot to "Drought Classification", and
  # removes the x-axis labels of the other plots
comb_pub_figs <- function(ls_plt) {
  # Determine index positions of bottom-most and other plots
  pos_bott_plt <- length(ls_plt)
  pos_top_plts <- seq.int(1, pos_bott_plt - 1)

  ls_plt %>%
    purrr::modify_at(pos_top_plts, ~ .x + ggplot2::xlab(NULL)) %>%
    purrr::modify_at(pos_bott_plt, ~ .x + ggplot2::xlab("Drought Classification")) %>%
    patchwork::wrap_plots(ncol = 1)
}

# Define color palette for the drought classifications
color_pal_drought <- function(aes_type = c("fill", "color"), scale_title = "Drought\nClassification") {
  # Evaluate choices for aes_type
  aes_type <- match.arg(aes_type, c("fill", "color"))

  # Define color palette and labels for the Drought categories
  pal_drought <- c("D" = "#FDE333", "N" = "#53CC67", "W" = "#00588B")
  labs_drought <- c("Drought", "Neutral", "Wet")

  # Apply palette based on aes_type
  if (aes_type == "fill") {
    ggplot2::scale_fill_manual(name = scale_title, values = pal_drought, labels = labs_drought)
  } else {
    ggplot2::scale_color_manual(name = scale_title, values = pal_drought, labels = labs_drought)
  }
}

# Define color palette for the year type classifications
color_pal_yrtype <- function(aes_type = c("fill", "color"), scale_title = "Year Type") {
  # Evaluate choices for aes_type
  aes_type <- match.arg(aes_type, c("fill", "color"))

  # Define color palette for the Year Type categories
  pal_yrtype <- c(
    "Critical" = "#FDE333",
    "Dry" = "#53CC67",
    "Below Normal" = "#009B95",
    "Above Normal" = "#00588B",
    "Wet" = "#4B0055"
  )

  # Apply palette based on aes_type
  if (aes_type == "fill") {
    ggplot2::scale_fill_manual(name = scale_title, values = pal_yrtype)
  } else {
    ggplot2::scale_color_manual(name = scale_title, values = pal_yrtype)
  }
}
