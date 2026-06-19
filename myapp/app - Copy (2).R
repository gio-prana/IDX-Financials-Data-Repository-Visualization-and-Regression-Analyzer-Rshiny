# ============================================================
# STATCAL ONLINE - IDX Financials Data Repository and Visualization Analyzer
# R Shiny Version
# ============================================================
# Required packages:
# install.packages(c(
#   "shiny", "shinydashboard", "DT", "readxl", "dplyr", "ggplot2",
#   "shinycssloaders", "lmtest", "car", "moments"
# ))

required_packages <- c(
  "shiny", "shinydashboard", "DT", "readxl", "dplyr", "ggplot2",
  "shinycssloaders", "lmtest", "car", "moments"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following R packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(lmtest)
library(car)
library(moments)
library(scales)

# ============================================================
# CONSTANTS
# ============================================================

APP_NAME <- "STATCAL ONLINE"
APP_TITLE <- "IDX Financials Data Repository, Visualization, and Regression Analyzer"
APP_UPDATED <- "Last updated on June 19, 2026"
WEBSITE_URL <- "https://statcal.com/"
STATCAL_ONLINE_URL <- "https://statcal.com/statcal%20online.html"
TRAINING_DATA_URL <- "https://drive.google.com/drive/folders/1s273Ad5FUElhzd5G16jWSBxbOtforzRR?usp=sharing"
IDX_STOCK_LIST_URL <- "https://www.idx.id/id/data-pasar/data-saham/daftar-saham/"
SAMPLE_DATA_PATH <- "data idx perbankan.xlsx"
LOGO_PATH <- "logo_statcal.png"
FINANCIAL_URL_COL <- "Financial Statement URL"

THEMES <- list(
  "White Publication" = list(
    figure_facecolor = "white", axes_facecolor = "white", text_color = "#111111",
    grid_color = "#D9D9D9", spine_color = "#222222", point_color = "#1F4E79"
  ),
  "Light Gray Editorial" = list(
    figure_facecolor = "#F7F7F7", axes_facecolor = "#FFFFFF", text_color = "#111111",
    grid_color = "#D0D0D0", spine_color = "#333333", point_color = "#1F4E79"
  ),
  "Warm Ivory Journal" = list(
    figure_facecolor = "#FBF7EF", axes_facecolor = "#FFFDF8", text_color = "#1F1F1F",
    grid_color = "#DDD4C4", spine_color = "#3A3A3A", point_color = "#8B4513"
  ),
  "Cool Blue Scientific" = list(
    figure_facecolor = "#F3F7FB", axes_facecolor = "#FFFFFF", text_color = "#0B1F33",
    grid_color = "#C8D6E5", spine_color = "#1F4E79", point_color = "#1F4E79"
  ),
  "Dark Navy Presentation" = list(
    figure_facecolor = "#0B1320", axes_facecolor = "#111C2E", text_color = "#FFFFFF",
    grid_color = "#3B4A5F", spine_color = "#B8C7D9", point_color = "#BBE1FA"
  )
)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

clean_dataframe <- function(df) {
  names(df) <- trimws(gsub("\\s+", " ", as.character(names(df))))
  df <- df[rowSums(is.na(df)) < ncol(df), , drop = FALSE]
  unnamed_cols <- grepl("^unnamed", tolower(names(df)))
  if (any(unnamed_cols)) {
    keep_unnamed <- vapply(df[unnamed_cols], function(x) !all(is.na(x)), logical(1))
    drop_names <- names(df)[unnamed_cols][!keep_unnamed]
    if (length(drop_names) > 0) df <- df[, !names(df) %in% drop_names, drop = FALSE]
  }
  rownames(df) <- NULL
  df
}

make_display_safe <- function(df) {
  df <- as.data.frame(df)
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  df
}

to_numeric_vector <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  txt <- as.character(x)
  txt <- gsub(",", "", txt, fixed = TRUE)
  txt <- gsub("−", "-", txt, fixed = TRUE)
  txt <- gsub("—", "", txt, fixed = TRUE)
  txt <- trimws(txt)
  txt[txt %in% c("", "nan", "NaN", "None", "NaT", "-", "N/A", "NA", "n/a", "na")] <- NA

  convert_one <- function(v) {
    if (is.na(v) || v == "") return(NA_real_)
    v <- trimws(v)
    negative <- FALSE
    if (grepl("^\\(.*\\)$", v)) {
      negative <- TRUE
      v <- sub("^\\(", "", sub("\\)$", "", v))
    }
    multiplier <- 1
    last_char <- tolower(substr(v, nchar(v), nchar(v)))
    if (last_char == "k") {
      multiplier <- 1000
      v <- substr(v, 1, nchar(v) - 1)
    } else if (last_char == "m") {
      multiplier <- 1000000
      v <- substr(v, 1, nchar(v) - 1)
    } else if (last_char == "b") {
      multiplier <- 1000000000
      v <- substr(v, 1, nchar(v) - 1)
    } else if (last_char == "t") {
      multiplier <- 1000000000000
      v <- substr(v, 1, nchar(v) - 1)
    }
    v <- trimws(gsub("%", "", v, fixed = TRUE))
    out <- suppressWarnings(as.numeric(v))
    if (is.na(out)) return(NA_real_)
    out <- out * multiplier
    if (negative) out <- -out
    out
  }

  vapply(txt, convert_one, numeric(1))
}

detect_numeric_columns <- function(df, min_valid_ratio = 0.45) {
  numeric_cols <- character(0)
  for (nm in names(df)) {
    non_null <- sum(!is.na(df[[nm]]))
    if (non_null == 0) next
    numeric_v <- to_numeric_vector(df[[nm]])
    valid_ratio <- sum(!is.na(numeric_v)) / max(non_null, 1)
    if (valid_ratio >= min_valid_ratio) numeric_cols <- c(numeric_cols, nm)
  }
  numeric_cols
}

sorted_unique_values <- function(x) {
  vals <- unique(x[!is.na(x)])
  vals[order(as.character(vals))]
}

default_numeric_columns <- function(cols, n = 5) {
  valid <- cols[!tolower(trimws(cols)) %in% c("year", "tahun")]
  if (length(valid) == 0) return(character(0))
  valid[seq_len(min(n, length(valid)))]
}

compact_number <- function(x, digits = 2) {
  out <- ifelse(is.na(x), NA_character_, as.character(round(x, digits)))
  absx <- abs(x)
  out <- ifelse(!is.na(x) & absx >= 1e12, paste0(round(x / 1e12, digits), "T"), out)
  out <- ifelse(!is.na(x) & absx >= 1e9 & absx < 1e12, paste0(round(x / 1e9, digits), "B"), out)
  out <- ifelse(!is.na(x) & absx >= 1e6 & absx < 1e9, paste0(round(x / 1e6, digits), "M"), out)
  out <- ifelse(!is.na(x) & absx >= 1e3 & absx < 1e6, paste0(round(x / 1e3, digits), "K"), out)
  out
}

safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_median <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
safe_sd <- function(x) if (sum(!is.na(x)) > 1) sd(x, na.rm = TRUE) else NA_real_
safe_var <- function(x) if (sum(!is.na(x)) > 1) var(x, na.rm = TRUE) else NA_real_
safe_skew <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 2 || sd(x) == 0) return(NA_real_)
  mean(((x - mean(x)) / sd(x))^3)
}
safe_kurtosis <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 3 || sd(x) == 0) return(NA_real_)
  mean(((x - mean(x)) / sd(x))^4) - 3
}

round_numeric_df <- function(df, digits) {
  df <- as.data.frame(df)
  numeric_cols <- vapply(df, is.numeric, logical(1))
  df[numeric_cols] <- lapply(df[numeric_cols], round, digits = digits)
  df
}

get_theme <- function(theme_name) {
  theme <- THEMES[[theme_name]]
  if (is.null(theme)) theme <- THEMES[["White Publication"]]
  theme
}

legend_position_value <- function(position_label) {
  if (is.null(position_label) || position_label == "None / Hide legend") return("none")
  tolower(position_label)
}

statcal_theme_gg <- function(theme_name,
                             title_size = 16,
                             subtitle_size = 11,
                             axis_title_size = 11,
                             axis_text_size = 9,
                             panel_title_size = 11,
                             legend_title_size = 10,
                             legend_text_size = 9,
                             legend_position = "Right",
                             x_text_angle = 45) {
  th <- get_theme(theme_name)
  theme_minimal(base_size = axis_text_size) +
    theme(
      plot.background = element_rect(fill = th$figure_facecolor, color = NA),
      panel.background = element_rect(fill = th$axes_facecolor, color = NA),
      panel.grid.major = element_line(color = th$grid_color, linewidth = 0.35),
      panel.grid.minor = element_line(color = th$grid_color, linewidth = 0.15),
      axis.text = element_text(color = th$text_color, size = axis_text_size),
      axis.text.x = element_text(angle = x_text_angle, hjust = ifelse(x_text_angle == 0, 0.5, 1), color = th$text_color, size = axis_text_size),
      axis.title = element_text(color = th$text_color, face = "bold", size = axis_title_size),
      plot.title = element_text(color = th$text_color, face = "bold", size = title_size),
      plot.subtitle = element_text(color = th$text_color, size = subtitle_size),
      strip.text = element_text(color = th$text_color, face = "bold", size = panel_title_size),
      legend.text = element_text(color = th$text_color, size = legend_text_size),
      legend.title = element_text(color = th$text_color, face = "bold", size = legend_title_size),
      legend.background = element_rect(fill = th$figure_facecolor, color = NA),
      legend.position = legend_position_value(legend_position)
    )
}

# ============================================================
# DESCRIPTIVE STATISTICS
# ============================================================

compute_descriptive_statistics <- function(df, numeric_cols, decimal_digits) {
  rows <- lapply(numeric_cols, function(nm) {
    s <- to_numeric_vector(df[[nm]])
    data.frame(
      Variable = nm,
      N = sum(!is.na(s)),
      Minimum = safe_min(s),
      Maximum = safe_max(s),
      Mean = safe_mean(s),
      Median = safe_median(s),
      Standard_Deviation = safe_sd(s),
      Variance = safe_var(s),
      Skewness = safe_skew(s),
      Kurtosis = safe_kurtosis(s),
      check.names = FALSE
    )
  })
  out <- bind_rows(rows)
  names(out) <- gsub("_", " ", names(out))
  round_numeric_df(out, decimal_digits)
}

compute_grouped_descriptive_statistics <- function(df, numeric_cols, group_cols, decimal_digits) {
  if (length(group_cols) == 0) return(compute_descriptive_statistics(df, numeric_cols, decimal_digits))

  rows <- lapply(numeric_cols, function(nm) {
    tmp <- df[, group_cols, drop = FALSE]
    tmp$.value <- to_numeric_vector(df[[nm]])
    out <- tmp %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(
        N = sum(!is.na(.value)),
        Minimum = safe_min(.value),
        Maximum = safe_max(.value),
        Mean = safe_mean(.value),
        Median = safe_median(.value),
        `Standard Deviation` = safe_sd(.value),
        .groups = "drop"
      )
    out <- as.data.frame(out)
    out <- cbind(out[, group_cols, drop = FALSE], Variable = nm, out[, setdiff(names(out), group_cols), drop = FALSE])
    out
  })
  round_numeric_df(bind_rows(rows), decimal_digits)
}

# ============================================================
# VISUALIZATION FUNCTIONS
# ============================================================

build_mean_line_data <- function(df, x_col, numeric_cols, split_col = "None", sort_x = TRUE) {
  rows <- lapply(numeric_cols, function(nm) {
    use_split <- !is.null(split_col) && split_col != "None" && split_col %in% names(df)
    if (use_split) {
      tmp <- data.frame(
        X = df[[x_col]],
        Split = as.character(df[[split_col]]),
        Value = to_numeric_vector(df[[nm]]),
        Variable = nm,
        stringsAsFactors = FALSE
      )
      tmp <- tmp[!is.na(tmp$X) & !is.na(tmp$Value), , drop = FALSE]
      tmp %>%
        group_by(X, Split, Variable) %>%
        summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")
    } else {
      tmp <- data.frame(
        X = df[[x_col]],
        Split = nm,
        Value = to_numeric_vector(df[[nm]]),
        Variable = nm,
        stringsAsFactors = FALSE
      )
      tmp <- tmp[!is.na(tmp$X) & !is.na(tmp$Value), , drop = FALSE]
      tmp %>%
        group_by(X, Split, Variable) %>%
        summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")
    }
  })
  out <- bind_rows(rows)
  if (nrow(out) == 0) return(out)
  out$X_Label <- as.character(out$X)
  if (sort_x) {
    suppressWarnings(x_num <- as.numeric(out$X_Label))
    if (all(is.na(x_num))) {
      levels_x <- sort(unique(out$X_Label))
    } else {
      order_df <- data.frame(label = out$X_Label, num = x_num, stringsAsFactors = FALSE)
      order_df <- order_df[order(order_df$num, order_df$label), , drop = FALSE]
      levels_x <- unique(order_df$label)
    }
    out$X_Label <- factor(out$X_Label, levels = levels_x)
  }
  out
}

create_panel_line_plot <- function(line_df, title, subtitle, x_label, y_label,
                                   theme_name, show_points = TRUE, line_width = 1.1,
                                   point_size = 2.4, show_value_labels = FALSE,
                                   value_label_size = 3.0, compact_labels = TRUE,
                                   share_y = FALSE, panel_cols = 2,
                                   legend_position = "Right",
                                   title_size = 16, subtitle_size = 11,
                                   axis_title_size = 11, axis_text_size = 9,
                                   panel_title_size = 11, legend_title_size = 10,
                                   legend_text_size = 9, x_text_angle = 45) {
  validate(need(nrow(line_df) > 0, "Line chart data is empty."))
  p <- ggplot(line_df, aes(x = X_Label, y = Mean, group = Split, color = Split)) +
    geom_line(linewidth = line_width, alpha = 0.95)
  if (show_points) {
    p <- p + geom_point(size = point_size, alpha = 0.95)
  }
  if (show_value_labels) {
    line_df$.Label <- if (compact_labels) compact_number(line_df$Mean, digits = 2) else as.character(round(line_df$Mean, 3))
    p <- p + geom_text(data = line_df, aes(label = .Label), vjust = -0.8, size = value_label_size, show.legend = FALSE)
  }
  scales_y <- if (share_y) "fixed" else "free_y"
  p +
    facet_wrap(~ Variable, scales = scales_y, ncol = panel_cols) +
    labs(title = title, subtitle = subtitle, x = x_label, y = y_label, color = "Group") +
    statcal_theme_gg(
      theme_name = theme_name,
      title_size = title_size,
      subtitle_size = subtitle_size,
      axis_title_size = axis_title_size,
      axis_text_size = axis_text_size,
      panel_title_size = panel_title_size,
      legend_title_size = legend_title_size,
      legend_text_size = legend_text_size,
      legend_position = legend_position,
      x_text_angle = x_text_angle
    )
}

compute_correlation_results <- function(df, numeric_cols, method = "pearson", digits = 3) {
  num_df <- as.data.frame(lapply(numeric_cols, function(nm) to_numeric_vector(df[[nm]])))
  names(num_df) <- numeric_cols
  corr <- cor(num_df, use = "pairwise.complete.obs", method = method)
  pvals <- matrix(NA_real_, nrow = length(numeric_cols), ncol = length(numeric_cols), dimnames = list(numeric_cols, numeric_cols))
  for (i in seq_along(numeric_cols)) {
    for (j in seq_along(numeric_cols)) {
      pair <- num_df[, c(i, j)]
      pair <- pair[complete.cases(pair), , drop = FALSE]
      if (nrow(pair) >= 3) {
        test <- tryCatch(cor.test(pair[[1]], pair[[2]], method = method), error = function(e) NULL)
        if (!is.null(test)) pvals[i, j] <- test$p.value
      }
    }
  }
  list(
    corr = round(corr, digits),
    pvals = round(pvals, digits),
    corr_table = cbind(Variable = rownames(corr), as.data.frame(round(corr, digits), check.names = FALSE)),
    pval_table = cbind(Variable = rownames(pvals), as.data.frame(round(pvals, digits), check.names = FALSE))
  )
}

create_correlation_heatmap <- function(corr_matrix, title, theme_name,
                                       show_values = TRUE, value_label_size = 3,
                                       legend_position = "Right",
                                       title_size = 16, axis_text_size = 9,
                                       legend_title_size = 10, legend_text_size = 9,
                                       x_text_angle = 45) {
  th <- get_theme(theme_name)
  corr_df <- as.data.frame(as.table(corr_matrix), stringsAsFactors = FALSE)
  names(corr_df) <- c("Var1", "Var2", "Correlation")
  p <- ggplot(corr_df, aes(x = Var2, y = Var1, fill = Correlation)) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-1, 1), na.value = "grey90") +
    labs(title = title, x = NULL, y = NULL, fill = "Correlation") +
    coord_fixed() +
    statcal_theme_gg(
      theme_name = theme_name,
      title_size = title_size,
      subtitle_size = 10,
      axis_title_size = 10,
      axis_text_size = axis_text_size,
      panel_title_size = 10,
      legend_title_size = legend_title_size,
      legend_text_size = legend_text_size,
      legend_position = legend_position,
      x_text_angle = x_text_angle
    ) +
    theme(panel.grid = element_blank())
  if (show_values) {
    p <- p + geom_text(aes(label = ifelse(is.na(Correlation), "", sprintf("%.2f", Correlation))), size = value_label_size, color = th$text_color)
  }
  p
}

build_scatter_long_data <- function(df, dependent_col, independent_cols, color_col = "None", label_col = "None") {
  rows <- lapply(independent_cols, function(xcol) {
    out <- data.frame(
      X = to_numeric_vector(df[[xcol]]),
      Y = to_numeric_vector(df[[dependent_col]]),
      Independent = xcol,
      stringsAsFactors = FALSE
    )
    if (!is.null(color_col) && color_col != "None" && color_col %in% names(df)) out$Color <- as.character(df[[color_col]]) else out$Color <- "All"
    if (!is.null(label_col) && label_col != "None" && label_col %in% names(df)) out$Label <- as.character(df[[label_col]]) else out$Label <- ""
    out <- out[is.finite(out$X) & is.finite(out$Y), , drop = FALSE]
    out
  })
  bind_rows(rows)
}

create_scatter_panel_plot <- function(scatter_df, dependent_col, title, subtitle,
                                      theme_name, point_size = 2.8, point_alpha = 0.8,
                                      add_reg_line = TRUE, show_labels = FALSE,
                                      label_size = 3.0, panel_cols = 2,
                                      legend_position = "Right",
                                      title_size = 16, subtitle_size = 11,
                                      axis_title_size = 11, axis_text_size = 9,
                                      panel_title_size = 11, legend_title_size = 10,
                                      legend_text_size = 9) {
  validate(need(nrow(scatter_df) > 0, "Scatterplot data is empty."))
  p <- ggplot(scatter_df, aes(x = X, y = Y, color = Color)) +
    geom_point(size = point_size, alpha = point_alpha)
  if (add_reg_line) {
    p <- p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, linetype = "dashed", color = "black")
  }
  if (show_labels) {
    p <- p + geom_text(aes(label = Label), hjust = -0.05, vjust = -0.35, size = label_size, show.legend = FALSE)
  }
  p +
    facet_wrap(~ Independent, scales = "free_x", ncol = panel_cols) +
    labs(title = title, subtitle = subtitle, x = "Independent variable", y = dependent_col, color = "Group") +
    statcal_theme_gg(
      theme_name = theme_name,
      title_size = title_size,
      subtitle_size = subtitle_size,
      axis_title_size = axis_title_size,
      axis_text_size = axis_text_size,
      panel_title_size = panel_title_size,
      legend_title_size = legend_title_size,
      legend_text_size = legend_text_size,
      legend_position = legend_position,
      x_text_angle = 0
    )
}


# ============================================================
# REGRESSION AND CLASSICAL ASSUMPTION TESTS
# ============================================================

significance_stars <- function(p_value) {
  ifelse(is.na(p_value), "",
         ifelse(p_value < 0.001, "***",
                ifelse(p_value < 0.01, "**",
                       ifelse(p_value < 0.05, "*",
                              ifelse(p_value < 0.10, "†", "")))))
}

significance_decision <- function(p_value, alpha) {
  ifelse(is.na(p_value), "Not available", ifelse(p_value < alpha, "Significant", "Not significant"))
}

assumption_decision <- function(p_value, alpha, ok_message, problem_message) {
  ifelse(is.na(p_value), "Not available", ifelse(p_value >= alpha, ok_message, problem_message))
}

prepare_regression_data <- function(df, dependent_col, independent_cols) {
  reg_df <- data.frame(Source_Row = seq_len(nrow(df)), check.names = FALSE)
  for (id_col in intersect(c("Ticker Code", "Company Name", "Year"), names(df))) {
    reg_df[[id_col]] <- df[[id_col]]
  }
  reg_df$Y <- to_numeric_vector(df[[dependent_col]])
  for (i in seq_along(independent_cols)) {
    reg_df[[paste0("X", i)]] <- to_numeric_vector(df[[independent_cols[i]]])
  }
  required <- c("Y", paste0("X", seq_along(independent_cols)))
  mapping <- data.frame(
    Model_Term = c("(Intercept)", paste0("X", seq_along(independent_cols))),
    Variable = c("Intercept", independent_cols),
    stringsAsFactors = FALSE
  )
  reg_df <- reg_df[complete.cases(reg_df[, required, drop = FALSE]), , drop = FALSE]
  attr(reg_df, "mapping") <- mapping
  rownames(reg_df) <- NULL
  reg_df
}

fit_ols_regression <- function(df, dependent_col, independent_cols) {
  reg_df <- prepare_regression_data(df, dependent_col, independent_cols)
  validate(need(nrow(reg_df) >= length(independent_cols) + 2, "Not enough complete observations for the selected regression model."))
  formula_text <- paste("Y ~", paste(paste0("X", seq_along(independent_cols)), collapse = " + "))
  model <- lm(as.formula(formula_text), data = reg_df)
  list(model = model, reg_df = reg_df, mapping = attr(reg_df, "mapping"))
}

term_to_variable <- function(terms, mapping) {
  out <- mapping$Variable[match(terms, mapping$Model_Term)]
  out[is.na(out)] <- terms[is.na(out)]
  out
}

model_fit_summary_table <- function(model, dependent_col, decimal_digits) {
  smry <- summary(model)
  fstat <- smry$fstatistic
  f_value <- if (!is.null(fstat)) unname(fstat[1]) else NA_real_
  f_p <- if (!is.null(fstat)) pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
  rmse <- sqrt(mean(residuals(model)^2, na.rm = TRUE))
  out <- data.frame(
    `Dependent Variable` = dependent_col,
    N = length(model$residuals),
    `R-Squared` = smry$r.squared,
    `Adjusted R-Squared` = smry$adj.r.squared,
    `F-statistic` = f_value,
    `Prob(F-statistic)` = f_p,
    AIC = AIC(model),
    BIC = BIC(model),
    RMSE = rmse,
    `DF Model` = length(coef(model)) - 1,
    `DF Residual` = df.residual(model),
    check.names = FALSE
  )
  round_numeric_df(out, decimal_digits)
}

regression_coefficient_table <- function(model, mapping, alpha, decimal_digits) {
  coef_mat <- summary(model)$coefficients
  ci <- tryCatch(confint(model, level = 1 - alpha), error = function(e) matrix(NA_real_, nrow = nrow(coef_mat), ncol = 2, dimnames = list(rownames(coef_mat), c("2.5 %", "97.5 %"))))
  out <- data.frame(
    Variable = term_to_variable(rownames(coef_mat), mapping),
    Coefficient = coef_mat[, 1],
    `Std. Error` = coef_mat[, 2],
    `t-statistic` = coef_mat[, 3],
    `p-value` = coef_mat[, 4],
    Significance = significance_stars(coef_mat[, 4]),
    Decision = significance_decision(coef_mat[, 4], alpha),
    `CI Lower` = ci[, 1],
    `CI Upper` = ci[, 2],
    check.names = FALSE
  )
  round_numeric_df(out, decimal_digits)
}

normality_tests <- function(residuals, alpha, decimal_digits) {
  residuals <- residuals[is.finite(residuals)]
  if (length(residuals) < 3) {
    return(data.frame(Test = character(), Statistic = numeric(), `p-value` = numeric(), Decision = character(), Note = character(), check.names = FALSE))
  }

  z <- if (sd(residuals) > 0) (residuals - mean(residuals)) / sd(residuals) else residuals
  rows <- list()

  ks <- suppressWarnings(tryCatch(stats::ks.test(z, "pnorm"), error = function(e) NULL))
  rows[[length(rows) + 1]] <- data.frame(
    Test = "Kolmogorov-Smirnov",
    Statistic = if (is.null(ks)) NA_real_ else unname(ks$statistic),
    `p-value` = if (is.null(ks)) NA_real_ else ks$p.value,
    Decision = if (!is.null(ks)) assumption_decision(ks$p.value, alpha, "Normal residuals", "Non-normal residuals") else "Not available",
    Note = "Based on standardized residuals using ks.test().",
    check.names = FALSE
  )

  jb <- tryCatch(moments::jarque.test(residuals), error = function(e) NULL)
  rows[[length(rows) + 1]] <- data.frame(
    Test = "Jarque-Bera",
    Statistic = if (is.null(jb)) NA_real_ else unname(jb$statistic),
    `p-value` = if (is.null(jb)) NA_real_ else jb$p.value,
    Decision = if (!is.null(jb)) assumption_decision(jb$p.value, alpha, "Normal residuals", "Non-normal residuals") else "Not available",
    Note = "Computed using moments::jarque.test().",
    check.names = FALSE
  )

  shapiro_values <- if (length(residuals) > 5000) residuals[1:5000] else residuals
  sw <- tryCatch(stats::shapiro.test(shapiro_values), error = function(e) NULL)
  rows[[length(rows) + 1]] <- data.frame(
    Test = "Shapiro-Wilk",
    Statistic = if (is.null(sw)) NA_real_ else unname(sw$statistic),
    `p-value` = if (is.null(sw)) NA_real_ else sw$p.value,
    Decision = if (!is.null(sw)) assumption_decision(sw$p.value, alpha, "Normal residuals", "Non-normal residuals") else "Not available",
    Note = if (length(residuals) > 5000) "Computed on the first 5000 residuals due to shapiro.test() limitation." else "Computed using shapiro.test().",
    check.names = FALSE
  )

  round_numeric_df(bind_rows(rows), decimal_digits)
}

multicollinearity_diagnostics <- function(model, reg_df, independent_cols, decimal_digits) {
  if (length(independent_cols) == 1) {
    vif_df <- data.frame(
      Variable = independent_cols,
      VIF = 1,
      Tolerance = 1,
      `Common Rule` = "Acceptable",
      check.names = FALSE
    )
  } else {
    vif_values <- tryCatch(car::vif(model), error = function(e) rep(NA_real_, length(independent_cols)))
    if (is.matrix(vif_values)) vif_values <- vif_values[, 1]
    model_terms <- names(vif_values)
    if (is.null(model_terms)) model_terms <- paste0("X", seq_along(independent_cols))
    mapping <- attr(reg_df, "mapping")
    original_names <- term_to_variable(model_terms, mapping)
    vif_df <- data.frame(
      Variable = original_names,
      VIF = as.numeric(vif_values),
      Tolerance = 1 / as.numeric(vif_values),
      `Common Rule` = ifelse(as.numeric(vif_values) > 10, "Potential multicollinearity", "Acceptable"),
      check.names = FALSE
    )
  }

  x_cols <- paste0("X", seq_along(independent_cols))
  corr_df <- as.data.frame(round(cor(reg_df[, x_cols, drop = FALSE], use = "pairwise.complete.obs"), decimal_digits))
  names(corr_df) <- independent_cols
  corr_df <- cbind(Variable = independent_cols, corr_df)

  list(vif = round_numeric_df(vif_df, decimal_digits), corr = corr_df)
}

runs_test <- function(residuals, alpha) {
  residuals <- residuals[is.finite(residuals)]
  med <- median(residuals)
  signs <- residuals > med
  signs <- signs[residuals != med]
  n1 <- sum(signs)
  n2 <- length(signs) - n1
  if (n1 == 0 || n2 == 0 || length(signs) < 2) {
    runs <- NA_real_
    z_stat <- NA_real_
    p_value <- NA_real_
  } else {
    runs <- 1 + sum(signs[-1] != signs[-length(signs)])
    expected_runs <- 1 + (2 * n1 * n2) / (n1 + n2)
    variance_runs <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) / (((n1 + n2)^2) * (n1 + n2 - 1))
    z_stat <- if (variance_runs > 0) (runs - expected_runs) / sqrt(variance_runs) else NA_real_
    p_value <- if (is.finite(z_stat)) 2 * pnorm(abs(z_stat), lower.tail = FALSE) else NA_real_
  }
  data.frame(
    Test = "Runs Test",
    Statistic = z_stat,
    Runs = runs,
    `p-value` = p_value,
    Decision = ifelse(!is.na(p_value) && p_value >= alpha, "No autocorrelation pattern detected", "Autocorrelation pattern detected"),
    check.names = FALSE
  )
}

autocorrelation_tests <- function(model, alpha, decimal_digits) {
  dw <- tryCatch(lmtest::dwtest(model), error = function(e) NULL)
  dw_df <- data.frame(
    Test = "Durbin-Watson",
    Statistic = if (is.null(dw)) NA_real_ else unname(dw$statistic),
    Runs = NA_real_,
    `p-value` = if (is.null(dw)) NA_real_ else dw$p.value,
    Decision = if (!is.null(dw)) assumption_decision(dw$p.value, alpha, "No autocorrelation detected", "Autocorrelation detected") else "Not available",
    check.names = FALSE
  )
  round_numeric_df(bind_rows(runs_test(residuals(model), alpha), dw_df), decimal_digits)
}

glejser_test <- function(model, reg_df, independent_cols, alpha, decimal_digits) {
  glejser_df <- reg_df
  glejser_df$ABS_RESID <- abs(residuals(model))
  formula_text <- paste("ABS_RESID ~", paste(paste0("X", seq_along(independent_cols)), collapse = " + "))
  gmodel <- lm(as.formula(formula_text), data = glejser_df)
  coef_mat <- summary(gmodel)$coefficients
  mapping <- attr(reg_df, "mapping")
  coef_df <- data.frame(
    Variable = term_to_variable(rownames(coef_mat), mapping),
    Coefficient = coef_mat[, 1],
    `t-statistic` = coef_mat[, 3],
    `p-value` = coef_mat[, 4],
    Decision = significance_decision(coef_mat[, 4], alpha),
    check.names = FALSE
  )
  smry <- summary(gmodel)
  fstat <- smry$fstatistic
  f_value <- if (!is.null(fstat)) unname(fstat[1]) else NA_real_
  f_p <- if (!is.null(fstat)) pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA_real_
  summary_df <- data.frame(
    Test = "Glejser Test",
    Statistic = f_value,
    `p-value` = f_p,
    Decision = ifelse(!is.na(f_p) && f_p < alpha, "Heteroskedasticity detected", "No heteroskedasticity detected"),
    check.names = FALSE
  )
  list(coef = round_numeric_df(coef_df, decimal_digits), summary = round_numeric_df(summary_df, decimal_digits))
}

heteroskedasticity_tests <- function(model, reg_df, independent_cols, alpha, decimal_digits) {
  bp <- tryCatch(lmtest::bptest(model), error = function(e) NULL)
  rows <- list(data.frame(
    Test = "Breusch-Pagan",
    Statistic = if (is.null(bp)) NA_real_ else unname(bp$statistic),
    `p-value` = if (is.null(bp)) NA_real_ else bp$p.value,
    Decision = if (!is.null(bp) && bp$p.value < alpha) "Heteroskedasticity detected" else "No heteroskedasticity detected",
    check.names = FALSE
  ))

  glejser <- glejser_test(model, reg_df, independent_cols, alpha, decimal_digits)
  rows[[length(rows) + 1]] <- glejser$summary

  list(summary = round_numeric_df(bind_rows(rows), decimal_digits), glejser_coef = glejser$coef)
}

outlier_influence_table <- function(model, reg_df, decimal_digits) {
  studentized <- tryCatch(rstudent(model), error = function(e) rep(NA_real_, length(residuals(model))))
  leverage <- tryCatch(hatvalues(model), error = function(e) rep(NA_real_, length(residuals(model))))
  cooks <- tryCatch(cooks.distance(model), error = function(e) rep(NA_real_, length(residuals(model))))
  n <- nrow(reg_df)
  p <- length(coef(model))
  leverage_cutoff <- 2 * p / max(n, 1)
  cooks_cutoff <- 4 / max(n, 1)

  out <- data.frame(Source_Row = reg_df$Source_Row, check.names = FALSE)
  for (id_col in intersect(c("Ticker Code", "Company Name", "Year"), names(reg_df))) {
    out[[id_col]] <- reg_df[[id_col]]
  }
  out$Residual <- residuals(model)
  out$Fitted_Value <- fitted(model)
  out$Studentized_Residual <- studentized
  out$Leverage <- leverage
  out$Cooks_Distance <- cooks
  out$Residual_Outlier <- abs(studentized) > 3
  out$High_Leverage <- leverage > leverage_cutoff
  out$Influential_by_Cook <- cooks > cooks_cutoff
  out$Any_Flag <- out$Residual_Outlier | out$High_Leverage | out$Influential_by_Cook
  out$Rule <- sprintf("|Studentized residual| > 3; leverage > %.4f; Cook's D > %.4f", leverage_cutoff, cooks_cutoff)
  names(out) <- gsub("_", " ", names(out))
  round_numeric_df(out, decimal_digits)
}

plot_regression_diagnostics <- function(model, theme_name, show_grid = TRUE) {
  theme <- get_theme(theme_name)
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 2), bg = theme$figure_facecolor, col.axis = theme$text_color, col.lab = theme$text_color, col.main = theme$text_color)
  plot(
    fitted(model), residuals(model),
    pch = 19, col = theme$point_color,
    main = "Residuals vs Fitted Values",
    xlab = "Fitted value", ylab = "Residual"
  )
  abline(h = 0, lty = 2, col = theme$spine_color)
  if (show_grid) grid(col = theme$grid_color)
  qqnorm(residuals(model), pch = 19, col = theme$point_color, main = "Normal Q-Q Plot")
  qqline(residuals(model), col = theme$spine_color, lwd = 2)
  if (show_grid) grid(col = theme$grid_color)
}

# ============================================================
# UI
# ============================================================

legend_choices <- c("Right", "Left", "Top", "Bottom", "None / Hide legend")

ui <- dashboardPage(
  dashboardHeader(title = APP_NAME, titleWidth = "100%"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(HTML("\n        .content-wrapper, .right-side { background-color: #f7f9fb; }\n        .box { border-radius: 10px; }\n        .statcal-title { font-size: 24px; font-weight: 700; color: #1F4E79; }\n        .statcal-subtitle { font-size: 18px; font-weight: 600; color: #333333; }\n        .statcal-note { line-height: 1.6; text-align: justify; }\n      "))
    ),

    fluidRow(
      box(
        width = 12, status = "primary", solidHeader = TRUE,
        title = "STATCAL ONLINE IDX Financials Data Repository, Visualization, and Regression Analyzer",
        fluidRow(
          column(
            width = 2,
            tags$img(src = LOGO_PATH, width = "130px")
          ),
          column(
            width = 10,
            div(class = "statcal-title", APP_TITLE),
            div(class = "statcal-subtitle", APP_UPDATED),
            tags$p(class = "statcal-note",
                   "This application is developed as a web-based financial data repository, visualization, and regression analysis tool for companies in the Financials Sector listed on the Indonesia Stock Exchange (IDX). It supports data filtering, descriptive statistics, grouped summaries, multi-panel line charts, correlation heatmaps, scatterplot panels, multiple linear regression, classical assumption tests, and high-resolution PNG export for scientific reporting."
            ),
            tags$p(
              tags$b("Website: "), tags$a(href = WEBSITE_URL, target = "_blank", WEBSITE_URL), tags$br(),
              tags$b("STATCAL ONLINE Page: "), tags$a(href = STATCAL_ONLINE_URL, target = "_blank", STATCAL_ONLINE_URL), tags$br(),
              tags$b("IDX Stock Data Source: "), tags$a(href = IDX_STOCK_LIST_URL, target = "_blank", IDX_STOCK_LIST_URL), tags$br(),
              tags$b("Training Data: "), tags$a(href = TRAINING_DATA_URL, target = "_blank", "Open Google Drive Folder")
            )
          )
        )
      )
    ),

    tabsetPanel(
      id = "main_tabs",

      tabPanel(
        "1. Data & Filters",
        br(),
        fluidRow(
          box(
            width = 12, title = "Data Input and Flexible Filters", status = "primary", solidHeader = TRUE,
            fileInput("uploaded_file", "Upload Excel file", accept = c(".xlsx", ".xls")),
            uiOutput("sheet_ui"),
            uiOutput("filter_ui")
          )
        ),
        fluidRow(
          valueBoxOutput("metric_original_rows", width = 3),
          valueBoxOutput("metric_filtered_rows", width = 3),
          valueBoxOutput("metric_columns", width = 3),
          valueBoxOutput("metric_numeric", width = 3)
        ),
        fluidRow(
          box(width = 12, title = "Dataset Preview", status = "warning", solidHeader = TRUE,
              shinycssloaders::withSpinner(DTOutput("data_preview")))
        ),
        fluidRow(
          box(width = 12, title = "Detected Numeric Variables", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              verbatimTextOutput("numeric_variables_text"))
        ),
        fluidRow(
          box(width = 12, title = "Financial Statement URL List", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              shinycssloaders::withSpinner(DTOutput("financial_url_table")))
        )
      ),

      tabPanel(
        "2. Univariate Descriptive",
        br(),
        fluidRow(
          box(width = 9, title = "Variable Selection", status = "primary", solidHeader = TRUE,
              uiOutput("univariate_numeric_ui")),
          box(width = 3, title = "Decimal Setting", status = "primary", solidHeader = TRUE,
              sliderInput("univariate_digits", "Decimal digits", min = 0, max = 8, value = 3, step = 1))
        ),
        fluidRow(
          box(width = 12, title = "Univariate Descriptive Statistics", status = "warning", solidHeader = TRUE,
              shinycssloaders::withSpinner(DTOutput("univariate_table")))
        )
      ),

      tabPanel(
        "3. Grouped Descriptive",
        br(),
        fluidRow(
          box(width = 6, title = "Numeric Variables", status = "primary", solidHeader = TRUE,
              uiOutput("group_numeric_ui")),
          box(width = 4, title = "Group Variables", status = "primary", solidHeader = TRUE,
              uiOutput("group_vars_ui")),
          box(width = 2, title = "Decimal", status = "primary", solidHeader = TRUE,
              sliderInput("group_digits", "Digits", min = 0, max = 8, value = 3, step = 1))
        ),
        fluidRow(
          box(width = 12, title = "Grouped Descriptive Statistics", status = "warning", solidHeader = TRUE,
              shinycssloaders::withSpinner(DTOutput("grouped_table")))
        )
      ),

      tabPanel(
        "4. Multi-Panel Line Chart",
        br(),
        fluidRow(
          box(width = 3, title = "X-axis and Panels", status = "primary", solidHeader = TRUE,
              uiOutput("line_x_ui"),
              uiOutput("line_numeric_ui")),
          box(width = 3, title = "Split and Layout", status = "primary", solidHeader = TRUE,
              uiOutput("line_split_ui"),
              checkboxInput("line_sort_x", "Sort X-axis", value = TRUE),
              sliderInput("line_panel_cols", "Panel columns", min = 1, max = 4, value = 2, step = 1),
              checkboxInput("line_share_y", "Share Y-axis across panels", value = FALSE),
              selectInput("line_legend_position", "Legend position", choices = legend_choices, selected = "Right")),
          box(width = 3, title = "Line and Point Style", status = "primary", solidHeader = TRUE,
              selectInput("line_theme", "Chart theme", choices = names(THEMES), selected = "White Publication"),
              checkboxInput("line_points", "Show points", value = TRUE),
              sliderInput("line_width_size", "Line width", min = 0.4, max = 4.0, value = 1.1, step = 0.1),
              sliderInput("line_point_size", "Point size", min = 1, max = 10, value = 2.4, step = 0.2),
              sliderInput("line_x_text_angle", "X-axis text angle", min = 0, max = 90, value = 45, step = 5)),
          box(width = 3, title = "Title and Labels", status = "primary", solidHeader = TRUE,
              textInput("line_title", "Title", value = "Mean Trend of IDX Financials Variables"),
              textInput("line_subtitle", "Subtitle", value = "Multi-panel chart based on mean values from filtered IDX data"),
              textInput("line_y_label", "Y-axis label", value = "Mean"),
              checkboxInput("line_show_values", "Show mean value labels", value = FALSE),
              checkboxInput("line_compact_labels", "Compact labels K/M/B/T", value = TRUE))
        ),
        fluidRow(
          box(width = 12, title = "Flexible Text Size Settings", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              fluidRow(
                column(2, sliderInput("line_title_size", "Title", 8, 34, 16, 1)),
                column(2, sliderInput("line_subtitle_size", "Subtitle", 6, 26, 11, 1)),
                column(2, sliderInput("line_axis_title_size", "Axis title", 6, 24, 11, 1)),
                column(2, sliderInput("line_axis_text_size", "Axis text", 5, 22, 9, 1)),
                column(2, sliderInput("line_panel_title_size", "Panel title", 6, 26, 11, 1)),
                column(2, sliderInput("line_legend_text_size", "Legend text", 5, 22, 9, 1))
              ),
              fluidRow(
                column(2, sliderInput("line_legend_title_size", "Legend title", 5, 24, 10, 1)),
                column(2, sliderInput("line_value_label_size", "Value labels", 2, 8, 3, 0.2))
              ))
        ),
        fluidRow(
          box(width = 12, title = "Multi-Panel Mean Line Chart", status = "warning", solidHeader = TRUE,
              shinycssloaders::withSpinner(plotOutput("line_plot", height = "650px")))
        ),
        fluidRow(
          box(width = 12, title = "Line Chart Data", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              shinycssloaders::withSpinner(DTOutput("line_data_table")))
        )
      ),

      tabPanel(
        "5. Correlation Heatmap",
        br(),
        fluidRow(
          box(width = 5, title = "Correlation Variables", status = "primary", solidHeader = TRUE,
              uiOutput("corr_numeric_ui")),
          box(width = 3, title = "Correlation Settings", status = "primary", solidHeader = TRUE,
              selectInput("corr_method", "Correlation method", choices = c("Pearson" = "pearson", "Spearman" = "spearman"), selected = "pearson"),
              sliderInput("corr_digits", "Decimal digits", min = 0, max = 8, value = 3, step = 1),
              selectInput("corr_legend_position", "Legend position", choices = legend_choices, selected = "Right")),
          box(width = 4, title = "Heatmap Settings", status = "primary", solidHeader = TRUE,
              selectInput("corr_theme", "Heatmap theme", choices = names(THEMES), selected = "White Publication"),
              checkboxInput("corr_show_values", "Show correlation values", value = TRUE),
              textInput("corr_title", "Title", value = "Correlation Heatmap of IDX Financials Variables"),
              sliderInput("corr_x_text_angle", "X-axis text angle", min = 0, max = 90, value = 45, step = 5))
        ),
        fluidRow(
          box(width = 12, title = "Flexible Text Size Settings", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              fluidRow(
                column(2, sliderInput("corr_title_size", "Title", 8, 34, 16, 1)),
                column(2, sliderInput("corr_axis_text_size", "Axis text", 5, 22, 9, 1)),
                column(2, sliderInput("corr_value_label_size", "Value labels", 2, 8, 3, 0.2)),
                column(2, sliderInput("corr_legend_title_size", "Legend title", 5, 24, 10, 1)),
                column(2, sliderInput("corr_legend_text_size", "Legend text", 5, 22, 9, 1))
              ))
        ),
        fluidRow(
          box(width = 12, title = "Correlation Heatmap", status = "warning", solidHeader = TRUE,
              shinycssloaders::withSpinner(plotOutput("corr_heatmap", height = "650px")))
        ),
        fluidRow(
          box(width = 6, title = "Correlation Matrix", status = "info", solidHeader = TRUE,
              shinycssloaders::withSpinner(DTOutput("corr_matrix_table"))),
          box(width = 6, title = "Correlation p-values", status = "info", solidHeader = TRUE,
              shinycssloaders::withSpinner(DTOutput("corr_pvalue_table")))
        )
      ),

      tabPanel(
        "6. Scatterplot Panel",
        br(),
        fluidRow(
          box(width = 3, title = "Dependent Variable", status = "primary", solidHeader = TRUE,
              uiOutput("scatter_dependent_ui")),
          box(width = 4, title = "Independent Variables", status = "primary", solidHeader = TRUE,
              uiOutput("scatter_independent_ui")),
          box(width = 3, title = "Grouping and Labels", status = "primary", solidHeader = TRUE,
              uiOutput("scatter_color_ui"),
              uiOutput("scatter_label_ui"),
              checkboxInput("scatter_show_labels", "Show point labels", value = FALSE),
              selectInput("scatter_legend_position", "Legend position", choices = legend_choices, selected = "Right")),
          box(width = 2, title = "Style", status = "primary", solidHeader = TRUE,
              selectInput("scatter_theme", "Theme", choices = names(THEMES), selected = "White Publication"),
              checkboxInput("scatter_reg_line", "Add linear trend line", value = TRUE),
              sliderInput("scatter_panel_cols", "Panel columns", min = 1, max = 4, value = 2, step = 1),
              sliderInput("scatter_point_size", "Point size", min = 1, max = 10, value = 2.8, step = 0.2),
              sliderInput("scatter_alpha", "Point alpha", min = 0.1, max = 1, value = 0.8, step = 0.05))
        ),
        fluidRow(
          box(width = 12, title = "Title and Flexible Text Size Settings", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              fluidRow(
                column(4, textInput("scatter_title", "Title", value = "Scatterplot Panel of IDX Financials Variables")),
                column(4, textInput("scatter_subtitle", "Subtitle", value = "Dependent and independent variable relationships from filtered data")),
                column(2, sliderInput("scatter_title_size", "Title", 8, 34, 16, 1)),
                column(2, sliderInput("scatter_subtitle_size", "Subtitle", 6, 26, 11, 1))
              ),
              fluidRow(
                column(2, sliderInput("scatter_axis_title_size", "Axis title", 6, 24, 11, 1)),
                column(2, sliderInput("scatter_axis_text_size", "Axis text", 5, 22, 9, 1)),
                column(2, sliderInput("scatter_panel_title_size", "Panel title", 6, 26, 11, 1)),
                column(2, sliderInput("scatter_legend_title_size", "Legend title", 5, 24, 10, 1)),
                column(2, sliderInput("scatter_legend_text_size", "Legend text", 5, 22, 9, 1)),
                column(2, sliderInput("scatter_label_size", "Point labels", 2, 8, 3, 0.2))
              ))
        ),
        fluidRow(
          box(width = 12, title = "Scatterplot Panel", status = "warning", solidHeader = TRUE,
              shinycssloaders::withSpinner(plotOutput("scatter_plot", height = "650px")))
        ),
        fluidRow(
          box(width = 12, title = "Scatterplot Data", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              shinycssloaders::withSpinner(DTOutput("scatter_data_table")))
        )
      ),

      tabPanel(
        "7. Regression & Assumption Tests",
        br(),
        fluidRow(
          box(width = 3, title = "Dependent Variable", status = "primary", solidHeader = TRUE,
              uiOutput("reg_dependent_ui")),
          box(width = 5, title = "Independent Variables", status = "primary", solidHeader = TRUE,
              uiOutput("reg_independent_ui")),
          box(width = 2, title = "Alpha and Digits", status = "primary", solidHeader = TRUE,
              selectInput("reg_alpha", "Alpha", choices = c("0.10" = 0.10, "0.05" = 0.05, "0.01" = 0.01), selected = 0.05),
              sliderInput("reg_digits", "Decimal digits", min = 0, max = 8, value = 4, step = 1)),
          box(width = 2, title = "Diagnostic Plot", status = "primary", solidHeader = TRUE,
              selectInput("reg_theme", "Plot theme", choices = names(THEMES), selected = "White Publication"),
              checkboxInput("reg_show_grid", "Show grid", value = TRUE))
        ),
        fluidRow(
          box(width = 6, title = "Model Fit Summary", status = "warning", solidHeader = TRUE,
              shinycssloaders::withSpinner(DTOutput("reg_model_fit"))),
          box(width = 6, title = "Coefficient Estimates", status = "warning", solidHeader = TRUE,
              shinycssloaders::withSpinner(DTOutput("reg_coefficients")))
        ),
        fluidRow(
          box(width = 6, title = "Residual Normality Tests", status = "primary", solidHeader = TRUE,
              tags$p("Normality tests use ks.test(), moments::jarque.test(), and shapiro.test()."),
              shinycssloaders::withSpinner(DTOutput("normality_table"))),
          box(width = 6, title = "Multicollinearity: VIF and Tolerance", status = "primary", solidHeader = TRUE,
              shinycssloaders::withSpinner(DTOutput("vif_table")))
        ),
        fluidRow(
          box(width = 6, title = "Heteroskedasticity Tests", status = "primary", solidHeader = TRUE,
              tags$p("Heteroskedasticity is evaluated using Glejser Test and Breusch-Pagan Test."),
              shinycssloaders::withSpinner(DTOutput("hetero_table"))),
          box(width = 6, title = "Autocorrelation Tests", status = "primary", solidHeader = TRUE,
              shinycssloaders::withSpinner(DTOutput("autocorr_table")))
        ),
        fluidRow(
          box(width = 6, title = "Glejser Test Coefficients", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              shinycssloaders::withSpinner(DTOutput("glejser_coefficients"))),
          box(width = 6, title = "Correlation among Independent Variables", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              shinycssloaders::withSpinner(DTOutput("independent_corr_table")))
        ),
        fluidRow(
          valueBoxOutput("metric_residual_outliers", width = 4),
          valueBoxOutput("metric_high_leverage", width = 4),
          valueBoxOutput("metric_influential_cook", width = 4)
        ),
        fluidRow(
          box(width = 12, title = "Outlier and Influential Observation Detection", status = "danger", solidHeader = TRUE,
              shinycssloaders::withSpinner(DTOutput("outlier_table")))
        ),
        fluidRow(
          box(width = 12, title = "Residual Diagnostic Plots", status = "warning", solidHeader = TRUE,
              shinycssloaders::withSpinner(plotOutput("reg_diagnostic_plot", height = "460px")))
        )
      ),

      tabPanel(
        "8. Export Charts",
        br(),
        fluidRow(
          box(width = 12, title = "Export Settings", status = "primary", solidHeader = TRUE,
              fluidRow(
                column(4, selectInput("export_dpi", "PNG resolution / DPI", choices = c(300, 600, 900, 1200, 1500), selected = 1200)),
                column(4, numericInput("export_width", "Export width (inches)", value = 12, min = 4, max = 30, step = 0.5)),
                column(4, numericInput("export_height", "Export height (inches)", value = 8, min = 3, max = 30, step = 0.5))
              ),
              tags$p(tags$b("Default DPI: "), "1200 DPI for publication-ready output."))
        ),
        fluidRow(
          box(width = 3, title = "Line Chart PNG", status = "warning", solidHeader = TRUE,
              downloadButton("download_line_png", "Download Line Chart PNG")),
          box(width = 3, title = "Correlation Heatmap PNG", status = "warning", solidHeader = TRUE,
              downloadButton("download_corr_png", "Download Correlation Heatmap PNG")),
          box(width = 3, title = "Scatterplot PNG", status = "warning", solidHeader = TRUE,
              downloadButton("download_scatter_png", "Download Scatterplot PNG")),
          box(width = 3, title = "Regression Diagnostic PNG", status = "warning", solidHeader = TRUE,
              downloadButton("download_regression_png", "Download Regression Diagnostic PNG"))
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  current_excel_path <- reactive({
    if (!is.null(input$uploaded_file)) {
      input$uploaded_file$datapath
    } else if (file.exists(SAMPLE_DATA_PATH)) {
      SAMPLE_DATA_PATH
    } else {
      NULL
    }
  })

  output$sheet_ui <- renderUI({
    path <- current_excel_path()
    if (is.null(path)) {
      return(helpText("Please upload an Excel file to start the analysis."))
    }
    sheets <- readxl::excel_sheets(path)
    selectInput("sheet_name", "Worksheet", choices = sheets, selected = sheets[1])
  })

  data_raw <- reactive({
    path <- current_excel_path()
    req(path)
    sheets <- readxl::excel_sheets(path)
    sheet <- input$sheet_name
    if (is.null(sheet) || !(sheet %in% sheets)) sheet <- sheets[1]
    clean_dataframe(readxl::read_excel(path, sheet = sheet))
  })

  numeric_columns <- reactive({
    detect_numeric_columns(data_raw())
  })

  output$filter_ui <- renderUI({
    df <- data_raw()
    ui_list <- list()
    if ("Year" %in% names(df)) {
      years <- sorted_unique_values(df$Year)
      ui_list <- c(ui_list, list(
        column(6, selectizeInput("filter_year", "Filter by Year", choices = years, selected = years, multiple = TRUE))
      ))
    }
    if ("Ticker Code" %in% names(df)) {
      tickers <- sorted_unique_values(df[["Ticker Code"]])
      ui_list <- c(ui_list, list(
        column(6, selectizeInput("filter_ticker", "Filter by Ticker Code", choices = tickers, selected = tickers, multiple = TRUE))
      ))
    }
    if (length(ui_list) == 0) return(helpText("No default Year or Ticker Code columns were found."))
    do.call(fluidRow, ui_list)
  })

  filtered_data <- reactive({
    df <- data_raw()
    if ("Year" %in% names(df) && !is.null(input$filter_year) && length(input$filter_year) > 0) {
      df <- df[df$Year %in% input$filter_year, , drop = FALSE]
    }
    if ("Ticker Code" %in% names(df) && !is.null(input$filter_ticker) && length(input$filter_ticker) > 0) {
      df <- df[df[["Ticker Code"]] %in% input$filter_ticker, , drop = FALSE]
    }
    df
  })

  output$metric_original_rows <- renderValueBox({
    valueBox(nrow(data_raw()), "Original rows", icon = icon("table"), color = "blue")
  })
  output$metric_filtered_rows <- renderValueBox({
    valueBox(nrow(filtered_data()), "Rows after filtering", icon = icon("filter"), color = "green")
  })
  output$metric_columns <- renderValueBox({
    valueBox(ncol(data_raw()), "Columns", icon = icon("columns"), color = "yellow")
  })
  output$metric_numeric <- renderValueBox({
    valueBox(length(numeric_columns()), "Detected numeric variables", icon = icon("calculator"), color = "purple")
  })

  output$data_preview <- renderDT({
    DT::datatable(make_display_safe(filtered_data()), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$numeric_variables_text <- renderPrint({
    print(numeric_columns())
  })

  output$financial_url_table <- renderDT({
    df <- filtered_data()
    if (!(FINANCIAL_URL_COL %in% names(df))) {
      return(DT::datatable(data.frame(Message = "Financial Statement URL column was not found.")))
    }
    url_cols <- intersect(c("Ticker Code", "Company Name", "Year", FINANCIAL_URL_COL), names(df))
    DT::datatable(make_display_safe(df[, url_cols, drop = FALSE]), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$univariate_numeric_ui <- renderUI({
    nums <- numeric_columns()
    selectizeInput("univariate_numeric_cols", "Select numeric variables", choices = nums, selected = default_numeric_columns(nums), multiple = TRUE)
  })

  output$univariate_table <- renderDT({
    req(input$univariate_numeric_cols)
    out <- compute_descriptive_statistics(filtered_data(), input$univariate_numeric_cols, input$univariate_digits)
    DT::datatable(make_display_safe(out), options = list(scrollX = TRUE, pageLength = 15))
  })

  output$group_numeric_ui <- renderUI({
    nums <- numeric_columns()
    selectizeInput("group_numeric_cols", "Select numeric variables", choices = nums, selected = default_numeric_columns(nums), multiple = TRUE)
  })

  output$group_vars_ui <- renderUI({
    df <- data_raw()
    defaults <- intersect(c("Year", "Ticker Code"), names(df))
    selectizeInput("group_cols", "Group by category variables", choices = names(df), selected = defaults, multiple = TRUE)
  })

  output$grouped_table <- renderDT({
    req(input$group_numeric_cols, input$group_cols)
    out <- compute_grouped_descriptive_statistics(filtered_data(), input$group_numeric_cols, input$group_cols, input$group_digits)
    DT::datatable(make_display_safe(out), options = list(scrollX = TRUE, pageLength = 15))
  })

  # ---------------- Line chart ----------------
  output$line_x_ui <- renderUI({
    df <- data_raw()
    selected <- if ("Year" %in% names(df)) "Year" else names(df)[1]
    selectInput("line_x", "X-axis variable", choices = names(df), selected = selected)
  })

  output$line_numeric_ui <- renderUI({
    nums <- numeric_columns()
    selectizeInput("line_numeric_cols", "Panel numeric variables", choices = nums, selected = default_numeric_columns(nums, n = 4), multiple = TRUE)
  })

  output$line_split_ui <- renderUI({
    df <- data_raw()
    choices <- c("None", names(df))
    selected <- if ("Ticker Code" %in% names(df)) "Ticker Code" else "None"
    selectInput("line_split", "Split lines by category", choices = choices, selected = selected)
  })

  line_chart_data <- reactive({
    req(input$line_x, input$line_numeric_cols)
    validate(need(length(input$line_numeric_cols) >= 1, "Please select at least one numeric variable."))
    build_mean_line_data(
      filtered_data(),
      x_col = input$line_x,
      numeric_cols = input$line_numeric_cols,
      split_col = input$line_split,
      sort_x = input$line_sort_x
    )
  })

  line_plot_object <- reactive({
    create_panel_line_plot(
      line_chart_data(),
      title = input$line_title,
      subtitle = input$line_subtitle,
      x_label = input$line_x,
      y_label = input$line_y_label,
      theme_name = input$line_theme,
      show_points = input$line_points,
      line_width = input$line_width_size,
      point_size = input$line_point_size,
      show_value_labels = input$line_show_values,
      value_label_size = input$line_value_label_size,
      compact_labels = input$line_compact_labels,
      share_y = input$line_share_y,
      panel_cols = input$line_panel_cols,
      legend_position = input$line_legend_position,
      title_size = input$line_title_size,
      subtitle_size = input$line_subtitle_size,
      axis_title_size = input$line_axis_title_size,
      axis_text_size = input$line_axis_text_size,
      panel_title_size = input$line_panel_title_size,
      legend_title_size = input$line_legend_title_size,
      legend_text_size = input$line_legend_text_size,
      x_text_angle = input$line_x_text_angle
    )
  })

  output$line_plot <- renderPlot({
    line_plot_object()
  })

  output$line_data_table <- renderDT({
    DT::datatable(make_display_safe(line_chart_data()), options = list(scrollX = TRUE, pageLength = 10))
  })

  # ---------------- Correlation heatmap ----------------
  output$corr_numeric_ui <- renderUI({
    nums <- numeric_columns()
    selectizeInput("corr_numeric_cols", "Select numeric variables", choices = nums, selected = default_numeric_columns(nums, n = 6), multiple = TRUE)
  })

  corr_results <- reactive({
    req(input$corr_numeric_cols)
    validate(need(length(input$corr_numeric_cols) >= 2, "Please select at least two numeric variables."))
    compute_correlation_results(filtered_data(), input$corr_numeric_cols, method = input$corr_method, digits = input$corr_digits)
  })

  corr_plot_object <- reactive({
    res <- corr_results()
    create_correlation_heatmap(
      res$corr,
      title = input$corr_title,
      theme_name = input$corr_theme,
      show_values = input$corr_show_values,
      value_label_size = input$corr_value_label_size,
      legend_position = input$corr_legend_position,
      title_size = input$corr_title_size,
      axis_text_size = input$corr_axis_text_size,
      legend_title_size = input$corr_legend_title_size,
      legend_text_size = input$corr_legend_text_size,
      x_text_angle = input$corr_x_text_angle
    )
  })

  output$corr_heatmap <- renderPlot({
    corr_plot_object()
  })

  output$corr_matrix_table <- renderDT({
    DT::datatable(make_display_safe(corr_results()$corr_table), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$corr_pvalue_table <- renderDT({
    DT::datatable(make_display_safe(corr_results()$pval_table), options = list(scrollX = TRUE, pageLength = 10))
  })

  # ---------------- Scatterplot panel ----------------
  output$scatter_dependent_ui <- renderUI({
    nums <- numeric_columns()
    preferred <- intersect(c("Net Income", "Return on Asset (ROA)", "Total Assets"), nums)
    selected <- if (length(preferred) > 0) preferred[1] else nums[1]
    selectInput("scatter_dependent", "Dependent variable", choices = nums, selected = selected)
  })

  output$scatter_independent_ui <- renderUI({
    nums <- numeric_columns()
    dep <- input$scatter_dependent
    choices <- setdiff(nums, dep)
    selected <- choices[seq_len(min(3, length(choices)))]
    selectizeInput("scatter_independent", "Independent variables", choices = choices, selected = selected, multiple = TRUE)
  })

  output$scatter_color_ui <- renderUI({
    df <- data_raw()
    choices <- c("None", names(df))
    selected <- if ("Ticker Code" %in% names(df)) "Ticker Code" else "None"
    selectInput("scatter_color", "Color by category", choices = choices, selected = selected)
  })

  output$scatter_label_ui <- renderUI({
    df <- data_raw()
    choices <- c("None", names(df))
    selected <- if ("Ticker Code" %in% names(df)) "Ticker Code" else "None"
    selectInput("scatter_label", "Point label variable", choices = choices, selected = selected)
  })

  scatter_chart_data <- reactive({
    req(input$scatter_dependent, input$scatter_independent)
    validate(need(length(input$scatter_independent) >= 1, "Please select at least one independent variable."))
    build_scatter_long_data(filtered_data(), input$scatter_dependent, input$scatter_independent, input$scatter_color, input$scatter_label)
  })

  scatter_plot_object <- reactive({
    create_scatter_panel_plot(
      scatter_chart_data(),
      dependent_col = input$scatter_dependent,
      title = input$scatter_title,
      subtitle = input$scatter_subtitle,
      theme_name = input$scatter_theme,
      point_size = input$scatter_point_size,
      point_alpha = input$scatter_alpha,
      add_reg_line = input$scatter_reg_line,
      show_labels = input$scatter_show_labels,
      label_size = input$scatter_label_size,
      panel_cols = input$scatter_panel_cols,
      legend_position = input$scatter_legend_position,
      title_size = input$scatter_title_size,
      subtitle_size = input$scatter_subtitle_size,
      axis_title_size = input$scatter_axis_title_size,
      axis_text_size = input$scatter_axis_text_size,
      panel_title_size = input$scatter_panel_title_size,
      legend_title_size = input$scatter_legend_title_size,
      legend_text_size = input$scatter_legend_text_size
    )
  })

  output$scatter_plot <- renderPlot({
    scatter_plot_object()
  })

  output$scatter_data_table <- renderDT({
    DT::datatable(make_display_safe(scatter_chart_data()), options = list(scrollX = TRUE, pageLength = 10))
  })


  # ---------------- Regression and assumption tests ----------------
  output$reg_dependent_ui <- renderUI({
    nums <- numeric_columns()
    preferred <- intersect(c("Net Income", "Return on Asset (ROA)", "Total Assets"), nums)
    selected <- if (length(preferred) > 0) preferred[1] else nums[1]
    selectInput("reg_dependent", "Dependent variable", choices = nums, selected = selected)
  })

  output$reg_independent_ui <- renderUI({
    nums <- numeric_columns()
    dep <- input$reg_dependent
    choices <- setdiff(nums, dep)
    preferred <- intersect(c("Total Assets", "Total Liabilities", "Total Equity", "Earning per Share", "Debt to Asset Ratio (DAR)", "Debt to Equity Ratio (DER)"), choices)
    selected <- if (length(preferred) > 0) preferred[seq_len(min(3, length(preferred)))] else choices[seq_len(min(3, length(choices)))]
    selectizeInput("reg_independent", "Independent variables", choices = choices, selected = selected, multiple = TRUE)
  })

  regression_results <- reactive({
    req(input$reg_dependent, input$reg_independent)
    validate(need(length(input$reg_independent) >= 1, "Please select at least one independent variable."))
    fit_ols_regression(filtered_data(), input$reg_dependent, input$reg_independent)
  })

  output$reg_model_fit <- renderDT({
    res <- regression_results()
    DT::datatable(model_fit_summary_table(res$model, input$reg_dependent, input$reg_digits), options = list(dom = "t", scrollX = TRUE))
  })

  output$reg_coefficients <- renderDT({
    res <- regression_results()
    DT::datatable(regression_coefficient_table(res$model, res$mapping, as.numeric(input$reg_alpha), input$reg_digits), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$normality_table <- renderDT({
    res <- regression_results()
    DT::datatable(normality_tests(residuals(res$model), as.numeric(input$reg_alpha), input$reg_digits), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$vif_table <- renderDT({
    res <- regression_results()
    diag <- multicollinearity_diagnostics(res$model, res$reg_df, input$reg_independent, input$reg_digits)
    DT::datatable(make_display_safe(diag$vif), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$independent_corr_table <- renderDT({
    res <- regression_results()
    diag <- multicollinearity_diagnostics(res$model, res$reg_df, input$reg_independent, input$reg_digits)
    DT::datatable(make_display_safe(diag$corr), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$hetero_table <- renderDT({
    res <- regression_results()
    h <- heteroskedasticity_tests(res$model, res$reg_df, input$reg_independent, as.numeric(input$reg_alpha), input$reg_digits)
    DT::datatable(make_display_safe(h$summary), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$glejser_coefficients <- renderDT({
    res <- regression_results()
    h <- heteroskedasticity_tests(res$model, res$reg_df, input$reg_independent, as.numeric(input$reg_alpha), input$reg_digits)
    DT::datatable(make_display_safe(h$glejser_coef), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$autocorr_table <- renderDT({
    res <- regression_results()
    DT::datatable(autocorrelation_tests(res$model, as.numeric(input$reg_alpha), input$reg_digits), options = list(scrollX = TRUE, pageLength = 10))
  })

  outlier_data <- reactive({
    res <- regression_results()
    outlier_influence_table(res$model, res$reg_df, input$reg_digits)
  })

  output$metric_residual_outliers <- renderValueBox({
    od <- outlier_data()
    valueBox(sum(od[["Residual Outlier"]], na.rm = TRUE), "Residual outliers", icon = icon("exclamation-triangle"), color = "red")
  })

  output$metric_high_leverage <- renderValueBox({
    od <- outlier_data()
    valueBox(sum(od[["High Leverage"]], na.rm = TRUE), "High leverage", icon = icon("balance-scale"), color = "yellow")
  })

  output$metric_influential_cook <- renderValueBox({
    od <- outlier_data()
    valueBox(sum(od[["Influential by Cook"]], na.rm = TRUE), "Influential by Cook", icon = icon("bullseye"), color = "purple")
  })

  output$outlier_table <- renderDT({
    od <- outlier_data()
    flagged <- od[od[["Any Flag"]], , drop = FALSE]
    if (nrow(flagged) == 0) {
      flagged <- data.frame(Message = "No flagged observations were detected using the selected diagnostic rules.")
    }
    DT::datatable(make_display_safe(flagged), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$reg_diagnostic_plot <- renderPlot({
    res <- regression_results()
    plot_regression_diagnostics(res$model, input$reg_theme, input$reg_show_grid)
  })

  # ---------------- Export PNG charts ----------------
  output$download_line_png <- downloadHandler(
    filename = function() paste0("statcal_online_idx_financials_panel_line_chart_", input$export_dpi, "dpi.png"),
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = line_plot_object(),
        width = input$export_width,
        height = input$export_height,
        dpi = as.numeric(input$export_dpi),
        units = "in",
        bg = get_theme(input$line_theme)$figure_facecolor
      )
    }
  )

  output$download_corr_png <- downloadHandler(
    filename = function() paste0("statcal_online_idx_financials_correlation_heatmap_", input$export_dpi, "dpi.png"),
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = corr_plot_object(),
        width = input$export_width,
        height = input$export_height,
        dpi = as.numeric(input$export_dpi),
        units = "in",
        bg = get_theme(input$corr_theme)$figure_facecolor
      )
    }
  )

  output$download_scatter_png <- downloadHandler(
    filename = function() paste0("statcal_online_idx_financials_scatterplot_panel_", input$export_dpi, "dpi.png"),
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = scatter_plot_object(),
        width = input$export_width,
        height = input$export_height,
        dpi = as.numeric(input$export_dpi),
        units = "in",
        bg = get_theme(input$scatter_theme)$figure_facecolor
      )
    }
  )

  output$download_regression_png <- downloadHandler(
    filename = function() paste0("statcal_online_idx_financials_regression_diagnostic_", input$export_dpi, "dpi.png"),
    content = function(file) {
      res <- regression_results()
      png(
        filename = file,
        width = input$export_width,
        height = input$export_height,
        units = "in",
        res = as.numeric(input$export_dpi),
        bg = get_theme(input$reg_theme)$figure_facecolor
      )
      plot_regression_diagnostics(res$model, input$reg_theme, input$reg_show_grid)
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
