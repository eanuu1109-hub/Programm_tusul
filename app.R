# ============================================================
# Used Car Price Dashboard
# ============================================================

options(shiny.maxRequestSize = 100 * 1024^2)

# Data prices are in Indian rupees. Dashboard displays prices in Mongolian tugrik.
INR_TO_MNT <- 38


required_pkgs <- c(
  "shiny", "shinydashboard", "ggplot2", "plotly", "DT",
  "scales", "randomForest", "MASS", "dplyr"
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Install missing packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))"
  )
}

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(randomForest)
# MASS is used with MASS::rlm only, not attached, to avoid select() conflicts.

# ============================================================
# 1. Helpers
# ============================================================

get_app_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path), error = function(e) "")
    if (nzchar(p) && dir.exists(p)) return(p)
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

get_default_paths <- function() {
  app_dir <- get_app_dir()
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

  paths <- character(0)
  add_path <- function(path) {
    if (!is.null(path) && length(path) > 0 && !is.na(path) && nzchar(path)) {
      paths <<- c(paths, path)
    }
  }

  add_path(file.path(app_dir, "cars.csv"))
  add_path(file.path(app_dir, "cars(2).csv"))
  add_path(file.path(wd, "cars.csv"))
  add_path(file.path(wd, "cars(2).csv"))
  add_path(file.path(wd, "car_dashboard_mnt_final", "cars.csv"))
  add_path(file.path(wd, "car_dashboard_mnt_FIXED_NO_EMPTY", "cars.csv"))
  add_path("/Users/sain/Documents/UFE/EZ-n program/BD/cars.csv")
  add_path("/Users/sain/Documents/UFE/EZ-n program/BD/cars(2).csv")
  add_path("/Users/sain/Downloads/proj/cars_202604101319.csv")
  add_path("/Users/sain/Downloads/proj/cars_202604101319(2).csv")

  unique(paths)
}

load_default_data <- function() {
  paths <- get_default_paths()
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0) return(NULL)
  read.csv(existing[1], stringsAsFactors = FALSE)
}

parse_number <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(x))))
}

winsorize <- function(x, probs = c(0.01, 0.99)) {
  q <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  pmin(pmax(x, q[1]), q[2])
}

clamp_vec <- function(x, bounds) {
  pmin(pmax(x, bounds[1]), bounds[2])
}

to_mnt <- function(x) {
  x * INR_TO_MNT
}

format_price <- function(x) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return("Not available")
  paste0("₮ ", scales::comma(round(to_mnt(x))))
}

safe_title <- function(x) tools::toTitleCase(tolower(trimws(as.character(x))))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}


metric_table <- function(actual, predicted) {
  ok <- is.finite(actual) & is.finite(predicted)
  actual <- actual[ok]
  predicted <- predicted[ok]
  if (length(actual) < 5) {
    return(data.frame(RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_))
  }
  data.frame(
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAE = mean(abs(actual - predicted)),
    R2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  )
}

percentile_summary <- function(df, vars) {
  out <- lapply(vars, function(v) {
    x <- df[[v]]
    qs <- stats::quantile(x, probs = c(0.01, 0.05, 0.50, 0.95, 0.99), na.rm = TRUE)
    data.frame(
      Variable = v,
      p1 = qs[[1]], p5 = qs[[2]], p50 = qs[[3]], p95 = qs[[4]], p99 = qs[[5]],
      row.names = NULL
    )
  })
  dplyr::bind_rows(out)
}

extract_coef_table <- function(model) {
  if (is.null(model)) return(data.frame(Message = "Model unavailable"))
  sm <- tryCatch(summary(model), error = function(e) NULL)
  if (is.null(sm) || is.null(sm$coefficients)) return(data.frame(Message = "Coefficient table unavailable"))
  cf <- as.data.frame(sm$coefficients)
  cf$Variable <- rownames(cf)
  rownames(cf) <- NULL
  cf <- cf[, c("Variable", setdiff(names(cf), "Variable")), drop = FALSE]
  cf
}

# ============================================================
# 2. Data cleaning
# ============================================================

clean_data <- function(df) {
  raw_n <- nrow(df)
  names(df) <- trimws(names(df))

  rename_map <- c(
    "Brand" = "brand", "brand" = "brand",
    "model" = "model", "Model" = "model",
    "Year" = "year", "year" = "year",
    "Age" = "age_original", "age" = "age_original",
    "Transmission" = "transmission", "transmission" = "transmission",
    "Owner" = "owner", "owner" = "owner",
    "FuelType" = "fuel", "Fuel" = "fuel", "fuel" = "fuel",
    "price" = "price", "Price" = "price", "AskPrice" = "ask_price",
    "mileage" = "mileage", "Mileage" = "mileage", "kmDriven" = "km_driven",
    "PostedDate" = "posted_date", "AdditionInfo" = "addition_info"
  )
  hit <- intersect(names(rename_map), names(df))
  names(df)[match(hit, names(df))] <- unname(rename_map[hit])

  if (!"price" %in% names(df) && "ask_price" %in% names(df)) df$price <- df$ask_price
  if (!"mileage" %in% names(df) && "km_driven" %in% names(df)) df$mileage <- df$km_driven

  req_cols <- c("brand", "model", "year", "price", "mileage", "fuel", "transmission", "owner")
  missing_cols <- setdiff(req_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  missing_summary <- data.frame(
    Variable = req_cols,
    Missing = vapply(req_cols, function(v) sum(is.na(df[[v]]) | trimws(as.character(df[[v]])) == ""), numeric(1)),
    row.names = NULL
  )
  missing_summary$MissingPercent <- 100 * missing_summary$Missing / max(raw_n, 1)

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  df1 <- df |>
    dplyr::mutate(
      brand = safe_title(brand),
      model = trimws(as.character(model)),
      year = suppressWarnings(as.numeric(year)),
      price = parse_number(price),
      mileage = parse_number(mileage),
      fuel = trimws(as.character(fuel)),
      transmission = trimws(as.character(transmission)),
      owner = trimws(as.character(owner))
    ) |>
    dplyr::mutate(
      fuel = dplyr::case_when(
        grepl("hybrid", tolower(fuel)) & grepl("cng", tolower(fuel)) ~ "Hybrid/CNG",
        grepl("petrol", tolower(fuel)) ~ "Petrol",
        grepl("diesel", tolower(fuel)) ~ "Diesel",
        grepl("cng", tolower(fuel)) ~ "CNG",
        grepl("hybrid", tolower(fuel)) ~ "Hybrid",
        TRUE ~ safe_title(fuel)
      ),
      transmission = safe_title(transmission),
      owner = safe_title(owner),
      age = current_year - year
    )

  after_parse_n <- nrow(df1)

  df2 <- df1 |>
    dplyr::filter(
      !is.na(year), !is.na(price), !is.na(mileage),
      brand != "", model != "", fuel != "", transmission != "", owner != "",
      year >= 1995, year <= current_year,
      price > 0, mileage >= 0,
      age >= 0, age <= 30
    )

  after_impossible_n <- nrow(df2)
  duplicate_n <- sum(duplicated(df2))

  df3 <- df2 |>
    dplyr::distinct()

  raw_percentiles <- percentile_summary(df3, c("price", "mileage", "age"))

  price_bounds <- stats::quantile(df3$price, probs = c(0.01, 0.99), na.rm = TRUE, names = FALSE)
  mileage_bounds <- stats::quantile(df3$mileage, probs = c(0.01, 0.99), na.rm = TRUE, names = FALSE)

  df4 <- df3 |>
    dplyr::mutate(
      price_w = winsorize(price),
      mileage_w = winsorize(mileage),
      mileage_per_year_w = dplyr::if_else(age > 0, mileage_w / age, mileage_w),
      log_price = log(price_w),
      log_mileage = log1p(mileage_w),
      price_mnt = to_mnt(price),
      price_w_mnt = to_mnt(price_w)
    )

  clean_percentiles <- percentile_summary(df4, c("price_w", "mileage_w", "age"))

  diagnostics <- list(
    raw_n = raw_n,
    cleaned_n = nrow(df4),
    current_year = current_year,
    missing_summary = missing_summary,
    cleaning_steps = data.frame(
      Step = c(
        "Raw observations",
        "After parsing variables",
        "After removing impossible/missing values",
        "Exact duplicate rows removed",
        "Final observations after winsorization"
      ),
      Count = c(raw_n, after_parse_n, after_impossible_n, duplicate_n, nrow(df4))
    ),
    percentile_raw = raw_percentiles,
    percentile_clean = clean_percentiles,
    price_bounds = price_bounds,
    mileage_bounds = mileage_bounds,
    price_winsorized_n = sum(df4$price != df4$price_w, na.rm = TRUE),
    mileage_winsorized_n = sum(df4$mileage != df4$mileage_w, na.rm = TRUE)
  )

  list(data = df4, diagnostics = diagnostics)
}

# ============================================================
# 3. Model fitting
# ============================================================

fit_models <- function(df, diagnostics) {
  set.seed(123)

  top_models <- names(sort(table(df$model), decreasing = TRUE))[seq_len(min(40, length(unique(df$model))))]

  model_df <- df |>
    dplyr::mutate(
      model_group = ifelse(model %in% top_models, model, "Other"),
      brand = factor(brand),
      model_group = factor(model_group),
      fuel = factor(fuel),
      transmission = factor(transmission),
      owner = factor(owner)
    ) |>
    dplyr::filter(is.finite(log_price), is.finite(age), is.finite(mileage_w), is.finite(log_mileage))

  validate_n <- nrow(model_df)
  if (validate_n < 200) stop("Not enough clean observations for model training.")

  idx <- sample(seq_len(validate_n), size = floor(0.80 * validate_n))
  train <- model_df[idx, , drop = FALSE]
  test <- model_df[-idx, , drop = FALSE]

  f_ols <- log_price ~ age + log_mileage + fuel + transmission + owner
  f_brand <- log_price ~ age + I(age^2) + log_mileage + brand + fuel + transmission + owner
  f_robust <- log_price ~ age + I(age^2) + log_mileage + brand + fuel + transmission + owner
  f_rf <- log_price ~ age + mileage_w + mileage_per_year_w + brand + model_group + fuel + transmission + owner

  safe_lm <- function(formula, data) tryCatch(stats::lm(formula, data = data), error = function(e) NULL)
  safe_rlm <- function(formula, data) tryCatch(MASS::rlm(formula, data = data, maxit = 100), error = function(e) NULL)

  models_train <- list(
    "Random Forest" = tryCatch(randomForest::randomForest(f_rf, data = train, ntree = 250, importance = TRUE), error = function(e) NULL),
    "OLS log-linear" = safe_lm(f_ols, train),
    "Brand FE + nonlinear age" = safe_lm(f_brand, train),
    "Robust regression" = safe_rlm(f_robust, train)
  )

  models_full <- list(
    "Random Forest" = tryCatch(randomForest::randomForest(f_rf, data = model_df, ntree = 250, importance = TRUE), error = function(e) NULL),
    "OLS log-linear" = safe_lm(f_ols, model_df),
    "Brand FE + nonlinear age" = safe_lm(f_brand, model_df),
    "Robust regression" = safe_rlm(f_robust, model_df)
  )

  eval_one <- function(name, model) {
    if (is.null(model)) {
      return(data.frame(Model = name, RMSE = NA_real_, MAE = NA_real_, R2 = NA_real_))
    }
    pred_log <- tryCatch(as.numeric(stats::predict(model, newdata = test)), error = function(e) rep(NA_real_, nrow(test)))
    pred <- exp(pred_log)
    cbind(data.frame(Model = name), metric_table(test$price_w, pred))
  }

  comparison <- dplyr::bind_rows(Map(eval_one, names(models_train), models_train)) |>
    dplyr::arrange(RMSE)

  rf_test_df <- test
  rf_test_pred <- tryCatch(exp(as.numeric(stats::predict(models_train[["Random Forest"]], newdata = test))), error = function(e) rep(NA_real_, nrow(test)))
  rf_test_df$actual_price <- rf_test_df$price_w
  rf_test_df$predicted_price <- rf_test_pred
  rf_test_df$residual <- rf_test_df$actual_price - rf_test_df$predicted_price
  rf_resid_q <- stats::quantile(rf_test_df$residual, probs = c(0.10, 0.90), na.rm = TRUE, names = FALSE)

  importance_df <- data.frame()
  rf_full <- models_full[["Random Forest"]]
  if (!is.null(rf_full)) {
    imp <- tryCatch(randomForest::importance(rf_full), error = function(e) NULL)
    if (!is.null(imp)) {
      importance_df <- data.frame(
        Variable = rownames(imp),
        Importance = if ("%IncMSE" %in% colnames(imp)) imp[, "%IncMSE"] else imp[, 1],
        row.names = NULL
      ) |>
        dplyr::arrange(dplyr::desc(Importance))
    }
  }

  list(
    model_df = model_df,
    top_models = top_models,
    factor_levels = list(
      brand = levels(model_df$brand),
      model_group = levels(model_df$model_group),
      fuel = levels(model_df$fuel),
      transmission = levels(model_df$transmission),
      owner = levels(model_df$owner)
    ),
    models_full = models_full,
    comparison = comparison,
    rf_test_df = rf_test_df,
    rf_resid_q = rf_resid_q,
    importance_df = importance_df,
    diagnostics = diagnostics
  )
}

# ============================================================
# 4. UI
# ============================================================

custom_css <- "
.content-wrapper, .right-side { background-color: #f5f7fb; }
.main-header .logo { font-weight: 700; letter-spacing: .2px; }
.box { border-radius: 14px; border-top: 0 !important; box-shadow: 0 5px 18px rgba(31, 41, 55, 0.08); }
.box-header { padding: 14px 16px 8px 16px; }
.box-title { font-weight: 700; color: #1f2937; }
.box-body { padding: 16px; }
.small-box { border-radius: 14px; box-shadow: 0 5px 18px rgba(31, 41, 55, 0.08); }
.small-box h3 { font-size: 24px !important; font-weight: 800; }
.sidebar-menu > li > a { font-weight: 600; }
.form-control, .selectize-input { border-radius: 8px !important; }
.btn { border-radius: 8px; }
.dataTables_wrapper { font-size: 13px; }
.help-text { color: #6b7280; font-size: 13px; margin-top: -5px; }
"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Used Car Price"),
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      id = "tabs",
      menuItem("Үнэ таамаг", tabName = "prediction", icon = icon("car")),
      menuItem("Өгөгдлийн тойм", tabName = "data", icon = icon("database")),
      menuItem("Загварууд", tabName = "models", icon = icon("chart-line")),
      menuItem("Ойролцоо машинууд", tabName = "comparables", icon = icon("table"))
    ),
    hr(),
    fileInput("file1", "CSV дата оруулах", accept = c(".csv")),
    uiOutput("data_status"),
    hr(),
    selectInput("prediction_model", "Таамаглах загвар", choices = c(
      "Random Forest", "OLS log-linear", "Brand FE + nonlinear age", "Robust regression"
    )),
    uiOutput("brand_ui"),
    uiOutput("model_ui"),
    numericInput("year_input", "Үйлдвэрлэсэн он", value = 2018, min = 1995, max = 2030),
    numericInput("mileage_input", "Явсан км", value = 50000, min = 0, step = 1000),
    uiOutput("fuel_ui"),
    uiOutput("trans_ui"),
    uiOutput("owner_ui"),
    sliderInput("years_ahead", "Хэдэн жилийн дараах үнэ", min = 0, max = 10, value = 3, step = 1),
    numericInput("annual_km", "Жилд дунджаар явах км", value = 12000, min = 1000, step = 1000)
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    tabItems(
      tabItem(
        tabName = "prediction",
        fluidRow(
          valueBoxOutput("pred_box", width = 4),
          valueBoxOutput("future_box", width = 4),
          valueBoxOutput("model_box", width = 4)
        ),
        fluidRow(
          box(width = 8, title = "Ирээдүйн үнийн таамаг", plotlyOutput("forecast_plot", height = 430)),
          box(width = 4, title = "Сонгосон машины мэдээлэл", tableOutput("input_summary"))
        )
      ),
      tabItem(
        tabName = "data",
        fluidRow(
          valueBoxOutput("raw_box", width = 3),
          valueBoxOutput("clean_box", width = 3),
          valueBoxOutput("outlier_box", width = 3),
          valueBoxOutput("duplicate_box", width = 3)
        ),
        fluidRow(
          box(width = 6, title = "Цэвэрлэгээний алхмууд", DTOutput("cleaning_steps_table"), br(), downloadButton("download_cleaned", "Cleaned data татах")),
          box(width = 6, title = "Missing value summary", DTOutput("missing_table"))
        ),
        fluidRow(
          box(width = 6, title = "Percentile summary", DTOutput("percentile_table")),
          box(width = 6, title = "Cleaned data preview", DTOutput("cleaned_preview"))
        )
      ),
      tabItem(
        tabName = "models",
        fluidRow(
          box(width = 12, title = "Model comparison: test data (₮)", DTOutput("model_comparison_table"), br(), downloadButton("download_model_comparison", "Model comparison татах"))
        ),
        fluidRow(
          box(width = 6, title = "Actual vs Predicted: Random Forest", plotlyOutput("actual_pred_plot", height = 340)),
          box(width = 6, title = "Random Forest variable importance", plotlyOutput("varimp_plot", height = 340))
        ),
        fluidRow(
          box(width = 7, title = "Coefficient table", DTOutput("coef_table")),
          box(width = 5, title = "Coefficient interpretation", uiOutput("coef_interpretation"))
        )
      ),
      tabItem(
        tabName = "comparables",
        fluidRow(
          box(width = 12, title = "Сонгосон машинтай ойролцоо зарууд", DTOutput("nearby_table"))
        )
      )
    )
  )
)

# ============================================================
# 5. Server
# ============================================================

server <- function(input, output, session) {

  raw_data <- reactive({
    if (!is.null(input$file1)) {
      read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    } else {
      load_default_data()
    }
  })

  clean_bundle <- reactive({
    req(raw_data())
    clean_data(raw_data())
  })

  cars <- reactive(clean_bundle()$data)
  diagnostics <- reactive(clean_bundle()$diagnostics)

  model_bundle <- reactive({
    req(cars())
    fit_models(cars(), diagnostics())
  })

  output$data_status <- renderUI({
    if (!is.null(input$file1)) {
      tags$p(HTML(paste0("<b>Ачаалсан файл:</b> ", input$file1$name)), style = "color:#16a34a;")
    } else if (!is.null(load_default_data())) {
      existing <- get_default_paths()[file.exists(get_default_paths())][1]
      tags$p(HTML(paste0("<b>Автоматаар уншсан:</b><br>", basename(existing))), style = "color:#16a34a; font-size:12px;")
    } else {
      tags$p("CSV олдсонгүй. Файлаа upload хийнэ үү.", style = "color:#dc2626;")
    }
  })

  observeEvent(cars(), {
    df <- cars()
    updateNumericInput(session, "year_input", value = round(stats::median(df$year, na.rm = TRUE)), min = min(df$year, na.rm = TRUE), max = max(df$year, na.rm = TRUE))
    updateNumericInput(session, "mileage_input", value = round(stats::median(df$mileage, na.rm = TRUE), -3))
  }, ignoreInit = FALSE)

  output$brand_ui <- renderUI({
    req(cars())
    selectInput("brand", "Brand", choices = sort(unique(cars()$brand)))
  })

  output$model_ui <- renderUI({
    req(cars(), input$brand)
    choices <- cars() |>
      dplyr::filter(brand == input$brand) |>
      dplyr::pull(model) |>
      unique() |>
      sort()
    selectInput("model", "Model", choices = choices)
  })

  output$fuel_ui <- renderUI({
    req(cars())
    selectInput("fuel", "Fuel", choices = sort(unique(cars()$fuel)))
  })

  output$trans_ui <- renderUI({
    req(cars())
    selectInput("transmission", "Transmission", choices = sort(unique(cars()$transmission)))
  })

  output$owner_ui <- renderUI({
    req(cars())
    selectInput("owner", "Owner", choices = sort(unique(cars()$owner)))
  })

  new_prediction_data <- reactive({
    req(model_bundle(), input$brand, input$model, input$year_input, input$mileage_input, input$fuel, input$transmission, input$owner)
    mb <- model_bundle()
    d <- diagnostics()
    future_years <- 0:input$years_ahead
    model_group_value <- ifelse(input$model %in% mb$top_models, input$model, "Other")

    out <- data.frame(
      t = future_years,
      age = (d$current_year - input$year_input) + future_years,
      mileage_raw = input$mileage_input + future_years * input$annual_km,
      stringsAsFactors = FALSE
    )
    out$mileage_w <- clamp_vec(out$mileage_raw, d$mileage_bounds)
    out$mileage_per_year_w <- ifelse(out$age > 0, out$mileage_w / out$age, out$mileage_w)
    out$log_mileage <- log1p(out$mileage_w)
    out$brand <- factor(input$brand, levels = mb$factor_levels$brand)
    out$model_group <- factor(model_group_value, levels = mb$factor_levels$model_group)
    out$fuel <- factor(input$fuel, levels = mb$factor_levels$fuel)
    out$transmission <- factor(input$transmission, levels = mb$factor_levels$transmission)
    out$owner <- factor(input$owner, levels = mb$factor_levels$owner)
    out
  })

  predict_selected_model <- reactive({
    req(model_bundle(), new_prediction_data(), input$prediction_model)
    mb <- model_bundle()
    nd <- new_prediction_data()
    model_name <- input$prediction_model
    mod <- mb$models_full[[model_name]]

    if (is.null(mod)) {
      return(dplyr::bind_cols(nd, data.frame(pred_price = NA_real_, lwr = NA_real_, upr = NA_real_)))
    }

    if (model_name == "Random Forest") {
      pred <- exp(as.numeric(stats::predict(mod, newdata = nd)))
      lwr <- pmax(0, pred + mb$rf_resid_q[1])
      upr <- pred + mb$rf_resid_q[2]
      return(dplyr::bind_cols(nd, data.frame(pred_price = pred, lwr = lwr, upr = upr)))
    }

    if (inherits(mod, "lm")) {
      pr <- tryCatch(stats::predict(mod, newdata = nd, interval = "prediction"), error = function(e) NULL)
      if (!is.null(pr)) {
        return(dplyr::bind_cols(nd, data.frame(pred_price = exp(pr[, "fit"]), lwr = exp(pr[, "lwr"]), upr = exp(pr[, "upr"]))))
      }
    }

    pred_log <- tryCatch(as.numeric(stats::predict(mod, newdata = nd)), error = function(e) rep(NA_real_, nrow(nd)))
    resid_sd <- tryCatch(stats::sd(stats::residuals(mod), na.rm = TRUE), error = function(e) NA_real_)
    pred <- exp(pred_log)
    lwr <- if (is.finite(resid_sd)) exp(pred_log - 1.64 * resid_sd) else NA_real_
    upr <- if (is.finite(resid_sd)) exp(pred_log + 1.64 * resid_sd) else NA_real_
    dplyr::bind_cols(nd, data.frame(pred_price = pred, lwr = lwr, upr = upr))
  })

  output$pred_box <- renderValueBox({
    pv <- predict_selected_model()
    valueBox(format_price(pv$pred_price[1]), "Одоогийн таамаг үнэ", icon = icon("car"), color = "light-blue")
  })

  output$future_box <- renderValueBox({
    pv <- predict_selected_model()
    valueBox(format_price(tail(pv$pred_price, 1)), paste0(input$years_ahead, " жилийн дараах үнэ"), icon = icon("chart-line"), color = "teal")
  })

  output$model_box <- renderValueBox({
    mb <- model_bundle()
    row <- mb$comparison |> dplyr::filter(Model == input$prediction_model) |> dplyr::slice(1)
    value <- if (nrow(row) == 1 && is.finite(row$R2)) paste0("R² = ", round(row$R2, 3)) else "Metric unavailable"
    subtitle <- if (nrow(row) == 1 && is.finite(row$RMSE)) paste0("Test RMSE: ", format_price(row$RMSE)) else ""
    valueBox(value, subtitle, icon = icon("calculator"), color = "navy")
  })

  output$forecast_plot <- renderPlotly({
    pv <- predict_selected_model() |>
      dplyr::mutate(pred_mnt = to_mnt(pred_price))

    y_min <- min(pv$pred_mnt, na.rm = TRUE)
    y_max <- max(pv$pred_mnt, na.rm = TRUE)
    if (!is.finite(y_min) || !is.finite(y_max)) {
      y_min <- 0
      y_max <- 1
    }
    pad <- max((y_max - y_min) * 0.20, y_max * 0.08, 1)
    y_limits <- c(max(0, y_min - pad), y_max + pad)

    p <- ggplot(pv, aes(x = t, y = pred_mnt)) +
      geom_line(linewidth = 1.15, color = "#111827") +
      geom_point(size = 2.4, color = "#111827") +
      coord_cartesian(ylim = y_limits) +
      scale_y_continuous(labels = comma) +
      labs(x = "Ирээдүйн жил", y = "Таамаг үнэ (₮)", title = paste("Загвар:", input$prediction_model)) +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })

  output$input_summary <- renderTable({
    values <- c(
      input$prediction_model %||% "",
      input$brand %||% "",
      input$model %||% "",
      as.character(input$year_input %||% ""),
      scales::comma(input$mileage_input %||% 0),
      input$fuel %||% "",
      input$transmission %||% "",
      input$owner %||% ""
    )
    data.frame(
      Үзүүлэлт = c("Загвар", "Brand", "Model", "Year", "Mileage", "Fuel", "Transmission", "Owner"),
      Утга = values,
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$raw_box <- renderValueBox({
    valueBox(comma(diagnostics()$raw_n), "Raw observations", icon = icon("database"), color = "light-blue")
  })

  output$clean_box <- renderValueBox({
    valueBox(comma(diagnostics()$cleaned_n), "Clean observations", icon = icon("check"), color = "teal")
  })

  output$outlier_box <- renderValueBox({
    d <- diagnostics()
    valueBox(comma(d$price_winsorized_n + d$mileage_winsorized_n), "Winsorized values", icon = icon("sliders"), color = "navy")
  })

  output$duplicate_box <- renderValueBox({
    d <- diagnostics()
    duplicate_count <- d$cleaning_steps$Count[d$cleaning_steps$Step == "Exact duplicate rows removed"]
    valueBox(comma(duplicate_count), "Duplicates removed", icon = icon("copy"), color = "purple")
  })

  output$cleaning_steps_table <- renderDT({
    datatable(diagnostics()$cleaning_steps, options = list(pageLength = 6, dom = "t"), rownames = FALSE) |>
      formatRound("Count", digits = 0, mark = ",")
  })

  output$missing_table <- renderDT({
    datatable(diagnostics()$missing_summary, options = list(pageLength = 8, dom = "t"), rownames = FALSE) |>
      formatRound(c("Missing", "MissingPercent"), digits = 2)
  })

  output$percentile_table <- renderDT({
    raw <- diagnostics()$percentile_raw |> dplyr::mutate(Stage = "Before")
    cleaned <- diagnostics()$percentile_clean |> dplyr::mutate(Stage = "After")
    out <- dplyr::bind_rows(raw, cleaned) |> dplyr::select(Stage, dplyr::everything())
    datatable(out, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) |>
      formatRound(c("p1", "p5", "p50", "p95", "p99"), digits = 0, mark = ",")
  })

  output$cleaned_preview <- renderDT({
    df <- cars() |>
      dplyr::select(brand, model, year, age, mileage, mileage_w, fuel, transmission, owner, price_mnt, price_w_mnt) |>
      head(150)
    datatable(df, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE) |>
      formatRound(c("mileage", "mileage_w", "price_mnt", "price_w_mnt"), digits = 0, mark = ",")
  })

  output$download_cleaned <- downloadHandler(
    filename = function() paste0("cleaned_cars_", Sys.Date(), ".csv"),
    content = function(file) write.csv(cars(), file, row.names = FALSE)
  )

  output$model_comparison_table <- renderDT({
    out <- model_bundle()$comparison |>
      dplyr::mutate(RMSE = to_mnt(RMSE), MAE = to_mnt(MAE))
    datatable(out, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE) |>
      formatRound(c("RMSE", "MAE"), digits = 0, mark = ",") |>
      formatRound("R2", digits = 4)
  })

  output$download_model_comparison <- downloadHandler(
    filename = function() paste0("model_comparison_", Sys.Date(), ".csv"),
    content = function(file) {
      out <- model_bundle()$comparison |>
        dplyr::mutate(RMSE_MNT = to_mnt(RMSE), MAE_MNT = to_mnt(MAE))
      write.csv(out, file, row.names = FALSE)
    }
  )

  output$actual_pred_plot <- renderPlotly({
    df <- model_bundle()$rf_test_df |>
      dplyr::filter(is.finite(actual_price), is.finite(predicted_price)) |>
      dplyr::mutate(actual_mnt = to_mnt(actual_price), predicted_mnt = to_mnt(predicted_price))
    p <- ggplot(df, aes(x = actual_mnt, y = predicted_mnt)) +
      geom_point(alpha = 0.45, color = "#374151") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      labs(x = "Бодит үнэ (₮)", y = "Таамагласан үнэ (₮)") +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })

  output$varimp_plot <- renderPlotly({
    df <- model_bundle()$importance_df
    validate(need(nrow(df) > 0, "Variable importance unavailable."))
    df <- df |> dplyr::slice_head(n = 12)
    p <- ggplot(df, aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_col() +
      coord_flip() +
      labs(x = "Variable", y = "Importance") +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })

  output$coef_table <- renderDT({
    if (input$prediction_model == "Random Forest") {
      return(datatable(model_bundle()$importance_df, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE))
    }
    coef_df <- extract_coef_table(model_bundle()$models_full[[input$prediction_model]])
    num_cols <- names(coef_df)[vapply(coef_df, is.numeric, logical(1))]
    dt <- datatable(coef_df, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
    if (length(num_cols) > 0) dt <- formatRound(dt, columns = num_cols, digits = 5)
    dt
  })

  output$coef_interpretation <- renderUI({
    if (input$prediction_model == "Random Forest") {
      return(tags$div(
        tags$p("Random Forest нь prediction буюу үнэ таамаглахад ашиглагдана."),
        tags$p("Коэффициентээр тайлбарлахын оронд variable importance графикаар аль хувьсагч илүү чухал байгааг харна.")
      ))
    }

    mod <- model_bundle()$models_full[[input$prediction_model]]
    cf <- extract_coef_table(mod)
    est_col <- intersect(c("Estimate", "Value"), names(cf))[1]
    if (is.na(est_col)) return(tags$p("Coefficient interpretation unavailable."))

    age_beta <- cf[[est_col]][cf$Variable == "age"]
    mileage_beta <- cf[[est_col]][cf$Variable == "log_mileage"]

    age_text <- if (length(age_beta) == 1 && is.finite(age_beta)) {
      paste0("Age coefficient = ", round(age_beta, 4), ". Нас 1 жилээр нэмэгдэхэд үнэ ойролцоогоор ", round((exp(age_beta) - 1) * 100, 2), "% өөрчлөгдөнө.")
    } else "Age coefficient шууд олдсонгүй."

    mileage_text <- if (length(mileage_beta) == 1 && is.finite(mileage_beta)) {
      paste0("Log mileage coefficient = ", round(mileage_beta, 4), ". Явсан км өсөхөд үнэ хэрхэн өөрчлөгдөхийг харуулна.")
    } else "Mileage coefficient шууд олдсонгүй."

    tags$ul(
      tags$li(age_text),
      tags$li(mileage_text),
      tags$li("Эдгээр загварууд log(price)-ийг ашигласан тул коэффициентийг ойролцоогоор хувийн өөрчлөлтөөр уншина."),
      tags$li("Econometric model-ууд хүчин зүйлсийн нөлөөг тайлбарлах, Random Forest үнэ таамаглах зорилготой.")
    )
  })

  output$nearby_table <- renderDT({
    req(input$brand, input$model, input$year_input, input$mileage_input, input$fuel, input$transmission, input$owner)
    current_pred <- predict_selected_model()$pred_price[1]

    df <- cars() |>
      dplyr::filter(
        brand == input$brand,
        abs(year - input$year_input) <= 3,
        abs(mileage - input$mileage_input) <= 50000
      ) |>
      dplyr::mutate(
        similarity_score = 100 -
          25 * as.integer(model != input$model) -
          10 * as.integer(fuel != input$fuel) -
          10 * as.integer(transmission != input$transmission) -
          5 * as.integer(owner != input$owner) -
          pmin(abs(year - input$year_input) * 5, 20) -
          pmin(abs(mileage - input$mileage_input) / 3000, 20),
        predicted_price = current_pred,
        difference = price_w - predicted_price,
        difference_percent = 100 * difference / predicted_price,
        listed_price_mnt = to_mnt(price_w),
        predicted_price_mnt = to_mnt(predicted_price),
        difference_mnt = to_mnt(difference)
      ) |>
      dplyr::arrange(dplyr::desc(similarity_score), abs(difference)) |>
      dplyr::select(
        brand, model, year, mileage, fuel, transmission, owner,
        listed_price_mnt, predicted_price_mnt, difference_mnt, difference_percent, similarity_score
      ) |>
      head(30)

    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) |>
      formatRound(c("mileage", "listed_price_mnt", "predicted_price_mnt", "difference_mnt"), digits = 0, mark = ",") |>
      formatRound(c("difference_percent", "similarity_score"), digits = 2)
  })
}

shinyApp(ui, server)

