# =============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 2 - Technical Analysis using R, Preliminary Stage
# Author : Anthony Dibor
# Repo   : https://github.com/Andib77/TechnicalAnalysis
#
# What this script does
#   Step 3  install_packages()      install / load quantmod, TTR and helpers
#   Step 4  portfolio.txt           stock symbols, one per line
#   Step 5  load_stock_data()       read portfolio.txt, download each symbol
#                                   with quantmod, store one data frame each
#   Step 6  calculate_statistics()  moving averages, mean, mode, median, SD
#   Step 7  display_*() functions   tables and charts of the data and stats
#
# How to run: open this file in RStudio, set the working directory to the
# folder that holds portfolio.txt (Session > Set Working Directory > To Source
# File Location) and click "Source". An internet connection is needed for
# Yahoo Finance.
# =============================================================================


# -----------------------------------------------------------------------------
# STEP 3 - Install and load the packages needed for technical analysis
# -----------------------------------------------------------------------------
install_packages <- function(pkgs = c("quantmod", "TTR", "xts", "zoo")) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      message("Installing package: ", p)
      install.packages(p, repos = "https://cloud.r-project.org")
    }
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
  message("Packages loaded: ", paste(pkgs, collapse = ", "))
  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# STEP 4 helper - read the symbols from portfolio.txt
# -----------------------------------------------------------------------------
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) stop("Cannot find ", file, " in ", getwd())
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- toupper(symbols[symbols != "" & !startsWith(symbols, "#")])
  unique(symbols)
}


# -----------------------------------------------------------------------------
# STEP 5 - Import and load the stock data
#   Reads portfolio.txt, downloads daily prices for every symbol from Yahoo
#   Finance with quantmod::getSymbols(), and returns a named list holding a
#   separate data frame for each symbol. Each data frame is also assigned to
#   its own variable in the global environment (e.g. AAPL_df) so it can be
#   inspected in the RStudio Environment pane.
# -----------------------------------------------------------------------------
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to   = Sys.Date(),
                            src  = "yahoo") {
  symbols <- read_portfolio(file)
  stock_data <- list()

  for (sym in symbols) {
    message("Downloading ", sym, " ...")
    result <- tryCatch(
      getSymbols(sym, src = src, from = from, to = to, auto.assign = FALSE),
      error = function(e) { warning("Could not load ", sym, ": ", conditionMessage(e)); NULL }
    )
    if (is.null(result)) next

    # Convert the xts object to a plain data frame with clean column names
    df <- data.frame(Date = index(result), coredata(result), row.names = NULL)
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    df <- na.omit(df)

    stock_data[[sym]] <- df
    assign(paste0(gsub("[^A-Za-z0-9]", "_", sym), "_df"), df, envir = .GlobalEnv)
  }

  message("Loaded ", length(stock_data), " of ", length(symbols), " symbols.")
  stock_data
}


# -----------------------------------------------------------------------------
# STEP 6 - Compute basic statistics
# -----------------------------------------------------------------------------

# Base R has no function for the statistical mode, so we write one.
# Prices are rounded first (to the cent by default) because continuous prices
# rarely repeat exactly. Ties return the first (lowest) most frequent value.
stat_mode <- function(x, digits = 2) {
  x <- round(x[!is.na(x)], digits)
  if (length(x) == 0) return(NA_real_)
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

# Takes ONE stock's data frame and returns its statistics as a list:
#   $summary          one-row data frame (mean, median, mode, SD, ...)
#   $moving_averages  the data frame with SMA20, SMA50 and EMA20 added
calculate_statistics <- function(df, price_col = "Close",
                                 short_n = 20, long_n = 50) {
  price <- df[[price_col]]

  ma <- df
  ma$SMA_short <- if (nrow(df) >= short_n) SMA(price, n = short_n) else NA
  ma$SMA_long  <- if (nrow(df) >= long_n)  SMA(price, n = long_n)  else NA
  ma$EMA_short <- if (nrow(df) >= short_n) EMA(price, n = short_n) else NA
  ma$Daily_Return <- c(NA, diff(price) / head(price, -1))

  summary <- data.frame(
    Start      = min(df$Date),
    End        = max(df$Date),
    Days       = nrow(df),
    Last_Close = tail(price, 1),
    Mean       = mean(price, na.rm = TRUE),
    Median     = median(price, na.rm = TRUE),
    Mode       = stat_mode(price),
    Std_Dev    = sd(price, na.rm = TRUE),
    Min        = min(price, na.rm = TRUE),
    Max        = max(price, na.rm = TRUE),
    SMA_short  = tail(ma$SMA_short, 1),
    SMA_long   = tail(ma$SMA_long, 1),
    Return_Pct = 100 * (tail(price, 1) / head(price, 1) - 1),
    Avg_Volume = mean(df$Volume, na.rm = TRUE)
  )

  list(summary = summary, moving_averages = ma)
}

# Runs calculate_statistics() for every stock and also builds one combined
# summary table (one row per symbol) for easy comparison.
calculate_all_statistics <- function(stock_data, ...) {
  stats <- lapply(stock_data, calculate_statistics, ...)
  combined <- do.call(rbind, lapply(names(stats), function(s)
    cbind(Symbol = s, stats[[s]]$summary)))
  rownames(combined) <- NULL
  list(per_stock = stats, summary_table = combined)
}


# -----------------------------------------------------------------------------
# STEP 7 - Utilities to display the output
#   Several display styles, modelled on how Yahoo Finance presents data:
#   a quote header, a historical-prices table, a statistics panel, a portfolio
#   comparison table, and price / volume / comparison charts.
# -----------------------------------------------------------------------------

fmt_num <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ",")

# 1. Yahoo-style quote header: last price, change and day range
display_quote <- function(df, symbol) {
  n <- nrow(df); last <- df[n, ]; prev <- df[n - 1, "Close"]
  chg <- last$Close - prev; pct <- 100 * chg / prev
  cat("\n================================================\n")
  cat(sprintf(" %s   %s   %s%s (%s%s%%)\n", symbol, fmt_num(last$Close),
              ifelse(chg >= 0, "+", ""), fmt_num(chg),
              ifelse(pct >= 0, "+", ""), fmt_num(pct)))
  cat(sprintf(" As of %s\n", format(last$Date, "%b %d, %Y")))
  cat(sprintf(" Open %s | Day range %s - %s | Volume %s\n",
              fmt_num(last$Open), fmt_num(last$Low), fmt_num(last$High),
              fmt_num(last$Volume, 0)))
  cat("================================================\n")
}

# 2. Historical prices table (newest first, like Yahoo's "Historical Data")
display_stock_data <- function(df, symbol, n = 10) {
  cat("\nHistorical prices for", symbol, "- last", n, "trading days\n")
  out <- df[order(df$Date, decreasing = TRUE), ][seq_len(min(n, nrow(df))), ]
  out[, c("Open", "High", "Low", "Close", "Adjusted")] <-
    lapply(out[, c("Open", "High", "Low", "Close", "Adjusted")], fmt_num)
  out$Volume <- fmt_num(out$Volume, 0)
  print(out, row.names = FALSE)
  invisible(out)
}

# 3. Statistics panel for one stock (like Yahoo's "Statistics" tab)
display_statistics <- function(stats, symbol) {
  s <- stats$summary
  cat("\nStatistics for", symbol, sprintf("(%s to %s, %d days)\n", s$Start, s$End, s$Days))
  panel <- data.frame(
    Measure = c("Last close", "Mean", "Median", "Mode", "Standard deviation",
                "Minimum", "Maximum", "SMA (short)", "SMA (long)",
                "Period return %", "Average volume"),
    Value = c(fmt_num(s$Last_Close), fmt_num(s$Mean), fmt_num(s$Median),
              fmt_num(s$Mode), fmt_num(s$Std_Dev), fmt_num(s$Min),
              fmt_num(s$Max), fmt_num(s$SMA_short), fmt_num(s$SMA_long),
              fmt_num(s$Return_Pct), fmt_num(s$Avg_Volume, 0)))
  print(panel, row.names = FALSE, right = FALSE)
  invisible(panel)
}

# 4. Portfolio comparison table (one row per stock)
display_portfolio_summary <- function(summary_table) {
  cat("\nPortfolio summary (closing prices)\n")
  out <- summary_table[, c("Symbol", "Last_Close", "Mean", "Median", "Mode",
                           "Std_Dev", "Min", "Max", "Return_Pct")]
  out[-1] <- lapply(out[-1], round, 2)
  print(out, row.names = FALSE)
  invisible(out)
}

# 5. Price chart with moving averages and volume (quantmod chartSeries)
plot_price_chart <- function(df, symbol, short_n = 20, long_n = 50) {
  x <- xts(df[, c("Open", "High", "Low", "Close", "Volume", "Adjusted")],
           order.by = df$Date)
  chartSeries(x, name = paste(symbol, "- daily price"), theme = chartTheme("white"),
              TA = sprintf("addVo();addSMA(n=%d,col='blue');addSMA(n=%d,col='red')",
                           short_n, long_n))
}

# 6. Closing price line chart with mean, median and +/- 1 SD bands (base R)
plot_statistics_chart <- function(stats, symbol) {
  ma <- stats$moving_averages; s <- stats$summary
  plot(ma$Date, ma$Close, type = "l", lwd = 2, col = "black",
       main = paste(symbol, "- closing price and statistics"),
       xlab = "Date", ylab = "Price (USD)")
  lines(ma$Date, ma$SMA_short, col = "blue")
  lines(ma$Date, ma$SMA_long,  col = "red")
  abline(h = s$Mean, col = "darkgreen", lty = 2)
  abline(h = s$Median, col = "purple", lty = 3)
  abline(h = s$Mean + c(-1, 1) * s$Std_Dev, col = "grey50", lty = 4)
  legend("topright", bg = "white", cex = 0.8,
         legend = c("Close", "SMA short", "SMA long", "Mean", "Median", "Mean +/- 1 SD"),
         col = c("black", "blue", "red", "darkgreen", "purple", "grey50"),
         lty = c(1, 1, 1, 2, 3, 4))
}

# 7. Portfolio comparison: growth of $100 invested in each stock
plot_portfolio_comparison <- function(stock_data) {
  norm <- lapply(stock_data, function(df) 100 * df$Close / df$Close[1])
  cols <- rainbow(length(norm))
  all_dates <- do.call(c, lapply(stock_data, `[[`, "Date"))
  plot(range(all_dates), range(unlist(norm)), type = "n",
       xlab = "Date", ylab = "Value of $100",
       main = "Portfolio comparison - growth of $100")
  for (i in seq_along(norm)) lines(stock_data[[i]]$Date, norm[[i]], col = cols[i], lwd = 2)
  abline(h = 100, lty = 2, col = "grey")
  legend("topleft", legend = names(norm), col = cols, lwd = 2, bty = "n", cex = 0.8)
}

# 8. Bar chart of mean price with standard deviation as error bars
plot_mean_sd_bars <- function(summary_table) {
  mids <- barplot(summary_table$Mean, names.arg = summary_table$Symbol,
                  col = "steelblue", ylim = c(0, max(summary_table$Mean + summary_table$Std_Dev) * 1.1),
                  main = "Mean closing price with standard deviation", ylab = "Price (USD)")
  arrows(mids, summary_table$Mean - summary_table$Std_Dev,
         mids, summary_table$Mean + summary_table$Std_Dev,
         angle = 90, code = 3, length = 0.05)
}


# -----------------------------------------------------------------------------
# MAIN - run the whole preliminary-stage workflow
# -----------------------------------------------------------------------------
run_analysis <- function(file = "portfolio.txt", days = 365,
                         save_plots = FALSE, out_dir = "output") {
  install_packages()

  stock_data <- load_stock_data(file, from = Sys.Date() - days)
  if (length(stock_data) == 0) stop("No stock data was loaded. Check your internet connection.")

  stats <- calculate_all_statistics(stock_data)

  # Text output for every stock
  for (sym in names(stock_data)) {
    display_quote(stock_data[[sym]], sym)
    display_stock_data(stock_data[[sym]], sym)
    display_statistics(stats$per_stock[[sym]], sym)
  }
  display_portfolio_summary(stats$summary_table)

  # Charts (shown in the RStudio Plots pane, or saved as PNG files)
  if (save_plots) dir.create(out_dir, showWarnings = FALSE)
  for (sym in names(stock_data)) {
    if (save_plots) png(file.path(out_dir, paste0(sym, "_price.png")), 1000, 600)
    plot_price_chart(stock_data[[sym]], sym)
    if (save_plots) dev.off()
    if (save_plots) png(file.path(out_dir, paste0(sym, "_stats.png")), 1000, 600)
    plot_statistics_chart(stats$per_stock[[sym]], sym)
    if (save_plots) dev.off()
  }
  if (save_plots) png(file.path(out_dir, "portfolio_comparison.png"), 1000, 600)
  plot_portfolio_comparison(stock_data)
  if (save_plots) dev.off()
  if (save_plots) png(file.path(out_dir, "mean_sd_bars.png"), 1000, 600)
  plot_mean_sd_bars(stats$summary_table)
  if (save_plots) dev.off()

  if (save_plots) write.csv(stats$summary_table, file.path(out_dir, "summary_statistics.csv"), row.names = FALSE)

  invisible(list(data = stock_data, stats = stats))
}

# Run automatically when the file is sourced in RStudio or with Rscript.
if (sys.nframe() == 0 || interactive()) {
  results <- run_analysis(save_plots = TRUE)
}
