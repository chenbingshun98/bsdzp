
minvariance <- function(assets, mu = 0.005) {
  return <- log(tail(assets, -1) / head(assets, -1))
  Q <- rbind(
    cov(return), rep(1, ncol(assets)),
    colMeans(return)
  )
  Q <- cbind(Q, rbind(t(tail(Q, 2)), matrix(0, 2, 2)))
  b <- c(rep(0, ncol(assets)), 1, mu)
  solve(Q, b)
}

install.packages("Quandl")
library(Quandl)
Quandl('USER_1KR/1KT',start_date = '2008-01-01', end_date = '2012-12-31')

assets <- IT[, -1]
return <- log(tail(assets, -1) / head(assets, -1))


library(tidyverse)
library(tidyquant)

Ra <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")
Ra

Rb <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")
Rb

RaRb <- left_join(Ra, Rb, by = c("date" = "date"))
RaRb

RaRb_capm <- RaRb %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)
RaRb_capm

RaRb_capm %>% select(symbol, Alpha, Beta)

args(SharpeRatio)

stock_prices <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31")
stock_prices
?tq_get

stock_returns_monthly <- stock_prices %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")
stock_returns_monthly

stock_returns_monthly %>%
  tq_performance(
    Ra = Ra, 
    Rb = NULL, 
    performance_fun = SharpeRatio
  )

stock_returns_monthly %>%
  tq_performance(
    Ra = Ra, 
    Rb = NULL, 
    performance_fun = SharpeRatio, 
    Rf = 0.03 / 12, 
    p  = 0.99
  )

stock_returns_monthly <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")
stock_returns_monthly

baseline_returns_monthly <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")
baseline_returns_monthly

wts <- c(0.5, 0.0, 0.5)
portfolio_returns_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = wts, 
               col_rename  = "Ra")
portfolio_returns_monthly

baseline_returns_monthly <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")
baseline_returns_monthly

wts_map <- tibble(
  symbols = c("AAPL", "NFLX"),
  weights = c(0.5, 0.5)
)
wts_map

stock_returns_monthly %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = wts_map, 
               col_rename  = "Ra_using_wts_map")

RaRb_single_portfolio <- left_join(portfolio_returns_monthly, 
                                   baseline_returns_monthly,
                                   by = "date")
RaRb_single_portfolio

RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)

stock_returns_monthly <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")

baseline_returns_monthly <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Rb")

stock_returns_monthly_multi <- stock_returns_monthly %>%
  tq_repeat_df(n = 3)
stock_returns_monthly_multi

weights <- c(
  0.50, 0.25, 0.25,
  0.25, 0.50, 0.25,
  0.25, 0.25, 0.50
)
stocks <- c("AAPL", "GOOG", "NFLX")
weights_table <-  tibble(stocks) %>%
  tq_repeat_df(n = 3) %>%
  bind_cols(tibble(weights)) %>%
  group_by(portfolio)
weights_table

portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = weights_table, 
               col_rename  = "Ra")
portfolio_returns_monthly_multi

RaRb_multiple_portfolio <- left_join(portfolio_returns_monthly_multi, 
                                     baseline_returns_monthly,
                                     by = "date")
RaRb_multiple_portfolio

RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)

RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = SharpeRatio)

wts <- c(0.5, 0.0, 0.5)
portfolio_returns_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = wts, 
               col_rename  = "Ra")
portfolio_returns_monthly %>%
  ggplot(aes(x = date, y = Ra)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Portfolio Returns",
       subtitle = "50% AAPL, 0% GOOG, and 50% NFLX",
       caption = "Shows an above-zero trend meaning positive returns",
       x = "", y = "Monthly Returns") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)
