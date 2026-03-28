# Central Limit Theorem - animint2 port
# animation package original: https://yihui.org/animation/example/clt-ani/
# GSoC 2026 - animint2 medium task

library(animint2)

obs  <- 1000
nmax <- 50
FUN  <- rnorm
theoretical_mean <- 0
theoretical_sd   <- 1

set.seed(42)

means_list <- lapply(seq_len(nmax), function(n) {
  means <- replicate(obs, mean(FUN(n)))
  data.frame(
    n                 = n,
    sample_size_label = paste0("n = ", n),
    x                 = means,
    sim_id            = seq_len(obs)
  )
})
means_df <- do.call(rbind, means_list)

density_df <- do.call(rbind, lapply(seq_len(nmax), function(n) {
  vals <- means_df$x[means_df$n == n]
  d    <- density(vals, n = 256)
  data.frame(
    n                 = n,
    sample_size_label = paste0("n = ", n),
    x                 = d$x,
    y                 = d$y,
    point_index       = seq_along(d$x)
  )
}))

normal_df <- do.call(rbind, lapply(seq_len(nmax), function(n) {
  theo_sd <- theoretical_sd / sqrt(n)
  xs      <- seq(theoretical_mean - 4 * theo_sd,
                 theoretical_mean + 4 * theo_sd,
                 length.out = 256)
  data.frame(
    n                 = n,
    sample_size_label = paste0("n = ", n),
    x                 = xs,
    y                 = dnorm(xs, mean = theoretical_mean, sd = theo_sd),
    point_index       = seq_along(xs)
  )
}))

pval_df <- do.call(rbind, lapply(seq_len(nmax), function(n) {
  vals <- means_df$x[means_df$n == n]
  sw   <- shapiro.test(sample(vals, min(length(vals), 5000)))
  data.frame(
    n                 = n,
    sample_size_label = paste0("n = ", n),
    pvalue            = sw$p.value
  )
}))

src_raw <- FUN(obs * 5)
src_d   <- density(src_raw, n = 256)
src_df  <- data.frame(x = src_d$x, y = src_d$y)

metrics_df <- do.call(rbind, lapply(seq_len(nmax), function(n) {
  vals <- means_df$x[means_df$n == n]
  data.frame(
    n                 = n,
    sample_size_label = paste0("n = ", n),
    emp_mean          = mean(vals),
    emp_sd            = sd(vals),
    theo_sd           = theoretical_sd / sqrt(n),
    pvalue            = pval_df$pvalue[pval_df$n == n]
  )
}))

pmeans <- ggplot() +
  geom_tallrect(
    data         = metrics_df,
    aes(xmin = n - 0.5, xmax = n + 0.5),
    clickSelects = "sample_size_label",
    alpha        = 0.2,
    fill         = "gold"
  ) +
  geom_line(
    data         = density_df,
    aes(x = x, y = y, key = point_index),
    showSelected = "sample_size_label",
    color        = "steelblue",
    size         = 1.2
  ) +
  geom_line(
    data         = normal_df,
    aes(x = x, y = y, key = point_index),
    showSelected = "sample_size_label",
    color        = "red",
    linetype     = "dashed",
    size         = 1
  ) +
  labs(
    title = "Distribution of Sample Means",
    x     = "Sample mean",
    y     = "Density"
  ) +
  theme_bw()

ppval <- ggplot() +
  geom_tallrect(
    data         = metrics_df,
    aes(xmin = n - 0.5, xmax = n + 0.5),
    clickSelects = "sample_size_label",
    alpha        = 0.2,
    fill         = "gold"
  ) +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  geom_line(
    data  = pval_df,
    aes(x = n, y = pvalue),
    color = "darkgreen",
    size  = 0.9
  ) +
  geom_point(
    data  = pval_df,
    aes(x = n, y = pvalue),
    color = "darkgreen",
    size  = 1.5
  ) +
  geom_point(
    data         = pval_df,
    aes(x = n, y = pvalue, key = n),
    showSelected = "sample_size_label",
    color        = "orange",
    size         = 5
  ) +
  labs(
    title = "Shapiro-Wilk p-value vs Sample Size",
    x     = "n",
    y     = "p-value"
  ) +
  theme_bw()

psd <- ggplot() +
  geom_tallrect(
    data         = metrics_df,
    aes(xmin = n - 0.5, xmax = n + 0.5),
    clickSelects = "sample_size_label",
    alpha        = 0.2,
    fill         = "gold"
  ) +
  geom_line(
    data  = metrics_df,
    aes(x = n, y = emp_sd),
    color = "steelblue",
    size  = 0.9
  ) +
  geom_line(
    data     = metrics_df,
    aes(x = n, y = theo_sd),
    color    = "red",
    linetype = "dashed",
    size     = 0.9
  ) +
  geom_point(
    data         = metrics_df,
    aes(x = n, y = emp_sd, key = n),
    showSelected = "sample_size_label",
    color        = "orange",
    size         = 5
  ) +
  labs(
    title = "SD of Sample Means vs Sample Size",
    x     = "n",
    y     = "SD"
  ) +
  theme_bw()

psrc <- ggplot() +
  geom_line(
    data  = src_df,
    aes(x = x, y = y),
    color = "gray40",
    size  = 1
  ) +
  geom_area(
    data  = src_df,
    aes(x = x, y = y),
    fill  = "gray70",
    alpha = 0.4
  ) +
  labs(
    title = "Source Distribution",
    x     = "x",
    y     = "Density"
  ) +
  theme_bw()

viz <- animint(
  means    = pmeans,
  pval     = ppval,
  sdplot   = psd,
  src      = psrc,
  first    = list(sample_size_label = "n = 1"),
  duration = list(sample_size_label = 1000),
  time     = list(variable = "sample_size_label", ms = 2000),
  title    = "Central Limit Theorem",
  source   = "https://github.com/Nishita-shah1/clt-animint"
)

animint2pages(viz, "clt-animint")
print(viz)
