# Archived Summary tab components.
# This file is intentionally not sourced by app.R. Source it and add the returned
# UI/plot components back to app.R if these Summary elements are needed again.

archived_strength_of_fit_step <- function(R, H_DXDY, H_DX2, YB, SIG, fmt, frac, step_card) {
  step_card(10, "Strength of the fit (r and R²)",
    shiny::div(class = "formula", "r = ",
      frac(paste0(SIG, H_DXDY), paste0("√[ ", SIG, H_DX2, " · ", SIG, "(Y − ", YB, ")² ]"))),
    shiny::div(class = "subst", paste0(SIG, "(Y − ", YB, ")² = ", fmt(R$Syy))),
    shiny::div(class = "subst", "r = ",
      frac(fmt(R$Sxy), paste0("√(", fmt(R$Sxx), " × ", fmt(R$Syy), ")")),
      paste0(" = ", fmt(R$r))),
    shiny::div(class = "subst", paste0("R² = r² = ", fmt(R$r2))),
    answer = paste0("r = ", fmt(R$r), "     R² = ", fmt(R$r2))
  )
}

archived_summary_metrics <- function(R, fmt) {
  stat <- function(k, v) {
    shiny::div(
      class = "stat",
      shiny::div(class = "k", k),
      shiny::div(class = "v", v)
    )
  }

  shiny::div(
    class = "stat-grid",
    stat("Correlation (r)", fmt(R$r)),
    stat("R²", fmt(R$r2)),
    stat("Std. error of estimate", fmt(R$se))
  )
}

archived_fit_plot_subtitle <- function(R, fmt) {
  sprintf(
    "Y = %s %s %sX   (R² = %s)",
    fmt(R$a),
    if (R$b < 0) "-" else "+",
    fmt(abs(R$b)),
    fmt(R$r2)
  )
}

archived_residual_plot <- function(R, PAL, plot_theme) {
  df <- data.frame(x = R$x, res = R$res)

  ggplot2::ggplot(df, ggplot2::aes(x, res)) +
    ggplot2::geom_hline(yintercept = 0, colour = PAL$accent, linewidth = 1) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = x, yend = 0),
      colour = "grey55",
      linetype = "dashed"
    ) +
    ggplot2::geom_point(colour = PAL$pred, size = 3.5) +
    ggplot2::labs(
      title = "Residuals (Actual Y - Predicted Y)",
      x = "X",
      y = "Residual"
    ) +
    plot_theme()
}
