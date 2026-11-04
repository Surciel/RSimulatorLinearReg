# =============================================================================
#  Linear Regression Simulator  (R Programming Language)
#  Group 5 - Cubacub, Klarc | Gomez, Ken Chie | Rey, Adrian Louis
#
#  Run from RStudio / R console:
#      install.packages(c("shiny", "ggplot2"))     # first time only
#      shiny::runApp("linear_regression_simulator")  # or open app.R and press "Run App"
#
#  The only manual input is the X / Y table. Everything else (means, deviations,
#  slope, intercept, predictions, r, R^2, tables, graphs) is computed in R.
# =============================================================================

library(shiny)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. Color palette  (NOT FINAL - change the values here and the whole app updates)
# -----------------------------------------------------------------------------
PAL <- list(
  bar    = "#B5C99A",   # top bar / buttons (green)
  bg     = "#C9D1DB",   # page background (blue-grey)
  card   = "#FFFFFF",   # cards / table cells
  ink    = "#1F2933",   # main text
  head   = "#FFF2CC",   # table header (light yellow)
  muted  = "#D9D9D9",   # locked / grey cells
  accent = "#2F5D8A",   # regression line
  pred   = "#C0504D"    # predicted Y points
)

# -----------------------------------------------------------------------------
# 2. Core calculations  (pure R, mirrors the class / notebook method)
# -----------------------------------------------------------------------------
calc_regression <- function(x, y) {
  n    <- length(x)
  xbar <- mean(x)
  ybar <- mean(y)

  dx   <- x - xbar            # X - X-bar
  dy   <- y - ybar            # Y - Y-bar
  dxdy <- dx * dy             # (X - X-bar)(Y - Y-bar)
  dx2  <- dx^2                # (X - X-bar)^2
  dy2  <- dy^2                # (Y - Y-bar)^2  (needed for r)

  Sxy <- sum(dxdy)
  Sxx <- sum(dx2)
  Syy <- sum(dy2)

  b    <- Sxy / Sxx           # slope
  a    <- ybar - b * xbar     # intercept
  yhat <- a + b * x           # predicted Y
  res  <- y - yhat            # residuals

  r  <- if (Syy > 0) Sxy / sqrt(Sxx * Syy) else NA_real_
  r2 <- if (Syy > 0) r^2 else NA_real_
  sse <- sum(res^2)
  se  <- if (n > 2) sqrt(sse / (n - 2)) else NA_real_

  list(n = n, x = x, y = y, xbar = xbar, ybar = ybar,
       dx = dx, dy = dy, dxdy = dxdy, dx2 = dx2, dy2 = dy2,
       Sx = sum(x), Sy = sum(y), Sxy = Sxy, Sxx = Sxx, Syy = Syy,
       b = b, a = a, yhat = yhat, res = res,
       r = r, r2 = r2, sse = sse, se = se)
}

# Returns an error message (string) or NULL if the data are usable.
check_data <- function(d) {
  if (nrow(d) < 2)
    return("Please enter at least 2 complete rows of X and Y values.")
  if (any(is.na(d$x) | is.na(d$y)))
    return("Every row needs both an X and a Y value. Fill in or remove incomplete rows.")
  if (sum((d$x - mean(d$x))^2) < 1e-12)
    return("All X values are identical, so the slope cannot be computed. Use at least two different X values.")
  NULL
}

# -----------------------------------------------------------------------------
# 3. Formatting helpers
# -----------------------------------------------------------------------------
fmt <- function(v, d = 4) {
  v <- round(v, d) + 0                       # "+ 0" turns -0 into 0
  out <- ifelse(is.na(v), "n/a",
                formatC(v, format = "f", digits = d, drop0trailing = TRUE))
  sub("^-", "−", out)                   # proper minus sign
}
graph_interpretation <- function(R) {
  slope <- round(R$b, 4)
  if (slope > 0) {
    list(
      classification = "Positive correlation",
      explanation = paste0(
        "As X increases, Y also tends to increase, as shown by the upward-sloping regression line (Y = ",
        fmt(R$a), " + ", fmt(R$b), "X)."
      )
    )
  } else if (slope < 0) {
    list(
      classification = "Negative correlation",
      explanation = paste0(
        "As X increases, Y tends to decrease, as shown by the downward-sloping regression line (Y = ",
        fmt(R$a), " - ", fmt(abs(R$b)), "X)."
      )
    )
  } else {
    list(
      classification = "No correlation",
      explanation = paste0(
        "As X increases, Y shows no consistent pattern or trend, as shown by the flat/horizontal regression line (Y = ",
        fmt(R$a), " + 0X)."
      )
    )
  }
}
paren <- function(v) ifelse(v < 0, paste0("(", fmt(v), ")"), fmt(v))
num1  <- function(v) if (is.null(v) || length(v) == 0 || is.na(v)) NA_real_ else as.numeric(v)

# Symbols (HTML-safe unicode)
XB <- "X̄"; YB <- "Ȳ"; YH <- "Ŷ"; SIG <- "Σ"
H_DX   <- paste0("X − ", XB)
H_DY   <- paste0("Y − ", YB)
H_DXDY <- paste0("(X − ", XB, ")(Y − ", YB, ")")
H_DX2  <- paste0("(X − ", XB, ")²")
H_PRED <- paste0("Predicted Y (", YH, ")")

html_table <- function(df, total = NULL, cls = "calc") {
  tags$table(
    class = cls,
    tags$thead(tags$tr(lapply(names(df), tags$th))),
    tags$tbody(lapply(seq_len(nrow(df)), function(i)
      tags$tr(lapply(seq_along(df), function(j) tags$td(df[i, j]))))),
    if (!is.null(total)) tags$tfoot(tags$tr(lapply(total, tags$td)))
  )
}

frac <- function(top, bottom)
  tags$span(class = "frac", tags$span(class = "num", top), tags$span(class = "den", bottom))

step_card <- function(num, title, ..., answer = NULL) {
  div(class = "step",
      div(class = "step-head", span(class = "badge-n", num), span(class = "step-title", title)),
      div(class = "step-body", ...),
      if (!is.null(answer)) div(class = "answer", span(class = "answer-label", "Answer"), answer))
}

# -----------------------------------------------------------------------------
# 4. Styling
# -----------------------------------------------------------------------------
app_css <- sprintf("
:root { --bar:%s; --bg:%s; --card:%s; --ink:%s; --head:%s; --muted:%s; --accent:%s; --pred:%s; }
body { background: var(--bg); color: var(--ink); font-family: 'Segoe UI', Helvetica, Arial, sans-serif; }
.container-fluid { max-width: 1040px; padding: 0 16px 48px; }

/* ---------- landing ---------- */
.landing { display: flex; flex-direction: column; align-items: center; text-align: center;
  min-height: calc(100vh - 48px); padding: 8vh 20px 5vh; }
.landing h1 { font-weight: 800; font-size: 56px; margin: 0; }
.landing h2 { font-weight: 400; font-size: 36px; margin: 8px 0 48px; }
.landing .group { font-size: 26px; margin-bottom: 12px; }
.landing .members { font-size: 20px; line-height: 1.8; margin-bottom: 48px; }
.landing .btn-main { margin-top: auto; padding: 14px 44px; font-size: 21px; }

/* ---------- buttons ---------- */
.btn-main { background: var(--bar); color: var(--ink); border: 2px solid var(--ink);
  border-radius: 999px; font-weight: 700; padding: 10px 34px; font-size: 17px; }
.btn-main:hover, .btn-main:focus { background: var(--ink); color: #fff; border-color: var(--ink); }
.btn-soft { background: var(--card); color: var(--ink); border: 2px solid var(--ink);
  border-radius: 999px; font-weight: 600; padding: 6px 20px; }
.btn-soft:hover { background: var(--head); color: var(--ink); border-color: var(--ink); }
.btn-row { text-align: center; margin: 28px 0 8px; }
.btn-row .btn { margin: 4px 8px; }

/* ---------- top bar (tabs) ---------- */
.nav-pills { background: var(--bar); margin: 0 -16px 22px; padding: 10px 16px;
  border: 1px solid var(--ink); }
.nav-pills > li > a { background: var(--bg); color: var(--ink); border: 2px solid var(--ink);
  border-radius: 999px; font-weight: 700; padding: 5px 18px; margin-right: 10px; }
.nav-pills > li.active > a, .nav-pills > li.active > a:hover, .nav-pills > li.active > a:focus
  { background: var(--card); color: var(--ink); }

/* ---------- data entry ---------- */
table.entry { margin: 0 auto; border-collapse: collapse; background: var(--card); }
table.entry th { background: var(--head); border: 1px solid var(--ink); text-align: center; padding: 8px 14px; min-width: 130px; }
table.entry th.idx, table.entry td.idx { min-width: 40px; width: 40px; background: var(--muted); }
table.entry td { border: 1px solid var(--ink); padding: 0; text-align: center; }
table.entry td.idx { font-weight: 600; }
table.entry .form-group { margin: 0; }
table.entry input.form-control { border: 0; border-radius: 0; box-shadow: none; text-align: center; height: 38px; }
table.entry input::-webkit-outer-spin-button, table.entry input::-webkit-inner-spin-button { -webkit-appearance: none; margin: 0; }
table.entry input[type=number] { -moz-appearance: textfield; }
.hint { text-align: center; margin: 0 0 16px; font-size: 14px; }

/* ---------- cards & steps ---------- */
.step { background: var(--card); border: 1px solid var(--ink); border-radius: 10px; margin-bottom: 16px; overflow: hidden; }
.step-head { background: var(--head); padding: 8px 14px; border-bottom: 1px solid var(--ink); font-weight: 700; }
.badge-n { display: inline-block; background: var(--ink); color: #fff; border-radius: 50%%; width: 24px; height: 24px;
  line-height: 24px; text-align: center; font-size: 13px; margin-right: 10px; }
.step-body { padding: 12px 16px; }
.formula { font-size: 18px; margin: 4px 0 8px; }
.subst { font-size: 17px; margin: 4px 0; }
.answer { background: var(--bar); border-top: 1px solid var(--ink); padding: 8px 16px; font-size: 17px; font-weight: 600; }
.answer-label { display: inline-block; background: var(--ink); color: #fff; border-radius: 999px; padding: 1px 10px;
  font-size: 12px; margin-right: 10px; text-transform: uppercase; letter-spacing: .5px; }
.frac { display: inline-block; vertical-align: middle; text-align: center; margin: 0 4px; }
.frac .num { display: block; border-bottom: 1px solid var(--ink); padding: 0 4px; }
.frac .den { display: block; padding: 0 4px; }
.means-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 20px; }
.means-grid .formula { margin-top: 4px; }
.mean-equation { display: grid; grid-template-columns: max-content 1.25em minmax(0, 1fr); align-items: baseline; row-gap: 4px; }
.mean-equation .mean-answer { font-size: 19px; font-weight: 700; }
@media (max-width: 600px) { .means-grid { grid-template-columns: 1fr; } }

table.calc { border-collapse: collapse; margin: 8px 0; min-width: 60%%; background: var(--card); }
table.calc th { background: var(--head); border: 1px solid var(--ink); padding: 5px 12px; text-align: center; }
table.calc td { border: 1px solid var(--ink); padding: 4px 12px; text-align: center; }
table.calc tfoot td { background: var(--muted); font-weight: 700; }
table.calc td:last-child, table.calc th:last-child { font-weight: 700; }
table.sum-table { width: 100%%; }
table.sum-table td:last-child, table.sum-table th:last-child { font-weight: 700; }

/* ---------- summary ---------- */
.stat-grid { display: flex; flex-wrap: wrap; gap: 12px; margin-bottom: 18px; }
.stat { flex: 1 1 150px; background: var(--card); border: 1px solid var(--ink); border-radius: 10px; padding: 10px 14px; text-align: center; }
.stat .k { font-size: 13px; text-transform: uppercase; letter-spacing: .5px; }
.stat .v { font-size: 24px; font-weight: 800; }
.stat.wide { flex: 2 1 280px; background: var(--bar); }
.panel-card { background: var(--card); border: 1px solid var(--ink); border-radius: 10px; padding: 14px; margin-bottom: 18px; }
.panel-card h4 { margin: 0 0 8px; font-weight: 700; }
.interpretation-body { padding: 10px 12px; border: 1px solid var(--ink); border-radius: 8px; }
.interpretation-classification { font-size: 18px; font-weight: 700; }
.interpretation-explanation { margin-top: 6px; }
.table-wrap { overflow-x: auto; }
.plot-tip { position: absolute; z-index: 100; pointer-events: none; white-space: nowrap;
  background: rgba(255,255,255,.97); border: 2px solid var(--ink); border-radius: 8px;
  padding: 7px 11px; font-size: 14px; line-height: 1.5; box-shadow: 0 3px 10px rgba(0,0,0,.25); }
.plot-tip .tip-title { font-weight: 800; margin-bottom: 2px; }
.plot-tip .tip-actual { color: var(--ink); }
.plot-tip .tip-pred { color: var(--pred); }
",
  PAL$bar, PAL$bg, PAL$card, PAL$ink, PAL$head, PAL$muted, PAL$accent, PAL$pred)

# -----------------------------------------------------------------------------
# 5. User interface
# -----------------------------------------------------------------------------
landing_ui <- div(
  class = "landing",
  h1("Linear Regression Simulator"),
  h2("R Programming Language"),
  div(class = "group", "Group 5"),
  div(class = "members", "Cubacub, Klarc", br(), "Gomez, Ken Chie", br(), "Rey, Adrian Louis"),
  actionButton("continue", "Continue", class = "btn-main")
)

app_ui <- tabsetPanel(
  id = "tabs", type = "pills",

  # ---- Page 1: Data Entry (the ONLY manual input) ---------------------------
  tabPanel(
    "Data Entry", value = "entry",
    p(class = "hint", "Type your X and Y values. Everything else is calculated for you."),
    uiOutput("entry_table"),
    div(class = "btn-row",
        actionButton("add_row", "Add Row", class = "btn-soft"),
        actionButton("remove_row", "Remove Last Row", class = "btn-soft"),
        actionButton("load_sample", "Load Class Sample", class = "btn-soft")),
    div(class = "btn-row", actionButton("calculate", "Calculate", class = "btn-main"))
  ),

  # ---- Page 2: Solution (step by step) ---------------------------------------
  tabPanel(
    "Solution", value = "solution",
    uiOutput("solution_ui"),
    div(class = "btn-row", actionButton("to_summary", "View Summary", class = "btn-main"))
  ),

  # ---- Page 3: Summary (compiled results + graphs) ---------------------------
  tabPanel(
    "Summary", value = "summary",
    uiOutput("summary_ui"),
    div(class = "panel-card",
        h4("Predicted Y Graph"),
        p(class = "hint", style = "text-align:left;margin:0 0 6px;",
          "Hover over any point to see its values."),
        div(style = "position:relative;",
            plotOutput("fit_plot", height = "700px",
                       hover = hoverOpts("fit_hover", delay = 40, delayType = "throttle",
                                         nullOutside = TRUE)),
            uiOutput("fit_tip"))),
    div(class = "panel-card", uiOutput("fit_interpretation")),
    div(class = "btn-row",
        actionButton("edit_rows", "Edit X/Y Rows", class = "btn-soft"),
        actionButton("new_entry", "Enter New Entry", class = "btn-soft"))
  )
)

ui <- fluidPage(
  tags$head(tags$title("Linear Regression Simulator"), tags$style(HTML(app_css))),
  conditionalPanel("!output.started", landing_ui),
  conditionalPanel("output.started", app_ui)
)

# -----------------------------------------------------------------------------
# 6. Server
# -----------------------------------------------------------------------------
sample_data <- data.frame(x = c(1, 2, 3, 4, 5), y = c(2, 4, 5, 4, 6))
blank_rows  <- function(n = 4) data.frame(x = rep(NA_real_, n), y = rep(NA_real_, n))

server <- function(input, output, session) {

  started <- reactiveVal(FALSE)
  rows    <- reactiveVal(blank_rows(4))   # what the entry table is built from
  result  <- reactiveVal(NULL)            # NULL until Calculate succeeds

  output$started <- reactive(started())
  outputOptions(output, "started", suspendWhenHidden = FALSE)

  show_results <- function(show) {
    if (show) { showTab("tabs", "solution"); showTab("tabs", "summary") }
    else      { hideTab("tabs", "solution"); hideTab("tabs", "summary") }
  }
  observeEvent(TRUE, show_results(FALSE), once = TRUE)   # Solution/Summary start hidden

  observeEvent(input$continue, started(TRUE))

  # ---- entry table -------------------------------------------------------------
  output$entry_table <- renderUI({
    df <- rows()
    tags$table(
      class = "entry",
      tags$thead(tags$tr(tags$th(class = "idx", "#"), tags$th("X"), tags$th("Y"))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i)
        tags$tr(
          tags$td(class = "idx", i),
          tags$td(numericInput(paste0("x_", i), NULL, value = df$x[i], width = "100%")),
          tags$td(numericInput(paste0("y_", i), NULL, value = df$y[i], width = "100%"))
        )))
    )
  })

  read_inputs <- function() {
    n <- nrow(rows())
    data.frame(
      x = vapply(seq_len(n), function(i) num1(input[[paste0("x_", i)]]), numeric(1)),
      y = vapply(seq_len(n), function(i) num1(input[[paste0("y_", i)]]), numeric(1))
    )
  }

  observeEvent(input$add_row, {
    rows(rbind(read_inputs(), data.frame(x = NA_real_, y = NA_real_)))
  })
  observeEvent(input$remove_row, {
    d <- read_inputs()
    if (nrow(d) <= 2) {
      showNotification("At least 2 rows are needed.", type = "warning")
    } else rows(d[-nrow(d), , drop = FALSE])
  })
  observeEvent(input$load_sample, rows(sample_data))

  # If the user changes any X/Y value after calculating, the old Solution/Summary
  # would be out of date, so those pages close and Calculate must be pressed again.
  current_values <- reactive(read_inputs())
  observeEvent(current_values(), {
    if (!is.null(result())) {
      result(NULL)
      show_results(FALSE)
    }
  }, ignoreInit = TRUE)

  # ---- Calculate -----------------------------------------------------------------
  observeEvent(input$calculate, {
    d <- read_inputs()
    d <- d[!(is.na(d$x) & is.na(d$y)), , drop = FALSE]     # ignore completely blank rows
    msg <- check_data(d)
    if (!is.null(msg)) {
      showNotification(msg, type = "error", duration = 7)
      return()
    }
    result(calc_regression(d$x, d$y))
    show_results(TRUE)
    updateTabsetPanel(session, "tabs", selected = "summary")
  })

  observeEvent(input$to_summary, updateTabsetPanel(session, "tabs", selected = "summary"))
  observeEvent(input$edit_rows,  updateTabsetPanel(session, "tabs", selected = "entry"))

  # ---- Enter new entry (wipe everything) ---------------------------------------------
  observeEvent(input$new_entry, {
    showModal(modalDialog(
      title = "Start a new entry?",
      "This clears all X/Y values and closes the Solution and Summary pages.",
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_new", "Clear everything", class = "btn-main")),
      easyClose = TRUE))
  })
  observeEvent(input$confirm_new, {
    removeModal()
    result(NULL)
    show_results(FALSE)
    rows(blank_rows(4))
    updateTabsetPanel(session, "tabs", selected = "entry")
  })

  # ---- Solution page ---------------------------------------------------------------------
  output$solution_ui <- renderUI({
    R <- req(result())
    n <- R$n
    xs <- paste(fmt(R$x), collapse = " + ")
    ys <- paste(fmt(R$y), collapse = " + ")

    tagList(
      step_card(1, "Compute the means",
        div(class = "means-grid",
          div(class = "mean-equation",
            span(XB), span("="), span(class = "formula", paste0(SIG, "X / n")),
            span(XB), span("="), span(class = "subst", paste0("(", xs, ") / ", n)),
            span(), span("="), span(class = "subst", paste0(fmt(R$Sx), " / ", n)),
            span(), span("="), span(class = "subst mean-answer", fmt(R$xbar))
          ),
          div(class = "mean-equation",
            span(YB), span("="), span(class = "formula", paste0(SIG, "Y / n")),
            span(YB), span("="), span(class = "subst", paste0("(", ys, ") / ", n)),
            span(), span("="), span(class = "subst", paste0(fmt(R$Sy), " / ", n)),
            span(), span("="), span(class = "subst mean-answer", fmt(R$ybar))
          )
        )),

      step_card(2, H_DX,
        html_table(setNames(data.frame(fmt(R$x), paste0(fmt(R$x), " − ", paren(R$xbar)), fmt(R$dx),
                                       stringsAsFactors = FALSE),
                            c("X", "Substitution", H_DX)),
                   total = c("", SIG, fmt(sum(R$dx)))),
        answer = paste0(H_DX, " = ", paste(fmt(R$dx), collapse = ",  "))),

      step_card(3, H_DY,
        html_table(setNames(data.frame(fmt(R$y), paste0(fmt(R$y), " − ", paren(R$ybar)), fmt(R$dy),
                                       stringsAsFactors = FALSE),
                            c("Y", "Substitution", H_DY)),
                   total = c("", SIG, fmt(sum(R$dy)))),
        answer = paste0(H_DY, " = ", paste(fmt(R$dy), collapse = ",  "))),

      step_card(4, H_DXDY,
        html_table(setNames(data.frame(paste0("(", fmt(R$dx), ") (", fmt(R$dy), ")"), fmt(R$dxdy),
                                       stringsAsFactors = FALSE),
                            c("Substitution", H_DXDY)),
                   total = c(paste0(SIG, H_DXDY), fmt(R$Sxy))),
        answer = paste0(SIG, H_DXDY, " = ", fmt(R$Sxy))),

      step_card(5, H_DX2,
        html_table(setNames(data.frame(paste0(paren(R$dx), "²"), fmt(R$dx2),
                                       stringsAsFactors = FALSE),
                            c("Substitution", H_DX2)),
                   total = c(paste0(SIG, H_DX2), fmt(R$Sxx))),
        answer = paste0(SIG, H_DX2, " = ", fmt(R$Sxx))),

      step_card(6, "Slope (b)",
        div(class = "mean-equation",
          span("b"), span("="), span(class = "formula", frac(paste0(SIG, H_DXDY), paste0(SIG, H_DX2))),
          span("b"), span("="), span(class = "subst", frac(fmt(R$Sxy), fmt(R$Sxx))),
          span(), span("="), span(class = "subst mean-answer", fmt(R$b))
        )),

      step_card(7, "Intercept (a)",
        div(class = "mean-equation",
          span("a"), span("="), span(class = "formula", paste0(YB, " − b", XB)),
          span(), span("="), span(class = "subst", paste0(fmt(R$ybar), " − ", paren(R$b), "(", fmt(R$xbar), ")")),
          span(), span("="), span(class = "subst", paste0(fmt(R$ybar), " − ", paren(R$b * R$xbar))),
          span(), span("="), span(class = "subst mean-answer", fmt(R$a))
        )),

      step_card(8, "Regression equation",
        div(class = "mean-equation",
          span(YH), span("="), span(class = "formula", "a + bX"),
          span(YH), span("="), span(class = "subst mean-answer",
                                    paste0(fmt(R$a), if (R$b < 0) " − " else " + ", fmt(abs(R$b)), "X"))
        )),

      step_card(9, H_PRED,
        div(class = "formula", paste0(YH, " = a + bX")),
        div(class = "subst", paste0("a = ", fmt(R$a), ", b = ", fmt(R$b))),
        html_table(setNames(data.frame(fmt(R$x),
                                       paste0(fmt(R$a), " + ", paren(R$b), "(", fmt(R$x), ")"), fmt(R$yhat),
                                       stringsAsFactors = FALSE),
                            c("X", "Substitution", H_PRED))),
        answer = paste0(YH, " = ", paste(fmt(R$yhat), collapse = ",  ")))
    )
  })

  # ---- Summary page ------------------------------------------------------------------------
  output$summary_ui <- renderUI({
    R <- req(result())
    stat <- function(k, v, cls = "") div(class = paste("stat", cls), div(class = "k", k), div(class = "v", v))
    eq <- paste0(YH, " = ", fmt(R$a), if (R$b < 0) " − " else " + ", fmt(abs(R$b)), "X")

    tbl <- data.frame(fmt(R$x), fmt(R$y), fmt(R$dx), fmt(R$dy), fmt(R$dxdy), fmt(R$dx2),
                      fmt(R$yhat), stringsAsFactors = FALSE)
    names(tbl) <- c("X", "Y", H_DX, H_DY, H_DXDY, H_DX2, H_PRED)
    totals <- c(fmt(R$Sx), fmt(R$Sy), fmt(sum(R$dx)), fmt(sum(R$dy)), fmt(R$Sxy), fmt(R$Sxx),
                fmt(sum(R$yhat)))
    total_row <- paste0(SIG, " = ", totals)

    tagList(
      div(class = "stat-grid",
          stat("Regression equation", eq, "wide"),
          stat("Intercept (a)", fmt(R$a)),
          stat("Slope (b)", fmt(R$b)),
          stat("Observations (n)", R$n)),
      div(class = "panel-card",
          h4("Completed table"),
          div(class = "table-wrap", html_table(tbl, total = total_row,
                                               cls = "calc sum-table")))
    )
  })

  # ---- Graphs -----------------------------------------------------------------------------------
  plot_theme <- function() {
    theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
            panel.grid.minor = element_blank())
  }

  # Every hoverable point (actual and predicted) in one table
  fit_points <- reactive({
    R <- req(result())
    n <- R$n
    data.frame(
      row  = rep(seq_len(n), 2),
      x    = rep(R$x, 2),
      y    = c(R$y, R$yhat),
      type = rep(c("Actual Y", "Predicted Y"), each = n),
      stringsAsFactors = FALSE
    )
  })

  output$fit_tip <- renderUI({
    h <- input$fit_hover
    if (is.null(h)) return(NULL)
    R   <- req(result())
    hit <- nearPoints(fit_points(), h, xvar = "x", yvar = "y", threshold = 14, maxpoints = 1)
    if (nrow(hit) == 0) return(NULL)

    i <- hit$row
    flip <- h$x > (h$domain$left + h$domain$right) / 2          # keep tip inside the plot
    pos <- sprintf("top:%spx; %s", round(h$coords_css$y - 12),
                   if (flip) sprintf("left:%spx; transform:translateX(-100%%);", round(h$coords_css$x - 14))
                   else      sprintf("left:%spx;", round(h$coords_css$x + 14)))

    if (hit$type == "Actual Y") {
      body <- tagList(
        div(class = "tip-title tip-actual", paste0("● Actual Y  (row ", i, ")")),
        div(paste0("X = ", fmt(R$x[i]))),
        div(paste0("Y = ", fmt(R$y[i]))),
        div(paste0("Predicted ", YH, " = ", fmt(R$yhat[i])))
      )
    } else {
      body <- tagList(
        div(class = "tip-title tip-pred", paste0("▲ Predicted Y  (row ", i, ")")),
        div(paste0("X = ", fmt(R$x[i]))),
        div(paste0(YH, " = ", fmt(R$a), " + ", paren(R$b), "(", fmt(R$x[i]), ") = ", fmt(R$yhat[i]))),
        div(paste0("Actual Y = ", fmt(R$y[i])))
      )
    }
    div(class = "plot-tip", style = pos, body)
  })

  output$fit_plot <- renderPlot({
    R <- req(result())
    df <- data.frame(x = R$x, y = R$y, yhat = R$yhat)

    # Axis limits: always include 0, add padding so no point sits on the border,
    # and snap to whole tick marks so the plane is a clean square grid.
    nice_axis <- function(v) {
      rng <- range(c(0, v))
      pad <- 0.08 * diff(rng)
      lo  <- if (min(v) < 0) rng[1] - pad else 0   # start at 0 unless data go negative
      br  <- pretty(c(lo, rng[2] + pad), n = 6)
      list(lim = range(br), breaks = br)
    }
    ax <- nice_axis(R$x)
    ay <- nice_axis(c(R$y, R$yhat))
    line_df <- data.frame(a = R$a, b = R$b)

    ggplot(df, aes(x = x)) +
      geom_segment(aes(y = y, xend = x, yend = yhat), colour = "grey55", linetype = "dashed") +
      geom_abline(data = line_df, aes(intercept = a, slope = b, linetype = "Regression line"),
                  colour = PAL$accent, linewidth = 1.2) +
      geom_point(aes(y = y, colour = "Actual Y", shape = "Actual Y"), size = 4.5) +
      geom_point(aes(y = yhat, colour = "Predicted Y", shape = "Predicted Y"), size = 4.5) +
      scale_colour_manual(values = c("Actual Y" = PAL$ink, "Predicted Y" = PAL$pred), name = NULL) +
      scale_shape_manual(values = c("Actual Y" = 16, "Predicted Y" = 17), name = NULL) +
      scale_linetype_manual(values = c("Regression line" = "solid"), name = NULL) +
      scale_x_continuous(breaks = ax$breaks) +
      scale_y_continuous(breaks = ay$breaks) +
      coord_cartesian(xlim = ax$lim, ylim = ay$lim, expand = FALSE) +
      labs(title = "Scatter Plot with Regression Line",
           subtitle = sprintf("Y = %s %s %sX", fmt(R$a),
                              if (R$b < 0) "-" else "+", fmt(abs(R$b))),
           x = "X", y = "Y") +
      plot_theme() +
      theme(aspect.ratio = 1,                                           # square plane
            panel.background = element_rect(fill = "white", colour = NA),
            panel.border     = element_rect(colour = PAL$ink, fill = NA, linewidth = 1.2),
            panel.grid.major = element_line(colour = "grey80", linewidth = 0.5),
            panel.grid.minor = element_line(colour = "grey92", linewidth = 0.3),
            axis.ticks       = element_line(colour = PAL$ink),
            axis.ticks.length = unit(5, "pt"),
            axis.text        = element_text(colour = PAL$ink, size = 13),
            axis.title       = element_text(face = "bold", size = 14))
  })

  output$fit_interpretation <- renderUI({
    R <- req(result())
    interpretation <- graph_interpretation(R)
    tagList(
      h4("Interpretation of the Graph"),
      div(class = "interpretation-body",
        div(class = "interpretation-classification", interpretation$classification)
      ),
      div(class = "interpretation-body interpretation-explanation",
        interpretation$explanation
      )
    )
  })

}

shinyApp(ui, server)