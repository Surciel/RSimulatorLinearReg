# Linear Regression Simulator

**R Programming Language | Group 5**
Cubacub, Klarc · Gomez, Ken Chie · Rey, Adrian Louis

This project is about the R Simulator; Linear Regression to be precise, refer to Group Chat for contents.

---

## Project Requirements

- Based on the R programming language; the main system is done in R itself, supported by packages for the UI (CSS/HTML additions are allowed so long as the main system is made out of R).
- Application based.
- The list of columns (add rows below) is the only thing the user changes or adds; it is the only part with manual user input.
  - Calculations for the rest are automated by the system.
- Output includes both numeric results and graphs.
- Solutions are also visible.

## Features

- **Simple linear regression** (one independent variable) using the class method: `Y = a + bX`
- **Data Entry page:** the user types only the X and Y values. Rows can be added or removed, and a class sample dataset can be loaded.
- **Solution page:** a step-by-step solution showing every formula, substitution and final answer, labeled by the column it belongs to:
  - Means (X̄, Ȳ)
  - X − X̄, Y − Ȳ, (X − X̄)(Y − Ȳ), (X − X̄)²
  - Slope `b = Σ(X − X̄)(Y − Ȳ) / Σ(X − X̄)²`
  - Intercept `a = Ȳ − bX̄`
  - Regression equation and Predicted Y
- **Summary page:** equation, a, b, number of observations, the completed table with totals, and the Predicted Y graph (scatter plot with regression line).
- **Interactive graph:** hover over any Actual Y or Predicted Y point to see its row, X, Y, predicted value, the substitution `a + b(X)` and the residual.
- Changing any X/Y value after calculating closes the Solution and Summary pages, so results are never out of date. Press **Calculate** again to refresh them.

## Requirements to Run

- R 4.0 or newer (RStudio recommended, but optional)
- R packages: `shiny` and `ggplot2`

Install the packages once:

```r
install.packages(c("shiny", "ggplot2"), repos = "https://cloud.r-project.org")
```

## How to Run

1. Put `app.R` in its own folder (the file must be named exactly `app.R`).
2. In RStudio, open `app.R` and click **Run App**, or run in the R console:

```r
shiny::runApp("path/to/linear_regression_simulator")
```

If symbols such as X̄, Ŷ or Σ look garbled, reopen the file as UTF-8 (RStudio: *File → Reopen with Encoding → UTF-8*).

## How to Use

1. Click **Continue** on the title screen.
2. On **Data Entry**, type the X and Y values (use **Add Row** for more) and click **Calculate**.
3. Read the working on **Solution**, then click **View Summary**.
4. On **Summary**, scroll down for the graphs. Use **Edit X/Y Rows** to go back and change values, or **Enter New Entry** to clear everything and start over.

## Sample Data (from class)

| X | Y |
|---|---|
| 1 | 2 |
| 2 | 4 |
| 3 | 5 |
| 4 | 4 |
| 5 | 6 |

Expected results: X̄ = 3, Ȳ = 4.2, b = 0.8, a = 1.8, so **Ŷ = 1.8 + 0.8X** (R² ≈ 0.7273).

## Files

| File | Description |
|---|---|
| `app.R` | The complete Shiny application (calculations, UI and graphs) |
| `archived_summary_elements.R` | Archived Summary metrics, residual plot, and strength-of-fit solution step; not loaded by the app |
| `README.txt` | This file |