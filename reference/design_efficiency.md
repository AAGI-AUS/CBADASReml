# Calculate the design efficiency/optimality of a small-plot trial design

Calculates model-based efficiency for a design using the treatment
information matrix induced by an AR1 x AR1 spatial dependence structure.
All this function does is evaluate how much information the design
contains about treatment contrasts, after adjusting for nuisance effects
(e.g. blocks , etc.)

## Usage

``` r
design_efficiency(
  design,
  treatment_cols = "treatment",
  row_col = "row",
  column_col = "col",
  block_col = NULL,
  rho_row = 0.1,
  rho_col = 0.1,
  alpha = 0.05,
  tolerance = 0.0000000001
)
```

## Arguments

- design:

  `data.frame`. The design data frame, one row per experimental unit.

- treatment_cols:

  `character` vector. Column name of the treatment in `design`. If
  multiple columns are provided, each unique combination is treated as a
  distinct treatment combination.

- row_col:

  `character` vector. Column name of the experiment's rows in `design`.

- column_col:

  `character` vector. Column name of the experiment's columns in
  `design`.

- block_col:

  `character` vector or `NULL`. Column name of the experiment's blocking
  factor in `design`. Use `NULL` for unblocked designs.

- rho_row:

  `numeric`. AR1 correlation parameter in the row direction.

- rho_col:

  `numeric`. AR1 correlation parameter in the column direction.

- alpha:

  significance level `numeric`.

- tolerance:

  `numeric`. Tolerance for numerical stability.

## Value

A list containing:

- fisher_info:

  The treatment information matrix, \\I_T\\.

- eigenvalues:

  Positive eigenvalues of the treatment information matrix.

- rank:

  Numerical rank of the treatment information matrix.

- estimable_design:

  Whether all treatment contrasts are estimable. For \\v\\ treatment
  levels, this requires rank at least \\v - 1\\.

- a_optimality:

  A-optimality score, calculated as the sum of inverse positive
  eigenvalues. Smaller values indicate better average treatment contrast
  precision.

- d_optimality:

  D-optimality score, calculated as the negative sum of log positive
  eigenvalues. Smaller values indicate greater overall information
  volume.

- pairwise_efficiency:

  Data frame containing pairwise treatment comparisons, estimability
  indicators, contrast variances, and iid-equivalent effective
  replicates.

- assumptions:

  List of spatial correlation and design assumptions used in the
  calculation.

A list with a lot of info in it.

## Details

The function computes the treatment information matrix \$\$ I_T =
X_1^\top L_R X_1, \$\$ where \\X_1\\ is the treatment indicator matrix
and \\L_R\\ is the covariance-adjusted projection matrix that removes
nuisance effects: \$\$ L = \Sigma^{-1} - \Sigma^{-1} X_2 \left(
X_2^\intercal \Sigma^{-1} X_2 \right)^{-1} \Sigma^{-1} \$\$ Pairwise
treatment contrast variances are then calculated from \\I_T^+\\, the
Moore-Penrose pseudoinverse of the treatment information matrix.

## Examples

``` r
if (FALSE) { # \dontrun{
# RCBD
df <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    treatment = rep(LETTERS[1:8], 3),
    block = rep(1:3, each = 8)
)

# Optimise while respecting blocks
result <- speed::speed(df,
    "treatment",
    swap_within = "block",
    iterations = 5000,
    seed = 42
)

## test on latin square
latinsquare <- data.frame(
    row = rep(1:4, each = 4),
    col = rep(1:4, times = 4),
    treatment = c(
        "A", "B", "C", "D",
        "B", "C", "D", "A",
        "C", "D", "A", "B",
        "D", "A", "B", "C"
    )
)

res_eff <- design_efficiency(
    design = latinsquare,
    treatment_cols = "treatment",
    row_col = "row",
    column_col = "col",
    block_col = NULL,
    rho_row = 0.3,
    rho_col = 0.3
)
} # }
```
