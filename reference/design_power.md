# Calculate the statistical power for an experimental design

Calculate pairwise treatment-comparison power from the Fisher
information of the provided experimental design using the treatment
information matrix induced by an AR1 x AR1 spatial dependence structure.
This function evaluates how much "power" the design has to detect a
specified treatment difference after adjusting for nuisance effects
(e.g. blocks, etc.)

## Usage

``` r
design_power(
  design,
  treatment_cols = "treatment",
  row_col = "row",
  column_col = "col",
  block_col = NULL,
  rho_row = 0.1,
  rho_col = 0.1,
  sigma2 = 1,
  delta = 1,
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

- sigma2:

  `numeric`. Assumed residual variance.

- delta:

  `numeric`. Treatment difference that is "worth detecting". The minimum
  difference between treatments that we care about.

- alpha:

  `numeric`. Significance/p-value threshold for the two-sided z-tests.

- tolerance:

  `numeric`. Tolerance for numerical stability.

## Value

A list containing:

- contrast_power:

  Data frame of pairwise treatment-comparison standard errors,
  noncentrality parameters, power values, and effective replicates.

- design_power:

  The minimum pairwise power across all estimable treatment comparisons.

- average_power:

  The average pairwise power across all estimable treatment comparisons.

- worst_comparison:

  The treatment comparison with the lowest power.

- fisher_info:

  The treatment information matrix.

- treatment_cov:

  The model-based covariance matrix of treatment estimates, equal to
  \\\sigma^2 I_T^+\\.

- eigenvalues:

  Positive eigenvalues of the treatment information matrix.

- rank:

  Numerical rank of the treatment information matrix.

- assumptions:

  List of covariance, effect-size, and testing assumptions used.

## Details

The calculation is based on the treatment information matrix \$\$ I_T =
X_1^\intercal L_R X_1, \$\$ where \\X_1\\ is the treatment indicator
matrix and \\L_R\\ is the covariance-adjusted projection matrix that
removes nuisance effects: \$\$ L = \Sigma^{-1} - \Sigma^{-1} X_2 \left(
X_2^\intercal \Sigma^{-1} X_2 \right)^{-1} \Sigma^{-1} \$\$ For a
treatment contrast \\c^\intercal \tau\\, the contrast standard error is
computed from \$\$ \operatorname{SE}(c^\intercal \hat{\tau}) =
\sqrt{\sigma^2 c^\intercal I_T^+ c}, \$\$ where \\I_T^+\\ is the
Moore-Penrose pseudoinverse of the treatment information matrix. The
effect size `delta` is then converted into a noncentrality parameter,
\$\$ \lambda = \frac{\delta}{\operatorname{SE}(c^\intercal \hat{\tau})},
\$\$ and power is calculated using a two-sided known-covariance Z-test
approximation.

The spatial covariance parameters `rho_row`, `rho_col`, and `sigma2` are
treated as assumed planning values, not estimated from the data. The
returned power is therefore conditional on the supplied covariance
structure, the chosen effect size `delta`, and the significance level
`alpha`.

Thus, you as the statistician must select four parameters based on
previous trials, domain knowledge, or *conservative* assumptions:

- \\\rho\_{\text{row}}\\:

  AR1 row dependence parameter. How correlated are observations that are
  one row apart? This is hard to select, I would recommend testing
  multiple different values such as \\0.0\\ for no dependence, \\0.3\\
  for moderate, and \\0.5\\ for strong dependence, and reporting each.

- \\\rho\_{\text{col}}\\:

  AR1 column dependence parameter. How correlated are observations that
  are one column apart? This is hard to select, I would recommend
  testing multiple different values such as \\0.0\\ for no dependence,
  \\0.3\\ for moderate, and \\0.5\\ for strong dependence, and reporting
  each.

- \\\sigma^2\\:

  The residual variance. This should be based on previous similar
  experiments, or a conservative estimate. Underestimating \\\sigma^2\\
  will result in *overstated power*.

- \\\delta\\:

  Detectable treatment difference. Choose \\\delta\\ as the *smallest*
  treatment difference that would matter scientifically, agronomically,
  commercially, etc. So if yield differences of less than \\0.1\\ t/ha
  are not important to the farmer, then use \\\delta = 0.1\\. Otherwise,
  test multiple deltas and report the power of each of them.

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

res_power <- design_power(
    design = latinsquare,
    treatment_cols = "treatment",
    row_col = "row",
    column_col = "col",
    block_col = NULL,
    rho_row = 0.3,
    rho_col = 0.3,
    sigma2 = 1,
    delta = 1,
    alpha = 0.05
)
} # }
```
