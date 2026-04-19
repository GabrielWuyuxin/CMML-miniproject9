# CMML miniproject9: Simulating speed-accuracy trade-off (SAT) by parameter tuning
This repository contains R scripts for simulating a cognitive decision‑making model (evidence accumulation with schema‑based and generic confidence), batch‑processing simulation outputs, and generating statistical comparisons with control data.

## Project Structure

| File | Description |
|------|-------------|
| `model_question1_modified2.R` | Main simulation script. Implements the full cognitive model (exploration, attention shifts, threshold‑based decisions). Supports parameter sweeps and saves subject‑level performance. |
| `batch_visualization.R` | Generates boxplots comparing multiple experimental conditions against a control, with significance annotations (t‑tests + FDR correction). |
| `data_visualization.R` | Processes individual CSV files from the simulation, creates boxplots, and exports t‑test summaries. |
| `statistical_test.r` | Batch t‑tests and FDR‑adjusted multiple comparisons across all parameter combinations. Outputs formatted tables. |

## Requirements

- R (≥ 4.0)
- Required packages: `ggplot2`, `dplyr`, `tidyr`, `scales`, `gridExtra`

Install missing packages with:
```r
install.packages(c("ggplot2", "dplyr", "tidyr", "scales", "gridExtra"))
