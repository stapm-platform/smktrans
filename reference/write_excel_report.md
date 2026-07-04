# Write Transition Probability Estimates to Excel

Generates a multi-sheet Excel workbook with a professional cover sheet
including detailed methodological metadata for reproducibility.

## Usage

``` r
write_excel_report(
  config,
  init_res,
  quit_res,
  relapse_res,
  net_init_dt = NULL,
  quit_no_init = NULL
)
```

## Arguments

- config:

  Configuration list containing metadata and model parameters.

- init_res:

  Initiation results list/dataframe.

- quit_res:

  Quitting results list/dataframe.

- relapse_res:

  Relapse results list/dataframe.

- net_init_dt:

  Optional net initiation table (can be NULL).

- quit_no_init:

  Optional quitting without initiation table (can be NULL).
