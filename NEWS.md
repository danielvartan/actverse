# actverse Development version

- `sri()` now `min_data` as an argument, which specifies the minimum proportion of non-missing values required to compute the SRI for each time point. If the proportion of non-missing values is below this threshold, the SRI will be set to `NA` for that time point. This helps to avoid computing the SRI when there is insufficient data.
- `sri()` now returns a `valid_data` column, which indicates the proportion of non-missing values in the `agreement` column for each time point. This provides additional information about the data quality used to compute the SRI.

# actverse 0.1.0

- First release. 🎉

# actverse 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
