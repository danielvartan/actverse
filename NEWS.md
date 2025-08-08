# actverse Development version

- `sri()` now `min_data` as an argument, which specifies the minimum proportion of non-missing values required to compute the SRI for each time point. If the proportion of non-missing values is below this threshold, the SRI will be set to `NA` for that time point. This helps to avoid computing the SRI when there is insufficient data.
- `sri()` now returns a `valid_data` column, which indicates the proportion of non-missing values in the `agreement` column for each time point. This provides additional information about the data quality used to compute the SRI.
- `sri()` now has different default values for `sleeping_states` and `awake_states`. The new defaults are `sleeping_states = c(1, 2)` and `awake_states = 0`. The previous defaults were `sleeping_states = 1` and `awake_states = c(0, 2)`, which considered *resting* states as awake, differing from the way that the index was originally computed.

# actverse 0.1.0

- First release. 🎉

# actverse 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
