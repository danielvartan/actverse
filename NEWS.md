# v0.1.0.9000 (development version)

- `read_acttrust()` has been updated to handle special characters in the input files more robustly. It now reads the file content, removes any special characters, and then writes it to a temporary file before processing. This ensures that the function can handle files with special characters without errors.
- `sri()` now has `min_data` as an argument, which specifies the minimum proportion of non-missing values required to compute the SRI for each time point. If the proportion of non-missing values is below this threshold, the SRI will be set to `NA` for that time point. This helps to avoid computing the SRI when there is insufficient data. Note that the first agreement value is always `NA` because there is no previous day for comparison, hence it is not treated as missing data.
- `sri()` now returns a `valid_data` column, which indicates the proportion of non-missing values in the `agreement` column for each time point. This provides additional information about the data quality used to compute the SRI.
- `sri()` now has different default values for `sleeping_states` and `awake_states`. The new defaults are `sleeping_states = c(1, 2)` and `awake_states = 0`. The previous defaults were `sleeping_states = 1` and `awake_states = c(0, 2)`, which considered *resting* states as awake, differing from the way that the index was originally computed.
- `state_prop()` now has a default value of `c(1, 2)` for `state_values`, which considers both *sleeping* and *resting* states (for Condor Instruments actigraphy data) as the target states for calculating the proportion. The previous default was `1`, which only considered the *sleeping* state.

# v0.1.0

- First release. 🎉

# v0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
