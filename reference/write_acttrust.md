# Write a `tsibble` to a readable ActTrust file

`write_acttrust()` allows you to adapt and write a
[`tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html) object
to a readable ActTrust file.

Note that your data must conform to a predefined structure. See
[`?acttrust`](https://danielvartan.github.io/actverse/reference/acttrust.md)
to learn more.

ActTrust is a trademark of [Condor Instruments
Ltda](https://condorinst.com/en/).

## Usage

``` r
write_acttrust(data, file, delim = ";", header = NULL)
```

## Arguments

- data:

  A [`tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html)
  object with a [`POSIXt`](https://rdrr.io/r/base/as.POSIXlt.html)
  index. The ActTrust software only deals with this type of index.

- file:

  A string with a file path to write to.

- delim:

  (optional) A string indicating the delimiter that must be used to
  separate values. Valid delimiters are: `";"` and `"\t"` (default:
  `";"`).

- header:

  (optional) A string indicating the path to a file (usually the raw
  data file) with the ActTrust header. This is not mandatory, the
  ActTrust software can read files without it (default: `NULL`).

## Value

An invisible `NULL`. This function is used for its side effect.

## Details

### `NA` values

`write_acttrust()` will transform any `NA` value to `0`. This is because
the ActTrust software only deals with numeric values.

## See also

Other read/write functions:
[`read_acttrust()`](https://danielvartan.github.io/actverse/reference/read_acttrust.md)

## Examples

``` r
library(readr)
#> 
#> Attaching package: ‘readr’
#> The following object is masked from ‘package:scales’:
#> 
#>     col_factor
#> The following object is masked from ‘package:curl’:
#> 
#>     parse_date

acttrust
#> # A tsibble: 1,441 x 17 [1m] <America/Sao_Paulo>
#>    timestamp             pim   tat   zcm orientation wrist_temperature
#>    <dttm>              <dbl> <dbl> <dbl>       <dbl>             <dbl>
#>  1 2021-04-24 04:14:00  7815   608   228           0              26.9
#>  2 2021-04-24 04:15:00  2661   160    64           0              27.2
#>  3 2021-04-24 04:16:00  3402   243    80           0              27.7
#>  4 2021-04-24 04:17:00  4580   317   125           0              27.9
#>  5 2021-04-24 04:18:00  2624   255    33           0              28.0
#>  6 2021-04-24 04:19:00  3929   246   105           0              28.1
#>  7 2021-04-24 04:20:00  5812   369   171           0              28.2
#>  8 2021-04-24 04:21:00  3182   270    54           0              28.4
#>  9 2021-04-24 04:22:00  6362   373   189           0              28.6
#> 10 2021-04-24 04:23:00  2621   159    64           0              28.7
#> # ℹ 1,431 more rows
#> # ℹ 11 more variables: external_temperature <dbl>, light <dbl>,
#> #   ambient_light <dbl>, red_light <dbl>, green_light <dbl>, blue_light <dbl>,
#> #   ir_light <dbl>, uva_light <dbl>, uvb_light <dbl>, event <dbl>, state <dbl>

file <- tempfile()

write_acttrust(
  data = acttrust,
  file = file,
  header = get_raw_data("acttrust.txt")
)
#> ℹ Adapting data
#> ✔ Adapting data [34ms]
#> 
#> ℹ Adding header
#> ✔ Adding header [19ms]
#> 
#> ℹ Writing data
#> ✔ Writing data [47ms]
#> 

readr::read_lines(file, n_max = 30)
#>  [1] "+-------------+ Condor Instruments Report +-------------+"                                                                                                                
#>  [2] "SOFTWARE_VERSION : 1.0.17"                                                                                                                                                
#>  [3] "SUBJECT_NAME : "                                                                                                                                                          
#>  [4] "SUBJECT_GENDER : Male"                                                                                                                                                    
#>  [5] "SUBJECT_DATE_OF_BIRTH : 20/09/1985"                                                                                                                                       
#>  [6] "SUBJECT_DESCRIPTION : "                                                                                                                                                   
#>  [7] "DEVICE_ID : 323"                                                                                                                                                          
#>  [8] "DEVICE_MODEL : ActTrust1"                                                                                                                                                 
#>  [9] "HARDWARE_VERSION : 3.1"                                                                                                                                                   
#> [10] "FIRMWARE_VERSION : 3.7"                                                                                                                                                   
#> [11] "MEMORY_SIZE : 3987712"                                                                                                                                                    
#> [12] "LOG_SIZE : 2296573"                                                                                                                                                       
#> [13] "MEMORY_USAGE : 57.59 %"                                                                                                                                                   
#> [14] "BATTERY_VOLTAGE : 4.21875"                                                                                                                                                
#> [15] "ERROR_FLAG : 0"                                                                                                                                                           
#> [16] "ERROR_CODE : 0"                                                                                                                                                           
#> [17] "POWER_DOWN_FLAG : 1"                                                                                                                                                      
#> [18] "TAT_THRESHOLD : 1024"                                                                                                                                                     
#> [19] "ORIENTATION : 0"                                                                                                                                                          
#> [20] "MODE : PIM/TAT/ZCM"                                                                                                                                                       
#> [21] "INTERVAL : 60"                                                                                                                                                            
#> [22] "DATA_CORRECTION : 1"                                                                                                                                                      
#> [23] "DATA_CORRECTION_DESCRIPTION : Lacuna tratada nos dados."                                                                                                                  
#> [24] "DATE_TIME : 07/06/2021 07:00:00"                                                                                                                                          
#> [25] "+-------------------------------------------------------+"                                                                                                                
#> [26] "DATE/TIME;MS;EVENT;TEMPERATURE;EXT TEMPERATURE;ORIENTATION;PIM;PIMn;TAT;TATn;ZCM;ZCMn;LIGHT;AMB LIGHT;RED LIGHT;GREEN LIGHT;BLUE LIGHT;IR LIGHT;UVA LIGHT;UVB LIGHT;STATE"
#> [27] "24/04/2021 04:14:00;0;1;26.87;24.62;0;7815;130.25;608;10.133333333333333;228;3.8;3.58;1.45;0.57;0.66;0.24;0.17;0;0;0"                                                     
#> [28] "24/04/2021 04:15:00;0;0;27.18;25.06;0;2661;44.35;160;2.6666666666666665;64;1.0666666666666667;5.23;2.12;0.86;0.95;0.36;0.25;0;0;0"                                        
#> [29] "24/04/2021 04:16:00;0;0;27.68;25.5;0;3402;56.7;243;4.05;80;1.3333333333333333;3.93;1.59;0.64;0.71;0.26;0.2;0;0;0"                                                         
#> [30] "24/04/2021 04:17:00;0;0;27.86;25.75;0;4580;76.33333333333333;317;5.283333333333333;125;2.0833333333333335;4.14;1.68;0.67;0.75;0.28;0.2;0;0;0"                             
```
