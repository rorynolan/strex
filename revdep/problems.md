# filesstrings

<details>

* Version: 3.1.3
* Source code: https://github.com/cran/filesstrings
* URL: https://www.github.com/rorynolan/filesstrings
* BugReports: https://www.github.com/rorynolan/filesstrings/issues
* Date/Publication: 2019-06-06 18:10:03 UTC
* Number of recursive dependencies: 55

Run `revdep_details(,"filesstrings")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: stringr
      > 
      > test_check("filesstrings")
      [31m──[39m [31m1. Failure: trim_anything works (@test_strings.R#254) [39m [31m──────────────────────────────────────────────────[39m
      trim_anything("..abcd.", ".", "left") not equal to "abcd.".
      1/1 mismatches
      x[1]: ""
      y[1]: "abcd."
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════
      OK: 173 SKIPPED: 0 WARNINGS: 0 FAILED: 1
      1. Failure: trim_anything works (@test_strings.R#254) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

