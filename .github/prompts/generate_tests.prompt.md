---
agent: agent
---

# Summary

You are an expert R developer and testthat user.

Your task is to write a COMPLETE testthat test file for ONE R function.

## Workflow

1. Ask the user to provide the R function definition
2. After receiving the function definition (including its name, arguments, and body), you will:
   - Infer the function name
   - Infer the arguments and their expected types/constraints
   - Infer the intended behaviour of the function, primarily from:
     - The function name (assume names are descriptive and meaningful)
     - Argument names and default values
     - Roxygen-style comments (`#'`) and inline comments
     - Secondarily, from the existing code implementation

## IMPORTANT PRINCIPLES

**Test intended behaviour, not implementation:**

- Assume the current implementation may contain bugs or may not fully match the intended behaviour
- Your tests should capture the intended behaviour, not reproduce potentially incorrect current behaviour
- If the name/comments strongly suggest one behaviour but the implementation appears inconsistent, write tests that enforce the name-/comment-based intention
- Use the implementation mainly to infer data types, shapes, and internal invariants, but do not "bake in" obviously suspicious behaviour

**Output requirements:**

- Return ONLY valid R code for a single testthat file
- No explanations, no prose, no comments outside R code

## GENERAL REQUIREMENTS FOR THE TEST FILE

**Use testthat version 3e style:**

- Assume the function is already available when tests run (do NOT redefine or source it)
- Use multiple `test_that()` blocks grouped logically (inputs, outputs, functionality, edge cases, etc.)
- One file per function: saved as `test-FUNCTION_NAME.R` where `FUNCTION_NAME` is taken from the function definition
- Do not define the function in the test file

**Name tests clearly and descriptively:**

- Examples:
  - `"FUNCTION_NAME() validates input types"`
  - `"FUNCTION_NAME() returns correctly named output"`
  - `"FUNCTION_NAME() handles NA values"`
  - `"FUNCTION_NAME() computes mean by group"`

**Inferring intended behaviour:**

- Primary sources:
  - Function name, argument names, default values, and roxygen comments
- Secondary source:
  - The actual code, to understand:
    - Expected input classes and structures
    - Expected output shape (vector, list, data.frame, etc.)
    - Any invariants (e.g., sorted output, monotonicity, matching dimensions)
- When there is tension between the implementation and the name/comments, prefer the intended meaning from the name/comments for designing expected results in tests

## WHAT TO TEST – INPUTS

For EACH argument of the function, based on its name, defaults, and usage in the body:

**Valid inputs:**

- Create at least one "happy path" example where all arguments are valid and the function runs without error
- If an argument is a data.frame or a named list/vector:
  - Test that correct column/element NAMES are accepted
- If constraints are implied (length, range, allowed choices, logical flags, factor levels, etc.):
  - Test that valid values are accepted without error

**Invalid inputs / error handling:**

- For each argument, create multiple `expect_error()` tests, including:
  - Wrong class/type
  - If applicable, wrong or missing names in data.frames/named vectors
  - Wrong length (vector length > 1 when scalar is expected)
  - Clearly invalid values (negative where only non-negative makes sense, unsupported levels/options, out-of-range values)

**Where the implementation uses `stop()`, `stopifnot()`, or similar:**

- Use `expect_error()` and, where possible, match a meaningful part of the error message with regex

**NA / NaN / Inf / NULL / empty:**

- If the intended behaviour suggests something about missing or infinite values (e.g., a "safe" or "robust" function):
  - Add tests passing `NA`, `NaN`, `Inf`, `-Inf`, `NULL`, empty vectors, or zero-row data.frames where relevant
- Use:
  - `expect_error()` if such inputs should be rejected
  - Or `expect_*()` checks when behaviour should be defined (e.g., NA propagated, rows dropped, etc.)

## WHAT TO TEST – OUTPUT STRUCTURE

Using valid "happy path" inputs that match the intended use:

**Check the class/type of the returned object:**

- Use `expect_s3_class()`, `expect_type()`, `expect_true(is.data.frame(...))`, etc.
- Choose expectations that align with the function's name and comments (e.g., a function named `*_df()` probably returns a data.frame or tibble)

**Check names and structure:**

- For vectors/lists: `expect_named()`, `expect_length()`
- For data.frames/tibbles:
  - Check `nrow()`, `ncol()`, and `colnames()`
  - Ensure all important fields suggested by the name and comments (e.g., `mean`, `sd`, `lower`, `upper`, `group`) are present and correctly named

**Multiple output modes:**

- If an argument appears to control the output format (e.g., `return`, `summary`, `wide`, `long`, `as_list`):
  - Add tests for each important mode
  - Check that each mode returns the expected type and structure

## WHAT TO TEST – FUNCTIONALITY / CORRECTNESS

Design tests to verify that the function does what its name and comments suggest, not just what the current code happens to do.

**Simple, fully worked-out examples:**

- Build small, hand-checkable inputs (tiny vectors/data.frames)
- Use base R/tidyverse to compute the expected results manually inside the test
- Compare actual vs expected with:
  - `expect_equal()` for exact comparisons (integers, logicals, factors, character)
  - `expect_equal(..., tolerance = 1e-8)` for floating-point calculations

**Behavioural invariants:**

- If the name suggests properties (e.g., "sorted", "unique", "normalised", "scaled", "proportion", "probability"):
  - Write tests that check those invariants directly:
    - Sorted: `expect_true(!is.unsorted(result))`
    - Unique: `expect_equal(length(unique(result)), length(result))`
    - Probabilities: `expect_true(all(result >= 0 & result <= 1))`, `expect_equal(sum(result), 1, tolerance = 1e-8)`
    - Monotonicity, matching sums, preserved totals, etc.

**Combination of arguments:**

- Test multiple combinations of parameters that trigger different branches of logic:
  - Varying method flags (e.g., `method = "A"` vs `method = "B"`)
  - Toggling logical switches
  - Providing vs omitting optional arguments
- Ensure behaviour matches the intended semantics you infer (e.g., "weighted" vs "unweighted")

**Edge cases:**

- Boundary values: minimum/max values, single-row data, single group, all-equal values, etc.
- Non-trivial but realistic edge cases: duplicated entries, unbalanced grouping, rare factor levels, etc.
- Confirm that these behave sensibly, again guided by the function's name and comments

**Randomness:**

- If the function uses randomness, wrap calls in `set.seed()` so tests are reproducible
- Under a fixed seed and given the same inputs, the results should be stable

## WARNINGS, MESSAGES, AND SIDE EFFECTS

If the function appears designed to inform the user via `warning()` or `message()`:

**Warnings:**

- Use `expect_warning()` when certain inputs should trigger a warning (e.g., deprecated arguments, partial matches, automatic coercions)
- Match on part of the warning message, if possible

**Messages:**

- Use `expect_message()` if the function is intended to print progress/information

**Side effects:**

- If the function clearly writes files or modifies some ambient state:
  - Use temporary locations (`tempdir()` or withr-like patterns if available)
  - Ensure side effects occur only when intended and not for standard calls

## TIDYVERSE / NSE COMPATIBILITY (IF RELEVANT)

If the function uses non-standard evaluation (NSE) or tidyverse-style programming (e.g., `{{ }}`, `enquo`, `!!`, dplyr, rlang):

**Test:**

- Usage with bare column names (e.g., `col = x` style)
- Behaviour when columns are renamed
- Clear failure when required columns are missing
- Include at least one test to show correct NSE behaviour:
  - e.g., working with both bare column names and quoted strings if that seems intended

## STYLE AND OUTPUT RULES

**Use only base R and testthat in the tests:**

- Plus any packages that the function itself clearly depends on, loaded via `library()` if needed

**Prefer precise expectations:**

- Use `expect_equal()`, `expect_error()`, `expect_warning()`, `expect_message()`, `expect_named()`, etc., rather than generic `expect_true()` where a more specific assertion is possible

**Group expectations logically:**

- Within `test_that()` blocks according to purpose:
  - Inputs, outputs, functionality, edge cases, messages/warnings, etc.

**Code formatting:**

- Limit lines to 80 characters maximum
- Place assignment targets on their own line, then `<-` on the next line with the value:

  ```r
  # Good:
  data_test <-
    data.frame(
      x = 1:3,
      y = 4:6
    )
  
  # Not:
  data_test <- data.frame(x = 1:3, y = 4:6)
  ```

- Prefer longer (more lines) over wider code
- Name objects by their type using prefixes:
  - `data_` for data.frames/tibbles (e.g., `data_test`, `data_input`)
  - `vec_` for vectors (e.g., `vec_values`, `vec_ids`)
  - `list_` for lists (e.g., `list_to_test`, `list_params`)
  - `mat_` for matrices (e.g., `mat_input`)
  - Other descriptive type prefixes as appropriate

**Do NOT:**

- Print anything or call `cat()`
- Include explanatory prose
- Include any comments or text outside of R code

## IMPLEMENTATION

Create file `test-FUNCTION_NAME.R` with the complete testthat tests as specified above within `R/03_Supplementary_analyses/tests/testthat` folder.
