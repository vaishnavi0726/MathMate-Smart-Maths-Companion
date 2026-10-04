# ==========================================================
# MathMate - Interactive Mathematics Learning & Analysis System
# Phase 1: Menu Engine + Smart Calculator
# Phase 2: Vector Lab + Matrix Lab
# Author : <your name>
# Date   : <date>
# Run in RStudio or the R console (readline() needs interactive mode)
# ==========================================================

if (!interactive()) {
  stop("Please run MathMate in RStudio or the R console.")
}

# ==========================================================
# HELPERS (input functions used by all modules)
# ==========================================================

# Read one number safely
get_number <- function(prompt) {
  repeat {
    x <- suppressWarnings(as.numeric(readline(prompt)))
    if (!is.na(x)) return(x)
    cat("Invalid input. Please enter a number.\n")
  }
}

# Read a whole number >= 1 (used for matrix sizes)
get_positive_int <- function(prompt) {
  repeat {
    x <- get_number(prompt)
    if (x >= 1 && x == floor(x)) return(x)
    cat("Please enter a whole number of 1 or more.\n")
  }
}

# Read comma-separated numbers into a vector, e.g. "2, 4, 6"
# expected_length is optional: if given, the vector must be that long
get_vector <- function(prompt, expected_length = NULL) {
  repeat {
    raw   <- readline(prompt)
    parts <- trimws(strsplit(raw, ",")[[1]])
    v     <- suppressWarnings(as.numeric(parts))
    
    if (length(v) == 0 || any(is.na(v))) {
      cat("Invalid input. Enter numbers separated by commas, e.g. 2, 4, 6\n")
      next
    }
    if (!is.null(expected_length) && length(v) != expected_length) {
      cat("Please enter exactly", expected_length, "numbers.\n")
      next
    }
    return(v)
  }
}

# Read a matrix: rows, columns, then elements row by row
get_matrix <- function(name) {
  cat("\nMatrix", name, "\n")
  nr <- get_positive_int("Number of rows: ")
  nc <- get_positive_int("Number of columns: ")
  vals <- get_vector(
    paste0("Enter ", nr * nc, " elements row by row (comma-separated): "),
    nr * nc
  )
  matrix(vals, nrow = nr, ncol = nc, byrow = TRUE)
}

# ==========================================================
# MODULE 1: SMART CALCULATOR (from Phase 1)
# ==========================================================

add_nums <- function(a, b) {
  cat("Step 1: Adding", a, "and", b, "\n")
  a + b
}

subtract_nums <- function(a, b) {
  cat("Step 1: Subtracting", b, "from", a, "\n")
  a - b
}

multiply_nums <- function(a, b) {
  cat("Step 1: Multiplying", a, "by", b, "\n")
  a * b
}

divide_nums <- function(a, b) {
  if (b == 0) {
    cat("Error: Division by zero is not possible.\n")
    return(NA)
  }
  cat("Step 1: Dividing", a, "by", b, "\n")
  a / b
}

power_nums <- function(a, b) {
  cat("Step 1: Raising", a, "to the power", b, "\n")
  a^b
}

sqrt_num <- function(a) {
  if (a < 0) {
    cat("Error: Square root of a negative number is not real.\n")
    return(NA)
  }
  cat("Step 1: Finding the number that multiplied by itself gives", a, "\n")
  sqrt(a)
}

factorial_num <- function(a) {
  if (a < 0 || a != floor(a)) {
    cat("Error: Factorial needs a non-negative whole number.\n")
    return(NA)
  }
  cat("Step 1: Multiplying all whole numbers from 1 to", a, "\n")
  factorial(a)
}

calculator <- function() {
  cat("\n--- Smart Calculator ---\n")
  cat("1. Add        2. Subtract    3. Multiply\n")
  cat("4. Divide     5. Power       6. Square root\n")
  cat("7. Factorial\n")
  op <- readline("Choose operation (1-7): ")
  
  if (op %in% c("1", "2", "3", "4", "5")) {
    a <- get_number("Enter first number: ")
    b <- get_number("Enter second number: ")
    result <- switch(op,
                     "1" = add_nums(a, b),
                     "2" = subtract_nums(a, b),
                     "3" = multiply_nums(a, b),
                     "4" = divide_nums(a, b),
                     "5" = power_nums(a, b)
    )
  } else if (op %in% c("6", "7")) {
    a <- get_number("Enter the number: ")
    result <- switch(op,
                     "6" = sqrt_num(a),
                     "7" = factorial_num(a)
    )
  } else {
    cat("Invalid operation. Please choose 1 to 7.\n")
    return(invisible(NULL))
  }
  
  if (is.na(result)) {
    cat("No valid result.\n")
  } else {
    cat("Result:", result, "\n")
  }
}

# ==========================================================
# MODULE 3A: VECTOR LAB (Phase 2)
# ==========================================================

vector_summary <- function() {
  v <- get_vector("Enter numbers separated by commas: ")
  cat("Step 1: Your vector is stored as:", v, "\n")
  cat("Step 2: Calculating summary values...\n\n")
  
  # A list can hold values of different lengths together
  info <- list(
    Length   = length(v),
    Sum      = sum(v),
    Mean     = mean(v),
    Minimum  = min(v),
    Maximum  = max(v),
    Sorted   = sort(v),
    Reversed = rev(v)
  )
  
  for (item in names(info)) {
    cat(item, ":", info[[item]], "\n")
  }
}

add_two_vectors <- function() {
  v1 <- get_vector("Enter first vector: ")
  v2 <- get_vector("Enter second vector (same length): ", length(v1))
  cat("Step 1: Adding element by element (position 1 + position 1, ...)\n")
  cat("Result:", v1 + v2, "\n")
}

dot_product <- function() {
  v1 <- get_vector("Enter first vector: ")
  v2 <- get_vector("Enter second vector (same length): ", length(v1))
  cat("Step 1: Multiplying element by element:", v1 * v2, "\n")
  cat("Step 2: Adding all those products together\n")
  cat("Dot product:", sum(v1 * v2), "\n")
}

vector_lab <- function() {
  cat("\n--- Vector Lab ---\n")
  cat("1. Summary of one vector\n")
  cat("2. Add two vectors\n")
  cat("3. Dot product\n")
  choice <- readline("Choose (1-3): ")
  
  if (choice == "1") {
    vector_summary()
  } else if (choice == "2") {
    add_two_vectors()
  } else if (choice == "3") {
    dot_product()
  } else {
    cat("Invalid choice. Please choose 1 to 3.\n")
  }
}

# ==========================================================
# MODULE 3B: MATRIX LAB (Phase 2)
# ==========================================================

matrix_lab <- function() {
  cat("\n--- Matrix Lab ---\n")
  cat("1. Add            2. Subtract       3. Multiply\n")
  cat("4. Transpose      5. Determinant    6. Inverse\n")
  op <- readline("Choose operation (1-6): ")
  
  if (op %in% c("1", "2")) {
    A <- get_matrix("A")
    B <- get_matrix("B")
    if (!all(dim(A) == dim(B))) {
      cat("Error: Both matrices must have the same size.\n")
      return(invisible(NULL))
    }
    if (op == "1") {
      cat("Step 1: Adding matching positions\n")
      print(A + B)
    } else {
      cat("Step 1: Subtracting matching positions\n")
      print(A - B)
    }
    
  } else if (op == "3") {
    A <- get_matrix("A")
    B <- get_matrix("B")
    if (ncol(A) != nrow(B)) {
      cat("Error: Columns of A must equal rows of B.\n")
      return(invisible(NULL))
    }
    cat("Step 1: Each row of A is multiplied with each column of B and added\n")
    print(A %*% B)
    
  } else if (op == "4") {
    A <- get_matrix("A")
    cat("Step 1: Rows become columns and columns become rows\n")
    print(t(A))
    
  } else if (op %in% c("5", "6")) {
    A <- get_matrix("A")
    if (nrow(A) != ncol(A)) {
      cat("Error: This operation needs a square matrix (rows = columns).\n")
      return(invisible(NULL))
    }
    d <- det(A)
    if (op == "5") {
      cat("Step 1: Calculating the determinant of a square matrix\n")
      cat("Determinant:", round(d, 4), "\n")
    } else {
      cat("Step 1: Checking the determinant:", round(d, 4), "\n")
      if (abs(d) < 1e-10) {
        cat("Determinant is 0, so the inverse does not exist.\n")
      } else {
        cat("Step 2: Determinant is not 0, so the inverse exists\n")
        print(round(solve(A), 4))
      }
    }
    
  } else {
    cat("Invalid operation. Please choose 1 to 6.\n")
  }
}

# ==========================================================
# MAIN MENU
# ==========================================================

main_menu <- function() {
  repeat {
    cat("\n===== MathMate =====\n")
    cat("1. Smart Calculator\n")
    cat("2. Vector Lab\n")
    cat("3. Matrix Lab\n")
    cat("4. Exit\n")
    choice <- readline("Enter choice: ")
    
    if (choice == "1") {
      calculator()
    } else if (choice == "2") {
      vector_lab()
    } else if (choice == "3") {
      matrix_lab()
    } else if (choice == "4") {
      cat("Thank you for using MathMate. Goodbye!\n")
      break
    } else {
      cat("Invalid choice. Please enter 1 to 4.\n")
    }
  }
}

main_menu()