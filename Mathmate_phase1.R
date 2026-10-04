# ==========================================================
# MathMate - Interactive Mathematics Learning & Analysis System
# Phase 1: Menu Engine + Smart Calculator
# Author : <your name>
# Date   : <date>
# Run in RStudio or the R console (readline() needs interactive mode)
# ==========================================================

if (!interactive()) {
  stop("Please run MathMate in RStudio or the R console.")
}

# ---------- Helper: safe number input ----------
# readline() always returns text, so we convert and validate it.
get_number <- function(prompt) {
  repeat {
    x <- suppressWarnings(as.numeric(readline(prompt)))
    if (!is.na(x)) return(x)
    cat("Invalid input. Please enter a number.\n")
  }
}

# ---------- Calculator functions (one job each) ----------
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

# ---------- Module 1: Smart Calculator ----------
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

# ---------- Module 2: Menu Engine ----------
main_menu <- function() {
  repeat {
    cat("\n===== MathMate =====\n")
    cat("1. Smart Calculator\n")
    cat("2. Exit\n")
    choice <- readline("Enter choice: ")
    
    if (choice == "1") {
      calculator()
    } else if (choice == "2") {
      cat("Thank you for using MathMate. Goodbye!\n")
      break
    } else {
      cat("Invalid choice. Please enter 1 or 2.\n")
    }
  }
}

# ---------- Start the program ----------
main_menu()