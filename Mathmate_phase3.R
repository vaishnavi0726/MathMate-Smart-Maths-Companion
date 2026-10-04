# ==========================================================
# MathMate - Interactive Mathematics Learning & Analysis System
# Phase 1: Menu Engine + Smart Calculator
# Phase 2: Vector Lab + Matrix Lab
# Phase 3: Statistics Lab + Student Marks Table (data frames)
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

# Read a whole number >= 1
get_positive_int <- function(prompt) {
  repeat {
    x <- get_number(prompt)
    if (x >= 1 && x == floor(x)) return(x)
    cat("Please enter a whole number of 1 or more.\n")
  }
}

# Read marks between 0 and 100
get_mark <- function(prompt) {
  repeat {
    x <- get_number(prompt)
    if (x >= 0 && x <= 100) return(x)
    cat("Marks must be between 0 and 100.\n")
  }
}

# Read comma-separated numbers into a vector, e.g. "2, 4, 6"
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
# MODULE 1: SMART CALCULATOR (Phase 1)
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
# MODULE 4: STATISTICS LAB (Phase 3)
# ==========================================================

# R has no built-in function for mode, so we write our own.
# table() counts how many times each value appears.
get_mode <- function(v) {
  freq <- table(v)
  if (max(freq) == 1) return(NA)   # no value repeats
  as.numeric(names(freq)[freq == max(freq)])
}

# Print the full statistics summary for a numeric vector
stats_summary <- function(v) {
  cat("\nStep 1: Finding the centre of the data\n")
  cat("Count    :", length(v), "\n")
  cat("Mean     :", round(mean(v), 2), "(sum divided by count)\n")
  cat("Median   :", median(v), "(middle value after sorting)\n")
  
  mode_val <- get_mode(v)
  if (all(is.na(mode_val))) {
    cat("Mode     : no repeated value\n")
  } else {
    cat("Mode     :", mode_val, "(most frequent value)\n")
  }
  
  cat("\nStep 2: Measuring how spread out the data is\n")
  cat("Minimum  :", min(v), "\n")
  cat("Maximum  :", max(v), "\n")
  cat("Range    :", max(v) - min(v), "(maximum minus minimum)\n")
  
  q <- quantile(v, c(0.25, 0.75))
  cat("Q1       :", q[[1]], "(25% of values are below this)\n")
  cat("Q3       :", q[[2]], "(75% of values are below this)\n")
  
  cat("Variance :", round(var(v), 2), "\n")
  cat("Std Dev  :", round(sd(v), 2), "(square root of variance)\n")
}

# Option 1 of the Statistics Lab
dataset_stats <- function() {
  v <- get_vector("Enter your data separated by commas: ")
  if (length(v) < 2) {
    cat("Please enter at least 2 numbers to calculate spread.\n")
    return(invisible(NULL))
  }
  stats_summary(v)
}

# Turn marks into grades using cut()
assign_grade <- function(marks) {
  as.character(cut(marks,
                   breaks = c(0, 40, 50, 65, 80, Inf),
                   labels = c("F", "D", "C", "B", "A"),
                   right  = FALSE))
}

# Option 2 of the Statistics Lab: build and analyse a data frame
student_table <- function() {
  n <- get_positive_int("How many students? ")
  if (n < 2) {
    cat("Please enter at least 2 students.\n")
    return(invisible(NULL))
  }
  
  names_vec <- character(n)
  marks_vec <- numeric(n)
  for (i in 1:n) {
    nm <- trimws(readline(paste0("Name of student ", i, ": ")))
    if (nm == "") nm <- paste("Student", i)
    names_vec[i] <- nm
    marks_vec[i] <- get_mark(paste0("Marks of ", nm, " (0-100): "))
  }
  
  # A data frame is a table: each column is a vector of the same length
  df <- data.frame(Name = names_vec, Marks = marks_vec,
                   stringsAsFactors = FALSE)
  df$Grade <- assign_grade(df$Marks)   # add a new column
  
  repeat {
    cat("\n--- Student Marks Table ---\n")
    cat("1. Show table              2. Statistics of marks\n")
    cat("3. Grade count             4. Sort by marks (high to low)\n")
    cat("5. Students above average  6. Back\n")
    ch <- readline("Choose (1-6): ")
    
    if (ch == "1") {
      print(df, row.names = FALSE)
      
    } else if (ch == "2") {
      stats_summary(df$Marks)
      cat("\nTopper   :", df$Name[which.max(df$Marks)], "\n")
      cat("Lowest   :", df$Name[which.min(df$Marks)], "\n")
      
    } else if (ch == "3") {
      cat("Step 1: Counting how many students got each grade\n")
      print(table(df$Grade))
      
    } else if (ch == "4") {
      cat("Step 1: Ordering rows by the Marks column\n")
      sorted <- df[order(df$Marks, decreasing = TRUE), ]
      print(sorted, row.names = FALSE)
      
    } else if (ch == "5") {
      avg <- mean(df$Marks)
      cat("Step 1: Class average is", round(avg, 2), "\n")
      cat("Step 2: Keeping only rows where Marks > average\n")
      above <- df[df$Marks > avg, ]
      if (nrow(above) == 0) {
        cat("No student is above the average.\n")
      } else {
        print(above, row.names = FALSE)
      }
      
    } else if (ch == "6") {
      break
      
    } else {
      cat("Invalid choice. Please choose 1 to 6.\n")
    }
  }
}

stats_lab <- function() {
  cat("\n--- Statistics Lab ---\n")
  cat("1. Statistics of a dataset\n")
  cat("2. Student marks table\n")
  choice <- readline("Choose (1-2): ")
  
  if (choice == "1") {
    dataset_stats()
  } else if (choice == "2") {
    student_table()
  } else {
    cat("Invalid choice. Please choose 1 or 2.\n")
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
    cat("4. Statistics Lab\n")
    cat("5. Exit\n")
    choice <- readline("Enter choice: ")
    
    if (choice == "1") {
      calculator()
    } else if (choice == "2") {
      vector_lab()
    } else if (choice == "3") {
      matrix_lab()
    } else if (choice == "4") {
      stats_lab()
    } else if (choice == "5") {
      cat("Thank you for using MathMate. Goodbye!\n")
      break
    } else {
      cat("Invalid choice. Please enter 1 to 5.\n")
    }
  }
}

main_menu()