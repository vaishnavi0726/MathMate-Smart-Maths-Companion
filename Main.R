# MathMate - Final Project (Phase 1 to 5 Combined)
cat("========================================\n")
cat(" MathMate - Your Smart Maths Companion\n")
cat("========================================\n")

repeat{
  cat("\n--- MAIN MENU ---\n")
  cat("1. Calculator\n2. Vector Lab\n3. Matrix Lab\n")
  cat("4. Statistics Lab\n5. Student Performance Table\n")
  cat("6. Graph Studio\n7. Fun Quiz\n8. Exit\n")
  ch <- as.integer(readline("Choose (1-8): "))
  
  if(ch==1){
    a <- as.numeric(readline("Enter num1: "))
    b <- as.numeric(readline("Enter num2: "))
    op <- readline("Op (+,-,*,/): ")
    if(op=="+") print(a+b) else if(op=="-") print(a-b)
    else if(op=="*") print(a*b) else print(a/b)
  }
  else if(ch==2){
    v <- as.numeric(strsplit(readline("Enter vector (e.g. 2,4,6,8): "),",")[[1]])
    cat("Mean:",mean(v)," Sum:",sum(v)," Max:",max(v),"\n")
    cat("Sorted:",sort(v),"\n")
  }
  else if(ch==3){
    m <- matrix(1:9, nrow=3)
    print(m)
    cat("Transpose:\n"); print(t(m))
    cat("Det:",det(m),"\n")
  }
  else if(ch==4){
    marks <- c(90,100,89,78,95)
    cat("Mean:",mean(marks)," Median:",median(marks)," Topper:",max(marks),"\n")
  }
  else if(ch==5){
    sname <- c("sri","sadhu","gopi","arun","bala")
    smarks <- c(90,100,89,78,95)
    df <- data.frame(Name=sname, Marks=smarks,
                     Grade=cut(smarks, breaks=c(0,50,75,90,100), labels=c("D","C","B","A")))
    print(df)
  }
  else if(ch==6){
    x <- c(10,20,30,40,25)
    barplot(x, main="MathMate Graph")
  }
  else if(ch==7){
    ans <- readline("What is mean of 2,4,6? ")
    if(ans=="4") cat("Correct da!\n") else cat("Wrong! Ans is 4\n")
  }
  else if(ch==8){
    cat("Thank you! Goodbye!\n"); break
  }
}