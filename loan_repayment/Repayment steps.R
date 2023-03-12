library(data.table)
calcRepayments <- function(principal, interestRate, repayment, freq) {
  frequencies <- c(
    'weekly' = 52.1429, 
    'fortnightly' = 52.1429 / 2, 
    'monthly' = 12
  )
  repaymentFrequency <- c(
    'weekly' = '1 week', 
    'fortnightly' = '2 weeks', 
    'monthly' = '1 month'
  )
  period <- frequencies[freq]
  increment <- repaymentFrequency[freq]
  dt <- data.table(
    Date             = Sys.Date()[0], 
    `Principal paid` = numeric(), 
    `Interest paid`  = numeric(),
    Owed             = numeric()
  )
  repaymentDay <- Sys.Date()
  while(principal > 0) {
    interest <- principal * (interestRate / period)
    if (interest > repayment) {
      stop('Please increase repayment as the principal is ballooning.')
    }
    principal <- principal + interest - repayment
    dtAppend <- data.table(
      Date             = repaymentDay, 
      `Principal paid` = repayment - interest, 
      `Interest paid`  = interest,
      Owed             = principal
    )
    repaymentDay <- seq.Date(
      from = repaymentDay, length.out = 2, by = increment
    )[2]
    dt <- rbindlist(list(dt, dtAppend))
  }
  return(dt)
}
x <- calcRepayments(
  principal = 667000, 
  interestRate = 0.042, 
  repayment = 720, 
  freq = 'weekly'
)
y <- calcRepayments(
  principal = 667000, 
  interestRate = 0.042, 
  repayment = 720 * 2, 
  freq = 'fortnightly'
)
z <- calcRepayments(
  principal = 667000, 
  interestRate = 0.042, 
  repayment = 720 * 52.1429 / 12, 
  freq = 'monthly'
)
