library(shiny)

# Source in repayment calculating function
source('Repayment steps.R')
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel('Loan repayment steps'),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        width = 2, 
        conditionalPanel(
          condition = 'input.tabset == \'interestShare\'',
          numericInput(
            inputId = 'principal', label = 'Principal', value = 1000000, 
          ), 
          numericInput(
            inputId = 'interestRate', label = 'Interest rate', value = 0.05
          ), 
          radioButtons(
            inputId = 'frequency', label = 'Repayment frequency', inline = TRUE, 
            choiceNames = c('Weekly', 'Fortnightly', 'Monthly'), 
            choiceValues = c('weekly', 'fortnightly', 'monthly')
          ), 
          sliderInput(
            inputId = 'repayment', label = 'Weekly repayments', min = 500, 
            max = 3000, value = 2689, pre = '$'
          )
        ), 
        conditionalPanel(
          condition = 'input.tabset == \'fixedAndFloating\'',
          fluidRow(
            tags$head(
              tags$style(
                type='text/css',
                '.inline label{
                  display: table-cell;
                  text-align: left;
                  vertical-align: middle;
                }
                .inline .form-group{
                  display: table-row;
                }
                '
              )
            ),
            tags$div(
              class = 'inline', 
              numericInput(
                inputId = 'totalPrincipal', label = 'Total principal', 
                value = 1000000
              ), 
              numericInput(
                inputId = 'weeklyIncome', label = 'Weekly income', 
                value = 2689
              ), 
              numericInput(
                inputId = 'fixedInt', label = 'Fixed rate', value = 0.04
              ),
              numericInput(
                inputId = 'floatInt', label = 'Float rate', value = 0.05
              )
            ),
            sliderInput(
              inputId = 'fixedShare', label = 'Fixed as a share of total', 
              value = 0.85, min = 0, max = 1, step = 0.01
            )
          )
        )
      ), 

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          id = 'tabset', 
          tabPanel(
            title = 'Interest share analysis', value = 'interestShare', 
            plotOutput('interestShare', height = '800px'), 
            tableOutput('repaymentTable')
          ), 
          tabPanel(
            title = 'Split of fixed & floating', value = 'fixedAndFloating', 
            plotOutput('shareOptimiser', height = '800px')
          )
        )

      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  repaymentsInterestShare <- reactive({
    repayments <- calcRepayments(
      principal = input$principal, 
      interestRate = input$interestRate, 
      repayment = input$repayment, 
      freq = input$frequency
    )
    repayments
  })

    output$interestShare <- renderPlot({
      x <- repaymentsInterestShare()
      x[, InterestShare := `Interest paid` / (`Interest paid` + `Principal paid`)]
      xdate <- x[.N * 0.8, Date]
      yshare1 <- x[order(InterestShare)][.N * 0.8, InterestShare]
      yshare2 <- x[order(InterestShare)][.N * 0.7, InterestShare]
      interestShare <- x[
        , sum(`Interest paid`) / (
          sum(`Interest paid`) + max(Owed) + min(`Principal paid`)
        )
      ]
      interestShare <- paste0(round(100 * interestShare, digits = 2), '%')
      interest <- x[, sum(`Interest paid`)]
      interest <- paste0('$', prettyNum(round(interest), big.mark = ','))
      plot(
        x = x$Date, y = x$InterestShare, type = 'l', xlab = '', 
        ylab = 'Interest paid as a share of each repayment'
      )
      text(
        x = xdate, y = yshare1, adj = 0, 
        labels = paste0('Total interest paid : ', interest)
      )
      text(
        x = xdate, y = yshare2, adj = 0, 
        labels = paste0('Total interest share: ', interestShare)
      )
    })
    
    output$repaymentTable <- renderTable({
      x <- repaymentsInterestShare()
      x[, Date := format(Date, '%d-%m-%Y')]
      x
    })
    
    repaymentsFixedShare <- reactive({
      maxRepaymentShare <- 0.0023
      fixedShare <- input$fixedShare
      fixedPrincipal <- fixedShare * input$totalPrincipal
      fixedRepayment <- fixedPrincipal * maxRepaymentShare
      if (input$weeklyIncome <= fixedRepayment) {
        stop('Income is low enough to consider not having a floating component.')
      }
      floatShare <- 1 - fixedShare
      floatPrincipal <- floatShare * input$totalPrincipal
      floatRepayment <- input$weeklyIncome - fixedRepayment
      fixed <- calcRepayments(
        principal = fixedPrincipal, 
        interestRate = input$fixedInt, 
        repayment = fixedRepayment, 
        freq = 'weekly'
      )
      float <- calcRepayments(
        principal = floatPrincipal, 
        interestRate = input$floatInt, 
        repayment = floatRepayment, 
        freq = 'weekly'
      )
      out <- list(fixed = fixed, float = float)
      out
    })
    
    output$optimiserStatus <- renderUI({
      
    })
    
    output$shareOptimiser <- renderPlot({
      x <- repaymentsFixedShare()
      fixed <- x[['fixed']]
      float <- x[['float']]
      fixedInt <- fixed[, sum(`Interest paid`)]
      floatInt <- float[, sum(`Interest paid`)]
      totalInt <- fixedInt + floatInt
      dt <- if (fixed[, .N] >= float[, .N]) {
        data.table(Date = fixed[, Date])
      } else {
        data.table(Date = float[, Date])
      }
      dt[
        fixed, on = 'Date', 
        FixedOwed := i.Owed
      ][
        float, on = 'Date', 
        FloatOwed := i.Owed
      ]
      # setnafill(dt, type = 'const', fill = 0)
      yl <- c(0, max(
        dt[, max(FixedOwed, na.rm = TRUE)], 
        dt[, max(FloatOwed, na.rm = TRUE)]
      ))
      plot(
        x = dt$Date, y = dt$FixedOwed, type = 'l', xlab = '', cex.axis = 1.5, 
        ylab = 'Principal owed', ylim = yl, col = 'orange4', yaxt = 'n', 
        lwd = 2, cex.lab = 1.5
      )
      matplot(
        x = dt$Date, y = dt$FloatOwed, col = 'purple4', type = 'l', lwd = 2, 
        add = TRUE
      )
      yat <- seq(0, max(yl), length.out = 10)
      comprss <- function(tx) { 
        div <- findInterval(as.numeric(gsub('\\,', '', tx)), 
                            c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
        paste0(round(as.numeric(gsub('\\,','',tx))/10^(3*(div-1)), 2), 
              c('','K','M','B','T')[div] )}
      yatmarks <- paste0('$', sapply(round(yat, digits = -3), comprss))
      axis(side = 2, at = yat, labels = yatmarks, cex.axis = 1.3)
      rect(
        xleft = quantile(as.numeric(dt$Date), 0.6), 
        xright = quantile(as.numeric(dt$Date), (0.6 + (0.29) * input$fixedShare)), 
        ytop = quantile(yat, 0.95), 
        ybottom = quantile(yat, 0.92), 
        col = rgb(t(col2rgb(col = 'orange4')), alpha = 0.8 * 255, maxColorValue = 255), 
        density = -0.1, 
        lty = 0
      )
      rect(
        xleft = quantile(as.numeric(dt$Date), (0.6 + (0.29 * input$fixedShare))), 
        xright = quantile(as.numeric(dt$Date), 0.89), 
        ytop = quantile(yat, 0.95), 
        ybottom = quantile(yat, 0.92), 
        col = rgb(t(col2rgb(col = 'purple4')), alpha = 0.8 * 255, maxColorValue = 255),  
        density = -0.1, 
        lty = 0
      )
      floatShare <- 1 - input$fixedShare
      maxRepaymentShare <- 0.0023
      fixedRepayment <- input$fixedShare * input$totalPrincipal * maxRepaymentShare
      floatPrincipal <- floatShare * input$totalPrincipal
      floatRepayment <- input$weeklyIncome - fixedRepayment
      text(
        x = rep(c(
          quantile(as.numeric(dt$Date), 0.6), 
          quantile(as.numeric(dt$Date), (0.6 + (0.29 * input$fixedShare))), 
          quantile(as.numeric(dt$Date), 0.95)
        ), times = 3), 
        y = mapply(
          quantile, 
          MoreArgs = list(x = yat), 
          probs = rep(c(0.99, 0.87, 0.82), each = 3)
        ), 
        labels = c(
          'Fixed', 'Float', 'Total', 
          paste0('$', prettyNum(round(c(fixedInt, floatInt, totalInt), 0), ',')), 
          paste0('$', prettyNum(c(fixedRepayment, floatRepayment, input$weeklyIncome), ','))
        ), 
        col = rep(c('orange4', 'purple4', 'black'), times = 3), 
        adj = 0, 
        cex = 1.5
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
