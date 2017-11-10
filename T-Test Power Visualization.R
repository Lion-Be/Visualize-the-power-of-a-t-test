

t.power.simulation <- function(n.s1, mean.s1, sd.s1, n.s2, mean.s2, sd.s2, alpha, test.number){
  
  # Seed Value is set
  set.seed(35463)
  
  # Empty vectors are created for p-values and t-statistics
  p.values <- rep(0, test.number)
  t.statistics <- rep(0, test.number)
  
  # Indicator for un(equal) population variances is created
  if (sd.s1 == sd.s2) {
    var <- TRUE
  } else {
    var <- FALSE
  }
  
  # Specified Number of T-Tests is carried out
  for (i in 1:test.number)
  {
    # Data is generated
    group.control <- rnorm(n=n.s1, mean=mean.s1, sd=sd.s1) 
    group.experimental <- rnorm(n=n.s2, mean=mean.s2, sd=sd.s2)
    
    # Performing Specified Number of T-Tests
    t.result <- t.test(group.control, group.experimental, var.equal = var)
    
    # Storing p-values and t-statistics in vectors
    p.values[i] <- t.result$p.value
    t.statistics[i] <- t.result$statistic
  }
  
  # Graphics
  
  # Displaying p-values in histogram
  histo.p <- hist(p.values, nclass=100, plot=FALSE)
  cuts.p <- cut(histo.p$breaks, c(-Inf, alpha))
  histo.p <- plot(histo.p, col="chartreuse"[cuts.p])
  
  # Displaying t-statistics in histogram
  histo.t <- hist(t.statistics, nclass=100, plot=FALSE)
  t <- abs(qt(alpha/2,t.result$parameter))
  cuts.t <- cut(histo.t$breaks, c(-Inf,-t,t,Inf))
  histo.t <- plot(histo.t, col=c("chartreuse", "white", "chartreuse")[cuts.t])
  
  # Obtain power estimate
  
  # Number of tests conducted
  total <- test.number
  
  # Number of significant p-values
  p.values.sig <- p.values[p.values<alpha]  
  sig <- length(p.values.sig)
  
  # Share of significant p-values of total number of p-values
  power <- sig/total
  
  # Construct a table that summarises group configurations
  
  # Order Components as a Table
  rows1 <- rbind(mean.s1, sd.s1, n.s1)
  rows2 <- rbind(mean.s2, sd.s2, n.s2)
  table.1 <- cbind(rows1, rows2)
  
  # Rename Columns and Rows
  col.names.1 <- c("Group1", "Group2")
  colnames(table.1) <- col.names.1
  row.names.1 <- c("Mean", "Std. Dev.", "n")
  rownames(table.1) <- row.names.1
  
  # Construct a table that summarises simulation configurations and gives power
  
  # Order Components as a Table  
  rows3 <- rbind(alpha, test.number, power)
  table.2 <- as.matrix(rows3)
  
  # Rename Columns and Rows
  colnames(table.2)[1] <- "Simulation"
  row.names.2 <- c("Specified Alpha Level", "Number of Tests", "Power Estimate")
  rownames(table.2) <- row.names.2
  
  # Ommit printing exponential numbers
  options(scipen = 999)
  
  # Specifying output list
  output <- list("Group Configurations in Population"=table.1, "Simulation Configurations and Power Estimate"=table.2)
  
  # Displaying output list
  output
  
  
}


# The function is executed
t.power.simulation(n.s1=50, mean.s1=150, sd.s1=15, n.s2=50, mean.s2=160, sd.s2=15, alpha=0.05, test.number=1000) 
