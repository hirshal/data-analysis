library(ggplot2)
library(patchwork)

#Ex 1a, creates function to evaluate MGFt and MGFe when given 3 variables

evaluate_MGF <- function(mu,sigma,t){
  
  set.seed(123)
  sims = 10000
  
  X <- rnorm(sims,mu,sigma)
  
  MGF_e <- mean(exp(t*X))
  print(c("empirical MGF:", MGF_e))
  
  MGF_t <- exp(mu*t+0.5*sigma^2*t^2)
  print(c("theoretical MGF:", MGF_t))
}

evaluate_MGF(0,1,2)

#Ex 1b plots MGF.t and MGF.e given mu, sigma, t end point

plot_MGF_X <- function(mu,sigma,tn){
  
  set.seed(123)
  
  t = seq(0, tn, 0.01)
  sims = 10000
  
  X <- rnorm(sims, mu, sigma)
  
  MGF.t <- sapply(t, function(t) exp(mu*t+0.5*sigma^2*t^2))
  MGF.e <- sapply(t, function(t) mean(exp(t*X)))
  
  mgf_theoretical <- data.frame(cbind(t,MGF.t))
  colnames(mgf_theoretical) <- c('t', 'MGF.t')
  
  mgf_empirical <- data.frame(cbind(t,MGF.e))
  colnames(mgf_empirical) <- c('t', 'MGF.e')
  
  MGF_plot = ggplot() + 
    geom_line(data = mgf_theoretical, aes(x = t, y = MGF.t), color = "black", linewidth = 1) +
    geom_line(data = mgf_empirical, aes(x = t, y = MGF.e), color = "red", linewidth = 1) +
    xlab('t') +
    ylab('Moment Generating Function')
  
  MGF_plot = MGF_plot + ggtitle("Theoretical v Empirical MGF for Normal Distribution")
  MGF_plot
}

plot_MGF_X(0,1,2)

plot_MGF_X(0,1,5)

plot_MGF_X(0,5,1)

#Ex 1c 

prove_sum <- function(mu1, mu2, sigma1, sigma2){

  set.seed(100)
  sims = 10000
  
  X = rnorm(sims, mu1, sigma1)
  Y = rnorm(sims, mu1, sigma1)
  Z = rnorm(sims, mu1 + mu2, sigma1 + sigma2)
  
  dfsum <- data.frame(
    key = factor(rep(c("X","Y"), each = sims/2)),
    value = c(X,Y))
  
  dfz <- data.frame(
    key = factor(rep("Z"), sims),
    value = Z)
  
  df <- data.frame(
    key = factor(rep(c("X+Y","Z"), each = sims)),
    value = c(X+Y,Z))
  
  sumplot <- ggplot(dfsum, aes(x = value, fill = key, color = key)) +
    geom_histogram(position = "stack", alpha = 0.5) +
    scale_color_manual(values=c("#ff0000", "#0000ff")) +
    scale_fill_manual(values=c("#ff0000", "#0000ff")) +
    ggtitle("X+Y")
  
  zplot <- ggplot(dfz, aes(x = value)) +
    geom_histogram(color = "black", fill = "yellow", alpha = 0.5) +
    ggtitle("Z")
  
  combplot <- ggplot(df, aes(x = value, fill = key, color = key)) +
    geom_histogram(aes(y=..density..), position = "identity", alpha = 0.5) +
    geom_density(alpha = 0.3) +
    scale_color_manual(values=c("#880080", "#000000")) +
    scale_fill_manual(values=c("#880080", "#ffff00")) +
    labs(title="Density Plots", x="value", y = "density") +
    theme_classic() + 
    facet_grid(key~.)
  
  sumplot + zplot + combplot

}

prove_sum(0,0,1,1)

#Ex 2a

plot_CLT <- function(a,b,n,nsim){
  
  set.seed(100)
  means <- NULL
  
  for (i in 1:nsim){
    means <- c(means, mean(runif(n,a,b)))
  }
                    
  varunif <- (((b-a)^2)/12)/n
  eunif <- (a+b)/2
  
  z <- (means-eunif)/sqrt(varunif)
  
  df <- as.data.frame(c(z, rnorm(nsim)))
  df$type <- c(rep("Means of Uniform(a,b)", nsim), rep("Normal(0,1)", nsim))
  colnames(df) <- c("Value", "Key")
    
  clt <- ggplot(df, aes(x=Value, fill=Key, color=Key)) +
    geom_histogram(aes(y=..density..),position = "identity", alpha = 0.5) +
    geom_density(alpha = 0.3) +
    scale_color_manual(values = c("#FF7800","#0087FF")) +
    scale_fill_manual(values = c("#FF7800","#0087FF")) +
    labs(title = "Normalised Means of Uniform vs Normal(0,1)", 
         x = "value", y = "density") +
    theme_classic()
  
  x <- runif(nsim,a,b)
  
  dat <- as.data.frame(c(x,means))
  dat$type <- c(rep("Uniform Sample", nsim),rep("Sample Means", nsim))
  colnames(dat) <- c("Value","Key")
  
  unifdist <- ggplot(dat,aes(x=Value, fill=Key, color=Key)) +
    geom_histogram(position = "identity", alpha = 0.5) +
    scale_color_manual(values = c("#00ff22", "#ff00dd")) +
    scale_fill_manual(values = c("#00ff22", "#ff00dd")) +
    labs(title = "Uniform Distribution vs Sample Means", x = "x", y = "count") +
    theme_classic()
  
  clt + unifdist

}

plot_CLT(2,5,500,10000)


