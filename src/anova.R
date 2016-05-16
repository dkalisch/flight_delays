# Load required libraries
library(psych) # For descriptive summary
library(car) # For leveneTest

# Ensure you got the data from the db
if (!exists("delaysData")) {
  source("src/delaysDescriptive.R")
}

# Global settings
alpha <- 0.05 # The error we are willing to accept
formula <- "DepartureTime~Carrier"


# ANOVA
## Calculate ANOVA results
fit <- lm(data = delaysData, ArrDelayMinutes~Carrier) # Get a linear model
delaysData.aov <- aov(fit) # ANOVA results
delaysData.aov.summary <- summary(delaysData.aov) # Summarized ANOVA results
delaysData.tukey <- TukeyHSD(delaysData.aov) # Tukey-Kramer procedure

## Check if assumptions are met
### Normality
delaysData.residuals <- residuals(fit)
delaysData.normal <- data.frame(skew = skew(delaysData.residuals),
                                kurt = kurtosi(delaysData.residuals))

## Homogenity of variance
delaysData.levene <- leveneTest(fit)

# Get values for further calculations
## Get the degrees of freedom
df <- anova(fit)[, "Df"]
names(df) <- c("between", "within")

## Get the f values
delaysData.f.crit <- qf(alpha, df["between"], df["within"], lower.tail = FALSE)
delaysData.f.value <- delaysData.aov.summary[[1]]$F[1]

## Plot ANOVA results
### Plot Boxplot of the data set
bp <- ggplot(delaysData, aes(x = Carrier, y = ArrDelayMinutes)) +
  stat_boxplot(geom = "errorbar") + # Add error bars to the boxplot
  geom_boxplot() + # Add boxplot
  labs(y = "DelayTime", x = "Carriers") # Tify up the axis title

### Save plot
ggsave("img/ANOVA/boxplot.pdf", bp)

## Plot F-distribution
### Display settings
ncp <- 0 # Non centrality parameter
frameEmpty <- "black" # Color for the empty frame
areaEmpty <- "white" # Color for the empty area
frameH0 <- "green4" # Color for the H0 frame
areaH0 <- "green3" # Color for the H0 area
frameH1 <- "red4" # Color for the H1 frame
areaH1 <- "red2" # Color for the H1 area

### Distribution specific settings
df1 <- df[1] # Degree of freedom first parameter
df2 <- df[2] # Degree of freedom second parameter
length <- 500 # number of elements

### Data preperation
x <- seq(from = 1, to = delaysData.f.value+2, length = length) # Set vector range 
dd <- data.frame(x = seq(from = 1, to = delaysData.f.value+2, length = length),  # Create data frame
                 y = df(x = x, df1 = df1, df2 = df2, ncp = 5))

### Create F-distribution plot
pf <- ggplot(data = dd) + # Create the plot
  labs(y = "Relative frequency", x = "F-values") + # Tidy up the axis title
  geom_area(aes(x = x, y = y),
            color = frameH0, fill = areaH0) + # Add the H0 area
  geom_area(data = subset(dd, x > delaysData.f.crit),
            aes(x = x, y = y),
            fill = areaH1, color = frameH1) + # Add the H1 area
  geom_vline(xintercept = delaysData.f.crit, colour = frameH1, linetype = "longdash") + # Add the F-critical value line
  geom_vline(xintercept = delaysData.f.value, colour = "black", linetype = "dotted") + # Add the F-value line
  scale_x_continuous(breaks = sort(round(c(seq(from = min(dd$x), # Add tick marks for the F values
                                               to = round(max(dd$x),0),
                                               by = 10), delaysData.f.crit, delaysData.f.value),2))) +
  annotate("text", y = .6, x = delaysData.f.value + 1,
           label = paste("Pr(>F) = ", round(delaysData.aov.summary[[1]]$Pr[1],3))) + # Add p-value to plot

  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) # Rotate x axis labels for better readability

### Save plot
ggsave("img/ANOVA/fDistribution.pdf", pf)

