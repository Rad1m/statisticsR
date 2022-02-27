---
title: "NBCC2 GT21 AccuPlots Analysis"
output: html_notebook
---
Read in CSV file  
```{r}
library(readxl)
dataInput <- read_excel("Accu-Plots_CSV.xlsx", col_names = TRUE, .name_repair = "unique", col_types = c("skip", "numeric", "skip", "numeric", "skip", "numeric", "skip", "numeric", "skip", "numeric", "skip", "numeric", "skip", "numeric"))
str(dataInput)
```

Define function for calculating statistical mode
```{r}
# Create the function.
Mode = function(x){
    ta = table(x)
    tam = max(ta)
    if (all(ta == tam))
         mod = NA
    else
         if(is.numeric(x))
    mod = as.numeric(names(ta)[ta == tam])
    else
         mod = names(ta)[ta == tam]
    return(mod)
}
```

Calculate basic statistics
```{r}
NBCC2GT21.median <- median(dataInput$`NBCC2-GT21[K]`, na.rm = TRUE)
NBCC2GT22.median <- median(dataInput$`NBCC2-GT22[K]`, na.rm = TRUE)
KLNGT31.median <- median(dataInput$`KLN-GT31[K]`, na.rm = TRUE)

NBCC2GT21.mean <- mean(dataInput$`NBCC2-GT21[K]`, na.rm = TRUE)
NBCC2GT22.mean <- mean(dataInput$`NBCC2-GT22[K]`, na.rm = TRUE)
KLNGT31.mean <- mean(dataInput$`KLN-GT31[K]`, na.rm = TRUE)

NBCC2GT21.mode <- Mode(round(dataInput$`NBCC2-GT21[K]`,0))
NBCC2GT22.mode <- Mode(round(dataInput$`NBCC2-GT22[K]`,0))
KLNGT31.mode <- Mode(round(dataInput$`KLN-GT31[K]`,0))
```

Plot histogram in R using basic libraries
```{r}
hist(dataInput$`NBCC2-GT21[K]`, col=rgb(1,0,0,1), main="THG2 GT26 U4 C-IN", xlab="THG2",xlim=c(1700,1790), breaks = 100,prob = TRUE)
hist(dataInput$`NBCC2-GT22[K]`, col=rgb(0,1,1,0.5), xlim=c(1700,1790), breaks = 100, prob = TRUE, add=T)
hist(dataInput$`KLN-GT31[K]`, col=rgb(0,1,0,0.5), xlim=c(1700,1790), breaks = 100 ,prob = TRUE, add=T)
box()
```

Plot hostogram in R using GGPLOT2
```{r}
library(ggplot2)
Bangkok <- ggplot(dataInput, aes(x = dataInput$`NBCC2-GT21[K]`)) +
        geom_histogram(aes(y = ..count..*1.6), fill = "orange" , binwidth = 10) +
        scale_x_continuous(name = "NBCC2 THG2", limits=c(1700, 1790)) +
        scale_y_continuous(name = "Time[Hrs]") +
        ggtitle("GT26 U4 THG2") +
        geom_vline(xintercept = NBCC2GT21.median, size = 1, colour = "black", linetype = "dashed")
Kolen <-ggplot(dataInput, aes(x = dataInput$`KLN-GT31[K]`)) +
        geom_histogram(aes(y = ..count..*1.6), fill = "dark blue" , binwidth = 10) +
        scale_x_continuous(name = "Koeln-Niehl THG2", limits=c(1700, 1790)) +
        scale_y_continuous(name = "Time[Hrs]") +
        ggtitle("GT26 U4 THG2") +
        geom_vline(xintercept = KLNGT31.median, size = 1, colour = "black", linetype = "dashed")
Bangkok
Kolen
```

Plot overlaping histograms for Engines
```{r}
library(ggplot2)

NBCC2GT21 <- data.frame(length = dataInput$`NBCC2-GT21[K]`)
NBCC2GT22 <- data.frame(length = dataInput$`NBCC2-GT22[K]`)
KLNGT31 <- data.frame(length = dataInput$`KLN-GT31[K]`)

NBCC2GT21$THG2 <- 'Bangkok-GT21'
NBCC2GT22$THG2 <- 'Bangkok-GT22'
KLNGT31$THG2 <- 'Koeln-Niehl-N3'

GT26 <- rbind(NBCC2GT21, NBCC2GT22, KLNGT31)

ggplot(GT26, aes(length, fill = THG2)) + 
        geom_histogram(aes(y = ..count..*1.6),alpha = 0.8, position = 'identity', bins = 20) +
        scale_x_continuous(name = "THG2", limits=c(1700, 1780)) +
        scale_y_continuous(name = "Time[Hrs]") +
        ggtitle("GT26 U4 THG2") +
  annotate("text", x=as.numeric(NBCC2GT21.mode), y= 12500, label= floor(NBCC2GT21.mode)) +
  annotate("text", x=as.numeric(NBCC2GT22.mode), y= 11500, label= floor(NBCC2GT22.mode)) +
  annotate("text", x=as.numeric(KLNGT31.mode), y= 8500, label= floor(KLNGT31.mode))
```
