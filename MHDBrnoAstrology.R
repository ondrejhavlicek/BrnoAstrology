#https://pbs.twimg.com/media/DNiDegAXUAAGlQv.jpg

library(tidyverse)
library(forcats)

n <- 52000
observedCats <- c("ryby", "byk", "beran", "blizenci", "lev", "rak", "panna", "kozoroh", "strelec", "vahy", "vodnar", "stir")
months <-       c("MAR", "MAY", "APR", "JUN", "AUG", "JUL", "SEP", "JAN", "DEC", "OCT", "FEB", "NOV")
monthLevels <-  c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
observedPct <- c(9.59, 8.75, 8.65, 8.57, 8.52, 8.45, 8.40, 8.32, 7.96, 7.91, 7.6, 7.26)/100

magic <- tibble(Sign = observedCats, ObservedPct = observedPct, Observed = ObservedPct*n, Month = parse_factor(months, levels=monthLevels))
magic <- magic %>% arrange(Month)
magic$ExpectedUnif <- 1/12

natality1993 <- c(9885, 9632, 10790, 10472, 10933, 10819, 11100, 10613, 10170, 9274, 8707, 8630)
natality1993rel <- natality1993/sum(natality1993)
magic$Natality <- natality1993rel

natalitySign <- rep(NA, 12)
natalitySign[1] <- magic$Natality[1]*2/3 + magic$Natality[12]*1/3
for(i in 2:12){
  natalitySign[i] <- magic$Natality[i]*2/3 + magic$Natality[i-1]*1/3
}
magic$NatalitySign <- natalitySign


with(magic, chisq.test(x=Observed, p=ExpectedUnif) )
with(magic, chisq.test(x=Observed, p=NatalitySign) )
        
magicLong <- magic %>% select(Sign, ObservedPct, NatalitySign) %>% gather(key="ObservedVsExpected", value="Percentage", -Sign)
magicLong$ObservedVsExpected <- factor(magicLong$ObservedVsExpected, labels=c("Expected", "Observed"))
   
magicLong %>% ggplot(., aes(y=Percentage, x=Sign, linetype=ObservedVsExpected, color=ObservedVsExpected, group=ObservedVsExpected)) +
  geom_point() + geom_line() + theme(legend.position = "top")
