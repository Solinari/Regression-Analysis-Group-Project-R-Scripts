SQF.2012$AgeAboveNinetyNine <- factor ( with ( SQF.2012, ifelse ((age > 99),1, 0) ) );
SQF.2012$AgeBelowOneHundred <- factor ( with ( SQF.2012, ifelse ((age < 100),1, 0) ) );
m1 <- lm(frisked ~ SQF.2012$AgeBelowOneHundred, data=SQF.2012)
m2 <- lm(frisked ~ SQF.2012$AgeAboveNinetyNine, data=SQF.2012)
summary(m1)
summary(m2)