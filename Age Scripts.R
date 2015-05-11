SQF.2012$AgeAboveNinetyNine <- factor ( with ( SQF.2012, ifelse ((age > 99),1, 0) ) );
SQF.2012$AgeBelowOneHundred <- factor ( with ( SQF.2012, ifelse ((age < 100),1, 0) ) );
mLowFrisk <- lm(frisked ~ SQF.2012$AgeBelowOneHundred, data=SQF.2012)
mHiFrisk <- lm(frisked ~ SQF.2012$AgeAboveNinetyNine, data=SQF.2012)
mLowSearch <- lm(searched ~ SQF.2012$AgeBelowOneHundred, data=SQF.2012)
mHiSearch <- lm(searched ~ SQF.2012$AgeAboveNinetyNine, data=SQF.2012)
mLowContra <- lm(contrabn ~ SQF.2012$AgeBelowOneHundred, data=SQF.2012)
mHiContra <- lm(contrabn ~ SQF.2012$AgeAboveNinetyNine, data=SQF.2012)
mLowArrest <- lm(arstmade ~ SQF.2012$AgeBelowOneHundred, data=SQF.2012)
mHiArrest <- lm(arstmade ~ SQF.2012$AgeAboveNinetyNine, data=SQF.2012)
mLowSummon <- lm(sumissue ~ SQF.2012$AgeBelowOneHundred, data=SQF.2012)
mHiSummon <- lm(sumissue ~ SQF.2012$AgeAboveNinetyNine, data=SQF.2012)

attach(SQF.2012)
SQF.2012$weaponfound <- pistol+riflshot+asltweap+knifcuti+machgun+othrweap

mLowWeapon <- lm(SQF.2012$weaponfound ~ SQF.2012$AgeBelowOneHundred, data=SQF.2012)
mHiWeapon <- lm(SQF.2012$weaponfound ~ SQF.2012$AgeAboveNinetyNine, data=SQF.2012)

summary(mLowFrisk)
summary(mHiFrisk)
summary(mLowSearch)
summary(mHiSearch)
summary(mLowContra)
summary(mHiContra)
summary(mLowArrest)
summary(mHiArrest)
summary(mLowSummon)
summary(mHiSummon)
summary(mLowWeapon)
summary(mHiWeapon)