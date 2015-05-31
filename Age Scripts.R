summary(SQF.2012$age)

# This is the five number summary
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   # 0.00   19.00   24.00   28.77   34.00  999.00

# consider breaking this into categories based on this
# one: 0 to 17
# two: 18 to 24
# thr: 25 to 34
# fou: 35 to 99
# fiv: 100 to 999
SQF.2012$ChildToTeens <- factor ( with ( SQF.2012, ifelse((age < 18),1,0 ) ) );
SQF.2012$EighteenPlus <- factor ( with ( SQF.2012, ifelse((17 < age < 25),1,0 ) ) );
SQF.2012$TwentyToThirtyish <- factor ( with ( SQF.2012, ifelse((24 < age < 35),1,0 ) ) );
SQF.2012$ThirtyFivePlus <- factor ( with ( SQF.2012, ifelse ((34 < age < 100),1, 0) ) );
SQF.2012$HundPlus <- factor ( with ( SQF.2012, ifelse ((age > 99),1, 0) ) );


#Frisks
ChildToTeensFrisk <- lm(frisked ~ SQF$ChildToTeens, data=SQF.2012)
EighteenPlusFrisk <- lm(frisked ~ SQF$EighteenPlus, data=SQF.2012)
TwentyToThirtyishFrisk <- lm(frisked ~ SQF$TwentyToThirtyish, data=SQF.2012)
ThirtyFivePlusFrisk <- lm(frisked ~ SQF.2012$ThirtyFivePlus, data=SQF.2012)
HundPlusFrisk <- lm(frisked ~ SQF.2012$HundPlus, data=SQF.2012)

#Searches
ChildToTeensSearch <- lm(searched ~ SQF.2012$ChildToTeens, data=SQF.2012)
EighteenPlusSearch <- lm(searched ~ SQF.2012$EighteenPlus, data=SQF.2012)
TwentyToThirtyishSearch <- lm(searched ~ SQF.2012$TwentyToThirtyish, data=SQF.2012)
ThirtyFivePlusSearch <- lm(searched ~ SQF.2012$ThirtyFivePlus, data=SQF.2012)
HundPlusSearch <- lm(searched ~ SQF.2012$HundPlus, data=SQF.2012)

#Contraband
ChildToTeensContra <- lm(contrabn ~ SQF.2012$ChildToTeens, data=SQF.2012)
EighteenPlusContra <- lm(contrabn ~ SQF.2012$EighteenPlus, data=SQF.2012)
TwentyToThirtyishContra <- lm(contrabn ~ SQF.2012$TwentyToThirtyish, data=SQF.2012)
ThirtyFivePlusContra <- lm(contrabn ~ SQF.2012$ThirtyFivePlus, data=SQF.2012)
HundPlusContra <- lm(contrabn ~ SQF.2012$HundPlus, data=SQF.2012)

#Arrests
ChildToTeensArrest <- lm(arstmade ~ SQF.2012$ChildToTeens, data=SQF.2012)
EighteenPlusArrest <- lm(arstmade ~ SQF.2012$EighteenPlus, data=SQF.2012)
TwentyToThirtyishArrest < - lm(arstmade ~ SQF.2012$TwentyToThirtyish, data=SQF.2012)
ThirtyFivePlusArrest <- lm(arstmade ~ SQF.2012$ThirtyFivePlus, data=SQF.2012)
HundPlusArrest <- lm(arstmade ~ SQF.2012$HundPlus, data=SQF.2012)

#Summons 
ChildToTeensSummon <- lm(sumissue ~ SQF.2012$ChildToTeens, data=SQF.2012)
EighteenPlusSummon <- lm(sumissue ~ SQF.2012$EighteenPlus, data=SQF.2012)
TwentyToThirtyishSummon <- lm(sumissue ~ SQF.2012$TwentyToThirtyish, data=SQF.2012)
ThirtyFivePlusSummon <- lm(sumissue ~ SQF.2012$ThirtyFivePlus, data=SQF.2012)
HundPlusSummon <- lm(sumissue ~ SQF.2012$HundPlus, data=SQF.2012)

# make the weapon variable
attach(SQF.2012)
SQF.2012$weaponfound <- pistol+riflshot+asltweap+knifcuti+machgun+othrweap

#Weapons
ChildToTeensWeapon <- lm(SQF.2012$weaponfound ~ SQF$ChildToTeens, data=SQF.2012)
EighteenPlusWeapon <- lm(SQF.2012$weaponfound ~ SQF$EighteenPlus, data=SQF.2012)
TwentyToThirtyishWeapon <- lm(SQF.2012$weaponfound ~ SQF$TwentyToThirtyish, data=SQF.2012)
ThirtyFivePlusWeapon <- lm(SQF.2012$weaponfound ~ SQF.2012$ThirtyFivePlus, data=SQF.2012)
HundPlusWeapon <- lm(SQF.2012$weaponfound ~ SQF.2012$HundPlus, data=SQF.2012)

#Summary - Frisk
summary(ChildToTeensFrisk)
summary(EighteenPlusFrisk)
summary(EighteenPlusFrisk)
summary(ThirtyFivePlusFrisk)
summary(HundPlusFrisk)

##Summary - Search
summary(ChildToTeensSearch)
summary(EighteenPlusSearch)
summary(TwentyToThirtyishSearch)
summary(ThirtyFivePlusSearch)
summary(HundPlusSearch)

#Summary - Contraband
summary(ChildToTeensContra)
summary(EighteenPlusContra)
summary(TwentyToThirtyishContra)
summary(ThirtyFivePlusContra)
summary(HundPlusContra)

#Summary - Arrest
summary(ChildToTeensArrest)
summary(EighteenPlusArrest)
summary(TwentyToThirtyishArrest)
summary(ThirtyFivePlusArrest)
summary(HundPlusArrest)

#Summary - Summons
summary(ChildToTeensSummon)
summary(EighteenPlusSummon)
summary(TwentyToThirtyishSummon)
summary(ThirtyFivePlusSummon)
summary(HundPlusSummon)

#Summary - Weapons
summary(ChildToTeensWeapon)
summary(EighteenPlusWeapon)
summary(TwentyToThirtyishWeapon)
summary(ThirtyFivePlusWeapon)
summary(HundPlusWeapon)

# This doesn't work for the model I am approaching*
# library(MASS)
# step1 <- stepAIC(ThirtyFivePlusFrisk, direction="backward")
# step2 <- stepAIC(HundPlusFrisk, direction="backward")
# step3 <- stepAIC(ThirtyFivePlusSearch, direction="backward")
# step4 <- stepAIC(HundPlusSearch, direction="backward")
# step5 <- stepAIC(ThirtyFivePlusContra, direction="backward")
# step6 <- stepAIC(HundPlusContra, direction="backward")
# step7 <- stepAIC(ThirtyFivePlusArrest, direction="backward")
# step8 <- stepAIC(HundPlusArrest, direction="backward")
# step9 <- stepAIC(ThirtyFivePlusSummon, direction="backward")
# step10 <- stepAIC(HundPlusSummon, direction="backward")
# step11 <- stepAIC(ThirtyFivePlusWeapon, direction="backward")
# step12 <- stepAIC(HundPlusWeapon, direction="backward")
