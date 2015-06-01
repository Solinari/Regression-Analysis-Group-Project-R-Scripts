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

SQF.2012$ChildToTeens <- factor ( with ( SQF.2012, ifelse( (age < 18),1,0 ) ) );
SQF.2012$EighteenPlus <- factor ( with ( SQF.2012, ifelse( ( (17 < age) & (age < 25) ),1,0 ) ) );
SQF.2012$TwentyToThirtyish <- factor ( with ( SQF.2012, ifelse( ( (24 < age) & (age < 35) ),1,0 ) ) );
SQF.2012$ThirtyFivePlus <- factor ( with ( SQF.2012, ifelse ( ( (34 < age) & (age < 100) ),1, 0) ) );
SQF.2012$HundPlus <- factor ( with ( SQF.2012, ifelse ((age > 99),1, 0) ) );


#Frisks
ChildToTeensFrisk <- glm(frisked ~ SQF.2012$ChildToTeens, data=SQF.2012, family=binomial());
EighteenPlusFrisk <- glm(frisked ~ SQF.2012$EighteenPlus, data=SQF.2012, family=binomial());
TwentyToThirtyishFrisk <- glm(frisked ~ SQF.2012$TwentyToThirtyish, data=SQF.2012, family=binomial());
ThirtyFivePlusFrisk <- glm(frisked ~ SQF.2012$ThirtyFivePlus, data=SQF.2012, family=binomial());
HundPlusFrisk <- glm(frisked ~ SQF.2012$HundPlus, data=SQF.2012, family=binomial());

#Searches
ChildToTeensSearch <- glm(searched ~ SQF.2012$ChildToTeens, data=SQF.2012, family=binomial());
EighteenPlusSearch <- glm(searched ~ SQF.2012$EighteenPlus, data=SQF.2012, family=binomial());
TwentyToThirtyishSearch <- glm(searched ~ SQF.2012$TwentyToThirtyish, data=SQF.2012, family=binomial());
ThirtyFivePlusSearch <- glm(searched ~ SQF.2012$ThirtyFivePlus, data=SQF.2012, family=binomial());
HundPlusSearch <- glm(searched ~ SQF.2012$HundPlus, data=SQF.2012, family=binomial());

#Contraband
ChildToTeensContra <- glm(contrabn ~ SQF.2012$ChildToTeens, data=SQF.2012, family=binomial());
EighteenPlusContra <- glm(contrabn ~ SQF.2012$EighteenPlus, data=SQF.2012, family=binomial());
TwentyToThirtyishContra <- glm(contrabn ~ SQF.2012$TwentyToThirtyish, data=SQF.2012, family=binomial());
ThirtyFivePlusContra <- glm(contrabn ~ SQF.2012$ThirtyFivePlus, data=SQF.2012, family=binomial());
HundPlusContra <- glm(contrabn ~ SQF.2012$HundPlus, data=SQF.2012, family=binomial());

#Arrests
ChildToTeensArrest <- glm(arstmade ~ SQF.2012$ChildToTeens, data=SQF.2012, family=binomial());
EighteenPlusArrest <- glm(arstmade ~ SQF.2012$EighteenPlus, data=SQF.2012, family=binomial());
TwentyToThirtyishArrest <- glm(arstmade ~ SQF.2012$TwentyToThirtyish, data=SQF.2012, family=binomial());
ThirtyFivePlusArrest <- glm(arstmade ~ SQF.2012$ThirtyFivePlus, data=SQF.2012, family=binomial());
HundPlusArrest <- glm(arstmade ~ SQF.2012$HundPlus, data=SQF.2012, family=binomial());

#Summons 
ChildToTeensSummon <- glm(sumissue ~ SQF.2012$ChildToTeens, data=SQF.2012, family=binomial());
EighteenPlusSummon <- glm(sumissue ~ SQF.2012$EighteenPlus, data=SQF.2012, family=binomial());
TwentyToThirtyishSummon <- glm(sumissue ~ SQF.2012$TwentyToThirtyish, data=SQF.2012, family=binomial());
ThirtyFivePlusSummon <- glm(sumissue ~ SQF.2012$ThirtyFivePlus, data=SQF.2012, family=binomial());
HundPlusSummon <- glm(sumissue ~ SQF.2012$HundPlus, data=SQF.2012, family=binomial());

# make the weapon variable
attach(SQF.2012)
SQF.2012$weaponfound <- pistol+riflshot+asltweap+knifcuti+machgun+othrweap;

#Weapons
ChildToTeensWeapon <- glm(SQF.2012$weaponfound ~ SQF.2012$ChildToTeens, data=SQF.2012, family=binomial());
EighteenPlusWeapon <- glm(SQF.2012$weaponfound ~ SQF.2012$EighteenPlus, data=SQF.2012, family=binomial());
TwentyToThirtyishWeapon <- glm(SQF.2012$weaponfound ~ SQF.2012$TwentyToThirtyish, data=SQF.2012, family=binomial());
ThirtyFivePlusWeapon <- glm(SQF.2012$weaponfound ~ SQF.2012$ThirtyFivePlus, data=SQF.2012, family=binomial());
HundPlusWeapon <- glm(SQF.2012$weaponfound ~ SQF.2012$HundPlus, data=SQF.2012, family=binomial());

#general summaries
summary(ChildToTeens);
summary(EighteenPlus);
summary(TwentyToThirtyish);
summary(ThirtyFivePlus);
summary(HundPlus);

#Summary - Frisk
summary(ChildToTeensFrisk);
summary(EighteenPlusFrisk);
summary(EighteenPlusFrisk);
summary(ThirtyFivePlusFrisk);
summary(HundPlusFrisk);

##Summary - Search
summary(ChildToTeensSearch);
summary(EighteenPlusSearch);
summary(TwentyToThirtyishSearch);
summary(ThirtyFivePlusSearch);
summary(HundPlusSearch);

#Summary - Contraband
summary(ChildToTeensContra);
summary(EighteenPlusContra);
summary(TwentyToThirtyishContra);
summary(ThirtyFivePlusContra);
summary(HundPlusContra);

#Summary - Arrest
summary(ChildToTeensArrest);
summary(EighteenPlusArrest);
summary(TwentyToThirtyishArrest);
summary(ThirtyFivePlusArrest);
summary(HundPlusArrest);

#Summary - Summons
summary(ChildToTeensSummon);
summary(EighteenPlusSummon);
summary(TwentyToThirtyishSummon);
summary(ThirtyFivePlusSummon);
summary(HundPlusSummon);

#Summary - Weapons
summary(ChildToTeensWeapon);
summary(EighteenPlusWeapon);
summary(TwentyToThirtyishWeapon);
summary(ThirtyFivePlusWeapon);
summary(HundPlusWeapon);

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
