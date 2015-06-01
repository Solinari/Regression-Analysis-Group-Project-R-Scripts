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
ChildToTeensFrisk <- glm(frisked ~ SQF.2012$ChildToTeens, data=SQF.2012, family=binomial(link="logit"));
EighteenPlusFrisk <- glm(frisked ~ SQF.2012$EighteenPlus, data=SQF.2012, family=binomial(link="logit"));
TwentyToThirtyishFrisk <- glm(frisked ~ SQF.2012$TwentyToThirtyish, data=SQF.2012, family=binomial(link="logit"));
ThirtyFivePlusFrisk <- glm(frisked ~ SQF.2012$ThirtyFivePlus, data=SQF.2012, family=binomial(link="logit"));
HundPlusFrisk <- glm(frisked ~ SQF.2012$HundPlus, data=SQF.2012, family=binomial(link="logit"));

#Searches
ChildToTeensSearch <- glm(searched ~ SQF.2012$ChildToTeens, data=SQF.2012, family=binomial(link="logit"));
EighteenPlusSearch <- glm(searched ~ SQF.2012$EighteenPlus, data=SQF.2012, family=binomial(link="logit"));
TwentyToThirtyishSearch <- glm(searched ~ SQF.2012$TwentyToThirtyish, data=SQF.2012, family=binomial(link="logit"));
ThirtyFivePlusSearch <- glm(searched ~ SQF.2012$ThirtyFivePlus, data=SQF.2012, family=binomial(link="logit"));
HundPlusSearch <- glm(searched ~ SQF.2012$HundPlus, data=SQF.2012, family=binomial(link="logit"));

#Contraband
ChildToTeensContra <- glm(contrabn ~ SQF.2012$ChildToTeens, data=SQF.2012, family=binomial(link="logit"));
EighteenPlusContra <- glm(contrabn ~ SQF.2012$EighteenPlus, data=SQF.2012, family=binomial(link="logit"));
TwentyToThirtyishContra <- glm(contrabn ~ SQF.2012$TwentyToThirtyish, data=SQF.2012, family=binomial(link="logit"));
ThirtyFivePlusContra <- glm(contrabn ~ SQF.2012$ThirtyFivePlus, data=SQF.2012, family=binomial(link="logit"));
HundPlusContra <- glm(contrabn ~ SQF.2012$HundPlus, data=SQF.2012, family=binomial(link="logit"));

#Arrests
ChildToTeensArrest <- glm(arstmade ~ SQF.2012$ChildToTeens, data=SQF.2012, family=binomial(link="logit"));
EighteenPlusArrest <- glm(arstmade ~ SQF.2012$EighteenPlus, data=SQF.2012, family=binomial(link="logit"));
TwentyToThirtyishArrest <- glm(arstmade ~ SQF.2012$TwentyToThirtyish, data=SQF.2012, family=binomial(link="logit"));
ThirtyFivePlusArrest <- glm(arstmade ~ SQF.2012$ThirtyFivePlus, data=SQF.2012, family=binomial(link="logit"));
HundPlusArrest <- glm(arstmade ~ SQF.2012$HundPlus, data=SQF.2012, family=binomial(link="logit"));

#Summons 
ChildToTeensSummon <- glm(sumissue ~ SQF.2012$ChildToTeens, data=SQF.2012, family=binomial(link="logit"));
EighteenPlusSummon <- glm(sumissue ~ SQF.2012$EighteenPlus, data=SQF.2012, family=binomial(link="logit"));
TwentyToThirtyishSummon <- glm(sumissue ~ SQF.2012$TwentyToThirtyish, data=SQF.2012, family=binomial(link="logit"));
ThirtyFivePlusSummon <- glm(sumissue ~ SQF.2012$ThirtyFivePlus, data=SQF.2012, family=binomial(link="logit"));
HundPlusSummon <- glm(sumissue ~ SQF.2012$HundPlus, data=SQF.2012, family=binomial(link="logit"));

# make the weapon variable
attach(SQF.2012)
SQF.2012$weaponfound <- pistol+riflshot+asltweap+knifcuti+machgun+othrweap;

#Weapons
ChildToTeensWeapon <- lm(SQF.2012$weaponfound ~ SQF.2012$ChildToTeens, data=SQF.2012);
EighteenPlusWeapon <- lm(SQF.2012$weaponfound ~ SQF.2012$EighteenPlus, data=SQF.2012);
TwentyToThirtyishWeapon <- lm(SQF.2012$weaponfound ~ SQF.2012$TwentyToThirtyish, data=SQF.2012);
ThirtyFivePlusWeapon <- lm(SQF.2012$weaponfound ~ SQF.2012$ThirtyFivePlus, data=SQF.2012);
HundPlusWeapon <- lm(SQF.2012$weaponfound ~ SQF.2012$HundPlus, data=SQF.2012);

#general summaries
summary(ChildToTeens);
summary(EighteenPlus);
summary(TwentyToThirtyish);
summary(ThirtyFivePlus);
summary(HundPlus);

#Summary - Frisk
summary(ChildToTeensFrisk);
summary(EighteenPlusFrisk);
summary(TwentyToThirtyishFrisk);
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
 library(MASS)
# step1 <- stepAIC(ThirtyFivePlusFrisk, direction="both")
# step2 <- stepAIC(HundPlusFrisk, direction="both")
# step3 <- stepAIC(ThirtyFivePlusSearch, direction="both")
# step4 <- stepAIC(HundPlusSearch, direction="both")
# step5 <- stepAIC(ThirtyFivePlusContra, direction="both")
# step6 <- stepAIC(HundPlusContra, direction="both")
# step7 <- stepAIC(ThirtyFivePlusArrest, direction="both")
# step8 <- stepAIC(HundPlusArrest, direction="both")
# step9 <- stepAIC(ThirtyFivePlusSummon, direction="both")
# step10 <- stepAIC(HundPlusSummon, direction="both")
stepChil <- stepAIC(ChildToTeensWeapon, direction="both")
stepEigh <- stepAIC(EighteenPlusWeapon, direction="both")
stepTwen <- stepAIC(TwentyToThirtyishWeapon, direction="both")
stepThir <- stepAIC(ThirtyFivePlusWeapon, direction="both")
stepHund <- stepAIC(HundPlusWeapon, direction="both")
