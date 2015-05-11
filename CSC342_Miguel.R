other <- SQF.2012$othpers
# Frisked and Other Persons Questioned
frisked_other <- lm(SQF.2012$frisked ~ other)
summary(frisked_other)

plot(resid(frisked_other))

# Searched and Other Persons Questioned
searched_other <- lm(SQF.2012$searched ~ other)
summary(searched_other)

resid(searched_other)

# Arrest Made and Other Persons Questioned
arrest_made_other <- lm(SQF.2012$arstmade ~ other)
summary(arrest_made_other)

resid(arrest_made_other)

# Summons Issued and Other Persons Questioned
summons_issued_other <- lm(SQF.2012$sumissue ~ other)
summary(summons_issued_other)

resid(summons_issued_other)

# Combine Weapons
attach(SQF.2012)
SQF.2012$weapon_found<- pistol+riflshot+asltweap+knifcuti+machgun+othrweap

# Weapon Found and Other Persons Questioned
weapon_found_other <- lm(SQF.2012$weaponfound ~ other)
summary(weapon_found_other)

resid(weapon_found_other)

# Contraband Found and Other Persons Questioned
contraband_found_other <- lm(SQF.2012$contrabn ~ other)
summary(contraband_found_other)

resid(contraband_found_other)

# counting precincts

w = table(precinct) # creating a table

class(w) # checking the class of variable w

t = as.data.frame(w) # converting table to a data frame
t # printing data frame of table

# easier way to count > <

install.packages('plyr') # install package plyr
library(plyr) # open package plyr
count(SQF.2012, 'pct') # function count(data_set, variable)

# Frisked and Precinct
frisked_precinct <- lm(SQF.2012$frisked ~ precinct)
summary(frisked_precinct)

resid(frisked_precinct)

# Searched and Precinct
searched_precinct <- lm(SQF.2012$searched ~ precinct)
summary(searched)

resid(searched_precinct)

# Arrest Made and Precinct
arrest_made_precinct <- lm(SQF.2012$arstmade ~ precinct)
summary(arrest_made_other)

resid(arrest_made_precinct)

# Summons Issued and Precinct
summons_issued_precinct <- lm(SQF.2012$sumissue ~ precinct)
summary(summons_issued_precinct)

resid(summons_issued_precinct)

# Weapon Found and Other Precinct
weapon_found_precinct <- lm(SQF.2012$weaponfound ~ precinct)
summary(weapon_found_precinct)

resid(weapon_found_precinct)

# contraband Found and Precinct
contraband_found_precinct <- lm(SQF.2012$contrabn ~ precinct)
summary(contraband_found_precinct)

resid(contraband_found_precinct)

# Frisked and Precinct * Other Persons Questioned
frisked_precinct_other <- lm(SQF.2012$frisked ~ precinct * other)
summary(frisked_precinct_other)

drop1(frisked_precinct_other)

# Searched and Precinct * Other Persons Questioned
searched_precinct_other <- lm(SQF.2012$searched ~ precinct * other)
summary(searched_precinct_other)

drop1(searched_precinct_other)

# Arrest Made and Precinct * Other Persons Questioned
arrest_made_precinct_other <- lm(SQF.2012$arstmade ~ precinct * other)
summary(arrest_made_precinct_other)

drop1(arrest_made_precinct_other)

# Summons Issued and Precinct * Other Persons Questioned
summons_issued_precinct_other <- lm(SQF.2012$sumissue ~ precinct * other)
summary(summons_issued_precinct_other)

drop1(summons_issued_precinct_other)

# Weapon Found and Other Precinct * Other Persons Questioned
weapon_found_precinct_other <- lm(SQF.2012$weaponfound ~ precinct * other)
summary(weapon_found_precinct_other)

drop1(weapon_found_precinct_other)

# contraband Found and Precinct * Other Persons Questioned
contraband_found_precinct_other <- lm(SQF.2012$contrabn ~ precinct * other)
summary(contraband_found_precinct_other)

drop1(contraband_found_precinct_other)
