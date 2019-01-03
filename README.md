# Last words of death row inmates
EDA and sentiment analysis of the last words of death row inmates

# Links
This repository: https://github.com/tastyCanOfMalk/inmate.last.words

Original data: https://www.kaggle.com/mykhe1097/last-words-of-death-row-inmates/home

# Data
The dataset consists of 545 observations with 21 variables. They are: 
- Execution: The order of execution, numeric. 
- LastName: Last name of the offender, character. 
- FirstName: First name of the offender, character. 
- TDCJNumber: TDCJ Number of the offender, numeric. 
- Age: Age of the offender, numeric. 
- Race: Race of the offender, categorical : Black, Hispanic, White, Other. 
- CountyOfConviction: County of conviction, character. 
- AgeWhenReceived: Age of offender when received, numeric. 
- EducationLevel: Education level of offender, numeric. 
- Native County: Native county of offender, categorical : 0 = Within Texas, 1= Outside Texas. 
- PreviousCrime : Whether the offender committed any crime before, categorical: 0= No, 1= Yes. 
- Codefendants: Number of co-defendants, numeric. 
- NumberVictim: Number of victims, numeric. 
- WhiteVictim, HispanicVictim, BlackVictim, VictimOtherRace. FemaleVictim, MaleVictim: Number of victims with specified demographic features, numeric. 
- LastStatement: Last statement of offender, character.

# Possible analyses
 * Sentiment differences between inmates with more victims
 * Differences in sentiment between races
 * Sentiment of inmate with most codefendents