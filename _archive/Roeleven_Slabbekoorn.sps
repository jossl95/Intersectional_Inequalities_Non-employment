* Encoding: UTF-8.
* Roeleven & Slabbekoorn - Intersectionele sekse- en herkomstverschillen in niet-werken:
    Een verklaring op basis van sociaal kapitaal.
**********************************************Working Directory*************************************************

* Set working directory .
CD 'U:\Scriptie\Data' .

* Import datasets .
    * Background variables 2018 .
GET FILE = 'avars_201810_EN_1.0p.sav'
    /KEEP nomem_encr geslacht herkomstgroep leeftijd aantalki oplcat .
DATASET NAME avars1810 .

    * Background variables 2019 .
GET FILE = 'avars_201910_EN_1.0p.sav'
    /KEEP nomem_encr belbezig .
DATASET NAME avars1910 .

    * Leisure & Integration 2018 .
GET FILE = 'cs18k_EN_1.0p.sav'
    /KEEP nomem_encr cs18k294 cs18k295 cs18k296 cs18k297 cs18k298 
        cs18k327 cs18k338 cs18k349 cs18k360 cs18k371 .
DATASET NAME integration .



**********************************************Creating Variables for analysis************************************.
DATASET ACTIVATE avars1810 .
SORT CASES BY nomem_encr (A) .

*************Sex variable*************.
* Create dummy variable for sex (1 = female) .
COMPUTE female = geslacht - 1 .
VALUE LABELS female 0 "Male" 1"Female".
VARIABLE LABELS female "sex" .

*************Etnicity variable*************
* Three dummy variables for three categories of origin .
RECODE herkomstgroep (101 102 201 202 = 0) (0 = 1) INTO dutch .
RECODE herkomstgroep (0 102 202 = 0) (101 201 = 1) INTO west .
RECODE herkomstgroep (0 101 201 = 0) (102 202 = 1) INTO nonwest .

*************Control variables*************.
* Age .
COMPUTE age = leeftijd .

* Dummy variable (1 = respondent has children) .
RECODE aantalki (0 = 0) (1 2 3 4 5 6 = 1) into child .

* Education in years .
RECODE oplcat (1 = 8) (2 = 12) (3 = 13.5) (4 = 16) (5 = 17) (6 = 18) INTO educ .

VARIABLE LABELS
    dutch 'no migration background'
    west 'western migration background'
    nonwest 'nonwestern migration background'
    educ 'education in years'
    age 'age' 
    child 'has child(ren)' .

DESCRIPTIVES female west nonwest .

* Remove 3317 missing values from this dataset. 7053 cases remain .
COUNT miss18 = female TO educ (missing) .
SELECT IF miss18 = 0 .
EXECUTE .


*************Unemployment variable*************.
DATASET ACTIVATE avars1910 .
SORT CASES BY nomem_encr (A) .

* Create filter variable for robustness analysis .
RECODE belbezig (8 = 0) (ELSE = 1) INTO robfilter.
VALUE LABELS robfilter 0 "exclude from robustness analysis" 1 "include in robustness analysis" .

* Create dummy variable for unemployment and remove 2 missing cases from this dataset.
RECODE belbezig (1 2 3 = 0) (4 5 8 11 = 1) (6 7 9 10 12 13 14 = SYSMIS) INTO unemp .
VALUE LABELS unemp 0 "employed" 1 "unemployed" .
VARIABLE LABELS 
    unemp "unemployed"
    robfilter "filter robustness analysis" .

* Remove all categories in the question: 'what is your main occupancy?' that are not used. 4931 cases remain in this dataset.
COUNT nomiss19 = unemp (missing) .
SELECT IF nomiss19 = 0 .
EXECUTE .


*********************Variables for number of (employed) social relations*********************.
DATASET ACTIVATE integration .
SORT CASES BY nomem_encr (A) .

*************Number of employed social relations*************.
    * Number of social relations filled in .
COUNT numcon =  cs18k294 cs18k295 cs18k296 cs18k297 cs18k298 (1) .

    * Number of employed social relations .
COUNT numwork = cs18k327 cs18k338 cs18k349 cs18k360 cs18k371 (1 2) .

VARIABLE LABELS
    numcon "number of social relations filled in"
    numwork "number of employed social relations" .



**********************************************Merge datasets*************************************************.
* Match and merge the three datasets into one .
MATCH FILES FILE = avars1810 / FILE = avars1910 / FILE = integration
    /BY nomem_encr
    /KEEP nomem_encr female dutch west nonwest unemp 
               numcon numwork age child educ robfilter.
EXECUTE .
DATASET NAME merged .
DATASET ACTIVATE merged .
display DICTIONARY .

* Create 'minor' and 'old' variables. We will only keep people aged between 25 and 65 .
COMPUTE minor = age < 25 .
COMPUTE old = age > 65 .

* Create variable that counts the amount of missing cases for each respondent.
COUNT miss = female TO robfilter (MISSING) .
EXECUTE .

* Remove people under 25 and above 65 and remove cases with missing values .
SELECT IF miss = 0  AND minor = 0 AND old = 0 .
EXECUTE .

*Create interaction variables for measuring intersectional effects .
COMPUTE f_d = female * dutch .
COMPUTE f_w = female * west .
COMPUTE f_nw = female * nonwest .
EXECUTE .

VARIABLE LABELS
    f_d "female*no_migration_background"
    f_w "female*western_migration_background"
    f_nw "female*non_western_migration_background" . 

DISPLAY DICTIONARY .

SAVE OUTFILE = 'merged_final.sav'
    /KEEP nomem_encr female dutch west nonwest unemp 
               numcon numwork age child educ robfilter f_d f_w f_nw .



**********************************************Analyses*************************************************.
DATASET CLOSE ALL .
GET FILE = 'merged_final.sav' .
DATASET NAME merged .
DATASET ACTIVATE merged.

DISPLAY DICTIONARY .

*T-tests important variables 

T-TEST
  /TESTVAL=1.7402
  /MISSING=ANALYSIS
  /VARIABLES=numwork
  /CRITERIA=CI(.95).

T-TEST
  /TESTVAL = .1608
  /MISSING = ANALYSIS
  /VARIABLES = unemp
  /CRITERIA = CI(.95) .

T-TEST
  /TESTVAL = .0944
  /MISSING = ANALYSIS
  /VARIABLE = west 
  /CRITERIA = CI(.95) .

T-TEST
  /TESTVAL = .0918
  /MISSING = ANALYSIS
  /VARIABLE = nonwest
  /CRITERIA = CI(.95)
  
T-TEST
  /TESTVAL = .5101
  /MISSING = ANALYSIS
  /VARIABLE = female 
  /CRITERIA = CI(.95)

*********************************************END**************************************************