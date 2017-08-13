---
title: "codebook.md"
output: html_document
---

## Variable list and descriptions

Variable name 	Description
subject 	ID the subject who performed the activity for each window sample. Its range is from 1 to 30.
activity 	Activity name
featDomain 	Feature: Time domain signal or frequency domain signal (Time or Freq)
featInstrument 	Feature: Measuring instrument (Accelerometer or Gyroscope)
featAcceleration 	Feature: Acceleration signal (Body or Gravity)
featVariable 	Feature: Variable (Mean or SD)
featJerk 	Feature: Jerk signal
featMagnitude 	Feature: Magnitude of the signals calculated using the Euclidean norm
featAxis 	Feature: 3-axial signals in the X, Y and Z directions (X, Y, or Z)
featCount 	Feature: Count of data points used to compute average
featAverage 	Feature: Average of each variable for each activity and each subject

## Structure
Classes 'data.table' and 'data.frame':	11880 obs. of  11 variables:
 $ subject         : int  1 1 1 1 1 1 1 1 1 1 ...
 $ activity        : Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ featDomain      : Factor w/ 2 levels "Time","Freq": 1 1 1 1 1 1 1 1 1 1 ...
 $ featAcceleration: Factor w/ 3 levels NA,"Body","Gravity": 1 1 1 1 1 1 1 1 1 1 ...
 $ featInstrument  : Factor w/ 2 levels "Accelerometer",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ featJerk        : Factor w/ 2 levels NA,"Jerk": 1 1 1 1 1 1 1 1 2 2 ...
 $ featMagnitude   : Factor w/ 2 levels NA,"Magnitude": 1 1 1 1 1 1 2 2 1 1 ...
 $ featVariable    : Factor w/ 2 levels "Mean","SD": 1 1 1 2 2 2 1 2 1 1 ...
 $ featAxis        : Factor w/ 4 levels NA,"X","Y","Z": 2 3 4 2 3 4 1 1 2 3 ...
 $ count           : int  50 50 50 50 50 50 50 50 50 50 ...
 $ average         : num  -0.0166 -0.0645 0.1487 -0.8735 -0.9511 ...
 - attr(*, "sorted")= chr  "subject" "activity" "featDomain" "featAcceleration" ...
 - attr(*, ".internal.selfref")=<externalptr> 
 
## Keys
[1] "subject"          "activity"         "featDomain"       "featAcceleration"
[5] "featInstrument"   "featJerk"         "featMagnitude"    "featVariable"    
[9] "featAxis"  

## sample rows

 subject activity featDomain featAcceleration featInstrument featJerk featMagnitude
1:       1   LAYING       Time               NA      Gyroscope       NA            NA
2:       1   LAYING       Time               NA      Gyroscope       NA            NA
3:       1   LAYING       Time               NA      Gyroscope       NA            NA
4:       1   LAYING       Time               NA      Gyroscope       NA            NA
5:       1   LAYING       Time               NA      Gyroscope       NA            NA
6:       1   LAYING       Time               NA      Gyroscope       NA            NA
   featVariable featAxis count     average
1:         Mean        X    50 -0.01655309
2:         Mean        Y    50 -0.06448612
3:         Mean        Z    50  0.14868944
4:           SD        X    50 -0.87354387
5:           SD        Y    50 -0.95109044
6:           SD        Z    50 -0.90828466

## Summary 
    subject                   activity    featDomain  featAcceleration
 Min.   : 1.0   LAYING            :1980   Time:7200   NA     :4680    
 1st Qu.: 8.0   SITTING           :1980   Freq:4680   Body   :5760    
 Median :15.5   STANDING          :1980               Gravity:1440    
 Mean   :15.5   WALKING           :1980                               
 3rd Qu.:23.0   WALKING_DOWNSTAIRS:1980                               
 Max.   :30.0   WALKING_UPSTAIRS  :1980                               
       featInstrument featJerk      featMagnitude  featVariable featAxis      count      
 Accelerometer:7200   NA  :7200   NA       :8640   Mean:5940    NA:3240   Min.   :36.00  
 Gyroscope    :4680   Jerk:4680   Magnitude:3240   SD  :5940    X :2880   1st Qu.:49.00  
                                                                Y :2880   Median :54.50  
                                                                Z :2880   Mean   :57.22  
                                                                          3rd Qu.:63.25  
                                                                          Max.   :95.00  
    average        
 Min.   :-0.99767  
 1st Qu.:-0.96205  
 Median :-0.46989  
 Mean   :-0.48436  
 3rd Qu.:-0.07836  
 Max.   : 0.97451 
 
## Possible combinations
    featDomain featAcceleration featInstrument featJerk featMagnitude featVariable
 1:       Time               NA      Gyroscope       NA            NA         Mean
 2:       Time               NA      Gyroscope       NA            NA         Mean
 3:       Time               NA      Gyroscope       NA            NA         Mean
 4:       Time               NA      Gyroscope       NA            NA           SD
 5:       Time               NA      Gyroscope       NA            NA           SD
 6:       Time               NA      Gyroscope       NA            NA           SD
 7:       Time               NA      Gyroscope       NA     Magnitude         Mean
 8:       Time               NA      Gyroscope       NA     Magnitude           SD
 9:       Time               NA      Gyroscope     Jerk            NA         Mean
10:       Time               NA      Gyroscope     Jerk            NA         Mean
11:       Time               NA      Gyroscope     Jerk            NA         Mean
12:       Time               NA      Gyroscope     Jerk            NA           SD
13:       Time               NA      Gyroscope     Jerk            NA           SD
14:       Time               NA      Gyroscope     Jerk            NA           SD
15:       Time               NA      Gyroscope     Jerk     Magnitude         Mean
16:       Time               NA      Gyroscope     Jerk     Magnitude           SD
17:       Time             Body  Accelerometer       NA            NA         Mean
18:       Time             Body  Accelerometer       NA            NA         Mean
19:       Time             Body  Accelerometer       NA            NA         Mean
20:       Time             Body  Accelerometer       NA            NA           SD
21:       Time             Body  Accelerometer       NA            NA           SD
22:       Time             Body  Accelerometer       NA            NA           SD
23:       Time             Body  Accelerometer       NA     Magnitude         Mean
24:       Time             Body  Accelerometer       NA     Magnitude           SD
25:       Time             Body  Accelerometer     Jerk            NA         Mean
26:       Time             Body  Accelerometer     Jerk            NA         Mean
27:       Time             Body  Accelerometer     Jerk            NA         Mean
28:       Time             Body  Accelerometer     Jerk            NA           SD
29:       Time             Body  Accelerometer     Jerk            NA           SD
30:       Time             Body  Accelerometer     Jerk            NA           SD
31:       Time             Body  Accelerometer     Jerk     Magnitude         Mean
32:       Time             Body  Accelerometer     Jerk     Magnitude           SD
33:       Time          Gravity  Accelerometer       NA            NA         Mean
34:       Time          Gravity  Accelerometer       NA            NA         Mean
35:       Time          Gravity  Accelerometer       NA            NA         Mean
36:       Time          Gravity  Accelerometer       NA            NA           SD
37:       Time          Gravity  Accelerometer       NA            NA           SD
38:       Time          Gravity  Accelerometer       NA            NA           SD
39:       Time          Gravity  Accelerometer       NA     Magnitude         Mean
40:       Time          Gravity  Accelerometer       NA     Magnitude           SD
41:       Freq               NA      Gyroscope       NA            NA         Mean
42:       Freq               NA      Gyroscope       NA            NA         Mean
43:       Freq               NA      Gyroscope       NA            NA         Mean
44:       Freq               NA      Gyroscope       NA            NA           SD
45:       Freq               NA      Gyroscope       NA            NA           SD
46:       Freq               NA      Gyroscope       NA            NA           SD
47:       Freq               NA      Gyroscope       NA     Magnitude         Mean
48:       Freq               NA      Gyroscope       NA     Magnitude           SD
49:       Freq               NA      Gyroscope     Jerk     Magnitude         Mean
50:       Freq               NA      Gyroscope     Jerk     Magnitude           SD
51:       Freq             Body  Accelerometer       NA            NA         Mean
52:       Freq             Body  Accelerometer       NA            NA         Mean
53:       Freq             Body  Accelerometer       NA            NA         Mean
54:       Freq             Body  Accelerometer       NA            NA           SD
55:       Freq             Body  Accelerometer       NA            NA           SD
56:       Freq             Body  Accelerometer       NA            NA           SD
57:       Freq             Body  Accelerometer       NA     Magnitude         Mean
58:       Freq             Body  Accelerometer       NA     Magnitude           SD
59:       Freq             Body  Accelerometer     Jerk            NA         Mean
60:       Freq             Body  Accelerometer     Jerk            NA         Mean
61:       Freq             Body  Accelerometer     Jerk            NA         Mean
62:       Freq             Body  Accelerometer     Jerk            NA           SD
63:       Freq             Body  Accelerometer     Jerk            NA           SD
64:       Freq             Body  Accelerometer     Jerk            NA           SD
65:       Freq             Body  Accelerometer     Jerk     Magnitude         Mean
66:       Freq             Body  Accelerometer     Jerk     Magnitude           SD
    featDomain featAcceleration featInstrument featJerk featMagnitude featVariable
    featAxis   N
 1:        X 180
 2:        Y 180
 3:        Z 180
 4:        X 180
 5:        Y 180
 6:        Z 180
 7:       NA 180
 8:       NA 180
 9:        X 180
10:        Y 180
11:        Z 180
12:        X 180
13:        Y 180
14:        Z 180
15:       NA 180
16:       NA 180
17:        X 180
18:        Y 180
19:        Z 180
20:        X 180
21:        Y 180
22:        Z 180
23:       NA 180
24:       NA 180
25:        X 180
26:        Y 180
27:        Z 180
28:        X 180
29:        Y 180
30:        Z 180
31:       NA 180
32:       NA 180
33:        X 180
34:        Y 180
35:        Z 180
36:        X 180
37:        Y 180
38:        Z 180
39:       NA 180
40:       NA 180
41:        X 180
42:        Y 180
43:        Z 180
44:        X 180
45:        Y 180
46:        Z 180
47:       NA 180
48:       NA 180
49:       NA 180
50:       NA 180
51:        X 180
52:        Y 180
53:        Z 180
54:        X 180
55:        Y 180
56:        Z 180
57:       NA 180
58:       NA 180
59:        X 180
60:        Y 180
61:        Z 180
62:        X 180
63:        Y 180
64:        Z 180
65:       NA 180
66:       NA 180
    featAxis   N
    
    
 