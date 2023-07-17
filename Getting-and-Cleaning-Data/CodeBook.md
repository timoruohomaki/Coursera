## Getting and Cleanind Data Course Project

The purpose of this project is to demonstrate your ability to collect,
work with, and clean a data set. The goal is to prepare tidy data that
can be used for later analysis.

The assignment deliverable is a R script that does the following:

1.  Merges the training and the test sets to create one data set.

2.  Extracts only the measurements on the mean and standard deviation
    for each measurement.

3.  Uses descriptive activity names to name the activities in the data
    set

4.  Appropriately labels the data set with descriptive variable names.

5.  From the data set in step 4, creates a second, independent tidy data
    set with the average of each variable for each activity and each
    subject.

This codebook has been created using with *Knitr* using *rmarkdown*
package.

#### Dependencies (Packages)

``` r
library(readr)
library(knitr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

#### Step 1: Merge the training and the test sets to create one data set.

``` r
  featureData <- read.table("ucidataset/features.txt", col.names = c("n","functions"))
  activityLabels <- read.table("ucidataset/activity_labels.txt", col.names = c("code","activity"))
  subjectTest <- read.table("ucidataset/test/subject_test.txt", col.names = "subject")

  xTestData <- read.table("ucidataset/test/X_test.txt", col.names = featureData$functions)
  yTestData <- read.table("ucidataset/test/Y_test.txt", col.names = "code")

  subjectTrain <- read.table("ucidataset/train/subject_train.txt", col.names = "subject")

  xTrainData <- read.table("ucidataset/train/X_train.txt", col.names = featureData$functions)
  yTrainData <- read.table("ucidataset/train/Y_train.txt", col.names = "code")

  xData <- rbind(xTrainData, xTestData)
  yData <- rbind(yTrainData, yTestData)

  subjectData <- rbind(subjectTrain, subjectTest)
  
  # create one master (merged) dataset

  masterDataset <- cbind(subjectData, xData, yData)
  
  summary(masterDataset)
```

    ##     subject      tBodyAcc.mean...X tBodyAcc.mean...Y  tBodyAcc.mean...Z 
    ##  Min.   : 1.00   Min.   :-1.0000   Min.   :-1.00000   Min.   :-1.00000  
    ##  1st Qu.: 9.00   1st Qu.: 0.2626   1st Qu.:-0.02490   1st Qu.:-0.12102  
    ##  Median :17.00   Median : 0.2772   Median :-0.01716   Median :-0.10860  
    ##  Mean   :16.15   Mean   : 0.2743   Mean   :-0.01774   Mean   :-0.10892  
    ##  3rd Qu.:24.00   3rd Qu.: 0.2884   3rd Qu.:-0.01062   3rd Qu.:-0.09759  
    ##  Max.   :30.00   Max.   : 1.0000   Max.   : 1.00000   Max.   : 1.00000  
    ##  tBodyAcc.std...X  tBodyAcc.std...Y   tBodyAcc.std...Z  tBodyAcc.mad...X 
    ##  Min.   :-1.0000   Min.   :-1.00000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.9924   1st Qu.:-0.97699   1st Qu.:-0.9791   1st Qu.:-0.9933  
    ##  Median :-0.9430   Median :-0.83503   Median :-0.8508   Median :-0.9482  
    ##  Mean   :-0.6078   Mean   :-0.51019   Mean   :-0.6131   Mean   :-0.6336  
    ##  3rd Qu.:-0.2503   3rd Qu.:-0.05734   3rd Qu.:-0.2787   3rd Qu.:-0.3020  
    ##  Max.   : 1.0000   Max.   : 1.00000   Max.   : 1.0000   Max.   : 1.0000  
    ##  tBodyAcc.mad...Y  tBodyAcc.mad...Z  tBodyAcc.max...X   tBodyAcc.max...Y  
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.00000   Min.   :-1.00000  
    ##  1st Qu.:-0.9770   1st Qu.:-0.9791   1st Qu.:-0.93579   1st Qu.:-0.56257  
    ##  Median :-0.8437   Median :-0.8451   Median :-0.87482   Median :-0.46821  
    ##  Mean   :-0.5257   Mean   :-0.6150   Mean   :-0.46673   Mean   :-0.30518  
    ##  3rd Qu.:-0.0874   3rd Qu.:-0.2881   3rd Qu.:-0.01464   3rd Qu.:-0.06734  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.00000   Max.   : 1.00000  
    ##  tBodyAcc.max...Z  tBodyAcc.min...X  tBodyAcc.min...Y  tBodyAcc.min...Z 
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.8122   1st Qu.: 0.2125   1st Qu.: 0.1139   1st Qu.: 0.3927  
    ##  Median :-0.7245   Median : 0.7842   Median : 0.6198   Median : 0.7722  
    ##  Mean   :-0.5622   Mean   : 0.5253   Mean   : 0.3895   Mean   : 0.5980  
    ##  3rd Qu.:-0.3456   3rd Qu.: 0.8438   3rd Qu.: 0.6852   3rd Qu.: 0.8367  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  tBodyAcc.sma..    tBodyAcc.energy...X tBodyAcc.energy...Y tBodyAcc.energy...Z
    ##  Min.   :-1.0000   Min.   :-1.0000     Min.   :-1.0000     Min.   :-1.0000    
    ##  1st Qu.:-0.9817   1st Qu.:-0.9999     1st Qu.:-0.9998     1st Qu.:-0.9994    
    ##  Median :-0.8769   Median :-0.9977     Median :-0.9929     Median :-0.9842    
    ##  Mean   :-0.5521   Mean   :-0.8255     Mean   :-0.9027     Mean   :-0.8547    
    ##  3rd Qu.:-0.1228   3rd Qu.:-0.7157     3rd Qu.:-0.8251     3rd Qu.:-0.7595    
    ##  Max.   : 1.0000   Max.   : 1.0000     Max.   : 1.0000     Max.   : 1.0000    
    ##  tBodyAcc.iqr...X  tBodyAcc.iqr...Y  tBodyAcc.iqr...Z  tBodyAcc.entropy...X
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.00000    
    ##  1st Qu.:-0.9942   1st Qu.:-0.9813   1st Qu.:-0.9785   1st Qu.:-0.56383    
    ##  Median :-0.9560   Median :-0.8849   Median :-0.8538   Median :-0.05712    
    ##  Mean   :-0.6892   Mean   :-0.6435   Mean   :-0.6407   Mean   :-0.10033    
    ##  3rd Qu.:-0.4079   3rd Qu.:-0.3247   3rd Qu.:-0.3364   3rd Qu.: 0.32959    
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.00000    
    ##  tBodyAcc.entropy...Y tBodyAcc.entropy...Z tBodyAcc.arCoeff...X.1
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000       
    ##  1st Qu.:-0.5496      1st Qu.:-0.4968      1st Qu.:-0.3686       
    ##  Median :-0.1017      Median :-0.1364      Median :-0.1362       
    ##  Mean   :-0.1288      Mean   :-0.1579      Mean   :-0.1190       
    ##  3rd Qu.: 0.2831      3rd Qu.: 0.1674      3rd Qu.: 0.1332       
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000       
    ##  tBodyAcc.arCoeff...X.2 tBodyAcc.arCoeff...X.3 tBodyAcc.arCoeff...X.4
    ##  Min.   :-1.00000       Min.   :-1.00000       Min.   :-1.00000      
    ##  1st Qu.:-0.07902       1st Qu.:-0.18995       1st Qu.:-0.03386      
    ##  Median : 0.07753       Median :-0.01764       Median : 0.12628      
    ##  Mean   : 0.10857       Mean   :-0.03570       Mean   : 0.12200      
    ##  3rd Qu.: 0.28607       3rd Qu.: 0.13332       3rd Qu.: 0.27768      
    ##  Max.   : 1.00000       Max.   : 1.00000       Max.   : 1.00000      
    ##  tBodyAcc.arCoeff...Y.1 tBodyAcc.arCoeff...Y.2 tBodyAcc.arCoeff...Y.3
    ##  Min.   :-1.00000       Min.   :-1.00000       Min.   :-1.00000      
    ##  1st Qu.:-0.22197       1st Qu.:-0.12905       1st Qu.: 0.02895      
    ##  Median :-0.04549       Median : 0.01765       Median : 0.16066      
    ##  Mean   :-0.02968       Mean   : 0.03172       Mean   : 0.15515      
    ##  3rd Qu.: 0.16328       3rd Qu.: 0.18085       3rd Qu.: 0.28816      
    ##  Max.   : 1.00000       Max.   : 1.00000       Max.   : 1.00000      
    ##  tBodyAcc.arCoeff...Y.4 tBodyAcc.arCoeff...Z.1 tBodyAcc.arCoeff...Z.2
    ##  Min.   :-1.00000       Min.   :-1.00000       Min.   :-1.000000     
    ##  1st Qu.:-0.16568       1st Qu.:-0.20648       1st Qu.:-0.118064     
    ##  Median :-0.01893       Median : 0.02070       Median : 0.009945     
    ##  Mean   :-0.01808       Mean   : 0.00611       Mean   : 0.037729     
    ##  3rd Qu.: 0.13119       3rd Qu.: 0.22353       3rd Qu.: 0.179591     
    ##  Max.   : 1.00000       Max.   : 1.00000       Max.   : 1.000000     
    ##  tBodyAcc.arCoeff...Z.3 tBodyAcc.arCoeff...Z.4 tBodyAcc.correlation...X.Y
    ##  Min.   :-1.00000       Min.   :-1.00000       Min.   :-1.00000          
    ##  1st Qu.:-0.11077       1st Qu.:-0.23954       1st Qu.:-0.36167          
    ##  Median : 0.04536       Median :-0.08330       Median :-0.16117          
    ##  Mean   : 0.03442       Mean   :-0.08267       Mean   :-0.12031          
    ##  3rd Qu.: 0.19429       3rd Qu.: 0.07475       3rd Qu.: 0.08015          
    ##  Max.   : 1.00000       Max.   : 1.00000       Max.   : 1.00000          
    ##  tBodyAcc.correlation...X.Z tBodyAcc.correlation...Y.Z tGravityAcc.mean...X
    ##  Min.   :-1.000000          Min.   :-1.0000            Min.   :-1.0000     
    ##  1st Qu.:-0.408798          1st Qu.:-0.1409            1st Qu.: 0.8117     
    ##  Median :-0.191758          Median : 0.1356            Median : 0.9218     
    ##  Mean   :-0.197746          Mean   : 0.1022            Mean   : 0.6692     
    ##  3rd Qu.: 0.002491          3rd Qu.: 0.3722            3rd Qu.: 0.9547     
    ##  Max.   : 1.000000          Max.   : 1.0000            Max.   : 1.0000     
    ##  tGravityAcc.mean...Y tGravityAcc.mean...Z tGravityAcc.std...X
    ##  Min.   :-1.000000    Min.   :-1.00000     Min.   :-1.0000    
    ##  1st Qu.:-0.242943    1st Qu.:-0.11671     1st Qu.:-0.9949    
    ##  Median :-0.143551    Median : 0.03680     Median :-0.9819    
    ##  Mean   : 0.004039    Mean   : 0.09215     Mean   :-0.9652    
    ##  3rd Qu.: 0.118905    3rd Qu.: 0.21621     3rd Qu.:-0.9615    
    ##  Max.   : 1.000000    Max.   : 1.00000     Max.   : 1.0000    
    ##  tGravityAcc.std...Y tGravityAcc.std...Z tGravityAcc.mad...X
    ##  Min.   :-1.0000     Min.   :-1.0000     Min.   :-1.0000    
    ##  1st Qu.:-0.9913     1st Qu.:-0.9866     1st Qu.:-0.9950    
    ##  Median :-0.9759     Median :-0.9665     Median :-0.9826    
    ##  Mean   :-0.9544     Mean   :-0.9389     Mean   :-0.9660    
    ##  3rd Qu.:-0.9464     3rd Qu.:-0.9296     3rd Qu.:-0.9628    
    ##  Max.   : 1.0000     Max.   : 1.0000     Max.   : 1.0000    
    ##  tGravityAcc.mad...Y tGravityAcc.mad...Z tGravityAcc.max...X
    ##  Min.   :-1.0000     Min.   :-1.0000     Min.   :-1.0000    
    ##  1st Qu.:-0.9915     1st Qu.:-0.9869     1st Qu.: 0.7556    
    ##  Median :-0.9766     Median :-0.9676     Median : 0.8590    
    ##  Mean   :-0.9554     Mean   :-0.9402     Mean   : 0.6091    
    ##  3rd Qu.:-0.9478     3rd Qu.:-0.9315     3rd Qu.: 0.8878    
    ##  Max.   : 1.0000     Max.   : 1.0000     Max.   : 1.0000    
    ##  tGravityAcc.max...Y tGravityAcc.max...Z tGravityAcc.min...X
    ##  Min.   :-1.00000    Min.   :-1.00000    Min.   :-1.0000    
    ##  1st Qu.:-0.24956    1st Qu.:-0.11161    1st Qu.: 0.8166    
    ##  Median :-0.15094    Median : 0.04303    Median : 0.9288    
    ##  Mean   :-0.01032    Mean   : 0.09674    Mean   : 0.6838    
    ##  3rd Qu.: 0.11788    3rd Qu.: 0.21680    3rd Qu.: 0.9666    
    ##  Max.   : 1.00000    Max.   : 1.00000    Max.   : 1.0000    
    ##  tGravityAcc.min...Y tGravityAcc.min...Z tGravityAcc.sma.. 
    ##  Min.   :-1.00000    Min.   :-1.00000    Min.   :-1.00000  
    ##  1st Qu.:-0.22964    1st Qu.:-0.13257    1st Qu.:-0.40928  
    ##  Median :-0.12835    Median : 0.02220    Median :-0.13126  
    ##  Mean   : 0.01661    Mean   : 0.07935    Mean   :-0.09859  
    ##  3rd Qu.: 0.12899    3rd Qu.: 0.19980    3rd Qu.: 0.18839  
    ##  Max.   : 1.00000    Max.   : 1.00000    Max.   : 1.00000  
    ##  tGravityAcc.energy...X tGravityAcc.energy...Y tGravityAcc.energy...Z
    ##  Min.   :-1.0000        Min.   :-1.0000        Min.   :-1.0000       
    ##  1st Qu.: 0.5209        1st Qu.:-0.9680        1st Qu.:-0.9911       
    ##  Median : 0.7912        Median :-0.9098        Median :-0.9508       
    ##  Mean   : 0.4462        Mean   :-0.7215        Mean   :-0.7636       
    ##  3rd Qu.: 0.8766        3rd Qu.:-0.7668        3rd Qu.:-0.7872       
    ##  Max.   : 1.0000        Max.   : 1.0000        Max.   : 1.0000       
    ##  tGravityAcc.iqr...X tGravityAcc.iqr...Y tGravityAcc.iqr...Z
    ##  Min.   :-1.0000     Min.   :-1.0000     Min.   :-1.0000    
    ##  1st Qu.:-0.9952     1st Qu.:-0.9924     1st Qu.:-0.9881    
    ##  Median :-0.9846     Median :-0.9795     Median :-0.9712    
    ##  Mean   :-0.9682     Mean   :-0.9586     Mean   :-0.9448    
    ##  3rd Qu.:-0.9656     3rd Qu.:-0.9521     3rd Qu.:-0.9384    
    ##  Max.   : 1.0000     Max.   : 1.0000     Max.   : 1.0000    
    ##  tGravityAcc.entropy...X tGravityAcc.entropy...Y tGravityAcc.entropy...Z
    ##  Min.   :-1.0000         Min.   :-1.0000         Min.   :-1.0000        
    ##  1st Qu.:-1.0000         1st Qu.:-1.0000         1st Qu.:-1.0000        
    ##  Median :-0.7631         Median :-1.0000         Median :-0.7808        
    ##  Mean   :-0.6748         Mean   :-0.8667         Mean   :-0.6691        
    ##  3rd Qu.:-0.3996         3rd Qu.:-0.8511         3rd Qu.:-0.4008        
    ##  Max.   : 1.0000         Max.   : 1.0000         Max.   : 1.0000        
    ##  tGravityAcc.arCoeff...X.1 tGravityAcc.arCoeff...X.2 tGravityAcc.arCoeff...X.3
    ##  Min.   :-1.0000           Min.   :-1.0000           Min.   :-1.0000          
    ##  1st Qu.:-0.6512           1st Qu.: 0.4048           1st Qu.:-0.7391          
    ##  Median :-0.5102           Median : 0.5552           Median :-0.5974          
    ##  Mean   :-0.5044           Mean   : 0.5429           Mean   :-0.5807          
    ##  3rd Qu.:-0.3667           3rd Qu.: 0.6933           3rd Qu.:-0.4403          
    ##  Max.   : 1.0000           Max.   : 1.0000           Max.   : 1.0000          
    ##  tGravityAcc.arCoeff...X.4 tGravityAcc.arCoeff...Y.1 tGravityAcc.arCoeff...Y.2
    ##  Min.   :-1.0000           Min.   :-1.0000           Min.   :-1.0000          
    ##  1st Qu.: 0.4711           1st Qu.:-0.5461           1st Qu.: 0.1278          
    ##  Median : 0.6424           Median :-0.3418           Median : 0.3289          
    ##  Mean   : 0.6178           Mean   :-0.3436           Mean   : 0.3302          
    ##  3rd Qu.: 0.7846           3rd Qu.:-0.1434           3rd Qu.: 0.5352          
    ##  Max.   : 1.0000           Max.   : 1.0000           Max.   : 1.0000          
    ##  tGravityAcc.arCoeff...Y.3 tGravityAcc.arCoeff...Y.4 tGravityAcc.arCoeff...Z.1
    ##  Min.   :-1.0000           Min.   :-1.0000           Min.   :-1.0000          
    ##  1st Qu.:-0.5584           1st Qu.: 0.2301           1st Qu.:-0.6200          
    ##  Median :-0.3659           Median : 0.4243           Median :-0.4261          
    ##  Mean   :-0.3607           Mean   : 0.4089           Mean   :-0.4281          
    ##  3rd Qu.:-0.1726           3rd Qu.: 0.6048           3rd Qu.:-0.2487          
    ##  Max.   : 1.0000           Max.   : 1.0000           Max.   : 1.0000          
    ##  tGravityAcc.arCoeff...Z.2 tGravityAcc.arCoeff...Z.3 tGravityAcc.arCoeff...Z.4
    ##  Min.   :-1.0000           Min.   :-1.0000           Min.   :-1.0000          
    ##  1st Qu.: 0.2792           1st Qu.:-0.6634           1st Qu.: 0.3306          
    ##  Median : 0.4501           Median :-0.4787           Median : 0.5052          
    ##  Mean   : 0.4534           Mean   :-0.4780           Mean   : 0.4995          
    ##  3rd Qu.: 0.6422           3rd Qu.:-0.3068           3rd Qu.: 0.6839          
    ##  Max.   : 1.0000           Max.   : 1.0000           Max.   : 1.0000          
    ##  tGravityAcc.correlation...X.Y tGravityAcc.correlation...X.Z
    ##  Min.   :-1.0000               Min.   :-1.0000              
    ##  1st Qu.:-0.4902               1st Qu.:-0.8060              
    ##  Median : 0.3609               Median :-0.2174              
    ##  Mean   : 0.1757               Mean   :-0.1083              
    ##  3rd Qu.: 0.8381               3rd Qu.: 0.5912              
    ##  Max.   : 1.0000               Max.   : 1.0000              
    ##  tGravityAcc.correlation...Y.Z tBodyAccJerk.mean...X tBodyAccJerk.mean...Y
    ##  Min.   :-1.00000              Min.   :-1.00000      Min.   :-1.000000    
    ##  1st Qu.:-0.60900              1st Qu.: 0.06298      1st Qu.:-0.018555    
    ##  Median : 0.17537              Median : 0.07597      Median : 0.010753    
    ##  Mean   : 0.08485              Mean   : 0.07894      Mean   : 0.007948    
    ##  3rd Qu.: 0.77599              3rd Qu.: 0.09131      3rd Qu.: 0.033538    
    ##  Max.   : 1.00000              Max.   : 1.00000      Max.   : 1.000000    
    ##  tBodyAccJerk.mean...Z tBodyAccJerk.std...X tBodyAccJerk.std...Y
    ##  Min.   :-1.000000     Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.031552     1st Qu.:-0.9913      1st Qu.:-0.9850     
    ##  Median :-0.001159     Median :-0.9513      Median :-0.9250     
    ##  Mean   :-0.004675     Mean   :-0.6398      Mean   :-0.6080     
    ##  3rd Qu.: 0.024578     3rd Qu.:-0.2912      3rd Qu.:-0.2218     
    ##  Max.   : 1.000000     Max.   : 1.0000      Max.   : 1.0000     
    ##  tBodyAccJerk.std...Z tBodyAccJerk.mad...X tBodyAccJerk.mad...Y
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.9892      1st Qu.:-0.9913      1st Qu.:-0.9833     
    ##  Median :-0.9543      Median :-0.9578      Median :-0.9265     
    ##  Mean   :-0.7628      Mean   :-0.6369      Mean   :-0.5940     
    ##  3rd Qu.:-0.5485      3rd Qu.:-0.2803      3rd Qu.:-0.1892     
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000     
    ##  tBodyAccJerk.mad...Z tBodyAccJerk.max...X tBodyAccJerk.max...Y
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.9880      1st Qu.:-0.9916      1st Qu.:-0.9901     
    ##  Median :-0.9548      Median :-0.9447      Median :-0.9411     
    ##  Mean   :-0.7565      Mean   :-0.6996      Mean   :-0.7478     
    ##  3rd Qu.:-0.5357      3rd Qu.:-0.4608      3rd Qu.:-0.5351     
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000     
    ##  tBodyAccJerk.max...Z tBodyAccJerk.min...X tBodyAccJerk.min...Y
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.9898      1st Qu.: 0.2531      1st Qu.: 0.4017     
    ##  Median :-0.9564      Median : 0.9377      Median : 0.9311     
    ##  Mean   :-0.8187      Mean   : 0.6160      Mean   : 0.6849     
    ##  3rd Qu.:-0.6830      3rd Qu.: 0.9896      3rd Qu.: 0.9886     
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000     
    ##  tBodyAccJerk.min...Z tBodyAccJerk.sma.. tBodyAccJerk.energy...X
    ##  Min.   :-1.0000      Min.   :-1.0000    Min.   :-1.0000        
    ##  1st Qu.: 0.5287      1st Qu.:-0.9898    1st Qu.:-0.9999        
    ##  Median : 0.9404      Median :-0.9478    Median :-0.9985        
    ##  Mean   : 0.7396      Mean   :-0.6470    Mean   :-0.8503        
    ##  3rd Qu.: 0.9867      3rd Qu.:-0.2909    3rd Qu.:-0.7460        
    ##  Max.   : 1.0000      Max.   : 1.0000    Max.   : 1.0000        
    ##  tBodyAccJerk.energy...Y tBodyAccJerk.energy...Z tBodyAccJerk.iqr...X
    ##  Min.   :-1.0000         Min.   :-1.0000         Min.   :-1.0000     
    ##  1st Qu.:-0.9997         1st Qu.:-0.9998         1st Qu.:-0.9899     
    ##  Median :-0.9965         Median :-0.9983         Median :-0.9634     
    ##  Mean   :-0.8273         Mean   :-0.9307         Mean   :-0.6275     
    ##  3rd Qu.:-0.6924         3rd Qu.:-0.8928         3rd Qu.:-0.2657     
    ##  Max.   : 1.0000         Max.   : 1.0000         Max.   : 1.0000     
    ##  tBodyAccJerk.iqr...Y tBodyAccJerk.iqr...Z tBodyAccJerk.entropy...X
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.00000        
    ##  1st Qu.:-0.9836      1st Qu.:-0.9863      1st Qu.:-0.72976        
    ##  Median :-0.9434      Median :-0.9583      Median :-0.33770        
    ##  Mean   :-0.6598      Mean   :-0.7708      Mean   :-0.08261        
    ##  3rd Qu.:-0.3295      3rd Qu.:-0.5757      3rd Qu.: 0.59944        
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.00000        
    ##  tBodyAccJerk.entropy...Y tBodyAccJerk.entropy...Z tBodyAccJerk.arCoeff...X.1
    ##  Min.   :-1.00000         Min.   :-1.0000          Min.   :-1.0000           
    ##  1st Qu.:-0.72627         1st Qu.:-0.7211          1st Qu.:-0.3598           
    ##  Median :-0.28416         Median :-0.3935          Median :-0.1159           
    ##  Mean   :-0.08788         Mean   :-0.1273          Mean   :-0.1086           
    ##  3rd Qu.: 0.56779         3rd Qu.: 0.4941          3rd Qu.: 0.1443           
    ##  Max.   : 1.00000         Max.   : 1.0000          Max.   : 1.0000           
    ##  tBodyAccJerk.arCoeff...X.2 tBodyAccJerk.arCoeff...X.3
    ##  Min.   :-1.00000           Min.   :-1.00000          
    ##  1st Qu.: 0.04889           1st Qu.:-0.08780          
    ##  Median : 0.17289           Median : 0.07724          
    ##  Mean   : 0.16966           Mean   : 0.06760          
    ##  3rd Qu.: 0.29159           3rd Qu.: 0.23426          
    ##  Max.   : 1.00000           Max.   : 1.00000          
    ##  tBodyAccJerk.arCoeff...X.4 tBodyAccJerk.arCoeff...Y.1
    ##  Min.   :-1.000000          Min.   :-1.00000          
    ##  1st Qu.:-0.003601          1st Qu.:-0.27656          
    ##  Median : 0.134771          Median :-0.08578          
    ##  Mean   : 0.126468          Mean   :-0.07272          
    ##  3rd Qu.: 0.263559          3rd Qu.: 0.13992          
    ##  Max.   : 1.000000          Max.   : 1.00000          
    ##  tBodyAccJerk.arCoeff...Y.2 tBodyAccJerk.arCoeff...Y.3
    ##  Min.   :-1.00000           Min.   :-1.00000          
    ##  1st Qu.:-0.06502           1st Qu.: 0.02252          
    ##  Median : 0.06698           Median : 0.18646          
    ##  Mean   : 0.06869           Mean   : 0.17453          
    ##  3rd Qu.: 0.20732           3rd Qu.: 0.33926          
    ##  Max.   : 1.00000           Max.   : 1.00000          
    ##  tBodyAccJerk.arCoeff...Y.4 tBodyAccJerk.arCoeff...Z.1
    ##  Min.   :-1.0000            Min.   :-1.00000          
    ##  1st Qu.: 0.1852            1st Qu.:-0.23740          
    ##  Median : 0.3185            Median :-0.01346          
    ##  Mean   : 0.3143            Mean   :-0.03269          
    ##  3rd Qu.: 0.4521            3rd Qu.: 0.18013          
    ##  Max.   : 1.0000            Max.   : 1.00000          
    ##  tBodyAccJerk.arCoeff...Z.2 tBodyAccJerk.arCoeff...Z.3
    ##  Min.   :-1.00000           Min.   :-1.000000         
    ##  1st Qu.:-0.03561           1st Qu.:-0.143299         
    ##  Median : 0.08742           Median : 0.013300         
    ##  Mean   : 0.08868           Mean   :-0.001039         
    ##  3rd Qu.: 0.21251           3rd Qu.: 0.158161         
    ##  Max.   : 1.00000           Max.   : 1.000000         
    ##  tBodyAccJerk.arCoeff...Z.4 tBodyAccJerk.correlation...X.Y
    ##  Min.   :-1.00000           Min.   :-1.00000              
    ##  1st Qu.:-0.01551           1st Qu.:-0.30989              
    ##  Median : 0.15024           Median :-0.13898              
    ##  Mean   : 0.13847           Mean   :-0.13812              
    ##  3rd Qu.: 0.30012           3rd Qu.: 0.02874              
    ##  Max.   : 1.00000           Max.   : 1.00000              
    ##  tBodyAccJerk.correlation...X.Z tBodyAccJerk.correlation...Y.Z
    ##  Min.   :-1.000000              Min.   :-1.00000              
    ##  1st Qu.:-0.198658              1st Qu.:-0.10247              
    ##  Median : 0.013307              Median : 0.07611              
    ##  Mean   : 0.003025              Mean   : 0.08032              
    ##  3rd Qu.: 0.206962              3rd Qu.: 0.26336              
    ##  Max.   : 1.000000              Max.   : 1.00000              
    ##  tBodyGyro.mean...X tBodyGyro.mean...Y tBodyGyro.mean...Z tBodyGyro.std...X
    ##  Min.   :-1.00000   Min.   :-1.00000   Min.   :-1.00000   Min.   :-1.0000  
    ##  1st Qu.:-0.04579   1st Qu.:-0.10399   1st Qu.: 0.06485   1st Qu.:-0.9872  
    ##  Median :-0.02776   Median :-0.07477   Median : 0.08626   Median :-0.9016  
    ##  Mean   :-0.03098   Mean   :-0.07472   Mean   : 0.08836   Mean   :-0.7212  
    ##  3rd Qu.:-0.01058   3rd Qu.:-0.05110   3rd Qu.: 0.11044   3rd Qu.:-0.4822  
    ##  Max.   : 1.00000   Max.   : 1.00000   Max.   : 1.00000   Max.   : 1.0000  
    ##  tBodyGyro.std...Y tBodyGyro.std...Z tBodyGyro.mad...X tBodyGyro.mad...Y
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.9819   1st Qu.:-0.9850   1st Qu.:-0.9881   1st Qu.:-0.9830  
    ##  Median :-0.9106   Median :-0.8819   Median :-0.9076   Median :-0.9194  
    ##  Mean   :-0.6827   Mean   :-0.6537   Mean   :-0.7265   Mean   :-0.6945  
    ##  3rd Qu.:-0.4461   3rd Qu.:-0.3379   3rd Qu.:-0.4918   3rd Qu.:-0.4640  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  tBodyGyro.mad...Z tBodyGyro.max...X tBodyGyro.max...Y tBodyGyro.max...Z
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.9859   1st Qu.:-0.8778   1st Qu.:-0.9473   1st Qu.:-0.7494  
    ##  Median :-0.8887   Median :-0.7948   Median :-0.8901   Median :-0.6451  
    ##  Mean   :-0.6672   Mean   :-0.6455   Mean   :-0.7380   Mean   :-0.4842  
    ##  3rd Qu.:-0.3635   3rd Qu.:-0.4367   3rd Qu.:-0.5798   3rd Qu.:-0.2566  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  tBodyGyro.min...X tBodyGyro.min...Y tBodyGyro.min...Z tBodyGyro.sma..  
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.: 0.4628   1st Qu.: 0.6036   1st Qu.: 0.3229   1st Qu.:-0.9785  
    ##  Median : 0.7706   Median : 0.8528   Median : 0.7418   Median :-0.8204  
    ##  Mean   : 0.6323   Mean   : 0.7343   Mean   : 0.5592   Mean   :-0.6027  
    ##  3rd Qu.: 0.8380   3rd Qu.: 0.9058   3rd Qu.: 0.8227   3rd Qu.:-0.2393  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  tBodyGyro.energy...X tBodyGyro.energy...Y tBodyGyro.energy...Z
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.9999      1st Qu.:-0.9997      1st Qu.:-0.9997     
    ##  Median :-0.9903      Median :-0.9925      Median :-0.9810     
    ##  Mean   :-0.9016      Mean   :-0.8842      Mean   :-0.8730     
    ##  3rd Qu.:-0.8368      3rd Qu.:-0.8417      3rd Qu.:-0.7882     
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000     
    ##  tBodyGyro.iqr...X tBodyGyro.iqr...Y tBodyGyro.iqr...Z tBodyGyro.entropy...X
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000      
    ##  1st Qu.:-0.9897   1st Qu.:-0.9856   1st Qu.:-0.9891   1st Qu.:-0.5081      
    ##  Median :-0.9187   Median :-0.9325   Median :-0.9073   Median :-0.1758      
    ##  Mean   :-0.7292   Mean   :-0.7172   Mean   :-0.7202   Mean   :-0.1449      
    ##  3rd Qu.:-0.5009   3rd Qu.:-0.5059   3rd Qu.:-0.4745   3rd Qu.: 0.1890      
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000      
    ##  tBodyGyro.entropy...Y tBodyGyro.entropy...Z tBodyGyro.arCoeff...X.1
    ##  Min.   :-1.00000      Min.   :-1.00000      Min.   :-1.00000       
    ##  1st Qu.:-0.35471      1st Qu.:-0.46470      1st Qu.:-0.42457       
    ##  Median :-0.06425      Median : 0.03286      Median :-0.24352       
    ##  Mean   :-0.09925      Mean   :-0.06325      Mean   :-0.22286       
    ##  3rd Qu.: 0.17896      3rd Qu.: 0.30890      3rd Qu.:-0.02796       
    ##  Max.   : 1.00000      Max.   : 1.00000      Max.   : 1.00000       
    ##  tBodyGyro.arCoeff...X.2 tBodyGyro.arCoeff...X.3 tBodyGyro.arCoeff...X.4
    ##  Min.   :-1.00000        Min.   :-1.00000        Min.   :-1.00000       
    ##  1st Qu.:-0.01852        1st Qu.:-0.01473        1st Qu.:-0.24373       
    ##  Median : 0.14146        Median : 0.14270        Median :-0.07804       
    ##  Mean   : 0.14774        Mean   : 0.12850        Mean   :-0.08031       
    ##  3rd Qu.: 0.30224        3rd Qu.: 0.28247        3rd Qu.: 0.08061       
    ##  Max.   : 1.00000        Max.   : 1.00000        Max.   : 1.00000       
    ##  tBodyGyro.arCoeff...Y.1 tBodyGyro.arCoeff...Y.2 tBodyGyro.arCoeff...Y.3
    ##  Min.   :-1.00000        Min.   :-1.00000        Min.   :-1.00000       
    ##  1st Qu.:-0.34867        1st Qu.: 0.03652        1st Qu.:-0.18878       
    ##  Median :-0.21034        Median : 0.16460        Median :-0.03930       
    ##  Mean   :-0.20479        Mean   : 0.16968        Mean   :-0.04246       
    ##  3rd Qu.:-0.06448        3rd Qu.: 0.29933        3rd Qu.: 0.11158       
    ##  Max.   : 1.00000        Max.   : 1.00000        Max.   : 1.00000       
    ##  tBodyGyro.arCoeff...Y.4 tBodyGyro.arCoeff...Z.1 tBodyGyro.arCoeff...Z.2
    ##  Min.   :-1.0000000      Min.   :-1.0000         Min.   :-1.00000       
    ##  1st Qu.: 0.0005374      1st Qu.:-0.3355         1st Qu.:-0.16668       
    ##  Median : 0.1447164      Median :-0.1025         Median : 0.06419       
    ##  Mean   : 0.1416983      Mean   :-0.0851         Mean   : 0.06673       
    ##  3rd Qu.: 0.2900701      3rd Qu.: 0.1626         3rd Qu.: 0.29045       
    ##  Max.   : 1.0000000      Max.   : 1.0000         Max.   : 1.00000       
    ##  tBodyGyro.arCoeff...Z.3 tBodyGyro.arCoeff...Z.4 tBodyGyro.correlation...X.Y
    ##  Min.   :-1.000000       Min.   :-1.00000        Min.   :-1.00000           
    ##  1st Qu.:-0.195456       1st Qu.:-0.03189        1st Qu.:-0.44386           
    ##  Median : 0.011391       Median : 0.14715        Median :-0.17932           
    ##  Mean   :-0.007924       Mean   : 0.14642        Mean   :-0.16934           
    ##  3rd Qu.: 0.187483       3rd Qu.: 0.32157        3rd Qu.: 0.08585           
    ##  Max.   : 1.000000       Max.   : 1.00000        Max.   : 1.00000           
    ##  tBodyGyro.correlation...X.Z tBodyGyro.correlation...Y.Z tBodyGyroJerk.mean...X
    ##  Min.   :-1.000000           Min.   :-1.00000            Min.   :-1.00000      
    ##  1st Qu.:-0.239278           1st Qu.:-0.41138            1st Qu.:-0.11723      
    ##  Median :-0.008025           Median :-0.09385            Median :-0.09824      
    ##  Mean   : 0.014346           Mean   :-0.10569            Mean   :-0.09671      
    ##  3rd Qu.: 0.259517           3rd Qu.: 0.18857            3rd Qu.:-0.07930      
    ##  Max.   : 1.000000           Max.   : 1.00000            Max.   : 1.00000      
    ##  tBodyGyroJerk.mean...Y tBodyGyroJerk.mean...Z tBodyGyroJerk.std...X
    ##  Min.   :-1.00000       Min.   :-1.00000       Min.   :-1.0000      
    ##  1st Qu.:-0.05868       1st Qu.:-0.07936       1st Qu.:-0.9907      
    ##  Median :-0.04056       Median :-0.05455       Median :-0.9348      
    ##  Mean   :-0.04232       Mean   :-0.05483       Mean   :-0.7313      
    ##  3rd Qu.:-0.02521       3rd Qu.:-0.03168       3rd Qu.:-0.4865      
    ##  Max.   : 1.00000       Max.   : 1.00000       Max.   : 1.0000      
    ##  tBodyGyroJerk.std...Y tBodyGyroJerk.std...Z tBodyGyroJerk.mad...X
    ##  Min.   :-1.0000       Min.   :-1.0000       Min.   :-1.0000      
    ##  1st Qu.:-0.9922       1st Qu.:-0.9926       1st Qu.:-0.9909      
    ##  Median :-0.9548       Median :-0.9503       Median :-0.9427      
    ##  Mean   :-0.7861       Mean   :-0.7399       Mean   :-0.7300      
    ##  3rd Qu.:-0.6268       3rd Qu.:-0.5097       3rd Qu.:-0.4779      
    ##  Max.   : 1.0000       Max.   : 1.0000       Max.   : 1.0000      
    ##  tBodyGyroJerk.mad...Y tBodyGyroJerk.mad...Z tBodyGyroJerk.max...X
    ##  Min.   :-1.0000       Min.   :-1.0000       Min.   :-1.0000      
    ##  1st Qu.:-0.9927       1st Qu.:-0.9928       1st Qu.:-0.9904      
    ##  Median :-0.9623       Median :-0.9579       Median :-0.9266      
    ##  Mean   :-0.7966       Mean   :-0.7466       Mean   :-0.7473      
    ##  3rd Qu.:-0.6400       3rd Qu.:-0.5133       3rd Qu.:-0.5373      
    ##  Max.   : 1.0000       Max.   : 1.0000       Max.   : 1.0000      
    ##  tBodyGyroJerk.max...Y tBodyGyroJerk.max...Z tBodyGyroJerk.min...X
    ##  Min.   :-1.0000       Min.   :-1.0000       Min.   :-1.0000      
    ##  1st Qu.:-0.9928       1st Qu.:-0.9909       1st Qu.: 0.5658      
    ##  Median :-0.9527       Median :-0.9374       Median : 0.9317      
    ##  Mean   :-0.8099       Mean   :-0.7427       Mean   : 0.7606      
    ##  3rd Qu.:-0.6862       3rd Qu.:-0.5317       3rd Qu.: 0.9906      
    ##  Max.   : 1.0000       Max.   : 1.0000       Max.   : 1.0000      
    ##  tBodyGyroJerk.min...Y tBodyGyroJerk.min...Z tBodyGyroJerk.sma..
    ##  Min.   :-1.0000       Min.   :-1.0000       Min.   :-1.0000    
    ##  1st Qu.: 0.7209       1st Qu.: 0.6439       1st Qu.:-0.9926    
    ##  Median : 0.9586       Median : 0.9557       Median :-0.9573    
    ##  Mean   : 0.8303       Mean   : 0.7994       Mean   :-0.7668    
    ##  3rd Qu.: 0.9937       3rd Qu.: 0.9938       3rd Qu.:-0.5544    
    ##  Max.   : 1.0000       Max.   : 1.0000       Max.   : 1.0000    
    ##  tBodyGyroJerk.energy...X tBodyGyroJerk.energy...Y tBodyGyroJerk.energy...Z
    ##  Min.   :-1.0000          Min.   :-1.0000          Min.   :-1.0000         
    ##  1st Qu.:-0.9999          1st Qu.:-1.0000          1st Qu.:-0.9999         
    ##  Median :-0.9976          Median :-0.9989          Median :-0.9985         
    ##  Mean   :-0.9169          Mean   :-0.9395          Mean   :-0.9195         
    ##  3rd Qu.:-0.8665          3rd Qu.:-0.9296          3rd Qu.:-0.8776         
    ##  Max.   : 1.0000          Max.   : 1.0000          Max.   : 1.0000         
    ##  tBodyGyroJerk.iqr...X tBodyGyroJerk.iqr...Y tBodyGyroJerk.iqr...Z
    ##  Min.   :-1.0000       Min.   :-1.0000       Min.   :-1.0000      
    ##  1st Qu.:-0.9914       1st Qu.:-0.9932       1st Qu.:-0.9932      
    ##  Median :-0.9534       Median :-0.9698       Median :-0.9670      
    ##  Mean   :-0.7375       Mean   :-0.8088       Mean   :-0.7663      
    ##  3rd Qu.:-0.4930       3rd Qu.:-0.6596       3rd Qu.:-0.5481      
    ##  Max.   : 1.0000       Max.   : 1.0000       Max.   : 1.0000      
    ##  tBodyGyroJerk.entropy...X tBodyGyroJerk.entropy...Y tBodyGyroJerk.entropy...Z
    ##  Min.   :-1.00000          Min.   :-1.000000         Min.   :-1.00000         
    ##  1st Qu.:-0.57464          1st Qu.:-0.509004         1st Qu.:-0.60550         
    ##  Median :-0.03394          Median :-0.002381         Median :-0.13208         
    ##  Mean   :-0.01519          Mean   : 0.018669         Mean   :-0.01511         
    ##  3rd Qu.: 0.54373          3rd Qu.: 0.551737         3rd Qu.: 0.57292         
    ##  Max.   : 1.00000          Max.   : 1.000000         Max.   : 1.00000         
    ##  tBodyGyroJerk.arCoeff...X.1 tBodyGyroJerk.arCoeff...X.2
    ##  Min.   :-1.00000            Min.   :-1.00000           
    ##  1st Qu.:-0.25044            1st Qu.:-0.07740           
    ##  Median :-0.08978            Median : 0.04018           
    ##  Mean   :-0.07248            Mean   : 0.04092           
    ##  3rd Qu.: 0.09698            3rd Qu.: 0.15582           
    ##  Max.   : 1.00000            Max.   : 1.00000           
    ##  tBodyGyroJerk.arCoeff...X.3 tBodyGyroJerk.arCoeff...X.4
    ##  Min.   :-1.00000            Min.   :-1.00000           
    ##  1st Qu.: 0.02469            1st Qu.: 0.02961           
    ##  Median : 0.16501            Median : 0.17147           
    ##  Mean   : 0.15967            Mean   : 0.16473           
    ##  3rd Qu.: 0.30281            3rd Qu.: 0.30750           
    ##  Max.   : 1.00000            Max.   : 1.00000           
    ##  tBodyGyroJerk.arCoeff...Y.1 tBodyGyroJerk.arCoeff...Y.2
    ##  Min.   :-1.00000            Min.   :-1.00000           
    ##  1st Qu.:-0.31625            1st Qu.: 0.09689           
    ##  Median :-0.16887            Median : 0.20044           
    ##  Mean   :-0.16236            Mean   : 0.20017           
    ##  3rd Qu.:-0.02141            3rd Qu.: 0.30070           
    ##  Max.   : 1.00000            Max.   : 1.00000           
    ##  tBodyGyroJerk.arCoeff...Y.3 tBodyGyroJerk.arCoeff...Y.4
    ##  Min.   :-1.00000            Min.   :-1.00000           
    ##  1st Qu.:-0.04272            1st Qu.:-0.07910           
    ##  Median : 0.09034            Median : 0.08264           
    ##  Mean   : 0.08353            Mean   : 0.08025           
    ##  3rd Qu.: 0.21606            3rd Qu.: 0.24745           
    ##  Max.   : 1.00000            Max.   : 1.00000           
    ##  tBodyGyroJerk.arCoeff...Z.1 tBodyGyroJerk.arCoeff...Z.2
    ##  Min.   :-1.00000            Min.   :-1.00000           
    ##  1st Qu.:-0.30897            1st Qu.:-0.10806           
    ##  Median :-0.05945            Median : 0.04328           
    ##  Mean   :-0.02865            Mean   : 0.05238           
    ##  3rd Qu.: 0.24832            3rd Qu.: 0.21179           
    ##  Max.   : 1.00000            Max.   : 1.00000           
    ##  tBodyGyroJerk.arCoeff...Z.3 tBodyGyroJerk.arCoeff...Z.4
    ##  Min.   :-1.00000            Min.   :-1.00000           
    ##  1st Qu.:-0.07285            1st Qu.:-0.13529           
    ##  Median : 0.09581            Median : 0.03963           
    ##  Mean   : 0.08880            Mean   : 0.03353           
    ##  3rd Qu.: 0.26285            3rd Qu.: 0.20765           
    ##  Max.   : 1.00000            Max.   : 1.00000           
    ##  tBodyGyroJerk.correlation...X.Y tBodyGyroJerk.correlation...X.Z
    ##  Min.   :-1.00000                Min.   :-1.00000               
    ##  1st Qu.:-0.15318                1st Qu.:-0.12235               
    ##  Median : 0.03434                Median : 0.04687               
    ##  Mean   : 0.03694                Mean   : 0.04927               
    ##  3rd Qu.: 0.22412                3rd Qu.: 0.21867               
    ##  Max.   : 1.00000                Max.   : 1.00000               
    ##  tBodyGyroJerk.correlation...Y.Z tBodyAccMag.mean.. tBodyAccMag.std..
    ##  Min.   :-1.00000                Min.   :-1.0000    Min.   :-1.0000  
    ##  1st Qu.:-0.28529                1st Qu.:-0.9819    1st Qu.:-0.9822  
    ##  Median :-0.11671                Median :-0.8746    Median :-0.8437  
    ##  Mean   :-0.11363                Mean   :-0.5482    Mean   :-0.5912  
    ##  3rd Qu.: 0.04804                3rd Qu.:-0.1201    3rd Qu.:-0.2423  
    ##  Max.   : 1.00000                Max.   : 1.0000    Max.   : 1.0000  
    ##  tBodyAccMag.mad.. tBodyAccMag.max.. tBodyAccMag.min.. tBodyAccMag.sma..
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.9838   1st Qu.:-0.9801   1st Qu.:-0.9925   1st Qu.:-0.9819  
    ##  Median :-0.8623   Median :-0.8492   Median :-0.9665   Median :-0.8746  
    ##  Mean   :-0.6427   Mean   :-0.5586   Mean   :-0.8378   Mean   :-0.5482  
    ##  3rd Qu.:-0.3367   3rd Qu.:-0.1700   3rd Qu.:-0.6947   3rd Qu.:-0.1201  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  tBodyAccMag.energy.. tBodyAccMag.iqr.. tBodyAccMag.entropy..
    ##  Min.   :-1.0000      Min.   :-1.0000   Min.   :-1.0000      
    ##  1st Qu.:-0.9996      1st Qu.:-0.9845   1st Qu.:-0.5456      
    ##  Median :-0.9888      Median :-0.8818   Median : 0.1650      
    ##  Mean   :-0.7775      Mean   :-0.7026   Mean   : 0.1328      
    ##  3rd Qu.:-0.6003      3rd Qu.:-0.4510   3rd Qu.: 0.8010      
    ##  Max.   : 1.0000      Max.   : 1.0000   Max.   : 1.0000      
    ##  tBodyAccMag.arCoeff..1 tBodyAccMag.arCoeff..2 tBodyAccMag.arCoeff..3
    ##  Min.   :-1.00000       Min.   :-1.00000       Min.   :-1.00000      
    ##  1st Qu.:-0.28583       1st Qu.:-0.14884       1st Qu.:-0.11088      
    ##  Median :-0.07185       Median : 0.01791       Median : 0.06458      
    ##  Mean   :-0.06954       Mean   : 0.02404       Mean   : 0.05858      
    ##  3rd Qu.: 0.13812       3rd Qu.: 0.18678       3rd Qu.: 0.23483      
    ##  Max.   : 1.00000       Max.   : 1.00000       Max.   : 1.00000      
    ##  tBodyAccMag.arCoeff..4 tGravityAccMag.mean.. tGravityAccMag.std..
    ##  Min.   :-1.00000       Min.   :-1.0000       Min.   :-1.0000     
    ##  1st Qu.:-0.23557       1st Qu.:-0.9819       1st Qu.:-0.9822     
    ##  Median :-0.05369       Median :-0.8746       Median :-0.8437     
    ##  Mean   :-0.05789       Mean   :-0.5482       Mean   :-0.5912     
    ##  3rd Qu.: 0.12264       3rd Qu.:-0.1201       3rd Qu.:-0.2423     
    ##  Max.   : 1.00000       Max.   : 1.0000       Max.   : 1.0000     
    ##  tGravityAccMag.mad.. tGravityAccMag.max.. tGravityAccMag.min..
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.9838      1st Qu.:-0.9801      1st Qu.:-0.9925     
    ##  Median :-0.8623      Median :-0.8492      Median :-0.9665     
    ##  Mean   :-0.6427      Mean   :-0.5586      Mean   :-0.8378     
    ##  3rd Qu.:-0.3367      3rd Qu.:-0.1700      3rd Qu.:-0.6947     
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000     
    ##  tGravityAccMag.sma.. tGravityAccMag.energy.. tGravityAccMag.iqr..
    ##  Min.   :-1.0000      Min.   :-1.0000         Min.   :-1.0000     
    ##  1st Qu.:-0.9819      1st Qu.:-0.9996         1st Qu.:-0.9845     
    ##  Median :-0.8746      Median :-0.9888         Median :-0.8818     
    ##  Mean   :-0.5482      Mean   :-0.7775         Mean   :-0.7026     
    ##  3rd Qu.:-0.1201      3rd Qu.:-0.6003         3rd Qu.:-0.4510     
    ##  Max.   : 1.0000      Max.   : 1.0000         Max.   : 1.0000     
    ##  tGravityAccMag.entropy.. tGravityAccMag.arCoeff..1 tGravityAccMag.arCoeff..2
    ##  Min.   :-1.0000          Min.   :-1.00000          Min.   :-1.00000         
    ##  1st Qu.:-0.5456          1st Qu.:-0.28583          1st Qu.:-0.14884         
    ##  Median : 0.1650          Median :-0.07185          Median : 0.01791         
    ##  Mean   : 0.1328          Mean   :-0.06954          Mean   : 0.02404         
    ##  3rd Qu.: 0.8010          3rd Qu.: 0.13812          3rd Qu.: 0.18678         
    ##  Max.   : 1.0000          Max.   : 1.00000          Max.   : 1.00000         
    ##  tGravityAccMag.arCoeff..3 tGravityAccMag.arCoeff..4 tBodyAccJerkMag.mean..
    ##  Min.   :-1.00000          Min.   :-1.00000          Min.   :-1.0000       
    ##  1st Qu.:-0.11088          1st Qu.:-0.23557          1st Qu.:-0.9896       
    ##  Median : 0.06458          Median :-0.05369          Median :-0.9481       
    ##  Mean   : 0.05858          Mean   :-0.05789          Mean   :-0.6494       
    ##  3rd Qu.: 0.23483          3rd Qu.: 0.12264          3rd Qu.:-0.2956       
    ##  Max.   : 1.00000          Max.   : 1.00000          Max.   : 1.0000       
    ##  tBodyAccJerkMag.std.. tBodyAccJerkMag.mad.. tBodyAccJerkMag.max..
    ##  Min.   :-1.0000       Min.   :-1.0000       Min.   :-1.0000      
    ##  1st Qu.:-0.9907       1st Qu.:-0.9913       1st Qu.:-0.9891      
    ##  Median :-0.9288       Median :-0.9383       Median :-0.9262      
    ##  Mean   :-0.6278       Mean   :-0.6469       Mean   :-0.6387      
    ##  3rd Qu.:-0.2733       3rd Qu.:-0.3093       3rd Qu.:-0.2978      
    ##  Max.   : 1.0000       Max.   : 1.0000       Max.   : 1.0000      
    ##  tBodyAccJerkMag.min.. tBodyAccJerkMag.sma.. tBodyAccJerkMag.energy..
    ##  Min.   :-1.0000       Min.   :-1.0000       Min.   :-1.0000         
    ##  1st Qu.:-0.9832       1st Qu.:-0.9896       1st Qu.:-0.9998         
    ##  Median :-0.9617       Median :-0.9481       Median :-0.9975         
    ##  Mean   :-0.7876       Mean   :-0.6494       Mean   :-0.8511         
    ##  3rd Qu.:-0.6108       3rd Qu.:-0.2956       3rd Qu.:-0.7323         
    ##  Max.   : 1.0000       Max.   : 1.0000       Max.   : 1.0000         
    ##  tBodyAccJerkMag.iqr.. tBodyAccJerkMag.entropy.. tBodyAccJerkMag.arCoeff..1
    ##  Min.   :-1.0000       Min.   :-1.00000          Min.   :-1.00000          
    ##  1st Qu.:-0.9920       1st Qu.:-0.80162          1st Qu.:-0.09056          
    ##  Median :-0.9547       Median :-0.32044          Median : 0.09303          
    ##  Mean   :-0.6992       Mean   :-0.06163          Mean   : 0.07876          
    ##  3rd Qu.:-0.4114       3rd Qu.: 0.70273          3rd Qu.: 0.25887          
    ##  Max.   : 1.0000       Max.   : 1.00000          Max.   : 1.00000          
    ##  tBodyAccJerkMag.arCoeff..2 tBodyAccJerkMag.arCoeff..3
    ##  Min.   :-1.00000           Min.   :-1.00000          
    ##  1st Qu.:-0.20842           1st Qu.:-0.25150          
    ##  Median :-0.05595           Median :-0.09390          
    ##  Mean   :-0.02904           Mean   :-0.09117          
    ##  3rd Qu.: 0.13693           3rd Qu.: 0.06532          
    ##  Max.   : 1.00000           Max.   : 1.00000          
    ##  tBodyAccJerkMag.arCoeff..4 tBodyGyroMag.mean.. tBodyGyroMag.std..
    ##  Min.   :-1.00000           Min.   :-1.0000     Min.   :-1.0000   
    ##  1st Qu.:-0.22067           1st Qu.:-0.9781     1st Qu.:-0.9775   
    ##  Median :-0.04614           Median :-0.8223     Median :-0.8259   
    ##  Mean   :-0.04167           Mean   :-0.6052     Mean   :-0.6625   
    ##  3rd Qu.: 0.13215           3rd Qu.:-0.2454     3rd Qu.:-0.3940   
    ##  Max.   : 1.00000           Max.   : 1.0000     Max.   : 1.0000   
    ##  tBodyGyroMag.mad.. tBodyGyroMag.max.. tBodyGyroMag.min.. tBodyGyroMag.sma..
    ##  Min.   :-1.0000    Min.   :-1.0000    Min.   :-1.0000    Min.   :-1.0000   
    ##  1st Qu.:-0.9758    1st Qu.:-0.9807    1st Qu.:-0.9884    1st Qu.:-0.9781   
    ##  Median :-0.8067    Median :-0.8490    Median :-0.8810    Median :-0.8223   
    ##  Mean   :-0.6302    Mean   :-0.6952    Mean   :-0.7300    Mean   :-0.6052   
    ##  3rd Qu.:-0.3330    3rd Qu.:-0.4523    3rd Qu.:-0.5283    3rd Qu.:-0.2454   
    ##  Max.   : 1.0000    Max.   : 1.0000    Max.   : 1.0000    Max.   : 1.0000   
    ##  tBodyGyroMag.energy.. tBodyGyroMag.iqr.. tBodyGyroMag.entropy..
    ##  Min.   :-1.0000       Min.   :-1.0000    Min.   :-1.0000       
    ##  1st Qu.:-0.9996       1st Qu.:-0.9788    1st Qu.:-0.1524       
    ##  Median :-0.9809       Median :-0.8164    Median : 0.2850       
    ##  Mean   :-0.8348       Mean   :-0.6537    Mean   : 0.2354       
    ##  3rd Qu.:-0.7034       3rd Qu.:-0.3718    3rd Qu.: 0.6574       
    ##  Max.   : 1.0000       Max.   : 1.0000    Max.   : 1.0000       
    ##  tBodyGyroMag.arCoeff..1 tBodyGyroMag.arCoeff..2 tBodyGyroMag.arCoeff..3
    ##  Min.   :-1.00000        Min.   :-1.00000        Min.   :-1.00000       
    ##  1st Qu.:-0.22800        1st Qu.:-0.25508        1st Qu.:-0.05679       
    ##  Median :-0.03027        Median :-0.07579        Median : 0.10978       
    ##  Mean   :-0.02446        Mean   :-0.06924        Mean   : 0.10761       
    ##  3rd Qu.: 0.17678        3rd Qu.: 0.10639        3rd Qu.: 0.27344       
    ##  Max.   : 1.00000        Max.   : 1.00000        Max.   : 1.00000       
    ##  tBodyGyroMag.arCoeff..4 tBodyGyroJerkMag.mean.. tBodyGyroJerkMag.std..
    ##  Min.   :-1.00000        Min.   :-1.0000         Min.   :-1.0000       
    ##  1st Qu.:-0.21671        1st Qu.:-0.9923         1st Qu.:-0.9922       
    ##  Median :-0.05272        Median :-0.9559         Median :-0.9403       
    ##  Mean   :-0.05445        Mean   :-0.7621         Mean   :-0.7780       
    ##  3rd Qu.: 0.11127        3rd Qu.:-0.5499         3rd Qu.:-0.6093       
    ##  Max.   : 1.00000        Max.   : 1.0000         Max.   : 1.0000       
    ##  tBodyGyroJerkMag.mad.. tBodyGyroJerkMag.max.. tBodyGyroJerkMag.min..
    ##  Min.   :-1.0000        Min.   :-1.0000        Min.   :-1.0000       
    ##  1st Qu.:-0.9929        1st Qu.:-0.9919        1st Qu.:-0.9900       
    ##  Median :-0.9485        Median :-0.9396        Median :-0.9709       
    ##  Mean   :-0.7926        Mean   :-0.7846        Mean   :-0.8029       
    ##  3rd Qu.:-0.6289        3rd Qu.:-0.6314        3rd Qu.:-0.6425       
    ##  Max.   : 1.0000        Max.   : 1.0000        Max.   : 1.0000       
    ##  tBodyGyroJerkMag.sma.. tBodyGyroJerkMag.energy.. tBodyGyroJerkMag.iqr..
    ##  Min.   :-1.0000        Min.   :-1.0000           Min.   :-1.0000       
    ##  1st Qu.:-0.9923        1st Qu.:-0.9999           1st Qu.:-0.9938       
    ##  Median :-0.9559        Median :-0.9985           Median :-0.9577       
    ##  Mean   :-0.7621        Mean   :-0.9319           Mean   :-0.8065       
    ##  3rd Qu.:-0.5499        3rd Qu.:-0.9014           3rd Qu.:-0.6448       
    ##  Max.   : 1.0000        Max.   : 1.0000           Max.   : 1.0000       
    ##  tBodyGyroJerkMag.entropy.. tBodyGyroJerkMag.arCoeff..1
    ##  Min.   :-1.00000           Min.   :-1.0000            
    ##  1st Qu.:-0.57206           1st Qu.: 0.1410            
    ##  Median : 0.08403           Median : 0.2989            
    ##  Mean   : 0.13067           Mean   : 0.2865            
    ##  3rd Qu.: 0.84250           3rd Qu.: 0.4533            
    ##  Max.   : 1.00000           Max.   : 1.0000            
    ##  tBodyGyroJerkMag.arCoeff..2 tBodyGyroJerkMag.arCoeff..3
    ##  Min.   :-1.00000            Min.   :-1.00000           
    ##  1st Qu.:-0.37015            1st Qu.:-0.20102           
    ##  Median :-0.23446            Median :-0.05870           
    ##  Mean   :-0.22694            Mean   :-0.05748           
    ##  3rd Qu.:-0.09745            3rd Qu.: 0.09032           
    ##  Max.   : 1.00000            Max.   : 1.00000           
    ##  tBodyGyroJerkMag.arCoeff..4 fBodyAcc.mean...X fBodyAcc.mean...Y
    ##  Min.   :-1.00000            Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.27936            1st Qu.:-0.9913   1st Qu.:-0.9792  
    ##  Median :-0.11873            Median :-0.9456   Median :-0.8643  
    ##  Mean   :-0.10669            Mean   :-0.6228   Mean   :-0.5375  
    ##  3rd Qu.: 0.05156            3rd Qu.:-0.2646   3rd Qu.:-0.1032  
    ##  Max.   : 1.00000            Max.   : 1.0000   Max.   : 1.0000  
    ##  fBodyAcc.mean...Z fBodyAcc.std...X  fBodyAcc.std...Y   fBodyAcc.std...Z 
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.00000   Min.   :-1.0000  
    ##  1st Qu.:-0.9832   1st Qu.:-0.9929   1st Qu.:-0.97689   1st Qu.:-0.9780  
    ##  Median :-0.8954   Median :-0.9416   Median :-0.83261   Median :-0.8398  
    ##  Mean   :-0.6650   Mean   :-0.6034   Mean   :-0.52842   Mean   :-0.6179  
    ##  3rd Qu.:-0.3662   3rd Qu.:-0.2493   3rd Qu.:-0.09216   3rd Qu.:-0.3023  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.00000   Max.   : 1.0000  
    ##  fBodyAcc.mad...X  fBodyAcc.mad...Y  fBodyAcc.mad...Z  fBodyAcc.max...X 
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.9916   1st Qu.:-0.9801   1st Qu.:-0.9809   1st Qu.:-0.9943  
    ##  Median :-0.9442   Median :-0.8571   Median :-0.8780   Median :-0.9434  
    ##  Mean   :-0.5908   Mean   :-0.5222   Mean   :-0.6323   Mean   :-0.6515  
    ##  3rd Qu.:-0.2026   3rd Qu.:-0.0789   3rd Qu.:-0.3056   3rd Qu.:-0.3516  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  fBodyAcc.max...Y  fBodyAcc.max...Z  fBodyAcc.min...X  fBodyAcc.min...Y 
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.9800   1st Qu.:-0.9739   1st Qu.:-0.9946   1st Qu.:-0.9920  
    ##  Median :-0.8580   Median :-0.8160   Median :-0.9767   Median :-0.9688  
    ##  Mean   :-0.6581   Mean   :-0.6357   Mean   :-0.8560   Mean   :-0.8802  
    ##  3rd Qu.:-0.3615   3rd Qu.:-0.3667   3rd Qu.:-0.7901   3rd Qu.:-0.8246  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  fBodyAcc.min...Z  fBodyAcc.sma..    fBodyAcc.energy...X fBodyAcc.energy...Y
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000     Min.   :-1.0000    
    ##  1st Qu.:-0.9916   1st Qu.:-0.9858   1st Qu.:-0.9999     1st Qu.:-0.9995    
    ##  Median :-0.9745   Median :-0.8983   Median :-0.9981     Median :-0.9849    
    ##  Mean   :-0.9138   Mean   :-0.5552   Mean   :-0.8259     Mean   :-0.7525    
    ##  3rd Qu.:-0.8804   3rd Qu.:-0.1147   3rd Qu.:-0.7170     3rd Qu.:-0.5507    
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000     Max.   : 1.0000    
    ##  fBodyAcc.energy...Z fBodyAcc.iqr...X  fBodyAcc.iqr...Y  fBodyAcc.iqr...Z 
    ##  Min.   :-1.0000     Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.9994     1st Qu.:-0.9898   1st Qu.:-0.9866   1st Qu.:-0.9828  
    ##  Median :-0.9866     Median :-0.9434   Median :-0.9240   Median :-0.9384  
    ##  Mean   :-0.8399     Mean   :-0.6526   Mean   :-0.6499   Mean   :-0.7468  
    ##  3rd Qu.:-0.7322     3rd Qu.:-0.3283   3rd Qu.:-0.3137   3rd Qu.:-0.5342  
    ##  Max.   : 1.0000     Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  fBodyAcc.entropy...X fBodyAcc.entropy...Y fBodyAcc.entropy...Z
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.9464      1st Qu.:-0.8704      1st Qu.:-0.8135     
    ##  Median :-0.4980      Median :-0.3638      Median :-0.3852     
    ##  Mean   :-0.2003      Mean   :-0.1839      Mean   :-0.2048     
    ##  3rd Qu.: 0.5492      3rd Qu.: 0.5017      3rd Qu.: 0.4097     
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000     
    ##  fBodyAcc.maxInds.X fBodyAcc.maxInds.Y fBodyAcc.maxInds.Z fBodyAcc.meanFreq...X
    ##  Min.   :-1.0000    Min.   :-1.0000    Min.   :-1.0000    Min.   :-1.00000     
    ##  1st Qu.:-1.0000    1st Qu.:-1.0000    1st Qu.:-1.0000    1st Qu.:-0.41878     
    ##  Median :-0.8065    Median :-0.8000    Median :-0.9231    Median :-0.23825     
    ##  Mean   :-0.7599    Mean   :-0.7973    Mean   :-0.8438    Mean   :-0.22147     
    ##  3rd Qu.:-0.7419    3rd Qu.:-0.7333    3rd Qu.:-0.7692    3rd Qu.:-0.02043     
    ##  Max.   : 1.0000    Max.   : 1.0000    Max.   : 1.0000    Max.   : 1.00000     
    ##  fBodyAcc.meanFreq...Y fBodyAcc.meanFreq...Z fBodyAcc.skewness...X
    ##  Min.   :-1.000000     Min.   :-1.00000      Min.   :-1.0000      
    ##  1st Qu.:-0.144772     1st Qu.:-0.13845      1st Qu.:-0.4893      
    ##  Median : 0.004666     Median : 0.06084      Median :-0.1735      
    ##  Mean   : 0.015401     Mean   : 0.04731      Mean   :-0.1359      
    ##  3rd Qu.: 0.176603     3rd Qu.: 0.24922      3rd Qu.: 0.1522      
    ##  Max.   : 1.000000     Max.   : 1.00000      Max.   : 1.0000      
    ##  fBodyAcc.kurtosis...X fBodyAcc.skewness...Y fBodyAcc.kurtosis...Y
    ##  Min.   :-1.0000       Min.   :-1.00000      Min.   :-1.0000      
    ##  1st Qu.:-0.8447       1st Qu.:-0.52560      1st Qu.:-0.8501      
    ##  Median :-0.5844       Median :-0.35897      Median :-0.7204      
    ##  Mean   :-0.4650       Mean   :-0.26805      Mean   :-0.5822      
    ##  3rd Qu.:-0.1792       3rd Qu.:-0.09974      3rd Qu.:-0.4524      
    ##  Max.   : 1.0000       Max.   : 1.00000      Max.   : 1.0000      
    ##  fBodyAcc.skewness...Z fBodyAcc.kurtosis...Z fBodyAcc.bandsEnergy...1.8
    ##  Min.   :-1.00000      Min.   :-1.0000       Min.   :-1.0000           
    ##  1st Qu.:-0.56232      1st Qu.:-0.8080       1st Qu.:-1.0000           
    ##  Median :-0.31395      Median :-0.6042       Median :-0.9980           
    ##  Mean   :-0.24117      Mean   :-0.4795       Mean   :-0.8180           
    ##  3rd Qu.: 0.03257      3rd Qu.:-0.2365       3rd Qu.:-0.7142           
    ##  Max.   : 1.00000      Max.   : 1.0000       Max.   : 1.0000           
    ##  fBodyAcc.bandsEnergy...9.16 fBodyAcc.bandsEnergy...17.24
    ##  Min.   :-1.0000             Min.   :-1.0000             
    ##  1st Qu.:-0.9999             1st Qu.:-0.9999             
    ##  Median :-0.9990             Median :-0.9982             
    ##  Mean   :-0.8875             Mean   :-0.8583             
    ##  3rd Qu.:-0.8223             3rd Qu.:-0.7823             
    ##  Max.   : 1.0000             Max.   : 1.0000             
    ##  fBodyAcc.bandsEnergy...25.32 fBodyAcc.bandsEnergy...33.40
    ##  Min.   :-1.0000              Min.   :-1.0000             
    ##  1st Qu.:-0.9999              1st Qu.:-0.9999             
    ##  Median :-0.9981              Median :-0.9986             
    ##  Mean   :-0.8941              Mean   :-0.9150             
    ##  3rd Qu.:-0.8563              3rd Qu.:-0.8773             
    ##  Max.   : 1.0000              Max.   : 1.0000             
    ##  fBodyAcc.bandsEnergy...41.48 fBodyAcc.bandsEnergy...49.56
    ##  Min.   :-1.0000              Min.   :-1.0000             
    ##  1st Qu.:-0.9999              1st Qu.:-0.9999             
    ##  Median :-0.9985              Median :-0.9991             
    ##  Mean   :-0.9128              Mean   :-0.9460             
    ##  3rd Qu.:-0.8754              3rd Qu.:-0.9297             
    ##  Max.   : 1.0000              Max.   : 1.0000             
    ##  fBodyAcc.bandsEnergy...57.64 fBodyAcc.bandsEnergy...1.16
    ##  Min.   :-1.0000              Min.   :-1.0000            
    ##  1st Qu.:-1.0000              1st Qu.:-0.9999            
    ##  Median :-0.9996              Median :-0.9981            
    ##  Mean   :-0.9558              Mean   :-0.8216            
    ##  3rd Qu.:-0.9728              3rd Qu.:-0.7125            
    ##  Max.   : 1.0000              Max.   : 1.0000            
    ##  fBodyAcc.bandsEnergy...17.32 fBodyAcc.bandsEnergy...33.48
    ##  Min.   :-1.0000              Min.   :-1.0000             
    ##  1st Qu.:-0.9999              1st Qu.:-0.9999             
    ##  Median :-0.9979              Median :-0.9985             
    ##  Mean   :-0.8468              Mean   :-0.9142             
    ##  3rd Qu.:-0.7622              3rd Qu.:-0.8733             
    ##  Max.   : 1.0000              Max.   : 1.0000             
    ##  fBodyAcc.bandsEnergy...49.64 fBodyAcc.bandsEnergy...1.24
    ##  Min.   :-1.0000              Min.   :-1.0000            
    ##  1st Qu.:-1.0000              1st Qu.:-0.9999            
    ##  Median :-0.9991              Median :-0.9981            
    ##  Mean   :-0.9493              Mean   :-0.8242            
    ##  3rd Qu.:-0.9408              3rd Qu.:-0.7136            
    ##  Max.   : 1.0000              Max.   : 1.0000            
    ##  fBodyAcc.bandsEnergy...25.48 fBodyAcc.bandsEnergy...1.8.1
    ##  Min.   :-1.0000              Min.   :-1.0000             
    ##  1st Qu.:-0.9998              1st Qu.:-0.9995             
    ##  Median :-0.9979              Median :-0.9823             
    ##  Mean   :-0.8831              Mean   :-0.7878             
    ##  3rd Qu.:-0.8300              3rd Qu.:-0.6202             
    ##  Max.   : 1.0000              Max.   : 1.0000             
    ##  fBodyAcc.bandsEnergy...9.16.1 fBodyAcc.bandsEnergy...17.24.1
    ##  Min.   :-1.0000               Min.   :-1.0000               
    ##  1st Qu.:-0.9998               1st Qu.:-0.9998               
    ##  Median :-0.9965               Median :-0.9972               
    ##  Mean   :-0.8474               Mean   :-0.8603               
    ##  3rd Qu.:-0.7654               3rd Qu.:-0.7781               
    ##  Max.   : 1.0000               Max.   : 1.0000               
    ##  fBodyAcc.bandsEnergy...25.32.1 fBodyAcc.bandsEnergy...33.40.1
    ##  Min.   :-1.0000                Min.   :-1.0000               
    ##  1st Qu.:-0.9997                1st Qu.:-0.9996               
    ##  Median :-0.9970                Median :-0.9958               
    ##  Mean   :-0.9028                Mean   :-0.8974               
    ##  3rd Qu.:-0.8590                3rd Qu.:-0.8489               
    ##  Max.   : 1.0000                Max.   : 1.0000               
    ##  fBodyAcc.bandsEnergy...41.48.1 fBodyAcc.bandsEnergy...49.56.1
    ##  Min.   :-1.0000                Min.   :-1.0000               
    ##  1st Qu.:-0.9996                1st Qu.:-0.9997               
    ##  Median :-0.9948                Median :-0.9946               
    ##  Mean   :-0.8827                Mean   :-0.9025               
    ##  3rd Qu.:-0.8218                3rd Qu.:-0.8650               
    ##  Max.   : 1.0000                Max.   : 1.0000               
    ##  fBodyAcc.bandsEnergy...57.64.1 fBodyAcc.bandsEnergy...1.16.1
    ##  Min.   :-1.0000                Min.   :-1.0000              
    ##  1st Qu.:-0.9999                1st Qu.:-0.9995              
    ##  Median :-0.9988                Median :-0.9840              
    ##  Mean   :-0.9515                Mean   :-0.7589              
    ##  3rd Qu.:-0.9683                3rd Qu.:-0.5661              
    ##  Max.   : 1.0000                Max.   : 1.0000              
    ##  fBodyAcc.bandsEnergy...17.32.1 fBodyAcc.bandsEnergy...33.48.1
    ##  Min.   :-1.0000                Min.   :-1.0000               
    ##  1st Qu.:-0.9998                1st Qu.:-0.9996               
    ##  Median :-0.9964                Median :-0.9949               
    ##  Mean   :-0.8369                Mean   :-0.8800               
    ##  3rd Qu.:-0.7342                3rd Qu.:-0.8156               
    ##  Max.   : 1.0000                Max.   : 1.0000               
    ##  fBodyAcc.bandsEnergy...49.64.1 fBodyAcc.bandsEnergy...1.24.1
    ##  Min.   :-1.0000                Min.   :-1.0000              
    ##  1st Qu.:-0.9998                1st Qu.:-0.9995              
    ##  Median :-0.9950                Median :-0.9848              
    ##  Mean   :-0.9200                Mean   :-0.7550              
    ##  3rd Qu.:-0.8972                3rd Qu.:-0.5560              
    ##  Max.   : 1.0000                Max.   : 1.0000              
    ##  fBodyAcc.bandsEnergy...25.48.1 fBodyAcc.bandsEnergy...1.8.2
    ##  Min.   :-1.0000                Min.   :-1.0000             
    ##  1st Qu.:-0.9997                1st Qu.:-0.9994             
    ##  Median :-0.9961                Median :-0.9845             
    ##  Mean   :-0.8907                Mean   :-0.8572             
    ##  3rd Qu.:-0.8317                3rd Qu.:-0.7775             
    ##  Max.   : 1.0000                Max.   : 1.0000             
    ##  fBodyAcc.bandsEnergy...9.16.2 fBodyAcc.bandsEnergy...17.24.2
    ##  Min.   :-1.0000               Min.   :-1.0000               
    ##  1st Qu.:-0.9997               1st Qu.:-0.9997               
    ##  Median :-0.9966               Median :-0.9981               
    ##  Mean   :-0.8994               Mean   :-0.9258               
    ##  3rd Qu.:-0.8587               3rd Qu.:-0.9046               
    ##  Max.   : 1.0000               Max.   : 1.0000               
    ##  fBodyAcc.bandsEnergy...25.32.2 fBodyAcc.bandsEnergy...33.40.2
    ##  Min.   :-1.0000                Min.   :-1.0000               
    ##  1st Qu.:-0.9998                1st Qu.:-0.9997               
    ##  Median :-0.9986                Median :-0.9982               
    ##  Mean   :-0.9619                Mean   :-0.9639               
    ##  3rd Qu.:-0.9543                3rd Qu.:-0.9548               
    ##  Max.   : 1.0000                Max.   : 1.0000               
    ##  fBodyAcc.bandsEnergy...41.48.2 fBodyAcc.bandsEnergy...49.56.2
    ##  Min.   :-1.0000                Min.   :-1.0000               
    ##  1st Qu.:-0.9995                1st Qu.:-0.9996               
    ##  Median :-0.9965                Median :-0.9956               
    ##  Mean   :-0.9377                Mean   :-0.9416               
    ##  3rd Qu.:-0.9174                3rd Qu.:-0.9248               
    ##  Max.   : 1.0000                Max.   : 1.0000               
    ##  fBodyAcc.bandsEnergy...57.64.2 fBodyAcc.bandsEnergy...1.16.2
    ##  Min.   :-1.0000                Min.   :-1.0000              
    ##  1st Qu.:-0.9999                1st Qu.:-0.9995              
    ##  Median :-0.9986                Median :-0.9865              
    ##  Mean   :-0.9579                Mean   :-0.8535              
    ##  3rd Qu.:-0.9729                3rd Qu.:-0.7626              
    ##  Max.   : 1.0000                Max.   : 1.0000              
    ##  fBodyAcc.bandsEnergy...17.32.2 fBodyAcc.bandsEnergy...33.48.2
    ##  Min.   :-1.0000                Min.   :-1.0000               
    ##  1st Qu.:-0.9998                1st Qu.:-0.9997               
    ##  Median :-0.9984                Median :-0.9976               
    ##  Mean   :-0.9389                Mean   :-0.9545               
    ##  3rd Qu.:-0.9187                3rd Qu.:-0.9403               
    ##  Max.   : 1.0000                Max.   : 1.0000               
    ##  fBodyAcc.bandsEnergy...49.64.2 fBodyAcc.bandsEnergy...1.24.2
    ##  Min.   :-1.0000                Min.   :-1.0000              
    ##  1st Qu.:-0.9997                1st Qu.:-0.9994              
    ##  Median :-0.9954                Median :-0.9866              
    ##  Mean   :-0.9460                Mean   :-0.8433              
    ##  3rd Qu.:-0.9340                3rd Qu.:-0.7408              
    ##  Max.   : 1.0000                Max.   : 1.0000              
    ##  fBodyAcc.bandsEnergy...25.48.2 fBodyAccJerk.mean...X fBodyAccJerk.mean...Y
    ##  Min.   :-1.0000                Min.   :-1.0000       Min.   :-1.0000      
    ##  1st Qu.:-0.9998                1st Qu.:-0.9912       1st Qu.:-0.9848      
    ##  Median :-0.9983                Median :-0.9516       Median :-0.9257      
    ##  Mean   :-0.9598                Mean   :-0.6567       Mean   :-0.6290      
    ##  3rd Qu.:-0.9485                3rd Qu.:-0.3270       3rd Qu.:-0.2638      
    ##  Max.   : 1.0000                Max.   : 1.0000       Max.   : 1.0000      
    ##  fBodyAccJerk.mean...Z fBodyAccJerk.std...X fBodyAccJerk.std...Y
    ##  Min.   :-1.0000       Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.9873       1st Qu.:-0.9920      1st Qu.:-0.9865     
    ##  Median :-0.9475       Median :-0.9562      Median :-0.9280     
    ##  Mean   :-0.7436       Mean   :-0.6550      Mean   :-0.6122     
    ##  3rd Qu.:-0.5133       3rd Qu.:-0.3203      3rd Qu.:-0.2361     
    ##  Max.   : 1.0000       Max.   : 1.0000      Max.   : 1.0000     
    ##  fBodyAccJerk.std...Z fBodyAccJerk.mad...X fBodyAccJerk.mad...Y
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.9895      1st Qu.:-0.9897      1st Qu.:-0.9864     
    ##  Median :-0.9590      Median :-0.9453      Median :-0.9287     
    ##  Mean   :-0.7809      Mean   :-0.5950      Mean   :-0.6240     
    ##  3rd Qu.:-0.5903      3rd Qu.:-0.2015      3rd Qu.:-0.2556     
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000     
    ##  fBodyAccJerk.mad...Z fBodyAccJerk.max...X fBodyAccJerk.max...Y
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.9884      1st Qu.:-0.9936      1st Qu.:-0.9881     
    ##  Median :-0.9550      Median :-0.9675      Median :-0.9430     
    ##  Mean   :-0.7645      Mean   :-0.7116      Mean   :-0.6821     
    ##  3rd Qu.:-0.5575      3rd Qu.:-0.4447      3rd Qu.:-0.3918     
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000     
    ##  fBodyAccJerk.max...Z fBodyAccJerk.min...X fBodyAccJerk.min...Y
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.9898      1st Qu.:-0.9950      1st Qu.:-0.9911     
    ##  Median :-0.9629      Median :-0.9829      Median :-0.9708     
    ##  Mean   :-0.8012      Mean   :-0.8827      Mean   :-0.8602     
    ##  3rd Qu.:-0.6400      3rd Qu.:-0.8304      3rd Qu.:-0.7977     
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000     
    ##  fBodyAccJerk.min...Z fBodyAccJerk.sma.. fBodyAccJerk.energy...X
    ##  Min.   :-1.0000      Min.   :-1.0000    Min.   :-1.0000        
    ##  1st Qu.:-0.9890      1st Qu.:-0.9884    1st Qu.:-0.9999        
    ##  Median :-0.9690      Median :-0.9357    Median :-0.9985        
    ##  Mean   :-0.8844      Mean   :-0.6203    Mean   :-0.8502        
    ##  3rd Qu.:-0.8453      3rd Qu.:-0.2445    3rd Qu.:-0.7457        
    ##  Max.   : 1.0000      Max.   : 1.0000    Max.   : 1.0000        
    ##  fBodyAccJerk.energy...Y fBodyAccJerk.energy...Z fBodyAccJerk.iqr...X
    ##  Min.   :-1.0000         Min.   :-1.0000         Min.   :-1.0000     
    ##  1st Qu.:-0.9997         1st Qu.:-0.9998         1st Qu.:-0.9892     
    ##  Median :-0.9965         Median :-0.9983         Median :-0.9456     
    ##  Mean   :-0.8273         Mean   :-0.9307         Mean   :-0.6341     
    ##  3rd Qu.:-0.6926         3rd Qu.:-0.8929         3rd Qu.:-0.2984     
    ##  Max.   : 1.0000         Max.   : 1.0000         Max.   : 1.0000     
    ##  fBodyAccJerk.iqr...Y fBodyAccJerk.iqr...Z fBodyAccJerk.entropy...X
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000         
    ##  1st Qu.:-0.9881      1st Qu.:-0.9856      1st Qu.:-1.0000         
    ##  Median :-0.9440      Median :-0.9509      Median :-0.6790         
    ##  Mean   :-0.7199      Mean   :-0.7715      Mean   :-0.2669         
    ##  3rd Qu.:-0.4543      3rd Qu.:-0.5839      3rd Qu.: 0.5196         
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000         
    ##  fBodyAccJerk.entropy...Y fBodyAccJerk.entropy...Z fBodyAccJerk.maxInds.X
    ##  Min.   :-1.0000          Min.   :-1.0000          Min.   :-1.0000       
    ##  1st Qu.:-1.0000          1st Qu.:-1.0000          1st Qu.:-0.6800       
    ##  Median :-0.6182          Median :-0.6706          Median :-0.4000       
    ##  Mean   :-0.2656          Mean   :-0.3654          Mean   :-0.4141       
    ##  3rd Qu.: 0.5085          3rd Qu.: 0.2886          3rd Qu.:-0.1600       
    ##  Max.   : 1.0000          Max.   : 1.0000          Max.   : 1.0000       
    ##  fBodyAccJerk.maxInds.Y fBodyAccJerk.maxInds.Z fBodyAccJerk.meanFreq...X
    ##  Min.   :-1.0000        Min.   :-1.0000        Min.   :-1.00000         
    ##  1st Qu.:-0.5600        1st Qu.:-0.4800        1st Qu.:-0.29770         
    ##  Median :-0.4000        Median :-0.3200        Median :-0.04544         
    ##  Mean   :-0.3969        Mean   :-0.3245        Mean   :-0.04771         
    ##  3rd Qu.:-0.2400        3rd Qu.:-0.1600        3rd Qu.: 0.20447         
    ##  Max.   : 1.0000        Max.   : 1.0000        Max.   : 1.00000         
    ##  fBodyAccJerk.meanFreq...Y fBodyAccJerk.meanFreq...Z fBodyAccJerk.skewness...X
    ##  Min.   :-1.000000         Min.   :-1.00000          Min.   :-1.0000          
    ##  1st Qu.:-0.427951         1st Qu.:-0.33139          1st Qu.:-0.5046          
    ##  Median :-0.236530         Median :-0.10246          Median :-0.3525          
    ##  Mean   :-0.213393         Mean   :-0.12383          Mean   :-0.3155          
    ##  3rd Qu.: 0.008651         3rd Qu.: 0.09124          3rd Qu.:-0.1632          
    ##  Max.   : 1.000000         Max.   : 1.00000          Max.   : 1.0000          
    ##  fBodyAccJerk.kurtosis...X fBodyAccJerk.skewness...Y fBodyAccJerk.kurtosis...Y
    ##  Min.   :-1.0000           Min.   :-1.0000           Min.   :-1.0000          
    ##  1st Qu.:-0.8570           1st Qu.:-0.5357           1st Qu.:-0.9135          
    ##  Median :-0.7733           Median :-0.4210           Median :-0.8580          
    ##  Mean   :-0.7132           Mean   :-0.3998           Mean   :-0.8231          
    ##  3rd Qu.:-0.6363           3rd Qu.:-0.2928           3rd Qu.:-0.7766          
    ##  Max.   : 1.0000           Max.   : 1.0000           Max.   : 1.0000          
    ##  fBodyAccJerk.skewness...Z fBodyAccJerk.kurtosis...Z
    ##  Min.   :-1.0000           Min.   :-1.0000          
    ##  1st Qu.:-0.6277           1st Qu.:-0.9118          
    ##  Median :-0.5194           Median :-0.8557          
    ##  Mean   :-0.4877           Mean   :-0.8159          
    ##  3rd Qu.:-0.3836           3rd Qu.:-0.7684          
    ##  Max.   : 1.0000           Max.   : 1.0000          
    ##  fBodyAccJerk.bandsEnergy...1.8 fBodyAccJerk.bandsEnergy...9.16
    ##  Min.   :-1.0000                Min.   :-1.0000                
    ##  1st Qu.:-1.0000                1st Qu.:-0.9999                
    ##  Median :-0.9996                Median :-0.9991                
    ##  Mean   :-0.8678                Mean   :-0.8866                
    ##  3rd Qu.:-0.8053                3rd Qu.:-0.8141                
    ##  Max.   : 1.0000                Max.   : 1.0000                
    ##  fBodyAccJerk.bandsEnergy...17.24 fBodyAccJerk.bandsEnergy...25.32
    ##  Min.   :-1.0000                  Min.   :-1.0000                 
    ##  1st Qu.:-0.9999                  1st Qu.:-0.9999                 
    ##  Median :-0.9986                  Median :-0.9985                 
    ##  Mean   :-0.8737                  Mean   :-0.8975                 
    ##  3rd Qu.:-0.8108                  3rd Qu.:-0.8650                 
    ##  Max.   : 1.0000                  Max.   : 1.0000                 
    ##  fBodyAccJerk.bandsEnergy...33.40 fBodyAccJerk.bandsEnergy...41.48
    ##  Min.   :-1.0000                  Min.   :-1.0000                 
    ##  1st Qu.:-0.9999                  1st Qu.:-0.9998                 
    ##  Median :-0.9989                  Median :-0.9987                 
    ##  Mean   :-0.9218                  Mean   :-0.9030                 
    ##  3rd Qu.:-0.8888                  3rd Qu.:-0.8603                 
    ##  Max.   : 1.0000                  Max.   : 1.0000                 
    ##  fBodyAccJerk.bandsEnergy...49.56 fBodyAccJerk.bandsEnergy...57.64
    ##  Min.   :-1.0000                  Min.   :-1.0000                 
    ##  1st Qu.:-0.9999                  1st Qu.:-1.0000                 
    ##  Median :-0.9992                  Median :-0.9999                 
    ##  Mean   :-0.9451                  Mean   :-0.9845                 
    ##  3rd Qu.:-0.9242                  3rd Qu.:-0.9912                 
    ##  Max.   : 1.0000                  Max.   : 1.0000                 
    ##  fBodyAccJerk.bandsEnergy...1.16 fBodyAccJerk.bandsEnergy...17.32
    ##  Min.   :-1.0000                 Min.   :-1.0000                 
    ##  1st Qu.:-1.0000                 1st Qu.:-0.9999                 
    ##  Median :-0.9992                 Median :-0.9981                 
    ##  Mean   :-0.8682                 Mean   :-0.8547                 
    ##  3rd Qu.:-0.7849                 3rd Qu.:-0.7799                 
    ##  Max.   : 1.0000                 Max.   : 1.0000                 
    ##  fBodyAccJerk.bandsEnergy...33.48 fBodyAccJerk.bandsEnergy...49.64
    ##  Min.   :-1.0000                  Min.   :-1.0000                 
    ##  1st Qu.:-0.9999                  1st Qu.:-0.9999                 
    ##  Median :-0.9987                  Median :-0.9992                 
    ##  Mean   :-0.9075                  Mean   :-0.9431                 
    ##  3rd Qu.:-0.8622                  3rd Qu.:-0.9211                 
    ##  Max.   : 1.0000                  Max.   : 1.0000                 
    ##  fBodyAccJerk.bandsEnergy...1.24 fBodyAccJerk.bandsEnergy...25.48
    ##  Min.   :-1.0000                 Min.   :-1.0000                 
    ##  1st Qu.:-0.9999                 1st Qu.:-0.9998                 
    ##  Median :-0.9987                 Median :-0.9980                 
    ##  Mean   :-0.8461                 Mean   :-0.8622                 
    ##  3rd Qu.:-0.7366                 3rd Qu.:-0.7992                 
    ##  Max.   : 1.0000                 Max.   : 1.0000                 
    ##  fBodyAccJerk.bandsEnergy...1.8.1 fBodyAccJerk.bandsEnergy...9.16.1
    ##  Min.   :-1.0000                  Min.   :-1.0000                  
    ##  1st Qu.:-0.9998                  1st Qu.:-0.9999                  
    ##  Median :-0.9953                  Median :-0.9978                  
    ##  Mean   :-0.8346                  Mean   :-0.8688                  
    ##  3rd Qu.:-0.7200                  3rd Qu.:-0.7973                  
    ##  Max.   : 1.0000                  Max.   : 1.0000                  
    ##  fBodyAccJerk.bandsEnergy...17.24.1 fBodyAccJerk.bandsEnergy...25.32.1
    ##  Min.   :-1.0000                    Min.   :-1.0000                   
    ##  1st Qu.:-0.9998                    1st Qu.:-0.9998                   
    ##  Median :-0.9974                    Median :-0.9979                   
    ##  Mean   :-0.8364                    Mean   :-0.9082                   
    ##  3rd Qu.:-0.7400                    3rd Qu.:-0.8663                   
    ##  Max.   : 1.0000                    Max.   : 1.0000                   
    ##  fBodyAccJerk.bandsEnergy...33.40.1 fBodyAccJerk.bandsEnergy...41.48.1
    ##  Min.   :-1.0000                    Min.   :-1.0000                   
    ##  1st Qu.:-0.9997                    1st Qu.:-0.9996                   
    ##  Median :-0.9980                    Median :-0.9968                   
    ##  Mean   :-0.9158                    Mean   :-0.8770                   
    ##  3rd Qu.:-0.8769                    3rd Qu.:-0.8108                   
    ##  Max.   : 1.0000                    Max.   : 1.0000                   
    ##  fBodyAccJerk.bandsEnergy...49.56.1 fBodyAccJerk.bandsEnergy...57.64.1
    ##  Min.   :-1.0000                    Min.   :-1.0000                   
    ##  1st Qu.:-0.9997                    1st Qu.:-1.0000                   
    ##  Median :-0.9979                    Median :-0.9996                   
    ##  Mean   :-0.9233                    Mean   :-0.9713                   
    ##  3rd Qu.:-0.8872                    3rd Qu.:-0.9838                   
    ##  Max.   : 1.0000                    Max.   : 1.0000                   
    ##  fBodyAccJerk.bandsEnergy...1.16.1 fBodyAccJerk.bandsEnergy...17.32.1
    ##  Min.   :-1.0000                   Min.   :-1.0000                   
    ##  1st Qu.:-0.9998                   1st Qu.:-0.9997                   
    ##  Median :-0.9967                   Median :-0.9970                   
    ##  Mean   :-0.8420                   Mean   :-0.8374                   
    ##  3rd Qu.:-0.7430                   3rd Qu.:-0.7346                   
    ##  Max.   : 1.0000                   Max.   : 1.0000                   
    ##  fBodyAccJerk.bandsEnergy...33.48.1 fBodyAccJerk.bandsEnergy...49.64.1
    ##  Min.   :-1.0000                    Min.   :-1.0000                   
    ##  1st Qu.:-0.9996                    1st Qu.:-0.9998                   
    ##  Median :-0.9969                    Median :-0.9981                   
    ##  Mean   :-0.8791                    Mean   :-0.9293                   
    ##  3rd Qu.:-0.8110                    3rd Qu.:-0.8961                   
    ##  Max.   : 1.0000                    Max.   : 1.0000                   
    ##  fBodyAccJerk.bandsEnergy...1.24.1 fBodyAccJerk.bandsEnergy...25.48.1
    ##  Min.   :-1.0000                   Min.   :-1.0000                   
    ##  1st Qu.:-0.9998                   1st Qu.:-0.9997                   
    ##  Median :-0.9964                   Median :-0.9975                   
    ##  Mean   :-0.8131                   Mean   :-0.8957                   
    ##  3rd Qu.:-0.6732                   3rd Qu.:-0.8361                   
    ##  Max.   : 1.0000                   Max.   : 1.0000                   
    ##  fBodyAccJerk.bandsEnergy...1.8.2 fBodyAccJerk.bandsEnergy...9.16.2
    ##  Min.   :-1.0000                  Min.   :-1.0000                  
    ##  1st Qu.:-0.9998                  1st Qu.:-0.9997                  
    ##  Median :-0.9970                  Median :-0.9977                  
    ##  Mean   :-0.9007                  Mean   :-0.8993                  
    ##  3rd Qu.:-0.8565                  3rd Qu.:-0.8594                  
    ##  Max.   : 1.0000                  Max.   : 1.0000                  
    ##  fBodyAccJerk.bandsEnergy...17.24.2 fBodyAccJerk.bandsEnergy...25.32.2
    ##  Min.   :-1.0000                    Min.   :-1.0000                   
    ##  1st Qu.:-0.9998                    1st Qu.:-0.9998                   
    ##  Median :-0.9987                    Median :-0.9991                   
    ##  Mean   :-0.9314                    Mean   :-0.9634                   
    ##  3rd Qu.:-0.9118                    3rd Qu.:-0.9562                   
    ##  Max.   : 1.0000                    Max.   : 1.0000                   
    ##  fBodyAccJerk.bandsEnergy...33.40.2 fBodyAccJerk.bandsEnergy...41.48.2
    ##  Min.   :-1.0000                    Min.   :-1.0000                   
    ##  1st Qu.:-0.9998                    1st Qu.:-0.9996                   
    ##  Median :-0.9989                    Median :-0.9980                   
    ##  Mean   :-0.9674                    Mean   :-0.9424                   
    ##  3rd Qu.:-0.9607                    3rd Qu.:-0.9253                   
    ##  Max.   : 1.0000                    Max.   : 1.0000                   
    ##  fBodyAccJerk.bandsEnergy...49.56.2 fBodyAccJerk.bandsEnergy...57.64.2
    ##  Min.   :-1.0000                    Min.   :-1.0000                   
    ##  1st Qu.:-0.9995                    1st Qu.:-0.9999                   
    ##  Median :-0.9975                    Median :-0.9994                   
    ##  Mean   :-0.9323                    Mean   :-0.9714                   
    ##  3rd Qu.:-0.9151                    3rd Qu.:-0.9871                   
    ##  Max.   : 1.0000                    Max.   : 1.0000                   
    ##  fBodyAccJerk.bandsEnergy...1.16.2 fBodyAccJerk.bandsEnergy...17.32.2
    ##  Min.   :-1.0000                   Min.   :-1.0000                   
    ##  1st Qu.:-0.9997                   1st Qu.:-0.9998                   
    ##  Median :-0.9968                   Median :-0.9989                   
    ##  Mean   :-0.8786                   Mean   :-0.9471                   
    ##  3rd Qu.:-0.8185                   3rd Qu.:-0.9288                   
    ##  Max.   : 1.0000                   Max.   : 1.0000                   
    ##  fBodyAccJerk.bandsEnergy...33.48.2 fBodyAccJerk.bandsEnergy...49.64.2
    ##  Min.   :-1.0000                    Min.   :-1.0000                   
    ##  1st Qu.:-0.9997                    1st Qu.:-0.9995                   
    ##  Median :-0.9986                    Median :-0.9975                   
    ##  Mean   :-0.9576                    Mean   :-0.9325                   
    ##  3rd Qu.:-0.9451                    3rd Qu.:-0.9157                   
    ##  Max.   : 1.0000                    Max.   : 1.0000                   
    ##  fBodyAccJerk.bandsEnergy...1.24.2 fBodyAccJerk.bandsEnergy...25.48.2
    ##  Min.   :-1.0000                   Min.   :-1.0000                   
    ##  1st Qu.:-0.9997                   1st Qu.:-0.9998                   
    ##  Median :-0.9976                   Median :-0.9989                   
    ##  Mean   :-0.8992                   Mean   :-0.9611                   
    ##  3rd Qu.:-0.8468                   3rd Qu.:-0.9502                   
    ##  Max.   : 1.0000                   Max.   : 1.0000                   
    ##  fBodyGyro.mean...X fBodyGyro.mean...Y fBodyGyro.mean...Z fBodyGyro.std...X
    ##  Min.   :-1.0000    Min.   :-1.0000    Min.   :-1.0000    Min.   :-1.0000  
    ##  1st Qu.:-0.9853    1st Qu.:-0.9847    1st Qu.:-0.9851    1st Qu.:-0.9881  
    ##  Median :-0.8917    Median :-0.9197    Median :-0.8877    Median :-0.9053  
    ##  Mean   :-0.6721    Mean   :-0.7062    Mean   :-0.6442    Mean   :-0.7386  
    ##  3rd Qu.:-0.3837    3rd Qu.:-0.4735    3rd Qu.:-0.3225    3rd Qu.:-0.5225  
    ##  Max.   : 1.0000    Max.   : 1.0000    Max.   : 1.0000    Max.   : 1.0000  
    ##  fBodyGyro.std...Y fBodyGyro.std...Z fBodyGyro.mad...X fBodyGyro.mad...Y
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.9808   1st Qu.:-0.9862   1st Qu.:-0.9861   1st Qu.:-0.9845  
    ##  Median :-0.9061   Median :-0.8915   Median :-0.8897   Median :-0.9204  
    ##  Mean   :-0.6742   Mean   :-0.6904   Mean   :-0.6880   Mean   :-0.7121  
    ##  3rd Qu.:-0.4385   3rd Qu.:-0.4168   3rd Qu.:-0.4142   3rd Qu.:-0.4955  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  fBodyGyro.mad...Z fBodyGyro.max...X fBodyGyro.max...Y fBodyGyro.max...Z
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.9850   1st Qu.:-0.9883   1st Qu.:-0.9835   1st Qu.:-0.9886  
    ##  Median :-0.8887   Median :-0.9072   Median :-0.9211   Median :-0.9102  
    ##  Mean   :-0.6409   Mean   :-0.7355   Mean   :-0.7337   Mean   :-0.7642  
    ##  3rd Qu.:-0.3103   3rd Qu.:-0.5374   3rd Qu.:-0.5703   3rd Qu.:-0.5813  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  fBodyGyro.min...X fBodyGyro.min...Y fBodyGyro.min...Z fBodyGyro.sma..  
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.9970   1st Qu.:-0.9946   1st Qu.:-0.9946   1st Qu.:-0.9842  
    ##  Median :-0.9822   Median :-0.9747   Median :-0.9717   Median :-0.9006  
    ##  Mean   :-0.9315   Mean   :-0.9007   Mean   :-0.9088   Mean   :-0.6603  
    ##  3rd Qu.:-0.9063   3rd Qu.:-0.8631   3rd Qu.:-0.8703   3rd Qu.:-0.3531  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  fBodyGyro.energy...X fBodyGyro.energy...Y fBodyGyro.energy...Z
    ##  Min.   :-1.0000      Min.   :-1.0000      Min.   :-1.0000     
    ##  1st Qu.:-0.9999      1st Qu.:-0.9998      1st Qu.:-0.9998     
    ##  Median :-0.9950      Median :-0.9958      Median :-0.9925     
    ##  Mean   :-0.9154      Mean   :-0.8854      Mean   :-0.8696     
    ##  3rd Qu.:-0.8652      3rd Qu.:-0.8455      3rd Qu.:-0.7788     
    ##  Max.   : 1.0000      Max.   : 1.0000      Max.   : 1.0000     
    ##  fBodyGyro.iqr...X fBodyGyro.iqr...Y fBodyGyro.iqr...Z fBodyGyro.entropy...X
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.00000     
    ##  1st Qu.:-0.9893   1st Qu.:-0.9900   1st Qu.:-0.9894   1st Qu.:-0.69912     
    ##  Median :-0.9300   Median :-0.9419   Median :-0.9333   Median :-0.16602     
    ##  Mean   :-0.7073   Mean   :-0.7538   Mean   :-0.6969   Mean   :-0.09318     
    ##  3rd Qu.:-0.4511   3rd Qu.:-0.5720   3rd Qu.:-0.4380   3rd Qu.: 0.51721     
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.00000     
    ##  fBodyGyro.entropy...Y fBodyGyro.entropy...Z fBodyGyro.maxInds.X
    ##  Min.   :-1.00000      Min.   :-1.0000       Min.   :-1.0000    
    ##  1st Qu.:-0.63653      1st Qu.:-0.7588       1st Qu.:-1.0000    
    ##  Median :-0.13205      Median :-0.2490       Median :-0.9333    
    ##  Mean   :-0.04674      Mean   :-0.1491       Mean   :-0.8789    
    ##  3rd Qu.: 0.56294      3rd Qu.: 0.4495       3rd Qu.:-0.8667    
    ##  Max.   : 1.00000      Max.   : 1.0000       Max.   : 1.0000    
    ##  fBodyGyro.maxInds.Y fBodyGyro.maxInds.Z fBodyGyro.meanFreq...X
    ##  Min.   :-1.0000     Min.   :-1.0000     Min.   :-1.00000      
    ##  1st Qu.:-1.0000     1st Qu.:-1.0000     1st Qu.:-0.27189      
    ##  Median :-0.9355     Median :-0.9310     Median :-0.09868      
    ##  Mean   :-0.8012     Mean   :-0.8079     Mean   :-0.10104      
    ##  3rd Qu.:-0.6774     3rd Qu.:-0.6552     3rd Qu.: 0.06810      
    ##  Max.   : 1.0000     Max.   : 1.0000     Max.   : 1.00000      
    ##  fBodyGyro.meanFreq...Y fBodyGyro.meanFreq...Z fBodyGyro.skewness...X
    ##  Min.   :-1.00000       Min.   :-1.00000       Min.   :-1.00000      
    ##  1st Qu.:-0.36257       1st Qu.:-0.23240       1st Qu.:-0.41722      
    ##  Median :-0.17298       Median :-0.05369       Median :-0.21947      
    ##  Mean   :-0.17428       Mean   :-0.05139       Mean   :-0.17675      
    ##  3rd Qu.: 0.01366       3rd Qu.: 0.12251       3rd Qu.: 0.03088      
    ##  Max.   : 1.00000       Max.   : 1.00000       Max.   : 1.00000      
    ##  fBodyGyro.kurtosis...X fBodyGyro.skewness...Y fBodyGyro.kurtosis...Y
    ##  Min.   :-1.0000        Min.   :-1.00000       Min.   :-1.0000       
    ##  1st Qu.:-0.7536        1st Qu.:-0.44949       1st Qu.:-0.8173       
    ##  Median :-0.5839        Median :-0.24900       Median :-0.6584       
    ##  Mean   :-0.4940        Mean   :-0.17847       Mean   :-0.5331       
    ##  3rd Qu.:-0.3072        3rd Qu.: 0.03578       3rd Qu.:-0.3575       
    ##  Max.   : 1.0000        Max.   : 1.00000       Max.   : 1.0000       
    ##  fBodyGyro.skewness...Z fBodyGyro.kurtosis...Z fBodyGyro.bandsEnergy...1.8
    ##  Min.   :-1.000000      Min.   :-1.0000        Min.   :-1.0000            
    ##  1st Qu.:-0.465284      1st Qu.:-0.7969        1st Qu.:-0.9999            
    ##  Median :-0.270366      Median :-0.6366        Median :-0.9953            
    ##  Mean   :-0.213648      Mean   :-0.5331        Mean   :-0.9292            
    ##  3rd Qu.:-0.005613      3rd Qu.:-0.3574        3rd Qu.:-0.8970            
    ##  Max.   : 1.000000      Max.   : 1.0000        Max.   : 1.0000            
    ##  fBodyGyro.bandsEnergy...9.16 fBodyGyro.bandsEnergy...17.24
    ##  Min.   :-1.0000              Min.   :-1.0000              
    ##  1st Qu.:-0.9999              1st Qu.:-0.9999              
    ##  Median :-0.9975              Median :-0.9983              
    ##  Mean   :-0.9076              Mean   :-0.9201              
    ##  3rd Qu.:-0.8673              3rd Qu.:-0.8874              
    ##  Max.   : 1.0000              Max.   : 1.0000              
    ##  fBodyGyro.bandsEnergy...25.32 fBodyGyro.bandsEnergy...33.40
    ##  Min.   :-1.0000               Min.   :-1.0000              
    ##  1st Qu.:-1.0000               1st Qu.:-0.9999              
    ##  Median :-0.9987               Median :-0.9980              
    ##  Mean   :-0.9589               Mean   :-0.9500              
    ##  3rd Qu.:-0.9480               3rd Qu.:-0.9367              
    ##  Max.   : 1.0000               Max.   : 1.0000              
    ##  fBodyGyro.bandsEnergy...41.48 fBodyGyro.bandsEnergy...49.56
    ##  Min.   :-1.0000               Min.   :-1.0000              
    ##  1st Qu.:-0.9999               1st Qu.:-0.9999              
    ##  Median :-0.9978               Median :-0.9980              
    ##  Mean   :-0.9522               Mean   :-0.9641              
    ##  3rd Qu.:-0.9350               3rd Qu.:-0.9584              
    ##  Max.   : 1.0000               Max.   : 1.0000              
    ##  fBodyGyro.bandsEnergy...57.64 fBodyGyro.bandsEnergy...1.16
    ##  Min.   :-1.0000               Min.   :-1.0000             
    ##  1st Qu.:-1.0000               1st Qu.:-0.9999             
    ##  Median :-0.9995               Median :-0.9951             
    ##  Mean   :-0.9754               Mean   :-0.9199             
    ##  3rd Qu.:-0.9836               3rd Qu.:-0.8747             
    ##  Max.   : 1.0000               Max.   : 1.0000             
    ##  fBodyGyro.bandsEnergy...17.32 fBodyGyro.bandsEnergy...33.48
    ##  Min.   :-1.0000               Min.   :-1.0000              
    ##  1st Qu.:-0.9999               1st Qu.:-0.9999              
    ##  Median :-0.9980               Median :-0.9977              
    ##  Mean   :-0.9190               Mean   :-0.9459              
    ##  3rd Qu.:-0.8830               3rd Qu.:-0.9286              
    ##  Max.   : 1.0000               Max.   : 1.0000              
    ##  fBodyGyro.bandsEnergy...49.64 fBodyGyro.bandsEnergy...1.24
    ##  Min.   :-1.0000               Min.   :-1.0000             
    ##  1st Qu.:-1.0000               1st Qu.:-0.9999             
    ##  Median :-0.9982               Median :-0.9950             
    ##  Mean   :-0.9691               Mean   :-0.9169             
    ##  3rd Qu.:-0.9686               3rd Qu.:-0.8680             
    ##  Max.   : 1.0000               Max.   : 1.0000             
    ##  fBodyGyro.bandsEnergy...25.48 fBodyGyro.bandsEnergy...1.8.1
    ##  Min.   :-1.0000               Min.   :-1.0000              
    ##  1st Qu.:-0.9999               1st Qu.:-0.9997              
    ##  Median :-0.9984               Median :-0.9945              
    ##  Mean   :-0.9547               Mean   :-0.8785              
    ##  3rd Qu.:-0.9382               3rd Qu.:-0.8466              
    ##  Max.   : 1.0000               Max.   : 1.0000              
    ##  fBodyGyro.bandsEnergy...9.16.1 fBodyGyro.bandsEnergy...17.24.1
    ##  Min.   :-1.0000                Min.   :-1.0000                
    ##  1st Qu.:-1.0000                1st Qu.:-1.0000                
    ##  Median :-0.9990                Median :-0.9994                
    ##  Mean   :-0.9574                Mean   :-0.9598                
    ##  3rd Qu.:-0.9534                3rd Qu.:-0.9647                
    ##  Max.   : 1.0000                Max.   : 1.0000                
    ##  fBodyGyro.bandsEnergy...25.32.1 fBodyGyro.bandsEnergy...33.40.1
    ##  Min.   :-1.0000                 Min.   :-1.0000                
    ##  1st Qu.:-1.0000                 1st Qu.:-1.0000                
    ##  Median :-0.9994                 Median :-0.9994                
    ##  Mean   :-0.9675                 Mean   :-0.9776                
    ##  3rd Qu.:-0.9692                 3rd Qu.:-0.9759                
    ##  Max.   : 1.0000                 Max.   : 1.0000                
    ##  fBodyGyro.bandsEnergy...41.48.1 fBodyGyro.bandsEnergy...49.56.1
    ##  Min.   :-1.0000                 Min.   :-1.0000                
    ##  1st Qu.:-0.9999                 1st Qu.:-0.9999                
    ##  Median :-0.9988                 Median :-0.9983                
    ##  Mean   :-0.9581                 Mean   :-0.9524                
    ##  3rd Qu.:-0.9530                 3rd Qu.:-0.9488                
    ##  Max.   : 1.0000                 Max.   : 1.0000                
    ##  fBodyGyro.bandsEnergy...57.64.1 fBodyGyro.bandsEnergy...1.16.1
    ##  Min.   :-1.0000                 Min.   :-1.0000               
    ##  1st Qu.:-1.0000                 1st Qu.:-0.9998               
    ##  Median :-0.9995                 Median :-0.9955               
    ##  Mean   :-0.9761                 Mean   :-0.8905               
    ##  3rd Qu.:-0.9866                 3rd Qu.:-0.8538               
    ##  Max.   : 1.0000                 Max.   : 1.0000               
    ##  fBodyGyro.bandsEnergy...17.32.1 fBodyGyro.bandsEnergy...33.48.1
    ##  Min.   :-1.0000                 Min.   :-1.0000                
    ##  1st Qu.:-1.0000                 1st Qu.:-1.0000                
    ##  Median :-0.9992                 Median :-0.9993                
    ##  Mean   :-0.9526                 Mean   :-0.9734                
    ##  3rd Qu.:-0.9547                 3rd Qu.:-0.9703                
    ##  Max.   : 1.0000                 Max.   : 1.0000                
    ##  fBodyGyro.bandsEnergy...49.64.1 fBodyGyro.bandsEnergy...1.24.1
    ##  Min.   :-1.0000                 Min.   :-1.0000               
    ##  1st Qu.:-0.9999                 1st Qu.:-0.9998               
    ##  Median :-0.9982                 Median :-0.9954               
    ##  Mean   :-0.9562                 Mean   :-0.8790               
    ##  3rd Qu.:-0.9554                 3rd Qu.:-0.8369               
    ##  Max.   : 1.0000                 Max.   : 1.0000               
    ##  fBodyGyro.bandsEnergy...25.48.1 fBodyGyro.bandsEnergy...1.8.2
    ##  Min.   :-1.0000                 Min.   :-1.0000              
    ##  1st Qu.:-1.0000                 1st Qu.:-0.9998              
    ##  Median :-0.9993                 Median :-0.9932              
    ##  Mean   :-0.9668                 Mean   :-0.8992              
    ##  3rd Qu.:-0.9657                 3rd Qu.:-0.8407              
    ##  Max.   : 1.0000                 Max.   : 1.0000              
    ##  fBodyGyro.bandsEnergy...9.16.2 fBodyGyro.bandsEnergy...17.24.2
    ##  Min.   :-1.0000                Min.   :-1.0000                
    ##  1st Qu.:-0.9999                1st Qu.:-0.9999                
    ##  Median :-0.9983                Median :-0.9987                
    ##  Mean   :-0.9329                Mean   :-0.9319                
    ##  3rd Qu.:-0.9141                3rd Qu.:-0.9123                
    ##  Max.   : 1.0000                Max.   : 1.0000                
    ##  fBodyGyro.bandsEnergy...25.32.2 fBodyGyro.bandsEnergy...33.40.2
    ##  Min.   :-1.0000                 Min.   :-1.0000                
    ##  1st Qu.:-1.0000                 1st Qu.:-0.9999                
    ##  Median :-0.9991                 Median :-0.9987                
    ##  Mean   :-0.9656                 Mean   :-0.9711                
    ##  3rd Qu.:-0.9602                 3rd Qu.:-0.9661                
    ##  Max.   : 1.0000                 Max.   : 1.0000                
    ##  fBodyGyro.bandsEnergy...41.48.2 fBodyGyro.bandsEnergy...49.56.2
    ##  Min.   :-1.0000                 Min.   :-1.0000                
    ##  1st Qu.:-0.9999                 1st Qu.:-0.9999                
    ##  Median :-0.9977                 Median :-0.9957                
    ##  Mean   :-0.9605                 Mean   :-0.9513                
    ##  3rd Qu.:-0.9498                 3rd Qu.:-0.9459                
    ##  Max.   : 1.0000                 Max.   : 1.0000                
    ##  fBodyGyro.bandsEnergy...57.64.2 fBodyGyro.bandsEnergy...1.16.2
    ##  Min.   :-1.0000                 Min.   :-1.0000               
    ##  1st Qu.:-1.0000                 1st Qu.:-0.9998               
    ##  Median :-0.9990                 Median :-0.9928               
    ##  Mean   :-0.9694                 Mean   :-0.8816               
    ##  3rd Qu.:-0.9813                 3rd Qu.:-0.8046               
    ##  Max.   : 1.0000                 Max.   : 1.0000               
    ##  fBodyGyro.bandsEnergy...17.32.2 fBodyGyro.bandsEnergy...33.48.2
    ##  Min.   :-1.0000                 Min.   :-1.0000                
    ##  1st Qu.:-0.9999                 1st Qu.:-0.9999                
    ##  Median :-0.9983                 Median :-0.9984                
    ##  Mean   :-0.9184                 Mean   :-0.9682                
    ##  3rd Qu.:-0.8920                 3rd Qu.:-0.9607                
    ##  Max.   : 1.0000                 Max.   : 1.0000                
    ##  fBodyGyro.bandsEnergy...49.64.2 fBodyGyro.bandsEnergy...1.24.2
    ##  Min.   :-1.0000                 Min.   :-1.0000               
    ##  1st Qu.:-0.9999                 1st Qu.:-0.9998               
    ##  Median :-0.9961                 Median :-0.9927               
    ##  Mean   :-0.9592                 Mean   :-0.8730               
    ##  3rd Qu.:-0.9585                 3rd Qu.:-0.7849               
    ##  Max.   : 1.0000                 Max.   : 1.0000               
    ##  fBodyGyro.bandsEnergy...25.48.2 fBodyAccMag.mean.. fBodyAccMag.std..
    ##  Min.   :-1.0000                 Min.   :-1.0000    Min.   :-1.0000  
    ##  1st Qu.:-0.9999                 1st Qu.:-0.9847    1st Qu.:-0.9829  
    ##  Median :-0.9988                 Median :-0.8755    Median :-0.8547  
    ##  Mean   :-0.9664                 Mean   :-0.5860    Mean   :-0.6595  
    ##  3rd Qu.:-0.9592                 3rd Qu.:-0.2173    3rd Qu.:-0.3823  
    ##  Max.   : 1.0000                 Max.   : 1.0000    Max.   : 1.0000  
    ##  fBodyAccMag.mad.. fBodyAccMag.max.. fBodyAccMag.min.. fBodyAccMag.sma..
    ##  Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000   Min.   :-1.0000  
    ##  1st Qu.:-0.9815   1st Qu.:-0.9863   1st Qu.:-0.9916   1st Qu.:-0.9847  
    ##  Median :-0.8549   Median :-0.8772   Median :-0.9692   Median :-0.8755  
    ##  Mean   :-0.5961   Mean   :-0.7565   Mean   :-0.8906   Mean   :-0.5860  
    ##  3rd Qu.:-0.2530   3rd Qu.:-0.5714   3rd Qu.:-0.8388   3rd Qu.:-0.2173  
    ##  Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000   Max.   : 1.0000  
    ##  fBodyAccMag.energy.. fBodyAccMag.iqr.. fBodyAccMag.entropy..
    ##  Min.   :-1.0000      Min.   :-1.0000   Min.   :-1.0000      
    ##  1st Qu.:-0.9997      1st Qu.:-0.9878   1st Qu.:-0.8735      
    ##  Median :-0.9863      Median :-0.9307   Median :-0.3548      
    ##  Mean   :-0.8219      Mean   :-0.6949   Mean   :-0.1901      
    ##  3rd Qu.:-0.7082      3rd Qu.:-0.4323   3rd Qu.: 0.4906      
    ##  Max.   : 1.0000      Max.   : 1.0000   Max.   : 1.0000      
    ##  fBodyAccMag.maxInds fBodyAccMag.meanFreq.. fBodyAccMag.skewness..
    ##  Min.   :-1.0000     Min.   :-1.00000       Min.   :-1.0000       
    ##  1st Qu.:-1.0000     1st Qu.:-0.09663       1st Qu.:-0.5752       
    ##  Median :-0.7931     Median : 0.07026       Median :-0.4228       
    ##  Mean   :-0.7526     Mean   : 0.07688       Mean   :-0.3524       
    ##  3rd Qu.:-0.5172     3rd Qu.: 0.24495       3rd Qu.:-0.2041       
    ##  Max.   : 1.0000     Max.   : 1.00000       Max.   : 1.0000       
    ##  fBodyAccMag.kurtosis.. fBodyBodyAccJerkMag.mean.. fBodyBodyAccJerkMag.std..
    ##  Min.   :-1.0000        Min.   :-1.0000            Min.   :-1.0000          
    ##  1st Qu.:-0.8440        1st Qu.:-0.9898            1st Qu.:-0.9907          
    ##  Median :-0.7345        Median :-0.9290            Median :-0.9255          
    ##  Mean   :-0.6321        Mean   :-0.6208            Mean   :-0.6401          
    ##  3rd Qu.:-0.5359        3rd Qu.:-0.2600            3rd Qu.:-0.3082          
    ##  Max.   : 1.0000        Max.   : 1.0000            Max.   : 1.0000          
    ##  fBodyBodyAccJerkMag.mad.. fBodyBodyAccJerkMag.max.. fBodyBodyAccJerkMag.min..
    ##  Min.   :-1.0000           Min.   :-1.0000           Min.   :-1.0000          
    ##  1st Qu.:-0.9882           1st Qu.:-0.9925           1st Qu.:-0.9868          
    ##  Median :-0.9233           Median :-0.9354           Median :-0.9597          
    ##  Mean   :-0.6143           Mean   :-0.6827           Mean   :-0.8033          
    ##  3rd Qu.:-0.2515           3rd Qu.:-0.4043           3rd Qu.:-0.6786          
    ##  Max.   : 1.0000           Max.   : 1.0000           Max.   : 1.0000          
    ##  fBodyBodyAccJerkMag.sma.. fBodyBodyAccJerkMag.energy..
    ##  Min.   :-1.0000           Min.   :-1.0000             
    ##  1st Qu.:-0.9898           1st Qu.:-0.9999             
    ##  Median :-0.9290           Median :-0.9967             
    ##  Mean   :-0.6208           Mean   :-0.8410             
    ##  3rd Qu.:-0.2600           3rd Qu.:-0.7305             
    ##  Max.   : 1.0000           Max.   : 1.0000             
    ##  fBodyBodyAccJerkMag.iqr.. fBodyBodyAccJerkMag.entropy..
    ##  Min.   :-1.0000           Min.   :-1.0000              
    ##  1st Qu.:-0.9885           1st Qu.:-1.0000              
    ##  Median :-0.9422           Median :-0.6821              
    ##  Mean   :-0.6766           Mean   :-0.3385              
    ##  3rd Qu.:-0.3717           3rd Qu.: 0.3462              
    ##  Max.   : 1.0000           Max.   : 1.0000              
    ##  fBodyBodyAccJerkMag.maxInds fBodyBodyAccJerkMag.meanFreq..
    ##  Min.   :-1.0000             Min.   :-1.000000             
    ##  1st Qu.:-0.9683             1st Qu.:-0.002959             
    ##  Median :-0.9048             Median : 0.164180             
    ##  Mean   :-0.8778             Mean   : 0.173220             
    ##  3rd Qu.:-0.8730             3rd Qu.: 0.357307             
    ##  Max.   : 1.0000             Max.   : 1.000000             
    ##  fBodyBodyAccJerkMag.skewness.. fBodyBodyAccJerkMag.kurtosis..
    ##  Min.   :-1.00000               Min.   :-1.0000               
    ##  1st Qu.:-0.60144               1st Qu.:-0.8788               
    ##  Median :-0.34752               Median :-0.7137               
    ##  Mean   :-0.29860               Mean   :-0.6017               
    ##  3rd Qu.:-0.05769               3rd Qu.:-0.4258               
    ##  Max.   : 1.00000               Max.   : 1.0000               
    ##  fBodyBodyGyroMag.mean.. fBodyBodyGyroMag.std.. fBodyBodyGyroMag.mad..
    ##  Min.   :-1.0000         Min.   :-1.0000        Min.   :-1.0000       
    ##  1st Qu.:-0.9825         1st Qu.:-0.9781        1st Qu.:-0.9787       
    ##  Median :-0.8756         Median :-0.8275        Median :-0.8456       
    ##  Mean   :-0.6974         Mean   :-0.7000        Mean   :-0.6810       
    ##  3rd Qu.:-0.4514         3rd Qu.:-0.4713        3rd Qu.:-0.4185       
    ##  Max.   : 1.0000         Max.   : 1.0000        Max.   : 1.0000       
    ##  fBodyBodyGyroMag.max.. fBodyBodyGyroMag.min.. fBodyBodyGyroMag.sma..
    ##  Min.   :-1.0000        Min.   :-1.0000        Min.   :-1.0000       
    ##  1st Qu.:-0.9796        1st Qu.:-0.9938        1st Qu.:-0.9825       
    ##  Median :-0.8272        Median :-0.9592        Median :-0.8756       
    ##  Mean   :-0.7346        Mean   :-0.8887        Mean   :-0.6974       
    ##  3rd Qu.:-0.5560        3rd Qu.:-0.8399        3rd Qu.:-0.4514       
    ##  Max.   : 1.0000        Max.   : 1.0000        Max.   : 1.0000       
    ##  fBodyBodyGyroMag.energy.. fBodyBodyGyroMag.iqr.. fBodyBodyGyroMag.entropy..
    ##  Min.   :-1.0000           Min.   :-1.0000        Min.   :-1.00000          
    ##  1st Qu.:-0.9997           1st Qu.:-0.9854        1st Qu.:-0.66500          
    ##  Median :-0.9843           Median :-0.9126        Median :-0.15502          
    ##  Mean   :-0.8813           Mean   :-0.7221        Mean   :-0.07628          
    ##  3rd Qu.:-0.8149           3rd Qu.:-0.4953        3rd Qu.: 0.51391          
    ##  Max.   : 1.0000           Max.   : 1.0000        Max.   : 1.00000          
    ##  fBodyBodyGyroMag.maxInds fBodyBodyGyroMag.meanFreq..
    ##  Min.   :-1.0000          Min.   :-1.00000           
    ##  1st Qu.:-1.0000          1st Qu.:-0.23436           
    ##  Median :-0.9487          Median :-0.05210           
    ##  Mean   :-0.8870          Mean   :-0.04156           
    ##  3rd Qu.:-0.8462          3rd Qu.: 0.15158           
    ##  Max.   : 1.0000          Max.   : 1.00000           
    ##  fBodyBodyGyroMag.skewness.. fBodyBodyGyroMag.kurtosis..
    ##  Min.   :-1.00000            Min.   :-1.0000            
    ##  1st Qu.:-0.49959            1st Qu.:-0.8077            
    ##  Median :-0.31771            Median :-0.6649            
    ##  Mean   :-0.26428            Mean   :-0.5759            
    ##  3rd Qu.:-0.08497            3rd Qu.:-0.4393            
    ##  Max.   : 1.00000            Max.   : 1.0000            
    ##  fBodyBodyGyroJerkMag.mean.. fBodyBodyGyroJerkMag.std..
    ##  Min.   :-1.0000             Min.   :-1.0000           
    ##  1st Qu.:-0.9921             1st Qu.:-0.9926           
    ##  Median :-0.9453             Median :-0.9382           
    ##  Mean   :-0.7798             Mean   :-0.7922           
    ##  3rd Qu.:-0.6122             3rd Qu.:-0.6437           
    ##  Max.   : 1.0000             Max.   : 1.0000           
    ##  fBodyBodyGyroJerkMag.mad.. fBodyBodyGyroJerkMag.max..
    ##  Min.   :-1.0000            Min.   :-1.0000           
    ##  1st Qu.:-0.9917            1st Qu.:-0.9935           
    ##  Median :-0.9351            Median :-0.9434           
    ##  Mean   :-0.7734            Mean   :-0.8099           
    ##  3rd Qu.:-0.6098            3rd Qu.:-0.6849           
    ##  Max.   : 1.0000            Max.   : 1.0000           
    ##  fBodyBodyGyroJerkMag.min.. fBodyBodyGyroJerkMag.sma..
    ##  Min.   :-1.0000            Min.   :-1.0000           
    ##  1st Qu.:-0.9937            1st Qu.:-0.9921           
    ##  Median :-0.9727            Median :-0.9453           
    ##  Mean   :-0.8712            Mean   :-0.7798           
    ##  3rd Qu.:-0.8058            3rd Qu.:-0.6122           
    ##  Max.   : 1.0000            Max.   : 1.0000           
    ##  fBodyBodyGyroJerkMag.energy.. fBodyBodyGyroJerkMag.iqr..
    ##  Min.   :-1.0000               Min.   :-1.0000           
    ##  1st Qu.:-0.9999               1st Qu.:-0.9912           
    ##  Median :-0.9980               Median :-0.9419           
    ##  Mean   :-0.9379               Mean   :-0.7727           
    ##  3rd Qu.:-0.9227               3rd Qu.:-0.6047           
    ##  Max.   : 1.0000               Max.   : 1.0000           
    ##  fBodyBodyGyroJerkMag.entropy.. fBodyBodyGyroJerkMag.maxInds
    ##  Min.   :-1.0000                Min.   :-1.0000             
    ##  1st Qu.:-0.9235                1st Qu.:-0.9683             
    ##  Median :-0.4145                Median :-0.9048             
    ##  Mean   :-0.2743                Mean   :-0.9000             
    ##  3rd Qu.: 0.3372                3rd Qu.:-0.8730             
    ##  Max.   : 1.0000                Max.   : 1.0000             
    ##  fBodyBodyGyroJerkMag.meanFreq.. fBodyBodyGyroJerkMag.skewness..
    ##  Min.   :-1.00000                Min.   :-1.0000                
    ##  1st Qu.:-0.01948                1st Qu.:-0.5362                
    ##  Median : 0.13625                Median :-0.3352                
    ##  Mean   : 0.12671                Mean   :-0.2986                
    ##  3rd Qu.: 0.28896                3rd Qu.:-0.1132                
    ##  Max.   : 1.00000                Max.   : 1.0000                
    ##  fBodyBodyGyroJerkMag.kurtosis.. angle.tBodyAccMean.gravity.
    ##  Min.   :-1.0000                 Min.   :-1.000000          
    ##  1st Qu.:-0.8418                 1st Qu.:-0.124694          
    ##  Median :-0.7034                 Median : 0.008146          
    ##  Mean   :-0.6177                 Mean   : 0.007705          
    ##  3rd Qu.:-0.4880                 3rd Qu.: 0.149005          
    ##  Max.   : 1.0000                 Max.   : 1.000000          
    ##  angle.tBodyAccJerkMean..gravityMean. angle.tBodyGyroMean.gravityMean.
    ##  Min.   :-1.000000                    Min.   :-1.00000                
    ##  1st Qu.:-0.287031                    1st Qu.:-0.49311                
    ##  Median : 0.007668                    Median : 0.01719                
    ##  Mean   : 0.002648                    Mean   : 0.01768                
    ##  3rd Qu.: 0.291490                    3rd Qu.: 0.53614                
    ##  Max.   : 1.000000                    Max.   : 1.00000                
    ##  angle.tBodyGyroJerkMean.gravityMean. angle.X.gravityMean. angle.Y.gravityMean.
    ##  Min.   :-1.000000                    Min.   :-1.0000      Min.   :-1.000000   
    ##  1st Qu.:-0.389041                    1st Qu.:-0.8173      1st Qu.: 0.002151   
    ##  Median :-0.007186                    Median :-0.7156      Median : 0.182029   
    ##  Mean   :-0.009219                    Mean   :-0.4965      Mean   : 0.063255   
    ##  3rd Qu.: 0.365996                    3rd Qu.:-0.5215      3rd Qu.: 0.250790   
    ##  Max.   : 1.000000                    Max.   : 1.0000      Max.   : 1.000000   
    ##  angle.Z.gravityMean.      code      
    ##  Min.   :-1.000000    Min.   :1.000  
    ##  1st Qu.:-0.131880    1st Qu.:2.000  
    ##  Median :-0.003882    Median :4.000  
    ##  Mean   :-0.054284    Mean   :3.625  
    ##  3rd Qu.: 0.102970    3rd Qu.:5.000  
    ##  Max.   : 1.000000    Max.   :6.000

#### Step 3:Create one master (merged) dataset

``` r
# 2) extract only mean and std dev for each measurement

  extractDataset <- masterDataset %>% select(subject, code, contains("mean"), contains("std"))

  extractDataset$code <- activityLabels[extractDataset$code, 2]

  summary(extractDataset$code)
```

    ##    Length     Class      Mode 
    ##     10299 character character
