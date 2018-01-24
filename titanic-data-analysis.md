Using the dataset from
[kaggle.com](https://www.kaggle.com/c/titanic/data).

``` r
# Load some helpful libraries
library(tidyverse)
library(tufte)
library(ggplot2)
```

1.  Exploratory analysis I will consider two questions in my
    exploration:

-   Who were the Titanic passengers? What characteristics did they have?
-   What passenger characteristics or other factors are associated with
    survival?

``` r
    titanic <- read.csv("titanic.csv.bz2")
```

``` r
head(titanic)
```

    ##   pclass survived                                            name    sex
    ## 1      1        1                   Allen, Miss. Elisabeth Walton female
    ## 2      1        1                  Allison, Master. Hudson Trevor   male
    ## 3      1        0                    Allison, Miss. Helen Loraine female
    ## 4      1        0            Allison, Mr. Hudson Joshua Creighton   male
    ## 5      1        0 Allison, Mrs. Hudson J C (Bessie Waldo Daniels) female
    ## 6      1        1                             Anderson, Mr. Harry   male
    ##       age sibsp parch ticket     fare   cabin embarked boat body
    ## 1 29.0000     0     0  24160 211.3375      B5        S    2   NA
    ## 2  0.9167     1     2 113781 151.5500 C22 C26        S   11   NA
    ## 3  2.0000     1     2 113781 151.5500 C22 C26        S        NA
    ## 4 30.0000     1     2 113781 151.5500 C22 C26        S       135
    ## 5 25.0000     1     2 113781 151.5500 C22 C26        S        NA
    ## 6 48.0000     0     0  19952  26.5500     E12        S    3   NA
    ##                         home.dest
    ## 1                    St Louis, MO
    ## 2 Montreal, PQ / Chesterville, ON
    ## 3 Montreal, PQ / Chesterville, ON
    ## 4 Montreal, PQ / Chesterville, ON
    ## 5 Montreal, PQ / Chesterville, ON
    ## 6                    New York, NY

``` r
tail(titanic)
```

    ##      pclass survived                      name    sex  age sibsp parch
    ## 1304      3        0     Yousseff, Mr. Gerious   male   NA     0     0
    ## 1305      3        0      Zabour, Miss. Hileni female 14.5     1     0
    ## 1306      3        0     Zabour, Miss. Thamine female   NA     1     0
    ## 1307      3        0 Zakarian, Mr. Mapriededer   male 26.5     0     0
    ## 1308      3        0       Zakarian, Mr. Ortin   male 27.0     0     0
    ## 1309      3        0        Zimmerman, Mr. Leo   male 29.0     0     0
    ##      ticket    fare cabin embarked boat body home.dest
    ## 1304   2627 14.4583              C        NA          
    ## 1305   2665 14.4542              C       328          
    ## 1306   2665 14.4542              C        NA          
    ## 1307   2656  7.2250              C       304          
    ## 1308   2670  7.2250              C        NA          
    ## 1309 315082  7.8750              S        NA

``` r
titanic$age <- as.integer(titanic$age)
```

The dataset has 14 variables and 1309 observations. The variables are:
pclass: The class of the passenger. Varies from 1st to 3rd. survived: A
binary variable where ‘1’ indicates survived and ‘0’ indicates death.
name: The full name of the passenger. sex: The sex of the indivisual,
can take ‘male’ or ‘female’. age: A decimal which indicates the age of
the passenger. sibsp: Number of siblings or spouse on board. parch:
Number of parents / children on board. ticket: Indicates the ticket
number. fare: Passenger fare. cabin: The cabin the passenfgers resided
in. embarked: The port from which the passenger embarked. C
indicates,Cherbourg, Q: Queenstown, and S: Southampton. boat: The number
of boats assigned to each cabin. body: The body tag number.(This is an
educated guess) home.dest: Intended destination.

First, I want to explore who the passengers aboard the Titanic were. I
created a basic visualization to help us understand the distributions of
age for Titanic passengers.

``` r
ggplot(data = titanic, aes(age)) + 
  geom_histogram(fill="blue")
```

![Age of PassenGers Aboard the
Titanic](titanic-data-analysis_files/figure-markdown_github/unnamed-chunk-1-1.png)

I went further to look at how passenger age might be related to
survival.

``` r
ggplot(titanic, aes(x = age, fill = as.factor(survived)))+ geom_histogram(binwidth = 2) + labs(y = "Passenger Count", x = "Age (binwidth = 2)", title = "Titanic Survival Rates by Age")
```

![Survival and Passenger
Age](titanic-data-analysis_files/figure-markdown_github/unnamed-chunk-2-1.png)

I then used passenger class as another variable to consider survival of
passengers.

``` r
survival_factor <- as.factor(titanic$survived)
ggplot(titanic, aes(x = pclass, fill = survival_factor)) + geom_bar() + labs(y = "Passenger Count", title = "Titanic Survival Rates by Pclass")
```

![Titanic Survival Rates by
Pclass](titanic-data-analysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

1.  Logistic Regression

I estimated a logistic regression model to predict the survival of the
passengers on the titanic.

``` r
titanic$pclass <- as.factor(titanic$pclass)
reg.model <- glm(survived ~  pclass, family = binomial(link = 'logit'), data = titanic)
summary(reg.model)
```

    ## 
    ## Call:
    ## glm(formula = survived ~ pclass, family = binomial(link = "logit"), 
    ##     data = titanic)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3896  -0.7678  -0.7678   0.9791   1.6525  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.4861     0.1146   4.242 2.21e-05 ***
    ## pclass2      -0.7696     0.1669  -4.611 4.02e-06 ***
    ## pclass3      -1.5567     0.1433 -10.860  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1741.0  on 1308  degrees of freedom
    ## Residual deviance: 1613.3  on 1306  degrees of freedom
    ## AIC: 1619.3
    ## 
    ## Number of Fisher Scoring iterations: 4

All the coefficients are statistically significant. They can be
interpretated as, for every one unit change in pclass2, the log odds of
survival (versus non-survival) decreases by 0.77. For every one unit
change in pclass3, the log odds of survival (versus non-survival)
decreases by 1.56.

I introduced interactions (cross effects) between gender and passenger
class.

``` r
reg.model2 <- glm(survived ~  sex * pclass, family = binomial(link = 'logit'), data = titanic)
summary(reg.model2)
```

    ## 
    ## Call:
    ## glm(formula = survived ~ sex * pclass, family = binomial(link = "logit"), 
    ##     data = titanic)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5924  -0.5745  -0.5745   0.4902   1.9610  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       3.3250     0.4549   7.309 2.68e-13 ***
    ## sexmale          -3.9848     0.4815  -8.277  < 2e-16 ***
    ## pclass2          -1.2666     0.5485  -2.309   0.0209 *  
    ## pclass3          -3.3621     0.4748  -7.081 1.43e-12 ***
    ## sexmale:pclass2   0.1617     0.6104   0.265   0.7911    
    ## sexmale:pclass3   2.3039     0.5158   4.467 7.95e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1741  on 1308  degrees of freedom
    ## Residual deviance: 1210  on 1303  degrees of freedom
    ## AIC: 1222
    ## 
    ## Number of Fisher Scoring iterations: 5

Interpreting the coefficients:

Intercept- Women are predicted to have a survival rate increase of 3.33
units when in first class, this is our baseline.

sexmale- Men are predicted to have a survival rate decrease of 3.98
units when in first class, compared to the baseline.

pclass2- Women are predicted to have a survival rate decrease of 1.27
units when in second class, compared to the baseline.

pclass3- Women are predicted to have a survival rate decrease of 3.36
units when in third class, compared to the baseline.

sexmale:pclass2- Men are predicted to have a survival rate increase of
0.16 units when in second class, compared to the baseline.

sexmale:pclass3- Men are predicted to have a survival rate increase of
2.3 units when in third class, compared to the baseline.

By observing the coefficients and the p values of each interaction, the
interactions that are not statistically significant are, males and
females in second class. As for the rest of the interactions, the model
shows that male passengers in the 3rd class had a higher predicted
survival rate compared to female passengers on the titanic.

Using fare and embarked variables, to check for prediction of survival

``` r
reg.model3 <- glm(survived ~ fare + embarked,family=binomial(link="logit"), data= titanic)
summary(reg.model3)
```

    ## 
    ## Call:
    ## glm(formula = survived ~ fare + embarked, family = binomial(link = "logit"), 
    ##     data = titanic)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.2853  -0.8931  -0.8194   1.2660   1.6235  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  12.68370  378.59289   0.034    0.973    
    ## fare          0.01103    0.00163   6.768  1.3e-11 ***
    ## embarkedC   -13.04249  378.59289  -0.034    0.973    
    ## embarkedQ   -13.40782  378.59293  -0.035    0.972    
    ## embarkedS   -13.68990  378.59289  -0.036    0.971    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1740.1  on 1307  degrees of freedom
    ## Residual deviance: 1632.2  on 1303  degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## AIC: 1642.2
    ## 
    ## Number of Fisher Scoring iterations: 12

The variable embarked does not seem to be statistically significant, and
thus cannot be used to explain the survival of a passenger. The variable
fare on the otherhand is statistically significant, and can be
interpreted as for every one unit change in fare, the log odds of
survival (versus non-survival) increases by 0.011.
