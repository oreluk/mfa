# Multiple Factor Analysis (MFA)

What is MFA? 
============

* Popular factorial method to study sets of variables that are collected on the *same* set of observations 
* Generalization of principal component analysis
* Provides factor loadings to indicate the impact of variables on components
* Factor scores (component scores in PCA) are returned to explain the effect of each observation on the factors
* Analysis provides a means of quantifying a level of agreement or disagreement between sets of variables

Installation
============
1) Install [devtools](https://github.com/hadley/devtools) if its not already installed. This can be done through [CRAN](https://cran.r-project.org/): 
```
install.packages("devtools")
```
2) Load the devtools package: 
```
library(devtools)
```
3) Install from the github repository: 
```
install_github("oreluk/MFA")
```
Shiny App Demo
============
You can view a demo of the Shiny App on YouTube: 

https://www.youtube.com/watch?v=tZbjV75L6F8

References
============

[Abdi, H., Williams, L. J., & Valentin, D. (2013). Multiple factor analysis: principal component analysis for multitable and multiblock data sets. Wiley Interdisciplinary reviews: computational statistics, 5(2), 149-179.](https://www.utdallas.edu/~herve/abdi-WiresCS-mfa-2013.pdf)

[Pages, Jérôme. "Multiple factor analysis: Main features and application to sensory data." Revista Colombiana de Estadística 27.1 (2004): 1-26.](http://factominer.free.fr/docs/PagesAFM.pdf)

[Abdi, Hervé. "Singular value decomposition (SVD) and generalized singular value decomposition." Encyclopedia of measurement and statistics (2007): 907-912.](https://www.utdallas.edu/~herve/Abdi-SVD2007-pretty.pdf)

============

This R package was created for a STAT243 Project. A description of the assignment can be found [here](https://github.com/ucb-stat243/stat243-fall-2016/blob/master/problem-sets/final-project/final-project.pdf). Package was written by [Yulin Chen](https://github.com/cl12102783), [Stephanie Wuerth](https://github.com/swuerth), [Eren Bilir](https://github.com/tebilir), and [Jim Oreluk](https://github.com/oreluk).   
