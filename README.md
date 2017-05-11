# d3mLm


## Installation

You can install d3mLm from github with:

```R
# install.packages("devtools")
devtools::install_github("d3m-purdue/d3mLm")
```

## Example

This is a basic example which shows you how to solve a common problem:

```r
data(radon_mn, package = "d3mLm")
str(radon_mn)
# 'data.frame':	919 obs. of  28 variables:
#  $ radonFile_index: chr  "5081" "5082" "5083" "5084" ...
#  $ state          : chr  "MN" "MN" "MN" "MN" ...
#  $ state2         : chr  "MN" "MN" "MN" "MN" ...
#  $ stfips         : num  27 27 27 27 27 27 27 27 27 27 ...
#  $ zip            : chr  "55735" "55748" "55748" "56469" ...
#  $ region         : num  5 5 5 5 3 3 3 3 3 3 ...
#  $ typebldg       : num  1 1 1 1 1 1 1 1 1 1 ...
#  $ floor          : num  1 0 0 0 0 0 0 0 0 0 ...
#  $ room           : num  3 4 4 4 4 4 4 2 4 4 ...
#  $ basement       : chr  "N" "Y" "Y" "Y" ...
#  $ windoor        : chr  NA NA NA NA ...
#  $ rep            : chr  "2" "5" "3" "2" ...
#  $ stratum        : num  4 2 2 2 2 2 2 2 2 2 ...
#  $ wave           : chr  "41" "40" "42" "24" ...
#  $ starttm        : chr  "930.0" "1615.0" "1030.0" "1410.0" ...
#  $ stoptm         : chr  "930.0" "1615.0" "1515.0" "1410.0" ...
#  $ startdt        : chr  "12088.0" "11888.0" "20288.0" "122987.0" ...
#  $ stopdt         : chr  "12288.0" "12088.0" "21188.0" "123187.0" ...
#  $ activity       : num  2.2 2.2 2.9 1 3.1 2.5 1.5 1 0.7 1.2 ...
#  $ pcterr         : num  9.7 14.5 9.6 24.3 13.8 12.8 14.5 18 25.1 17.5 ...
#  $ adjwt          : num  1146 471 433 462 433 ...
#  $ dupflag        : num  1 0 0 0 0 0 0 0 0 0 ...
#  $ zipflag        : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ cntyfips       : num  1 1 1 1 3 3 3 3 3 3 ...
#  $ county         : chr  "AITKIN" "AITKIN" "AITKIN" "AITKIN" ...
#  $ fips           : chr  "27001.0" "27001.0" "27001.0" "27001.0" ...
#  $ Uppm           : num  0.502 0.502 0.502 0.502 0.429 ...
#  $ county_code    : chr  "0" "0" "0" "0" ...


radon_model <- lm(Uppm ~ typebldg + basement + dupflag, data = radon_mn)
radon_model
#
# Call:
# lm(formula = Uppm ~ typebldg + basement + dupflag, data = radon_mn)
#
# Coefficients:
# (Intercept)     typebldg    basementY      dupflag
#    0.820478     0.029328     0.087523     0.008665
#

result <- extract_lm(radon_model)
result$as_json()
```
```json
{
  "model_type": "linear model",
  "model_call": "lm(formula = Uppm ~ typebldg + basement + dupflag, data = radon_mn)",
  "response_variable": "Uppm",
  "beta_variables": ["(Intercept)", "typebldg", "basementY", "dupflag"],
  "diag_model": {
    "r.squared": 0.005,
    "adj.r.squared": 0.0016,
    "sigma": 0.3203,
    "statistic": 1.4628,
    "p.value": 0.2233,
    "df": 4,
    "logLik": -244.147,
    "AIC": 498.2939,
    "BIC": 522.1822,
    "deviance": 89.65,
    "df.residual": 874
  },
  "diag_coefs": {
    "term": ["(Intercept)", "typebldg", "basementY", "dupflag"],
    "estimate": [0.8205, 0.0293, 0.0875, 0.0087],
    "std.error": [0.0565, 0.0261, 0.0445, 0.0319],
    "statistic": [14.516, 1.122, 1.9662, 0.2713],
    "p.value": [6.1985e-43, 0.2622, 0.0496, 0.7862]
  },
  "diag_data": {
    ".rownames": ["1", "2", "3", "4", "5", "6"],
    "Uppm": [0.5021, 0.5021, 0.5021, 0.5021, 0.4286, 0.4286],
    "typebldg": [1, 1, 1, 1, 1, 1],
    "basement": ["N", "Y", "Y", "Y", "Y", "Y"],
    "dupflag": [1, 0, 0, 0, 0, 0],
    ".fitted": [0.8585, 0.9373, 0.9373, 0.9373, 0.9373, 0.9373],
    ".se.fit": [0.0524, 0.0116, 0.0116, 0.0116, 0.0116, 0.0116],
    ".resid": [-0.3564, -0.4353, -0.4353, -0.4353, -0.5088, -0.5088],
    ".hat": [0.0268, 0.0013, 0.0013, 0.0013, 0.0013, 0.0013],
    ".sigma": [0.3202, 0.3201, 0.3201, 0.3201, 0.32, 0.32],
    ".cooksd": [0.0088, 0.0006, 0.0006, 0.0006, 0.0008, 0.0008],
    ".std.resid": [-1.1281, -1.36, -1.36, -1.36, -1.5896, -1.5896]
  }
}
```
