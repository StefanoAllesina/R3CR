How to make sure your code is correct
================
Stefano Allesina
April, 2020

## Programming for Science

When programming for science, **you need to make sure that your programs
do exactly, and exclusively, what they are meant to do**. Bugs (i.e.,
errors) are not simply annoying, unwanted features (as if you were
programming a game app for your phone): **any bug in your code can make
the results and conclusions of your research unwarranted**.

Industry average: about 15 bugs per 1000 lines of code (clearly, depends
on language, how the code is written etc.). We need 0.

### Why?

Small errors in code can have devastating consequences, for example:

> One of the most spectacular flameouts in science happened last year.
> In a short letter (barely over 300 words long) published in Science in
> the very last issue of 2006, Geoffrey Chang, a crystallographer,
> retracted 3 Science articles, a Nature article, a PNAS article and a
> JMB article. The sum of 5 years of work was destroyed, apparently,
> over a single sign error in a data-processing program.
> [read](http://boscoh.com/protein/a-sign-a-flipped-structure-and-a-scientific-flameout-of-epic-proportions.html)

## Tools

We’re going to showcase unit testing, assertions, and benchmarking. If
you haven’t installed these packages, please install them now\!

``` r
install.packages("testthat")
install.packages("assertthat")
install.packages("microbenchmark")
```

## Nepotism in Italian Academia

In Italy, all professorships are tenured, and funded by the government.
To become a professor, the candidate has to be the winner of a
competition (*concorso*). Unfortunately, certain professors (*baroni*)
are able to manipulate the proceedings of the competition, to make
“their” candidate win irrespective of merit. In documented lawsuits,
professors influenced competitions to make their relatives (spouses,
children) win.

How prevalent are nepotistic hires? I have attempted to measure this
phenomenon in [Allesina (PLoS
One 2011)](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0021160)
and [Grilli & Allesina
(PNAS 2017)](http://www.pnas.org/content/114/29/7600).

## Facts about last names

I have been fascinated by the parallel between last names and genetics
for a while now. If you want to hear more, check out this talk of mine:

<div data-align="center">

<iframe width="560" height="315" src="https://www.youtube.com/embed/f62TDUQUrkk" frameborder="0" allowfullscreen>
</iframe>

</div>

  - US Top 10 surnames cover 4.9% of the population
    
      - SMITH 2,442,977
      - JOHNSON 1,932,812
      - WILLIAMS 1,625,252
      - BROWN 1,437,026
      - JONES 1,425,470
      - GARCIA 1,166,120
      - MILLER 1,161,437
      - DAVIS 1,116,357
      - RODRIGUEZ 1,094,924
      - MARTINEZ 1,060,159

  - China: an estimated 87% of the population shares one of 100
    surnames, and more than one in five Chinese citizens is surnamed Li,
    Wang, or Zhang: more than 275 million people in all.

  - Denmark: top 10 names cover 25% of the population

  - Germany: top 10 names cover 4.09% of the population

  - Italy: top 10 names cover 0.67% of the population (top 50 = 1.76%,
    top 100 = 2.55%)

Example: [my own
name](http://www.cognomix.it/mappe-dei-cognomi-italiani/ALLESINA)

### About nepotism

Typically, the **power of the *barone* is limited** to their own
department/discipline/institution.

**Children take the name of their father** (this law was changed
recently).

**Women maintain their maiden name** (i.e., husband and wife have
different names; mothers and children have different names)

## Idea of the analysis

**If nepotism is rampant** in certain disciplines, **we should observe a
scarcity of last names**. Analyzing disciplines is good because
researcher are uniformly spread geographically, and Italian names tend
to be regional.

We can take a discipline, count the number of people, and the number of
last names. Then, we can repeatedly sample the same number of people at
random, and compute an approximate p-value by measuring how many times
do we observe fewer last names in the random sample than in the actual
data.

## Getting the data

``` r
link <- "https://raw.githubusercontent.com/StefanoAllesina/namepairs/master/data/ita_2000.csv"
shortlink <- "https://bit.ly/2HO6nSC"
# also available:
# ita_2005.csv
# ita_2010.csv
# ita_2015.csv
# cnrs_maiden_2016.csv
# cnrs_married_2016.csv
# us_2016.csv
df <- read.csv(shortlink, stringsAsFactors = FALSE)
head(df)
```

    ##   X first_id last_id gender                rank institution_id city_id
    ## 1 1     5001       3      F associate professor              1       1
    ## 2 2     2749      23      M assistant professor              1       1
    ## 3 3     6516      23      M           professor              1       1
    ## 4 4     8635      94      M associate professor              1       1
    ## 5 5     3064     588      M           professor              1       1
    ## 6 6     6703     685      F associate professor              1       1
    ##   region sector year
    ## 1 Puglia   Math 2000
    ## 2 Puglia   Math 2000
    ## 3 Puglia   Math 2000
    ## 4 Puglia   Math 2000
    ## 5 Puglia   Math 2000
    ## 6 Puglia   Math 2000

## Counting names

We’re going to write a function that takes the name of a discipline, and
counts the number of people as well as the number of last names. Let’s
start by looking at the number of disciplines represented in the data

``` r
sort(unique(df$sector))
```

    ##  [1] "Agr"          "Bio"          "Chem"         "Econ"        
    ##  [5] "Eng-Civ"      "Eng-Ind"      "Geo"          "Hist-Ped-Psi"
    ##  [9] "Hum"          "Law"          "Math"         "Med"         
    ## [13] "Phys"         "Soc"

For example, we can analyze medicine (`Med`)

``` r
discipline <- "Med"
```

We want to count how many people are in Medicine. We can extract the
last names and count them:

``` r
last_names <- df$last_id[df$sector == discipline]
num_people <- length(last_names)
```

Now we need to count the number of distinct last names. Because we’re
going to need to do this very many times, we can write two different
functions, and see what’s fastest:

``` r
count_names_table <- function(last_names){
  return(length(table(last_names)))
}
count_names_unique <- function(last_names){
  return(length(unique(last_names)))
}
```

For testing speed, you can use the library `microbenchmark`:

``` r
library(microbenchmark)
microbenchmark(count_names_table(last_names), count_names_unique(last_names))
```

    ## Unit: microseconds
    ##                            expr      min        lq      mean    median
    ##   count_names_table(last_names) 4810.180 4865.8535 5271.3179 5009.3975
    ##  count_names_unique(last_names)  298.296  301.1285  319.2936  304.7645
    ##        uq      max neval
    ##  5350.568 7302.263   100
    ##   329.536  503.015   100

You can see that the difference is huge\! It takes about 18 times longer
to perform the same operation using `count_names_table`.

You can come up with more inventive ways to write the same function.
However, we need to make sure that the code is correct. How can we test
it?

## Unit testing

When you write code, you typically test it thoroughly. The idea of unit
testing is to save the tests you would run, so that they can be re-run
every time you modify the code. If the modified code still passes all
the tests, chances are it’s still correct.

Of course, unit testing is only as good as the tests are. Writing a
large number of tests probing different situations would ensure that no
bugs went unnoticed.

The library `testthat` facilitates the use of unit testing in `R`.
Here’s a brief intro.

### Expectations

When you test a function, you expect a certain result.

``` r
library(testthat)
```

``` r
# this test passes
expect_that(1:3, equals(c(1,2,3)))
# but this doesn't
expect_that(1:4, equals(c(1,2,3)))
```

Besides `equals` you can use `is_identical_to` which does not tolerate
numerical approximations:

``` r
x <- 0.22
sprintf("%.20f", sin(x))
sprintf("%.20f", sqrt(1 - cos(x)^2))
# this passes
expect_that(sin(x), equals(sqrt(1 - cos(x)^2)))
# this fails
expect_that(sin(x), is_identical_to(sqrt(1 - cos(x)^2)))
```

You can also create expectations using regular expressions (for text),
and for many other cases. All come with shortcuts:

``` r
x <- 0.2
expect_equal(sin(x), cos(pi / 2 - x))
expect_identical(sin(x), cos(pi / 2 - x))
```

### Collecting expectations in a test

``` r
test_that("a meaningful message",{
 x <- 0.4
 expect_equal(sin(x), cos(pi / 2 - x))
 expect_equal(sin(x), sqrt(1 - cos(x)^2))
})
```

### Workflow

You can run all of the tests in a file by calling `test_file(MY_PATH)`:

``` r
test_file("unit_testing.R")
```

In the same way, you can collect all the tests for your project in a
directory `tests` and then run all of them every time you modify
something:

``` r
test_dir("tests")
```

## Back to nepotism

Now we can see that the two functions give the same result:

``` r
# passes
expect_equal(count_names_table(last_names), count_names_unique(last_names))
```

Let’s build a function that analyzes a discipline:

``` r
count_names_unique <- function(last_names){
  return(length(unique(last_names)))
}

compute_p_value <- function(filename, discipline = "Med"){
  # read the data
  df <- read.csv(filename, stringsAsFactors = FALSE)
  # extract names and count people and names
  last_names <- df$last_id[df$sector == discipline]
  num_people <- length(last_names)
  num_names <- count_names_unique(last_names)
  return(
    data.frame(
      discipline = discipline,
      num_people = num_people,
      num_names = num_names
    )
  )
}

shortlink <- "https://bit.ly/2HO6nSC"
compute_p_value(shortlink, "Med")
```

    ##   discipline num_people num_names
    ## 1        Med       9559      6718

``` r
compute_p_value(shortlink, "Phys")
```

    ##   discipline num_people num_names
    ## 1       Phys       2408      2125

## Checking the input: assertions

Many bugs sneak in because you are passing the wrong type of argument to
a function. You want the code to fail whenever something is not right.
You can use assertions to control the quality and type of the input.

``` r
library(assertthat)
```

``` r
# this will pass
probability <- 0
# check the type of a variable
assert_that(is.numeric(probability))
# probabilities cannot be negative!
assert_that(probability >= 0.0)
# probabilities cannot larger than 1!
assert_that(probability <= 1.0)
```

``` r
# this will pass
probability <- -0.12
# check the type of a variable
assert_that(is.numeric(probability))
# probabilities cannot be negative!
assert_that(probability >= 0.0)
# probabilities cannot larger than 1!
assert_that(probability <= 1.0)
```

## Add randomizations to compute p-values

We can add the randomization part to our code, and use assertions to
check the quality of the input:

``` r
count_names_unique <- function(last_names){
  return(length(unique(last_names)))
}

compute_p_value <- function(filename, discipline = "Med", nrand = 1000){
  # read the data
  df <- read.csv(filename, stringsAsFactors = FALSE)
  assert_that(is.data.frame(df))
  assert_that(nrow(df) > 1)
  # extract names and count people and names
  last_names <- df$last_id[df$sector == discipline]
  assert_that(length(last_names) > 0)
  num_people <- length(last_names)
  num_names <- count_names_unique(last_names)
  # compute pvalue
  pval <- 0.0
  expected_num_names <- 0
  for (i in 1:nrand){
    last_names <- sample(df$last_id, num_people)
    tmp <- count_names_unique(last_names)
    expected_num_names <- expected_num_names + tmp
    if (tmp <= num_names){
      pval <- pval + 1 
    }
  }
  pval <- pval / nrand
  expected_num_names <- expected_num_names / nrand
  return(
    data.frame(
      discipline = discipline,
      num_people = num_people,
      num_names = num_names,
      expected_names = expected_num_names,
      pvalue = pval
    )
  )
}

shortlink <- "https://bit.ly/2HO6nSC"
compute_p_value(shortlink, "Med", 1000)
```

    ##   discipline num_people num_names expected_names pvalue
    ## 1        Med       9559      6718       7045.811      0

``` r
compute_p_value(shortlink, "Phys", 1000)
```

    ##   discipline num_people num_names expected_names pvalue
    ## 1       Phys       2408      2125       2146.319  0.087

## Try by yourself

  - Try running the code with 100,000 randomizations: do you ever
    observe a lower number of names in `Med`? And in `Phys`?

  - Modify the code such that it uses first names (`first_id`) instead
    of last names. Run the randomizations for the humanities (`Hum`) and
    industrial engineering (`Eng-Ind`): why do you observe a scarcity of
    first names in engineering, but an excess in the humanities?

## Summary

  - Most bugs sneak in when you modify the code, not when you write it
  - Unit testing helps in these cases: you can automatically check the
    code every time you make changes
  - Test-driven development: write the tests before writing the code\!
  - Another common source of bugs is input with unexpected features:
    using assertions, you can make sure the input is of the right type
  - Good strategies for avoiding bugs:
      - Pair programming
      - Code reviews
