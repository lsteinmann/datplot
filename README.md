<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/datplot)](https://CRAN.R-project.org/package=datplot)
[![R-CMD-check](https://github.com/lsteinmann/datplot/workflows/R-CMD-check/badge.svg)](https://github.com/lsteinmann/datplot/actions)
[![codecov](https://app.codecov.io/gh/lsteinmann/datplot/branch/main/graph/badge.svg?token=CVNCAL9U4W)](https://app.codecov.io/gh/lsteinmann/datplot)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4285912.svg)](https://doi.org/10.5281/zenodo.4285912)
[![DOI](https://img.shields.io/badge/Publication-10.1017/aap.2021.8-green.svg)](https://doi.org/10.1017/aap.2021.8)

<!-- badges: end -->

datplot
=======

Converting date ranges into dating 'steps' eases the visualization of changes in e.g. pottery consumption, style and other variables over time. This package provides tools to process and prepare data for visualization.

A rather common problem in archaeology is the fuzzyness of dates assigned to objects. If one wants to visualize overall changes in - let's say - pottery consumption, bar charts often fall short in that regard. If, e.g., the Phases a -- f are employed, then some of the objects can usually be dated to a, c, and f, as an example, but others will by classified as "a to c" or "b to c". But how can these data still be used for examining changes in a large set of objects without completely disregarding the information added by providing multiple phases for one object?

This package proposes implements the concepts of aoristic analysis to prepare archaeological data for the visualization using density plots. An example is shown in the vignettes, which can be found at

    browseVignettes("datplot")

after installing the package, or on GitHub in the /vignettes/ directory. Density plots are easy to understand and are usually aesthetically pleasing. They do omit a some information, such as individual counts, that bar histograms can communicate better. On the other hand, ranges can be incorporated into the visualization as well to regard the variety of timespans archaeological objects may be dated to.

![Attic Pottery from BAPD by Date](inst/extdata/demo_readme.png "Attic Pottery from BAPD by Date")

Publication
-------
The package has been published along with a case study on inscriptions from Bithynia: [Steinmann, L., & Weissova, B. (2021). Datplot: A New R Package for the Visualization of Date Ranges in Archaeology. Advances in Archaeological Practice, 1-11. doi:10.1017/aap.2021.8](https://doi.org/10.1017/aap.2021.8). Data used in the case study is included in the package.

Recommendation
-------
People interested in employing this method should also consider taking a look at [ISAAKiel's package aoristAAR](https://github.com/ISAAKiel/aoristAAR/), or at [archSeries](https://github.com/davidcorton/archSeries), [tabula](https://github.com/tesselle/tabula), [rtefact](https://github.com/ahb108/rtfact) and [aoristic-analysis (LimesLimits)](https://github.com/LimesLimits/aoristic-analysis). 


Installation 
-------
'datplot' is on CRAN with version 1.0.0. 

    install.packages("datplot")


Later versions can be installed from GitHub with devtools:

    devtools::install_github("lsteinmann/datplot")

Or via downloading the latest release and installing from the file: 

    devtools::install_local(path = "../datplot_1.0.0.tar.gz")
    
In case you are unable to find the vignettes after installing from github directly, try: 

    devtools::install_github("lsteinmann/datplot", build_vignettes = TRUE)

But you may have to install vignette dependencies manually (see suggests in the DESCRIPTION). Anyone who has the tidyverse installed should encounter no issues.

Contact
-------

Please feel free to use and change the code to your liking. I would be happy if you contacted me if you do, because I'm interested to know whether this helps anybody. Also, if you have any ideas, critique or improvements, I would be very glad to hear from you! 

