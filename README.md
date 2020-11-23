 <!-- badges: start -->
  [![R build status](https://github.com/anonymousgit/datplot/workflows/R-CMD-check/badge.svg)](https://github.com/anonymousgit/datplot/actions)
  [![Travis build status](https://travis-ci.com/anonymousgit/datplot.svg?branch=master)](https://travis-ci.com/anonymousgit/datplot)
  [![Codecov test coverage](https://codecov.io/gh/anonymousgit/datplot/branch/master/graph/badge.svg)](https://codecov.io/gh/anonymousgit/datplot?branch=master)
  <!-- badges: end -->

datplot
=======

Converting date ranges into dating 'steps' eases the visualization of changes in e.g. pottery consumption, style and other variables over time. This package provides tools to process and prepare data for visualization.

A rather common problem in archaeology is the fuzzyness of dates assigned to objects. If one wants to visualize overall changes in - let's say - pottery consumption, bar charts often fall short in that regard. If, e.g., the Phases a -- f are employed, then some of the objects can usually be dated to a, c, and f, as an example, but others will by classified as "a to c" or "b to c". But how can these data still be used for examining changes in a large set of objects without completely disregarding the information added by providing multiple phases for one object?

This package proposes a method to prepare archaeological data for the visualization using density plots. An example is shown in /vignettes/. Density plots are easy to understand and are usually aesthetically pleasing. They do omit a some information, such as individual counts, that bar histograms can communicate better. On the other hand, ranges can be incorporated into the visualization as well to regard the variety of timespans archaeological objects may be dated to.

I imagine this to be mostly useful for surveys and other analysis aimed at the longue duree. 

![Attic Pottery from BAPD by Date](inst/extdata/demo_readme.png "Attic Pottery from BAPD by Date")

Installation 
-------
'datplot' can be installed from GitHub directly with devtools:

    devtools::install_github("anonymousgit/datplot")

Recommendation
-------
People interested in employing this method should also consider taking a look at [ISAAKiel's package aoristAAR](https://github.com/ISAAKiel/aoristAAR/). 

Contact
-------

Please feel free to use this idea or the package in your research. I would be happy if you contacted me if you do, because I'm interested to know whether this helps anybody. Also, if you have other ideas, critique or improvements, I would be very glad to hear from you or review your pull requests and changes! 

[![Licence](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)
