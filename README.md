<!-- README.md is generated from README.Rmd. Please edit that file -->
datplot
=======

Converting date ranges into dating 'steps' eases the visualization of changes in e.g. pottery consumption, style and other variables over time. This package provides tools to process and prepare data for visualization.

A very common problen in archaeology is the fuzzyness of dates assigned to objects. If one wants to visualize overall changes in - let's say - pottery consumption, bar charts often fall short in that regard. Let's say we have Phases a -- f, then some of the objects can usually be dated to a, c, and f, as an example, but others will by classified as "a to c" or "b to c". But one can these data still be used for examining changes in a large set of objects?

I would propose using density plots as I did in the Demo (vignettes). They are easy to understand and are usually asthetically pleasing. Obviously they do omit a great number of information, such as individual counts, that bar charts display a lot better. On the other hand, ranges can be incorporated into the visualization as well, which is a lot more difficult using bar charts.

I imagine this to be mostly usefull for surveys and other analysis aimed at the longue duree.

![Attic Pottery from BAPD by Date](vignettes/demo_image.png "Attic Pottery from BAPD by Date")

Contact
-------

Please feel free to use this idea or the package in your research. I would be happy if you contacted me if you do, because I'm interested whether this helps anybody. Also, if you have other ideas, critique or improvements, I would be very glad to hear from you.

[![Licence](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)
