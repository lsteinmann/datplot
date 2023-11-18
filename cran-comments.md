## Resubmission
This is a resubmission. In this version I have:
  
* Improved the messaging/warning behaviour as requested (no more printing & 'verbose'-argument for info-messages). 
* (Package was archived on CRAN before due to UTF-8/Latin-1 strings in data and failure to correct on time. All strings have been converted to ASCII.)

## Test environments
* local R installation, R 4.2.2 on Windows
* GitHub actions (release) on macos, win and ubuntu
* GitHub actions (devel and oldrel-1) on ubuntu
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes
win-builder says "Aoristic / aoristic" in the DESCRIPTION may be misspelled, 
but they are not. 
