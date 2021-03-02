## Test environments
* local R installation, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## R CMD check results (rhub)

Maintainer: 'Lisa Steinmann <lisa.steinmann@rub.de>'
  
  New submission
  
  Found the following (possibly) invalid URLs:
      From: README.md
    URL: https://codecov.io/gh/lsteinmann/datplot
      Status: Error
        	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
      Message: libcurl error code 35:

0 errors √ | 0 warnings √ | 1 note x

## Notes

Sometimes a Note states that there are UTF-8 and Latin1 marked characters. 
I think it is because of the raw data, and doesn't seem to influence 
the package build on any system (using travis).
