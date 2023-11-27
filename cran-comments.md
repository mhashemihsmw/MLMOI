## Resubmission

This is a resubmission. In this Version: 

* we have replaced the XLConnect package with openxlsx package to import/export excel files

## Resubmission
This is a resubmission. In this version I:

* omited the redundant "An R Package to" from the title.
* reduced the title to less than 65 chars.
* removed "The package MLMOI..." from the beginning of description.
* fixed the references in the description.
* added () behind all function names in the description text.
* made sure the settings are reset by using on.exit() properly.
* avoided writing information messages to the console that cannot be easily suppressed. 
* added small executable examples in your Rd-files.

## Test environments
* local OS X install, R 3.1.2
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 
