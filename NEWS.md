# ReSources 24.05.0

## New Features
- _Model diagnostic plots (Trace, AutoCor)_: optionally select the x and y axes ranges to be displayed (#54)

## Bug Fixes
- _Oxcal Export_: catch error when trying to load files for curves without internet connection

# ReSources 23.12.1

## Bug Fixes
- fixes an issue with a missing case when splitting user estimate samples into groups (#109)

# ReSources 23.12.0

## New Features
- _Import of models from Pandora_: 
  - display of "About" information that is associated to a selected Pandora Repository

## Bug Fixes
- _Import of models from Pandora_: 
  - an error message occurred when trying to load a model from pandora.
  - fix: adding the missing download of the zip file from the url before unpacking the zip

# ReSources 23.07.2

## Bug Fixes
- catch definition of source and concentration names when respective data is missing (#99)

# ReSources 23.07.1

## Bug Fixes
- error message when prior names are not found or do mismatch (#100)

# ReSources 23.07.0

## New Features
- checkbox for optionally only displaying the input for nimbleModel() instead of running the model
when pressing _Run_ (#96)

# ReSources 23.06.0

## New Features
- _Import Model_ button: additional option to import a model from pandora or url

# ReSources 23.05.1

## New Features
- button to reset a single table (#3)

# ReSources 23.03.4

## Bug Fixes
- Fix for baseline models when only one proxy present (#83)

# ReSources 23.03.3

## Updates
- remote models are loaded from the github folder `inst/app/predefinedModels` of the respective 
repository
- if there is no internet connection remote models are taken from the models that were saved with
  the last deployed app version

# ReSources 23.03.2

## Bug Fixes
- use most recent version of the shinyMatrix package (#73)
- fix update of tables after using enter to submit values (#82)

# ReSources 23.03.1

## Bug fixes
- forward more specific error messages to the user if conditions to run a model are not met (#81, #83) 

# ReSources 23.02.4

## Bug fixes
- switch to kaleido package instead of orca for plotly export (#76)

# ReSources 23.02.3

## Bug fixes
- fix update of covariate variables (#74)

# ReSources 23.02.2

## New Features
- the _Import Data_ module is now imported from the package DataTools (#70, PR #72)
- additionally to file import, now import from URL or from Pandora Platform is possible
- specific checks for matrix imports are integrated into the new _Import Data_ module
- all redundant code was removed
- using "file" as default source in Import Data

# ReSources 23.02.1

## Bug fixes
- fix bug in export of xlsx files
- fix height of sidebar with auto scroll

# ReSources 22.12.1

## Bug fixes
- fix loop when deleting a column or a row (#58)

# ReSources 22.11.3

## Bug fixes
- fix loop when entering new columns (#64)

# ReSources 22.11.2

## Bug fixes
- endless loop when uploading data with no names of list elements, that is no baseline model (#62)

# ReSources 22.11.1

## Bug fixes
- fix update of list names for sources and concentration tables after names where changed in target
tables, values are not reset anymore (#34)

# ReSources 22.09.1

## Enhancements
- check if uploaded data contains empty tables
- use shinyalerts in data uploads with different types of alarms: "error", "warning", "success"

# ReSources 22.08.2

## Bug Fixes
- fix conflicts with column names (#44)

# ReSources < 22.08.2

## Enhancements
- optimal prior (#42)
- interface updates (#23)

## Bug Fixes
- fix copy-paste bug (#33)
- fix crash after deleting cov column (#12)
