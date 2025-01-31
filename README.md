# rbids - the R interface to the Brain Imagining Data Structure

BIDS is cool!

I work in R, and it solves my problems for standardized datasets.

## Quick Sample

```r
# devtools::install_github("markromanmiller/rbids")
library(rbids)

bd <- bids("path/to/the/bids/dataset")

# get participant-level data
bd %>%
  bids_subjects()

# get metadata about all the sessions files
bd %>%
  bids_sessions()

# get metadata about all the motion files
bd %>%
  bids_motion_datafiles()
```
