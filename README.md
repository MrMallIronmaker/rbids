# rbids - the R interface to the Brain Imagining Data Structure

BIDS is cool!

I work in R, and it solves my problems for standardized datasets.

## Quick Sample

```r
library(rbids)

bids <- Bids$new(root = "/Users/fengsifang/Documents/PuzzleProjectBIDS")

valid_participants <- bids$load_participant() %>%
  dplyr::filter(Anomaly == "No")

motion_data <- bids$load_motion(
  tracksys == "LeftControllerStabilizedMovement",
  subject %in% valid_participants$participant_id
)

log_files <- bids$load_logs()
```
