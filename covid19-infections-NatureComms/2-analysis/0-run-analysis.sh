#!/bin/bash

R CMD BATCH 1-obtain-priors-state.R 
R CMD BATCH 2-est-expected-cases-state.R 
R CMD BATCH 3-est-expected-cases-state-perf-testing.R
R CMD BATCH 4-obtain-testing-protocols.R
R CMD BATCH 5-summarize-results.R 
