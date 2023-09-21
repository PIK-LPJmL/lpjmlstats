# Project: New Benchmark System

based on ideas documented [HERE](https://hackmd.io/TXPM45ZVQdq5c6u2oIdV_w) 


### Objective

The aim of this project is to develop a new software to benchmark LPJmL with available data sources that is able to
* easily calculate different statistics based on LPJmL and available data for comparison
* summarize data in pre defined metrics (global sums table, global sums time series, country maps, etc.)
* visualize these metrics
* generate reports

**What we mean by benchmarking in the LPJmL community:**   
Check out [the LPJmL wiki](https://gitlab.pik-potsdam.de/lpjml/LPJmL_internal/-/wikis/Benchmarking) and recent merge requests for LPJmL where in most cases a benchmark is mandatory for a successfull merge into the master. 


### Requirements

* [*lpjmlkit*](https://gitlab.pik-potsdam.de/lpjml/lpjmlkit) is the base for this project
* Make usage of the [R6 class object oriented programming system](https://r6.r-lib.org) (more info in the first issue) to natively inherit from *lpjmlkit*'s data classes
* Follow [the guidelines and conventions defined in the lpjmlkit project](https://gitlab.pik-potsdam.de/lpjml/lpjmlkit/-/wikis/How-To/Code-Style-Guide)
* Complete the tasks in the issue board in the given order, starting with issue 1.1


### Instructions

* The issue board is enumarated with tasks corresponding to each Milestone (also defined)
* Use each issue to document the status of your work and ask questions/start discussions if something is unclear or problems arise
* If necessary new issues can be added for each Milestone

### Reference data

* there's a collection of possible reference data sets [here](https://gitlab.pik-potsdam.de/lpjml/LPJmL_internal/-/wikis/Future-benchmarking)
* reference data should be downloaded, processed, stored and used in a strucutured manner, such as with the MADRAT universe

### Contact
For any questions add @jannesbr to the corresponding question or write a mail at [jannesbr@pik-potsdam.de](mailto:jannesbr@pik-potsdam.de)
