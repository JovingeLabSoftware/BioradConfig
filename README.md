# BioradConfig

## Background

`BioradConfig` includes a collection of classes and functions for interacting with the Lab's inventory systems. It's main functionality includes:

- providing basic functions for interacting with the APIs of our data management applications ([REDCap](http://project-redcap.org/) & [LabGuru](http://www.labguru.com/))
- providing a helper class `SyncHandler` that syncs inventory information between [REDCap](http://project-redcap.org/) & [LabGuru](http://www.labguru.com/)
- providing the data managment back-end for a [Shiny](http://shiny.rstudio.com/) application for planning [Bio-Rad Bio-Plex](http://www.bio-rad.com/en-us/product/bio-plex-200-systems) cytokine profiling experiments
