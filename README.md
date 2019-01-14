# GeneralizedCR
Based on the generalized CR formulation from: 
"Quantifying source and sink habitats and pathways in spatially structured populations: a generalized modelling approach." 
 - Christine Sample, Joanna A. Bieri, Benjamin Allen, Yulia Dementieva, Alyssa Carson, Connor Higgins, Sadie Piatt, Shirley Qiu, Summer Stafford, Brady J. Mattsson, Darius Semmens, Wayne E. Thogmartin, Jay E. Diffendorfer
 
To run the code for any of the case studies:

1. Hypothetical
2. Elk
3. Monarchs
4. Pintails

Navigate to the species folder. 

Here you will find a spreadsheet (.xlsx) that contains data for each class in the example. The formating and location of these files is important since the CR code reads in the data based on location in the spreadsheet. There should be one tab for each season in the annual cycle. For each season, Node Attributes are at the top of the spreadsheet, followed by Path Survival rates and then Path Transitions.  

Also within each folder is the Run*species name*.R code. This is the code that should be sourced to get the CR results. The User Defined Data in this code should match what is provided in the spreadsheets.
 
 
 
The main mathematical calculation is found in CR.R. Users should not need to interact with this code.
