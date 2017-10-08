# Statistics 2501: Clustering Project
## Temple University, Fox School of Business

### Who are you?
Group 3 consists of David Azizi and Max Tsisyk.

### What's going on here?
This is the repository for Group 3's attempt at clustering flow cytometry data from the FlowCAP-I project. We're looking specifically at the GvHD dataset, and starting with Kmeans before moving to a supervised learning system.

### What are all these files?
The files named like people contain code fragments that we used in constructing the main file. So too do the other code files.

The file "Main R Code.r":
1. takes you from a folder of ".fcs" files to a dataframe containing all 12 patients,
2. creates elbow plots for each of those 12 patients based on kmeans with k = 1:30,
3. adds the manual gates to the dataframe,
4. runs kmeans on each patient (without the manual gates) and evaluates the results with a confusion matrix, an Fmeasure, and NMI.

The "Helpful Reading" folder contains research papers and guides that helped us along the way and will be cited in our final report.

The "Graphics" folder contains plots made in R during the course of exploration or as the result of one run of the main file.

"FlowCore" data and "Manual Gates" contain the data we started with.

The other .Rdata objects contain exports of the data at various stages of the main file. The most useful is probably "dataframe.rdata" which itself contains 12 dataframes (one for each patient) and includes the manual gate information.
