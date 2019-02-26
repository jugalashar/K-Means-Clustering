# K-Means-Clustering
Clustering FIFA19 players and identifying potential players that can be bought.
FIFA19 data set has over 18k records and 100 variables. Some aspects of data are not in the right format or some data is missing that doesnt help us in our analysis.
As a result Data munging has been carried out wherever necessary. Unwanted columns which would disrupt our clusters and those that were not helpful to my analysis have been left out too.
Before performing K-means, no.of clusters to divide data into has been carried out by elbow-point method. Elbow-point is the point from where change in MSE is minimum or null. For my dataset it turns out that the elbow-point is at 7. Thus, I have used 7 clusters.

Have used visualizations of these clusters through GGplot and Plotly libraries. The difference between data points in a cluster are made on basis of player age. If player is less than 27 years old, data point is represented in Green and if player is above 27 years the data point is represented as red. Hovering over data pointsd also revelas player name, Position he plays, his Overall rating, his potential to improve and his Transfer Value.
