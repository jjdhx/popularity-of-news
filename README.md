# popularity-of-news

In an era of information, numerous pieces of information are exchanged constantly and spread rapidly through Internet. One of the most organized forms of information is online news, which provides timely information on ongoing events compared to traditional media. Hence, predicting the popularity of online news has become a trendy topic. 

This study is dedicated to this trend. We focused our exploration on Mashable website, one of the largest online news publishing websites. In this study, we carefully selected most relevant variables to our prediction from a set of available features. 


We started with the parametric classification methods, logistic regression and Linear Discriminant Analysis (LDA). Also, we applied the following nonparametric classification methods to predict the popularity: k-Nearest Neighbors (kNN); Support Vector Machine (SVM); Adaptive Boosting (AdaBoost) and Random Forest. Finally, we compared the performances of all the classification methods and analyzed the top important features that affect the popularity. 

We applied feature selection after data standardization, and proved that very little prediction accuracy is sacrificed. Overall, all the models had prediction accuracy on the test set between 63% to 66%, while Random Forest had the best predicting performance. AdaBoost and Random Forest provided balanced true positive and true negative rates, thus they would have larger AUC values. However, in real life, people may pursue specific high true negative rate. In this case, SVM with larger cost on soft margin was recommended. On the other hand, LDA with “t” estimating method and SVM with smaller cost turned out to hold high true positive rate. Nevertheless, our large data set considered, SVM was not recommended because of the long running time. 

We also analyzed the reason for an online news article of being popular. According to our study, social media and technology news tended to be more popular than those in categories of lifestyle, entertainment, business and world. Moreover, publishing news on weekends is a good idea. One should also include more links, images and keywords in the article to gain popularity.  


