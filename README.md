### This report repo is in response to 2022 GWU Datathon

For the final report: please refer to the pdf version `report.pdf`.

For the final report with code: please refer to the Rmarkdown `report.Rmd`.

For the final vedio presentation: please refer to the [link](https://drive.google.com/file/d/1_gck0DtHVBHyAjeM3WF0HU6K7DlB1_7Z/view?usp=sharing).

For the vedio presentation slide: please refer to the `presentation.pdf`.

For the data preprocessing pipeline code: please refer to the `data_pipeline.R`.

For the customized helper functions: please refer to the `function.R`.

### Overview
The social media has become an indispensable part in human’s life, which has served as a major platform for knowledge & news sharing and makes it possible for people around the
world get connected. Among all the social media platforms, Twitter has been a critical tool for politicians to advocate their policies. Therefore, a natural question to ask is how politicians can make Twitter users to engage with their tweets so that they can amplify their influence and facilitate the proceedings of their policies and campaigns. In this report, given the tweets posted by the US senators and representatives in the month January 2021, I will investigate how the emotions expressed in the tweets, such as anger, joy, sadness etc., are affecting people’s engagements with the politicians’ tweets, both quantitatively and qualitatively. The final data set used for the analysis has 19 variables and 39396 observations/tweets. The outcome variable is binary and it identifies whether or not a tweet has active engagement with people. The independent predictors are composed of two parts: user-specific predictors and tweet-specific predictors. A set of logistic models are fitted to investigate the contributions of different emotions to the probability of people engaging with the tweet. The result shows that, among all the analyzed emotions (anger, joy, sadness, anticipation, trust, surprise and disgust), “anger” contributes more to the tweets engagement then other emotions, “joy” makes the second largest contribution. In addition, the contribution from “anger” and “joy” to the tweets engagement varies dependent on which party the politician belongs to. Democratic and republican politicians are more divided in their tweets content when the tweets express anger emotions, while they are less divided when the tweets content express joy emotions.

