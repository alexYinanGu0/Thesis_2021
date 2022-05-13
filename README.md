# Social Media Analysis on Panic Buying in the COVID-19 Pandemic

Author: Alex Yinan Guo, Department of Economics, Keio University
 
Supervisor: [Masahiro Fukuhara](https://www.linkedin.com/in/masahiro-fukuhara-8107239/?originalSubdomain=jp), Department of Economics, Keio University
  
Co-supervisor: [Peter Romero](https://www.psychometrics.cam.ac.uk/about-us/directory/peter-romero), Psychometrics Centre, University of Cambridge

July 31, 2021


## Abstract

This paper aimed to explore people's collective response to the COVID-19 outbreak and the behavior of panic buying with the impact of social media. To achieve this goal, sales data of facemasks in Tokyo region from the end of 2019 to mid-2020 was analyzed to explore people’s changes of purchasing patterns and consumption behaviors during the pandemic. In addition, this paper examined social media data from 7,435 Japanese Twitter users and 23,501,189 Tweets from 2009 to mid-2020 to inspect impacts of social media factors, i.e. extracts topics, keywords, and sentiment, on panic buying.

*Keywords*: COVID-19; social media analysis; topic analysis; sentiment analysis; panic buying

## Files

```
.
├── docs                          # Documentation files
│   ├── appendix.pdf              # Appendix of thesis
│   └── results                   # Results
│       ├── before.jpg            # Word cloud of keywords before the 1st COVID case in Japan
│       ├── after.jpg             # Word clout of keywords after the 1st COVID case in Japan
│       ├── Matrix_to_delete.xlsx # An excel file containing features needed to delete according to similarity matrix
│       └── MICLogOdds.xlsx       # An excel file corss-checking same features with high importance from MIC and Log Odds results
├── src                           # Source files
│   ├── SALES.ipynb               # Python code for processing sales data.
│   ├── TOPICS.ipynb              # Python code for processing topics data from Twitter.
│   ├── KEYWORDS.ipynb            # Python code for processing keywords and sentiment data from Twitter.
│   ├── ADD.ipynb                 # Python code for processing addtional variables and events data.
│   ├── SALES_V.ipynb             # Python code for data visualization for sales data. 
│   ├── TOPICS_V.ipynb            # Python code for data visualization for topics data.
│   ├── KEYWORDS_V.ipynb          # Python code for data visualization for keywords and sentiment data.
│   ├── Random_Forest.ipynb       # Python code for creating Random Forest model.
│   └── FEX_GLM.R                 # R code for feature engineering and creating GLMs.
├── thesis.pdf
└── README.md
```
