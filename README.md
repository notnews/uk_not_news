### Not News: Provision of Apolitical News in British News Media

What proportion of news is *not news*? What proportion of news stories are about topics unrelated to public affairs, very broadly construed, and instead are about things like cooking, sports, travel, movie reviews and such. Using a large corpus of text from web pages from hundreds of news outlets from the U.K., we tally the provision of *not news.* We describe how the provision of *not news* varies across outlets and over time.

#### Scripts, Figures, and Write-up

* [Scripts](scripts/)
    - [Coding News Media Outlets](scripts/00_uk_coding_outlets.ipynb)
    - [Summary of the Data](scripts/01_subset_summarize_uk_news_media_data_by_label.ipynb)
    - [Produce Summary Table for the Data](scripts/01a_describe_data.R)
    - [Not News Classifier](scripts/02_url_classify_uk.ipynb)
    - [Pretty Model Validation Tables, Top Coefficients, etc.](scripts/02a_model_stats_interp.R)
    - [Share of Not News, Over Time, etc.](scripts/03_describe_not_news.R) (see also [here](scripts/02_url_classify_uk.ipynb))

* [Tables](tabs/)
* [Graphs](figs/)
* [Manuscript (.tex, .bib, pdf)](ms/)

#### Data

Given copyright issues, we cannot share full-text data publicly. An abbreviated dataset without the story text but including the URL, source,  date, predicted and training labels can be [found here](https://doi.org/10.7910/DVN/VZ8DB3). 

We are happy to share the raw article text data under the following conditions:

* you will not share the data with anyone else, and 
* you will only use it for research purposes. 

To request the data, please fill out this [form](https://goo.gl/forms/WMv6qtmr5H4IehgF3). If your request is approved, you will get read access to a file in a Google Coldline Storage bucket for a month. The bucket is setup such that the requester pays. Thus, you will need to create a project that can be used for billing.

#### Authors

Suriyan Laohaprapanon and Gaurav Sood

#### Contribute to the Project

If you see something, create a pull request or issue for that something! Be it an inconsistency in the data, issue with the analysis or writing, or a suggestion, or data that you would like to contribute to the project, or something else.
