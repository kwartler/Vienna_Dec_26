# Day 2 scripts

R scripts for Day 2, lettered in the order we use them in class.

| Script | Purpose |
|--------|---------|
| `00_ggplot_echarts4r.R` | The same scatter plot as a static ggplot2 chart and an interactive echarts4r chart, on diabetes data from teaching-datasets |
| `00_flexdashboard_example_master_track.Rmd` | Reference for the Master track post-module: a flexdashboard with echarts4r charts and a DT table. Not covered in class; knit it to HTML to see the layout |
| `E_openrouter_basics.R` | Call large language models through the OpenRouter API: list models and prices, send one request, read the JSON response, wrap it in `askLLM()`, and ask several models the same question |
| `F_nlp_airbnb_bag_of_words.R` | Bag of words on Boston Airbnb reviews: download the data from GitHub (or read the backup in `lessons/data`), keyword scanning, cleaning, the DTM, term frequency, word association, a bigram word cloud, commonality and comparison clouds for top vs lower rated listings, and bing and NRC sentiment with a radar chart |

The deck for the visualization demo is `lessons/day2/00_ggplot_echarts4r.pptx`.

The deck for the OpenRouter block is `lessons/day2/E_openrouter_apis.pptx`. Store your key as described in [`keys.example.md`](../../../keys.example.md) before running the script.

The deck for the NLP block is `lessons/day2/F_nlp_bag_of_words.pptx`.
