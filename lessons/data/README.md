# Course data backups

Local copies so the scripts still run when the classroom Wi-Fi fails. Scripts try GitHub first and fall back to these files; open `Vienna_Dec_26.Rproj` so the relative paths work.

| File | Used by | What it is |
|------|---------|------------|
| `airbnb_boston_reviews_3k.csv` | `day2/day2_scripts/F_nlp_airbnb_bag_of_words.R` | 3,000 English Boston Airbnb reviews (1,500 from top rated listings scoring 98+, 1,500 from lower rated listings scoring 86 or less) with the listing rating, sampled with `set.seed(1234)` from [Airbnb-boston_only.zip](https://github.com/kwartler/teaching-datasets/blob/main/Airbnb-boston_only.zip) |
| `nrc.csv` | `day2/day2_scripts/F_nlp_airbnb_bag_of_words.R` | NRC emotion lexicon (word, sentiment) from [teaching-datasets](https://github.com/kwartler/teaching-datasets) |
