# Investment simulation: Magic: The Gathering

Day 1 lab that follows the writing functions block. Students build small R functions that open a booster pack, price it, open a box and simulate many boxes. Then they use the simulation to decide whether a box of cards is a good investment. We replay the original 2018 trade (100 boxes of Guilds of Ravnica at $79.50) and ask the same question about Reality Fracture in 2026. After that, the class opens a real Play Booster Display and compares the pulls with the simulation.

## Contents

| Path | What it is |
|------|------------|
| `F_investment_sim_mtg.pptx` | The deck: non-traditional market setup, the functions, then vs now, practice and the pack opening |
| `scripts/A_sample_w_probs.R` | Warm up: `sample()` with probability weights |
| `scripts/B_triangle_example.R` | Warm up: the triangle distribution for card prices |
| `scripts/C_mtg_investment_BASIC_STUDENT.R` | Fill in the blanks version with hints |
| `scripts/C_mtg_investment_STUDENT.R` | Function names, inputs and steps are given; students write the bodies |
| `scripts/C_mtg_investment_KEY.R` | Completed lab, including the sensitivity extension |
| `scripts/D_class_pack_opening.R` | In class: price the real pulls and compare them with simulated packs |
| `scripts/Z_mtg_functions.R` | Every lab function in one file; `source()` it if you fall behind |
| `data/guilds_of_ravnica_10_16_18.csv` | 2018 price list: name, rarity, low, market, high |
| `data/reality_fracture_play_booster.csv` | The 417 cards and tokens that can appear in a Play Booster: name, rarity, collector number, treatment, echoed pair flag, market price |
| `data/play_booster_collation.csv` | Each Play Booster slot, its possible outcomes and the published odds |
| `data/reality_fracture_sealed_prices.csv` | Sealed product prices (Play Booster $5.23, Play Booster Display $119.75) |
| `data/reality_fracture_prices_all.csv` | All 470 singles, including Collector Booster exclusives (reference only) |
| `data/class_pulls_template.csv`, `data/class_predictions_template.csv` | Blank sheets for the live pack opening |
| `data/class_pulls_example.csv`, `data/class_predictions_example.csv` | A simulated box, so `D_class_pack_opening.R` runs before class |

The scripts read data from the GitHub raw URLs, so they run without cloning the repo.

## Functions

| Function | Job |
|----------|-----|
| `cardPrice(low, market)` | One sale price for one card, drawn from a triangle distribution between the low price and market |
| `crackPack(cardDF, packsPerMythic, packsPerFoil)` | Opens a classic 15 card pack (2018 structure) |
| `packValue(pack, sellFee, bulkCutoff, bulkPrice)` | Net value of a pack after selling below market, bulk and fees |
| `openBox(cardDF, packFun, nPacks, ...)` | Opens a box with whichever pack function it is given |
| `simulateBoxes(cardDF, packFun, nBoxes, nPacks, boxCost, ...)` | Repeats `openBox()` and returns revenue and profit per box |
| `maxBid(revenue, confidence)` | The most to pay per box for a given chance of profit |
| `compareStrategies(...)` | Opening the box against selling it sealed or selling the packs |
| `addLowPrice(cardDF, discount)` | Adds a low price when a list only has market prices |
| `openSlot(cardDF, slotOdds)`, `openPlayBooster(cardDF, odds)` | Build a 2026 Play Booster slot by slot from the odds file |

## Results from the KEY (100 simulated boxes each)

| | Guilds of Ravnica, 2018 | Reality Fracture, 2026 |
|---|---|---|
| Packs per box | 36 | 30 |
| Box cost | $79.50 paid | about $100 estimated wholesale |
| Mean net revenue per box | $93 | $68 |
| Share of boxes that lose money | 30% | 86% |
| Max bid for an 80% chance of profit | $74 | $43 |
| Profit on 100 boxes, opened | +$1,341 | -$3,182 |

At market value a Reality Fracture box holds about $161 in cards, but most of that sits in cards under $1 that sell as bulk. Selling the 30 packs one at a time at $5.23 is the better 2026 strategy. A simulated pack has a mean market value of $5.10, a median of $3.59, and about 1 in 4 packs is worth more than the pack price.

## Assumptions to discuss
- Sale prices are drawn between the low and market price, most often near market. Reality Fracture only has market prices, so the low price is market less 25% (`addLowPrice()`).
- Cards under $1 sell as bulk at $5 per 1,000; the marketplace keeps 15% of each sale.
- Foils are priced like non-foils, so foil value is slightly understated.
- Odds that Wizards lists only as "<1%" are set to 0.5%. The echoed pair slot draws its three cards from the echoed pair cards of the chosen rarity.
- Special Guests cards (1 in 55 packs) are a separate set and are not in the price list.
- The $100 wholesale price is an estimate for a 100 box order, not a quote.

## Running the pack opening
1. Before opening, each student writes a guess for their pack's market value in a copy of `class_predictions_template.csv`.
2. While opening, record each card's collector number (bottom left of the card) in a copy of `class_pulls_template.csv`. Enter tokens as `T` and mark foils `yes`.
3. Point the two `read.csv()` lines in `D_class_pack_opening.R` at the filled in files and run it. The script lists the best pack, the closest guess, and where the class box falls among the simulated boxes.

## Sources
- Prices: [TCGplayer Reality Fracture price guide](https://www.tcgplayer.com/categories/trading-and-collectible-card-games/magic-the-gathering/price-guides/reality-fracture), Near Mint market prices, retrieved November 1, 2026. Prices move daily.
- Pack contents and odds: [Collecting Reality Fracture](https://magic.wizards.com/news/feature/collecting-reality-fracture), Wizards of the Coast.
- Treatment card counts used to map collector numbers: [Reality Fracture Collector's Guide](https://realityfracture.wiki/guides/collecting/).
- 2018 prices: Guilds of Ravnica list from [kwartler/teaching-datasets](https://github.com/kwartler/teaching-datasets), October 16, 2018.
- Setup slides adapted from the Vienna 2025 deck `Winter/day1/E_NonTraditionalMkts.pptx`.
