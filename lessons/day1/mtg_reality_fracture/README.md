# MTG Reality Fracture functions lab

Data for the Day 1 functions lab: build `openPack()` and `packValue()` from the price list and pack odds, simulate a Play Booster box, then open the real box in class and compare.

| File | Contents |
|------|----------|
| `reality_fracture_play_booster_prices.csv` | The 417 cards and tokens that can appear in a Play Booster: name, rarity, collector number, treatment, market price in USD. **Use this one for the lab.** |
| `play_booster_collation.csv` | What is in each Play Booster slot and the published odds |
| `reality_fracture_prices_all.csv` | All 470 singles, including the 53 Collector Booster exclusives (reference only) |
| `reality_fracture_sealed_prices.csv` | Market prices for sealed products; a Play Booster Display (30 packs) lists at $119.75 |

## Sources
- Prices: [TCGplayer Reality Fracture price guide](https://www.tcgplayer.com/categories/trading-and-collectible-card-games/magic-the-gathering/price-guides/reality-fracture), Near Mint market prices, retrieved November 1, 2026. Prices move daily.
- Pack contents and odds: [Collecting Reality Fracture](https://magic.wizards.com/news/feature/collecting-reality-fracture), Wizards of the Coast.
- Treatment card counts used to map collector numbers: [Reality Fracture Collector's Guide](https://realityfracture.wiki/guides/collecting/).

## Notes
- Collector numbers 402 to 461 are excluded from the Play Booster list: Japan Showcase foils (403 to 422), extended art (423 to 431), foil-only borderless (438 to 448) and Facet Foil shattered mirror (449 to 461).
- Foil versions of regular cards are not priced separately; the foil slot uses the non-foil price, so simulated values understate foils slightly.
- Special Guests cards (1 in 55 packs) are a separate set and are not in the price list.
- Odds Wizards lists only as "<1%" have no number in `play_booster_collation.csv`.
