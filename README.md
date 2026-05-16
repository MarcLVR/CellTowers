# Cell Tower Coverage in Egypt — Operators and Radio Technology

A geospatial analysis of mobile cellular infrastructure across Egypt at the individual tower level, broken down by operator (Etisalat, Orange, Vodafone, WE) and radio family (GSM, UMTS, LTE, NR). Interactive Leaflet maps surface operator concentration, generational rollout, and the urban–rural divide on the network.


**Author:** Marc Pérez Bernús

---

## What this is

Four operators, four radio generations, one country. The site makes the structure of Egypt's mobile network visible at a glance: where each operator has put its towers, how 2G/3G/4G coexist, and where 5G NR is beginning to appear.

This is a side project at the intersection of two things I work with day to day — telecom (I'm Project Manager at OmniAccess, an industry-leading maritime connectivity provider) and geospatial data. Egypt was chosen because it has reasonable OpenCellID coverage, a clean four-operator structure, and meaningful urban/rural variation along the Nile.

---

## Dataset

> [OpenCellID](https://opencellid.org), the largest open database of mobile cell towers worldwide. Filtered to MCC 602 (Egypt). MNC codes mapped to operator names (Etisalat, Orange, Vodafone, WE). Radio family tagged from the dataset's `radio` field (GSM, UMTS, LTE, NR).

OpenCellID is crowdsourced. Coverage is not uniform: urban areas (Cairo, Alexandria, Giza, the Nile Delta) are densely captured; the Western Desert and the southern interior are not. Reported tower counts in this analysis are therefore a *lower bound* on actual infrastructure, and that asymmetry shows up in every map below.


---

## Maps

Two parallel views of the same data.

**Per operator** — one map per operator, all radio technologies overlaid:

- `tower_map_Etisalat.html`
- `tower_map_Orange.html`
- `tower_map_Vodafone.html`
- `tower_map_WE.html`

**Per radio family** — one map per technology, all operators overlaid:

- `map_radio_GSM.html`
- `map_radio_UMTS.html`
- `map_radio_LTE.html`
- `map_radio_NR.html`

The interactive view lets you zoom into specific cities, toggle layers, and click towers for metadata.

---

## What the maps surface

- **Geographic concentration.** Tower density follows the Nile Delta and the Cairo–Alexandria corridor; sparse coverage in the Western Desert and southern interior — likely a mix of real infrastructure thinness and OpenCellID's urban bias.
- **Operator footprint.** _[Which operator leads in tower count? Where do they overlap vs. differentiate?]_
- **Technology mix.** _[Where does 2G GSM still dominate? Where has 4G LTE consolidated? Where, if anywhere, is 5G NR visible?]_
- **Urban vs. rural generational gap.** _[Older technology persistent in rural areas vs. newer concentrated in cities?]_

---

## Reproducing the results

```bash
git clone https://github.com/MarcLVR/CellTowers
cd CellTowers
```

Render the site with Quarto:

```bash
quarto render        # builds _site/
quarto preview       # local preview at http://localhost:port
```

Requirements: R (>= 4.0), Quarto (>= 1.3), and the R packages used in `plots.R` and `CellTowers.Rmd` (`sf`, `leaflet`, `tidyverse`, `dplyr`, `htmlwidgets`).

---

## Repository structure

```
.
├── README.md
├── LICENSE
├── _quarto.yml             # Quarto site config
├── index.qmd               # Landing page
├── about.qmd               # Project context
├── CellTowers.Rmd          # Main analysis notebook
├── plots.R                 # Map generation logic
├── styles.css              # Site styling
├── tower_map_*.html        # Per-operator interactive Leaflet maps
├── map_radio_*.html        # Per-radio-family interactive Leaflet maps
├── _site/                  # Rendered Quarto output
└── docs/                   # Supporting documentation
```

---

## Limitations

A few things worth flagging up front.

**Crowdsourced data.** OpenCellID is not authoritative. Operator-published infrastructure data (where available) would correct the rural undercount and the unevenness of the urban capture. This analysis assumes the relative pattern across operators/radios is informative even if absolute counts are not.

**Static snapshot.** No temporal dimension. The picture reflects the dataset at one moment in time and won't capture the in-progress 5G NR rollout that's happening as I write this.

**Towers ≠ coverage.** This is a tower-location analysis, not a signal-propagation model. A real coverage view would require terrain, antenna pattern, transmit power, frequency band, and clutter modelling — none of which are in scope here.

**No validation against operator filings.** Operator names are derived from the MNC mapping in OpenCellID; I haven't cross-checked counts against operator-published infrastructure reports.

---

## Stack

R, Quarto, Leaflet, `sf`, `tidyverse`, RMarkdown, `htmlwidgets`.

---

## License

MIT — see [LICENSE](LICENSE).

---

## Author

Marc Pérez Bernús

- GitHub: [github.com/MarcLVR](https://github.com/MarcLVR)
- LinkedIn: [linkedin.com/in/marcpb](https://www.linkedin.com/in/marcpb)
