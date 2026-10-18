# Forcing normalisation (global equivalent against regional mean): review by Opus 5.5 standing in for Fable (with reconciliation)

_Asked 18 October 2026, with the same question, code and run as Astra (`2026-10-18_forcing_normalisation_review_codex_gpt6astra.md`) and before seeing Astra's view. Fable's usage limit had been reached that day, so Chris chose Opus 5.5 at maximum effort, given the `science-reviewer` role text, as the stand-in; the text calls itself Fable because of that role. Its findings 2 and 4 (the vegetation and ice variants; coastal sea area) were then put to Astra, see `2026-10-18_forcing_code_check_codex_gpt6astra.md`._

**Verdict.** Chris is half right. The global equivalent is a valid like-for-like comparison and should stay the headline. It is the same normalisation the IPCC used for its land-use albedo number, which was itself made with satellite albedo and radiative kernels, as this pipeline does. But on its own it hides how strong the forcing is where it happens. Neither of his rescalings, as he worded them, has published support.

**Benefit.** The paper needs a headline comparison that a referee will accept as like-for-like and that does not hide how strong the North American signal is locally. Choosing the normalisation decides which sentence the paper can write.

---

## Hand check (8–10 ka, HadGEM3, `veg_ice_thresh`)

Source: `.rundirs/full-current-2026-10-18/output/forcing/forcing_by_period.csv`, row 7.

**Conversion from domain mean to global equivalent**
- Domain mean = 4.90286736 W/m².
- Area = 19,223,828,407,176.9 m².
- Earth's area = 5.101e14 m².
- Area fraction = 1.92238e13 / 5.101e14 = 0.0376864.
- 4.90286736 × 0.0376864 = 0.1847714 W/m². The CSV's `global_equiv` is 0.184771379762344, so the two agree exactly.
- They agree exactly because the HadGEM3 column has no NA. `weighted.mean(..., na.rm=TRUE)` (9_forcing_barplot.R:155) and `sum(rf*area)/earth_area` (:156) then cover the same cells. The same identity holds for CACK: 3.81214735 × 0.0376864 = 0.1436661, matching the CSV.

**Breakdown by latitude band** (from `forcing_by_month_latitude.csv`, HadGEM3, 8–10 ka). I summed `contribution_to_annual` over the 12 months of each band:

| Band | Contribution, W/m² global | Annual band mean, W/m² of band |
|---|---|---|
| 27–30 N | −0.00105 | |
| 30–40 N | −0.01632 | −1.96 |
| 40–50 N | 0.03279 | 3.50 |
| 50–60 N | 0.13563 | 14.5 |
| 60–70 N | 0.02934 | 3.57 |
| 70–74 N | 0.00438 | 3.93 |
| **Total** | **0.18477** | |

- The total matches script 9 to rounding, so script 10's breakdown adds up.
- The 50–60 N band alone gives 73% of the bar. Its June to August band means are 30–32 W/m².
- So the domain mean of 4.9 is itself a 3x dilution of the core signal, and the global equivalent is a further 26.5x.

**Telescoping across periods**
- Summing the seven global equivalents gives the forcing of the 50 BP surface relative to the 11.5 ka surface: **+0.329 W/m²**.
- The same sum of domain means is 8.722 W/m². 8.722 × 0.03769 = 0.3287, which matches.
- 50 BP relative to 6 ka is the sum of the four youngest periods: **−0.057 W/m²** global equivalent.

**Sign and units**
- Positive means albedo fell (8_radiative.R:99–104).
- HadGEM3 is W/m² per 1% albedo and stored negative, hence ×100. CACK is per unit albedo and stored positive, hence −1.
- I re-read these lines and they are consistent.

---

## Findings (worst first)

**1. Should-fix: the bar labelled "albedo (land use)" is not albedo only.**
- Where: 9_forcing_barplot.R:188–189.
- What: AR6's `land_use` ERF of −0.20 W/m² is surface albedo −0.15 plus irrigation −0.05. AR6 §7.3.4.3 takes the albedo part from Ghimire et al. 2014, which used MODIS albedo plus radiative kernels, normalised to the global mean.
- Consequence: the most like-for-like agent on the chart is mislabelled and 33% too large for albedo. The irrigation part is a cloud and humidity effect, not albedo.
- Evidence: the AR6 text as quoted by a search of the chapter's final government distribution: "that study is used as the assessed albedo ERF … The ERF from irrigation, assessed to be –0.05 … Adding the irrigation effect … –0.20 ± 0.10". The ipcc.ch pages returned 403, so I could not read the chapter directly.
- Check: assert that the plotted value equals the label's definition, or relabel the bar "land use (albedo + irrigation)".

**2. Should-fix: the vegetation and ice variants of the sensitivity figure are not the parts of the total.**
- Where: 9_forcing_barplot.R:151 (period sum with `na.rm=FALSE`), :155 (`na.rm=TRUE` mean), :157, :171–175; R/differences.R:132–139 under `difference_method = "endpoint"`.
- What happens:
  - Each pair is either a vegetation change or an ice change; the other entry is NA.
  - A cell that goes from ice to vegetation within a period has NA in some pairs of both variants. Its whole cell-month then drops out of both `veg_thresh` and `ice_thresh`.
- Numbers for 8–10 ka:
  - veg −0.0364 + ice +0.0385 = 0.0021 W/m², against a total of 0.1848. About 99% of the bar, the deglaciating cells, appears in neither.
  - `ice_thresh`'s "domain_mean" of 11.61 W/m² is averaged over about 1.69e12 m² (8.8% of the domain), while its `area_m2` column says 1.92e13. Implied valid area: 0.03849 × 5.101e14 / 11.61.
- Consequence: the sensitivity figure suggests ice and vegetation nearly cancel at 8–10 ka. Anyone who reads `domain_mean` for these variants gets a number over a different, unstated area.
- Check: output the valid area per variant and period, and assert that veg + ice + mixed cells equals the total.

**3. Should-fix (records): the code calls the global-equivalent headline a decision, and its wording overstates the case.**
- config.R:130–131 says "(decision C5, 2026-10-14)". But C5 in the questions file still asks Andria to "Please confirm", and Chris now objects. Either an explicit choice by Chris exists and is being reopened, or this was not a decision. The record should show it as open.
- The wording in 9_forcing_barplot.R:91–95 ("overstates the Holocene signal … the like-for-like comparison") and :290 ("NOT comparable") is too absolute.
  - The global equivalent is like-for-like for the energy-budget question only.
  - For well-mixed gases, the global mean is roughly the local value, so a regional mean beside CO₂ is roughly like-for-like for local strength (see answer 3).

**4. Should-fix, size unknown: cell area includes the sea in coastal cells.**
- Where: scripts/7a_alb_diff_full.R:95–98. `area` is `st_area` of the whole 1° polygon.
- Consequence: in coastal cells, the ocean fraction is counted as land whose albedo changed. This raises the global equivalent and changes the domain-mean weights.
- It matters most around Hudson Bay, in the 50–60 N band that carries 73% of the 8–10 ka bar.
- Check: weight by land fraction (the MOD44W water mask under `data/mod44w/` would serve) and compare the two.

**5. Nit: the kernels are modern, which slightly understates early-Holocene summer forcing.**
- Kernels are roughly proportional to the sunlight reaching the top of the atmosphere. Early-Holocene June–July sunlight at 60 N was several percent above today's (the Berger insolation calculation).
- The forcing here is concentrated in May–August, so the early bars are probably understated by a few percent. This is real but small next to the 26x question.
- I found no published precedent for scaling a kernel by insolation, so I recommend no change, only a sentence.

---

## Answers to the five questions

### 1. Is the global equivalent valid, and does it under- or over-state the effect?

**(a) Contribution to the global energy budget.** Yes, it is valid, and it is exactly what the IPCC numbers measure.
- sum(F·A)/A_Earth is North America's change in global-mean top-of-atmosphere forcing.
- This is the convention for every regional surface-albedo forcing in the assessments:
  - AR6 land use (Forster et al. 2021, AR6 WG1 ch. 7, doi:10.1017/9781009157896.009), via Ghimire et al. 2014 (GRL 41:9087–9096; DOI 10.1002/2014GL061671 from memory, not verified). Their method is satellite albedo plus kernels, giving −0.15 ± 0.1 W/m².
  - Betts 2001 (Atmos. Sci. Lett.): −0.2 W/m² global mean, with regional cooling of 1–2 K.
  - Pongratz et al. 2009 (GRL 36:L02709, doi:10.1029/2008GL036394).
  - Ward & Mahowald 2015, "Local sources of global climate forcing" (ESD 6:175, doi:10.5194/esd-6-175-2015).
  - In paleoclimate, land-ice and vegetation albedo are routinely expressed as global-mean forcing next to GHG forcing: Köhler et al. 2010 (QSR 29:129–145, doi:10.1016/j.quascirev.2009.09.026).
- It does not understate (a).
- It is also the only normalisation that does not depend on where the domain boundary is drawn. Adding unchanged cells leaves it unchanged; the domain mean falls.

**(b) Strength where it happens.** Here it understates, by 26.5x on average and much more in the core.
- The core is the 50–60 N band at 14.5 W/m² annual, about 30 W/m² in summer.
- The domain mean is itself an arbitrary dilution of that, by about 3x.
- This is Chris's valid point.

**(c) A global Holocene land-surface albedo forcing.** The global equivalent is not that number, and it is incomplete rather than diluted.
- Thompson et al. 2022 (Sci. Adv. 8:eabj6535, doi:10.1126/sciadv.abj6535; CESM1.2, APRP diagnosis) give the vegetation albedo forcing at 6 ka relative to pre-industrial:
  - +2.41 W/m² global mean, of which +1.91 is the Sahara.
  - That leaves about +0.5 W/m² for all other Northern Hemisphere vegetation.
- North America's 6 ka-versus-50 BP contribution here is +0.057, about a tenth of that remainder. Their vegetation prescription is idealised, so this is an order-of-magnitude comparison only.
- The early-Holocene bars are mostly the retreat of the Laurentide ice. That is most of the world's land-ice albedo change after 11.5 ka, but the vegetation part is a small share of the global vegetation forcing.

### 2. Option A: scaling the North American effect up to a global estimate

- **Plotting the domain mean as if it were global** (what slide 18 effectively did) assumes the whole planet, oceans included, changed like deglaciating North America. I found no precedent; it is not defensible.
- **"Assume other land behaved similarly"** (×7.7 for land area): no precedent, and it is contradicted by the evidence.
  - The Sahara dominates the 6 ka vegetation forcing and has the opposite timing: it greened, then brightened after about 5 ka.
  - The Fennoscandian ice sheet was gone by about 10 ka.
  - The error would be several-fold, and of the wrong sign in the late Holocene.
- **What is defensible:**
  - Report North America's contribution as a fraction of published global numbers: Thompson 2022 for vegetation; Köhler et al. 2010 for the land-ice albedo series (values not checked; full text inaccessible).
  - Or, as a separate project, apply the same kernel method to global reconstructions: LandCover6k (Harrison et al. 2020, GMD 13:805, doi:10.5194/gmd-13-805-2020) and the mid-Holocene vegetation-feedback model studies (Otto et al. 2011, Clim. Past 7:1027; Thompson 2022).

### 3. Option B: comparing at the regional scale

- **CO₂ and CH₄.** The forcing is nearly uniform, so the global mean is roughly the local value.
  - Huang, Tan & Xia 2016 (JGR 121:2780, doi:10.1002/2015JD024569) show local CO₂ forcing varies with the vertical temperature profile and with clouds.
  - Their 2xCO₂ map is open: doi:10.17632/khmt3myywn.1, CC-BY. Its domain mean divided by its global mean could scale AR6's 2.16 W/m².
  - I found no paper doing exactly that scaling; it would be our assumption and should be stated as such.
- **Aerosols and land use.** These are strongly regional, and North America's regional values would be larger in magnitude than the global means. A regional comparison would therefore make the Holocene look relatively less dominant than slide 18 did.
- **Regional ERF sources:**
  - CMIP6 piClim-aer and piClim-lu (Smith et al. 2020, ACP 20:9591, doi:10.5194/acp-20-9591-2020). These are 30-year fixed-SST runs, and the paper warns that "the climatological-SST method … includes the TOA flux changes resulting from land surface warming or cooling as part of the ERF". Land use is −0.09 ± 0.13 W/m², noisy across models.
  - For land-use albedo, Ghimire 2014's map is the method-matched source. I could not access its regional values.
- **Is it like-for-like?** Roughly yes for the gases, using a local instantaneous albedo forcing against a near-uniform ERF. It needs new regional data for aerosols and land use, and regional ERF is not robustly defined because adjustments are not local.
- **Precedent:** Pongratz 2009 compared a regional land-cover albedo forcing (−2.0 W/m² over parts of India in AD 800) with present-day GHG forcing "at a strength comparable", alongside a global mean below 0.05. Carlson et al. 2008 (Nat. Geosci., doi:10.1038/ngeo285; abstract seen only in a search snippet) compared regional early-Holocene summer forcing over the Laurentide with GHG forcing.

### 4. Is the current approach common for regional land-cover forcing?

Yes, and reporting both numbers is the norm.
- Pongratz 2009 (both numbers), Betts 2001 (global forcing plus regional temperature), Ghimire 2014 (global forcing with regional variation), Ward & Mahowald 2015 (where the global forcing comes from), Köhler 2010 (paleo albedo forcing as a global mean).
- Davin et al. 2007 (GRL 34:L13702, doi:10.1029/2007GL029678) caution that land-cover forcing produced "substantially lower" cooling than an equal CO₂ forcing. Equal W/m² therefore does not mean equal climate effect, a caveat the paper should carry.

### 5. Recommendation

Use two numbers, each labelled for the question it answers. No rescaling.

1. **Headline bars: keep the global equivalent.**
   - Label it "North America's contribution to global-mean forcing".
   - Compare it first with land-use albedo at −0.15, then with the other agents.
   - Defensible sentence: the 10→8 ka change alone (+0.18 W/m²) is as large as all industrial-era land-use albedo forcing, with the opposite sign, and about a third of methane's.
   - Precedent: AR6/Ghimire, Pongratz, Köhler.
2. **Second panel or figure: strength where it happens.**
   - Show latitude bands or a map; script 10's `band_mean` already gives these.
   - Optionally add the domain mean, compared in the text with CO₂'s roughly uniform 2.16 W/m².
   - Never put it on the same axis as global-mean aerosol or land-use values.
   - Precedent: Pongratz 2009.
3. **In the text: global context.** Quote published global values (Thompson 2022; Köhler 2010) rather than scaling.
4. **Optional:** cumulative state relative to 50 BP, which is a cumulative sum of the existing period values (+0.33 W/m² from 11.5 ka).

Everything here comes from `forcing_by_period.csv` and `forcing_by_month_latitude.csv`; no new pipeline code is needed except the label fix.

**Is the slide's domain mean defensible on its own?** Yes, if labelled as "mean TOA albedo forcing over the 27–74 N study area, W per m² of study area". Two caveats: its value depends on where the boundary is drawn, and it was compared with global-mean aerosol and land-use numbers, which is not like-for-like.

---

## Questions for Chris or Andria

1. **Which question does the headline answer?** (i) Energy-budget contribution only (now); (ii) that plus a regional-strength panel (recommended); (iii) domain mean beside the IPCC values, as on slide 18. Meanwhile the code does (i), with the domain mean in a separate figure.
2. **Which IPCC land-use bar?** −0.15 (albedo only, the method-matched value) or −0.20 (with irrigation)? Meanwhile it is −0.20 labelled "albedo (land use)".
3. **Bars as increments or as a cumulative state?** Per-period increments (now) or the cumulative change relative to 50 BP?
4. **Should the paper attempt a global Holocene estimate?** Options: cite published values in the text, or extend the method globally (a separate project). Meanwhile neither is done.
5. **Is a vegetation/ice split needed in the paper?** If so, the sensitivity variants must be redefined (finding 2).
6. **Coastal area weights:** weight by land fraction? Meanwhile the whole 1° cell area is used.
7. **For Andria: the word "forcing".** Should the paper call vegetation and ice changes that responded to orbital forcing a "forcing", in the prescribed-boundary sense?
8. **C5 status.** Is it decided (config.R:131) or reopened by Chris's objection?

---

## Not checked

- R/kernels.R, script 10's code and the kernel files.
- Ghimire 2014's regional values (no access to the paper).
- Köhler 2010's numbers, and the AR5 ch. 8 land-use albedo map (fetch failed).
- Hansen 2005's efficacy values.
- The AR6 quote, taken from a search excerpt of the final government distribution draft; ipcc.ch returned 403.
- The DOIs of Ghimire 2014, Betts 2001 and He et al. 2014 are from memory.
- How much of the cell area is sea.
- Thompson 2022's numbers were read through a summarising fetch of the PMC page, with verbatim sentences returned.
- One fetch automatically saved a PDF of Bright & Kvalevåg 2013 to the session's tool-results folder. I did not open it.

---

Files read (14): the prompt; scripts/8_radiative.R, 9_forcing_barplot.R, 7a_alb_diff_full.R; R/forcing_periods.R, R/differences.R; config.R; questions_for_andria_scientific.md; the regression contract; forcing_by_period.csv, modern_ipcc_ar6_erf.csv, forcing_by_period_variant_sensitivity.csv, forcing_by_month_latitude.csv (read in part). 24 web searches and 19 web fetches.
## Reconciliation reply


Besides my first report, I read only Astra's `result.md` and `citation_check.md`. I made no new web searches and edited nothing.

### (a) Which number leads, and whether the IPCC bars share its axis

- **AGREE** that both quantities should be prominent. They are the domain mean (strength over the study area) and the contribution to global-mean forcing. Both are in `forcing_by_period.csv`. I have no objection to giving them equal weight.
- **AGREE** that the domain mean must never share an axis with the global-mean IPCC bars. We both hold this.
- **UNRESOLVED:** should the paleo contribution to global-mean forcing share an axis with the IPCC bars, or should the IPCC bars sit in a separate "context" panel?
  - **For a shared axis:** that is how the precedents present it.
    - Betts 2001 compares global-mean land-use albedo forcing with the ozone, N₂O and halocarbon forcings (search snippet I saw).
    - Pongratz et al. 2009 and Köhler et al. 2010 do the same kind of comparison.
    - AR6's land-use albedo value comes from the same kind of calculation as ours (see b).
  - **For a separate panel:** Astra's caution about the forcing definition (b) and the different intervals (Astra's finding 3).
  - No numerical test settles this; it is a presentation choice. **Question for Chris or Andria:** should the Holocene contributions and the modern forcings be read on one scale, or shown as separate context? Meanwhile the code puts them on one shared axis, `forcing_barplot_slide18`.

### (b) Kernel albedo forcing is not ERF

- **AGREE in substance.** The kernel gives an instantaneous top-of-atmosphere albedo forcing, without adjustments. "Equivalent" in "global equivalent" refers only to the area normalisation.
  - I propose renaming it "contribution to global-mean forcing" (as Astra also writes).
  - I propose labelling the bars "albedo radiative forcing (kernel)".
  - My first report covered only the related caveat on efficacy (Davin et al. 2007), not the definition. This is a gap Astra found.
- **DISAGREE** that this blocks the comparison with the land-use bar.
  - Evidence: AR6 takes its land-use albedo ERF of −0.15 directly from Ghimire et al. 2014, a satellite-albedo-plus-kernel calculation like ours ("that study is used as the assessed albedo ERF", AR6 text via search excerpt; the citation check confirms the −0.15 + −0.05 split).
  - So for the most like-for-like comparator, AR6 itself treats kernel RF as the ERF. For CO₂, CH₄ and aerosols the mismatch is a caveat to state, not a blocker.
  - What would settle it: a sentence in the methods quoting AR6's choice, and checking every plotted value's forcing definition (Astra's proposed check, which I support).
- **AGREE, correction to my report:** the AR6 section is 7.3.4.1 (per the citation check), not 7.3.4.3.

### (c) Findings Astra could not raise (no repository access)

**Finding 2: vegetation and ice variants do not add up to the total.** I maintain it. The main session has confirmed the CSV numbers.
- veg −0.0364 + ice +0.0385 = 0.0021 at 8–10 ka, against a total of 0.1848.
- `ice_thresh`'s domain mean is averaged over about 1.69e12 m² while `area_m2` reports 1.92e13.
- Mechanism: 9_forcing_barplot.R:151 sums with `na.rm=FALSE`; R/differences.R:137–138 gives each pair either a vegetation or an ice value, the other NA.
- Test: per period, valid area per variant, and assert vegetation-only + ice-only + mixed cells = total.
- This bears directly on Astra's question about whether to call it vegetation or all surface albedo: **AGREE** that question must stay open, because the current outputs cannot separate the two.
- **Correction to my report:** I wrote that the early-Holocene bars are "mostly Laurentide retreat". That is an inference from where (50–60 N, summer) and when the forcing occurs, not a measurement. The mixed-cell decomposition above would measure it.

**Finding 4: coastal cells count their sea area.** I maintain it, size unknown.
- Mechanism: 7a_alb_diff_full.R:95–98 uses the area of the whole 1° polygon.
- Test: weight by land fraction (MOD44W) and compare the 8–10 ka values, with the cells round Hudson Bay listed separately.

### (d) How sure am I of the Thompson numbers and the regional ERF claim

**Thompson et al. 2022.**
- What I saw: two WebFetch passes over PMC9012463 (an intermediary model summarises the page). Both returned the same sentence as verbatim: "The SW radiative forcing associated with changes in vegetation albedo is +2.41 W/m2 (table S2), of which +1.91 W/m2 comes from greening of the African Sahara".
- What I did not see:
  - Table S2.
  - Whether these are global means. The fetch model said so; my back-of-envelope check finds +1.91 from the Sahara plausible but at the high end.
  - Whether the number includes snow albedo changes the model induced.
- How sure: moderately sure the sentence exists; less sure of what it measures.
- The "+0.5 for the rest of the Northern Hemisphere" is my own subtraction. The "North America is about a tenth of it" compares a CESM APRP diagnosis with our kernel RF of a reconstruction.
- **AGREE with Astra** that such model diagnoses are not interchangeable with ours. Downgrade both to unchecked context, not for the paper until table S2 is read.

**North America's regional aerosol and land-use ERF "larger than the global mean".** I withdraw this. I saw no regional values.
- The one regional statement I did see (Smith et al. 2020) says the large negative cloud-driven aerosol ERF is over southern Asia and the eastern Pacific, not North America.
- Our domain includes Canada and Alaska, with little aerosol or land use, and North American aerosol emissions had fallen by 2019. The domain mean could be smaller in magnitude than the global one.
- I therefore also withdraw my inference that option B would make the Holocene look less dominant.
- Test: average piClim-aer and piClim-lu minus piClim-control over the same cells, area weights and months. Note Smith's 1850–2014 period against AR6's 1750–2019.

### Other points Astra raised

- **Not a lower bound — AGREE.** North America's contribution is not a lower bound on a global Holocene value. Other regions can add to it or offset it, and the sign over intervals other than 6 ka is unknown. My word "incomplete" could have been read as a lower bound.
- **The hand-check discrepancy (0.18443) — resolved.** At full precision, 4.90286736 × (19,223,828,407,176.9 / 5.101e14) = 0.1847714, equal to the CSV. It was rounding.
- **Option B, under a uniform-forcing assumption — AGREE.** Under that assumption the regional CO₂ value is 2.16, not 2.16 × 0.0377. This is the same as my point.
- **CH₄ double-counting — AGREE.** CH₄ perturbation experiments can include stratospheric water vapour, which the figure shows as a separate bar. Not checked by me.
- **Option B's value — UNRESOLVED.** Astra calls it the stronger extension if a regional quantitative claim is central; I judged its benefit doubtful. Both of us agree it cannot be computed from the current CSVs. **Question for Chris or Andria:** must the paper make a quantitative regional comparison with modern forcings, or is modern forcing context only? Meanwhile no regional comparators exist in the code.
- **Interval mismatch — AGREE.** The bars are changes over different intervals, compared with 1750–2019. This matches my question on increments versus a cumulative change since 50 BP, which is a one-line cumulative sum of the existing values.
- **Telescoping test, including an ice advance — AGREE.** With `endpoint` and complete pairs within each period (A10), telescoping should hold. I have not read `tests/test_differences.R` to see whether it already asserts this.
- **Option A — AGREE.** Neither of us found a published multiplier; KK10 and HYDE cover anthropogenic land use only.
- **Citation problems in Astra's references — noted, not re-checked by me.** These are from the citation check:
  - The Betts 2001 DOI should be 10.1006/asle.2001.0037.
  - Betts et al. 2007's −2 (local) and −0.2 (global) W/m² use different baselines, so it is a weaker precedent for side-by-side reporting.
  - Pincus 2016 does not contain the flux formula or piClim-aer attributed to it.

### Agreed but checked by only one of us

- Earth-area normalisation does not understate the global contribution: both of us did the arithmetic.
- The −0.20 bar includes −0.05 irrigation: confirmed by the citation check.
- The domain mean is defensible when labelled as such: both of us said so, but it remains a matter of judgement.
- Pongratz 2009 and Köhler 2010 (mine), and Bright & Lund 2021 and Bright et al. 2020 (Astra's): each checked only by the reviewer who cited it; Köhler only at abstract level.

Files read this round: 2 (Astra's result.md and citation_check.md). Tool calls this round: 2 reads, no searches.