# Forcing normalisation (global equivalent against regional mean): review by Astra (Codex gpt-6-astra, maximum effort, with reconciliation)

_Asked 18 October 2026, at Chris's request, after his objection that dividing North America's forcing by Earth's area dilutes the effect and is not a valid comparison with the IPCC agents; he proposed scaling the forcing up to global or the IPCC values down to the region. Code reviewed: `feature/single-config` at 4ba4d2a, with the full run of that commit (18 October). The citations were then checked separately (`2026-10-18_forcing_normalisation_citation_check.md`); the other independent review is `2026-10-18_forcing_normalisation_review_opus.md`._

**Verdict.** Chris is right that the figure can conceal regional strength; he is wrong that Earth-area normalisation itself underestimates North America’s contribution to global-mean forcing.

**Benefit.** Presenting regional strength alongside global contribution would make the result interpretable without inventing changes outside the reconstructed domain.

**Scope.** This is a literature-grounded method review. Your prohibition on commands prevented repository inspection: the available filesystem reader requires shell commands, and permission to use those remains unanswered. Consequently, the arithmetic below uses the packet, not an independently inspected CSV; code locations are packet pointers.

**Hand check.**

For regional area \(A_R\), Earth area \(A_E\), cell areas \(A_i\), and forcing \(F_i\):

\[
\overline F_R=\frac{\sum_{i\in R}A_iF_i}{A_R},
\qquad
F_{R\rightarrow G}=\frac{\sum_{i\in R}A_iF_i}{A_E}
=\overline F_R\frac{A_R}{A_E}.
\]

Using the packet’s rounded 8–10 ka result and area fraction:

\[
4.90\times0.0377=0.18473\approx0.185\ {\rm W\,m^{-2}}.
\]

Using its more coarsely rounded areas gives \(4.90(1.92\times10^{13})/(5.101\times10^{14})=0.18443\). That small discrepancy requires the full-precision CSV and area to resolve; it is not evidence of a scientific error.

A two-cell check makes the distinction explicit. Let areas be 3 and 1 units, and the kernel be \(-100\ {\rm W\,m^{-2}}\) per unit albedo. Changes \(0.30\rightarrow0.20\) and \(0.20\rightarrow0.30\) give forcings \(+10\) and \(-10\). Their area-weighted domain mean is

\[
(3\times10-10)/4=5.
\]

If Earth has 100 area units, their global contribution is \(20/100=0.20\). Both numbers correctly describe the same perturbation. The smaller number does not imply weaker forcing within either cell. This agrees with the packet’s formula; agreement with the implementation remains unchecked.

**Findings, worst first.**

1. **Blocker to a claim of strict equivalence — `scripts/9_forcing_barplot.R`, panel after line 175.** Matching the area denominator does not make kernel-derived albedo forcing equivalent to ERF. An all-sky kernel incorporates the reference atmosphere’s cloud effects on radiation; it does not calculate the clouds’ adjustment to the reconstructed land change. Calling the bars equivalent climate impacts would therefore overinterpret them. Bright and Lund explicitly distinguish instantaneous albedo RF, ERF and climate efficacy. Verification requires identifying the forcing definition of each plotted quantity; passing numerical anchors cannot supply the missing adjustments. [Bright & Lund, 2021, *Atmospheric Chemistry and Physics*, DOI: 10.5194/acp-21-9887-2021](https://acp.copernicus.org/articles/21/9887/2021/).

2. **Blocker to option A as a correction — proposed method; no implementation reviewed.** Dividing by 0.0377 changes the question from North America’s global contribution to a hypothetical worldwide perturbation with North America’s mean intensity. No evidence in this packet establishes that hypothesis. A defensible global estimate needs information about other regions, not a different denominator. The published reconstruction literature documents substantial regional disagreement even between global land-use scenarios. Verification would require independent spatial evidence and a consistent forcing calculation. [Harrison et al., 2020, *Geoscientific Model Development*, DOI: 10.5194/gmd-13-805-2020](https://gmd.copernicus.org/articles/13/805/2020/).

3. **Should-fix — the headline comparison’s scope and labels, same plotting location.** A global contribution alone inadequately communicates regional intensity. Conversely, the seven bars represent endpoint changes over different intervals, not time-integrated energy, rates, or seven estimates against one common baseline. Their comparison with 1750–2019 forcing can provide magnitude context, but cannot establish which process dominated Holocene climate. Verification should trace each bar to its actual older and younger endpoints.

4. **Should-fix — the −0.20 comparator’s description, same plotting location.** AR6’s assessed land-use ERF includes an irrigation contribution; it is broader than pure surface-albedo forcing. Section 7.3.4.1 combines an albedo estimate near −0.15 with irrigation near −0.05 W/m². Calling −0.20 simply “land-use albedo” hides this distinction. Check the plotted label and explanatory text against the assessed category. [Forster et al., 2021, *IPCC AR6 WGI*, Chapter 7, DOI: 10.1017/9781009157896.009](https://www.ipcc.ch/report/ar6/wg1/chapter/chapter-7/).

**1. Is the global contribution a valid comparison?**

**Yes, as a comparison of contributions to a common global radiative budget, with the forcing-definition and reference-period qualifications above.** It is not a complete comparison of climate consequences.

| Question | Appropriate quantity | What the packet establishes |
|---|---|---|
| How much does this domain’s reconstructed change contribute to global-mean direct forcing? | \(F_{R\rightarrow G}\) | About +0.185 W/m² for 10→8 ka |
| How strong is the forcing over the study domain? | \(\overline F_R\) | About +4.90 W/m² |
| What was the total worldwide Holocene albedo forcing? | Contributions from all relevant regions | Undetermined |

For the first question, the normalisation does not underestimate the contribution merely because the region is small. Regional forestry studies explicitly normalise albedo forcing to Earth’s area before comparison with CO₂. [Bright et al., 2020, *Global Change Biology*, DOI: 10.1111/gcb.15228](https://onlinelibrary.wiley.com/doi/10.1111/gcb.15228).

For the second, using 0.185 as though it were the North American mean would understate the reported intensity by approximately 26.5-fold. The domain mean is a defensible number in its own right. The old slide’s **clear-sky regional mean** is likewise meaningful when labelled accurately, although it cannot stand for an all-sky regional mean.

For the third, the North American contribution is **not a guaranteed lower bound**. Contributions elsewhere can reinforce or offset it. Furthermore, the domain integral captures the calculated direct radiative effect; it does not capture remote atmospheric adjustments caused by that change.

A regional mean also averages strong and weak effects within the domain. Neither mean alone establishes regional temperature response.

**2. Option A: estimate the global Holocene effect**

I found **no defensible published scalar that converts these North American results into the total global Holocene albedo forcing**.

Multiplying the contribution by \(1/0.0377\) would reproduce 4.90 W/m² as a hypothetical global mean. Applied literally over Earth’s area, that assumes the same mean forcing over oceans too. Restricting an extrapolation to land would remove that particular problem but would still assume comparable changes, snow masking, radiation and timing across continents.

There are published routes to global analysis, but they require additional evidence:

- **Global land-use reconstructions.** Kaplan’s KK10 and HYDE provide spatially explicit anthropogenic land-use histories. They are inputs to an analysis, not complete reconstructions of natural vegetation, ice-sheet retreat and their albedo forcing. KK10’s cited archive begins at 8 ka, so it also does not cover the earliest packet intervals. [Kaplan et al., 2011, *The Holocene*, DOI: 10.1177/0959683610386983](https://journals.sagepub.com/doi/10.1177/0959683610386983); [Klein Goldewijk et al., 2017, *Earth System Science Data*, DOI: 10.5194/essd-9-927-2017](https://essd.copernicus.org/articles/9/927/2017/).
- **Controlled paleo model experiments.** These can distinguish vegetation effects from orbital and ocean effects. Otto et al. used factor separation for this purpose; an undifferentiated mid-Holocene-minus-preindustrial climate difference would not isolate albedo forcing. [Otto et al., 2009, *Geophysical Research Letters*, DOI: 10.1029/2009GL037482](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2009GL037482).
- **Published global Holocene land-use simulations as external context.** He et al. estimated global and regional temperature responses, including −0.17°C globally from biogeophysical effects. That result includes more than albedo and is a temperature response, so it cannot be inserted into this forcing plot. [He et al., 2014, *Geophysical Research Letters*, DOI: 10.1002/2013GL058085](https://hub.hku.hk/handle/10722/268656).

How wrong could uniform extrapolation be? **These results cannot establish a numerical bound.** Cancellation can even change the global sign. There is substantial physical sensitivity: changing snow-masking parameterisation produced a threefold difference in simulated high-latitude spring warming in Otto et al. This is evidence against assuming transportability, not a factor to apply to paleo-RF. [Otto, Raddatz & Claussen, 2011, *Climate of the Past*, DOI: 10.5194/cp-7-1027-2011](https://cp.copernicus.org/articles/7/1027/2011/).

The Sahara is another reason to require region-specific histories: its modeled mid-Holocene transition involves vegetation–atmosphere interactions and timing distinct from a North American trajectory. [Claussen et al., 1999, *Geophysical Research Letters*, DOI: 10.1029/1999GL900494](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/1999GL900494).

I did not identify a ready-made global estimate matching all seven intervals and this pipeline’s precise forcing definition.

**3. Option B: compare forcing over the same North American domain**

**This is defensible and more directly answers Chris’s regional-strength question. It requires spatial forcing fields.**

A global mean cannot generally be converted to a regional mean using area alone. Under an explicitly uniform CO₂-forcing assumption, a global mean of 2.16 would also be a regional mean of 2.16—not \(2.16\times0.0377=0.0814\). The latter is that region’s contribution to the global mean under the uniform assumption.

Even well-mixed gases have spatially varying forcing. Temperature profiles, water vapour and clouds matter; uniform concentration does not imply uniform radiative forcing. [Huang, Tan & Xia, 2016, *Journal of Geophysical Research: Atmospheres*, DOI: 10.1002/2015JD024569](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2015JD024569).

Published sources and methods exist:

| Component | Defensible source or method | Essential qualification |
|---|---|---|
| CO₂ | RFMIP fields analysed by Smith et al.; published CO₂ radiative-transfer calculations | RFMIP’s quadrupled-CO₂ experiment needs an explicitly justified concentration scaling |
| Aerosols | `piClim-aer` minus matching `piClim-control` | Use total all-sky ERF, including cloud effects |
| Land use | `piClim-lu` minus control; or an isolated albedo-kernel component | Total land-use ERF and albedo-only RF answer different questions |
| CH₄ | AerChemMIP methane perturbation experiments | Interactive chemistry can include effects on ozone and other species |

Smith et al. provide spatial forcing/adjustment results and identify the underlying ESGF model archive. Their principal historical comparison is **1850–2014**, not AR6’s 1750–2019. They also distinguish fixed-SST ERF from estimates corrected for land-temperature change. [Smith et al., 2020, *Atmospheric Chemistry and Physics*, DOI: 10.5194/acp-20-9591-2020](https://acp.copernicus.org/articles/20/9591/2020/).

The standard starting diagnostic is the perturbed-minus-control difference in net downward TOA flux:

\[
\Delta N=\Delta(rsdt-rsut-rlut),
\]

using the matched atmosphere-only experiments with prescribed SST and sea ice, followed by the selected adjustment treatment. A historical coupled-model flux difference also contains climate responses. [Pincus, Forster & Stevens, 2016, *Geoscientific Model Development*, DOI: 10.5194/gmd-9-3447-2016](https://gmd.copernicus.org/articles/9/3447/2016/).

For CH₄, category matching matters particularly if stratospheric water vapour remains a separate bar: methane perturbation experiments can include chemically mediated effects, so adding components without checking attribution risks double counting. [Thornhill et al., 2021, *Atmospheric Chemistry and Physics*, DOI: 10.5194/acp-21-853-2021](https://acp.copernicus.org/articles/21/853/2021/).

Ghan’s method is relevant when decomposing aerosol effects; an ordinary change in cloud radiative effect is a biased estimate of aerosol–cloud forcing. It is not necessary to invent a decomposition merely to average an existing total-ERF field. [Ghan, 2013, *Atmospheric Chemistry and Physics*, DOI: 10.5194/acp-13-9971-2013](https://acp.copernicus.org/articles/13/9971/2013/).

For each field, average over the **same actual cells, area fractions and months** as paleo-RF. This resolves the spatial averaging mismatch. Differences in forcing definition, baseline and climate state still need explicit treatment. It measures forcing **over** North America, not forcing attributable exclusively to North American emissions.

**4. What does the literature actually do?**

There is clear precedent for global normalisation and for displaying regional strength alongside global effects:

| Precedent | What it supports |
|---|---|
| **Betts (2000), Nature**, DOI: [10.1038/35041545](https://www.nature.com/articles/35041545) | Converts regional forestation albedo effects into carbon-stock equivalents through a common global forcing comparison |
| **Betts (2001), Atmospheric Science Letters**, DOI: [10.1006/asle.2001.0023](https://www.sciencedirect.com/science/article/pii/S1530261X01900234) | Compares global-mean albedo forcing with other global forcings while discussing substantial regional climate effects separately |
| **Betts, Falloon, Klein Goldewijk & Ramankutty (2007), Agricultural and Forest Meteorology**, DOI: [10.1016/j.agrformet.2006.08.021](https://www.sciencedirect.com/science/article/pii/S0168192306002887) | Reports local forcing around −2 W/m² in some regions and global forcing around −0.2 W/m²; explicitly addresses baseline matching for comparisons with GHGs |
| **Ghimire et al. (2014), Geophysical Research Letters**, DOI: [10.1002/2014GL061671](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2014GL061671) | Uses spatially explicit satellite albedo, historical cover and radiative kernels; presents global forcing histories alongside seasonal and annual spatial patterns |
| **He et al. (2014), cited above** | Demonstrates that biogeophysical effects can be comparable with biogeochemical effects regionally while being much smaller globally |

These precedents support the **two questions**, not an assertion that every paper uses precisely the proposed two-bar layout.

Published paleo model studies frequently compare controlled climate responses or separated feedback contributions. Those responses should not be treated as interchangeable with an instantaneous kernel diagnostic. Agreement between such studies and paleo-RF would require checking the quantities first.

**5. Recommendation for the paper**

Make **two quantities jointly prominent**, using what the pipeline already produces:

1. **North American domain-mean albedo radiative effect.** This answers how strong the reconstructed perturbation is over the study area. Retain the monthly/latitude presentation to expose concentration in particular seasons and places.
2. **North American contribution to global-mean albedo radiative effect.** This answers its contribution to the global radiative budget. Use the existing global-contribution column, with its area denominator stated.

The paired presentation follows the local/global distinction in Betts et al. (2007) and the spatial/global reporting in Ghimire et al. (2014). Choosing this exact layout is my presentation recommendation, not a new scientific method.

Keep the AR6 bars in a **separate, clearly labelled context panel**: “Modern global ERF, 1750–2019.” State that the paleo bars are interval-specific albedo-kernel estimates. They can convey magnitude without implying matched experiments or equivalent temperature effects.

If a quantitative regional comparison is central to the paper’s claim, option B is the stronger extension. It follows published RFMIP/AerChemMIP practice, but cannot be computed from the three current CSVs alone.

I would not introduce a North-America-to-world multiplier. The existing two summaries already answer useful questions without that unsupported assumption.

**What the proposed verification would miss.**

A verification specification should check:

- **Exact normalisation:** full-precision global contribution equals regional mean times the corresponding area ratio.
- **Missingness and denominators:** distinguish zero change from unavailable data; report coverage; never silently make an entirely missing group zero.
- **Sign and units:** the two-cell case above, including equivalent kernels expressed per unit albedo and per 0.01 albedo.
- **Telescoping:** consecutive differences equal endpoint differences when the kernel and support are fixed. Exercise ice advance explicitly; masking can invalidate an unconditional telescoping claim.
- **Regional comparators:** a uniform field retains its value after regional averaging; integration over the globe reproduces the source calculation; spatial remapping preserves the area integral.
- **Definitions and endpoints:** every displayed value retains its sky condition, forcing definition, older/younger endpoints and averaging domain.

Area conversion and plotting should need no additional random draws. I have not checked seeds or draw counts. A seed-1 anchor or tolerance comparison would establish numerical preservation, not validate global extrapolation or supply missing ERF physics.

**Questions for Chris or Andria.**

- **Which claim should lead: regional strength, global contribution, or both?** I recommend both. Meanwhile, the packet says both are calculated and the global contribution is plotted.
- **Is modern forcing contextual, or must it be a quantitatively matched regional comparison?** The latter needs additional spatial data and a choice of forcing definition. Meanwhile, the comparator remains global ERF.
- **Should the result describe all reconstructed surface-albedo change, or specifically vegetation’s contribution?** The treatment and contribution of ice transitions need inspection before choosing that wording. Meanwhile, retain the current calculation without asserting a vegetation-only attribution.
- **Does Chris want option A investigated as a separate global reconstruction, or option B developed?** “Either … or conversely …” expresses alternatives, not an explicit decision. Neither extension should be recorded as selected.

**Not checked.**

Repository code, the exact CSV rows, regression contract, scientific questions file, kernel variables/levels, masks, NA behaviour, seed configuration and existing verification specifications were not inspected. No R or shell commands were executed. No global or regional model fields were downloaded or recomputed. Publication identities and supporting passages were checked through publisher/institutional HTML; repository reference-checker utilities were not run. No other reviewer’s findings were read, and no files were changed.

Files read: none locally; searches made: regional/global albedo normalisation, Betts/Bright/Ghimire/Ward precedents, Holocene vegetation and land-use reconstructions, PMIP feedback separation, AR6 definitions, RFMIP/AerChemMIP fields, CO₂ forcing patterns and Ghan aerosol diagnostics; tools used: tool/resource discovery, web search/open/find, clarification request and sleep.
## Reconciliation reply

**Verdict.** Retain Earth-area normalisation; reject additive interpretation of the variants and unsupported regional-comparator claims; headline priority and coastal treatment remain unresolved.

**Benefit.** This reconciliation separates regional intensity, global contribution and attribution so the paper does not draw conclusions that the calculated quantities cannot support.

Repository locations below are **packet pointers**, not independently inspected code. I rechecked the supplied arithmetic and accessible primary-source passages.

**Hand check.**

The supplied normalisation is consistent:

\[
4.90286736\times0.0376864
=0.1847714205\ {\rm W\,m^{-2}}.
\]

That matches the quoted CSV to its displayed precision. It is not an independent CSV check.

The confirmed variant values give:

\[
-0.0364+0.0385=0.0021,
\qquad
0.1848-0.0021=0.1827.
\]

The residual is **98.9% of the combined net value**. This establishes non-additivity; it does not establish that 98.9% is a causally isolated ice effect.

A minimal illustration of the reported aggregation mechanism uses two equal-area cells, three instants and one month. With \(K=-100\ {\rm W\,m^{-2}}\) per unit albedo:

| Cell | Albedo sequence | Consecutive forcing changes |
|---|---|---|
| A | Ice 0.60 → vegetation 0.30 → vegetation 0.20 | +30, +10 |
| B | Vegetation 0.20 → 0.22 → 0.25 | −2, −3 |

The combined endpoint change is \(40-5=35\), giving a domain mean of \(17.5\). If Earth has 100 area units, the global contribution is \(0.35\).

If A’s successive steps are assigned to ice and vegetation respectively, its variant arrays are `(30, NA)` and `(NA, 10)`. Summing each with `na.rm=FALSE` produces two missing period totals, although the combined total is +40. **Excluding that cell loses both steps.** This demonstrates the packet’s proposed mechanism; the actual classification and downstream missing-value handling remain uninspected.

**Findings.**

1. **AGREE — blocker to additive or vegetation-only interpretation; should-fix for sensitivity labels.**  
   **Location:** variant aggregation and CSV summaries; exact aggregation file/line not supplied.

   The confirmed numbers prove that `veg_thresh` and `ice_thresh` are not components that reconstruct `veg_ice_thresh`. They may remain valid conditional sensitivity summaries, provided their meaning and coverage are explicit.

   I agree with the reported NA mechanism as an explanation consistent with the hand check. I have not independently established that it explains the entire residual. In particular, dropping a mixed cell’s whole period can discard vegetation changes *after* retreat as well as the transition itself.

   The reported `ice_thresh` mean also needs its actual denominator. Under matching temporal weights,

   \[
   A_{\mathrm{effective}}
   \approx\frac{0.0385(5.101\times10^{14})}{11.61}
   =1.69\times10^{12}\ {\rm m^2}.
   \]

   This is consistent with the reported 8.8% coverage, not the full domain. An `area_m2` field may legitimately describe the domain footprint, but must not appear to be the denominator of that conditional mean. Where coverage varies monthly, report monthly support rather than implying one geometric annual denominator.

   **Settling check:** trace every cell-month-step into the combined and variant totals, including mixed periods, genuinely missing observations and ice advance. Do not repair this merely by replacing every NA with zero.

2. **AGREE that coastal support needs checking; UNRESOLVED whether whole-cell weighting is erroneous.**  
   **Location:** `scripts/7a_alb_diff_full.R:95–98`, as reported.

   Whole-cell area is appropriate when the forcing already represents the whole cell. If the predicted forcing represents only land, with unchanged ocean and fixed land fraction \(f_i\), its contribution is instead

   \[
   A_i f_i F_{i,\mathrm{land}}.
   \]

   For two unit-area cells with land fractions 0.25 and 1, and land forcings +10 and 0, the correct land integral is 2.5; applying +10 to the entire coastal cell gives 10. Conversely, multiplying an already mixed-cell forcing by 0.25 would apply the fraction twice.

   This could affect the paper’s numbers. Neither its magnitude nor its net sign is established: coastal cells can contain warming and cooling changes.

   **Settling check:** trace whether ocean area has already entered cover fractions, albedo aggregation or masking; then inspect representative coastal cells. Historical grounded ice over present-day sea also requires an explicit spatial definition. A modern land mask is not automatically the correct repair.

3. **AGREE with Thompson’s quoted numbers; DISAGREE with interpreting the ratio as North America’s share.**  
   **Location:** Opus answer 1c and proposed literature context.

   Thompson et al.’s “Mechanism for warming” passage explicitly reports +2.41 W/m² and +1.91 W/m² from Sahara greening. Its methods diagnose TOA shortwave components using APRP on vegetation sensitivity experiments relative to `6ka_PI_VEG`. These are confirmed primary-text statements. [Thompson et al., 2022, DOI: 10.1126/sciadv.abj6535](https://pmc.ncbi.nlm.nih.gov/articles/PMC9012463/).

   The arithmetic \(2.41-1.91=0.50\), followed by \(0.057/0.50=0.114\), is correct. But it does not establish a spatially isolated “non-Sahara remainder” comparable to paleo-RF. Experimental contrasts, feedback-related albedo changes, spatial support and baselines need matching first.

   There is also a **direction-label inconsistency**: the packet reports **50 BP minus 6 ka = −0.057**. Its reverse is **6 ka minus 50 BP = +0.057**. Calling the latter “6 ka→50 BP” reverses the stated chronological change.

   Additional relevant evidence: Dawson et al. explicitly argue that Thompson’s prescribed North American forest expansion exceeds their reconstructed changes. That makes Thompson useful sensitivity context, but a poor calibration target for a North American fraction. [Dawson et al., 2025, §4.2.2, DOI: 10.5194/cp-21-2031-2025](https://cp.copernicus.org/articles/21/2031/2025/index.html).

   **Settling check:** identify the exact experiments and spatial integrals behind Table S2, separate prescribed land changes from responding snow/ice where necessary, and align the paleo contrast. Until then, quote the studies separately.

4. **DISAGREE that stronger North American aerosol and land-use ERF has been established; UNRESOLVED numerically.**  
   **Location:** Opus answer 3.

   Spatial concentration does not prove that an average over this particular North American domain exceeds the global mean in magnitude. Smith et al. report global land-use ERF of −0.09 ± 0.13 W/m² and caution that many regional forcing and adjustment signals are not robust. They do not establish the requested domain ranking. [Smith et al., 2020, §§5.4–5.5, DOI: 10.5194/acp-20-9591-2020](https://acp.copernicus.org/articles/20/9591/2020/).

   Consequently, “a regional comparison makes the Holocene look relatively less dominant” is an untested prediction.

   **Settling check:** calculate matched regional and global means from each model’s fields using the actual paleo-RF support, temporal weights and forcing definition; report model spread and sampling uncertainty.

   I also retain my objection to treating well-mixed CO₂ or CH₄ as spatially uniform forcing. Huang’s published field and dataset support examining that assumption; a doubled-CO₂ instantaneous-forcing ratio is not automatically a conversion factor for historical CO₂ ERF, still less methane. [Huang et al., 2016](https://agupubs.onlinelibrary.wiley.com/doi/10.1002/2015JD024569), [associated dataset](https://data.mendeley.com/datasets/khmt3myywn/1).

5. **AGREE on the land-use label; DISAGREE with unqualified “like-for-like”.**  
   **Location:** `scripts/9_forcing_barplot.R:188–189` and the comparison panel.

   The −0.20 W/m² assessment includes irrigation. Use −0.15 for the assessed albedo component, or retain −0.20 with a label identifying land use including irrigation. AR6 explicitly adopts Ghimire’s estimate as assessed albedo ERF because it agrees with modelling evidence. That strengthens the case for contextual comparison. [IPCC AR6 WGI, §7.3.4.1](https://www.ipcc.ch/report/ar6/wg1/chapter/chapter-7/).

   It does not establish that paleo-RF calculates ERF. All-sky kernels account for clouds in the reference atmosphere; they do not calculate cloud adjustment to the reconstructed perturbation. My earlier blocker applies to **claims of physical equivalence**, not to a clearly qualified magnitude comparison. [Bright and Lund, 2021, §2.2](https://acp.copernicus.org/articles/21/9887/2021/).

   **AGREE** with the additional efficacy caveat: Davin et al. found less cooling from land-cover change than from radiatively equivalent CO₂ changes. Their efficacy is not a transferable correction for this reconstruction. [Davin et al., 2007, §4](https://doi.org/10.1029/2007GL029678).

6. **AGREE that the records conflict; UNRESOLVED whether C5 was explicitly decided.**  
   **Location:** `config.R:130–131`; `scripts/9_forcing_barplot.R:91–95, 290`; scientific questions file, C5.

   The packet contrasts “decision C5, 2026-10-14” with a question still awaiting Andria’s confirmation. That is evidence of inconsistent records, not proof that a decision occurred or was reopened.

   **Settling check:** locate the explicit dated choice. Without it, retain C5 as a question.

   “Overstates” is justified if a regional value is presented as a global contribution. It is inappropriate for a correctly labelled regional mean. “NOT comparable” is too categorical when a qualified contextual comparison is intended.

7. **AGREE on modern-kernel limitations; DISAGREE with the proposed “few percent understated” conclusion.**  
   **Location:** Opus finding 5.

   Higher summer insolation alone does not determine the correction to an annual, spatially aggregated bar. The result depends on when and where albedo changes occur, their signs, and the atmospheric state. State that the diagnostic uses modern radiative conditions; do not attach a signed percentage without calculating it.

   **Settling check:** evaluate the relevant monthly kernel–albedo products under justified paleo conditions. An insolation multiplier is not established by either review.

8. **UNRESOLVED — headline priority.**  
   **Location:** figure design and C5.

   I retain **joint prominence** for regional intensity and global contribution. Opus’s global-first presentation is defensible if the paper’s leading claim concerns the global radiative budget. Neither arithmetic nor published convention chooses that claim for Andria.

   I have now verified Ghimire’s global forcing histories in Figure 2 and seasonal/annual spatial patterns in Figure 3. This supplies precedent for showing both scales; it does not establish that one layout is mandatory or that “both numbers” is universally the norm. [Ghimire et al., 2014](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2014GL061671).

9. **AGREE on normalisation and rejection of unsupported extrapolation; qualify boundary independence.**  
   **Location:** both reviews’ answers on global contribution.

   Earth-area division correctly expresses the calculated domain integral as a global-mean contribution. It does not underestimate that contribution merely because the domain is small. Neither multiplying by 26.5 nor extrapolating across global land supplies missing regional histories.

   However, the **value** is not independent of domain boundaries. Adding zero-forcing cells leaves the global contribution unchanged; adding nonzero-forcing cells changes it. The validity of the denominator is boundary-independent, not the integral.

   I also retain the point absent from Opus’s summary: a positive North American contribution is not necessarily a lower bound on the worldwide total, because other regions can offset it.

10. **AGREE that cumulative contrasts may help; UNRESOLVED on their implementation and interpretation.**  
    **Location:** proposed cumulative presentation and reported +0.329/−0.057 totals.

    The totals remain packet evidence. They equal endpoint contrasts only if the necessary kernel, support and missingness conditions hold. Mixed-period exclusions make that especially important here.

    These are forcing differences, not accumulated energy or rates. The reported summer band values must also retain their seasonal labels when compared with annual quantities.

    **UNRESOLVED:** vegetation and ice responses may be feedbacks within the broader Holocene causal system while still having diagnosable radiative effects. Calling the diagnostic “forcing” must not imply an independently imposed external driver or justify adding it twice to a causal budget.

11. **Citation reconciliation — corrections and verification limits.**

    | Item | Reconciliation |
    |---|---|
    | **Betts 2001 DOI** | **AGREE:** replace my DOI with **10.1006/asle.2001.0037**. Smith’s reference list corroborates it. The archived ScienceDirect entry still prints the conflicting `0023`; direct Wiley/registry access failed, so I cannot claim a fresh resolver-level check. |
    | **Betts 2007 numbers** | **AGREE:** my juxtaposition obscured different endpoints: approximately −2 locally by 1750 versus approximately −0.2 globally at present, both relative to natural vegetation. They are not a matched regional/global pair. |
    | **Betts 2007 baseline claim** | **DISAGREE that it remains unsupported:** the accessible publisher introduction explicitly discusses the need to compare against vegetation at the industrial-era baseline and diagnoses 1990 relative to 1750. [Betts et al., 2007](https://www.sciencedirect.com/science/article/pii/S0168192306002887). |
    | **Bright 2020** | **AGREE with the verification gap:** my prior statement overstated direct confirmation. Use the explicit Earth-area expressions in [Bright and Lund 2021, Eqs. 6–7](https://acp.copernicus.org/articles/21/9887/2021/) for that mathematical precedent. |
    | **Ghimire 2014 presentation** | **AGREE that the earlier check was insufficient; now verified:** Figures 2–3 explicitly provide the global temporal and seasonal/spatial presentation described above. |
    | **Otto 2011** | **AGREE:** specify the forest-albedo-feedback spring-warming contrast. It is not a threefold forcing uncertainty or a correction factor for paleo-RF. [Otto et al., 2011](https://cp.copernicus.org/articles/7/1027/2011/). |
    | **Pincus 2016** | **AGREE:** do not attribute the literal CMIP variable expression to a passage not checked. **DISAGREE with assigning `piClim-aer` exclusively to AerChemMIP:** Smith §2 explicitly lists it among the RFMIP experiments. Use that source for the experiment names. |

**What the proposed verification misses.**

Anchors can preserve these problems. The verification specification should additionally require:

- The mixed-period hand case, including advance, retreat and genuinely missing observations.
- Explicit coverage and denominator checks for every variant and month; an entirely unavailable group must not silently become zero.
- Coastal tests distinguishing land-only values from already mixed-cell values.
- Zero forcing for zero albedo change, correct signs, and equivalence of kernels expressed per unit albedo and per 0.01 albedo.
- Telescoping against endpoints under fixed kernels and common support; explicit handling when those conditions fail.
- Regional-comparator tests: a uniform field retains its value, remapping preserves integrals, and the global calculation reproduces its source.
- Labels that retain sky condition, forcing definition, endpoints, season and averaging domain.

I retain the methane attribution check from my first review: chemically mediated effects must not be double-counted with separate ozone or stratospheric-water-vapour bars. Aerosol decomposition is unnecessary merely to average an existing total-ERF field.

These changes should require no new random draws. Seeds and streams were not inspected. Neither a seed-1 anchor nor tolerance agreement establishes scientific correctness.

**Questions for Chris or Andria.**

| Question and options | What the code reportedly does meanwhile |
|---|---|
| **C5:** global contribution first, or regional intensity and global contribution jointly prominent? | Calculates both; plots the global contribution. The decision record is inconsistent. |
| Keep vegetation/ice variants as conditional sensitivities, or require an explicitly defined additive attribution? | Variants are non-additive and omit mixed periods according to the packet. |
| Should coastal support represent reconstructed terrestrial area, or the complete grid footprint including specified ice/ocean transitions? | Uses whole polygon areas; prior fractional treatment is unchecked. |
| Use −0.15 albedo context, or −0.20 land use including irrigation? Is a matched regional comparison essential? | Plots −0.20 under an albedo label; no matched regional comparator is established. |
| Show interval changes, common-baseline contrasts, or both? | Shows interval changes; endpoint equivalence of cumulative totals is unverified. |
| Describe the result as a diagnosed surface-albedo radiative effect, or use “forcing” with an explicit system-boundary definition? | Current forcing terminology does not itself establish causal independence. |

**Not checked.**

No local files, code, CSVs, regression contract, question files, kernel metadata, masks or RNG configuration were inspected. No command-free local reader was available. The supplied CSV confirmation is therefore main-session evidence, not an independent reproduction. Thompson Table S2 and underlying fields were not opened; regional comparator fields were not downloaded or averaged. Additional claims based on Carlson, Koehler, Pongratz and Ward were not independently rechecked. Agreement between reviewers does not validate those unchecked claims.

Files read: none locally; packet and cited HTML passages consulted. Tools: `functions.exec` for discovery/orchestration, `web__run` search/open/find/click, MCP resource discovery; no commands, R, writes, `writing/` access, or `.docx`/`.pptx`/`.pdf` opens.