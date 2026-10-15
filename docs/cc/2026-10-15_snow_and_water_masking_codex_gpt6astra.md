# Snow and water masking: review by Astra (Codex gpt-6-astra, maximum effort)

Asked 2026-10-15 at Chris's request: is the assumption that past snow cover equals modern
snow cover (built into the satellite blue-sky albedo used for calibration and prediction)
a weak point, what middle-ground remedies exist, what literature bears on it, and how
should water cells be masked in light of the full methods of Oleson et al. (2003)?
Read-only sandbox, web search on. The report below is Astra's text, unedited.

Reference check (done by Claude after the run): all 14 DOIs resolve on Crossref, and
author, year and title match Astra's citations; none is flagged as retracted
(RefChecker `--check-retractions`). Whether each source says what Astra says it says
has not been checked.

Nothing here is a decision. The decisions it raises are put to Chris separately.

---

**Chris’s snow concern is well founded. The present calculation is best interpreted as the albedo effect of changing land cover under a modern climatic relationship—not a complete reconstruction of historical albedo.** A quantitative snow sensitivity analysis is the smallest useful improvement.

**The land-only water-masking decision is sound in purpose. Its implementation depends on what is already inside the 0.25° albedo pixels: a 250 m mask cannot remove water that has already been averaged into them.**

I personally read all eight named code files, the README data table, and the relevant question-file sections. B2c is absent from this checkout; I assessed that issue from the code and external sources. I did not change files or rerun the pipeline. Below, repository findings and published evidence have citations; recommendations and proposed bias mechanisms are identified as judgments or inferences.

**For question 1, I would rank the snow issues as follows.**

1. **The main issue is whether the modern land-cover relationship transfers to past snow conditions.**

   The selected monthly model contains location, elevation and ET/ST/OL fractions, with no snow, temperature or precipitation variable. Prediction supplies past land cover to that same fitted relationship. This directly verifies Chris’s concern. See [R/model_registry.R:32](/home/expert/Documents/contracting/Andria/paleo-RF/R/model_registry.R:32) and [scripts/6_prediction_model.R:36](/home/expert/Documents/contracting/Andria/paleo-RF/scripts/6_prediction_model.R:36).

   Snow makes open ground bright; trees conceal some of it. Consequently, the albedo difference between forest and open land usually becomes larger when snow is present. The same vegetation change can therefore produce different forcing under different snow seasons. This dependence is supported by [Lejeune et al. (2020)](https://doi.org/10.5194/esd-11-1209-2020).

   More precisely, the code assumes that the **modern conditional relationship between vegetation and albedo remains valid**. It does not explicitly hold a measured snow-depth field constant. That relationship can contain modern snow duration, forest masking, temperature–vegetation associations, and other unmeasured influences.

   The spatial smooth can absorb modern snow climatology, because snow varies geographically. It cannot make that climatology evolve through the Holocene. Nor does it identify snow’s contribution separately from vegetation’s. The model also lacks an explicit vegetation-by-snow interaction.

   One correction to B3: location and elevation cancel when differences are taken **on the logit scale**. Predictions are differenced on the albedo scale, after the nonlinear inverse-logit transformation, so their influence does not cancel exactly. The code verifies that distinction; whether spatial confounding makes the vegetation effect too small or too large requires testing. It is not necessarily an attenuation toward zero.

2. **Before estimating the snow bias, establish what snow actually survives in the calibration raster.**

   MCD43A3 can represent both snowy and snow-free surfaces. “Blue-sky” means combining direct-light and diffuse-light albedo; it does not mean snow-free. ERA5’s involvement in that combination would not, by itself, correct snow conditions. See the [MCD43A3 product guide](https://www.umb.edu/spectralmass/modis-user-guide-v006-and-v0061/mcd43a3-albedo-product/).

   The companion MCD43A2 product supplies snow-status, retrieval-quality, observation-availability and land/water information. Its snow flag distinguishes a snowy retrieval from a snow-free retrieval; it is not a measurement of monthly snow duration or ground snow depth. See the [Collection 6.1 band definitions](https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD43A2).

   **Oleson’s 2003 warning should not be transferred unchanged to Collection 6.1.** Collection 6 changed the snow-state decision from the predominant state within the retrieval window to the state on the target day. Nevertheless, clouds, poor illumination and filtering can still remove short snow events or snowmelt observations. NASA specifically warns that rapid changes can receive lower-quality flags. See [Wang et al. (2018)](https://modis.gsfc.nasa.gov/sci_team/pubs/abstract.php?id=27680) and [NASA’s validation guidance](https://modis-land.gsfc.nasa.gov/ValStatus.php?ProductID=MOD43).

   **A new repository finding:** all twelve band descriptions in [the GeoTIFF’s metadata](/home/expert/Documents/contracting/Andria/paleo-RF/data/blue_sky_monthly_2000-2009.tif) say `median`, whereas [README.md:170](/home/expert/Documents/contracting/Andria/paleo-RF/README.md:170) describes a mean. This does **not** prove a median was used—the label could be inherited—but it makes the construction code important. A temporal median could suppress intermittent snowy conditions differently from a mean.

   The producer needs to establish the month order, temporal reducer, snow and quality filters, gap filling, valid-day counts, water treatment, and ERA5 mixing procedure. Also check whether monthly albedo represents a simple average or an average appropriate to incoming sunlight: snow on bright days matters more energetically than snow on dim days.

3. **Spring snow is likely more consequential than the darkest winter gaps.**

   B2b records an earlier calibration table with 39% missing albedo in December and 24% in January. Those are documented historical diagnostics, not counts I regenerated for the current configuration. See [the recorded table](/home/expert/Documents/contracting/Andria/paleo-RF/docs/cc/questions_for_andria_scientific.md:173).

   Two parts of its interpretation need correction:

   - Missing values extending south of the Arctic Circle cannot all be literal polar night. Low solar elevation, retrieval thresholds and cloud filtering also need investigation.
   - A large winter albedo error need not produce a large forcing error. Snow remaining into spring receives much more sunlight. Experiments by [Thackeray et al. (2019)](https://doi.org/10.1175/JCLI-D-18-0083.1) demonstrate this distinction.

   **My priority regions and months**, to be checked against the actual monthly forcing maps, are March–May across the boreal zone and forest–open-land transitions; February farther south; and May–June at northern and mountainous snow margins. Land near retreating ice sheets deserves separate attention. November–January still matters where sunlight remains appreciable, but missing-cell counts alone are a poor measure of importance.

   The monthly kernels already represent the radiative sensitivity associated with seasonal illumination and atmospheric transmission. Do not add another sunlight weight to kernel-derived forcing. CACK translates an albedo change into a radiative effect; it does not provide historical snow. See [Bright and O’Halloran (2019)](https://doi.org/10.5194/gmd-12-3975-2019).

   **A targeted code check:** annual aggregation currently uses `mean(rf, na.rm = TRUE)` at [scripts/9_forcing_barplot.R:171](/home/expert/Documents/contracting/Andria/paleo-RF/scripts/9_forcing_barplot.R:171). If genuinely dark months arrive as missing rather than zero forcing, the annual average can acquire the wrong denominator. Establish physically zero forcing where incoming sunlight is zero, retain the full-year denominator, and distinguish that case from missing data during illuminated months. I have not established that this numerical error occurs in the current outputs.

4. **The ice treatment does not resolve seasonal snow on surrounding land.**

   The current configuration uses the endpoint ice-difference method and all-sky TOA kernels, including CACK’s climatological mean. Some older README descriptions no longer describe those choices. See [config.R:90](/home/expert/Documents/contracting/Andria/paleo-RF/config.R:90) and [config.R:103](/home/expert/Documents/contracting/Andria/paleo-RF/config.R:103).

   The headline threshold method assigns either vegetation albedo or prescribed ice albedo at each endpoint. It therefore handles replacement of ice by land, but it does not represent a colder, snowier climate on ice-free land beside the ice sheet. Nor does the prescribed ice albedo evolve with Holocene changes in melt, snow cover or exposed ice. See [R/differences.R:127](/home/expert/Documents/contracting/Andria/paleo-RF/R/differences.R:127).

   **Inference:** after deglaciation, immediately assigning the modern vegetation–albedo relationship could make newly exposed land too dark if snow persisted longer than today. That would exaggerate the darkening associated with retreat. The opposite is possible under shorter snow seasons. This needs an ice-margin sensitivity, not an assumed universal correction.

   Any added seasonal-snow treatment should apply to the appropriate ice-free land portion. Applying it again to prescribed ice-sheet albedo would risk double counting. C0’s unresolved ice chronology and glacier-albedo provenance remain separate uncertainties; a snow model would not settle them.

5. **The sign of the overall bias is unknown, but the potential size is material.**

   For a specified forest-versus-open-land comparison:

   - More snow generally strengthens both the warming from forest expansion and the cooling from forest removal.
   - Less snow generally weakens those effects.
   - Incorrect snow timing can matter even when annual snow duration is approximately right.

   Those statements do not establish the sign of the bias in this paper’s complete time series. Early-Holocene orbital forcing changed seasonality, while remnant ice affected regional climate. “The early Holocene was warmer, therefore everywhere had less snow” is not an adequate correction. PMIP simulations show contrasting seasonal responses; see [Brierley et al. (2020)](https://doi.org/10.5194/cp-16-1847-2020).

   **Illustration only—not an estimate for this project:** suppose the open-land–forest albedo contrast is 0.05 without snow and 0.30 with snow. A snow-fraction error of 0.20, applied to a forest-cover change of 0.20, changes the cell’s albedo-change estimate by

   \[
   0.20\times0.20\times(0.30-0.05)=0.01.
   \]

   With an assumed kernel magnitude of 100 W m⁻² per unit albedo, that is 1 W m⁻² in the affected month, or approximately 0.25 W m⁻² annually if it persists for three months over the affected land. Continental and global-equivalent effects depend on affected area and cancellation between regions.

   This demonstrates a potentially important scale, not a confidence interval. Neither GAM simulation intervals nor agreement among three kernels measures this structural uncertainty.

**The middle-ground options below are ranked by their usefulness for this paper.** Effort estimates are my judgment, assuming usable inputs; rebuilding a continental daily MODIS archive could dominate the work.

| Rank and option | Data and approximate effort | What it fixes | What remains; likely referee concern |
|---|---|---|---|
| **1. Seasonal diagnosis plus explicit snow sensitivity** | Existing monthly forcing, modern snow climatology, and snow-free/snow-covered vegetation albedos. Days to a few weeks. | Shows whether conclusions depend on snowy months; tests changes in snow fraction or melt timing quantitatively. | Scenarios are sensitivity tests, not reconstructed snow or probabilistic bounds. Defensible with a conditional-climate claim. |
| **2. Separate snowy and snow-free calibration** | Daily MCD43A3/A2, contemporary vegetation and MODIS snow data; or existing class-specific lookup maps. Several weeks or more. | Separates vegetation reflectance from the frequency of snowy conditions; allows vegetation–snow interactions. | Still needs past snow to reconstruct historical conditions. Snow depth, canopy structure and snow properties remain simplified. |
| **3. A PMIP 6 ka sensitivity experiment** | Monthly mid-Holocene and reference snow fraction, or temperature/precipitation, from several models; option 2 or a lookup relationship. Weeks once fields are available. | Tests the modern-snow assumption at a well-studied Holocene date and exposes model disagreement. | One date cannot supply 25 slices. Standard 6 ka experiments do not reproduce early-Holocene ice-margin conditions. |
| **4. Transient simulation anomalies** | Monthly snow or climate from TraCE, HadCM3, CESM or another suitable transient run. Several weeks to months. | Supplies changing snow conditions through time without developing a new climate model. | Coarse resolution, model snow biases, different ice histories and simulated vegetation. A single simulation is a scenario, not ground truth. |
| **5. Degree-day model or statistical snow emulator** | Preferably daily/submonthly temperature and precipitation; modern snow observations for validation. Weeks to months. | A manageable representation of accumulation and melt, driven by paleo climate. | Monthly temperature thresholds alone miss snowfall, storage and melt history. Transferability and validation become additional methodological work. |
| **6. Restrict the headline to demonstrably snow-free conditions** | Snow observations and a defined spatial/seasonal domain. Relatively low effort. | Reduces reliance on the uncertain snowy relationship. | Changes the scientific claim. A summer mean is not annual forcing; modern snow-free months were not necessarily snow-free near past ice margins. |

A simple physical structure for options 1–4 is

\[
\alpha(L,s)=(1-s)\alpha_{\rm snowfree}(L)+s\alpha_{\rm snowy}(L),
\]

where \(L\) is land cover and \(s\) is snow-covered fraction, defined consistently with the albedo observations.

Two cautions matter:

- **Compare both land-cover states under the same specified snow conditions when isolating the land-cover effect.** Letting snow change between endpoints adds climate-driven albedo change. That can be informative, but report it separately.
- MODIS “visible snow” and a climate model’s ground snow fraction are not automatically interchangeable. Forests hide snow from optical instruments. Adding an optical snow predictor without thinking through this distinction can absorb part of the forest-masking effect itself.

Simply adding modern MOD10 to the GAM and retaining that same modern value for every paleo slice would improve interpretation of the calibration, but would leave the temporal assumption in place.

**The data-availability check gives a practical order of attack.**

| Source | What I verified | Implication |
|---|---|---|
| **Modern MODIS snow** | [MOD10CM Collection 6.1](https://nsidc.org/data/mod10cm/versions/61) provides monthly snow extent at 0.05°; [MOD10A1](https://nsidc.org/data/mod10a1/versions/61) provides daily 500 m information. | Suitable starting points for modern seasonality. Respect cloud/darkness flags; the daily NDSI value is not simply a percentage of ground covered by snow. |
| **Existing snow-state albedos** | The [USDA catalogue](https://catalog.data.gov/dataset/data-from-multiscale-climatological-albedo-look-up-maps-derived-from-moderate-resolution-i) lists public monthly, land-class-specific snowy and snow-free albedo lookup maps, including direct and diffuse illumination. | A sensitivity analysis need not begin by fitting an entirely new albedo model. Mapping their vegetation classes to ET/ST/OL—especially heterogeneous OL—needs explicit choices. |
| **PMIP3/PMIP4** | Public experiments are documented through the [PMIP model database](https://pmip.lsce.ipsl.fr/protocols_and_data/model_database). The [6 ka protocol](https://pmip4.lsce.ipsl.fr/doku.php/exp_design%3Amh) specifies the experiment’s boundary conditions. | Check monthly `snc` availability model by model; I did not verify a download-ready snow ensemble. `snw` is snow mass, not snow fraction. A `piControl` comparison also needs an explicit bridge to the observed 2000–2009 baseline. |
| **TraCE-21ka** | The [NCAR archive](https://gdex.ucar.edu/datasets/d651050/) is public. Its [land-file listing](https://gdex.ucar.edu/datasets/d651050/filelist/131/?fl=glade) includes `FSNO`, but in the **decadal-average** collection; monthly atmosphere output is separately listed. | Promising for the full period, but the needed seasonal snow resolution must be established. A public snow variable does not establish a usable monthly snow history. |
| **HadCM3 transient** | Hopcroft et al. (2026), coauthored by Andria, covers the last 10 kyr. Its [public archive](https://zenodo.org/records/16748474) lists annual and JJA temperature outputs and land-use fields. | A particularly relevant collaborator route to monthly snow or temperature/precipitation. Those required fields are not established by the public deposit, and 10 kyr does not cover 11.5 ka. |
| **CESM/iTRACE and LOVECLIM** | A public [iTRACE-Holocene archive](https://zenodo.org/records/14523565) covers 11–0 ka with decadal seasonal/annual outputs. LOVECLIM simulations are listed by the [paleoclimate modelling archive](https://apdrc.soest.hawaii.edu/projects/paleomodeling/model_simulations.php). | Alternatives worth investigating, but I did not verify complete monthly ground-snow fields for the required period. They should not yet be presented as ready-to-use inputs. |

For simulation-based approaches, I would generally use **modelled changes relative to a matched reference climate**, anchored to observed modern snow, rather than substitute raw simulated snow. Check the modern seasonal cycle first, align calendars and ice masks, and retain the uncertainty introduced by downscaling. Interpolating a coarse simulation to 1° does not add climatic detail.

**My recommendation is a small package with a clear stopping rule.**

First resolve the blue-sky product’s construction. Then:

1. Show the existing forcing by month and broad region, identifying unsupported winter predictions and spring snow-sensitive contributions.
2. Build a separate snow-state sensitivity using observed modern snow and existing albedo lookup maps. Check its modern seasonal behaviour against the calibration product.
3. Perturb snow fraction and melt timing—for example, ±0.1/0.2 fraction and earlier/later melt—clearly labelled as stress tests, not historical estimates. Use the actual kernels and report changes in the paper’s principal conclusions.
4. If plausible scenarios change the sign, regional ranking or central interpretation, add a climate-model-informed experiment or narrow the headline claim. If conclusions remain stable, the conditional-climate analysis is much easier to defend.

The benefit to the paper is concrete: it establishes whether the missing process changes the result, without making a full paleo snow model a prerequisite.

If nothing changes, I would use wording along these lines:

> We estimate the albedo effect of reconstructed land-cover change using relationships calibrated against modern satellite observations. These relationships incorporate the modern seasonal snow regime and its interaction with vegetation. Applying them throughout the Holocene assumes that these relationships remain applicable despite changes in temperature, precipitation and snow seasonality. Our estimates therefore describe land-cover and prescribed ice-cover effects evaluated using modern climatic relationships and radiative kernels; they do not reconstruct the complete historical albedo response to changing snow and atmospheric conditions. This structural uncertainty is not represented by the calibration intervals or inter-kernel spread, and its net direction has not been determined.

I would avoid claiming that the spatial smooth or the kernels “account for paleo snow.”

**These are the principal papers I would use, ordered by relevance to the proposed analysis.** The titles, publication details and DOIs below were checked against publisher, institutional or agency sources.

- **Hasler, N., Williams, C. A., Denney, V. C., Ellis, P. W., Shrestha, S., Hart, D. E. T., Wolff, N. H., Yeo, S., Crowther, T. W., Werden, L. K., & Cook-Patton, S. C. (2024).** Accounting for albedo change to identify climate-positive tree cover restoration. *Nature Communications, 15*, 2275. [doi:10.1038/s41467-024-46577-1](https://doi.org/10.1038/s41467-024-46577-1).  
  The most useful recent methodological precedent: combines vegetation-specific snowy/snow-free albedos, monthly snow and illumination, and radiative kernels. It uses modern snow, so it supports the method rather than validating Holocene stationarity.

- **Lejeune, Q., Davin, E. L., Duveiller, G., Crezee, B., Meier, R., Cescatti, A., & Seneviratne, S. I. (2020).** Biases in the albedo sensitivity to deforestation in CMIP5 models and their impacts on the associated historical radiative forcing. *Earth System Dynamics, 11*, 1209–1232. [doi:10.5194/esd-11-1209-2020](https://doi.org/10.5194/esd-11-1209-2020).  
  Supports evaluating the **forest–open-land contrast**, including its snowy and snow-free states, rather than only validating mean albedo. Also cautions against treating simulated albedo responses as unbiased.

- **Thackeray, C. W., Fletcher, C. G., & Derksen, C. (2019).** Diagnosing the Impacts of Northern Hemisphere Surface Albedo Biases on Simulated Climate. *Journal of Climate, 32*, 1777–1795. [doi:10.1175/JCLI-D-18-0083.1](https://doi.org/10.1175/JCLI-D-18-0083.1).  
  Strong support for prioritising spring: the largest albedo biases and the largest climatic consequences need not occur in the same season.

- **Jia, A., Wang, D., Liang, S., Peng, J., & Yu, Y. (2022).** Global Daily Actual and Snow-Free Blue-Sky Land Surface Albedo Climatology From 20-Year MODIS Products. *Journal of Geophysical Research: Atmospheres, 127*, e2021JD035987. [doi:10.1029/2021JD035987](https://doi.org/10.1029/2021JD035987).  
  Provides a relevant modern diagnostic product and separates actual from snow-free conditions. “Actual” is not a fully snow-covered endmember, and this is not evidence that it produced the repository’s raster.

- **Bright, R. M., & O’Halloran, T. L. (2019).** Developing a monthly radiative kernel for surface albedo change from satellite climatologies of Earth’s shortwave radiation budget: CACK v1.0. *Geoscientific Model Development, 12*, 3975–3990. [doi:10.5194/gmd-12-3975-2019](https://doi.org/10.5194/gmd-12-3975-2019).  
  Defines the radiative conversion and its modern climatological basis. It supplies no evolving paleo snow field and does not turn albedo forcing into a complete coupled climate response.

- **Thompson, A. J., Zhu, J., Poulsen, C. J., Tierney, J. E., & Skinner, C. B. (2022).** Northern Hemisphere vegetation change drives a Holocene thermal maximum. *Science Advances, 8*, eabj6535. [doi:10.1126/sciadv.abj6535](https://doi.org/10.1126/sciadv.abj6535).  
  Direct Holocene modelling evidence that vegetation changes and climatic feedbacks matter. Its prescribed vegetation experiments do not validate the magnitude of this pollen-based albedo reconstruction.

- **Hopcroft, P. O., Pirzamanbin, B., Klein Goldewijk, K., Lindström, J., Dawson, A., Kaplan, J. O., Li, F., & Gaillard, M.-J. (2026).** Biogeophysical Impact of Land-Use Scenarios on Holocene Surface Temperatures. *Geophysical Research Letters, 53*, e2025GL118518. [doi:10.1029/2025GL118518](https://doi.org/10.1029/2025GL118518).  
  Particularly relevant recent Holocene/pollen context and a potential source of collaborator-held simulation fields. It is not a validation of a fixed modern snow assumption.

- **Song, Y., Lv, M., Wang, M., Li, X., & Qu, Y. (2021).** Reconstruction of Historical Land Surface Albedo Changes in China From 850 to 2015 Using Land Use Harmonization Data and Albedo Look-Up Maps. *Earth and Space Science, 8*, e2021EA001799. [doi:10.1029/2021EA001799](https://doi.org/10.1029/2021EA001799).  
  A historical land-cover/albedo reconstruction using lookup information, snow data and kernels; explicitly recognises vegetation masking of snow. Its geography and historical period differ from this project.

- **Wang, Z., Schaaf, C. B., Sun, Q., Shuai, Y., & Román, M. O. (2018).** Capturing rapid land surface dynamics with Collection V006 MODIS BRDF/NBAR/Albedo (MCD43) products. *Remote Sensing of Environment, 207*, 50–64. [doi:10.1016/j.rse.2018.02.001](https://doi.org/10.1016/j.rse.2018.02.001).  
  Essential for interpreting modern retrievals and avoiding an outdated application of Oleson’s snow-compositing criticism.

- **Gao, F., He, T., Wang, Z., Ghimire, B., Shuai, Y., Masek, J., Schaaf, C., & Williams, C. (2014).** Multiscale climatological albedo look-up maps derived from moderate resolution imaging spectroradiometer BRDF/albedo products. *Journal of Applied Remote Sensing, 8*, 083532. [doi:10.1117/1.JRS.8.083532](https://doi.org/10.1117/1.JRS.8.083532).  
  Older, but directly useful for implementing a modest snow sensitivity with class-specific snowy and snow-free albedos.

**For question 2, I would approve the land-only objective, with the following implementation priorities.**

1. **Establish whether the input albedo is already a land-only quantity.**

   The existing code still extracts the native 0.25° pixel at each calibration point. It also writes a coarse-average table, but script 4 reads the native-point table. See [scripts/2_calibration_lct_bluesky.R:153](/home/expert/Documents/contracting/Andria/paleo-RF/scripts/2_calibration_lct_bluesky.R:153) and [scripts/4_calibration_model.R:23](/home/expert/Documents/contracting/Andria/paleo-RF/scripts/4_calibration_model.R:23).

   There are two materially different cases:

   - **If each 0.25° value already averages valid land retrievals only**, mapped land area and valid-observation coverage can provide suitable aggregation weights.
   - **If it averages land and water**, weighting that value by a finer land fraction does not recover land albedo.

   For example, a pixel containing equal areas of land with albedo 0.20 and water with albedo 0.05 has mixed albedo 0.125. Applying a land mask to the already averaged value still leaves 0.125.

   The preferred route is therefore water screening at the native MODIS retrieval scale, followed by area-weighted aggregation. Even 500 m retrievals can contain mixed shorelines: the 250 m mask helps identify those, but does not unmix them. Establish a rule for mixed retrievals and report the land area lost through screening.

   If rebuilding is impractical, land-fraction-weighted coarse aggregation remains a possible approximation. Label it as such and test results using increasingly land-dominated input pixels.

2. **Match albedo, elevation, land-cover fractions and forcing area—not just their grid coordinates.**

   Land-mean elevation is appropriate because the model is intended to describe terrestrial vegetation. However, establish that ET/ST/OL describe that same terrestrial portion. Fractions summing to one do not, alone, prove a matching denominator.

   The source posterior contains a `cell_area` field that the pipeline drops, according to [README.md:169](/home/expert/Documents/contracting/Andria/paleo-RF/README.md:169). Its meaning should be recovered before choosing replacement area weights.

   **A consequential existing detail:** script 7a calculates whole-grid-cell areas at [scripts/7a_alb_diff_full.R:124](/home/expert/Documents/contracting/Andria/paleo-RF/scripts/7a_alb_diff_full.R:124), and script 9 uses them in regional and global-equivalent forcing.

   For vegetation forcing based on land-only albedo, use the corresponding land area, or convert the land albedo change to a whole-cell change using land fraction. Apply that correction once. Otherwise, a coastal cell with little land can contribute as though its entire area changed vegetation.

   The ice component needs its own consistent area accounting. Establish whether Dalton’s fraction refers to the whole cell or a land subset; do not blindly multiply every ice term by the modern land fraction. Vegetated land, ice and water must form an explicit, non-overlapping area partition.

3. **Keep lakes excluded when frozen if the target is terrestrial vegetation albedo.**

   A frozen lake remains a lake for this purpose. Excluding it in winter is consistent with excluding it in summer. Including frozen lakes only during winter would change the surface represented by the calibration response and could associate bright lake ice with nearby OL or forest fractions.

   The consequence is a clear scope limitation: the analysis excludes albedo change from lake freeze-up, breakup and snow on lake ice. That is reasonable for a terrestrial vegetation study, but it is not complete landscape albedo forcing.

   Already mixed coarse pixels remain problematic: open water can darken them, whereas snowy lake ice can brighten them. Thus unresolved water contamination can change sign seasonally.

   Also, MOD44W is an **annual land/water classification**, not simply a summer image. Collection 6.1 includes classification and quality information that should be inspected. See the [MOD44W user guide](https://landweb.modaps.eosdis.nasa.gov/data/userguide/MOD44W_User_Guide_C61.pdf).

4. **Retaining coastal cells is defensible; dropping all of them is a sensitivity test.**

   There is no physical requirement to discard a whole 1° cell because some of it is ocean. Retaining its adequately observed land portion is appropriate for a land-only estimand.

   I would retain such cells subject to documented land-area and valid-data criteria, then repeat the analysis after excluding the most water-dominated cells. Keep **land fraction** separate from **valid observed land fraction**: winter retrieval gaps must not silently become missing land area.

   Check representative cells around the Great Lakes, Canadian lake districts, Alaska and complex coastlines, including winter and spring. Compare centre-pixel, unmasked cell-mean and proposed land-mean responses. Look for effects on the fitted vegetation contrast as well as on average albedo.

5. **Treat 2015 geography as a declared approximation.**

   A 2015 mask differs from the 2000–2009 calibration period. Large stable water bodies may make that inconsequential in many cells, but reservoirs, changing shorelines and seasonal water deserve checks. Differences between annual MOD44W maps should not automatically be interpreted as real water change; the product guide cautions against that.

   More substantially, a modern mask does not reconstruct Holocene coastlines or proglacial lakes. A fixed modern terrestrial domain can be a legitimate analysis choice, but it cannot represent the full albedo consequences of changing land–water geography. Snow corrections do not remove that limitation.

**The three water precedents support different parts of the decision.**

- **Moody, E. G., King, M. D., Platnick, S., Schaaf, C. B., & Gao, F. (2005).** Spatially complete global spectral surface albedos: Value-added datasets derived from Terra MODIS land products. *IEEE Transactions on Geoscience and Remote Sensing, 43*, 144–158. [doi:10.1109/TGRS.2004.838359](https://doi.org/10.1109/TGRS.2004.838359).  
  **Strongest precedent for screening water before aggregating land albedo onto coarser grids.** Their processing also removed ephemeral snow, so it is not a precedent for preserving actual all-season snow effects. The methods are available in the [NASA-hosted paper](https://modis-images.gsfc.nasa.gov/reference/docs/Moody_et_al._(2005).pdf).

- **Helbig, M., Wischnewski, K., Kljun, N., Chasmer, L. E., Quinton, W. L., Detto, M., & Sonnentag, O. (2016).** Regional atmospheric cooling and wetting effect of permafrost thaw-induced boreal forest loss. *Global Change Biology, 22*, 4048–4066. [doi:10.1111/gcb.13348](https://doi.org/10.1111/gcb.13348).  
  **Direct precedent for discarding MOD44W open-water pixels in a MODIS albedo analysis.** It does not establish that a fine mask can correct already mixed 0.25° pixels. See [Methods, p. 5](https://www.scottycreek.com/media/documents/publications/79_Helbig%20et%20al.%202016.pdf).

- **Oleson, K. W., Bonan, G. B., Schaaf, C., Gao, F., Jin, Y., & Strahler, A. (2003).** Assessment of global climate model land surface albedo using MODIS data. *Geophysical Research Letters, 30*(8), 1443. [doi:10.1029/2002GL016749](https://doi.org/10.1029/2002GL016749).  
  **Supports area weighting and careful reconciliation of land/water definitions.** Their model retained inland water; coastal mixtures were excluded and lake-related discrepancies remained. It is therefore not an identical inland-water-free target. Its 4% deep-water fill describes that historical product, not evidence of a fill value in this GeoTIFF. I verified those points in [Methods §2 and Results §3](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2002gl016749).

**For the paper, the wording should describe the operation actually performed.** Once native-scale screening and the support checks are established, I suggest:

> To match the terrestrial support of the land-cover reconstruction, we estimated monthly broadband blue-sky albedo over the land portion of each 1° grid cell. Inland water and ocean were identified using the MODIS MOD44W Collection 6.1 land/water mask at 250 m resolution for 2015. Water and unsuitable mixed-pixel retrievals were screened before spatial aggregation, and valid land observations were averaged using their area of intersection with each grid cell. Elevation was averaged over the corresponding land mask. Coastal cells were retained subject to the stated land-area and observation-coverage criteria. Lake and ocean albedo, including seasonal ice and snow on water, were excluded. Vegetation forcing was integrated over the corresponding terrestrial area, with ice-covered area treated separately.

The criteria and permanent-ice screening need to be specified in the methods or supplement.

If only coarse land-fraction weighting is implemented, replace the screening claim with:

> We aggregated the existing 0.25° albedo product using mapped land-area weights. This reduces the influence of water-dominated pixels but cannot remove unresolved water contributions within individual input pixels.

That distinction is more important than adding another precedent citation.

**The short decision/check list for Chris and Andria is:**

1. **Define the claim:** land-cover effects evaluated under modern climatic relationships, or a reconstruction of historical albedo including changing snow?
2. **Recover the blue-sky construction:** producer, code, mean versus `median` label, month order, snow/quality filters, water handling, temporal averaging and ERA5 mixing.
3. **Approve the minimum snow analysis:** monthly/regional contribution plots plus explicit snow-fraction and melt-timing sensitivities; escalate if the conclusions change materially.
4. **Check the most accessible paleo fields:** especially monthly HadCM3 outputs through the existing collaboration, and actual seasonal resolution of TraCE snow.
5. **Specify water processing:** native screening versus coarse weighting, mixed-pixel rules, coastal-cell criteria, and matching ET/ST/OL and elevation support.
6. **Reconcile areas and annual averaging:** original `cell_area`, land versus whole-cell forcing weights, Dalton fraction denominators, and zero-sunlight versus missing months.
7. **Keep ice uncertainties separate:** resolve glacier-albedo provenance and the differing ice histories; document the modern-mask limitation for past lakes and coastlines.