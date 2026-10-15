# Albedo route and the gap between modern land cover and satellite albedo: review by Fable

Asked by Chris (2026-10-15): is adopting Jia et al. 2022 worse than building from MODIS ourselves, and is the gap between the modern land-cover slice and the 2000s albedo a calibration problem, and what can be done? Fable's text, unedited; source copies stay in the scratchpad (copyright). Nothing here is a decision.

---

# Albedo product alignment and the land-cover time gap: review by Fable (2026-10-15)

Independent scientific review for paleo-RF, answering Chris's two questions. Read-only
throughout (worktree `wt-feature`, `data/` in the main checkout, both reviews, both
reconciliations); nothing under `writing/` and no .docx/.pptx/.pdf inside the repository was
opened. Every DOI below was resolved through the Crossref API on 2026-10-15. "Verified" means
I read the passage in the paper or product page itself (copies in `century_gap/sources/` and
`albedo_lit/fable_sources/`); **[unverified]** marks what I could only read as an abstract or
not at all. Evidence (section 1) is kept apart from assessment (section 2).

## Summary

**The 50 BP slice is not AD 1900.** Dawson et al. (2025, Clim. Past, Sect. 2.3, verified)
define the youngest interval as 0.1 to −0.074 ka, AD 1850 to 2024, following the LandCover6k
protocol ("present to 100 BP, where present is the year of coring"; Githumbi et al. 2022,
verified). The file's 25 ages match the paper's 25 intervals ("0.05 ka" in the paper). The predictors are a count-weighted average of pollen from 1850 to the coring year,
clearance peak and regrowth together. The posterior file shows the clearance: open land in the north-eastern US is
0.16 at 50 BP against 0.07-0.09 before 1600; Midwest 0.39 against 0.28-0.31; Plains, Pacific
north-west and boreal unchanged. HYDE 3.2: North American
cropland 44 Mha in 1850, 153 in 1900, 230 in 2000, so most of the change falls inside the bin.

**The gap is a blurred bin, not a century offset.** It bites in the eastern half of the
domain, in the snow months where forest masks snow, as covariate error about the size of the
posterior spread (0.08-0.10 per class). Random error attenuates the fitted cover-albedo
contrast; systematic underestimation of open land in cropland steepens it; the net sign needs
measuring. Two cheap tests settle it: refit on cells with little land-use change
since 1850 (HYDE 3.2 or Anthromes 12K), and compare the 50 BP fractions with MCD12 PFT
fractions.

**On question 1 I revise my ranking.** Jia et al. 2022 is a clear-sky product (its authors say
so); the project's kernels are all-sky, CACK is defined from all-sky surface fluxes, and Jia
ships no BSA and WSA, so the sky-condition sensitivity cannot be run on it; the other points are minor. Own build first (MCD43C3 Duveiller-style as the
pilot, the reconciled 500 m recipe as the product); Jia as the cross-check.

---

## 1. Evidence

### 1.1 What the 50 BP slice covers

**Code and data.** `config.R` lists the slices as `c(50, 200, seq(500, 11500, by = 500))`;
script 1 takes `ages == 50` as the modern table; script 2 attaches the legacy raster
`blue_sky_monthly_2000-2009.tif` to it. `data/veg_posts_interp_ice.RDS` (read with
`Rscript`, read-only) is a plain data frame of 41,961,600 rows with columns `cell_area,
cell_id, x, y, ages, iter, LCT, value, ice` and **no attributes or metadata**: nothing in the
file states bin edges. Its 25 ages are 50, 200, 500 and every 500 years to 11,500.

**The paper (Dawson et al. 2025, Clim. Past 21, 2031-2060, doi:10.5194/cp-21-2031-2025;
verified, p. 2035, Sect. 2.3).** "Time intervals are specified in kiloyears before present
(ka), where present is defined as 1950 CE ... Intervals in the period from 11.7 to 0.7 ka have a
500-year temporal grain (11.7 to 11.2 ka, 11.2 to 10.7 ka, etc.), while the three most recent
intervals have a finer temporal grain (0.7-0.35 ka, 1250-1600 CE, 350 years; 0.35-0.1 ka,
1600-1850 CE, 250 years; 0.1-(−0.074) ka, 1850-2024 CE, 174 years) in order to better capture
the changes associated with intensifying anthropogenic land use over the last 5 centuries.
Pollen samples were assigned to a time interval based on their mean calibrated radiocarbon age.
If multiple samples for a record fell within the same interval, pollen counts were summed by
taxon, so each record would have at most one set of pollen counts per time interval."
Twenty-two 500-year intervals from 11.7 to 0.7 ka plus three young ones make 25, the number in
the file; the labels 11,500 ... 1,000, 500, 200, 50 are consistent with the upper edge minus
200 years for the 500-year bins and with rounded midpoints for the young bins. The paper refers
to the youngest interval as "0.05 ka" (Sect. 2.4: "total unglaciated land area at 0.05 ka"),
and also uses "0.25 ka" as the "modern" reference in the same section (verified; the paper is
not internally consistent on that word). Also verified (Sect. 2.3 and 3.1): age-depth models by
Bchron with extrapolation beyond the youngest dated level limited to 1000 years; "74 % of sites
have a youngest sample in [the interval 0.5 to −0.074 ka]"; spatial interpolation
by the REVEALS-GMRF model of Pirzamanbein et al. (2018) with elevation as the only covariate
("to maintain independence, we refrain from using simulated land cover as a covariate"). The
Discussion (verified, Sect. 4.1): "after the Euro-American arrival ... North American ecosystems
were massively transformed ... These land cover changes can be detected as an increase in open
land from 0.5 ka onwards (Figs. 3, 4, 7)." And in the caveats: "reconstructed Acer cover in
the NEUS/SEC (Fig. 4c) is high compared to settlement-era estimates from witness trees and land
surveys (Thompson et al., 2013; Paciorek et al., 2016)."

**The protocol.** Githumbi et al. 2022 (ESSD 14, 1581, doi:10.5194/essd-14-1581-2022;
verified, Sect. 2.3): "The first three time windows (present-100 BP (where present is the year
of coring), 100-350 BP, 350-700 BP) capture the major human-induced land-cover changes since
the early Middle Ages. Subsequent time windows are contiguous 500-year-long intervals". Trondman
et al. 2015 (GCB 21, 676, doi:10.1111/gcb.12737; abstract verified) reconstruct windows "around
6k, 3k, 0.5k, 0.2k, and 0.05k calendar years before present", so "0.05k" is the protocol's name
for the youngest window. Gaillard et al. 2010 (Clim. Past 6, 483, doi:10.5194/cp-6-483-2010;
figure caption verified) show the same "0-100 cal BP, 100-350 cal BP" scheme.

**What this means for the pairing.** The predictors at 50 BP are not pre-settlement. They are
the pollen of 1850 to the coring year, pooled by summed counts, so each record's contribution
is weighted by how many of its samples fall in those 174 years (core tops are often sampled
densely, which weights the twentieth century, but this is record-specific and not stated in the
paper). Twenty-six per cent of sites have no sample younger than 0.5 ka, and for those cells
the modern value comes from the spatial model and from neighbours. The 1000-year extrapolation
rule means some undated core tops could be assigned to the youngest bin on an extrapolated age
**[a possibility, not verified in the data]**.

### 1.2 What the modern slice shows (read-only summary of the posterior file)

Mean of the 200 draws per cell, then equal-weight means over cells in rectangular regions
(`century_gap/lc_by_slice.R`; values are fractions):

| Region (lon, lat box) | cells | slice | ET | ST | OL |
|---|---|---|---|---|---|
| NE US and SE Canada (−80 to −66, 39-47) | 75 | 50 | 0.25 | 0.58 | **0.16** |
| | | 200 | 0.30 | 0.57 | 0.13 |
| | | 500 | 0.34 | 0.58 | 0.09 |
| | | 1000-2000 | 0.30-0.32 | 0.61-0.63 | 0.07 |
| Midwest (−95 to −80, 37-47) | 133 | 50 | 0.10 | 0.50 | **0.39** |
| | | 200 | 0.12 | 0.57 | 0.31 |
| | | 500-2000 | 0.11-0.12 | 0.59-0.60 | 0.28-0.30 |
| Southeast (−92 to −76, 30-37) | 93 | 50 | 0.12 | 0.48 | **0.40** |
| | | 200 | 0.12 | 0.46 | 0.42 |
| | | 500-2000 | 0.14-0.15 | 0.53-0.55 | 0.30-0.32 |
| Great Plains (−105 to −95, 35-52) | 170 | all | 0.09-0.12 | 0.18-0.21 | 0.67-0.72 |
| Pacific NW (−125 to −116, 42-52) | 80 | all | 0.63-0.65 | 0.19-0.21 | 0.15-0.16 |
| Boreal Canada (−120 to −70, 52-62) | 390 | all | 0.41-0.44 | 0.30-0.32 | 0.24-0.29 |

Per cell, OL(50) − OL(200) has median 0.009, 5th/95th percentiles −0.11/+0.16; 318 of 2,794
cells gain more than 0.10 and 89 more than 0.20 (the latter mostly in the high Arctic, 69-75 N,
where the posterior is poorly constrained, plus the eastern US). The posterior standard
deviation over the 200 draws at 50 BP has a median across cells of 0.077 (ET), 0.078 (ST) and
0.096 (OL). So the land-use signal in the east (about +0.08 to +0.10 in OL relative to
pre-settlement) is of the same size as the posterior spread of a single cell.

### 1.3 Land-use change inside the bin

- HYDE 3.2 (Klein Goldewijk et al. 2017, ESSD 9, 927, doi:10.5194/essd-9-927-2017; Table 2
  and Table 5 verified): North American cropland 0.8 Mha (1700), 8.3 (1800), 44 (1850), 153
  (1900), 199 (1920), 232 (1950), 230 (2000); grazing land 4.2 (1700), 36 (1850), 158 (1900),
  278 (1950), 252 (2000). Two-thirds of the cropland expansion and three-quarters of the grazing
  expansion fall inside the 1850-2024 bin.
- Thompson et al. 2013 (PLoS ONE 8, e72540, doi:10.1371/journal.pone.0072540; introduction
  verified): in the north-eastern US "two centuries of logging and agricultural clearing ...
  removed more than half of the forest cover and cut over almost all of the rest ... Forest
  cover reached its nadir in the mid nineteenth century, after which ... a century of natural
  reforestation and forest growth [followed]. The modern landscape appears to have recently
  reached its apex of reforestation". So in the east the bin starts at the clearance maximum and
  ends near the regrowth maximum.
- Ramankutty, Heller and Rhemtulla 2010 (Ann. AAG 100, 502, doi:10.1080/00045601003788876;
  abstract verified): "croplands were indeed abandoned in the eastern portions of the continent
  accompanied by forest regrowth, but there was compensating cropland expansion and forest
  clearing in the west"; national totals "stabilized around the mid-twentieth century".
- Steyaert and Knox 2008 (JGR 113, D02101, doi:10.1029/2006JD008277; abstract verified):
  20-km land-cover and biophysical-parameter maps, **including surface albedo**, of the eastern
  US at 1650, 1850, 1920 and 1992; "Although much of the eastern forest has returned, the
  average biophysical parameters for recent landscapes remain markedly different from those of
  earlier periods." An independent, published albedo history for the eastern US.
- Nowacki and Abrams 2008 (BioScience 58, 123, doi:10.1641/B580207; abstract verified):
  fire suppression from the 1920s converted "fire-maintained open lands ... to closed-canopy
  forests" in the East (within-bin, and partly a change from OL to ST in the pollen).
- Sohl et al. 2016 (J. Land Use Sci. 11, 476, doi:10.1080/1747423X.2016.1147619; abstract
  verified): annual 250 m land use and land cover of the conterminous US, 1938-1992, 14
  classes (USGS). Ellis et al. 2010 (GEB 19, 589, doi:10.1111/j.1466-8238.2010.00540.x) and
  Ellis et al. 2021 (PNAS 118, e2023483118, doi:10.1073/pnas.2023483118; full text verified):
  anthromes at century steps 1700-2000 and the Anthromes 12K data set (HYDE 3.2-based; "Wildlands
  are defined by zero population and no intensive land use (urban + crops + grazing), Cultured
  anthromes have low populations and <20 % intensive use, and Intensive anthromes are >= 20 %
  intensive"). LUH2 (Hurtt et al. 2020, GMD 13, 5425, doi:10.5194/gmd-13-5425-2020) is the
  CMIP6 harmonisation at 0.25 degree, 850-2015 (title and scope verified; not read).
- Within-class changes that the three classes cannot see: grassland to cropland in the Plains
  (OL to OL); canopy densification in the west and the East (ET to ET, ST to ST); urban land
  (small at one degree; not quantified here).

### 1.4 Settlement-era and modern vegetation data for North America

- Public Land Survey and town-proprietor witness trees give the pre-settlement forest
  composition: Paciorek et al. 2016 (PLoS ONE 11, e0150087, doi:10.1371/journal.pone.0150087;
  abstract verified: 8 km gridded tree composition at Euro-American settlement, NE US) and
  Goring et al. 2016 (PLoS ONE 11, e0151935, doi:10.1371/journal.pone.0151935; upper Midwest).
  Dawson et al. 2016 (QSR 137, 156, doi:10.1016/j.quascirev.2016.01.012; title verified) and
  Trachsel et al. 2020 (Quat. Res. 95, 23, doi:10.1017/qua.2019.81; abstract verified) calibrate
  and compare pollen-vegetation models (STEPPS, REVEALS) against exactly these settlement-era
  data, so Andria's group has the pre-settlement comparison in hand for the NEUS and UMW.
- Modern surface pollen: Whitmore et al. 2005 (QSR 24, 1828, doi:10.1016/j.quascirev.2005.03.005;
  the North American Modern Pollen Database). REVEALS validated on modern pollen against
  vegetation surveys: Sugita et al. 2010 (Quat. Res. 74, 289, doi:10.1016/j.yqres.2010.07.008;
  abstract verified, northern Michigan and Wisconsin); Hopcroft et al. 2026 (GRL,
  doi:10.1029/2025GL118518; verified Sect. 2.1) cite the same validations and note that the
  pollen scenario "does not presently separate natural from anthropogenic open-land". Serge et
  al. 2023 (Land 12, 986, doi:10.3390/land12050986; abstract verified) "validate our
  reconstructions with the global forest change dataset", i.e. REVEALS modern windows against
  satellite tree cover **[details not read; MDPI blocked]**.
- Satellite land cover with a plant-functional-type scheme: MCD12Q1 v061 `LC_Type5`
  (verified on the GEE catalogue page): 1 evergreen needleleaf trees, 2 evergreen broadleaf
  trees, 3 deciduous needleleaf trees, 4 deciduous broadleaf trees, 5 shrub, 6 grass, 7 cereal
  croplands, 8 broadleaf croplands, 9 urban, 10 permanent snow and ice, 11 non-vegetated, 0
  water; 500 m, annual 2001 onwards; MCD12C1 v061 gives the same at 0.05 degree with class
  fractions. Product DOIs 10.5067/MODIS/MCD12Q1.061 and 10.5067/MODIS/MCD12C1.061 (LP DAAC pages
  fetched); algorithm in Sulla-Menashe et al. 2019 (RSE 222, 183, doi:10.1016/j.rse.2018.12.013;
  not read).

### 1.5 The albedo-product alignment points

**Jia et al. 2022 (JGR Atmos. 127, e2021JD035987, doi:10.1029/2021JD035987; read in full).**
- Inputs: "MCD43A3 C06" 2001-2020, A2 C06 snow mark, AOD from MOD08 **C61** (Sect. 2.1,
  verified). Blue-sky by Eq. 1 with S(theta, tau) "retrieved from a LUT based on the solar zenith
  angle and AOD (Schaaf et al., 2002)" (verified).
- The authors describe their own product as clear-sky: "most satellite-derived albedo data are
  only available for clear-sky conditions" (Introduction) and "In the future, we would like to
  combine the albedo clear-sky retrieval product with new climatology datasets to generate a
  global all-sky albedo product" (Conclusions; both verified).
- Climatology: per day of year, mean over 2001-2020 with the maximum and minimum removed; SZA
  above 75 degrees removed; snow-free by ignoring A2-flagged snow pixels (Sect. 2.2.2, verified).
  No BSA, WSA or per-year layers are published (Zenodo record and UMD listing, verified earlier).
- Validation: 38 sites, RMSE 0.031, bias −0.003; snow cases RMSE 0.055 (verified).
- Temporal variability (Sect. 3.4, verified): within-month standard deviation of daily albedo
  exceeds 0.2 in snow-transition regions when aggregated to months; stable over forest, desert
  and permanent snow.

**Kernel definition.** `R/kernels.R` selects HadGEM3 `albedo_sw` (all-sky, TOA), CAM5 `FSNT`
(all-sky, TOA) and `CACK CM` (all-sky, 2001-2016 mean) under Chris's 2026-10-14 decision
(verified in `config.R` lines 103-128). Bright and O'Halloran 2019 (GMD 12, 3975,
doi:10.5194/gmd-12-3975-2019; Table 2 verified) define the kernel's surface albedo as
alpha_s = SW_up,SFC / SW_down,SFC from CERES EBAF **all-sky** boundary fluxes. The albedo change
that is consistent with such a kernel is the change under actual (all-sky) illumination, i.e.
blue-sky with an all-sky diffuse fraction. Ghimire et al. 2014 and Jiao et al. 2017 (Climatic
Change 142, 463, doi:10.1007/s10584-017-1962-8; Sect. 2.3-2.4 verified) instead keep BSA and WSA
separate, weight them by the NCEP reanalysis diffuse and direct fractions (1981-2010 means) and
apply separate black-sky and white-sky kernels (Shell et al. 2008); Hasler et al. 2024 (Nat.
Commun. 15, 2275; Methods verified) do the same weighting with NCEP. Duveiller et al. 2018,
Li et al. 2015 and Lejeune et al. 2020 use the plain mean of BSA and WSA (verified in each; Li:
"their difference is very small and they are highly correlated (R = 0.9992)"). Boisier et al.
2013 (Biogeosciences 10, 1501, doi:10.5194/bg-10-1501-2013; Sect. 2 verified) use white-sky
only, "comparable to that simulated in climate models". No published study found uses a
clear-sky-only blue-sky albedo with an all-sky kernel. (Aside: the README data table, line
177, still describes the HadGEM3 kernel as "clear-sky, top of atmosphere"; the configured
variable is the all-sky one.)

**Magnitude of the sky-condition effect.** The difference between an all-sky and a clear-sky
blue-sky albedo is (f_all − f_clear) x (WSA − BSA_noon). The user guide (verified) states the
isotropic blue-sky interpolation "captures the major part of the phenomenon" and that the
anisotropic form matters "for large angles and bright surfaces" (Roman et al. 2010,
doi:10.1016/j.rse.2009.11.014; abstract not obtainable, **[its numbers unverified]**). I found no
published table of WSA − BSA_noon by cover type that I could read (Gao et al. 2005, JGR 110,
D01104, doi:10.1029/2004JD005190, is the reference; abstract only). Li et al. 2015's "very
small" and R = 0.9992 is the only verified quantitative statement. The *contrast-relevant*
quantity is the difference of (WSA − BSA_noon) between forest and open land, smaller again. No
published number; it is directly measurable from any product that ships BSA and WSA (MCD43C3,
MCD43A3), and not from Jia.

**Collection 6 versus 6.1 (LP DAAC MCD43A3 v061 page, verified).** "The following changes and
improvements were implemented for inputs to MODIS Version 6.1 BRDF/Albedo Model Parameters data
products: (1) Version 6.1 Level-1B (L1B) products have been improved by undergoing various
calibration changes that include: changes to the response-versus-scan angle (RVS) approach that
affects reflectance bands for Aqua and Terra MODIS, corrections to adjust for the optical
crosstalk in Terra MODIS infrared (IR) bands, and corrections to the Terra MODIS forward look-up
table (LUT) update for the period 2012-2017, and (2) A polarization correction has been applied
to the L1B Reflective Solar Bands (RSB)." The 2012-2017 LUT correction lies outside 2000-2009;
the crosstalk correction concerns thermal bands not used by MCD43. I found **no published
quantification of MCD43 C6 versus C6.1 albedo differences**; Roman et al. 2024 (RSE 302,
113963) covers C6.1 versus VIIRS, not C6 versus C6.1.

**Solar zenith 75 versus 70.** Noon SZA = |latitude − declination|; with declination −23.4
degrees on 21 December the 70-degree rule removes December north of 46.6 N and the 75-degree
rule north of 51.6 N; the same five-degree shift applies to the other months (January 49 to 54 N,
November 51.5 to 56.5 N, February 57 to 62 N). The product team's guidance is 70 (NASA MOD43
validation page and MCD43GF guide, verified earlier); Wang and Zender 2010
(doi:10.1016/j.rse.2009.10.014) document the snow-albedo bias at high SZA **[magnitudes
unverified; abstract not obtainable]**.

**Period: 2000-2009 versus 2001-2020.** Evidence on decadal change of the snow season over
North America: Mudryk et al. 2020 (The Cryosphere 14, 2495, doi:10.5194/tc-14-2495-2020;
abstract verified): NH snow extent trends 1981-2018 "are negative in all months and exceed
−50 x 10^3 km2 yr−1 during November, December, March, and May". Derksen and Brown 2012 (GRL 39,
L19504, doi:10.1029/2012GL053387; abstract verified): June snow-cover extent loss 1979-2011 of
−17.8 % per decade, records set in 3 of 5 years for North America. He et al. 2014 (JGR 119,
10281, doi:10.1002/2014JD021667; abstract verified): over 1981-2010 "albedo of the Northern
Hemisphere was found to be decreasing in July, likely due to the shrinking snow cover.
Meanwhile, albedo in January was found to be increasing". Hou et al. 2025 (Nature 641, 1162,
doi:10.1038/s41586-025-08987-z; abstract verified): over 2001-2020 the global snow-free land
albedo rose by 2.2 %, and the snow-dynamics forcing was one-seventh of the snow-free effect and
of opposite sign. None of these gives the 2000s-versus-2010s difference of a monthly albedo
climatology over North America by month; the direction is a shorter snow season in spring and
early summer, the size at one degree is not published. Jia's within-month variability figures
(Sect. 3.4) show that in snow-transition regions the climatology's sampling error is dominated
by interannual snow timing, which a 20-year mean halves in variance relative to a 10-year mean.

**Trimming.** Jia drop the maximum and minimum of up to 20 values per day of year: a 10 %
trimmed mean. No evidence needed: for a near-symmetric sample the trimmed and plain means differ
by far less than the sampling error; for a skewed snow-timing distribution the trimmed mean
moves toward the median by a few thousandths at most.

**Winter snow-free blanks.** Jia Sect. 3.3 (verified): "The snow-free map in winter left blanks
over high latitudes". This is a property of the snow-free *state*, identical for any product
that leaves gaps (the reconciled own build also leaves them; Moody 2008 and MCD43GF fill them).

**Water.** Jia's 0.05-degree means include lake pixels; the 500 m tiles can be screened with
MOD44W exactly as in the own build (verified earlier). Not a differentiator at 500 m.

### 1.6 Published precedents for pairing land cover with satellite albedo

| Study | Land cover used for calibration | Albedo | How history or pollen enters |
|---|---|---|---|
| Myhre et al. 2005 (GRL 32, L21410; abstract verified) | MODIS land cover (current) | MODIS class albedos | potential natural vegetation map for the pre-agricultural state |
| Boisier et al. 2013 (verified Sect. 2) | MODIS land cover, 0.05-degree cells with > 95 % of one class | MCD43C3 white-sky 2000-2011, snow-covered and snow-free | class albedos applied to LUCID 1870 and 1992 fractions |
| Ghimire et al. 2014; Jiao et al. 2017 (verified) | MCD12 majority class, 30 arcsec | Gao et al. 2014 look-up maps 2001-2011 | LUH fractions 1700-2005; snow and radiation climatologies "assumed stationary over the full historical period" (Jiao Sect. 2.3) |
| Hasler et al. 2024 (verified) | same look-up maps | same | restoration scenarios |
| Hou et al. 2025 (abstract; methods per Astra's review) | MCD12 at native resolution, water masked | MCD43A3 C6.1 2001-2020, class means at 1 degree | 2001-2020 conversions |
| Duveiller et al. 2018; Lejeune et al. 2020 (verified) | ESA CCI 300 m fractions 2008-2012; GlobCover > 95 % pure cells | MCD43C3 C5 2008-2012; GlobAlbedo 1998-2011 | Lejeune: LUH2 1850-2000 fractions for the historical forcing |
| Zhao and Jackson 2014 (verified) | MCD12 9-year majority map | MCD43 2001-2011 | potential forestry activities |
| Strandberg et al. 2014 (Clim. Past 10, 661; verified Sect. 1-2) | none: RCM with LPJ-GUESS potential vegetation plus HYDE 3.1 or KK10 deforestation at 6 ka and 0.2 ka (AD 1750) | model albedo (snow-forest albedo fixed at 0.2) | pollen used for evaluation; 0.2 ka is "pre-industrial" |
| Hopcroft et al. 2026 (verified) | none: HadCM3 with HYDE 3.2/3.3, KK10 and the pollen-based LandCover6k scenario | model albedo | 6 minus 0.25 ka BP anomalies |
| Barnes and Roy 2008 (GRL 35, L09706; abstract verified) | Landsat land cover 1973 and 2000 | MODIS snow-free class albedos | contemporary change within 36 ecoregions |
| Betts et al. 2007 (AFM 142, 216; abstract verified) | model land use since 1750 | HadAM3 | −0.2 W m−2 present-day albedo forcing |

**Common pattern (verified across the table):** every observational study calibrates class or
cell albedo on land cover from the *same years* as the satellite albedo, then applies the
fitted class albedos to historical, potential or scenario land cover, assuming the class
albedo, the snow climatology and the radiation are stationary. None calibrates on a historical
or pollen-based land cover against present-day albedo, and none calibrates on a 174-year pooled
window. paleo-RF is the first of its kind in that respect; the closest analogue is Boisier's
pure-cell selection and Lejeune's > 95 % rule, which are *restrictions of the calibration set to
cells whose cover is known*.

---

## 2. Assessment (mine, not evidence)

### 2.1 Question 1: is Jia et al. 2022 the worse option?

Weighing each point by its likely effect on what the calibration estimates: the level of
albedo (absorbed by the model's intercept and spatial smooth, and cancelling in the
slice-to-slice differences) and the forest-open contrast by month and place (what drives the
forcing).

| Point | Effect on the level | Effect on the contrast | Weight |
|---|---|---|---|
| 2001-2020 versus 2000-2009 | small decadal drift in spring snow months at high latitude (direction: shorter snow season; size unpublished at 1 degree, my guess 0.01-0.03 in April-May north of 50 N) | the contrast depends on snow presence; a drift in snow timing shifts the month in which the contrast peaks, not its size; a 20-year mean is *less noisy* | low; and the land-cover bin (1850-2024) does not fix a decade to match, so "2000-2009" is the manuscript's choice, not a scientific constraint |
| clear-sky LUT diffuse fraction versus ERA5 all-sky | (f_all − f_clear)(WSA − BSA): perhaps 0.005-0.01 in cloudy months (judgement) | the forest-open difference in (WSA − BSA); small but of unknown sign by month; the kernel is all-sky and CACK is defined from all-sky fluxes, so the all-sky form is the consistent one | **high, for a different reason than size**: Jia ships no BSA and WSA, so the sensitivity cannot be measured on it, and the all-sky form matching the kernel cannot be built from it |
| Collection 6 versus 6.1 | calibration-level changes, unquantified in print; expected far below the 0.03 validation RMSE | negligible | low |
| SZA cut 75 versus 70 | adds five degrees of latitude of winter coverage, with higher-SZA snow retrievals that the product team flags | changes which cells enter the December-February models north of 47-57 N | moderate for the winter north only; both are published; 75 is reversible in Jia only by masking, 70 is not reversible to 75 in an own build without the pilot's "75" stream (already planned) |
| min/max trimming | thousandths | none | negligible |
| winter snow-free blanks | property of the snow-free state in any unfilled product | same | not a differentiator |
| water at 0.05 degree | lake contamination; fixable at 500 m with MOD44W | same as own build | not a differentiator at 500 m |
| **not on Chris's list: no per-year, no BSA/WSA, no quality layers in the deliverable** | | no quality sensitivity, no period sensitivity, no snowy-state stream, no kernel-consistent illumination | **high** |

**Verdict.** I agree with Chris, with one correction to the reasoning. The decisive point is
not that Jia's choices are worse (each is published and defensible); it is that Jia is a
*finished* climatology under a clear-sky definition that does not match the all-sky kernels
the project has chosen, and it cannot be re-weighted, re-periodised or quality-split. The
project's calibration needs a response whose illumination definition can be made consistent
with the forcing step and whose sensitivities (quality, zenith, snow state, period) can be
reported. Only a build from MCD43 (A3/A2 at 500 m, or C3 at 0.05 degree) gives that.

**Revised ranking:**
1. **Own build from MCD43 v061 with ERA5 all-sky direct and diffuse fractions**, as the two
   reconciliations specify, with **MCD43C3 v061 Duveiller-style as the pilot and first-pass
   product** (22 GB for one retrieval per month, or a monthly mean from the daily files; BSA
   and WSA present; `Percent_Snow`, `Quality` present; coastal and lake-shore cells flagged by
   MOD44W water fraction and excluded or reported). The pilot measures in one pass the three
   quantities that decide everything else: the all-sky versus clear-sky (and versus f = 0.5)
   effect on the forest-open contrast by month; the 70 versus 75 degree effect; and the
   2000-2009 versus 2001-2020 effect (C3 is cheap enough to pull both periods).
2. **Jia et al. 2022** as the independent cross-check of the own build and of the legacy raster
   (0.05-degree set, 5 GB; or the 0.5-degree file, 79 MB, first), and as the fallback response
   if the build fails, in which case the methods text must say "clear-sky blue-sky albedo
   (Jia et al. 2022) paired with all-sky kernels" and give the C3-based sensitivity as the bound.
3. CLARA-A3 blue-sky as a second, non-MODIS check only.

What I got wrong earlier: I ranked by "fewest unpublished judgements" and treated the
diffuse-fraction source as a fork with precedent on both sides. It is, but the fork is tied to
the kernel: Jiao, Ghimire and Hasler, the only studies that carry MODIS albedo through to a
kernel forcing, all use reanalysis all-sky fractions (or separate BSA/WSA kernels); the
clear-sky LUT users (Jia; the ABoVE product) stop at the albedo. With all-sky kernels fixed by
Chris on 2026-10-14, the own build is the option grounded in that lineage.

**Residual judgement to flag honestly:** the size of the sky-condition effect on the contrast
is not in the literature. If the C3 pilot shows it below 0.005 in every month (my expectation
for snow-free months, less sure for snow months), Jia would be scientifically acceptable and
the ranking could be revisited on effort grounds; the manuscript's stated method (MODIS + ERA5,
2000-2009) would still favour the build.

### 2.2 Question 2: the time gap

**How serious, and where it bites.** Less serious than the question assumes, and differently
shaped:

- The modern bin is 1850 to the coring year, so the pairing is *within-window*; there is no
  century offset. The problem is a **blurred window** whose pollen mixes the clearance peak with
  the regrowth, weighted by sample counts that the paper does not report. In the east, where
  clearance peaked around 1850-1900 and forest cover is now near its regrowth maximum, the bin
  average lies somewhere between the two; the file's NE US value (OL 0.16, trees 0.84) is close
  to the modern forested landscape, not to the 1850 nadir. In the Midwest and Southeast the bin
  shows OL 0.39-0.40, which is plausibly *below* the present cropland-dominated fraction (I
  have no satellite number per cell here; option A below measures it).
- The Plains conversion (grassland to cropland), western canopy densification and eastern
  mesophication are mostly within-class and invisible to ET/ST/OL; they appear as within-class
  albedo variation that the spatial smooth absorbs. They do not bias the three-class contrast,
  but they are a reminder that the fitted "open land" albedo in the Plains is cropland albedo.
- The boreal, the west and the Arctic, where the Holocene changes the paper quantifies are
  largest (ice retreat, afforestation, treeline), show no land-use signal in the bin (table in
  1.2). The calibration cells that matter most for the paper's result are the least affected.
- Months: the forest-open contrast is largest with snow on the ground (verified in Betts 2007,
  Burakowski et al. 2018, Jiao 2017, Dawson 2025's own discussion of snow masking); in the east
  and Midwest that is December to March. So the mismatch bites the winter and early-spring
  models for the eastern half of the domain, and barely the summer ones (summer forest-open
  contrasts are 0.03-0.06, and a 0.1 error in OL moves the predicted albedo by about 0.005).

**Direction of bias.** With the model `albedo ~ s(x,y) + s(elev) + s(OL, ET, ST)` (beta
regression, logit link; `R/model_registry.R` verified), an error in the cover fractions is
covariate error. Two cases:
- Random error (posterior spread, sampling of the bin): classical attenuation; the cover
  contrasts are pulled toward zero, hindcast albedo changes and forcing are *underestimated* in
  magnitude. The posterior spread alone (0.08-0.10 per class) already does this; the land-use
  blur adds a comparable variance in the east.
- Systematic error: if OL is underestimated where cropland dominates (high winter albedo), the
  fitted OL effect is *steeper* than the truth, and forcing is overestimated; if OL is
  overestimated where forest has regrown (east), the effect is flatter. Both occur in different
  regions; the spatial smooth absorbs the regional part of each and leaves the within-region
  part to the cover terms. The net sign is not decidable by reasoning; it needs the two tests
  below.
- One mitigating structural point: the hindcasts use the same reconstruction, so a REVEALS
  bias common to all slices (e.g. too much tree cover in open landscapes) cancels in the
  calibration-prediction pair; only the *time-specific* blur does not cancel. And every
  published class-albedo study (table 1.6) pairs modern class albedo with historical fractions
  under a stationarity assumption that is logically the same as ours.

**Options with precedent** (what each fixes, what it costs, data, and the interaction with the
product choice):

| Option | Precedent | Fixes | Costs | Data | Interaction with question 1 |
|---|---|---|---|---|---|
| **A. Measure the blur: compare the 50 BP fractions with satellite PFT fractions per cell** (MCD12C1 v061 `LC_Type5`: ET = classes 1+2, ST = 3+4, OL = 5-9 and 11, water excluded; or MOD44B tree fraction for trees only) and with HYDE 3.2 cropland + grazing | REVEALS modern windows validated against vegetation maps and satellite forest (Sugita 2010; Hellman 2008 per Hopcroft 2026; Serge 2023); Dawson's group's own settlement-era comparisons (Trachsel 2020) | tells where and how much the modern bin departs from the 2000s landscape; a methods-paragraph number | one script; a definitional caveat (REVEALS cover is pollen-productivity-weighted plant cover, MCD12 is canopy > 10 % by pixel) | MCD12C1 v061 2001-2009 or 2001-2020 (one 0.05-degree file per year, ~ 100 MB each [size unverified]); HYDE 3.2 5-arcmin grids (free) | none; do first |
| **B. Restrict the calibration to cells with little land-use change since 1850** (HYDE 3.2: cropland + grazing fraction change 1850 to 2000 below x; or Anthromes 12K: cells "Wildlands" or "Cultured" in both 1850 and 2000), as a sensitivity run, adopted as primary only if it changes the contrasts beyond their posterior uncertainty | restriction of the calibration set to cells whose cover is known: Boisier 2013 and Lejeune 2020 (> 95 % pure cells); potential-natural-vegetation masks (Myhre 2005; Ramankutty and Foley 1999) | removes the cells where the blur is worst | drops the east and Midwest (my estimate 10-20 % of cells), which carry most of the summergreen variation at mid-latitude; the compositional smooth then extrapolates there; the spatial smooth's k must be re-checked | HYDE 3.2 (ESSD, free) or Anthromes 12K (Harvard Dataverse, DOI not verified) | none; cheap; the natural second test after A |
| **C. Re-pair: calibrate on satellite land cover translated to ET/ST/OL, apply to the REVEALS slices** | the whole class-albedo lineage: Myhre 2005, Boisier 2013, Ghimire 2014, Jiao 2017, Hasler 2024, Hou 2025, Zhao and Jackson 2014 | removes the time mismatch entirely; land cover and albedo from the same years | introduces a *definition* mismatch between MCD12 fractions (calibration) and REVEALS fractions (prediction); A quantifies it; if the two scales differ, a translation (regression of REVEALS on MCD12 fractions at 50 BP) is needed, which is a new modelling step and hurts readability | MCD12C1 v061 for the chosen years | strong: the satellite land-cover years should equal the albedo years (2001-2009 for the own build; Jia's attached MCD12Q1 2001-2020 mode climatology if Jia) |
| **D. Modern-pollen REVEALS layer** (NAMPD surface samples through REVEALS-GMRF) as the calibration land cover | Trondman 2015's 0.05k window; Sugita 2010; the NAMPD (Whitmore 2005) | same units as the paleo slices; dated to collection (1960s-2000s) | a REVEALS and GMRF rerun by Andria's group, outside this repository; surface samples have their own basin and age biases | NAMPD; Andria's pipeline | none |
| **E. Class look-up albedos instead of the cell regression** (Gao 2014 maps, or Hou 2025's 1-degree ALLUMs 2001-2020, by month and snow state) | Ghimire, Jiao, Hasler, Bright and O'Halloran 2019, Hou 2025 | no modern land cover needed; time pairing moot | replaces the paper's method (spatial smooth, elevation, no-analogue compositions); the REVEALS-versus-MCD12 definition issue remains; against Chris's readability rule as a replacement | Gao 2014 (USDA, public domain) or ALLUMs (Zenodo, 3.4 GB) | makes the cell-mean response a cross-check only |
| **F. Land use as a covariate** (HYDE cropland or anthrome intensity at 2000 in the calibration; set to its 1850 value or zero for the paleo slices) | crops as a separate class: Duveiller 2018, Lejeune 2020, Boisier 2013 | separates the cropland albedo from the natural open-land albedo | collinear with OL; a counterfactual covariate; changes the model ladder; one more thing for Andria to read | HYDE 3.2 | none |
| **G. Out-of-sample check of the hindcasts at 200 and 500 BP against Steyaert and Knox 2008's 1650, 1850, 1920 and 1992 albedo maps of the eastern US** (question E1 in the file) | Steyaert and Knox 2008 (20 km, eastern US) | tests the whole chain where the land-use signal is strongest | the maps are model parameters, not observations; eastern US only | the Steyaert and Knox data set (availability not checked) | none |

**Recommendation.** Do A and B now, in that order; they are a few scripts and no new method.
Report A as a methods paragraph ("the youngest window, 1850 to the coring year, differs from
MCD12 PFT fractions by x in the east and y elsewhere") and B as a sensitivity ("restricting the
calibration to cells with < x % intensive land-use change since 1850 changes the hindcast
forcing by z"). Adopt C only if A shows the two cover definitions agree well (so that the
re-paired model predicts REVEALS fractions without a translation) and B shows a material
effect; otherwise the full-domain calibration stays, with the time window stated plainly. D is
the scientifically cleanest fix but is Andria's call and outside this repository. E stays a
comparison. F and G are optional.

**Interaction with the product period.** Because the land-cover window spans 1850-2024, the
albedo period is not constrained by the pairing; choose it for consistency with the kernel and
the diffuse fraction (CACK CM is 2001-2016; ERA5 any). Under C, land cover and albedo should
share the same years exactly. Under A and B, the comparison land cover should be the albedo's
years.

---

## 3. Decisions for Chris

1. Primary response: own build (reconciled MCD43A3/A2 v061 + ERA5 all-sky recipe), with
   MCD43C3 v061 Duveiller-style as the pilot and first-pass product; Jia 2022 demoted to the
   cross-check and fallback. Agree?
2. Add three streams to the C3 pilot: diffuse fraction from ERA5 all-sky, from the clear-sky
   LUT (or f = 0.5 as the Duveiller/Li proxy), and f = 0 (noon black-sky); report the
   forest-open contrast difference by month. This is the number that decides whether Jia is
   acceptable at all.
3. Period: pull both 2000-2009 and 2001-2020 in the C3 pilot (cheap) and report the difference
   by month and latitude band; then fix one period for albedo, ERA5 and the methods text.
4. Run option A (50 BP fractions versus MCD12C1 PFT fractions and HYDE 3.2 change) and option
   B (restricted calibration) as sensitivities before any re-pairing decision. Approve the small
   downloads (MCD12C1 v061 for the chosen years; HYDE 3.2).
5. Fold question B10 into general question 1 (slice definition), rewritten with the Dawson
   2025 definition, and add the questions below.
6. Fix the README data-table entry that calls the HadGEM3 kernel clear-sky (config selects
   `albedo_sw`, all-sky).

## 4. Questions for Andria

1. Is the 50 BP slice in `veg_posts_interp_ice.RDS` the 1850-2024 CE interval of Dawson et al.
   2025 (0.1 to −0.074 ka), with pollen counts summed per record within the interval, and are
   the other labels the paper's bins (200 = 1600-1850 CE, 500 = 1250-1600 CE, 1000 = 1.2-0.7 ka
   ...)? (Supersedes B10 and general question 1.)
2. Within the youngest interval, how are records weighted: do most records contribute many
   twentieth-century samples (core tops dated to the collection year), or one or two samples
   near 1850-1900? Is there a per-record count of samples in that interval we could use?
3. Was the pairing of the 1850-2024 window with 2000-2009 albedo considered when the
   calibration was designed, and has the group compared the youngest REVEALS window with
   satellite land cover (MCD12, VCF) for North America, as Serge et al. 2023 did for Europe?
4. Would a modern-pollen (NAMPD) REVEALS-GMRF layer be feasible as the calibration land cover
   (option D), or is the settlement-era comparison (Trachsel 2020) the closer check?
5. Preference, for readability, between restricting the calibration to low-land-use-change
   cells (B) and re-pairing on satellite land cover (C), if the tests show the blur matters?

---

## References (DOIs resolved on Crossref, 2026-10-15)

- Barnes, C. A., Roy, D. P. (2008). GRL 35, L09706. doi:10.1029/2008GL033567 [abstract]
- Betts, R. A. et al. (2007). Agric. For. Meteorol. 142, 216-233. doi:10.1016/j.agrformet.2006.08.021 [abstract]
- Boisier, J. P., de Noblet-Ducoudré, N., Ciais, P. (2013). Biogeosciences 10, 1501-1516. doi:10.5194/bg-10-1501-2013 [read]
- Bright, R. M., O'Halloran, T. L. (2019). GMD 12, 3975-3990. doi:10.5194/gmd-12-3975-2019 [read]
- Burakowski, E. et al. (2018). Agric. For. Meteorol. 249, 367-376. doi:10.1016/j.agrformet.2017.11.030 [abstract]
- Dawson, A. et al. (2016). QSR 137, 156-175. doi:10.1016/j.quascirev.2016.01.012 [title only]
- Dawson, A. et al. (2025). Clim. Past 21, 2031-2060. doi:10.5194/cp-21-2031-2025 [read]
- Derksen, C., Brown, R. (2012). GRL 39, L19504. doi:10.1029/2012GL053387 [abstract]
- Duveiller, G., Hooker, J., Cescatti, A. (2018). Nat. Commun. 9, 679. doi:10.1038/s41467-017-02810-8 [read]
- Ellis, E. C. et al. (2010). GEB 19, 589-606. doi:10.1111/j.1466-8238.2010.00540.x [abstract]
- Ellis, E. C. et al. (2021). PNAS 118, e2023483118. doi:10.1073/pnas.2023483118 [read]
- Gaillard, M.-J. et al. (2010). Clim. Past 6, 483-499. doi:10.5194/cp-6-483-2010 [figure caption]
- Gao, F. et al. (2005). JGR 110, D01104. doi:10.1029/2004JD005190 [abstract]
- Ghimire, B. et al. (2014). GRL 41, 9087-9096. doi:10.1002/2014GL061671 [abstract; method per Jiao 2017]
- Githumbi, E. et al. (2022). ESSD 14, 1581-1619. doi:10.5194/essd-14-1581-2022 [read, Sect. 2.3]
- Goring, S. J. et al. (2016). PLoS ONE 11, e0151935. doi:10.1371/journal.pone.0151935 [title]
- Hasler, N. et al. (2024). Nat. Commun. 15, 2275. doi:10.1038/s41467-024-46577-1 [read]
- He, T., Liang, S., Song, D.-X. (2014). JGR Atmos. 119, 10281-10298. doi:10.1002/2014JD021667 [abstract]
- Hopcroft, P. O. et al. (2026). GRL 53, e2025GL118518. doi:10.1029/2025GL118518 [read]
- Hou, Z. et al. (2025). Nature 641, 1162-1171. doi:10.1038/s41586-025-08987-z [abstract]
- Hurtt, G. C. et al. (2020). GMD 13, 5425-5464. doi:10.5194/gmd-13-5425-2020 [title]
- Jia, A. et al. (2022). JGR Atmos. 127, e2021JD035987. doi:10.1029/2021JD035987 [read]
- Jiao, T. et al. (2017). Climatic Change 142, 463-476. doi:10.1007/s10584-017-1962-8 [read]
- Klein Goldewijk, K. et al. (2017). ESSD 9, 927-953. doi:10.5194/essd-9-927-2017 [read, tables]
- Lejeune, Q. et al. (2020). ESD 11, 1209-1232. doi:10.5194/esd-11-1209-2020 [read]
- Li, Y. et al. (2015). Nat. Commun. 6, 6603. doi:10.1038/ncomms7603 [read]
- Mudryk, L. et al. (2020). The Cryosphere 14, 2495-2514. doi:10.5194/tc-14-2495-2020 [abstract]
- Myhre, G. et al. (2005). GRL 32, L21410. doi:10.1029/2005GL024004 [abstract]
- Nowacki, G. J., Abrams, M. D. (2008). BioScience 58, 123-138. doi:10.1641/B580207 [abstract]
- Paciorek, C. J. et al. (2016). PLoS ONE 11, e0150087. doi:10.1371/journal.pone.0150087 [abstract]
- Pirzamanbein, B. et al. (2018). Spatial Statistics 24, 14-31. doi:10.1016/j.spasta.2018.03.005 [title]
- Ramankutty, N., Foley, J. A. (1999). GBC 13, 997-1027. doi:10.1029/1999GB900046 [abstract]
- Ramankutty, N., Heller, E., Rhemtulla, J. (2010). Ann. AAG 100, 502-512. doi:10.1080/00045601003788876 [abstract]
- Román, M. O. et al. (2010). RSE 114, 738-760. doi:10.1016/j.rse.2009.11.014 [title only]
- Román, M. O. et al. (2024). RSE 302, 113963. doi:10.1016/j.rse.2023.113963 [abstract]
- Serge, M. A. et al. (2023). Land 12, 986. doi:10.3390/land12050986 [abstract]
- Sohl, T. et al. (2016). J. Land Use Sci. 11, 476-492. doi:10.1080/1747423X.2016.1147619 [abstract]
- Steyaert, L. T., Knox, R. G. (2008). JGR 113, D02101. doi:10.1029/2006JD008277 [abstract]
- Strandberg, G. et al. (2014). Clim. Past 10, 661-680. doi:10.5194/cp-10-661-2014 [read, Sect. 1-2]
- Strandberg, G. et al. (2022). QSR 281, 107431. doi:10.1016/j.quascirev.2022.107431 [abstract]
- Sugita, S. et al. (2010). Quat. Res. 74, 289-300. doi:10.1016/j.yqres.2010.07.008 [abstract]
- Sulla-Menashe, D. et al. (2019). RSE 222, 183-194. doi:10.1016/j.rse.2018.12.013 [title]
- Thompson, J. R. et al. (2013). PLoS ONE 8, e72540. doi:10.1371/journal.pone.0072540 [read, introduction]
- Trachsel, M. et al. (2020). Quat. Res. 95, 23-42. doi:10.1017/qua.2019.81 [abstract]
- Trondman, A.-K. et al. (2015). GCB 21, 676-697. doi:10.1111/gcb.12737 [abstract]
- Wang, X., Zender, C. S. (2010). RSE 114, 563-575. doi:10.1016/j.rse.2009.10.014 [title only]
- Wang, Z. et al. (2018). RSE 207, 50-64. doi:10.1016/j.rse.2018.02.001 [abstract]
- Whitmore, J. et al. (2005). QSR 24, 1828-1848. doi:10.1016/j.quascirev.2005.03.005 [title]
- Zhao, K., Jackson, R. B. (2014). Ecol. Monogr. 84, 329-353. doi:10.1890/12-1705.1 [read]
- Product pages verified: LP DAAC MCD43A3 v061 (version description), MCD12Q1 v061 and MCD12C1 v061; GEE catalogue MCD12Q1.061 (`LC_Type5` classes); NASA MOD43 validation status page.

Not obtainable at source today (Wiley, Elsevier and MDPI blocked curl): Steyaert and Knox 2008,
Gao et al. 2005, Román et al. 2010, Cescatti et al. 2012, Wang and Zender 2010, Kujawa et al.
2016, Serge et al. 2023 full texts; the AGU 2023 abstract of the project's method (CAPTCHA).
