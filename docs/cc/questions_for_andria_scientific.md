# Open questions for Andria: scientific and technical

**Frozen 20 October 2026.** This content went into the Word document on
Andria's Google Drive, where her answers are being collected. New scientific
questions go in `questions_for_andria_scientific_from_2026-10-20.md`.

Maintained by Chris with Claude Code; kept current as work proceeds (see
AGENTS.md). Simple and logistical questions are in
`questions_for_andria_general.md`. Each entry says what the code does now,
why it matters, what the options are, and what we need from you. Line
numbers refer to the code as received (branches `main` / `legacy`).
Updated 2026-09-18. Decisions Chris took on 2026-10-14 (to be reviewed by Andria) are marked "Decided 2026-10-14" under their questions; the review behind them is docs/cc/2026-10-14_methodology_decisions_check_codex_gpt6astra.md on the migration branch.

## A. Uncertainty

### A1. The 200 land-cover draws are averaged away before use
**What the code does.** `veg_posts_interp_ice.RDS` holds 200 posterior
draws (`iter`) of each cover fraction for every cell and time slice.
Script 1 (`1_veg_lct_prep.R`, lines 218-222) takes the mean over the 200
draws and passes a single cover value per cell and slice to everything
downstream. From that point on, no land-cover uncertainty exists in the
pipeline: the calibration is fitted to the mean modern field and the
Holocene hindcasts use the mean paleo field.
**Why it matters.** In the meeting you described the intended approach
as "take multiple samples from the posterior and summarise mean and
sd". Today that summary happens only for land cover, and only implicitly,
because the mean is used and the spread is discarded. The albedo and
forcing results therefore carry none of the uncertainty in the
reconstructions.
**Options.** (i) Keep using the mean, and say in the paper that
land-cover uncertainty is not propagated. (ii) Carry a subset of the
draws (for example 20 to 50 of the 200) through prediction: fit the
calibration once on the mean modern field, then run the hindcast once per
draw and pool the results, so the albedo intervals include land-cover
uncertainty. (iii) Both calibration and prediction per draw. Revised
2026-10-07: refitting only the selected model (mod8) per draw is 1-5 min
per month, so about an hour per draw and under two days of wall time for
200 draws on the workstation; (iii) is feasible, but it is a different
claim from (ii), not a bigger version of it.
**What we need from you.** Is (i) acceptable for this paper, or should we
implement (ii)? If (ii), how many draws, and should the calibration also
see the spread?

**(d) Added 2026-10-07, from the design reviews of the ensemble idea.**
Two things only you can tell us, and both must be settled before any
ensemble number is quoted. First: is `iter` a *joint* posterior draw of
the spatial interpolation, i.e. does draw k in one cell, slice and the
modern slice belong to the same realisation as draw k everywhere else?
The file shows `iter` 1-200 complete for every cell-slice-class and every
draw summing to exactly 1, but it cannot show whether draws are joint
across cells. If they are independent per cell, summing forcing across
cells within a draw averages the uncertainty away and the ensemble spread
of the continental total would be spuriously narrow. Second: which
uncertainty claim does the paper make? Land cover only, conditional on
the fitted calibration (option ii, what a single ensemble switch can
deliver), or including the GAM coefficient uncertainty (A2, more code)?

**(e) Added 2026-10-08.** The configuration migration will add a switch,
`land_cover.reduction = "mean" | "median"`, default mean. Two things to
know before asking for a median run. It refits the calibration (about 27
hours) and regenerates everything after it, because the modern slice is
taken after the reduction. And the median path renormalises the three
medians to sum to one per cell and slice, so it is a different estimator
from the vector of marginal medians; the paper must say which was used.
Script 1's diagnostic mode already reports how far the medians sit from
the means, which tells you whether the run is worth making.
Measured 2026-10-14 when the switch was built (issue I04a): before
rescaling, the three medians of a cell and slice sum to between 0.80 and
1.02; after rescaling, median and mean cover differ by 0.012 on average
and by at most 0.16 in a fraction.

**Andria's answer (email to Chris, early October; passed on 2026-10-19).** The mean of the 200 draws is defensible; 200 was probably chosen for quick summary statistics, more would be slightly more defensible but unlikely to change much; the median is not obviously better (worth looking at whether some posteriors are oddly shaped). Not a priority. Uncertainty propagation: "a can of worms", maybe one to open; first work through the whole workflow without uncertainty, then think about where it would enter; pinned to revisit.

### A2. What the albedo "posterior samples" actually are
**What the code does.** Script 6 (`6_prediction_model.R`, line 49) calls
`simulate(model, nsim = 100)` on the fitted GAM. With the gratia package
attached, that function draws new *observations* from the fitted beta
distribution at each cell's fitted mean. The model's coefficients are
held fixed; the uncertainty of the fitted albedo-cover relationship is
never sampled. Every `alb_sd`, `alb_lo`, `alb_hi` and the sd and CV maps
in script 7 are therefore the residual scatter of the calibration data
(its beta dispersion), reproduced at each paleo cell, not the model's
uncertainty. The "fraction of data inside the 95% interval" statistic in
script 5 (lines 206-244) checks that the dispersion parameter was
estimated consistently, not that the model is calibrated.
**Why it matters.** The two kinds of uncertainty behave differently.
Observation noise is the same size everywhere and does not grow when the
model extrapolates; coefficient uncertainty grows where the paleo cover
combinations are far from the modern calibration data, which is exactly
where the early-Holocene results live.
**Options.** (i) Keep observation noise, and describe it as such in the
paper. (ii) Replace it with coefficient draws: sample coefficient vectors
from the fitted model's posterior (gratia's `fitted_samples`, or a
multivariate normal from `vcov(model)` applied to the prediction matrix),
which costs seconds per month. (iii) Add both.
**What we need from you.** Which of these did you intend? The manuscript
says "posterior samples", which readers will take to mean (ii).
**Raised again 2026-10-16** by Fable and Astra, reviewing the spatial-experiment fits: script
5's seven-model figures use the same `simulate()`, so their intervals are also observation
noise only. Note for the options: gratia's `fitted_samples()` draws the fitted mean's
(coefficient) uncertainty, `posterior_samples()` draws both coefficients and observation
noise, and `predicted_samples()` is observation noise only, like `simulate()` (gratia
documentation, read by Astra). Chris asked that this go to you (2026-10-16).

### A3. 100 or 1,000 draws
The manuscript says 1,000 posterior samples per cell and month; the
scripts draw 100. Which is intended? (This interacts with A1 and A2.)

**Decided 2026-10-14 (Chris, to be reviewed by Andria; a config switch, with the original as the legacy setting):** 100 draws for both scripts 5 and 6 while the pipeline is in development; 1,000 for both is possible for the very final results but not decided (more draws reduce Monte Carlo noise; they do not by themselves add model or land-cover uncertainty, which is A1/A2). Revised by Chris the same day.

## B. The calibration model

### B1. The land-cover smooth has one direction with no data in it
**What the code does.** Model 8, the selected model, includes a
three-dimensional smooth `s(OL, ET, ST, bs = "tp", k = 200)`
(`4_calibration_model.R` line 286, and the March version at line 755).
The three fractions always sum to one, so every calibration point lies
on a flat two-dimensional sheet inside the three-dimensional (OL, ET, ST)
space. The smooth has nothing to learn about the direction perpendicular
to that sheet, and `mgcv` reports the fit as rank-deficient: the fitted
March model has rank 444 of 448 coefficients.
**Why it matters.** On the sheet this is harmless. Off the sheet the
smooth is unconstrained, and some paleo rows are off it: script 1
renormalises across all taxa and then drops taxa whose cover class is NA
(`Ericaceae`, `Boraginaceae`, `Plantaginaceae`, `Urticaceae`, lines
54-59), so those rows sum to less than one and are predicted by
extrapolation in the empty direction.
**Options.** (i) Keep the 3-D smooth but renormalise paleo rows to sum to
one before prediction, and check how many rows were affected. (ii) Use a
two-dimensional smooth of two fractions, `s(ET, ST)`, or a compositional
(isometric log-ratio) transform, and refit the ladder.
**What we need from you.** Did you know about the rank deficiency, and do
you have a preference? (ii) changes the model and the results; (i) is a
small correction.

**Decided 2026-10-14 (Chris, to be reviewed by Andria; a config switch, with the original as the legacy setting):** Treated as a check: the three fractions must sum to one before calibration and prediction, and the run stops with a report if not, rather than rescaling silently. (The posterior now read is already three-class; the median option is rescaled by design.)

### B2. Calibration on the interpolated field rather than pollen cells
**What the code does.** The all-months calibration reads
`calibration_modern_lct_interp_bluesky.RDS`, the modern slice of the
interpolated product, so it is fitted to all ~2,900 land cells, of which
only ~500 contain pollen sites. The rest are outputs of the interpolation
model. The draft manuscript still says "estimates are made for grid cells
that contain one or more pollen records".
**Why it matters.** Fitting to interpolated cells raises the apparent
sample size and skill of the spatial term, and smooths the cover
heterogeneity the albedo model is meant to learn from; the model is
partly being fitted to another model's output.
**What we need from you.** Is this the intended design, or should the
calibration be fitted to pollen-bearing cells only and the interpolated
field used for prediction alone? Either way the manuscript text needs to
match.

### B2a. The two flavours fit the spatial smooth in different coordinate systems
**Found 2026-09-19** while running the interp pipeline for the first time.

**What the code does.** Scripts 4, 5 and 6 fit the spatial term as
`s(x, y, bs = "gp", k = 500)`, taking whatever columns are named `x` and
`y`. Those columns mean different things in the two flavours:

| Flavour | `x`, `y` are | Range |
|---|---|---|
| non-interp (`lct_paleo_reveals.RDS`) | Albers equal-area metres, with `long`/`lat` held separately | x -3,103,504 to 3,076,936 |
| interp (`lct_*_reveals_interp.RDS`) | **longitude and latitude in degrees**; no projected columns exist | x -171.5 to -53.5 |

In the interp half of `1_veg_lct_prep.R` the `x`, `y` of
`veg_posts_interp_ice.RDS` are passed straight to `get_elev_point(...,
prj = ll_proj)` (line 228) and never projected, so they stay in degrees.

**Why it matters.** A Gaussian-process smooth measures distance between
points, so the units and the geometry matter. In degrees, distance is
anisotropic and latitude-dependent: one degree of longitude is about
105 km at 25°N but only about 38 km at 70°N. The smooth therefore treats
the far north as horizontally compressed relative to the south, which is
exactly the distortion the Albers projection exists to remove, and the
study's largest Holocene changes are in the north. The fitted
length-scale and the effective degrees of freedom of the spatial term
are not comparable between the two flavours either, which may be part of
why our March non-interp results and the talk's interp results differ
over 4 to 6 ka.

**Options.** (i) Leave as is, and state in the methods that the interp
spatial term is fitted on unprojected coordinates. (ii) Project the
interp cell centres to the same Albers grid the non-interp flavour uses,
before calibration, so both flavours and all the published comparisons
share one coordinate system.

**What we need from you.** Was fitting the interp spatial smooth on
degrees intended, or an oversight when the interpolated product was
swapped in? This changes results, so we have not altered it; the first
interp run reproduces the code as written.

### B2b. Winter albedo is missing across the north: polar night
**Found 2026-09-19** in the first interp calibration table.

**What the data show.** The blue-sky albedo has no value for high-latitude
cells in winter, because there is no incoming sunlight to reflect. In the
interp calibration table (2,860 cells):

| Month | Cells with no albedo | Latitudes affected |
|---|---|---|
| December | 1,109 (39%) | every cell above 60°N, 9% of 50-60°N |
| January | 694 (24%) | above 62.5°N |
| November | 400 (14%) | above 66.5°N |
| February | 95 (3%) | far north only |
| March to October | 33 (1%) | above 76.5°N |

**What the code does with them.** The calibration models are fitted with
`na.action = na.omit`, so those rows are dropped silently: the December
model never sees a cell north of 60°N. The prediction step then applies
that model to all 2,860 cells for every time slice, so December albedo
across the whole north is produced by extrapolating both the spatial
smooth and the land-cover smooth into a region and a vegetation range
the model had no data for.

**Why it matters.** This is the month and the region where the paper's
mechanism is strongest: snow masking by vegetation, at high latitude, in
winter. It is also where the Laurentide ice sheet sat, so the
early-Holocene forcing draws on exactly these extrapolated cells. The
spatial term cancels when consecutive slices are differenced (B3), but
the land-cover smooth does not, and in December that smooth was fitted
only on southern vegetation.

**Options.** (i) Keep, and state that winter high-latitude albedo is
extrapolated. (ii) Restrict the reported forcing to months and cells
with calibration support, for example by masking any cell-month whose
latitude lies outside the fitted range. (iii) Weight the annual forcing
by available sunlight, which is near zero exactly where the data are
missing, so the extrapolation would matter much less.

**What we need from you.** Was the winter data gap known, and how do you
want it handled in the maps and in the annual means? Option (iii) may be
the most defensible physically, since a forcing computed where no
sunlight falls is close to meaningless.

**A statistic this distorts (found 2026-09-23).** Script 5's "fraction
of data inside the 95 % interval" divides by all 2,860 cells, so the
polar-night cells with no observation count as misses. The anchored
table therefore shows 0.56 for December, 0.70 for January and 0.80 for
November against 0.90-0.94 elsewhere; on the cells that have data every
month is about 0.92. The winter models are not worse; the denominator
is. Worth fixing before the number is quoted.

**Considered 2026-10-14 (Chris), not decided:** weighting the annual forcing by sunlight was dropped, because the monthly kernels already include sunlight and it would count it twice. How to treat the extrapolated winter cells remains your call.

**Added 2026-10-15 (Astra's review of 2026-10-15, docs/cc/2026-10-15_snow_and_water_masking_codex_gpt6astra.md on the migration branch).** Two refinements. Gaps south of the Arctic Circle cannot all be
polar night: a low sun, the retrieval's thresholds and cloud filtering also remove values,
and the product's quality flags would show which. And a large winter albedo error need not
give a large forcing error, because little sunlight arrives then; snow lingering into
spring (March to May in the boreal zone, May and June at northern and mountain margins)
matters more. We checked one consequence in the September run: every cell has all 12
months, dark months enter as zero forcing, not as missing, so the annual mean in script 9
always divides by 12.

### B2c. How snow is treated in the satellite albedo
**Raised 2026-10-15** while checking Oleson et al. (2003, GRL, doi:10.1029/2002GL016749), a
reference for the land-only albedo. They note that the MODIS albedo product of their time
"tends toward a snow-free albedo": when most observations in a 16-day window were snow-free,
the snowy ones were discarded, so in places with intermittent snow the satellite albedo is
lower than the albedo averaged over all days. Their product (MOD43B3, collection 4) is older
than ours (MCD43A3 v061, with ERA5), whose snow handling differs; we have not checked how.
**Why it matters.** Winter and spring albedo in cells with patchy or ephemeral snow sets
the cold-season part of the calibration, and so the forcing in those months.
**What we need from you.** Does the blue-sky albedo product keep snow-covered retrievals
(and in what proportion), or lean towards snow-free as described? Was this considered when
it was built?

**Added 2026-10-15 (Astra's review of 2026-10-15, docs/cc/2026-10-15_snow_and_water_masking_codex_gpt6astra.md on the migration branch).** Oleson's warning should not be carried over unchanged:
from Collection 6 the snow state is that of the target day, not the majority of the 16-day
window. Clouds and filtering can still drop short snow spells and melt. "Blue-sky" means the
mix of direct and diffuse light, not snow-free. A new finding: all twelve bands of
`blue_sky_monthly_2000-2009.tif` are labelled `median` in the file's metadata, while the
README says mean; the label may be inherited, but a median and a mean treat intermittent
snow differently. Nothing in the file confirms the band order either: script 2 assigns
months by position, January first (methods-from-code pilot, 2026-10-15). **What we need, in addition:** the code or a description of how the
monthly raster was built: who made it, mean or median over days and years, month order,
snow and quality filters, gap filling, how water pixels were treated, and how ERA5 enters.

**Decided 2026-10-15 (Chris):** ask you for any surviving code (or the account where the
raster was made), and meanwhile design a rebuild from the public MODIS and ERA5 data,
following published methods, with water removed at the satellite's 500 m scale using
MOD44W, downloaded and processed on our workstation. Your raster stays as the legacy
setting and the two are compared. Nothing is built until Chris approves the design.

**Decided 2026-10-15 (Chris), after two literature reviews and Fable's comparison:** the
calibration albedo will be our own build from MODIS MCD43A3/A2 v6.1 with the ERA5 all-sky
diffuse fraction, 2000-2009, water removed at 500 m; Jia et al. (2022, doi:10.1029/2021JD035987)
is kept as a cross-check, not the primary response, because it is a clear-sky product while the
radiative kernels are all-sky. Settings, each following published practice (Chris, 2026-10-15):
full-inversion retrievals only, as NASA's product guidance recommends, with all processed
retrievals as a sensitivity (Wang et al. 2018); no minimum coverage, every value kept with
its count of valid days, as the published climatologies do (Jia et al. 2022; Duveiller et
al. 2018); water removed by keeping only the 500 m pixels that the product's own land/water
class (MCD43A2 `BRDF_Albedo_LandWaterType`) labels land, as NASA's MCD43 processing does.

### B2d. What the paper claims about past snow
**Raised 2026-10-15** by Chris ("snow is perhaps the weakest point"), reviewed in Astra's review of 2026-10-15, docs/cc/2026-10-15_snow_and_water_masking_codex_gpt6astra.md on the migration branch.
**What the code does.** The calibration models have location, elevation and the three cover
fractions, and no snow or climate term. Prediction applies today's relation between cover
and albedo to past cover. So past snow seasons are in effect today's, and the snow-masking
effect of forest is today's.
**Why it matters.** Snow makes open ground bright and forest hides it, so the albedo gap
between forest and open land grows with snow. A snowier or less snowy past would change the
forcing of the same cover change; the sign of the overall bias is unknown. Near the
retreating ice sheets, land newly exposed is given today's snow, which may be too little.
**Options** (Astra's ranking, cheapest first). (i) State the result as land-cover effects
under modern climatic relationships, with monthly and regional plots of where the forcing
comes from, and stress tests that shift snow fraction and melt timing (for example by 0.1
and 0.2), using snow-free and snowy albedo look-up maps (Gao et al. 2014); widen the work
only if the conclusions change. (ii) Separate snowy and snow-free calibrations. (iii) Snow
from paleo climate simulations: PMIP 6 ka, or transient runs (TraCE; HadCM3 as in Hopcroft
et al. 2026, doi:10.1029/2025GL118518, on which you are a coauthor; iTRACE). (iv) A
degree-day snow model. (v) Report only snow-free months as the headline.
**What we need from you.** Which claim the paper makes: land-cover effects under modern
climate, or a reconstruction of past albedo including changing snow? And can monthly snow
fields (cover or water equivalent) from the HadCM3 runs be shared?

**Decided 2026-10-15 (Chris, to be reviewed by Andria):** do the simplest analysis first, in
two steps: (1) a monthly and regional breakdown of where the forcing comes from, from the
existing outputs; (2) a sensitivity test that shifts snow cover and melt timing a little
from today's, using snowy and snow-free albedo by cover type. What the paper finally
claims stays your call.

**Step 1 done 2026-10-16** (script 10 on the migration branch, reviewed by Fable and Astra): the
forcing broken down by month and latitude band. On the original settings the 6-12 ka forcing
comes mostly from May to September at 50-70 N (ice-sheet retreat), the last 4,000 years mostly
from spring; November to March add little in every period. The headline variant mixes
vegetation change and ice retreat, so this locates the combined forcing and says nothing by
itself about snow; and land newly freed of ice gets today's vegetation albedo, so the snow
assumption touches ice retreat too.

**Decided 2026-10-16 (Chris, to be reviewed by Andria):** a simple estimate of the snow effect
comes first: calibrate the whole pipeline on snow-free albedo (the snow-free version from our
MODIS rebuild, by the product's own snow flag) and compare with the all-days calibration; the
difference shows how much of the forcing depends on snow being there (today's snow against
none, not past snow). Northern winter months have few or no snow-free days and drop out of
that run (valid-day counts are kept). The small shift in snow cover and melt timing is kept as
a second test. Built after the MODIS rebuild, once checked against published practice.

**Literature check 2026-10-16 (Fable) and Chris's choice the same day.** Splitting MODIS
albedo into snow-free and snowy retrievals with the MCD43A2 snow flag is standard (Moody et al.
2005, 2007; Gao et al. 2014; NASA's own snow-free products are made so). No study ran the exact
"all-days minus snow-free" comparison; the nearest precedent is Barnes & Roy (2008, GRL
35:L09706, snow-free albedo only; 2010, JGR 115:G04033, snow and snow-free albedo, snow
"exacerbated" the forcing). The established form mixes snow-free and snowy relations by a
snow-cover fraction (Barnes & Roy 2010; Ghimire et al. 2014; Jiao et al. 2017, Climatic Change
142:463). Offered both, **Chris chose the two runs** (all-days against snow-free); the
snow-shift test stays separate, its method to be set later. Open: northern winter has few
snow-free retrievals; published practice fills from each cover type's seasonal curve (NASA
MCD43GF), restricts to full inversions, or drops the month (Bright et al. 2015, Biogeosciences
12:2195, dropped December). To be decided once the build shows the counts. Report:
docs/cc/2026-10-16_snow_free_estimate_literature_fable.md on the migration branch.

**Decided 2026-10-19 (Chris, to be reviewed by Andria; replaces the two runs of 16 October).**
Calibrate twice, on snowy and on snow-free MODIS albedo (the product's snow flag), and mix the
two predictions by a snow share per cell and month: albedo = share x snowy + (1 - share) x
snow-free, the established form (Barnes & Roy 2010; Ghimire et al. 2014; Jiao et al. 2017).
It contains the two runs as cases: share 0 is the snow-free run; today's share should come close
to the all-days calibration (exact for the data, close but not identical for two fitted models;
the gap is measured as a check). Then vary the share away from today's (amount and melt timing)
as the snow-shift test, without refitting. Where a cell-month has no snow-free days the share is
1, so the poorly known snow-free value gets no weight. Built after the MODIS build, with a
literature check first of the shift sizes and of one caveat: the satellite sees less snow under
forest canopy, so today's share partly reflects today's forest, not only today's climate.
Past snow from climate-model runs (the HadCM3 question above) would be the full version.

### B3. The spatial term cancels through time and may have absorbed the cover effect
**What the code does.** Every prediction uses the same location and
elevation values for a cell in every time slice; only the cover fractions
change. On the link scale the location and elevation terms therefore
cancel in every slice-to-slice difference, and the entire Holocene albedo
signal comes from the shape of the cover smooth alone. That smooth was
fitted alongside a very flexible spatial surface (`s(x, y, bs = "gp",
k = 500)`) on the same modern data.
**Why it matters.** Cover and location are strongly correlated in the
modern data (forest in the east, prairie in the middle, tundra in the
north). A flexible spatial term can take credit for part of the true
cover effect, leaving the cover smooth biased towards zero and the
Holocene forcing understated. The "spatial effects experiment" in scripts
4 and 5 compares fits, not the size of the cover effect.
**What we need from you.** Was this examined, for example by comparing
the cover effect with and without the spatial term, or by spatially
blocked cross-validation? If not, would you like us to?

**Correction added 2026-10-15 (Astra's review of 2026-10-15, docs/cc/2026-10-15_snow_and_water_masking_codex_gpt6astra.md on the migration branch).** The location and elevation terms cancel on the
logit scale only. Differences are taken after converting back to albedo, which is
nonlinear, so they do not cancel exactly. And the spatial term could make the cover effect
too small or too large; it is not necessarily biased towards zero. Spatially blocked
cross-validation or a fit without the spatial term would show which.
**Raised again 2026-10-16** by Fable and Astra: the seven spatial-experiment figures of script 5
judge each model on the cells it was fitted to, so they show fit, not skill at predicting cells
the model has not seen, which is what a hindcast does. Spatially blocked cross-validation needs
no new data (the map is split into blocks; each block is predicted from a fit to the rest), but
each block means refitting the models (script 4's full set of fits took about 27 hours on 8
cores). No observations of past albedo exist; the partial independent checks are E1 and the
modern-slice test. Chris asked that this go to you (2026-10-16): **would you like the models
tested by spatially blocked cross-validation?**

### B4. Model selection: model 8 is fixed by hand, but the data support it
**Updated 2026-09-20 with the interp AIC table**, which script 4 produced
overnight for all twelve months (`output/calibration/AIC_table.csv`).

**What the code does.** Script 4 prints an AIC table and analysis of
deviance for models 1 to 8 (lines 315-335), but script 5 (line 28) loads
model 8 as the selected model for every month unconditionally.

**What the interp data show.** The hard-coding turns out to encode the
right answer. Model 8 has the lowest AIC in 11 of 12 months, losing
narrowly to model 7 in May, and is never the worst. AIC falls from model 1 to
model 8 in every month, though not at every step:

| | jan | apr | jul | oct | dec |
|---|---|---|---|---|---|
| model 1 | -6583 | -8904 | -12642 | -9449 | -6027 |
| model 8 | -6669 | -9503 | -13032 | -10405 | -6127 |

This supersedes the worry raised from the non-interp May run, where AIC
rose with complexity in a way correctly fitted nested models cannot. On
the interp data the behaviour is orderly.
(**Corrected 2026-10-16:** "in a way correctly fitted nested models cannot" is wrong for AIC,
which penalises each added degree of freedom and can rise when a term is added; only the
deviance cannot. Found by Astra.)

**Correction 2026-10-15.** An earlier version of this entry said AIC "decreases
monotonically with model complexity in every month". It does not: in the same table AIC
rises by a few units at three steps, model 6 to 7 in April (-9500 to -9497), model 7 to 8
in May (-10202 to -10199) and model 5 to 6 in June (-11796 to -11790). The overall fall from
model 1 to model 8 holds in every month. Found by the methods-from-code pilot.

**Decided 2026-10-15 (Chris):** the seven spatial experiments of script 4 follow the same
comparison setting as the ladder (ML in the current configuration, REML in the legacy one),
for the same reason: REML scores are not comparable between models with different fixed
terms (mgcv 1.9.4 documentation, `gam.selection` and `AIC.gam`; Wood 2017, doi:10.1201/9781315370279;
Zuur et al. 2009, doi:10.1007/978-0-387-87458-6).
**Correction 2026-10-16 (Claude), decision reopened, then settled the same day (below):** the question put to Chris said the
spatial experiments are compared by AIC. They are not, in the original code or now: no AIC is
computed for them anywhere. Script 5 only draws simulated predictions from them for the
predicted-against-observed figures and error histograms (the off-pipeline
`6_prediction_model_spatial_eval.R` also predicts from them). The reason given for ML does
not apply, so the change (branch spatial-ml) is held unmerged and the question goes back to
Chris.

**What is left of the question.** A formal one: the models are fitted by
REML and compared across differing mean structures, which is not a valid
likelihood comparison (mgcv's own guidance is to use `method = "ML"`
when comparing models that differ in their terms), and models 6 to 8 are
rank deficient because the cover fractions sum to one, so the penalty
term is ambiguous. A referee could object. Given how consistent and
monotone the ordering is, refitting by ML would very likely confirm
model 8 rather than overturn it, so this is tidying rather than a
threat. **Would you like the ladder refitted with `method = "ML"` so the
selection can be defended as stated?**
(**Corrected 2026-10-16**, after independent reviews by Fable and Astra: the paragraph above
misdescribes the AIC. What cannot be compared across models with different terms is the REML
*score*. The AIC mgcv computes is the conditional AIC (likelihood of the data at the fitted
coefficients, with the corrected degrees of freedom of Wood, Pya & Saefken 2016, JASA
111:1548, doi:10.1080/01621459.2016.1180986; `logLik.gam` help), which is valid on REML fits
too, and Wood's own help examples compare REML fits with it. Checked on the original fits:
the eight ladder models use identical rows and carry the correction. So ML is the usual
convention for comparing models that differ in their terms (mgcv's `gam.selection` help),
not a requirement. The 15 October citations of Wood 2017 and Zuur et al. 2009 were not
checked against the books' text.)

**Decided 2026-10-14 (Chris, to be reviewed by Andria; a config switch, with the original as the legacy setting):** Compare the ladder after fitting by ML; fit the model used for prediction by REML; check rank and fit diagnostics before selection.

**Found 2026-10-15, January only (the ML ladder of issue I16, one month so far).** The
expectation above, that ML would confirm model 8, does not hold for January: refitted by
ML, model 7 (a Gaussian-process smooth of the three cover fractions) has the lower AIC.

| January AIC | model 7 | model 8 | lower |
|---|---|---|---|
| REML, all cells (the original) | -6656 | -6669 | model 8, by 13 |
| ML, all cells | -6646 | -6633 | model 7, by 13 |
| ML, present-day ice caps left out (the new configuration) | -6615 | -6608 | model 7, by 7 |

So the switch to ML is what reverses the order; leaving out the ice caps narrows it. Model 8
remains the configured model (`analysis$calibration$model_id`). The other eleven months by
ML come with the final full run (27 hours). **If model 7 wins in most months by ML, should
the prediction model change to model 7, or stay model 8 for continuity with your earlier
results?**

**Decided 2026-10-16 (Chris, to be reviewed by Andria),** after independent reviews by Fable
and Astra (filed under `docs/cc/` on the migration branch):
- The seven spatial experiments stay REML, the original, in both configurations: nothing
  compares them by likelihood; they are only simulated (script 5) and predicted
  (`6_prediction_model_spatial_eval.R`, against the REML prediction model). The ML change was
  deleted unmerged.
- The ladder stays ML in the current configuration (a setting; REML in the legacy one), and
  **both ladders are to be compared** (January above already differs).
- **The ladder is compared by AIC only:** the chi-square analysis of deviance is removed from
  scripts 4 and 5 (nothing saved or read it), because models 5 to 8 are not nested (model 6
  replaces three smooths by one; models 7 and 8 change its basis), so those tests did not test
  what they appeared to. **For your review.**
- Recorded, not changed: the spatial experiment "elevation and cover with location" is model
  8 fitted a second time; on the original files the two fits are identical (same formula
  and rows; fitted values differ by 0). See general question 7.

**Found 2026-10-18, during the full run of the current configuration (ML ladder, all twelve
months).** Model 7 often stops at the iteration limit without converging, and the
"converged: TRUE" line that script 4 prints after every fit does not show it: mgcv's `bam()`
sets `converged` to TRUE whatever happens (mgcv 1.9.4 source); the real signal is the warning
"algorithm did not converge" from `bgam.fit`, printed only after the loop, so without saying
which fit. Counting the iterations of the saved fits (limit 500, `CALIBRATION_CONTROL` in
`R/model_registry.R`), over the twelve months:

| | months |
|---|---|
| model 7 at the limit, not converged | feb, mar, apr, jun, sep, oct, dec |
| model 7 converged, 147 to 365 iterations | jan, may, jul, aug, nov |
| models 1 to 6 and 8 | all converged, under 100 iterations |

So the January result above (model 7 lower by ML) rests on a converged fit, but in the
months where model 7 did not converge its AIC, and so the 7-against-8 comparison, is not
reliable. The prediction model (model 8 refitted by REML) is not affected. The ML ranking over all twelve months
(the four parts' `AIC_table.csv` combined): model 8 lowest in nine months, model 7 in January,
May and December, and of those only January and May rest on converged fits. This also means
the "fit diagnostics" in the 14 October decision are not yet checked: the convergence report
is empty. Chris (2026-10-18): note it, let the run finish, then propose a fix that reports
convergence from mgcv's real signal. **Does model 7 (a Gaussian-process smooth of the three
cover fractions) need to stay in the ladder, and if so, should it get more iterations or a
different set-up rather than be compared unconverged?**

### B4a. Land cover matters most in autumn, least in midwinter
**Found 2026-09-20 in the interp AIC table.**

The improvement from adding land cover to the model varies strongly by
season. Taking the AIC spread across the ladder within each month as a
measure of how much the cover terms buy:

| jan | feb | mar | apr | may | jun | jul | aug | sep | oct | nov | dec |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 86 | 167 | 331 | 599 | 798 | 486 | 390 | 461 | 891 | 956 | 370 | 100 |

Land cover explains most in September and October, and least in December
and January, by roughly a factor of ten.

**Why it matters.** The paper's mechanism is snow masking: vegetation
changes albedo most where it hides or exposes snow, which should make
winter the season where cover matters most. The data say the opposite.
Two candidate explanations, which point in different directions:

- *Physical.* In midwinter at these latitudes there is little sunlight
  and deep snow cover everywhere, so albedo is high and uniform whatever
  the vegetation; the shoulder seasons, with partial snow and active
  canopy, are when cover discriminates.
- *Artefactual.* Midwinter is exactly when the polar-night gap removes
  the northern cells (B2b): the December model is fitted only south of
  60°N, so the cells where snow masking is strongest are absent from the
  fit. The weak winter signal may be a sampling artefact rather than a
  physical result.

**What we need from you.** Which do you think it is, and is this
seasonality something the paper should report? If it is the second, it
strengthens the case for handling the winter gap explicitly rather than
extrapolating into it.

### B5. Calibration response: centre pixel or cell mean
**What the code does.** Script 2 computes both the native 0.25-degree
albedo at the 1-degree cell centre and the 1-degree cell mean
(`_coarse`), and the calibration uses the centre pixel (lines 190 and
195; script 4 line 17). The predictors are 1-degree cell means.
**Why it matters.** A centre pixel over a lake, a city or a clear-cut is
attributed to the whole cell's cover.
**What we need from you.** Was the centre pixel a deliberate choice?

**Added 2026-09-23, with the specifics.** Script 2 builds the calibration table by sampling the 0.25-degree albedo
raster at each 1-degree cell's centre point (`terra::extract` at a
point returns the pixel under it). So every row of the table pairs a
land-cover fraction that describes the whole 1-degree cell with the
albedo of one quarter-degree pixel, a sixteenth of the cell. The
cell-average albedo is computed in the same script (`resample(method =
"average")`, saved as `..._coarse.RDS`) but is only used for a
diagnostic scatter; script 4 fits to the centre-pixel table. The code
review of 2026-09-18 noted this; recording it here so it reaches you.

Was the centre-pixel choice deliberate, for instance to avoid averaging
in water or ice pixels at coasts and lake margins, and have you tried
fitting to the cell average instead? If the two calibrations differ, the
cell average is the like-for-like match to the 1-degree predictors.

**Decided 2026-10-14 (Chris, to be reviewed by Andria; a config switch, with the original as the legacy setting):** Calibrate on the mean albedo over the land part of each 1-degree cell, with inland water (lakes, Hudson Bay) masked with MODIS MOD44W, instead of the centre pixel. **Still open:** the mask's version and year (v6.1, 2015 annual layer proposed), the DEM product and resolution for B9 (about 1 km via elevatr proposed), and whether a cell-month needs a minimum observed land coverage to enter the calibration (the original code has none; 50% was suggested, not decided).

**Added 2026-10-15 (Astra's review of 2026-10-15, docs/cc/2026-10-15_snow_and_water_masking_codex_gpt6astra.md on the migration branch).** A 250 m water mask cannot remove water already averaged into a
0.25-degree albedo pixel: half land at 0.20 and half water at 0.05 gives 0.125, and masking
afterwards still leaves 0.125. Weighting the 0.25-degree pixels by their land fraction is
therefore an approximation that lowers, but does not remove, the influence of water. The
exact route is to screen water at the satellite's native scale (500 m) before averaging,
which needs the code that built the raster (B2c). **What we need from you:** do the
0.25-degree values already average land retrievals only? Frozen lakes would stay excluded
in winter, so lake ice and snow on lakes are outside the analysis; a stated limitation.

**Andria's answer (email to Chris, early October; passed on 2026-10-19).** A cell average is probably better than the centre value; if easy, fix it. (Done for elevation; the MODIS rebuild does it for albedo.)

### B6. Albedo values of exactly zero are set to 0.0001
*Checked 2026-09-19: no cell-month in the interp calibration table hits
this replacement, so it affects the non-interp flavour only.*
Script 2 (line 388) replaces zeros with 0.0001 before fitting. On the
logit scale that is about -9, a very influential value for a beta model.
Where do the zeros come from (water, fill values, failed retrievals), and
should those cells be dropped instead?

### B7. Twelve independent monthly models
You said in the meeting that a model with within-year structure would be
better but is out of scope. Should the paper state that "consistent
pattern across months" is an observation rather than a constraint of the
model?


### B8. Present-day ice caps sit in the modern calibration as vegetated cells
The `ice` flag in `veg_posts_interp_ice.RDS` is applied only to slices
of 1,000 BP and older: at 50, 200 and 500 BP no cell is flagged, although
the 1 ka ice polygons contain 48, 35 and 48 of those slices' cells
respectively (point-in-polygon test, 2026-09-25). So 48 of the 2,860
modern cells are present-day ice caps (20 in Alaska west of 100 W, 10
in the Canadian Arctic islands, 18 on Baffin and Labrador), and they
enter the calibration table with land cover assigned by the
interpolation (mean ET 0.30, OL 0.49, ST 0.21) and the satellite's ice
albedo: July mean 0.36 against 0.13 for everything else, April 0.69
against 0.38. They are 1.7 % of the cells but carry the highest albedos
in the dataset and an open-land fraction that describes rock and ice,
not vegetation.

Was leaving them in deliberate? If not, excluding them (or masking with
the 1 ka polygons, which the flag already uses for older slices) is a
one-line change in script 2 that could shift the fitted open-land
albedo, which is the term the whole hindcast rests on.

**Decided 2026-10-14 (Chris, to be reviewed by Andria; a config switch, with the original as the legacy setting):** Exclude cells whose centre lies inside the 1 ka ice polygons (48) from the calibration; they stay in the prediction domain. The 1 ka mask is a proxy for recent ice.

**Andria's answer (email to Chris, early October; passed on 2026-10-19).** The most recent ice outline is for 1 ka. She used it for the younger slices for a while, then was advised to assume no ice there; worth a literature search or expert ask, since the ice-to-vegetation transition matters for the forcing. Found 2026-10-19: the land-cover product has no ice at 50, 200 and 500 BP, but the Dalton ice fraction used for the forcing keeps the 1 ka ice to 50 BP (in July, 156 cells with some ice and 28 above 50% at 50, 500, 1,000 and 1,500 BP; 135 and 17 at 200 BP, where the three northern rows are absent). The forcing is unaffected (no ice change from 1.5 ka to 50 BP; the 200 BP dip cancels within its period), but the two sources disagree on today's ice.

### B9. Elevation is a single point at the cell centre, at an unstated resolution
The original elevation (now the legacy setting; its table is made by
`tools/prepare_centre_elevation.R`) comes from `elevatr::get_elev_point()` at each cell's
centre coordinate: one spot in a cell 70-100 km across, against land
cover that is a cell average. In flat country it hardly matters; in the
Rockies or the Mexican highlands (the table reaches 3,492 m) the centre
point can sit hundreds of metres from the cell mean, and for 
coastal cells whose centre falls offshore the service returns the sea
floor (the table's minimum is -28 m). The tile resolution queried is
elevatr's default (`z = NULL`, version 0.99.1) and is nowhere stated.
This is the same point-versus-cell mismatch as B5, applied to the second
predictor. Was a cell-mean elevation (e.g. from a DEM averaged over the
cell) considered, and should offshore centres be clipped to zero?

**Decided 2026-10-14 (Chris, to be reviewed by Andria; a config switch, with the original as the legacy setting):** Elevation is the DEM mean over the land part of each cell (water excluded), instead of the centre point.

**Amended 2026-10-15 (Chris), after a literature check:** the mean excludes the **ocean**
only; lakes stay in. This follows the two verified precedents, which average elevation over
a cell's non-ocean pixels and keep lake surfaces: New et al. (2002), Climate Research 21:1-25,
doi:10.3354/cr021001, p. 7 (each land cell's elevation is the average of 100-400 GLOBE 1 km
points); ISLSCP II HYDRO1k elevation products (Verdin 2011, doi:10.3334/ORNLDAAC/1007, sec.
9.2.1: "mean elevation value within each cell" over "the non-ocean pixels"). No precedent was
found for removing lakes from elevation, so the albedo (lakes removed, after Moody et al.
2005 and Helbig et al. 2016) and the elevation differ on that point. Record of the check:
docs/cc/2026-10-15_land_mean_elevation_precedents.md on the migration branch.

**Amended again 2026-10-15 (Chris), after the two independent reviews:** the sea is GMTED2010's own
0 m pixels, as New et al. took their elevation model's own land points, rather than MOD44W's ocean
class, which starts about 1 km offshore and labels Nettilling Lake as ocean. 116 coastal cells rise
(median 12.7 m); four eastern Baffin Island cells, where GMTED does not resolve the fjords, fall.

**Andria's answer (email to Chris, early October; passed on 2026-10-19).** A cell average is probably better than the centre point; if easy, fix it. (Done: land-mean elevation.)

### B10. The "modern" land cover is nominally AD 1900; the albedo is 2000-2009
The calibration pairs the land cover of the 50 BP slice with a satellite
albedo climatology for 2000-2009. If 50 BP means what it usually means
(before 1950, so about AD 1900), the predictors describe a landscape a
century older than the response, and that century saw the largest land
use changes in the record over much of the domain: agricultural
expansion and later abandonment, forest regrowth in the east, fire
suppression in the west. If instead the 50 BP slice is a bin that reaches
to the present (say 150 BP to -60 BP), the mismatch is small. What does
the slice actually cover, and was the pairing with 2000-2009 albedo
considered acceptable on that basis?

**Added 2026-10-15 (Fable's review, docs/cc/2026-10-15_albedo_route_and_modern_slice_gap_fable.md
on the migration branch).** Dawson et al. (2025), Clim. Past 21:2031, doi:10.5194/cp-21-2031-2025,
Sect. 2.3, define the youngest interval as 0.1 to -0.074 ka, AD 1850 to 2024, following the
LandCover6k protocol (Githumbi et al. 2022); the file's 25 ages match the paper's intervals. If
so, the gap is a blurred bin (clearance peak and regrowth averaged together) rather than a
century's offset. The posterior shows the clearance: open land in the north-eastern US is 0.16
at 50 BP against 0.07-0.09 before 1600, Midwest 0.39 against 0.28-0.31; Plains, Pacific
north-west and boreal unchanged. Every published albedo-change study found calibrates on land
cover from the satellite years. Two cheap tests would show whether the blur matters: refit on
cells with little land-use change since 1850 (HYDE 3.2), and compare the 50 BP fractions with
satellite land cover (MODIS MCD12). **What we need from you:**
1. Is the 50 BP slice the AD 1850-2024 interval of Dawson et al. 2025, with pollen counts
   summed per record within it, and are the other labels the paper's bins?
2. Within that interval, how are records weighted: do most contribute many twentieth-century
   samples (core tops), or one or two near 1850-1900? Is there a per-record sample count?
3. Was the pairing of this window with 2000-2009 albedo considered, and has the group compared
   the youngest REVEALS window with satellite land cover for North America?
4. Would a modern-pollen REVEALS layer be feasible as the calibration land cover?
5. If the tests show the blur matters, would you prefer restricting the calibration to
   low-land-use-change cells, or re-pairing on satellite land cover?

**Chris, 2026-10-15:** the two tests wait for your answers to these questions.


## C. Differencing, ice and forcing

### C0. One ice chronology, plus a second one only for the fraction
**Checked 2026-09-19.** An earlier draft of this question worried that
the pipeline held three unreconciled ice definitions. Testing showed
that two of them are the same thing:

| Source | Form | Used by | In the repo? |
|---|---|---|---|
| `ice` flag in `veg_posts_interp_ice.RDS` | binary, per cell and slice | the vegetation interpolation, to avoid inventing vegetation under ice | yes |
| `map-data/ice/glacier_shapefiles_21-1k.RDS` | 21 polygon sets, 1,000-year steps | `7_plot_preds.R`, for overlays and ICE status | yes |
| `Dalton_QSR_2020_Ice/dalton_interpolated_LC6k.tif` | continuous ice **fraction** | `7a_alb_diff_full.R`, for the vegetation/ice albedo mixing | **no** |

A point-in-polygon test of every cell against the shapefiles reproduces
the `ice` flag exactly at every age tried: 69 of 69 cells at 6 ka, 213
of 213 at 8 ka, 596 of 596 at 10 ka, 780 of 780 at 11 ka, with no
disagreement either way. So the flag was derived from these shapefiles,
and the pipeline really has **one** ice chronology, used consistently
for the vegetation mask and for the maps.

The only reason a second product appears is that script 7a needs a
*fraction* per cell, to mix vegetation and ice albedo by area, whereas
the shapefiles give a yes or no. Dalton supplies a fraction.

**What we need from you.** Could the fraction be computed from the ice
polygons we already have, by rasterising them onto the 1-degree grid
with fractional coverage? That would give the whole pipeline a single
source of truth for where the ice was, and remove the missing Dalton
file from the critical path. The trade-off is chronology: Dalton et al.
2020 is an update to the older reconstruction these polygons appear to
come from, so using the polygons means using the older margins
throughout. Which would you prefer: one consistent older chronology, or
the newer Dalton fraction alongside the older mask and overlays?

One related point either way: `1_veg_lct_prep.R` drops the `ice` column
when it aggregates, so the flag never reaches the predictions and the
pipeline picks ice up again later from a different file. Should it be
carried through?

**A coverage note (2026-09-25).** The flag itself is consistent: at every
slice it equals the cells present in the product that fall inside the
nearest polygon set (ties to the younger). Where the counts look odd it
is the product's cell coverage: at 4,500 BP only 41 cells are flagged
against 55 at 4,000 because 14 cells that exist at 4,000 are absent from
the product at 4,500. That is the 1,814 missing cell-slices (2.5 %) of
script 1's header showing through; which cells drop out at which
slices, and why, has not been characterised.

**The two sources disagree at the modern slice (2026-09-25).** At 50 BP
script 7's polygon test flags 48 cells as ice while script 7a's Dalton
raster puts 28 cells over half ice (156 with any ice at all). Both are
"today", so this is a direct measure of how far the two chronologies
differ, independent of any dating question.

### C0a. `albedo_glacier_monthly.csv` cannot be sourced; what should the values be?
The code needs twelve rows with the columns `month`, `ice_albedo_fixed`
and `ice_albedo_sc` (`7a:264-268`), plus `ice_albedo` for
`7_plot_preds.R` (`7:258-262`). This is not a published dataset, it is a
set of chosen values, so there is nothing for us to download.

Its influence is large: it sets the albedo of every ice-covered cell, so
it drives the deglaciation forcing that dominates 12 to 8 ka.

**What we need from you.** The file if you have it, or the values and
where they came from. Failing that, may we derive them from the
blue-sky albedo product over present-day ice and document the choice?
That would at least be internally consistent, same product and same
months as the calibration. We would also need to know what distinguishes
`ice_albedo_fixed` from `ice_albedo_sc` ("scaled" by what?).

**Sensitivity measured 2026-10-19** (Chris asked whether 0.68 is too high). The headline uses
`ice_albedo_fixed`, 0.68 in every month (`ice_albedo` 0.6-0.8 is read only by script 7's maps;
`ice_albedo_sc` 0.56-0.80 by the scaled sensitivity variant). The threshold forcing is linear in the
glacier value, so it was recomputed exactly from the full run of 2026-10-18 (study-area mean,
HadGEM3 all-sky, W/m²): 8-10 ka 2.50 / 3.83 / 4.90 / 6.51 for glacier albedo 0.50 / 0.60 / 0.68 /
0.80; 10-12 ka 3.12 / 3.76 / 4.27 / 5.05; 6-8 ka 0.27 / 0.70 / 1.05 / 1.57. Bare ice at a summer
ice-sheet margin is often quoted well below 0.68 (to be checked against the literature). Also in the
Word document.

### C0b. The Dalton raster: can we reconstruct it?
`dalton_interpolated_LC6k.tif` is not Dalton et al. 2020's published
data; it is their ice margins interpolated to the LandCover6k time
slices and rasterised to a per-cell fraction, a derived product specific
to this project. The originals are behind the journal paywall with no
open repository we could find, and the interpolation method is not
recorded anywhere in the repository.

**What we need from you.** The file itself, or failing that the script
that made it. Searched 2026-09-19: Dalton et al. 2020's margins are not
deposited in any open repository we can find and the paper is
paywalled, so we cannot rebuild the file independently. And because
`dalton_interpolated_LC6k.tif` is not Dalton's published data but their
margins interpolated to the LandCover6k slices by someone on the
project, even the original shapefiles would leave us guessing at the
resampling. **This file is the only route to reproducing the paper's
forcing numbers**; everything below is a substitution that changes
results.

*Substitutes, in case the file is gone.* (i) Rasterise the ice polygons
already in the repository to a fractional coverage (see C0): no new data
needed, and it gives the pipeline a single ice chronology, but it is the
older reconstruction. (ii) NADI-1, Dalton et al. 2023, is openly
available on Zenodo (doi:10.5281/zenodo.8161764, CC-BY-4.0, 147
shapefiles): a newer chronology at 500-year steps from 25 to 1 ka, which
happens to match this project's slice spacing so it would need little
interpolation, and it ships minimum and maximum margins alongside the
optimal one. That last point is worth noting even if we do not switch
now: the pipeline currently carries no ice-extent uncertainty at all,
which your EGU speaker notes acknowledge, and NADI-1 would let that be
quantified rather than just stated. Either substitute means the paper
must cite the chronology actually used.

### C1. Which ice representation the paper reports
**What the code does.** Script 7a computes two representations of ice in
each cell and slice: a threshold (cell is ice if the Dalton ice fraction
exceeds 0.5) and an area-weighted mix of vegetation and ice albedo. Each
is computed with a fixed glacier albedo and with a scaled one, and script
8 computes forcing for every combination.
**What we need from you.** Which combination do the talk and paper
report, and how different are the continental totals between them?

**Measured 2026-10-19** (full run of 2026-10-18, commit 4ba4d2a; study-area mean, HadGEM3 all-sky,
W/m²; from `forcing_by_period_variant_sensitivity.csv`), threshold against area-weighted mix, fixed
glacier albedo: 6-8 ka 1.05 against 1.10; 8-10 ka 4.90 against 4.55 (-7%); 10-12 ka 4.27 against
4.43 (+4%); after 6 ka within about 0.05. Chris asked (2026-10-19) whether the ice should be
counted by fraction, like the water masking, rather than all-or-nothing at 50%.
**Arithmetic checked 2026-10-19** (read-only, hand-made cells through `pair_albedo_differences()`,
scratch script): the mixed variant equals the exact change of f·ice + (1-f)·vegetation albedo for
retreat, advance, partial and full cases, to rounding. Algebraically its error is f_young·(ice
albedo old − ice albedo young), which is zero because the glacier albedo depends on the month
only. It uses the readvance-adjusted fractions, so question C2 applies to it as to the threshold.

**Andria's answer (email to Chris, early October; passed on 2026-10-19).** Further down the pipeline she used the fraction of each cell covered by ice to split the albedo between ice and vegetation, i.e. the area-weighted (partial-ice) variant, while the configured headline is the threshold variant. Which she intends for the paper is asked in the Word document.

**Answered 2026-10-19 (Andria, via Chris):** partial ice (the area-weighted mix,
`veg_ice_parts`, fixed glacier albedo) is what is intended for the paper's headline. Chris:
keep the threshold version (`veg_ice_thresh`) as a sensitivity check; the month-by-month
glacier albedo (`_icesc_`) stays a further sensitivity check. **Switched 2026-10-19**
(Chris approved): config.R's headline (`analysis$forcing$variant`) is now `veg_ice_parts`,
branch `headline-partial-ice` (change a099008), merged into feature/single-config at 1716516;
the legacy configuration keeps `veg_ice_thresh`. The headline setting now accepts only the
four totals. HadGEM3 8-10 ka study-area mean: 4.55 W/m² (was 4.90 all-or-nothing).

### C2. Ice readvances are averaged away
**What the code does.** Script 7a (lines 452-459) looks for cells where
the ice fraction increases from an older to a younger slice, and replaces
that value by the mean of its neighbours in time. Real readvances are
erased; the adjustment is applied once, not repeated, and it fails with an
error if the increase occurs in the first pair. A later sum with
`na.rm = TRUE` (lines 526-530) turns such pairs into a confident zero
albedo change that still receives a kernel.
**Why it matters.** The early-Holocene forcing is dominated by ice, so
the rule for handling the ice chronology sets the largest numbers.
**What we need from you.** Is suppressing readvances deliberate? If so we
will clamp the chronology to monotone retreat cleanly; if not we will
keep real readvances.

**Two code-level consequences, found 2026-09-23.** (i) The one-step
smoothing is applied once, not iterated; 2,844 pairs in the anchored run
still show ice growing forward in time afterwards. (ii) Where ice
advances ACROSS the 50 % threshold (young end ice, old end vegetation),
both the vegetation and the ice difference are `NA` and `rowSums(na.rm
= TRUE)` turns them into 0, a silent "no change" that then receives a
kernel: 36 pairs. And if the offending step were ever the first pair of
a series the index arithmetic would hit position 0 and the script would
stop; it has not happened in the data so far.

**Decided 2026-10-14 (Chris, to be reviewed by Andria; a config switch, with the original as the legacy setting):** The arithmetic error is fixed as a code correction (legacy switch kept): an ice advance across the threshold was recorded as zero change; changes are now computed from each slice's complete albedo state. Whether to keep readvances rather than smoothing them stays with you.

**A third consequence, found 2026-10-14 when the fix was verified (issue I16, first part).**
The readvance adjustment and the choice of glacier albedo disagree: the adjustment lowers
a slice's ice fraction, but script 7a picks the glacier albedo from the *unadjusted*
fraction. In 6 pairs of slices (cells 3077, 3693, 3966, 7551, 7554, 9086) the adjusted
fraction is below 50 % while the unadjusted one is above, so the original arithmetic
counted both a vegetation change and an ice change for the same pair: about -0.6 in July
albedo, where the vegetation change alone is about -0.02. The corrected arithmetic uses
the adjusted fraction throughout and keeps only the vegetation change. The effect on the
forcing is small (with the 3 advances, at most 4.7 mW/m² global-equivalent in any period),
but it is one more reason the readvance rule matters.

### C3. The three kernels are not like for like
**Verified on 2026-09-19** for HadGEM3 and CAM5, and **on 2026-09-21**
for CACK, by downloading the kernels and opening them (README.md records
provenance, licences and sizes).

**What the code does and what the files contain.**

| Script reads | Variable long name | Flux level | Sky | Units |
|---|---|---|---|---|
| `8:58` HadGEM3 `albedo_sw_cs` | "SW Surface albedo clear-sky kernel" | **top of atmosphere** | clear-sky | W/m² per 1% |
| `8:163` CAM5 `FSNSC` | "Clearsky net solar flux at **surface**" | **surface** | clear-sky | W/m² per 1% |
| `8:197` CACK `CACK`, `level=month`, `band=3` | "Temporally-explicit kernels" | top of atmosphere | all-sky | W/m² per unit albedo |

**CACK's units are different, and the code already handles it.** CACK
declares `Units = W/m^2` for a unit (0 to 1) albedo change, not per 1%.
That is why `8:295-301` uses `alb_diff * (-rk_cack)` with no factor of
100, while HadGEM3 and CAM5 get `alb_diff * 100 * kernel`. The sign flip
is also right: CACK is stored positive (13 to 258 W/m² over our cells),
whereas HadGEM3 and CAM5 are stored negative. So all three conversions
are dimensionally consistent as written.

**But `band = 3` is a year, not a sky condition.** The CACK file's
fourth dimension is documented as `Year: Years after 2000 AD`, with 16
entries. `raster(..., level = month, band = 3)` therefore pulls the
kernel for **2003 alone** — one arbitrary satellite year — rather than a
climatology. The file also contains `CACK CM`, described as
"Climatological mean kernel", which is almost certainly what a Holocene
study wants, and which would remove a source of interannual noise that
differs between the three kernels.

**CACK also ships uncertainty layers.** `Sigma_total`, `Sigma_me` (model
error) and `Sigma_du` (data uncertainty) are in the same file at the same
resolution, with climatological-mean twins. These would let the kernel
contribute a genuine uncertainty band to the forcing (see section A)
rather than being treated as exact.

**Good news on units for the other two.** Both files are per 1% albedo change: HadGEM3
states `units = W/m2/%`, and Pendergrass et al. (2018, §2.1) define their
albedo kernel as "the change in radiative flux for a 1 % change in
surface albedo". So the `alb_diff * 100 * kernel` convention at
`8:256-301` is right for both, and the talk's "W/m²/%" is correct.

**The problem.** The two kernels answer different questions: HadGEM3
gives the change in flux at the top of the atmosphere, CAM5 the change at
the surface. They are not comparable, so "radiative forcing was not
sensitive to kernel" is comparing unlike quantities. The like-for-like
CAM5 variable is `FSNTC`, "Clearsky net solar flux at top of model",
which is in the same file, so the fix is a one-word change.

**Also available at no cost.** Each file contains its all-sky twin
(HadGEM3 `albedo_sw`, CAM5 `FSNT`/`FSNS`), so the clear-sky versus
all-sky test in C4 needs no new download.

**What we need from you.** (a) Was top of atmosphere the intended flux
level throughout? If so, may we switch CAM5 to `FSNTC` and re-run the
comparison? (b) Was `band = 3` meant to select the year 2003, or was the
climatological mean `CACK CM` intended? (c) CACK is all-sky while HadGEM3
and CAM5 are read as clear-sky, which is a second way the three are not
like for like, on top of the flux-level mismatch; was that deliberate?
(d) The kernel grids are aligned to the data by arithmetic on longitude
and latitude (`8:230-250`) with the check plots commented out; was that
verified? We have now re-run that arithmetic against the 2,870 interp
cells and all three kernels return values with no missing cells, which is
consistent with correct alignment but does not prove the grids are not
transposed.

**Decided 2026-10-14 (Chris, to be reviewed by Andria; a config switch, with the original as the legacy setting):** (a) top-of-atmosphere flux for all kernels (CAM5 read at TOA); (b) CACK's climatological mean kernel instead of the year 2003. (c) is settled by C4. (d) remains open.

**Still to ask (Chris, 2026-10-19).** Were the differences between the three kernels (clear-sky
or all-sky, top of atmosphere or surface, one year or a climatology) deliberate, for example as a
sensitivity test? And is there a reason for three kernels beyond showing that the result does not
depend on the kernel? (Also in the Word document.)

### C4. Clear-sky, pre-industrial kernels across the Holocene
The HadGEM kernel is clear-sky and pre-industrial. Clear-sky albedo
kernels are typically 1.5 to 2 times larger than all-sky because clouds
mask the surface, and early-Holocene summer insolation at high latitudes
was tens of W/m² above pre-industrial. What is the case for this choice,
and would an all-sky kernel change the headline numbers?

**Now measured, 2026-09-21.** The first complete interp run of script 8
answers the second half of the question: yes, substantially. Mean
forcing per slice-pair over all cells and months, `rf_*_veg_ice_thresh`:

| slice-pair (BP) | HadGEM3 (clear) | CAM5 (clear) | CACK (all-sky) | CACK / HadGEM3 |
|---|---|---|---|---|
| 11,000 | 2.270 | 2.377 | 1.205 | 0.53 |
| 10,500 | 2.945 | 3.076 | 1.618 | 0.55 |
| 10,000 | 2.428 | 2.553 | 1.447 | 0.60 |
| 8,500 | 3.383 | 3.556 | 1.810 | 0.54 |
| 8,000 | 2.594 | 2.741 | 1.386 | 0.53 |

HadGEM3 and CAM5 agree to within about 5%, which is the basis for the
talk's claim that the result is not sensitive to the choice of kernel.
But both are read as clear-sky, so that agreement is between two
like-for-like quantities and does not test the choice. CACK, the only
all-sky kernel of the three, gives consistently **just over half** the
forcing, a ratio of 0.53 to 0.60. That sits squarely in the 1.5 to 2
times range quoted above for clear-sky over all-sky.

So the kernel choice moves the headline number by roughly a factor of
two, and the claim of insensitivity does not survive including CACK.
This needs resolving before the numbers are quoted. It is also separable
from the top-of-atmosphere versus surface mismatch in C3: switching CAM5
to `FSNTC` and comparing all-sky against all-sky would isolate the two
effects, and both variables are already in the files we hold.

**Caveat on these figures.** They are unweighted means over cells and
months, taken directly from `output/forcing/RF_holocene_all_cases.RDS`;
the talk's bar plots are per-period and area-weighted, so the ratio is
the meaningful quantity here rather than the absolute values.

**Decided 2026-10-14 (Chris, to be reviewed by Andria; a config switch, with the original as the legacy setting):** All-sky kernels for the headline (HadGEM3 albedo_sw, CAM5 FSNT, CACK), clear-sky kept as a sensitivity.

### C4b. Ice left out of the slide-18 bars
**Raised 2026-10-19** by Chris, from the full run of 2026-10-18 and the split of its forcing into
ice and vegetation change (one-off figure, not in the pipeline).
Andria's slide-18 code (git 383002d, scripts/8_radiative.R, `rf_long` set to NA where `ice_young`
or `ice_old` is present) blanks every step with ice at either end, so the slide shows vegetation
change on land that stayed ice-free. The current pipeline counts the land uncovered by the
retreating ice sheet, and it dominates before 6 ka: at 8-10 ka the study-area mean is +5.67 W/m²
from ice change and -0.77 from vegetation change (total 4.90, HadGEM3 all-sky), against 0.37 on
the slide. (Each 500-year step counts as ice change if ice is present at either end; the two parts
add up to the total.)
**What we need from you.** Was leaving the ice out deliberate, to show vegetation alone? Should
the paper report ice and vegetation separately, the total, or both? (Also in the Word document.)

### C5. The comparison with modern forcing agents
The talk compares the Holocene forcing with IPCC forcing from modern
agents. The Holocene number is local to North American land, clear-sky,
and per interval; the IPCC numbers are global means, all-sky, and
cumulative since 1750. North American land is a few percent of Earth's
surface. What is the fair comparison: the Holocene forcing expressed as a
global mean, or the modern agents expressed regionally?

**Attempted 2026-09-21, and the choice decides the headline claim.**
`scripts/9_forcing_barplot.R` now builds the slide 18 chart from
`output/forcing/RF_holocene_all_cases.RDS`. No code for it existed in
the repository, so every aggregation choice is ours and is listed in
that script's header. The study area works out at **3.8% of Earth's
surface**, and the two normalisations differ by a factor of about 27:

| period | domain mean (W/m², HadGEM3) | global equivalent (W/m²) |
|---|---|---|
| 8 - 10 ka | 8.04 | 0.303 |
| 10 - 12 ka | 7.17 | 0.270 |
| 6 - 8 ka | 2.89 | 0.109 |
| 4 - 6 ka | -1.74 | -0.066 |
| 0.05 - 0.5 ka | -1.17 | -0.044 |

IPCC AR6 gives methane 0.544 W/m² (1750-2019, global mean). On the
**global-equivalent** basis the early-Holocene peak is 0.30 W/m², about
56% of methane, which matches the talk's "comparable to modern methane
forcing" well. On the **domain-mean** basis it is 8.0 W/m², roughly
fifteen times methane, which would not be described that way. So the
talk's own wording implies the global-equivalent normalisation was used.

**Please confirm** that is what you did, because it is the single
biggest lever on the headline number, and the script currently assumes
it. Related assumptions in the same script that we would like checked:
(a) forcing variant `veg_ice_thresh` rather than one of the nine others,
notably the `_parts` family that mixes vegetation and ice albedo by area
fraction instead of by threshold [**answered by C1, 2026-10-19:** the
`_parts` family, `veg_ice_parts`, is the headline since 19 October];
(b) slice-pairs **summed** within each
period, which telescopes to the endpoint difference, rather than
averaged; (c) months averaged with equal weight; (d) cells area-weighted
by the `area` column. A sensitivity plot over (a) is at
`figures/forcing_barplot_variant_sensitivity.pdf`.

**The layout now matches slide 18, but the numbers do not.** Reading the
bars off `ppt/media/image37.png` in `writing/dawson_EGU.pptx` and
comparing with our global-equivalent values (HadGEM3, `veg_ice_thresh`,
the all-or-nothing headline of that time; the headline is `veg_ice_parts`
since 2026-10-19, see C1):

| period (ka) | slide 18 | ours | ratio |
|---|---|---|---|
| 0.05 - 0.5 | -0.38 | -0.044 | 8.6 |
| 0.5 - 2 | -0.17 | -0.047 | 3.6 |
| 2 - 4 | 0.22 | 0.063 | 3.5 |
| 4 - 6 | ~0.00 | -0.066 | - |
| 6 - 8 | 0.36 | 0.109 | 3.3 |
| 8 - 10 | 0.37 | 0.303 | 1.2 |
| 10 - 12 | 0.72 | 0.270 | 2.7 |

The ratio is not constant, so this is **not** a normalisation or unit
difference that we could simply correct. We checked all six forcing
variants and none is closer: the best has essentially the same error as
the worst. The shape differs too. On the slide the forcing grows
monotonically into the past and peaks at 10-12 ka; ours peaks at 8-10 ka
and dips negative at 4-6 ka where the slide is flat. The slide's
late-Holocene cooling is also far deeper than ours.

**Andria confirmed on the call that slide 18 is the interp version**, and
the code that made it has now been recovered from git history: commit
`383002d` (2 April 2024, twelve days before EGU 2024), `8_radiative.R`
lines 584-628. It was marked "incorrect now" and commented out in that
commit, then deleted in `aba7e1a` (Feb 2025), so it was absent from the
code we received. Its recipe differs from our first attempt on nearly
every assumption:

| choice | slide 18 code (Apr 2024) | our first attempt |
|---|---|---|
| input | script 7's `alb_interp_preds_diffs`: 7 coarse pairs, one per period | 7a's 24 consecutive pairs, summed |
| months | **feb, may, aug, nov only** | all twelve |
| ice | binary-flagged cells masked out | ice fraction via `veg_ice_thresh` |
| aggregation | **plain unweighted mean** over cells x months (`mean_forcing`) | area-weighted, global-equivalent |
| kernel | HadGEM3 | HadGEM3 |

So the slide is a **domain mean over the study area, not a global
equivalent**, and the "comparable to modern methane" comparison on the
slide sets a regional mean beside a global one. That is the C5 question
answered, in the direction that needs discussing.

**Rerunning her exact recipe on today's data** (`scripts/9_forcing_barplot.R`,
`figures/forcing_barplot_slide18_egu2024recipe.pdf`) reproduces the
late-Holocene shape but not the numbers:

| period (ka) | slide 18 | her recipe, today's data |
|---|---|---|
| 0.05 - 0.5 | -0.38 | -1.27 |
| 0.5 - 2 | -0.17 | -0.94 |
| 2 - 4 | 0.22 | 0.76 |
| 4 - 6 | ~0.00 | -2.14 |
| 6 - 8 | 0.36 | 0.05 |
| 8 - 10 | 0.37 | **-2.38** |
| 10 - 12 | 0.72 | 3.97 |

Magnitudes are 3 to 5 times larger and 8-10 ka changes sign. Since the
recipe is now hers, the residual is in the **data**: the April 2024
versions of `veg_posts_interp_ice.RDS` and the calibration tables are not
in git (the only committed copy is the one you sent on 2026-09-18), and
no intermediate table or figure was ever committed.
**Answered (Andria, via Chris, 2026-10-19):** she no longer has the code and data behind the
slide and does not want to reproduce it. The aim is to get the method right and produce results
we are confident in, even if they differ from the slide. The differences above stay as a note
only. For comparison, the vegetation part of the full run of 2026-10-18 (study-area mean,
HadGEM3 all-sky, steps with ice at either end left out) is -0.70, -0.68, +0.48, -0.71, -0.51,
-0.77 and +1.34 W/m² from 0.05-0.5 to 10-12 ka; the 6-8 and 8-10 ka bars have the opposite
sign to the slide. Chris sent her that figure on 2026-10-19.

The IPCC panel does match: CO2 2.16, methane 0.54, water vapour 0.05,
albedo (land use) -0.20 and aerosols -1.06 are AR6 values and reproduce
the slide's bottom panel exactly, which confirms the AR6 source and that
the slide's x axis really is a global-mean W/m² scale shared by both
panels. That is strong evidence for the global-equivalent reading of
assumption A8.

**Decided 2026-10-14 (Chris, to be reviewed by Andria; a config switch, with the original as the legacy setting):** Express the regional effect as a global-equivalent flux (area-weighted, over Earth's area) for the IPCC comparison, and distinguish this direct albedo effect from IPCC effective radiative forcing. Slide 18's numbers: not pursued (Andria, 2026-10-19; see above).

### C6. Consecutive-slice differences and interval length
Forcing is defined per consecutive pair of slices. The pairs span
different lengths (150, 300, 500 years) and do not accumulate to a change
relative to a baseline. Should a fixed reference slice be used, or forcing
be expressed per unit time?

### C7. The 11.5 ka slice is labelled 12 ka
Script 7 relabels age 11,500 as 12,000 for plotting (line 100); script 7a
does not. Is 12 ka the intended label, and is it the same slice? (The
broader time-slice question is in the general file.)

### C8. Forcing area for coastal and lake cells
*Added 2026-10-14, from the methodology decisions (issue I16).* Forcing is the albedo
change of each cell times the kernel times the cell's area, using the whole 1-degree cell
as if it were all land (`7a:119-124`), and the Dalton ice fraction is likewise per whole
cell. The calibration will now use land-only albedo (decision B5). Water's albedo does not
change with vegetation, so a cell that is part lake or sea contributes more forcing than its
land can produce, by its water share. Weighting by land area would be more exact, but the
ice covered places that are water today (Hudson Bay, the Great Lakes), which needs an
explicit treatment of ice-to-water transitions. For now the whole-cell weighting is kept
(Chris, 2026-10-14). **Should coastal and lake cells count only their land area?**

**Added 2026-10-15 (Astra's review of 2026-10-15, docs/cc/2026-10-15_snow_and_water_masking_codex_gpt6astra.md on the migration branch).** With land-only albedo, a land albedo change should be applied
to land area, or converted to a whole-cell change by the land fraction, and only once.
Two facts would settle how: what the `cell_area` field in the land-cover posterior means
(the pipeline drops it), and whether the Dalton ice fraction is a share of the whole cell
or of its land. Vegetated land, ice and water should form one partition of each cell
without overlap. A modern mask also cannot show past coastlines or proglacial lakes.

**Added 2026-10-22 (size of the effect and published practice; Chris keeps whole-cell weighting
for now and leaves the choice to Andria).** The new MODIS albedo is land only, so the question is
sharper. Recomputed on the current run (feature/single-config 1716516, partial ice, vegetation kept on
ice caps), holding each cell's forcing fixed and weighting by today's land share (sea removed, lakes
kept; `non_ocean_share` in `data/elev_land_mean_cells.csv`): area falls 3.7%; the HadGEM3 global
equivalent falls 4.0% at 8-10 ka and 4.2% at 10-12 ka (CAM5 and CACK about the same); the study-area
mean moves only 0.3%. Small periods move more and in both directions (2-4 ka +6.7%, 0.5-2 ka -6.2%).
Ocean only from MOD44W: -3.5% at 8-10 ka. Dropping cells under 50% land instead: -2.3%. Studies that
use observed land albedo weight by land area (Boisier et al. 2013, Biogeosciences,
doi:10.5194/bg-10-1501-2013, "global ice-free land"; Oleson et al. 2003, GRL,
doi:10.1029/2002GL016749, land-only albedo and mixed cells dropped); none found that weights a
land-only change by whole-cell area. Climate-model studies (e.g. Strandberg 2014, Dallmeyer and
Claussen 2011) mix land and sea in each cell by construction, so whole-cell weights are right there.
Today's land share would drop the real ice-to-sea change at Hudson Bay; a land share for each time
slice (e.g. from ICE-6G_C land-sea masks) would handle it but is the most work. Script:
`land_weight_effect.R` (Chris's scratch folder, coastal/).

## D. Land cover

**Andria's answer (email to Chris, early October; passed on 2026-10-19).** The `cell_area` column of `veg_posts_interp_ice.RDS` exists; checked 2026-10-19: it is the whole 1-degree cell area in km² (ratio to our polygon areas 0.996-0.999), not land area, so it gives no land share.

### D1. Taxon-to-class mapping in the north
`taxon2LCT_translation_v2.0.csv` maps Betula, Alnus and Salix to
summergreen trees and drops Ericaceae, so dwarf-birch tundra looks like
deciduous forest to the model, and open land lumps prairie, tundra and
cropland with very different albedo. Was this examined? It matters most
in the north, where the largest Holocene changes are.

### D2. Age uncertainty and binning
How are samples whose age posteriors straddle a bin boundary assigned to
time slices? Sharp events such as the hemlock decline can be smeared or
sharpened by that choice.

### D3. The 1,814 absent cell-slices: all at the edges, none used downstream
Characterised 2026-09-25. The land-cover product has 2,870 cells x 25
slices = 71,750 cell-slices, of which 1,814 are absent. All of them
belong to 211 cells, and every one of those cells lies outside the band
the pipeline uses: 114 south of 27 N (1,422 absent cell-slices) and 97
north of 74 N (392). No interior cell is missing any slice, so script
8's 27-74 N trim removes every incomplete cell before the forcing is
computed, and script 7a's coverage check finds exactly these 211.
**Conclusion: harmless for the modelling as run.** Two patterns are still
worth understanding as data quality:

- South of 27 N the coverage shrinks back in time in steps: 10 cells
  (a strip along 17.5 N, 100.5 W to 91.5 W, the grid's southern edge)
  exist only at 1,000-2,000 BP; 84 cells are absent from 5,500 BP back;
  all 114 are absent from 8,000 BP back. That reads like the southern
  reach of the pollen-site network thinning into the past.
- North of 74 N, 66 cells are absent at exactly three slices, 200, 4,500
  and 6,000 BP, and present at every other slice including older ones;
  97 are absent at 11,000 and 11,500 BP. Three isolated slices is not a
  geological pattern; it looks like an artefact of the interpolation run
  (and it is why the ice-flag count at 4,500 BP, 41, is below its
  neighbours: 14 flagged cells are simply absent at that slice).

**Tested 2026-09-25: the northern absences are not the ice mask.** The
same 66 cells are absent at all three of 200, 4,500 and 6,000 BP, and
they are exactly the grid's three northernmost rows (76.5, 77.5, 78.5 N).
Only 14 of them are flagged ice at the neighbouring slices and 33 are
ever flagged at any slice, so this is not ice being removed rather than
flagged. At 11,000 and 11,500 BP the 97 absent northern cells include
those rows plus the 48 cells that are ice at 10,500 BP. The three
isolated slices look like interpolation runs done on a slightly smaller
extent, or clipped afterwards; the product does not say.

Is the southern thinning intended (no pollen data), and what happened to
the top three rows of the grid at 200, 4,500 and 6,000 BP?

**Why is the forcing limited to 27-74 N? (Chris, 2026-10-19.)** The two lines that keep cells
between 27 and 74 N came in with Andria's commit aba7e1a of 2025-02-14 ("updating processing and
plotting scripts"), with no comment; the draft, its CM-comments copy and the EGU 2024 slides do
not mention the band (searched 2026-10-19). It removes 211 of 2,866 cells, 7.6% of the area (114
south of 27 N, 6.3%; 97 north of 74 N, 1.3%), exactly the incomplete cells above. Was the band
meant to keep only cells with a complete record, or is it a choice of region? If the former, a
rule "keep cells complete at every slice" states it directly and gives the same cells today.
(Also in the Word document.)

**Andria's answer (email to Chris, early October; passed on 2026-10-19).** She had checked that the 1,814 missing cell-slices are all at the northern and southern extremes and so not included in the modelling later. Why the 27-74 N band (which is what excludes them) is asked in the Word document.

### D4. Script 7's coverage filter removes nothing
*Added 2026-10-12, from issue I02 of the configuration migration.*
**What the code does.** After choosing the eight coarse ages (0.05 to 12 ka),
script 7 drops "cells with fewer than 8 rows" before mapping and computing
the coarse differences. But it counts rows per cell across ages *and*
months, and a cell present at any age has 12 rows per age, so the test
never fires: it removed 0 of 2,870 cells. The line is now commented out,
with an explanation, so that a run on fewer months cannot make it start
removing cells for the wrong reason; the outputs are unchanged.
**Why it matters.** The line looks meant to keep only cells present at all
eight coarse ages. 211 of the 2,870 cells are missing at least one of them
(10 are present at only one age, 74 at four), mostly the edge cells of D3.
Those cells currently enter the maps, and any coarse difference for which
both ages exist, as they are. If they should be removed, the coarse
differences and the EGU 2024 recipe in script 9 change.
**What we need from you.** What was the filter meant to do? Keep all cells
(as now), keep only cells present at all eight coarse ages, or something
else?


## E. Validation

### E1. Independent check of the hindcasts
Is there any out-of-sample test of the hindcast albedo? The last 150
years of Euro-American clearance, with land-survey or HYDE land cover and
published deforestation forcing estimates, looks like the natural one.
