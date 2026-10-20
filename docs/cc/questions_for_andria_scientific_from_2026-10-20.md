# Open questions for Andria: scientific and technical (from 20 October 2026)

Maintained by Chris with Claude Code; kept current as work proceeds (see
AGENTS.md). This file holds the scientific questions raised **from
20 October 2026 onwards**. The earlier questions are in
`questions_for_andria_scientific.md`, which is frozen as of that date: its
content went into the Word document Chris shared with Andria on Google
Drive, and her answers are being collected there. Keeping the new
questions in a separate file lets Chris copy them across by hand.

Each entry says what we found, why it matters, what we propose to check,
and what we need from you.

## 1. The model predicts a darker surface where hemlock collapsed (northeast, 6 to 4 ka)

*Raised on the 19 October call (Chris: check against figure 4a of the
Climate of the Past paper, Dawson et al. 2025); analysis 20 October.*

**What we found.** In the northeastern US and southeastern Canada (box
80-66°W, 40-48°N, the region of figure 4a), the land-cover reconstruction
shows evergreen cover falling from 0.40 at 6 ka to 0.23 at 4 ka, and
summergreen rising from 0.54 to 0.71: the hemlock decline. Replacing
conifers with deciduous trees would normally make the surface brighter,
especially over spring snow, which should give cooling. Instead the
calibration model predicts a darker surface in almost every month, by up
to 0.09 in April and May, so our 4-6 ka bar shows warming in that region
(18 October headline run, partial ice, HadGEM3 kernel). Eastern Canada
shows the same pattern where evergreen cover expanded.

**Why it matters.** The predicted values look implausible. At 4 ka the
median May albedo in the box is 0.068 and some cells reach 0.040, while
the satellite albedo today in the same cells is 0.116. If the model is
wrong here, the sign of a regional signal your paper highlights is wrong
in our forcing, and the same may happen wherever the past cover mix lies
outside the range of today's mixes.

**Possible causes (not yet confirmed).**
- The cover term is confounded with location: in the box, evergreen share rises
  northwards with latitude, so the fitted cover surface can take a steep slope that the
  location term offsets today but not when the cover changes in the past.
- The model is extrapolating: past cover mixes in this region lie outside
  the mixes it was fitted on today, and the fitted smooth bends the wrong
  way there.
- Water in the calibration albedo: lakes and coastline mixed into your
  0.25° albedo raster may have taught the model that some cover mixes are
  very dark.

**Further evidence (20 October, same run).**
- The forcing in the box is vegetation only after 9.5 ka; the ice part is zero there in
  every period from 8 ka onwards.
- The forcing follows the evergreen curve with no lag. Evergreen falls from 6 to 4.5 ka
  (0.40 to 0.22), giving warming (+5.2 W/m2 over the box for 4-6 ka). It then recovers
  from 4 to 0.5 ka (0.23 to 0.37), giving cooling (-2.9 for 2-4 ka, -1.7 for 0.5-2 ka).
  The cooling after 4 ka is the same effect in reverse.
- At a fixed place in the box, the May model gives albedo 0.06 for 10% evergreen and
  0.49 for 60% evergreen (open land fixed at 6%). Both ends are implausible for forest.
- The data the model was fitted on point the other way. Across the 83 box cells today,
  May albedo falls as evergreen share rises (correlation -0.63). Evergreen share is almost
  a function of latitude there (correlation 0.92), so the location term and the cover term
  can trade off against each other (related to question B3 in the earlier file).
- The 4.5 ka cover mixes lie outside the mixes the model was fitted on: the median distance
  to the nearest modern mix is 0.12 in cover fraction, against 0.04 at 6 ka.
- Checked for May with the selected model only; other months are not yet checked.

**What we propose to check.**
- The model's fitted albedo for pure evergreen and pure summergreen cover,
  month by month, against published values for conifer and deciduous
  forest.
- Whether the past cover mixes in the box fall outside the range of
  today's mixes used in the fitting.
- Whether the effect disappears with the land-only satellite albedo now
  being rebuilt.
- Spatially blocked cross-validation (already proposed on 16 October),
  which measures how well the model predicts places and mixes it has not
  seen.

**What we need from you.** Have you seen dark predicted albedo for
mixed conifer-deciduous cover before? Do you know whether your albedo
raster excluded water? And do you agree that the hemlock decline should
give a brighter surface, and so cooling, in this region?

## 2. Questions raised by the independent review of the hemlock result

*From Astra's interpretation of the northeast result, 20 October
(`2026-10-20_northeast_cover_effect_interpretation_codex_gpt6astra.md`). Its leading explanation:
in today's data, evergreen share follows latitude so closely that the model cannot tell the effect
of evergreen cover apart from the effect of being further north (colder, snowier); and the 4.5 ka
cover mixes have no close modern counterparts. Checks are under way.*

1. **What should the vegetation forcing represent?** The effect of past vegetation under today's
   climate and snow, or the total albedo change including past changes in snow? The model has no
   input for past snow.
2. **Which modern observations best show hemlock being replaced by deciduous forest in the
   northeast?** We would use them to test the direction of the model's effect.
3. **What do the pollen-based cover fractions measure:** share of land area, of vegetated area, or
   canopy cover? And how should shrubs map onto satellite land-cover classes?
4. **Which years does the "modern" (50 BP) vegetation slice represent,** and is it a fair match for
   satellite albedo from 2000-2009?
5. **Was taking the albedo at the cell centre deliberate?** Could the rebuilt land-only cell averages
   become the calibration albedo instead?
6. **Past cover mixes with no modern counterpart:** should their forcing be left out, given as a
   bounded range, or shown separately as unsupported?
7. **Would a snow-aware estimate built from albedo per cover class** (evergreen, deciduous, open;
   snowy and snow-free) be enough as a comparison for the paper, or should it replace the model if
   the model's cover effect stays unstable?
