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
- The model is extrapolating: past cover mixes in this region lie outside
  the mixes it was fitted on today, and the fitted smooth bends the wrong
  way there.
- Water in the calibration albedo: lakes and coastline mixed into your
  0.25° albedo raster may have taught the model that some cover mixes are
  very dark.

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
