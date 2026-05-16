---
title: "Late-Night Gaming and Sleep in Adults: A Registered Report Using Multi-Platform Telemetry"
subtitle: Chronotype, Sleep, and Wellbeing in Adult Gamers
running-head: Late-Night Gaming and Sleep
author:
  - name: Tamás A. Földes
    email: contact@tamasfoldes.mozmail.com
    orcid: 0000-0002-0623-9149
    corresponding: true
    affiliation:
      - ref: 1
  - name: Other Authors
    affiliation:
      - ref: 2
affiliations:
  - id: 1
    name: University of Oxford
    department: Oxford Internet Institute
    city: Oxford
    country: United Kingdom
  - id: 2
    name: Second University
abstract: |
  Late-night gaming has been linked to disrupted sleep and poorer wellbeing, but most evidence relies on self-reported play time in adolescent samples. Using the Open Play dataset—a three-month longitudinal study of adult gamers in the UK and US that paired session-level digital-trace data from Nintendo, Xbox, and Steam with biweekly panel and daily-diary self-reports—this Registered Report tested whether late-night gaming (23:00–06:00) predicted (H1a) poorer sleep quality, (H1b) shorter sleep duration, (H1c) greater daytime sleepiness, and (H1d) lower mental wellbeing, and whether these associations were amplified among individuals with more evening-leaning chronotypes (H2). Outcomes were assessed with the PSQI, ESS, and SWEMWBS across six biweekly waves; chronotype was indexed by the MCTQ-derived MSF~sc~. Only H1a was supported: each additional 10 minutes of daily late-night gaming was associated with an approximate 0.9 percentage-point increase in the probability of reporting poor sleep quality, consistent across imputed, complete-case, and exploratory diary specifications. With a smallest effect size of interest set to ±0.1 × SD of the outcome, TOST equivalence tests accepted the null for H1b–H1d, bounding any true effects below ±7 minutes of nightly sleep, ±0.38 Epworth points, and ±0.53 SWEMWBS points per one-SD (≈36 min/day) increase in late-night gaming. None of the H2 chronotype × late-night gaming interactions reached significance in the pooled imputation models; equivalence tests accepted the null for H2b–H2d, with H2a undecided. The "quality down, duration preserved" pattern is inconsistent with sleep displacement but compatible with pre-sleep arousal, attribution, or compensatory phase delay — mechanisms self-report alone cannot adjudicate.
categories: [late-night gaming, sleep quality, sleep duration, daytime sleepiness, mental wellbeing, chronotype, digital trace data, registered report]
authornote: |
  Created with Quarto {{< version >}} and *preprint-typst* 1.4.3 on 2026-05-16.
date: last-modified
bibliography: bibliography.bib
prefer-html: true
execute:
  echo: false
  warning: false
  message: false
  cache: true
params:
  refit_panel: false
  refit_diary: false
  refit_diary_imputed: false
  refit_appendix: false
  rerun_preprocessing: false
  # When false (default), get_custom_rows_pooled() reports the structural ICC
  # from each bundle's first fit (~30-60 s per diary bundle). When true, it
  # averages performance::icc() across all imputations (~15-18 min cold per
  # diary bundle, ~1-2 min per panel bundle) and caches the scalar to
  # manuscript_cache/icc/ so subsequent renders pay nothing.
  icc_full: false
format:
  html:
    toc: true
    toc-location: left
    theme: cosmo
    code-fold: true
    html-math-method: katex
    embed-resources: true
    include-after-body: appendix-prefix.html
  preprint-typst:
    wordcount: true
    citeproc: false
    # Single-column layout (omit `theme: jou` which would force 2-column).
    # Other available themes: `dracula` (dark colour scheme, still single-column).
    # line-number: true            # Line numbering
    # fontsize: 11pt               # Font size
    # leading: 0.6em               # Line spacing
    # first-line-indent: 1.8em     # Paragraph indent
  docx:
    toc: false
    number-sections: false
---

<!--
Render runtime notes
====================
The  dominant bottleneck — `tbl-diary-h1h2` at ~15-18 min per pass
— can be reduced to ~30-60 s by computing the structural ICC from a single
fit per bundle (the default).  We can `-P icc_full:true` to instead average
`performance::icc()` across all imputations; the first such render will take capital T TIME.
The heavy model-fitting chunks (`fit-h1-models`, `fit-h2-models`,
`fit-diary-imputed-models`, `appendix-panel-h1-spline-bic-fit`) are gated by
`refit_*` params defaulting to false, so they only run when explicitly
requested via `-P refit_*:true`.
-->

# Introduction

Concerns have been raised about the potential negative impacts of video gaming on sleep and overall wellbeing, particularly for adolescents and young adults and especially when gaming occurs late at night [@hale2015screen; @muppalla2023effects; @peracchia2018exposure]. Pre-sleep technology use is also the modal evening behaviour in adults: a recent nationally representative US survey found that 89.9% of adults use an electronic device within an hour of bedtime, with 71.3% using a TV or gaming console specifically and 49.3% doing so inside the bedroom [@carlson2026technology]. Heavy or problematic gaming has been shown to disrupt sleep patterns, reduce sleep duration, lower sleep quality, and increase daytime sleepiness [@exelmans2015sleep; @han2024electronic; @kristensen2021problematic; @kemp2021sleep]. Crucially, however, @kristensen2021problematic noted that none of the studies in their review registered the time of day gaming took place, meaning that timing-specific effects — particularly those concentrated in the pre-sleep window — remain poorly characterised. This is especially concerning given the far-reaching effects of sleep disturbances on cognitive and emotional functioning [@mccoy2011cognitive; @simon2020sleep; @vriend2013manipulating]. For instance, habitual gaming between 10 p.m. and 6 a.m. has been associated with an increased risk of depressive symptoms, partially mediated by daytime sleepiness [@lemola2011habitual]. Understanding the consequences of late-night gaming is thus vital for both gamers and health professionals.

## Mechanisms Linking Late-Night Gaming to Sleep Disturbance

Two key mechanisms have been proposed to explain the impact of late-night digital engagement (including gaming) on sleep [@cain2010electronic; @lebourgeois2017digital]. The first is the displacement hypothesis, which argues that late-night gaming is more harmful than daytime gaming because it cuts into sleep time [@twenge2019more; @lemola2011habitual; @exelmans2015sleep]. Gamers often feel compelled to continue playing and may struggle with self-regulation, which can lead to insufficient sleep [@king2009understanding; @pirrone2024why; @spada2017metacognitions]. For example, adolescents with high trait flow delayed bedtime by ~90 minutes when playing challenging games [@smith2017mechanisms].

The second mechanism involves arousal-related disturbances in sleep architecture caused by late-night gaming. Empirical studies have shown that extended gaming, especially when involving violent content, significantly decreases REM sleep and total sleep time [@king2013impact]. Polysomnographic evidence in school-aged children indicates that a single bout of evening computer gaming reduced slow-wave sleep, lengthened sleep-onset latency, and impaired next-day verbal memory [@dworak2007impact]. @weaver2010effect found that pre-sleep gaming extended sleep latency by approximately 5–10 minutes, while @king2013impact demonstrated that such arousal-related disturbances can also alter the natural progression into sleep stages. This delay in sleep onset could be exacerbated by lower melatonin levels following an evening of gaming, compared to neutral activities like board games, which are crucial for regulating the sleep-wake cycle [@hartmann2019effects].

## The Moderating Role of Chronotype

Negative effects of late-night gaming may be compounded among individuals with an eveningness chronotype—a group naturally predisposed to staying up late and consistently shown to spend more time on screen-based media in the pre-sleep window [@reardon2023adolescent; @kortesoja2023latenight]. Pre-sleep technology use, in turn, appears to affect this group more strongly: in a large adult cohort, daily screen use before bed was associated with later bedtimes and shorter sleep among both chronotypes, but the delay and sleep loss were markedly greater in evening types — a pattern the authors interpret as evidence of compounded social jetlag, i.e. the misalignment between endogenous circadian rhythms and socially imposed schedules [@zhong2025electronic]. Adolescent evidence is consistent: late-night digital media use mediates the link between evening chronotype and poorer sleep quality and daytime tiredness, and these effects are most pronounced for evening types [@kortesoja2023latenight].

The downstream picture for psychosocial outcomes is more nuanced. @reardon2023adolescent found that shorter sleep on weekdays was associated with greater psychological distress, but technology medium and chronotype themselves were not direct predictors of distress. @gumport2021impact reported that, in evening-type adolescents, technology use was linked to better emotional, social, cognitive, and physical health but worse behavioral health (sensation-seeking, ADHD diagnosis, and alcohol/substance use); notably, electronic game use specifically was not associated with behavioral health in that study the behavioral-health association was driven by other technology uses. The adult evidence reviewed above [@zhong2025electronic] suggests that the chronotype-amplified effect of pre-sleep screen exposure on sleep timing and duration is not confined to adolescence, but adult evidence specifically isolating gaming (rather than screen use broadly) within the pre-sleep window remains sparse.

## The Present Study

In sum, the literature indicates that video gaming, particularly when it occurs late at night, has significant implications for sleep quality, sleep duration, and overall wellbeing. This disruption can be attributed to both the displacement hypothesis [@twenge2019more; @lemola2011habitual; @exelmans2015sleep] and arousal-related disturbances in sleep architecture [@king2013impact]. Individual differences, such as chronotype, may moderate these effects, with eveningness chronotypes particularly vulnerable to the negative consequences of pre-sleep screen exposure [@zhong2025electronic; @kortesoja2023latenight]. The present study aims to empirically test the following hypotheses regarding the relationship between late-night gaming and sleep outcomes:

H1: Late-night gaming is associated with:

- H1a: Poorer sleep quality
- H1b: Shorter sleep duration
- H1c: Higher daytime sleepiness
- H1d: Lower wellbeing

In addition to testing direct relationships between late-night gaming and various sleep-related outcomes are critical to understand, we further assess the potential moderating role of chronotype, which refers to a person's natural preference for activities during certain times of the day (morningness or eveningness). Individuals with an evening chronotype tend to stay up later and may be more inclined to engage in late-night gaming, potentially exacerbating the negative impacts on sleep and wellbeing. The combination of an evening chronotype and late-night gaming may even have a compounded effect on overall wellbeing, as both factors are independently associated with poorer mental health outcomes. Given this, we propose the following:

H2: Chronotype moderates the relationships between late-night gaming and all outcomes in H1 (sleep quality, sleep duration, daytime sleepiness, and wellbeing), such that these negative associations are stronger for individuals with more of an eveningness chronotype.

By examining chronotype on a continuous scale as a moderating factor, this study seeks to provide a more nuanced understanding of the potential risks associated with late-night gaming and to identify individuals who may be most vulnerable to its negative effects.


::: {.cell}

:::



::: {.cell}

:::



::: {.cell}

:::



::: {.cell}

:::


# Methods

## Data Source and Measures

The analyses reported here are part of a Stage 1 Registered Report [@ballou2024psychological] and utilize data from the Open Play dataset [@ballou2025openplay], a longitudinal study that collected multi-platform video game digital trace data alongside psychological measures from adult gamers in the UK and US over a three-month period. The study combined objective behavioral telemetry from gaming platforms with repeated self-report surveys administered biweekly across six waves. Importantly, the present analyses use only a subset of the Open Play dataset, specifically data from Nintendo, Xbox, and Steam platforms, as these provide session-level (Nintendo, Xbox) or near session-level (Steam) temporal granularity necessary for hourly aggregation of playtime to operationalize late-night gaming (23:00–06:00). The following validated measures were administered via panel surveys at multiple timepoints: Wellbeing was assessed using the Short Warwick-Edinburgh Mental Well-being Scale [SWEMWBS; @tennant2007warwick], a 7-item measure of mental wellbeing covering psychological functioning and subjective well-being over the past 2 weeks, with responses on a 5-point Likert scale ranging from "None of the time" to "All of the time" (score range: 7–35). 

Sleep quality and duration were assessed using the Pittsburgh Sleep Quality Index [PSQI; @buysse1989pittsburgh], a 19-item questionnaire evaluating sleep quality over the past month. The measure yields seven component scores (sleep quality, sleep latency, sleep duration, sleep efficiency, sleep disturbances, use of sleep medication, and daytime dysfunction) and a global score (range: 0–21), with scores above 5 indicating poor sleep quality. Excessive daytime sleepiness was measured using the Epworth Sleepiness Scale [ESS; @johns1991new], an 8-item scale assessing the likelihood of dozing off in various situations (score range: 0–24). Higher scores indicate greater propensity for daytime sleepiness, with scores above 10 typically indicating clinically significant excessive sleepiness. Chronotype was measured at baseline (Wave 1) using the Munich Chronotype Questionnaire [MCTQ; @roenneberg2003life]. The key metric used in this study is MSF~sc~ (Mid-Sleep on Free Days corrected for sleep debt on work days), which represents an individual's natural sleep-wake preference when not constrained by social obligations. Higher MSF~sc~ values indicate a preference for eveningness (later sleep-wake times). 

## Handling Missing Data

Missingness in the longitudinal self-report outcomes (PSQI components, PSQI sleep duration, Epworth Sleepiness Scale, and SWEMWBS) was addressed via hierarchical two-level multiple imputation by chained equations [MICE; `mice` v3.16.0 and `miceadds` v3.19 in R; @vanbuuren2011mice; @robitzsch2024miceadds] using predictive mean matching for multilevel data (`2l.pmm`). This method models participants as clusters with repeated waves nested within, preserving between-person variation that single-level PMM would attenuate. We imputed under a Missing at Random assumption conditional on rich auxiliary information. Because sleep measures (PSQI, ESS) were only collected at waves 2, 4, and 6 by design, the imputation was run in two passes: biweekly variables (SWEMWBS, measured at all six waves) and monthly variables (PSQI components, sleep duration, ESS, measured at waves 2, 4, 6 only). This two-pass approach avoids exposing the mixed-effects model inside `2l.pmm` to the structural NAs that arise at non-measurement waves. The PSQI global score was derived via passive imputation (sum of seven imputed component scores). Following van Buuren's [-@vanbuuren2018flexible] multilevel imputation recipe, predictor matrices coded participant ID as the random-intercept cluster (code −2), level-2 (person-level) predictors as fixed effects only (code 1), and level-1 outcome cross-predictors with disaggregated cluster means (code 3). In the biweekly pass, biweekly gaming exposure averages also received code 3; in the monthly pass, wave, gaming, and lag/lead terms remained at code 1 to avoid inflating the predictor count. This coding ensures that both the raw within-person value and its person mean enter the imputation model where appropriate, preserving the between vs. within-person decomposition critical for contextual effects. Continuous outcomes were grand-mean centred before imputation and back-transformed afterwards to improve stability of the mixed-effects models inside `2l.pmm` [@vanbuuren2018flexible]. 

Quality control diagnostics (convergence, density, strip, missingness, and between-imputation variability plots) were generated and inspected for both the panel and diary imputations and found to be acceptable. Level-2 (person-level) predictors included static demographics (age, BMI, SES, region, gender recoded as male/female/other) and chronotype (MSF~sc~); level-1 time-varying predictors included wave number, biweekly gaming exposure averages (total and late-night minutes over the preceding 14 days), and ±1-wave temporal lag and lead terms for each outcome variable to capture within-person trajectories. To ensure each participant contributed a full six-wave panel, we first expanded the self-report data to the complete participant × wave grid and inferred survey timestamps for missing waves by aligning observed dates with wave-specific medians; those inferred dates were then used to compute the rolling gaming exposures. For the primary panel dataset we generated 20 imputed datasets with 20 iterations per pass—sufficient to stabilize estimates given wave-specific missingness rates up to ~56%. Diagnostic density and strip plots confirmed plausible imputations. All regressions were fit separately in each imputed dataset and combined using Rubin’s rules.

Missingness in the adjustment covariates was limited in the original (pre-imputation) analytical dataset; exact per-covariate counts are reported in the Results section below. The imputed outcomes serve as the primary analytic dataset throughout the main text. Complete-case versions of every regression (using only non-imputed observations for each outcome) were re-estimated in parallel and are reported in the Appendix (@tbl-appendix-h1-completecase, @tbl-appendix-h2-completecase, @tbl-appendix-diary-completecase). Wave-level outcome missingness rates are reported in @tbl-appendix-wave-missingness.

# Results


::: {.cell}

:::



::: {.cell}

:::


## Data Quality Controls

Prior to hypothesis testing, we conducted three positive data quality controls (DQCs) to check the directions and magnitudes of three expected associations in the data. First, self-reported playtime was positively correlated with digital trace playtime (r = 0.49, 95% CI [0.47, 0.51], p < .001). Second, social jetlag was positively associated with daytime sleepiness (Spearman's ρ = 0.09, p < .001, one-sided), in the same direction reported for circadian misalignment and sleepiness in prior work [@Fernandes2023jetlag; @Wu2025socialjetlag]. Third, sleep quality was negatively associated with wellbeing (Spearman's ρ = -0.26, p < .001, one-sided), in the same direction reported for sleep and mental health in prior work [@Gadie2016how]. All three associations were in the preregistered direction.

<!-- TODO: include session integrity-related DQCs for Steam, Nintendo, and Xbox ensuring no overlapping sessions on Nintendo/Xbox (within each platform), and no impossible >60 minute intervals for Steam sessions. (DQC 2) -->


## Sample Demographics


::: {.cell}

:::


Of 34,922 participants who signed up for the study, 1,948 completed at least one biweekly panel survey. The analytical sample was derived from these participants by applying a three-step filter. First, participants were required to have at least one valid outcome measure (SWEMWBS, PSQI, or ESS) at wave 1, which excluded 107 participants (*n* = 1,841). Second, participants needed valid timezone data (either self-reported or imputed for UK participants) to accurately classify late-night gaming sessions, which excluded a further 120 participants (*n* = 1,721). Third, participants were required to have contributed at least one valid gaming session during the study period, which excluded 143 additional participants, yielding a final analytical sample of *N* = 1,578. @tbl-demographics presents the demographic composition of both the total survey sample and the analytical sample.


::: {#tbl-demographics .cell tbl-cap='Sample Characteristics'}
::: {.cell-output-display}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_ppushexdxpgvg9qzvsuc = TinyTable.createTableFunctions("tinytable_ppushexdxpgvg9qzvsuc");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '27', j: 2 }, { i: '27', j: 3 } ], css_id: 'tinytable_css_0oasqm9tvgv49k2dfc0f',}, 
          { positions: [ { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '5', j: 2 }, { i: '6', j: 2 }, { i: '7', j: 2 }, { i: '8', j: 2 }, { i: '9', j: 2 }, { i: '10', j: 2 }, { i: '11', j: 2 }, { i: '12', j: 2 }, { i: '14', j: 2 }, { i: '15', j: 2 }, { i: '17', j: 2 }, { i: '18', j: 2 }, { i: '19', j: 2 }, { i: '21', j: 2 }, { i: '22', j: 2 }, { i: '23', j: 2 }, { i: '24', j: 2 }, { i: '25', j: 2 }, { i: '26', j: 2 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '5', j: 3 }, { i: '6', j: 3 }, { i: '7', j: 3 }, { i: '8', j: 3 }, { i: '9', j: 3 }, { i: '10', j: 3 }, { i: '11', j: 3 }, { i: '12', j: 3 }, { i: '14', j: 3 }, { i: '15', j: 3 }, { i: '17', j: 3 }, { i: '18', j: 3 }, { i: '19', j: 3 }, { i: '21', j: 3 }, { i: '22', j: 3 }, { i: '23', j: 3 }, { i: '24', j: 3 }, { i: '25', j: 3 }, { i: '26', j: 3 } ], css_id: 'tinytable_css_4pkerem5l33uleih9jwq',}, 
          { positions: [ { i: '1', j: 2 }, { i: '13', j: 2 }, { i: '16', j: 2 }, { i: '20', j: 2 }, { i: '1', j: 3 }, { i: '13', j: 3 }, { i: '16', j: 3 }, { i: '20', j: 3 } ], css_id: 'tinytable_css_o2w4670xjddnbmtwhbig',}, 
          { positions: [ { i: '0', j: 2 }, { i: '0', j: 3 } ], css_id: 'tinytable_css_06nolb6hj0u45jm9k04r',}, 
          { positions: [ { i: '27', j: 1 } ], css_id: 'tinytable_css_rs4enzn7wxa6igm69yhl',}, 
          { positions: [ { i: '5', j: 1 }, { i: '6', j: 1 }, { i: '7', j: 1 }, { i: '9', j: 1 }, { i: '10', j: 1 }, { i: '15', j: 1 }, { i: '23', j: 1 }, { i: '24', j: 1 }, { i: '26', j: 1 } ], css_id: 'tinytable_css_v29i2e5nk1y8mphshlxz',}, 
          { positions: [ { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '4', j: 1 }, { i: '8', j: 1 }, { i: '11', j: 1 }, { i: '12', j: 1 }, { i: '14', j: 1 }, { i: '17', j: 1 }, { i: '18', j: 1 }, { i: '19', j: 1 }, { i: '21', j: 1 }, { i: '22', j: 1 }, { i: '25', j: 1 } ], css_id: 'tinytable_css_ctsofa1zlh899xopzep3',}, 
          { positions: [ { i: '1', j: 1 }, { i: '13', j: 1 }, { i: '16', j: 1 }, { i: '20', j: 1 } ], css_id: 'tinytable_css_ttbxzv3nehj1379ajvty',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_chy7rlx588d0mkfv9in3',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_ppushexdxpgvg9qzvsuc.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_ppushexdxpgvg9qzvsuc td.tinytable_css_0oasqm9tvgv49k2dfc0f, #tinytable_ppushexdxpgvg9qzvsuc th.tinytable_css_0oasqm9tvgv49k2dfc0f {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: right }
    #tinytable_ppushexdxpgvg9qzvsuc td.tinytable_css_4pkerem5l33uleih9jwq, #tinytable_ppushexdxpgvg9qzvsuc th.tinytable_css_4pkerem5l33uleih9jwq { text-align: right }
    #tinytable_ppushexdxpgvg9qzvsuc td.tinytable_css_o2w4670xjddnbmtwhbig, #tinytable_ppushexdxpgvg9qzvsuc th.tinytable_css_o2w4670xjddnbmtwhbig { font-weight: bold; text-align: right }
    #tinytable_ppushexdxpgvg9qzvsuc td.tinytable_css_06nolb6hj0u45jm9k04r, #tinytable_ppushexdxpgvg9qzvsuc th.tinytable_css_06nolb6hj0u45jm9k04r {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: right }
    #tinytable_ppushexdxpgvg9qzvsuc td.tinytable_css_rs4enzn7wxa6igm69yhl, #tinytable_ppushexdxpgvg9qzvsuc th.tinytable_css_rs4enzn7wxa6igm69yhl {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    #tinytable_ppushexdxpgvg9qzvsuc td.tinytable_css_v29i2e5nk1y8mphshlxz, #tinytable_ppushexdxpgvg9qzvsuc th.tinytable_css_v29i2e5nk1y8mphshlxz { text-align: left; padding-left: 1em }
    #tinytable_ppushexdxpgvg9qzvsuc td.tinytable_css_ctsofa1zlh899xopzep3, #tinytable_ppushexdxpgvg9qzvsuc th.tinytable_css_ctsofa1zlh899xopzep3 { text-align: left }
    #tinytable_ppushexdxpgvg9qzvsuc td.tinytable_css_ttbxzv3nehj1379ajvty, #tinytable_ppushexdxpgvg9qzvsuc th.tinytable_css_ttbxzv3nehj1379ajvty { font-weight: bold; text-align: left }
    #tinytable_ppushexdxpgvg9qzvsuc td.tinytable_css_chy7rlx588d0mkfv9in3, #tinytable_ppushexdxpgvg9qzvsuc th.tinytable_css_chy7rlx588d0mkfv9in3 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_ppushexdxpgvg9qzvsuc" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        <caption>Sample Characteristics</caption>
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1">Characteristic</th>
                <th scope="col" data-row="0" data-col="2">Total</th>
                <th scope="col" data-row="0" data-col="3">Analytical</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='3'>Values are M (SD) unless noted. ¹ Mdn (IQR). LN = late-night.</td></tr></tfoot>
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">A. Sociodemographics</td>
                  <td data-row="1" data-col="2"></td>
                  <td data-row="1" data-col="3"></td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">N</td>
                  <td data-row="2" data-col="2">1948</td>
                  <td data-row="2" data-col="3">1578</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">Age</td>
                  <td data-row="3" data-col="2">26.8 (5.0)</td>
                  <td data-row="3" data-col="3">27.1 (5.1)</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">Gender</td>
                  <td data-row="4" data-col="2"></td>
                  <td data-row="4" data-col="3"></td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">Woman</td>
                  <td data-row="5" data-col="2">518 (26.6%)</td>
                  <td data-row="5" data-col="3">444 (28.1%)</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">Man</td>
                  <td data-row="6" data-col="2">1211 (62.2%)</td>
                  <td data-row="6" data-col="3">1035 (65.6%)</td>
                </tr>
                <tr>
                  <td data-row="7" data-col="1">Other</td>
                  <td data-row="7" data-col="2">111 (5.7%)</td>
                  <td data-row="7" data-col="3">99 (6.3%)</td>
                </tr>
                <tr>
                  <td data-row="8" data-col="1">Region</td>
                  <td data-row="8" data-col="2"></td>
                  <td data-row="8" data-col="3"></td>
                </tr>
                <tr>
                  <td data-row="9" data-col="1">UK</td>
                  <td data-row="9" data-col="2">719 (36.9%)</td>
                  <td data-row="9" data-col="3">672 (42.6%)</td>
                </tr>
                <tr>
                  <td data-row="10" data-col="1">US</td>
                  <td data-row="10" data-col="2">1121 (57.5%)</td>
                  <td data-row="10" data-col="3">906 (57.4%)</td>
                </tr>
                <tr>
                  <td data-row="11" data-col="1">BMI (kg/m²)</td>
                  <td data-row="11" data-col="2">22.0 (7.0)</td>
                  <td data-row="11" data-col="3">22.1 (7.0)</td>
                </tr>
                <tr>
                  <td data-row="12" data-col="1">SES index</td>
                  <td data-row="12" data-col="2">2.27 (0.54)</td>
                  <td data-row="12" data-col="3">2.26 (0.54)</td>
                </tr>
                <tr>
                  <td data-row="13" data-col="1">B. Chronotype</td>
                  <td data-row="13" data-col="2"></td>
                  <td data-row="13" data-col="3"></td>
                </tr>
                <tr>
                  <td data-row="14" data-col="1">No alarm on free days</td>
                  <td data-row="14" data-col="2">1240 (74.4%)</td>
                  <td data-row="14" data-col="3">1141 (74.5%)</td>
                </tr>
                <tr>
                  <td data-row="15" data-col="1">MCTQ-MSFsc (HH:MM)¹</td>
                  <td data-row="15" data-col="2">06:00 (03:23)</td>
                  <td data-row="15" data-col="3">05:52 (03:05)</td>
                </tr>
                <tr>
                  <td data-row="16" data-col="1">C. Gaming</td>
                  <td data-row="16" data-col="2"></td>
                  <td data-row="16" data-col="3"></td>
                </tr>
                <tr>
                  <td data-row="17" data-col="1">Gaming (min/day)¹</td>
                  <td data-row="17" data-col="2">59.6 (138.3)</td>
                  <td data-row="17" data-col="3">83.7 (137.2)</td>
                </tr>
                <tr>
                  <td data-row="18" data-col="1">LN gaming (min/day)¹</td>
                  <td data-row="18" data-col="2">4.4 (24.1)</td>
                  <td data-row="18" data-col="3">9.3 (30.7)</td>
                </tr>
                <tr>
                  <td data-row="19" data-col="1">% nights LN gaming</td>
                  <td data-row="19" data-col="2">13.3 (16.3)</td>
                  <td data-row="19" data-col="3">16.4 (16.7)</td>
                </tr>
                <tr>
                  <td data-row="20" data-col="1">D. Outcomes</td>
                  <td data-row="20" data-col="2"></td>
                  <td data-row="20" data-col="3"></td>
                </tr>
                <tr>
                  <td data-row="21" data-col="1">Sleep (h)</td>
                  <td data-row="21" data-col="2">7.2 (1.2)</td>
                  <td data-row="21" data-col="3">7.2 (1.1)</td>
                </tr>
                <tr>
                  <td data-row="22" data-col="1">PSQI global</td>
                  <td data-row="22" data-col="2">6.7 (2.9)</td>
                  <td data-row="22" data-col="3">6.7 (2.8)</td>
                </tr>
                <tr>
                  <td data-row="23" data-col="1">Sleep quality</td>
                  <td data-row="23" data-col="2">1.3 (0.6)</td>
                  <td data-row="23" data-col="3">1.3 (0.6)</td>
                </tr>
                <tr>
                  <td data-row="24" data-col="1">Poor sleep (PSQI>5)</td>
                  <td data-row="24" data-col="2">793 (63.8%)</td>
                  <td data-row="24" data-col="3">754 (63.8%)</td>
                </tr>
                <tr>
                  <td data-row="25" data-col="1">ESS</td>
                  <td data-row="25" data-col="2">5.6 (3.5)</td>
                  <td data-row="25" data-col="3">5.6 (3.5)</td>
                </tr>
                <tr>
                  <td data-row="26" data-col="1">Excessive sleepiness (ESS>10)</td>
                  <td data-row="26" data-col="2">126 (10.2%)</td>
                  <td data-row="26" data-col="3">120 (10.2%)</td>
                </tr>
                <tr>
                  <td data-row="27" data-col="1">SWEMWBS</td>
                  <td data-row="27" data-col="2">23.2 (5.0)</td>
                  <td data-row="27" data-col="3">23.2 (5.0)</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::


Self-reported sleep duration in the analytical sample was 7.2 hours (SD = 1.1), mean PSQI sleep-quality component scores were 1.3 (SD = 0.6), mean daytime sleepiness was 5.6 on the Epworth Sleepiness Scale (SD = 3.5), and mean wellbeing was 23.2 on the SWEMWBS (SD = 5.0). @fig-raincloud displays the distributions of gaming patterns and outcomes across the analytical sample.


::: {.cell}
::: {.cell-output-display}
![Gaming patterns and outcome distributions in the complete-case analytical sample. (A) Hourly playtime: average minutes played per participant-day (total minutes summed across all sessions divided by the number of contributing participant-days), shown by hour of day, with grouped bars distinguishing weekdays (Sunday-Thursday nights, solid fill) from weekend nights (Friday-Saturday, striped fill); the pre-registered late-night window (23:00-06:00 local time) is delimited by red dashed lines and shaded in red. (B) Self-reported sleep quality (PSQI Item 6) shown as the percentage of person-wave responses in each ordinal category; missing responses are excluded. (C-E) Continuous outcomes displayed as raincloud plots: half-violin shapes show the kernel density estimate (bandwidth multiplier = 2, normalised within panel), boxplots show the median (centre line), interquartile range (box) and whiskers extending to the most extreme value within 1.5 x IQR of the hinges; outliers beyond the whiskers are not plotted. Self-reported sleep duration in panel C is derived from PSQI Item 4. Panel-specific sample sizes are reported in the panel subtitles. Abbreviations: PSQI = Pittsburgh Sleep Quality Index; ESS = Epworth Sleepiness Scale (range 0-24, higher = greater daytime sleepiness); SWEMWBS = Short Warwick-Edinburgh Mental Well-Being Scale (range 7-35, higher = greater wellbeing); IQR = interquartile range.](manuscript_files/figure-html/fig-raincloud-1.png){#fig-raincloud width=1152}
:::
:::



::: {.cell}

:::


Missingness in the adjustment covariates was minimal in the original (pre-imputation) analytical dataset of 1182 participants: BMI was missing for 80 (6.8%), while age, SES index, region, and the weekend/weekday indicator were complete.


## Panel

### H1


::: {.cell}

:::


For sleep quality (H1a), each additional 10 minutes of average daily late-night gaming was associated with a probit-scale change of b = 0.051, 95% CI [0.023, 0.079], p < .001, corresponding to an approximate 0.9 percentage-point increase in the marginal probability of reporting fairly bad or very bad sleep quality (@fig-latenight-sleepquality-exceedance translates this coefficient to the probability scale, with marginal predicted probabilities computed in each of the 20 imputed datasets, pooled via Rubin's rules, and 95% confidence intervals derived from the combined within- and between-imputation variance using the delta method). The corresponding pooled effects were b = -0.005, 95% CI [-0.019, 0.008], p = 0.456 (hours per 10 min/day) for sleep duration (H1b), b = 0.013, 95% CI [-0.034, 0.061], p = 0.577 (Epworth points per 10 min/day) for daytime sleepiness (H1c), and b = -0.004, 95% CI [-0.038, 0.030], p = 0.805 (SWEMWBS points per 10 min/day) for wellbeing (H1d) — each indicating negligible variation in the outcome as a function of late-night play. Full coefficient estimates, confidence intervals, and variance components are reported in the H1 regression summary table.

These estimates depart from the Stage 1 protocol [@ballou2024psychological] in two respects. First, the preregistered by-participant random slopes on late-night minutes were dropped after they produced convergence problems and boundary estimates (near-zero variance components) in the Open Play data — most acutely in the H1a cumulative link mixed model — so all four H1 models retain only random intercepts for participants, and for gender in H1c and H1d where it remained supported. Second, the multiply imputed outcomes (20 datasets pooled via Rubin's rules [@rubin1987multiple]) are used as the primary analytic dataset rather than the incomplete original outcomes. All models additionally adjust for age, BMI, SES index, region, and weekend versus weekday timing, and use rolling 14- and 28-day windows of late-night play anchored to each survey date.


::: {.cell}

:::


Two further sensitivity checks support the linear parameterization used in the preregistered models. The H1a model was refit using the PSQI global score (sum of all seven components, range 0–21) as a continuous alternative to the ordinal item-6 outcome; results are reported in @tbl-appendix-psqi-global. A natural cubic spline sensitivity analysis (@tbl-appendix-panel-h1-spline-bic; @fig-appendix-panel-h1-spline-bic), evaluated by BIC under the rule of @jones2001nagin (|2ΔBIC| < 2 = indifferent, 2–6 = positive evidence), returned outcome-specific verdicts rather than a blanket endorsement of linearity: evidence was indifferent between the linear and spline parameterizations for H1a (sleep quality, |2ΔBIC| = 0.6) and H1d (wellbeing, |2ΔBIC| = 0.6); positive but weak evidence favored the linear specification for H1b (sleep duration, |2ΔBIC| = 3.6); and positive but weak evidence favored a non-linear (spline) specification for H1c (daytime sleepiness, |2ΔBIC| = 2.8). We retained the linear parameterization in the preregistered models for all four outcomes; for H1c on the basis of parsimony and direct comparability with the other H1 models.




::: {.cell}

:::



:::: {.place arguments='top, scope: "parent", float: true'}


::: {#tbl-h1-combined .cell tbl-cap='Summary of H1 Hypotheses: Effects of Late-Night Gaming on Sleep and Wellbeing'}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_ah74vkh29si2yar9bhi5 = TinyTable.createTableFunctions("tinytable_ah74vkh29si2yar9bhi5");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '10', j: 2 }, { i: '10', j: 3 }, { i: '10', j: 4 }, { i: '10', j: 5 } ], css_id: 'tinytable_css_rd76radwfbmsmwsieabj',}, 
          { positions: [ { i: '1', j: 2 }, { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '5', j: 2 }, { i: '6', j: 2 }, { i: '7', j: 2 }, { i: '8', j: 2 }, { i: '9', j: 2 }, { i: '1', j: 3 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '5', j: 3 }, { i: '6', j: 3 }, { i: '7', j: 3 }, { i: '8', j: 3 }, { i: '9', j: 3 }, { i: '1', j: 4 }, { i: '2', j: 4 }, { i: '3', j: 4 }, { i: '4', j: 4 }, { i: '5', j: 4 }, { i: '6', j: 4 }, { i: '7', j: 4 }, { i: '8', j: 4 }, { i: '9', j: 4 }, { i: '1', j: 5 }, { i: '2', j: 5 }, { i: '3', j: 5 }, { i: '4', j: 5 }, { i: '5', j: 5 }, { i: '6', j: 5 }, { i: '7', j: 5 }, { i: '8', j: 5 }, { i: '9', j: 5 } ], css_id: 'tinytable_css_0etd8q25zepzk2c4abuv',}, 
          { positions: [ { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 }, { i: '0', j: 5 } ], css_id: 'tinytable_css_vt5qxt7dpw34tlx7xz8m',}, 
          { positions: [ { i: '10', j: 1 } ], css_id: 'tinytable_css_hjq215z49bk16q1766lv',}, 
          { positions: [ { i: '1', j: 1 }, { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '4', j: 1 }, { i: '5', j: 1 }, { i: '6', j: 1 }, { i: '7', j: 1 }, { i: '8', j: 1 }, { i: '9', j: 1 } ], css_id: 'tinytable_css_5gog47g5x0e2sqokccvr',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_djqpslwpelpn762oifwm',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_ah74vkh29si2yar9bhi5.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_ah74vkh29si2yar9bhi5 td.tinytable_css_rd76radwfbmsmwsieabj, #tinytable_ah74vkh29si2yar9bhi5 th.tinytable_css_rd76radwfbmsmwsieabj {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_ah74vkh29si2yar9bhi5 td.tinytable_css_0etd8q25zepzk2c4abuv, #tinytable_ah74vkh29si2yar9bhi5 th.tinytable_css_0etd8q25zepzk2c4abuv { text-align: center }
    #tinytable_ah74vkh29si2yar9bhi5 td.tinytable_css_vt5qxt7dpw34tlx7xz8m, #tinytable_ah74vkh29si2yar9bhi5 th.tinytable_css_vt5qxt7dpw34tlx7xz8m {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_ah74vkh29si2yar9bhi5 td.tinytable_css_hjq215z49bk16q1766lv, #tinytable_ah74vkh29si2yar9bhi5 th.tinytable_css_hjq215z49bk16q1766lv {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    #tinytable_ah74vkh29si2yar9bhi5 td.tinytable_css_5gog47g5x0e2sqokccvr, #tinytable_ah74vkh29si2yar9bhi5 th.tinytable_css_5gog47g5x0e2sqokccvr { text-align: left }
    #tinytable_ah74vkh29si2yar9bhi5 td.tinytable_css_djqpslwpelpn762oifwm, #tinytable_ah74vkh29si2yar9bhi5 th.tinytable_css_djqpslwpelpn762oifwm {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_ah74vkh29si2yar9bhi5" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        <caption>H1: Effects of Late-Night Gaming on Sleep and Wellbeing (Imputed data)</caption>
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1"> </th>
                <th scope="col" data-row="0" data-col="2">H1a: Sleep Quality</th>
                <th scope="col" data-row="0" data-col="3">H1b: Sleep Duration</th>
                <th scope="col" data-row="0" data-col="4">H1c: Daytime Sleepiness</th>
                <th scope="col" data-row="0" data-col="5">H1d: Wellbeing</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='5'>+ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001</td></tr>
<tr><td colspan='5'>LN = late-night. Confidence intervals shown in brackets.</td></tr>
<tr><td colspan='5'>ICC = Intraclass Correlation Coefficient (adjusted).</td></tr></tfoot>
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">Daily LN gaming (per 10 min/day, monthly)</td>
                  <td data-row="1" data-col="2">0.05 [0.02, 0.08]***</td>
                  <td data-row="1" data-col="3">-0.01 [-0.02, 0.01]</td>
                  <td data-row="1" data-col="4">0.01 [-0.03, 0.06]</td>
                  <td data-row="1" data-col="5"></td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">Daily LN gaming (per 10 min/day, biweekly)</td>
                  <td data-row="2" data-col="2"></td>
                  <td data-row="2" data-col="3"></td>
                  <td data-row="2" data-col="4"></td>
                  <td data-row="2" data-col="5">-0.00 [-0.04, 0.03]</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">Age (scaled)</td>
                  <td data-row="3" data-col="2">0.16 [-0.17, 0.50]</td>
                  <td data-row="3" data-col="3">-0.45 [-0.62, -0.27]***</td>
                  <td data-row="3" data-col="4">-0.68 [-1.27, -0.09]*</td>
                  <td data-row="3" data-col="5">0.07 [-0.59, 0.72]</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">BMI (scaled)</td>
                  <td data-row="4" data-col="2">0.18 [0.05, 0.31]**</td>
                  <td data-row="4" data-col="3">-0.07 [-0.14, -0.00]*</td>
                  <td data-row="4" data-col="4">0.18 [-0.05, 0.41]</td>
                  <td data-row="4" data-col="5">-0.21 [-0.48, 0.07]</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">SES (scaled)</td>
                  <td data-row="5" data-col="2">-0.24 [-0.39, -0.09]**</td>
                  <td data-row="5" data-col="3">-0.09 [-0.16, -0.01]*</td>
                  <td data-row="5" data-col="4">0.09 [-0.17, 0.34]</td>
                  <td data-row="5" data-col="5">0.98 [0.70, 1.26]***</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">Region: US</td>
                  <td data-row="6" data-col="2">-0.14 [-0.37, 0.09]</td>
                  <td data-row="6" data-col="3">0.04 [-0.08, 0.17]</td>
                  <td data-row="6" data-col="4">0.25 [-0.18, 0.69]</td>
                  <td data-row="6" data-col="5">0.19 [-0.29, 0.68]</td>
                </tr>
                <tr>
                  <td data-row="7" data-col="1">Day: Weekend</td>
                  <td data-row="7" data-col="2">0.03 [-0.16, 0.23]</td>
                  <td data-row="7" data-col="3">-0.00 [-0.09, 0.09]</td>
                  <td data-row="7" data-col="4">0.04 [-0.25, 0.33]</td>
                  <td data-row="7" data-col="5">-0.01 [-0.27, 0.25]</td>
                </tr>
                <tr>
                  <td data-row="8" data-col="1">N Obs</td>
                  <td data-row="8" data-col="2">3561</td>
                  <td data-row="8" data-col="3">3553</td>
                  <td data-row="8" data-col="4">3551</td>
                  <td data-row="8" data-col="5">7425</td>
                </tr>
                <tr>
                  <td data-row="9" data-col="1">N Participants</td>
                  <td data-row="9" data-col="2">1294</td>
                  <td data-row="9" data-col="3">1293</td>
                  <td data-row="9" data-col="4">1290</td>
                  <td data-row="9" data-col="5">1469</td>
                </tr>
                <tr>
                  <td data-row="10" data-col="1">ICC</td>
                  <td data-row="10" data-col="2">0.75</td>
                  <td data-row="10" data-col="3">0.69</td>
                  <td data-row="10" data-col="4">0.69</td>
                  <td data-row="10" data-col="5">0.71</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```
:::


::::


::: {.cell}

:::



::: {.cell}
::: {.cell-output-display}
![Marginal predicted probability of poor sleep quality (Fairly bad or Very bad) as a function of late-night gaming. Predictions are derived from the H1a probit cumulative link mixed model fitted separately on each of 20 multiply imputed datasets; predicted probabilities and their within-imputation variances are then pooled via Rubin's rules. The solid line shows the pooled point estimate and the shaded ribbon the 95% confidence interval, both computed on the probability scale using the delta method applied to each imputation's threshold, gaming coefficient, and variance--covariance matrix, then combined with Rubin's within- and between-imputation variance components. Probabilities are population-average (marginalised over the participant random intercept) with other covariates held at their reference or mean values. The top panel shows the marginal density of late-night gaming in the sample; vertical reference lines mark the median, mean, 75th, and 90th percentiles.](manuscript_files/figure-html/fig-latenight-sleepquality-exceedance-1.png){#fig-latenight-sleepquality-exceedance width=864}
:::
:::


We also conducted frequentist equivalence tests (TOST; @lakens2017equivalence) on the focal H1 predictors against a Region of Practical Equivalence (ROPE) derived from ±0.1 × SD(outcome) rescaled to the raw coefficient's native units (see @sec-appendix-equivalence for derivation, the rationale for the ROPE width, and response-scale thresholds. The pooled 90% CIs for H1b, H1c, and H1d fell entirely within their respective ROPEs. The H1a sleep-quality CI lay partly outside the stricter ordinal-probit ROPE, returning an "Undecided" TOST decision. Full per-hypothesis TOST decisions, ROPE widths, and pooled 90% CIs are reported in @tbl-appendix-equivalence.

### H2

Having established a small but consistent late-night gaming → poor-sleep-quality association in H1, we next asked whether this association is moderated by chronotype, on the prior expectation that evening types (higher MSF~sc~) may pay a larger sleep cost for the same late-night play.


::: {.cell}

:::


None of the preregistered chronotype × late-night gaming interactions reached conventional significance after pooling across imputations, so H2 was not supported for any outcome (H2a sleep quality: b = -0.003, 95% CI [-0.013, 0.006], p = 0.487 probit per 10 min/day per hour of MSFsc; H2b sleep duration: b = 0.003, 95% CI [-0.002, 0.008], p = 0.199 hours per 10 min/day per hour of MSFsc; H2c daytime sleepiness: b = -0.009, 95% CI [-0.024, 0.006], p = 0.225 Epworth points per 10 min/day per hour of MSFsc; H2d wellbeing: b = -0.009, 95% CI [-0.019, 0.002], p = 0.101 SWEMWBS points per 10 min/day per hour of MSFsc). Equivalence tests (@tbl-appendix-equivalence) go further and provide positive evidence that the three linear interactions are practically negligible: applying the TOST procedure to the chronotype × late-night gaming coefficients against their interaction-scale ROPEs, the pooled 90% CIs for H2b, H2c, and H2d all lie entirely within the ROPE; the H2a interaction is undecided under the stricter ordinal-probit ROPE. The H2a main effect of late-night gaming was b = 0.064, 95% CI [0.024, 0.105], p = 0.002 (probit per 10 min/day, ≈ 1.0 percentage-point increase in the probability of reporting fairly bad or worse sleep), closely mirroring the H1a estimate and indicating that the small late-night-gaming → sleep-quality association is not concentrated in chronotype-specific subgroups. Full model summaries are reported in @tbl-h2-combined.

The H2 models added a late-night gaming × chronotype (MSF~sc~) interaction to each H1 specification; chronotype had a median of 5.9 hours (IQR 3.1) in the analytical sample and was mean-centred before entering the interaction. The Open Play data made the moderation structure more demanding than H1, so we applied the same random-effects simplification (random intercepts for participants, and for gender where supported, dropping the by-participant random slopes on late-night minutes) while retaining the gaming × chronotype interaction for all four outcomes. With these adjustments the ordinal H2a CLMM converged cleanly alongside the H2b–H2d linear mixed-effects models, with finite standard errors for all fixed effects (including the interaction term) in both the complete-case and imputed fits.





:::: {.place arguments='top, scope: "parent", float: true'}


::: {#tbl-h2-combined .cell tbl-cap='Summary of H2 Hypotheses: Chronotype Moderation of Late-Night Gaming Effects'}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_7824spi95f8nr2cewywe = TinyTable.createTableFunctions("tinytable_7824spi95f8nr2cewywe");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '13', j: 2 }, { i: '13', j: 3 }, { i: '13', j: 4 }, { i: '13', j: 5 } ], css_id: 'tinytable_css_hstsp7494aoswhyg7hik',}, 
          { positions: [ { i: '1', j: 2 }, { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '5', j: 2 }, { i: '6', j: 2 }, { i: '7', j: 2 }, { i: '8', j: 2 }, { i: '9', j: 2 }, { i: '10', j: 2 }, { i: '11', j: 2 }, { i: '12', j: 2 }, { i: '1', j: 3 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '5', j: 3 }, { i: '6', j: 3 }, { i: '7', j: 3 }, { i: '8', j: 3 }, { i: '9', j: 3 }, { i: '10', j: 3 }, { i: '11', j: 3 }, { i: '12', j: 3 }, { i: '1', j: 4 }, { i: '2', j: 4 }, { i: '3', j: 4 }, { i: '4', j: 4 }, { i: '5', j: 4 }, { i: '6', j: 4 }, { i: '7', j: 4 }, { i: '8', j: 4 }, { i: '9', j: 4 }, { i: '10', j: 4 }, { i: '11', j: 4 }, { i: '12', j: 4 }, { i: '1', j: 5 }, { i: '2', j: 5 }, { i: '3', j: 5 }, { i: '4', j: 5 }, { i: '5', j: 5 }, { i: '6', j: 5 }, { i: '7', j: 5 }, { i: '8', j: 5 }, { i: '9', j: 5 }, { i: '10', j: 5 }, { i: '11', j: 5 }, { i: '12', j: 5 } ], css_id: 'tinytable_css_wxod0pbpr77husc2a5h1',}, 
          { positions: [ { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 }, { i: '0', j: 5 } ], css_id: 'tinytable_css_2djf8158t4hqwuaf5tb8',}, 
          { positions: [ { i: '13', j: 1 } ], css_id: 'tinytable_css_d47emmghuf62a6n3oecs',}, 
          { positions: [ { i: '1', j: 1 }, { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '4', j: 1 }, { i: '5', j: 1 }, { i: '6', j: 1 }, { i: '7', j: 1 }, { i: '8', j: 1 }, { i: '9', j: 1 }, { i: '10', j: 1 }, { i: '11', j: 1 }, { i: '12', j: 1 } ], css_id: 'tinytable_css_o17vgydnkf0rmcl1p45x',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_tgz3mwgv0htm8gtesryh',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_7824spi95f8nr2cewywe.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_7824spi95f8nr2cewywe td.tinytable_css_hstsp7494aoswhyg7hik, #tinytable_7824spi95f8nr2cewywe th.tinytable_css_hstsp7494aoswhyg7hik {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_7824spi95f8nr2cewywe td.tinytable_css_wxod0pbpr77husc2a5h1, #tinytable_7824spi95f8nr2cewywe th.tinytable_css_wxod0pbpr77husc2a5h1 { text-align: center }
    #tinytable_7824spi95f8nr2cewywe td.tinytable_css_2djf8158t4hqwuaf5tb8, #tinytable_7824spi95f8nr2cewywe th.tinytable_css_2djf8158t4hqwuaf5tb8 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_7824spi95f8nr2cewywe td.tinytable_css_d47emmghuf62a6n3oecs, #tinytable_7824spi95f8nr2cewywe th.tinytable_css_d47emmghuf62a6n3oecs {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    #tinytable_7824spi95f8nr2cewywe td.tinytable_css_o17vgydnkf0rmcl1p45x, #tinytable_7824spi95f8nr2cewywe th.tinytable_css_o17vgydnkf0rmcl1p45x { text-align: left }
    #tinytable_7824spi95f8nr2cewywe td.tinytable_css_tgz3mwgv0htm8gtesryh, #tinytable_7824spi95f8nr2cewywe th.tinytable_css_tgz3mwgv0htm8gtesryh {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_7824spi95f8nr2cewywe" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        <caption>H2: Chronotype Moderation of Late-Night Gaming Effects (Imputed data)</caption>
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1"> </th>
                <th scope="col" data-row="0" data-col="2">H2a: Sleep Quality</th>
                <th scope="col" data-row="0" data-col="3">H2b: Sleep Duration</th>
                <th scope="col" data-row="0" data-col="4">H2c: Daytime Sleepiness</th>
                <th scope="col" data-row="0" data-col="5">H2d: Wellbeing</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='5'>+ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001</td></tr>
<tr><td colspan='5'>LN = late-night. Confidence intervals shown in brackets.</td></tr>
<tr><td colspan='5'>ICC = Intraclass Correlation Coefficient (adjusted).</td></tr></tfoot>
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">Daily LN gaming (per 10 min/day, monthly)</td>
                  <td data-row="1" data-col="2">0.06 [0.02, 0.11]**</td>
                  <td data-row="1" data-col="3">-0.01 [-0.03, 0.01]</td>
                  <td data-row="1" data-col="4">0.02 [-0.04, 0.09]</td>
                  <td data-row="1" data-col="5"></td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">Daily LN gaming (per 10 min/day, biweekly)</td>
                  <td data-row="2" data-col="2"></td>
                  <td data-row="2" data-col="3"></td>
                  <td data-row="2" data-col="4"></td>
                  <td data-row="2" data-col="5">0.04 [-0.01, 0.08]</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">Chronotype (h, centered)</td>
                  <td data-row="3" data-col="2">0.03 [-0.02, 0.08]</td>
                  <td data-row="3" data-col="3">-0.02 [-0.05, 0.01]</td>
                  <td data-row="3" data-col="4">0.04 [-0.04, 0.12]</td>
                  <td data-row="3" data-col="5">-0.07 [-0.16, 0.03]</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">LN gaming × Chronotype (h, monthly)</td>
                  <td data-row="4" data-col="2">-0.00 [-0.01, 0.01]</td>
                  <td data-row="4" data-col="3">0.00 [-0.00, 0.01]</td>
                  <td data-row="4" data-col="4">-0.01 [-0.02, 0.01]</td>
                  <td data-row="4" data-col="5"></td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">LN gaming × Chronotype (h, biweekly)</td>
                  <td data-row="5" data-col="2"></td>
                  <td data-row="5" data-col="3"></td>
                  <td data-row="5" data-col="4"></td>
                  <td data-row="5" data-col="5">-0.01 [-0.02, 0.00]</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">Age (scaled)</td>
                  <td data-row="6" data-col="2">-0.06 [-0.53, 0.40]</td>
                  <td data-row="6" data-col="3">-0.45 [-0.69, -0.21]***</td>
                  <td data-row="6" data-col="4">-0.93 [-1.71, -0.16]*</td>
                  <td data-row="6" data-col="5">0.14 [-0.74, 1.01]</td>
                </tr>
                <tr>
                  <td data-row="7" data-col="1">BMI (scaled)</td>
                  <td data-row="7" data-col="2">0.06 [-0.10, 0.23]</td>
                  <td data-row="7" data-col="3">-0.04 [-0.13, 0.05]</td>
                  <td data-row="7" data-col="4">0.12 [-0.15, 0.39]</td>
                  <td data-row="7" data-col="5">-0.06 [-0.41, 0.28]</td>
                </tr>
                <tr>
                  <td data-row="8" data-col="1">SES (scaled)</td>
                  <td data-row="8" data-col="2">-0.28 [-0.50, -0.07]**</td>
                  <td data-row="8" data-col="3">-0.09 [-0.19, 0.01]+</td>
                  <td data-row="8" data-col="4">0.13 [-0.20, 0.46]</td>
                  <td data-row="8" data-col="5">1.08 [0.71, 1.44]***</td>
                </tr>
                <tr>
                  <td data-row="9" data-col="1">Region: US</td>
                  <td data-row="9" data-col="2">-0.17 [-0.47, 0.14]</td>
                  <td data-row="9" data-col="3">-0.03 [-0.19, 0.13]</td>
                  <td data-row="9" data-col="4">0.38 [-0.17, 0.93]</td>
                  <td data-row="9" data-col="5">0.30 [-0.34, 0.93]</td>
                </tr>
                <tr>
                  <td data-row="10" data-col="1">Day: Weekend</td>
                  <td data-row="10" data-col="2">0.07 [-0.19, 0.32]</td>
                  <td data-row="10" data-col="3">0.00 [-0.11, 0.12]</td>
                  <td data-row="10" data-col="4">0.04 [-0.31, 0.39]</td>
                  <td data-row="10" data-col="5">-0.06 [-0.38, 0.27]</td>
                </tr>
                <tr>
                  <td data-row="11" data-col="1">N Obs</td>
                  <td data-row="11" data-col="2">2580</td>
                  <td data-row="11" data-col="3">2580</td>
                  <td data-row="11" data-col="4">2580</td>
                  <td data-row="11" data-col="5">5160</td>
                </tr>
                <tr>
                  <td data-row="12" data-col="1">N Participants</td>
                  <td data-row="12" data-col="2">860</td>
                  <td data-row="12" data-col="3">860</td>
                  <td data-row="12" data-col="4">860</td>
                  <td data-row="12" data-col="5">860</td>
                </tr>
                <tr>
                  <td data-row="13" data-col="1">ICC</td>
                  <td data-row="13" data-col="2">0.76</td>
                  <td data-row="13" data-col="3">0.70</td>
                  <td data-row="13" data-col="4">0.70</td>
                  <td data-row="13" data-col="5">0.70</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```
:::


::::

## Diary

### H1 and H2


::: {.cell}

:::



::: {.cell}

:::



::: {.cell}

:::



::: {.cell}

:::



::: {.cell}

:::



::: {.cell}

:::


The diary stage tests whether the panel-level H1a/H2a finding holds when sleep quality and late-night gaming are measured day-by-day, with the trimmed late-night exposure (capped at the 99th percentile) decomposed into a within-person component (daily deviation from the participant's own mean) and a between-person component (participant mean centred at the grand mean).

For the direct-effects model (H1), the within-person effect of late-night gaming on sleep quality was b = -0.003, 95% CI [-0.007, 0.002], p = 0.214 (probit coefficient per 10 additional minutes); on days when participants gamed more than their own average, the association with sleep quality was not statistically significant. The between-person effect was b = 0.029, 95% CI [0.012, 0.047], p = 0.001 (probit coefficient per 10 min/day) and was statistically significant, indicating that participants who habitually engaged in more late-night gaming tended to report worse sleep quality on average. On the probability scale, each additional 10 minutes of habitual daily late-night gaming is associated with an approximate 0.6 percentage-point increase in the probability of reporting poor or very poor sleep, broadly consistent with the panel-level H1a estimate (0.9 pp per 10 min; @fig-diary-sleepquality, Panel A).

For the chronotype moderation model (H2), the interaction between within-person late-night gaming and chronotype was b = -0.002, 95% CI [-0.004, -0.000], p = 0.024 and the interaction between between-person late-night gaming and chronotype was b = -0.001, 95% CI [-0.009, 0.007], p = 0.730. The within-person interaction was statistically significant, indicating that the day-to-day link between late-night gaming and sleep quality was somewhat weaker for evening types; the between-person interaction was not statistically significant, suggesting that the trait-level association between habitual late-night gaming and sleep quality did not meaningfully vary across chronotypes. The main effect of chronotype was b = 0.035, 95% CI [0.005, 0.065], p = 0.022, with a later chronotype associated with worse sleep quality (statistically significant; @fig-diary-sleepquality, Panel B).

These diary analyses were not formally pre-registered and are exploratory; they re-use the H1a/H2a probit CLMM specification (random intercept for participant, late-night gaming scaled per 10 min/day, same covariate set) but operate over the preceding 24 hours rather than the 14- or 28-day windows used in the panel, with sleep quality recorded on a 5-level ordinal scale (Very poor, Poor, Fair, Good, Very good). The analytical diary sample comprised 1271 participants contributing 15,842 diary entries with valid sleep quality ratings (@tbl-diary-demographics in the Appendix). Region was dropped because all diary participants are US-based, and a random intercept for gender failed to converge with only three levels, so gender was excluded as well; continuous covariates (age, BMI, SES) were rescaled within the diary subsample, and SES values missing for participants with unrecognised employment categories (n = 23) were imputed via standard PMM (m = 5) using age, BMI, gender, and region as predictors. Missing diary outcomes and daily predictors were handled with the same hierarchical two-level `2l.pmm` approach and predictor-matrix coding used in the panel imputation, with participants as clusters and diary days nested within (`miceadds`); level-2 variables were age, BMI, SES, chronotype, gender, and person-mean late-night gaming, level-1 daily predictors with genuine missingness (gaming played, basic psychological needs, stress, day type, late-night gaming hours) were imputed via standard PMM within the same MICE run, and ±5-day (rather than ±1-wave) lag and lead terms provided temporal context. We generated 60 imputed datasets with 20 iterations each — more than for the panel because of higher per-day missingness — inspected QC diagnostics and judged them acceptable, and combined diary regression estimates across imputations using Rubin's rules.

We compared the linear specification against natural cubic spline alternatives (df = 2–6). Among the splines, df = 2 was preferred, but the linear model had a lower BIC by 7.7 units (2|ΔBIC| = 15.4) — very strong evidence for the linear specification under the @jones2001nagin rule of thumb. We therefore retained the linear specification, both as a parsimonious summary of the average within- and between-person associations and for direct comparability with the panel H1a/H2a estimates.


::: {.cell}
::: {.cell-output-display}
![Predicted probability of poor sleep quality from the diary CLMM. (A) Between-person effect (H1): marginal probability of reporting Poor or Very poor sleep quality as a function of participants' average daily late-night gaming, with the within-person component held at zero. The ribbon shows 95% CIs from the between-person coefficient SE. (B) Within-person x chronotype interaction (H2): marginal probability of poor sleep quality as a function of daily within-person gaming deviation at three chronotype levels (morning = -1 SD, mean, evening = +1 SD), with the between-person component held at zero. Ribbons show 95% CIs via the delta method pooled across 60 imputations using Rubin's rules. Vertical reference lines mark the median (dotted), mean (dashed), 75th percentile (dotted), and 90th percentile (dashed) of the respective predictor distributions. Density plots (top) show predictor distributions.](manuscript_files/figure-html/fig-diary-sleepquality-1.png){#fig-diary-sleepquality width=1248}
:::
:::



::: {#tbl-diary-h1h2 .cell tbl-cap='Diary CLMM Regression Results: H1 (Direct Effects) and H2 (Chronotype Moderation) for Sleep Quality'}
::: {.cell-output-display}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_e0j5tu3s8be3aokzx18v = TinyTable.createTableFunctions("tinytable_e0j5tu3s8be3aokzx18v");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '12', j: 2 }, { i: '12', j: 3 } ], css_id: 'tinytable_css_2ydk7ibp6t9x4hgp1f10',}, 
          { positions: [ { i: '1', j: 2 }, { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '5', j: 2 }, { i: '6', j: 2 }, { i: '7', j: 2 }, { i: '8', j: 2 }, { i: '9', j: 2 }, { i: '10', j: 2 }, { i: '11', j: 2 }, { i: '1', j: 3 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '5', j: 3 }, { i: '6', j: 3 }, { i: '7', j: 3 }, { i: '8', j: 3 }, { i: '9', j: 3 }, { i: '10', j: 3 }, { i: '11', j: 3 } ], css_id: 'tinytable_css_5o1p6agmhzvcolb5qhgi',}, 
          { positions: [ { i: '0', j: 2 }, { i: '0', j: 3 } ], css_id: 'tinytable_css_li6vtx5nvsmddbe1eenl',}, 
          { positions: [ { i: '12', j: 1 } ], css_id: 'tinytable_css_p37c89beco2sl5mh5uw0',}, 
          { positions: [ { i: '1', j: 1 }, { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '4', j: 1 }, { i: '5', j: 1 }, { i: '6', j: 1 }, { i: '7', j: 1 }, { i: '8', j: 1 }, { i: '9', j: 1 }, { i: '10', j: 1 }, { i: '11', j: 1 } ], css_id: 'tinytable_css_i7tomtox9mlgu1dnu3za',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_7n1msd84pstu81fx4yw1',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_e0j5tu3s8be3aokzx18v.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_e0j5tu3s8be3aokzx18v td.tinytable_css_2ydk7ibp6t9x4hgp1f10, #tinytable_e0j5tu3s8be3aokzx18v th.tinytable_css_2ydk7ibp6t9x4hgp1f10 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_e0j5tu3s8be3aokzx18v td.tinytable_css_5o1p6agmhzvcolb5qhgi, #tinytable_e0j5tu3s8be3aokzx18v th.tinytable_css_5o1p6agmhzvcolb5qhgi { text-align: center }
    #tinytable_e0j5tu3s8be3aokzx18v td.tinytable_css_li6vtx5nvsmddbe1eenl, #tinytable_e0j5tu3s8be3aokzx18v th.tinytable_css_li6vtx5nvsmddbe1eenl {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_e0j5tu3s8be3aokzx18v td.tinytable_css_p37c89beco2sl5mh5uw0, #tinytable_e0j5tu3s8be3aokzx18v th.tinytable_css_p37c89beco2sl5mh5uw0 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    #tinytable_e0j5tu3s8be3aokzx18v td.tinytable_css_i7tomtox9mlgu1dnu3za, #tinytable_e0j5tu3s8be3aokzx18v th.tinytable_css_i7tomtox9mlgu1dnu3za { text-align: left }
    #tinytable_e0j5tu3s8be3aokzx18v td.tinytable_css_7n1msd84pstu81fx4yw1, #tinytable_e0j5tu3s8be3aokzx18v th.tinytable_css_7n1msd84pstu81fx4yw1 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_e0j5tu3s8be3aokzx18v" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1"> </th>
                <th scope="col" data-row="0" data-col="2">H1: Sleep Quality</th>
                <th scope="col" data-row="0" data-col="3">H2: Chronotype Moderation</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='3'>+ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001</td></tr>
<tr><td colspan='3'>LN = late-night. Confidence intervals shown in brackets below each estimate.</td></tr>
<tr><td colspan='3'>Estimates pooled across 60 multiply imputed datasets using Rubin's rules.</td></tr>
<tr><td colspan='3'>Cumulative link mixed models (random intercept for participant) on 5-level ordinal sleep quality (positive coefficients = higher probability of worse sleep). Both H1 and H2 use probit link. Late-night gaming expressed per 10 minutes; chronotype in centered hours; age, BMI, SES scaled within the diary subsample. Region excluded: all diary participants are US-only.</td></tr>
<tr><td colspan='3'>Within = daily deviation from person mean; Between = person mean - grand mean.</td></tr></tfoot>
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">LN gaming within-person (per 10 min)</td>
                  <td data-row="1" data-col="2">-0.00<br>[-0.01, 0.00]</td>
                  <td data-row="1" data-col="3">0.00<br>[-0.00, 0.01]</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">LN gaming between-person (per 10 min)</td>
                  <td data-row="2" data-col="2">0.03**<br>[0.01, 0.05]</td>
                  <td data-row="2" data-col="3">0.02<br>[-0.01, 0.05]</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">Chronotype (h, centered)</td>
                  <td data-row="3" data-col="2"></td>
                  <td data-row="3" data-col="3">0.04*<br>[0.00, 0.07]</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">LN within × Chronotype</td>
                  <td data-row="4" data-col="2"></td>
                  <td data-row="4" data-col="3">-0.00*<br>[-0.00, -0.00]</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">LN between × Chronotype</td>
                  <td data-row="5" data-col="2"></td>
                  <td data-row="5" data-col="3">-0.00<br>[-0.01, 0.01]</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">Age (scaled)</td>
                  <td data-row="6" data-col="2">0.01<br>[-0.06, 0.07]</td>
                  <td data-row="6" data-col="3">-0.06<br>[-0.16, 0.04]</td>
                </tr>
                <tr>
                  <td data-row="7" data-col="1">BMI (scaled)</td>
                  <td data-row="7" data-col="2">0.07*<br>[0.01, 0.14]</td>
                  <td data-row="7" data-col="3">0.08<br>[-0.02, 0.18]</td>
                </tr>
                <tr>
                  <td data-row="8" data-col="1">SES (scaled)</td>
                  <td data-row="8" data-col="2">-0.13***<br>[-0.20, -0.07]</td>
                  <td data-row="8" data-col="3">-0.17**<br>[-0.28, -0.07]</td>
                </tr>
                <tr>
                  <td data-row="9" data-col="1">Day: Weekend</td>
                  <td data-row="9" data-col="2">-0.29***<br>[-0.33, -0.24]</td>
                  <td data-row="9" data-col="3">-0.28***<br>[-0.34, -0.22]</td>
                </tr>
                <tr>
                  <td data-row="10" data-col="1">N Obs</td>
                  <td data-row="10" data-col="2">14690</td>
                  <td data-row="10" data-col="3">8399</td>
                </tr>
                <tr>
                  <td data-row="11" data-col="1">N Participants</td>
                  <td data-row="11" data-col="2">1132</td>
                  <td data-row="11" data-col="3">509</td>
                </tr>
                <tr>
                  <td data-row="12" data-col="1">ICC</td>
                  <td data-row="12" data-col="2">0.48</td>
                  <td data-row="12" data-col="3">0.53</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::



::: {.cell}

:::


# Discussion

This preregistered study examined whether late-night gaming is associated with poorer sleep quality, shorter sleep duration, greater daytime sleepiness, and lower wellbeing among adult gamers, and whether chronotype moderates these associations. Using objective telemetry linked to repeated self-report surveys, we found a consistent but small association between late-night gaming and sleep quality and little evidence that late-night gaming affects sleep duration, daytime sleepiness, or wellbeing directly. Under the same, pooled multiple-imputation analysis, chronotype did not meaningfully moderate any of the sleep- or wellbeing-related associations.

Of the four preregistered direct-effect hypotheses (H1a–H1d), only the association between late-night gaming and sleep quality was supported (H1a). Across the panel models (@tbl-h1-combined), each additional 10 minutes of average daily late-night gaming was associated with an approximate 0.9 percentage-point increase in the marginal probability of reporting fairly bad or very bad sleep quality. This estimate was consistent across analytic specifications: the complete-case panel model (@tbl-appendix-h1-completecase) produced a closely matching 1.0 percentage-point increase; the exploratory diary between-person component (@tbl-diary-h1h2) (capturing stable individual differences in habitual late-night gaming) gave a comparable 0.6 percentage-point increase in the probability of reporting poor or very poor daily sleep.

A sensitivity analysis substituting the continuous PSQI global score (@tbl-appendix-psqi-global) yielded directionally consistent positive fixed effects of late-night gaming on total PSQI points (imputed: b = 0.089, 95% CI [0.055, 0.123], p < .001; complete-case: b = 0.036, 95% CI [0.002, 0.070]), where the coefficient denotes the change in PSQI global score per additional 10 late-night minutes per day. Translated to the probability scale, these correspond to an approximate 1.0 (imputed) / 0.4 (complete-case) percentage-point increase in the probability of exceeding the PSQI poor-sleeper cutoff (> 5) at the sample mean. Linearity checks for the primary H1a specification, comparing natural-spline alternatives to the linear form on the gaming-exposure term, produced similar slopes at representative exposure levels (@tbl-appendix-panel-h1-spline-bic; @fig-appendix-panel-h1-spline-bic), supporting the preregistered linear parameterisation.

The convergence of these findings across different operationalisations of sleep quality, analytic strategies, and study designs strengthens our confidence that the association, though small, is genuine. Our observational design does not permit causal inference: although the association is robust across specifications, we cannot determine whether late-night gaming itself degrades sleep quality, whether poor sleepers are drawn to late-night gaming, or whether a third, uncontrolled factor drives both.

While remaining direct-effect hypotheses were not supported, these are not merely underpowered nulls: frequentist equivalence tests (TOST; @lakens2017equivalence) allow us to actively accept the null hypothesis of a negligible effect. For all three non-significant direct effects (H1b–H1d), the 90% confidence intervals fell entirely within the region of practical equivalence (±0.1 × SD(*y*); see @tbl-appendix-equivalence), providing positive evidence that any true effects of late-night gaming in this sample are too small to be of practical consequence. In substantive terms, we can rule out effects larger than roughly ±7 minutes of nightly sleep per one-SD increase in late-night gaming (H1b) — far below the amount that would plausibly affect next-day functioning; ±0.38 points on the 0–24 Epworth Sleepiness Scale (H1c) — much smaller than the ~5-point gap separating normal from clinically excessive daytime sleepiness [@johns1991new]; and ±0.53 points on the SWEMWBS wellbeing scale (H1d), well under the ~1 to 3-point change regarded as clinically meaningful [@maheswaran2012evaluating]. In other words, even the largest effects compatible with our data would be undetectable against the everyday variability in sleep, mood, and alertness.

These findings speak to the sleep displacement hypothesis, which posits that late-night gaming displaces sleep time, thereby shortening sleep duration and producing daytime sleepiness [@twenge2019more; @lemola2011habitual; @exelmans2015sleep]. Reduced sleep duration, the core prediction of that account is directly contradicted by our equivalence-confirmed null for H1b, and the absence of any detectable effect on daytime sleepiness (H1c) removes a key downstream consequence that would be expected if meaningful displacement were occurring. Taken together, the pattern of results argues against a displacement explanation: sleep duration and alertness were preserved, while subjective sleep quality alone was modestly worse. The residual quality effect is multiply realisable: at least three non-displacement mechanisms predict the same "quality down, duration preserved" signature. Pre-sleep physiological arousal or cognitive stimulation from gaming [@king2013impact] is one; an attribution mechanism whereby players who game late at night perceive their sleep as poorer regardless of whether its duration or architecture is objectively altered is a second; and compensatory phase delay, in which gamers delay rather than shorten their sleep window so that duration and alertness are preserved but the sleep occurs at a circadian phase misaligned with their work-week schedule — leaving subjective quality degraded even when the hours are intact, is a third. The classical arousal account is not unambiguously preferred among these: it typically predicts disrupted sleep architecture — slow-wave and REM suppression and lengthened sleep-onset latency — with downstream daytime costs such as poorer next-day memory and alertness [@dworak2007impact; @higuchi2005effects; @weaver2010effect], and our equivalence-confirmed null on daytime sleepiness (H1c) does not see that cost. The data therefore rule against displacement and are compatible with, but do not positively single out, any one of the surviving candidates; self-report instruments alone cannot adjudicate among them. This quality-without-duration dissociation is mirrored in contemporary adult evidence. A nationally representative US survey found that pre-bedtime device use was associated with poorer self-reported sleep quality and greater sleep deficits but showed no association with sleep duration after adjustment for sociodemographic covariates [@carlson2026technology]; another adult sample contrasting habitual gamers (≥7 h/week) with occasional and non-gamers found no group differences in PSQI global score, sleep duration, or daytime sleepiness, and weekly hours of play actually predicted lower daytime sleepiness and dysfunction in the continuous analyses [@derosa2023habitual]. Yet another adult cohort sits at the edge of this pattern, reporting elevated odds of excessive daytime sleepiness for adults who often play games before bed [@schrempft2024bedtime]; the divergence may reflect design choices (dichotomised Epworth cutoff, categorical Likert exposure rather than a continuous one) that concentrate signal in heavy users in a way our continuous, population-mean equivalence test explicitly bounds.

The null finding for wellbeing (H1d) sits within a growing body of trace-data evidence that objectively logged gameplay is at most weakly related to mental wellbeing. A narrative review of 13 proposed mechanisms argues that gaming operates through offsetting pathways that may involve relaxation, need satisfaction, and social connection on one side; displacement of sleep, exercise, and social activity, plus pre-sleep arousal on the other, making a small or null net association with global wellbeing the expected result [@ballou2024mechanisms]. The empirical record agrees: large telemetry studies consistently find trivial playtime–wellbeing links [@johannes2021video; @vuorre2022time; @ballou2025perceived], with perceived value of play and not hours logged driving wellbeing outcomes. Our null fits this pattern: the hedonic benefits of late-night gaming appear to offset any modest cost routed through perceived sleep quality.

## Chronotype Moderation

The preregistered moderation hypotheses (H2) predicted that evening chronotype would amplify the negative associations between late-night gaming and all four outcomes. These predictions were grounded in theories of circadian misalignment and social jetlag, which hold that evening chronotypes are especially affected by late-night stimulation because it exacerbates the existing misalignment between their endogenous sleep–wake rhythm and socially imposed schedules [@zhong2025electronic; @kortesoja2023latenight]. None of the preregistered moderation hypotheses were supported under the pooled multiple-imputation analysis: the chronotype × late-night gaming interaction was non-significant for sleep quality (H2a: b = -0.003, 95% CI [-0.013, 0.006], p = 0.487), sleep duration (H2b: b = 0.003, 95% CI [-0.002, 0.008], p = 0.199), daytime sleepiness (H2c: b = -0.009, 95% CI [-0.024, 0.006], p = 0.225), and wellbeing (H2d: b = -0.009, 95% CI [-0.019, 0.002], p = 0.101).

Frequentist equivalence tests with a ROPE rescaled to the coefficient's native units (±0.1 × SD(*y*) / SD(*x*); see @tbl-appendix-equivalence) supported practical equivalence for the H2b, H2c, and H2d interactions. The H2a interaction was undecided: the 90% CI was narrow but extended marginally beyond the lower bound of the latent-scale ROPE.

The exploratory diary data told a similar story at the trait level: the between-person chronotype × late-night gaming interaction was small and non-significant (b = -0.001, 95% CI [-0.009, 0.007], p = 0.730), indicating that the habitual association between late-night gaming and nightly sleep quality did not meaningfully vary across chronotypes. The within-person interaction did reach conventional significance (b = -0.002, 95% CI [-0.004, -0.000], p = 0.024), suggesting that on days when evening-type participants gamed more than usual, the day-to-day link to sleep quality was paradoxically somewhat weaker rather than stronger; a finding that runs against the preregistered prediction and that we treat as exploratory. Notably, the diary model did show a significant main effect of chronotype on nightly sleep quality (b = 0.035, 95% CI [0.005, 0.065], p = 0.022), with later chronotypes reporting worse sleep on average. A pattern of trait-level chronotype penalty for sleep quality, but no amplification of the late-night-gaming effect by chronotype is consistent with the broader circadian-misalignment literature without supporting the more specific claim that evening types are differentially harmed by late-night play.

The largely null moderation results have several possible explanations. Evening-type gamers may have developed coping mechanisms or adapted sleep routines that buffer them against the effects of late-night play, for instance, by habitually sleeping in later or napping to compensate. More likely, the circadian misalignment pathway may simply be weaker than previously assumed in adult populations who have more autonomy over their schedules than the adolescent samples on which much of the chronotype–technology literature is based [@bruni2015technology; @reardon2023adolescent]. Consensus on how to operationalise problematic gaming in adults with greater scheduling autonomy is also still being established [@costa2019current; @mannikko2020problematic], which complicates direct read-across from adolescent-focused effect estimates. This reading is corroborated by independent adult-cohort evidence: @schrempft2024bedtime explicitly tested chronotype as both a mediator and a moderator of the bedtime-media-sleep link in 4,188 adults, and found that evening chronotypes engaged in pre-sleep media more frequently but were not differentially harmed by it, exactly the pattern our pre-registered H2 equivalence tests recover.

## Contextualising the effect

Viewed at the per-person, per-night scale, the association is small in absolute terms. At the marginal rate of 0.9 percentage points per 10 minutes, even a gamer averaging an hour of play past 11pm would see only a roughly 5 percentage-point increase in the probability of reporting poor sleep quality. Against the backdrop of 64% of participants already qualifying as poor sleepers by PSQI criteria, late-night gaming appears to be a minor per-person contributor to an issue with broader origins that likely involves work schedules, general screen use, caffeine consumption, and other lifestyle factors not captured here.

It is useful to anchor the implied per-hour magnitude against other modifiable lifestyle influences on adult sleep. At roughly 5 percentage points per hour, the sleep-quality cost of late-night gaming appears to be in the same broad range as the perceived sleep-quality cost reported for a high (400 mg) dose of caffeine consumed within four hours of bedtime [@gardiner2025dose]. This comparison is most directly interpretable for our exploratory diary substudy, which used a structurally similar single-item nightly sleep-quality rating; there, the between-person estimate (0.6 percentage points per 10 min of habitual late-night gaming) aligned closely with the panel PSQI result. A comparable benchmark on the continuous PSQI scale comes from @yan2024chronobiological, who report a ≈0.39-point higher PSQI total score for adults in the latest vs earliest tertile of last-meal time (a ≈3-hour shift). For a contrast of similar practical magnitude in our data — moving from ~10 min/day to ~3 h/day of late-night gaming — the panel PSQI sensitivity model implies a ≈1.52-point increase, of comparable order but somewhat smaller, suggesting that the per-person sleep-quality penalty of habitual late-night gaming sits below that of habitually late evening meal timing.

That we found this effect specifically for sleep quality — rather than duration, sleepiness, or wellbeing — narrows the field to mechanisms that disturb perceived sleep without producing a detectable downstream cost, namely the three candidates flagged above (pre-sleep arousal, attribution, and compensatory phase delay). The exploratory diary decomposition further constrains which of these is most plausible. The H1a-analog signal is carried almost entirely by the between-person component (b = 0.029, 95% CI [0.012, 0.047], p = 0.001), with the within-person, same-night estimate non-significant (b = -0.003, 95% CI [-0.007, 0.002], p = 0.214): habitually late-night-gaming participants reported worse sleep on average, but nights on which a given participant gamed more than their own average were not reliably followed by worse sleep that night. An acute pre-sleep arousal mechanism predicts precisely the latter within-person, same-night degradation, so the diary pattern sits more comfortably with trait-level processes such as a durable attributional style, stable lifestyle confounding, or selection of more arousable players into habitual late-night play than with acute arousal per se. Because our sleep measures are all self-reported, we cannot distinguish an attributional process from a genuine change in sleep architecture; wearable-based ambulatory measurement would be needed to adjudicate.

A small per-night effect can nonetheless carry weight when it is sustained over time and aggregated across a common behaviour. Restricting sleep by only about an hour can still disturb emotion regulation [@tomaso2021effect] and undermine cognitive and behavioural performance [@belenky2003patterns]; if that restriction is maintained for two weeks, losses in alertness and working memory can rival those seen after an entire night without sleep [@vandongen2003cumulative]. Repeated small shifts in nightly sleep quality from habitual late-night play may therefore matter for mood, vigilance, and cognition over weeks and months, even if any single night looks unremarkable. Benchmarks tailored to sleep outcomes support this interpretation: Panjeh and colleagues' recalibration from 72 effect sizes in 65 sleep-quality intervention trials [@panjeh2023establishing] maps "small", "medium", and "large" to Cohen's *d* of roughly 0.18, 0.33, and 0.56 — below the usual 0.2 / 0.5 / 0.8 anchors. Judged against those sleep-specific standards, the cumulative impact of 1–2 hours of late-night gaming falls in the small-to-medium band, whereas the same pattern might look small on the generic Cohen scale.

Population burden adds another layer. De Rosa and colleagues [-@derosa2024videogaming] note that video gaming is now one of the most widespread adult leisure exposures and that sleep outcomes track how intensively and how often people play, so a shallow per-night gradient can still shift the upper tail of poor sleep when layered onto a common behaviour. Public-health importance depends on both per-person effect magnitude and how prevalent the behaviour is and on this combined view, our findings sit alongside a broader literature reporting statistically detectable but per-person modest associations between digital media use and psychological outcomes.

A key gap in the existing literature is the near-complete absence of timing-specific measurement: @kristensen2021problematic noted that none of the studies in their systematic review registered the time of day gaming took place. Most adult gaming-and-sleep studies consequently operationalise exposure as aggregate daily volume or as binary/categorical problematic-gaming contrasts, which are likely to dilute any signal concentrated in the pre-sleep window. A parallel gap arises at the device-specific level: even a recent nationally representative US adult survey [@carlson2026technology] collapsed pre-bedtime TV and gaming-console use into a single category, an acknowledged limitation given that passively watching TV and actively playing video games imply very different levels of interactivity and likely have differential impacts on sleep. Our telemetry-based, continuous, time-of-day-specific operationalisation of late-night play is deliberately designed to isolate exactly that window, and the fact that we find a small but robust association only for sleep quality is consistent with reviews identifying pre-sleep timing, arousal, and session duration as key moderators of gaming's effect on sleep [@derosa2024videogaming; @kemp2021sleep]. That the effect is detectable at all under a stringent, preregistered, multiply-imputed longitudinal specification suggests that timing-specific exposures capture signal that aggregate-volume measures may miss.

## Limitations

Constraints bound the scope of our findings. First, our telemetry captures console and PC play but excludes mobile platforms, which account for a substantial share of late-night leisure — particularly in-bed use that bypasses "household curfews" and serves as a pre-sleep emotional-regulation tool. Late-night minutes that shifted to phones therefore go unmeasured, likely attenuating observed associations, and we cannot separate gaming-specific effects from the broader bundle of nocturnal screen behaviours (short-form video, messaging, doomscrolling) that share the same devices.

Second, even within the platforms we do observe, session-level logs cannot distinguish active engagement from idle time, an ambiguity that is especially acute at night when consoles may stay on while players drift off or step away. This can inflate exposure and produce spurious late-night "activity" that does not reflect cognitive arousal. Relatedly, session logs do not record game content or in-session intensity, so arousal-inducing genres (competitive, horror, fast-paced action) cannot be separated from more routine play — a moderator that adult-focused reviews identify as plausibly shaping sleep effects [@derosa2024videogaming; @kemp2021sleep].

Third, the analytic sample is restricted to adults recruited from UK- and US-based Prolific panels. Younger players have different sleep physiology, school schedules, parental oversight, and motivational profiles, and our estimates should not be extrapolated to pediatric or older populations. Prolific panels themselves are self-selected online workers with reliable broadband, high digital literacy, and tolerance for repeated surveys; eligibility filters around residency, language, and compliance further exclude players in other linguistic and regulatory contexts, as well as shift workers, caregivers, lower-income players, and those engaged in more extreme or stigmatized gaming profiles. Taken together, these constraints mean our findings speak most directly to digitally engaged young adults in the UK and US who play on major console and PC ecosystems; extension to other regions, life stages, device ecologies should await complementary data sources.

## Future Directions

Future work would benefit from combining telemetry with finer-grained metadata on game content, arousal level, and session proximity to bedtime, and from extending logging to mobile platforms so that the full pre-sleep screen bundle can be measured rather than inferred. Linking telemetry to ambulatory physiological measurements (e.g., consumer-grade actigraphy, heart-rate variability, or ambient light sensing) would additionally allow researchers to separate subjective perception of sleep disturbance from objective architecture, which our self-report instruments could not disentangle, and would be the natural way to adjudicate among the three non-displacement candidates the present design leaves on the table — acute pre-sleep arousal (which predicts within-person, same-night HRV elevation, lengthened sleep-onset latency, and reduced slow-wave activity), attribution (which predicts no such physiological signature), and compensatory phase delay (which predicts a within-person delay in actigraphic sleep onset and offset without a duration loss). Such measurements would also index mechanistic pathways our session-level logs cannot — notably evening light exposure, which has been proposed as a contributor to gaming-related sleep disturbance alongside physiological arousal [@hartmann2019effects]. The rapid growth of consumer wearables (smartwatches, fitness trackers, and rings) now reach a substantial share of the populations from which gaming cohorts are recruited makes participant-initiated data donation an increasingly tractable route to such measurements. Rather than provisioning research-grade devices to every participant, future telemetry studies could invite players to donate sleep and activity records from devices they already wear, mirroring the platform-level donation model used here for play behaviour. This would lower per-participant cost, extend follow-up windows beyond what dedicated instrumentation typically allows, and bring objective sleep estimates into the same within-person, high-frequency frame as the telemetry itself, while raising its own questions about device heterogeneity, missingness, and the selection pressures introduced by wearable ownership.

Second, the observational design of this study limits the strength of causal claims that can be drawn from any single estimate. One natural next step is to embed telemetry-based exposures within a target trial emulation framework [@hernan2016using; @hernan2022target], which makes the hypothetical randomized experiment explicit (e.g., a late-night play session versus none on a given weekday). Target trial emulation is particularly well suited to device-use research because the relevant question is rarely "do gamers sleep worse than non-gamers" but rather "does the same person sleep worse on nights they play late than on nights they do not." High-frequency telemetry naturally supports such within-person contrasts, and pairing it with g-methods or sequential exchangeability assumptions would allow future work to move from associational gradients toward more interpretable, policy-relevant causal estimates while keeping each individual as their own control.

The telemetry–survey linkage protocol piloted in Open Play is, in principle, portable: with appropriate ethical safeguards, assent procedures, and clinical validation, an analogous continuous-telemetry approach could be used to study adolescents and problematic gamers, populations in which the timing-specific effects of late-night play are most hotly contested and where aggregate self-report measures are especially vulnerable to recall bias. Extending the present methodology to those populations rather than extrapolating our estimates to them is, in our view, the most productive next step.

# Data, Code, and Materials Availability

The parent Open Play dataset [@ballou2025openplay], including the Nintendo, Xbox, and Steam telemetry and the intake, biweekly, and daily survey tables used here, is openly released at <https://github.com/digital-wellbeing/open-play>. All analytical code for this study is openly released under MIT (code) and CC BY 4.0 (text) at <https://github.com/digital-wellbeing/platform-study-rr-sleep>.

# References

:::{#refs}
:::

```{=typst}
#show: appendix.with()

// Override the extension's "A<h>.<n>" figure numbering to a flat "A<n>"
// so appendix figures/tables read as Fig A1, Fig A2, Table A1, ...
// Reset counters to zero so numbering starts at A1 regardless of main-text count.
#counter(figure.where(kind: "quarto-float-fig")).update(0)
#counter(figure.where(kind: "quarto-float-tbl")).update(0)
#counter(figure.where(kind: image)).update(0)
#counter(figure.where(kind: table)).update(0)

#set figure(numbering: n => "A" + str(n))
```

# Appendix

## Panel H1 Sensitivity Analysis: Complete-Case


::: {#tbl-appendix-h1-completecase .cell tbl-cap='Panel H1 Effects of Late-Night Gaming — Complete-Case (Non-Imputed Data)'}
::: {.cell-output-display}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_u6uhv4gz6i8ouotmgsyt = TinyTable.createTableFunctions("tinytable_u6uhv4gz6i8ouotmgsyt");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '13', j: 2 }, { i: '13', j: 3 }, { i: '13', j: 4 }, { i: '13', j: 5 } ], css_id: 'tinytable_css_eds2zcmjeiawm3y5e6h4',}, 
          { positions: [ { i: '1', j: 2 }, { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '5', j: 2 }, { i: '6', j: 2 }, { i: '7', j: 2 }, { i: '8', j: 2 }, { i: '9', j: 2 }, { i: '10', j: 2 }, { i: '11', j: 2 }, { i: '12', j: 2 }, { i: '1', j: 3 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '5', j: 3 }, { i: '6', j: 3 }, { i: '7', j: 3 }, { i: '8', j: 3 }, { i: '9', j: 3 }, { i: '10', j: 3 }, { i: '11', j: 3 }, { i: '12', j: 3 }, { i: '1', j: 4 }, { i: '2', j: 4 }, { i: '3', j: 4 }, { i: '4', j: 4 }, { i: '5', j: 4 }, { i: '6', j: 4 }, { i: '7', j: 4 }, { i: '8', j: 4 }, { i: '9', j: 4 }, { i: '10', j: 4 }, { i: '11', j: 4 }, { i: '12', j: 4 }, { i: '1', j: 5 }, { i: '2', j: 5 }, { i: '3', j: 5 }, { i: '4', j: 5 }, { i: '5', j: 5 }, { i: '6', j: 5 }, { i: '7', j: 5 }, { i: '8', j: 5 }, { i: '9', j: 5 }, { i: '10', j: 5 }, { i: '11', j: 5 }, { i: '12', j: 5 } ], css_id: 'tinytable_css_t5i9spbxylzx8qdabv4h',}, 
          { positions: [ { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 }, { i: '0', j: 5 } ], css_id: 'tinytable_css_w12dc01th9p5xu99m1eg',}, 
          { positions: [ { i: '13', j: 1 } ], css_id: 'tinytable_css_1lc7pgr60olyhe9fwn7t',}, 
          { positions: [ { i: '1', j: 1 }, { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '4', j: 1 }, { i: '5', j: 1 }, { i: '6', j: 1 }, { i: '7', j: 1 }, { i: '8', j: 1 }, { i: '9', j: 1 }, { i: '10', j: 1 }, { i: '11', j: 1 }, { i: '12', j: 1 } ], css_id: 'tinytable_css_nun0z9ox5ihokutkzluv',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_rr7nko4e8bmq4wgrj6w0',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_u6uhv4gz6i8ouotmgsyt.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_u6uhv4gz6i8ouotmgsyt td.tinytable_css_eds2zcmjeiawm3y5e6h4, #tinytable_u6uhv4gz6i8ouotmgsyt th.tinytable_css_eds2zcmjeiawm3y5e6h4 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_u6uhv4gz6i8ouotmgsyt td.tinytable_css_t5i9spbxylzx8qdabv4h, #tinytable_u6uhv4gz6i8ouotmgsyt th.tinytable_css_t5i9spbxylzx8qdabv4h { text-align: center }
    #tinytable_u6uhv4gz6i8ouotmgsyt td.tinytable_css_w12dc01th9p5xu99m1eg, #tinytable_u6uhv4gz6i8ouotmgsyt th.tinytable_css_w12dc01th9p5xu99m1eg {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_u6uhv4gz6i8ouotmgsyt td.tinytable_css_1lc7pgr60olyhe9fwn7t, #tinytable_u6uhv4gz6i8ouotmgsyt th.tinytable_css_1lc7pgr60olyhe9fwn7t {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    #tinytable_u6uhv4gz6i8ouotmgsyt td.tinytable_css_nun0z9ox5ihokutkzluv, #tinytable_u6uhv4gz6i8ouotmgsyt th.tinytable_css_nun0z9ox5ihokutkzluv { text-align: left }
    #tinytable_u6uhv4gz6i8ouotmgsyt td.tinytable_css_rr7nko4e8bmq4wgrj6w0, #tinytable_u6uhv4gz6i8ouotmgsyt th.tinytable_css_rr7nko4e8bmq4wgrj6w0 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_u6uhv4gz6i8ouotmgsyt" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1"> </th>
                <th scope="col" data-row="0" data-col="2">H1a: Sleep Quality</th>
                <th scope="col" data-row="0" data-col="3">H1b: Sleep Duration</th>
                <th scope="col" data-row="0" data-col="4">H1c: Daytime Sleepiness</th>
                <th scope="col" data-row="0" data-col="5">H1d: Wellbeing</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='5'>+ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001</td></tr>
<tr><td colspan='5'>LN = late-night. Confidence intervals shown in brackets.</td></tr>
<tr><td colspan='5'>ICC = Intraclass Correlation Coefficient (adjusted).</td></tr></tfoot>
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">Daily LN gaming (per 10 min/day, monthly)</td>
                  <td data-row="1" data-col="2">0.06 [0.03, 0.08]***</td>
                  <td data-row="1" data-col="3">-0.00 [-0.02, 0.01]</td>
                  <td data-row="1" data-col="4">0.02 [-0.02, 0.06]</td>
                  <td data-row="1" data-col="5"></td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">Daily LN gaming (per 10 min/day, biweekly)</td>
                  <td data-row="2" data-col="2"></td>
                  <td data-row="2" data-col="3"></td>
                  <td data-row="2" data-col="4"></td>
                  <td data-row="2" data-col="5">-0.02 [-0.05, 0.01]</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">Age (scaled)</td>
                  <td data-row="3" data-col="2">0.24 [-0.12, 0.60]</td>
                  <td data-row="3" data-col="3">-0.45 [-0.62, -0.28]***</td>
                  <td data-row="3" data-col="4">-0.80 [-1.36, -0.24]**</td>
                  <td data-row="3" data-col="5">0.07 [-0.58, 0.71]</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">BMI (scaled)</td>
                  <td data-row="4" data-col="2">0.23 [0.08, 0.39]**</td>
                  <td data-row="4" data-col="3">-0.09 [-0.16, -0.01]*</td>
                  <td data-row="4" data-col="4">0.24 [-0.00, 0.48]+</td>
                  <td data-row="4" data-col="5">-0.20 [-0.48, 0.07]</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">SES (scaled)</td>
                  <td data-row="5" data-col="2">-0.26 [-0.42, -0.11]***</td>
                  <td data-row="5" data-col="3">-0.10 [-0.17, -0.03]**</td>
                  <td data-row="5" data-col="4">0.13 [-0.11, 0.38]</td>
                  <td data-row="5" data-col="5">0.94 [0.67, 1.21]***</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">Region: US</td>
                  <td data-row="6" data-col="2">-0.23 [-0.50, 0.05]</td>
                  <td data-row="6" data-col="3">0.07 [-0.06, 0.20]</td>
                  <td data-row="6" data-col="4">0.29 [-0.13, 0.72]</td>
                  <td data-row="6" data-col="5">0.17 [-0.31, 0.66]</td>
                </tr>
                <tr>
                  <td data-row="7" data-col="1">Day: Weekend</td>
                  <td data-row="7" data-col="2">0.04 [-0.16, 0.24]</td>
                  <td data-row="7" data-col="3">0.02 [-0.08, 0.11]</td>
                  <td data-row="7" data-col="4">0.00 [-0.30, 0.31]</td>
                  <td data-row="7" data-col="5">-0.01 [-0.27, 0.24]</td>
                </tr>
                <tr>
                  <td data-row="8" data-col="1">SD (Intercept | Participant)</td>
                  <td data-row="8" data-col="2">2.01</td>
                  <td data-row="8" data-col="3">0.96</td>
                  <td data-row="8" data-col="4">3.12</td>
                  <td data-row="8" data-col="5">4.23</td>
                </tr>
                <tr>
                  <td data-row="9" data-col="1">SD (Residual)</td>
                  <td data-row="9" data-col="2"></td>
                  <td data-row="9" data-col="3">0.66</td>
                  <td data-row="9" data-col="4">2.16</td>
                  <td data-row="9" data-col="5">2.81</td>
                </tr>
                <tr>
                  <td data-row="10" data-col="1">SD (Intercept | Gender)</td>
                  <td data-row="10" data-col="2"></td>
                  <td data-row="10" data-col="3"></td>
                  <td data-row="10" data-col="4">0.46</td>
                  <td data-row="10" data-col="5">1.20</td>
                </tr>
                <tr>
                  <td data-row="11" data-col="1">N Obs</td>
                  <td data-row="11" data-col="2">2482</td>
                  <td data-row="11" data-col="3">2482</td>
                  <td data-row="11" data-col="4">2482</td>
                  <td data-row="11" data-col="5">5704</td>
                </tr>
                <tr>
                  <td data-row="12" data-col="1">N Participants</td>
                  <td data-row="12" data-col="2">1102</td>
                  <td data-row="12" data-col="3">1102</td>
                  <td data-row="12" data-col="4">1102</td>
                  <td data-row="12" data-col="5">1469</td>
                </tr>
                <tr>
                  <td data-row="13" data-col="1">ICC</td>
                  <td data-row="13" data-col="2">0.80</td>
                  <td data-row="13" data-col="3">0.68</td>
                  <td data-row="13" data-col="4">0.68</td>
                  <td data-row="13" data-col="5">0.71</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::


```{=typst}
#pagebreak()
```


## Panel H2 Sensitivity Analysis: Complete-Case


::: {#tbl-appendix-h2-completecase .cell tbl-cap='Panel H2 Chronotype Moderation — Complete-Case (Non-Imputed Data)'}
::: {.cell-output-display}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_kt3hxujr8cr3f6ko8f4v = TinyTable.createTableFunctions("tinytable_kt3hxujr8cr3f6ko8f4v");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '16', j: 2 }, { i: '16', j: 3 }, { i: '16', j: 4 }, { i: '16', j: 5 } ], css_id: 'tinytable_css_djt247e469qsg4e80jox',}, 
          { positions: [ { i: '1', j: 2 }, { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '5', j: 2 }, { i: '6', j: 2 }, { i: '7', j: 2 }, { i: '8', j: 2 }, { i: '9', j: 2 }, { i: '10', j: 2 }, { i: '11', j: 2 }, { i: '12', j: 2 }, { i: '13', j: 2 }, { i: '14', j: 2 }, { i: '15', j: 2 }, { i: '1', j: 3 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '5', j: 3 }, { i: '6', j: 3 }, { i: '7', j: 3 }, { i: '8', j: 3 }, { i: '9', j: 3 }, { i: '10', j: 3 }, { i: '11', j: 3 }, { i: '12', j: 3 }, { i: '13', j: 3 }, { i: '14', j: 3 }, { i: '15', j: 3 }, { i: '1', j: 4 }, { i: '2', j: 4 }, { i: '3', j: 4 }, { i: '4', j: 4 }, { i: '5', j: 4 }, { i: '6', j: 4 }, { i: '7', j: 4 }, { i: '8', j: 4 }, { i: '9', j: 4 }, { i: '10', j: 4 }, { i: '11', j: 4 }, { i: '12', j: 4 }, { i: '13', j: 4 }, { i: '14', j: 4 }, { i: '15', j: 4 }, { i: '1', j: 5 }, { i: '2', j: 5 }, { i: '3', j: 5 }, { i: '4', j: 5 }, { i: '5', j: 5 }, { i: '6', j: 5 }, { i: '7', j: 5 }, { i: '8', j: 5 }, { i: '9', j: 5 }, { i: '10', j: 5 }, { i: '11', j: 5 }, { i: '12', j: 5 }, { i: '13', j: 5 }, { i: '14', j: 5 }, { i: '15', j: 5 } ], css_id: 'tinytable_css_h1m4rj0cw4u9njqzahoh',}, 
          { positions: [ { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 }, { i: '0', j: 5 } ], css_id: 'tinytable_css_azosvqvti4bxhvvshjlj',}, 
          { positions: [ { i: '16', j: 1 } ], css_id: 'tinytable_css_11swwkvr53ny0khd3yyf',}, 
          { positions: [ { i: '1', j: 1 }, { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '4', j: 1 }, { i: '5', j: 1 }, { i: '6', j: 1 }, { i: '7', j: 1 }, { i: '8', j: 1 }, { i: '9', j: 1 }, { i: '10', j: 1 }, { i: '11', j: 1 }, { i: '12', j: 1 }, { i: '13', j: 1 }, { i: '14', j: 1 }, { i: '15', j: 1 } ], css_id: 'tinytable_css_ph95ocu6mxer2gy6twri',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_11w54410awbylo4fqpha',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_kt3hxujr8cr3f6ko8f4v.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_kt3hxujr8cr3f6ko8f4v td.tinytable_css_djt247e469qsg4e80jox, #tinytable_kt3hxujr8cr3f6ko8f4v th.tinytable_css_djt247e469qsg4e80jox {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_kt3hxujr8cr3f6ko8f4v td.tinytable_css_h1m4rj0cw4u9njqzahoh, #tinytable_kt3hxujr8cr3f6ko8f4v th.tinytable_css_h1m4rj0cw4u9njqzahoh { text-align: center }
    #tinytable_kt3hxujr8cr3f6ko8f4v td.tinytable_css_azosvqvti4bxhvvshjlj, #tinytable_kt3hxujr8cr3f6ko8f4v th.tinytable_css_azosvqvti4bxhvvshjlj {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_kt3hxujr8cr3f6ko8f4v td.tinytable_css_11swwkvr53ny0khd3yyf, #tinytable_kt3hxujr8cr3f6ko8f4v th.tinytable_css_11swwkvr53ny0khd3yyf {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    #tinytable_kt3hxujr8cr3f6ko8f4v td.tinytable_css_ph95ocu6mxer2gy6twri, #tinytable_kt3hxujr8cr3f6ko8f4v th.tinytable_css_ph95ocu6mxer2gy6twri { text-align: left }
    #tinytable_kt3hxujr8cr3f6ko8f4v td.tinytable_css_11w54410awbylo4fqpha, #tinytable_kt3hxujr8cr3f6ko8f4v th.tinytable_css_11w54410awbylo4fqpha {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_kt3hxujr8cr3f6ko8f4v" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1"> </th>
                <th scope="col" data-row="0" data-col="2">H2a: Sleep Quality</th>
                <th scope="col" data-row="0" data-col="3">H2b: Sleep Duration</th>
                <th scope="col" data-row="0" data-col="4">H2c: Daytime Sleepiness</th>
                <th scope="col" data-row="0" data-col="5">H2d: Wellbeing</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='5'>+ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001</td></tr>
<tr><td colspan='5'>LN = late-night. Confidence intervals shown in brackets.</td></tr>
<tr><td colspan='5'>ICC = Intraclass Correlation Coefficient (adjusted).</td></tr></tfoot>
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">Daily LN gaming (per 10 min/day, monthly)</td>
                  <td data-row="1" data-col="2">0.07 [0.02, 0.12]**</td>
                  <td data-row="1" data-col="3">-0.02 [-0.04, 0.01]</td>
                  <td data-row="1" data-col="4">0.04 [-0.02, 0.10]</td>
                  <td data-row="1" data-col="5"></td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">Daily LN gaming (per 10 min/day, biweekly)</td>
                  <td data-row="2" data-col="2"></td>
                  <td data-row="2" data-col="3"></td>
                  <td data-row="2" data-col="4"></td>
                  <td data-row="2" data-col="5">0.03 [-0.01, 0.07]</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">Chronotype (h, centered)</td>
                  <td data-row="3" data-col="2">0.02 [-0.04, 0.08]</td>
                  <td data-row="3" data-col="3">-0.02 [-0.05, 0.01]</td>
                  <td data-row="3" data-col="4">0.02 [-0.07, 0.11]</td>
                  <td data-row="3" data-col="5">-0.05 [-0.15, 0.04]</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">LN gaming × Chronotype (h, monthly)</td>
                  <td data-row="4" data-col="2">-0.01 [-0.02, 0.00]</td>
                  <td data-row="4" data-col="3">0.00 [-0.00, 0.01]+</td>
                  <td data-row="4" data-col="4">-0.01 [-0.03, 0.00]</td>
                  <td data-row="4" data-col="5"></td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">LN gaming × Chronotype (h, biweekly)</td>
                  <td data-row="5" data-col="2"></td>
                  <td data-row="5" data-col="3"></td>
                  <td data-row="5" data-col="4"></td>
                  <td data-row="5" data-col="5">-0.01 [-0.02, -0.00]*</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">Age (scaled)</td>
                  <td data-row="6" data-col="2">0.01 [-0.44, 0.46]</td>
                  <td data-row="6" data-col="3">-0.43 [-0.67, -0.19]***</td>
                  <td data-row="6" data-col="4">-1.18 [-1.90, -0.46]**</td>
                  <td data-row="6" data-col="5">0.16 [-0.69, 1.01]</td>
                </tr>
                <tr>
                  <td data-row="7" data-col="1">BMI (scaled)</td>
                  <td data-row="7" data-col="2">0.04 [-0.15, 0.24]</td>
                  <td data-row="7" data-col="3">-0.06 [-0.15, 0.04]</td>
                  <td data-row="7" data-col="4">0.19 [-0.09, 0.48]</td>
                  <td data-row="7" data-col="5">-0.04 [-0.37, 0.30]</td>
                </tr>
                <tr>
                  <td data-row="8" data-col="1">SES (scaled)</td>
                  <td data-row="8" data-col="2">-0.11 [-0.32, 0.09]</td>
                  <td data-row="8" data-col="3">-0.11 [-0.21, -0.01]*</td>
                  <td data-row="8" data-col="4">0.22 [-0.08, 0.52]</td>
                  <td data-row="8" data-col="5">1.05 [0.71, 1.40]***</td>
                </tr>
                <tr>
                  <td data-row="9" data-col="1">Region: US</td>
                  <td data-row="9" data-col="2">-0.16 [-0.52, 0.19]</td>
                  <td data-row="9" data-col="3">-0.02 [-0.20, 0.16]</td>
                  <td data-row="9" data-col="4">0.51 [-0.02, 1.05]+</td>
                  <td data-row="9" data-col="5">0.26 [-0.36, 0.88]</td>
                </tr>
                <tr>
                  <td data-row="10" data-col="1">Day: Weekend</td>
                  <td data-row="10" data-col="2">-0.01 [-0.27, 0.25]</td>
                  <td data-row="10" data-col="3">0.05 [-0.08, 0.17]</td>
                  <td data-row="10" data-col="4">-0.02 [-0.39, 0.35]</td>
                  <td data-row="10" data-col="5">-0.08 [-0.40, 0.24]</td>
                </tr>
                <tr>
                  <td data-row="11" data-col="1">SD (Intercept | Participant)</td>
                  <td data-row="11" data-col="2">4.51</td>
                  <td data-row="11" data-col="3">1.02</td>
                  <td data-row="11" data-col="4">3.03</td>
                  <td data-row="11" data-col="5">4.12</td>
                </tr>
                <tr>
                  <td data-row="12" data-col="1">SD (Intercept | Gender)</td>
                  <td data-row="12" data-col="2"></td>
                  <td data-row="12" data-col="3">0.04</td>
                  <td data-row="12" data-col="4">0.33</td>
                  <td data-row="12" data-col="5">0.85</td>
                </tr>
                <tr>
                  <td data-row="13" data-col="1">SD (Residual)</td>
                  <td data-row="13" data-col="2"></td>
                  <td data-row="13" data-col="3">0.68</td>
                  <td data-row="13" data-col="4">2.03</td>
                  <td data-row="13" data-col="5">2.79</td>
                </tr>
                <tr>
                  <td data-row="14" data-col="1">N Obs</td>
                  <td data-row="14" data-col="2">1520</td>
                  <td data-row="14" data-col="3">1520</td>
                  <td data-row="14" data-col="4">1520</td>
                  <td data-row="14" data-col="5">3462</td>
                </tr>
                <tr>
                  <td data-row="15" data-col="1">N Participants</td>
                  <td data-row="15" data-col="2">673</td>
                  <td data-row="15" data-col="3">673</td>
                  <td data-row="15" data-col="4">673</td>
                  <td data-row="15" data-col="5">860</td>
                </tr>
                <tr>
                  <td data-row="16" data-col="1">ICC</td>
                  <td data-row="16" data-col="2">0.95</td>
                  <td data-row="16" data-col="3">0.69</td>
                  <td data-row="16" data-col="4">0.69</td>
                  <td data-row="16" data-col="5">0.69</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::


```{=typst}
#pagebreak()
```

## Panel: Wave-Level Missingness

We summarised the extent of missingness for the key self-report outcomes across each survey wave in the raw data before imputation. The `Observations` column reports the number of participants in a wave who completed at least one of the listed measures; the percentages in each row are calculated relative to that wave-specific participant count.


::: {#tbl-appendix-wave-missingness .cell tbl-cap='Wave-level missingness for key self-report measures (pre-imputation data)'}
::: {.cell-output-display}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_tpswmxsqc58x60yv1kdo = TinyTable.createTableFunctions("tinytable_tpswmxsqc58x60yv1kdo");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '6', j: 1 }, { i: '6', j: 2 }, { i: '6', j: 3 }, { i: '6', j: 4 }, { i: '6', j: 5 }, { i: '6', j: 6 } ], css_id: 'tinytable_css_fzmds9sgbt79jm3bbvhh',}, 
          { positions: [ { i: '0', j: 1 }, { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 }, { i: '0', j: 5 }, { i: '0', j: 6 } ], css_id: 'tinytable_css_0h9muzo6r90hk93r0b93',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_tpswmxsqc58x60yv1kdo.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_tpswmxsqc58x60yv1kdo td.tinytable_css_fzmds9sgbt79jm3bbvhh, #tinytable_tpswmxsqc58x60yv1kdo th.tinytable_css_fzmds9sgbt79jm3bbvhh {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%;  }
    #tinytable_tpswmxsqc58x60yv1kdo td.tinytable_css_0h9muzo6r90hk93r0b93, #tinytable_tpswmxsqc58x60yv1kdo th.tinytable_css_0h9muzo6r90hk93r0b93 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%;  }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_tpswmxsqc58x60yv1kdo" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        <caption>Wave-level missingness for key self-report measures (pre-imputation data). Entries show the number and percentage of participants missing each measure within a wave.</caption>
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1">Wave</th>
                <th scope="col" data-row="0" data-col="2">Observations</th>
                <th scope="col" data-row="0" data-col="3">Sleep quality (PSQI item 6)</th>
                <th scope="col" data-row="0" data-col="4">Sleep duration (hours)</th>
                <th scope="col" data-row="0" data-col="5">Daytime sleepiness (ESS)</th>
                <th scope="col" data-row="0" data-col="6">Wellbeing (SWEMWBS)</th>
              </tr>
        </thead>
        
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">1</td>
                  <td data-row="1" data-col="2">1578</td>
                  <td data-row="1" data-col="3"></td>
                  <td data-row="1" data-col="4"></td>
                  <td data-row="1" data-col="5"></td>
                  <td data-row="1" data-col="6">1 (0.1%)</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">2</td>
                  <td data-row="2" data-col="2">1578</td>
                  <td data-row="2" data-col="3">458 (29.0%)</td>
                  <td data-row="2" data-col="4">471 (29.8%)</td>
                  <td data-row="2" data-col="5">472 (29.9%)</td>
                  <td data-row="2" data-col="6">455 (28.8%)</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">3</td>
                  <td data-row="3" data-col="2">1578</td>
                  <td data-row="3" data-col="3"></td>
                  <td data-row="3" data-col="4"></td>
                  <td data-row="3" data-col="5"></td>
                  <td data-row="3" data-col="6">530 (33.6%)</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">4</td>
                  <td data-row="4" data-col="2">1578</td>
                  <td data-row="4" data-col="3">663 (42.0%)</td>
                  <td data-row="4" data-col="4">674 (42.7%)</td>
                  <td data-row="4" data-col="5">669 (42.4%)</td>
                  <td data-row="4" data-col="6">661 (41.9%)</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">5</td>
                  <td data-row="5" data-col="2">1578</td>
                  <td data-row="5" data-col="3"></td>
                  <td data-row="5" data-col="4"></td>
                  <td data-row="5" data-col="5"></td>
                  <td data-row="5" data-col="6">740 (46.9%)</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">6</td>
                  <td data-row="6" data-col="2">1578</td>
                  <td data-row="6" data-col="3">876 (55.5%)</td>
                  <td data-row="6" data-col="4">881 (55.8%)</td>
                  <td data-row="6" data-col="5">888 (56.3%)</td>
                  <td data-row="6" data-col="6">872 (55.3%)</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::


```{=typst}
#pagebreak()
```

## Panel: PSQI Global Score Sensitivity Analysis

This section presents a sensitivity analysis using the PSQI global score as an alternative sleep quality outcome. The PSQI global score is the sum of all 7 PSQI component scores (range 0-21, higher = worse sleep quality), providing a continuous measure compared to the ordinal PSQI item 6 outcome used in the pre-registered H1a hypothesis.



::: {#tbl-appendix-psqi-global .cell tbl-cap='Sensitivity Analysis: PSQI Global Score Models (Imputed vs. Original)'}
::: {.cell-output-display}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_t5hu6cuwa46oxty1m62x = TinyTable.createTableFunctions("tinytable_t5hu6cuwa46oxty1m62x");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '-1', j: 3 }, { i: '-1', j: 4 }, { i: '-1', j: 5 } ], css_id: 'tinytable_css_9pncudpsu4dsc2ice7sr',}, 
          { positions: [ { i: '14', j: 2 }, { i: '14', j: 3 }, { i: '14', j: 4 }, { i: '14', j: 5 } ], css_id: 'tinytable_css_ogotf4j4o1pzjejxi628',}, 
          { positions: [ { i: '1', j: 2 }, { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '5', j: 2 }, { i: '6', j: 2 }, { i: '7', j: 2 }, { i: '8', j: 2 }, { i: '9', j: 2 }, { i: '10', j: 2 }, { i: '11', j: 2 }, { i: '12', j: 2 }, { i: '13', j: 2 }, { i: '1', j: 3 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '5', j: 3 }, { i: '6', j: 3 }, { i: '7', j: 3 }, { i: '8', j: 3 }, { i: '9', j: 3 }, { i: '10', j: 3 }, { i: '11', j: 3 }, { i: '12', j: 3 }, { i: '13', j: 3 }, { i: '1', j: 4 }, { i: '2', j: 4 }, { i: '3', j: 4 }, { i: '4', j: 4 }, { i: '5', j: 4 }, { i: '6', j: 4 }, { i: '7', j: 4 }, { i: '8', j: 4 }, { i: '9', j: 4 }, { i: '10', j: 4 }, { i: '11', j: 4 }, { i: '12', j: 4 }, { i: '13', j: 4 }, { i: '1', j: 5 }, { i: '2', j: 5 }, { i: '3', j: 5 }, { i: '4', j: 5 }, { i: '5', j: 5 }, { i: '6', j: 5 }, { i: '7', j: 5 }, { i: '8', j: 5 }, { i: '9', j: 5 }, { i: '10', j: 5 }, { i: '11', j: 5 }, { i: '12', j: 5 }, { i: '13', j: 5 } ], css_id: 'tinytable_css_xmyy4w9maoi1r4l64xeu',}, 
          { positions: [ { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 }, { i: '0', j: 5 } ], css_id: 'tinytable_css_o4bmhrhb78j9wds0m4vc',}, 
          { positions: [ { i: '-1', j: 2 } ], css_id: 'tinytable_css_b9fmi4hwbve59hu6oum3',}, 
          { positions: [ { i: '14', j: 1 } ], css_id: 'tinytable_css_j7fe0x1bdr1gwofol34g',}, 
          { positions: [ { i: '1', j: 1 }, { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '4', j: 1 }, { i: '5', j: 1 }, { i: '6', j: 1 }, { i: '7', j: 1 }, { i: '8', j: 1 }, { i: '9', j: 1 }, { i: '10', j: 1 }, { i: '11', j: 1 }, { i: '12', j: 1 }, { i: '13', j: 1 } ], css_id: 'tinytable_css_y4sdhq18dh6scdk8h0h1',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_4o8ns2aueop9k01aoj9w',}, 
          { positions: [ { i: '-1', j: 1 } ], css_id: 'tinytable_css_iluqvocz0p8d6y2fqy6o',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_t5hu6cuwa46oxty1m62x.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_t5hu6cuwa46oxty1m62x td.tinytable_css_9pncudpsu4dsc2ice7sr, #tinytable_t5hu6cuwa46oxty1m62x th.tinytable_css_9pncudpsu4dsc2ice7sr {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_t5hu6cuwa46oxty1m62x td.tinytable_css_ogotf4j4o1pzjejxi628, #tinytable_t5hu6cuwa46oxty1m62x th.tinytable_css_ogotf4j4o1pzjejxi628 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_t5hu6cuwa46oxty1m62x td.tinytable_css_xmyy4w9maoi1r4l64xeu, #tinytable_t5hu6cuwa46oxty1m62x th.tinytable_css_xmyy4w9maoi1r4l64xeu { text-align: center }
    #tinytable_t5hu6cuwa46oxty1m62x td.tinytable_css_o4bmhrhb78j9wds0m4vc, #tinytable_t5hu6cuwa46oxty1m62x th.tinytable_css_o4bmhrhb78j9wds0m4vc {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_t5hu6cuwa46oxty1m62x td.tinytable_css_b9fmi4hwbve59hu6oum3, #tinytable_t5hu6cuwa46oxty1m62x th.tinytable_css_b9fmi4hwbve59hu6oum3 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 3%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_t5hu6cuwa46oxty1m62x td.tinytable_css_j7fe0x1bdr1gwofol34g, #tinytable_t5hu6cuwa46oxty1m62x th.tinytable_css_j7fe0x1bdr1gwofol34g {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    #tinytable_t5hu6cuwa46oxty1m62x td.tinytable_css_y4sdhq18dh6scdk8h0h1, #tinytable_t5hu6cuwa46oxty1m62x th.tinytable_css_y4sdhq18dh6scdk8h0h1 { text-align: left }
    #tinytable_t5hu6cuwa46oxty1m62x td.tinytable_css_4o8ns2aueop9k01aoj9w, #tinytable_t5hu6cuwa46oxty1m62x th.tinytable_css_4o8ns2aueop9k01aoj9w {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    #tinytable_t5hu6cuwa46oxty1m62x td.tinytable_css_iluqvocz0p8d6y2fqy6o, #tinytable_t5hu6cuwa46oxty1m62x th.tinytable_css_iluqvocz0p8d6y2fqy6o {  position: relative; --border-bottom: 0; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_t5hu6cuwa46oxty1m62x" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
<tr>
<th scope="col" align="center" colspan=1 data-row="-1" data-col="1"> </th>
<th scope="col" align="center" colspan=4 data-row="-1" data-col="2">PSQI Global</th>
</tr>
              <tr>
                <th scope="col" data-row="0" data-col="1"> </th>
                <th scope="col" data-row="0" data-col="2">Playtime (Imputed)</th>
                <th scope="col" data-row="0" data-col="3">Chronotype × Playtime (Imputed)</th>
                <th scope="col" data-row="0" data-col="4">Playtime (Complete-Case)</th>
                <th scope="col" data-row="0" data-col="5">Chronotype × Playtime (Complete-Case)</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='5'>+ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001</td></tr>
<tr><td colspan='5'>PSQI global score range: 0-21 (higher = worse sleep quality)</td></tr>
<tr><td colspan='5'>Confidence intervals shown in brackets.</td></tr>
<tr><td colspan='5'>ICC = Intraclass Correlation Coefficient (adjusted).</td></tr></tfoot>
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">Daily LN gaming (per 10 min/day, monthly)</td>
                  <td data-row="1" data-col="2">0.09 [0.05, 0.12]***</td>
                  <td data-row="1" data-col="3">0.09 [0.04, 0.15]***</td>
                  <td data-row="1" data-col="4">0.04 [0.00, 0.07]*</td>
                  <td data-row="1" data-col="5">0.04 [-0.00, 0.09]+</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">Age (scaled)</td>
                  <td data-row="2" data-col="2">1.44 [1.04, 1.83]***</td>
                  <td data-row="2" data-col="3">1.16 [0.63, 1.69]***</td>
                  <td data-row="2" data-col="4">0.51 [0.07, 0.95]*</td>
                  <td data-row="2" data-col="5">0.15 [-0.44, 0.74]</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">BMI (scaled)</td>
                  <td data-row="3" data-col="2">0.14 [-0.03, 0.31]</td>
                  <td data-row="3" data-col="3">0.01 [-0.20, 0.22]</td>
                  <td data-row="3" data-col="4">0.31 [0.13, 0.50]**</td>
                  <td data-row="3" data-col="5">0.14 [-0.09, 0.37]</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">SES (scaled)</td>
                  <td data-row="4" data-col="2">-0.18 [-0.34, -0.01]*</td>
                  <td data-row="4" data-col="3">-0.24 [-0.46, -0.02]*</td>
                  <td data-row="4" data-col="4">-0.41 [-0.60, -0.22]***</td>
                  <td data-row="4" data-col="5">-0.39 [-0.64, -0.15]**</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">Chronotype (h, centered)</td>
                  <td data-row="5" data-col="2"></td>
                  <td data-row="5" data-col="3">0.01 [-0.05, 0.07]</td>
                  <td data-row="5" data-col="4"></td>
                  <td data-row="5" data-col="5">0.10 [0.03, 0.18]**</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">LN gaming × Chronotype (h, monthly)</td>
                  <td data-row="6" data-col="2"></td>
                  <td data-row="6" data-col="3">0.00 [-0.01, 0.01]</td>
                  <td data-row="6" data-col="4"></td>
                  <td data-row="6" data-col="5">-0.01 [-0.02, 0.00]</td>
                </tr>
                <tr>
                  <td data-row="7" data-col="1">Region: US</td>
                  <td data-row="7" data-col="2">-0.20 [-0.49, 0.10]</td>
                  <td data-row="7" data-col="3">-0.17 [-0.55, 0.22]</td>
                  <td data-row="7" data-col="4">-0.38 [-0.71, -0.04]*</td>
                  <td data-row="7" data-col="5">-0.42 [-0.86, 0.02]+</td>
                </tr>
                <tr>
                  <td data-row="8" data-col="1">Day: Weekend</td>
                  <td data-row="8" data-col="2">0.23 [-0.03, 0.48]+</td>
                  <td data-row="8" data-col="3">0.24 [-0.09, 0.56]</td>
                  <td data-row="8" data-col="4">-0.23 [-0.46, 0.00]+</td>
                  <td data-row="8" data-col="5">-0.19 [-0.48, 0.10]</td>
                </tr>
                <tr>
                  <td data-row="9" data-col="1">SD (Residual)</td>
                  <td data-row="9" data-col="2"></td>
                  <td data-row="9" data-col="3"></td>
                  <td data-row="9" data-col="4">1.60</td>
                  <td data-row="9" data-col="5">1.55</td>
                </tr>
                <tr>
                  <td data-row="10" data-col="1">SD (Intercept | Participant)</td>
                  <td data-row="10" data-col="2"></td>
                  <td data-row="10" data-col="3"></td>
                  <td data-row="10" data-col="4">2.47</td>
                  <td data-row="10" data-col="5">2.52</td>
                </tr>
                <tr>
                  <td data-row="11" data-col="1">SD (Intercept | Gender)</td>
                  <td data-row="11" data-col="2"></td>
                  <td data-row="11" data-col="3"></td>
                  <td data-row="11" data-col="4">0.71</td>
                  <td data-row="11" data-col="5">0.55</td>
                </tr>
                <tr>
                  <td data-row="12" data-col="1">N Obs</td>
                  <td data-row="12" data-col="2">4410</td>
                  <td data-row="12" data-col="3">2580</td>
                  <td data-row="12" data-col="4">2482</td>
                  <td data-row="12" data-col="5">1520</td>
                </tr>
                <tr>
                  <td data-row="13" data-col="1">N Participants</td>
                  <td data-row="13" data-col="2">1470</td>
                  <td data-row="13" data-col="3">860</td>
                  <td data-row="13" data-col="4">1102</td>
                  <td data-row="13" data-col="5">673</td>
                </tr>
                <tr>
                  <td data-row="14" data-col="1">ICC</td>
                  <td data-row="14" data-col="2">0.51</td>
                  <td data-row="14" data-col="3">0.51</td>
                  <td data-row="14" data-col="4">0.72</td>
                  <td data-row="14" data-col="5">0.73</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::


```{=typst}
#pagebreak()
```

## Diary: Daily Diary Subsample


::: {#tbl-diary-demographics .cell tbl-cap='Daily Diary Subsample Characteristics'}
::: {.cell-output-display}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_dexzdwom2s7n5qgv9nci = TinyTable.createTableFunctions("tinytable_dexzdwom2s7n5qgv9nci");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '23', j: 2 }, { i: '23', j: 3 } ], css_id: 'tinytable_css_ueso7x16p3apoxwy5l6o',}, 
          { positions: [ { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '5', j: 2 }, { i: '6', j: 2 }, { i: '7', j: 2 }, { i: '8', j: 2 }, { i: '9', j: 2 }, { i: '10', j: 2 }, { i: '11', j: 2 }, { i: '13', j: 2 }, { i: '14', j: 2 }, { i: '16', j: 2 }, { i: '17', j: 2 }, { i: '19', j: 2 }, { i: '20', j: 2 }, { i: '21', j: 2 }, { i: '22', j: 2 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '5', j: 3 }, { i: '6', j: 3 }, { i: '7', j: 3 }, { i: '8', j: 3 }, { i: '9', j: 3 }, { i: '10', j: 3 }, { i: '11', j: 3 }, { i: '13', j: 3 }, { i: '14', j: 3 }, { i: '16', j: 3 }, { i: '17', j: 3 }, { i: '19', j: 3 }, { i: '20', j: 3 }, { i: '21', j: 3 }, { i: '22', j: 3 } ], css_id: 'tinytable_css_bog6339l9bf8vzz0dz5z',}, 
          { positions: [ { i: '1', j: 2 }, { i: '12', j: 2 }, { i: '15', j: 2 }, { i: '18', j: 2 }, { i: '1', j: 3 }, { i: '12', j: 3 }, { i: '15', j: 3 }, { i: '18', j: 3 } ], css_id: 'tinytable_css_islnkej6ssqh0sdygcgv',}, 
          { positions: [ { i: '0', j: 2 }, { i: '0', j: 3 } ], css_id: 'tinytable_css_9g9rknxny6khhafb3kvt',}, 
          { positions: [ { i: '23', j: 1 } ], css_id: 'tinytable_css_tpi0npq0uppetysxdk6z',}, 
          { positions: [ { i: '4', j: 1 }, { i: '7', j: 1 }, { i: '8', j: 1 }, { i: '9', j: 1 }, { i: '14', j: 1 }, { i: '19', j: 1 }, { i: '20', j: 1 }, { i: '21', j: 1 }, { i: '22', j: 1 } ], css_id: 'tinytable_css_cuzxo7h3xrlv5xunvm03',}, 
          { positions: [ { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '5', j: 1 }, { i: '6', j: 1 }, { i: '10', j: 1 }, { i: '11', j: 1 }, { i: '13', j: 1 }, { i: '16', j: 1 }, { i: '17', j: 1 } ], css_id: 'tinytable_css_cpyeqcgcbnc0806mfuxu',}, 
          { positions: [ { i: '1', j: 1 }, { i: '12', j: 1 }, { i: '15', j: 1 }, { i: '18', j: 1 } ], css_id: 'tinytable_css_8ald1tmo6hxd532p2s16',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_p4e78qps9cqfoz1hzs3g',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_dexzdwom2s7n5qgv9nci.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_dexzdwom2s7n5qgv9nci td.tinytable_css_ueso7x16p3apoxwy5l6o, #tinytable_dexzdwom2s7n5qgv9nci th.tinytable_css_ueso7x16p3apoxwy5l6o {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: right }
    #tinytable_dexzdwom2s7n5qgv9nci td.tinytable_css_bog6339l9bf8vzz0dz5z, #tinytable_dexzdwom2s7n5qgv9nci th.tinytable_css_bog6339l9bf8vzz0dz5z { text-align: right }
    #tinytable_dexzdwom2s7n5qgv9nci td.tinytable_css_islnkej6ssqh0sdygcgv, #tinytable_dexzdwom2s7n5qgv9nci th.tinytable_css_islnkej6ssqh0sdygcgv { font-weight: bold; text-align: right }
    #tinytable_dexzdwom2s7n5qgv9nci td.tinytable_css_9g9rknxny6khhafb3kvt, #tinytable_dexzdwom2s7n5qgv9nci th.tinytable_css_9g9rknxny6khhafb3kvt {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: right }
    #tinytable_dexzdwom2s7n5qgv9nci td.tinytable_css_tpi0npq0uppetysxdk6z, #tinytable_dexzdwom2s7n5qgv9nci th.tinytable_css_tpi0npq0uppetysxdk6z {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left; padding-left: 1em }
    #tinytable_dexzdwom2s7n5qgv9nci td.tinytable_css_cuzxo7h3xrlv5xunvm03, #tinytable_dexzdwom2s7n5qgv9nci th.tinytable_css_cuzxo7h3xrlv5xunvm03 { text-align: left; padding-left: 1em }
    #tinytable_dexzdwom2s7n5qgv9nci td.tinytable_css_cpyeqcgcbnc0806mfuxu, #tinytable_dexzdwom2s7n5qgv9nci th.tinytable_css_cpyeqcgcbnc0806mfuxu { text-align: left }
    #tinytable_dexzdwom2s7n5qgv9nci td.tinytable_css_8ald1tmo6hxd532p2s16, #tinytable_dexzdwom2s7n5qgv9nci th.tinytable_css_8ald1tmo6hxd532p2s16 { font-weight: bold; text-align: left }
    #tinytable_dexzdwom2s7n5qgv9nci td.tinytable_css_p4e78qps9cqfoz1hzs3g, #tinytable_dexzdwom2s7n5qgv9nci th.tinytable_css_p4e78qps9cqfoz1hzs3g {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_dexzdwom2s7n5qgv9nci" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        <caption>Daily Diary Subsample Characteristics</caption>
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1">Characteristic</th>
                <th scope="col" data-row="0" data-col="2">Total</th>
                <th scope="col" data-row="0" data-col="3">Analytical</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='3'>Values are M (SD) unless noted. LN = late-night. Sleep quality distribution counts are at the diary-entry level.</td></tr></tfoot>
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">A. Sociodemographics</td>
                  <td data-row="1" data-col="2"></td>
                  <td data-row="1" data-col="3"></td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">N participants</td>
                  <td data-row="2" data-col="2">1275</td>
                  <td data-row="2" data-col="3">1271</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">N diary entries</td>
                  <td data-row="3" data-col="2">16131</td>
                  <td data-row="3" data-col="3">15842</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">Diary entries per person (Mdn, IQR)</td>
                  <td data-row="4" data-col="2">8 (20)</td>
                  <td data-row="4" data-col="3">8 (21)</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">Age</td>
                  <td data-row="5" data-col="2">26.6 (4.9)</td>
                  <td data-row="5" data-col="3">26.6 (4.9)</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">Gender</td>
                  <td data-row="6" data-col="2"></td>
                  <td data-row="6" data-col="3"></td>
                </tr>
                <tr>
                  <td data-row="7" data-col="1">Man</td>
                  <td data-row="7" data-col="2">746 (58.5%)</td>
                  <td data-row="7" data-col="3">744 (58.5%)</td>
                </tr>
                <tr>
                  <td data-row="8" data-col="1">Non-binary or other gender identity</td>
                  <td data-row="8" data-col="2">78 (6.1%)</td>
                  <td data-row="8" data-col="3">78 (6.1%)</td>
                </tr>
                <tr>
                  <td data-row="9" data-col="1">Woman</td>
                  <td data-row="9" data-col="2">384 (30.1%)</td>
                  <td data-row="9" data-col="3">382 (30.1%)</td>
                </tr>
                <tr>
                  <td data-row="10" data-col="1">BMI (scaled)</td>
                  <td data-row="10" data-col="2">0.00 (1.00)</td>
                  <td data-row="10" data-col="3">0.00 (1.00)</td>
                </tr>
                <tr>
                  <td data-row="11" data-col="1">SES (scaled)</td>
                  <td data-row="11" data-col="2">0.00 (1.00)</td>
                  <td data-row="11" data-col="3">0.00 (1.00)</td>
                </tr>
                <tr>
                  <td data-row="12" data-col="1">B. Chronotype</td>
                  <td data-row="12" data-col="2"></td>
                  <td data-row="12" data-col="3"></td>
                </tr>
                <tr>
                  <td data-row="13" data-col="1">No alarm on free days</td>
                  <td data-row="13" data-col="2">657 (77.8%)</td>
                  <td data-row="13" data-col="3">657 (77.8%)</td>
                </tr>
                <tr>
                  <td data-row="14" data-col="1">MSF~sc~ (HH:MM)</td>
                  <td data-row="14" data-col="2">06:10 (03:22)</td>
                  <td data-row="14" data-col="3">06:10 (03:22)</td>
                </tr>
                <tr>
                  <td data-row="15" data-col="1">C. Gaming</td>
                  <td data-row="15" data-col="2"></td>
                  <td data-row="15" data-col="3"></td>
                </tr>
                <tr>
                  <td data-row="16" data-col="1">LN gaming (min/day, Mdn, IQR)</td>
                  <td data-row="16" data-col="2">0.0 (15.1)</td>
                  <td data-row="16" data-col="3">0.0 (15.0)</td>
                </tr>
                <tr>
                  <td data-row="17" data-col="1">% entries with any LN gaming</td>
                  <td data-row="17" data-col="2">19.1 (29.3)</td>
                  <td data-row="17" data-col="3">19.2 (29.4)</td>
                </tr>
                <tr>
                  <td data-row="18" data-col="1">D. Sleep Quality Distribution</td>
                  <td data-row="18" data-col="2"></td>
                  <td data-row="18" data-col="3"></td>
                </tr>
                <tr>
                  <td data-row="19" data-col="1">Very poor</td>
                  <td data-row="19" data-col="2">836 (5.3%)</td>
                  <td data-row="19" data-col="3">836 (5.3%)</td>
                </tr>
                <tr>
                  <td data-row="20" data-col="1">Poor</td>
                  <td data-row="20" data-col="2">2436 (15.4%)</td>
                  <td data-row="20" data-col="3">2436 (15.4%)</td>
                </tr>
                <tr>
                  <td data-row="21" data-col="1">Fair</td>
                  <td data-row="21" data-col="2">5575 (35.2%)</td>
                  <td data-row="21" data-col="3">5575 (35.2%)</td>
                </tr>
                <tr>
                  <td data-row="22" data-col="1">Good</td>
                  <td data-row="22" data-col="2">5494 (34.7%)</td>
                  <td data-row="22" data-col="3">5494 (34.7%)</td>
                </tr>
                <tr>
                  <td data-row="23" data-col="1">Very good</td>
                  <td data-row="23" data-col="2">1501 (9.5%)</td>
                  <td data-row="23" data-col="3">1501 (9.5%)</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::


```{=typst}
#pagebreak()
```

## Diary: Models — Complete-Case


::: {#tbl-appendix-diary-completecase .cell tbl-cap='Diary H1 and H2 Probit CLMM — Complete-Case (Non-Imputed Data)'}
::: {.cell-output-display}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_nu0xu4a6g7vawk2hhmve = TinyTable.createTableFunctions("tinytable_nu0xu4a6g7vawk2hhmve");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '12', j: 2 }, { i: '12', j: 3 } ], css_id: 'tinytable_css_0g2ubd4azi8uddscm9o8',}, 
          { positions: [ { i: '1', j: 2 }, { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '5', j: 2 }, { i: '6', j: 2 }, { i: '7', j: 2 }, { i: '8', j: 2 }, { i: '9', j: 2 }, { i: '10', j: 2 }, { i: '11', j: 2 }, { i: '1', j: 3 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '5', j: 3 }, { i: '6', j: 3 }, { i: '7', j: 3 }, { i: '8', j: 3 }, { i: '9', j: 3 }, { i: '10', j: 3 }, { i: '11', j: 3 } ], css_id: 'tinytable_css_xa7yvrut4g3mb0dbewsx',}, 
          { positions: [ { i: '0', j: 2 }, { i: '0', j: 3 } ], css_id: 'tinytable_css_r4e1pxmpphu17op28bg1',}, 
          { positions: [ { i: '12', j: 1 } ], css_id: 'tinytable_css_r5mjqyz0pm3k2xtn230o',}, 
          { positions: [ { i: '1', j: 1 }, { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '4', j: 1 }, { i: '5', j: 1 }, { i: '6', j: 1 }, { i: '7', j: 1 }, { i: '8', j: 1 }, { i: '9', j: 1 }, { i: '10', j: 1 }, { i: '11', j: 1 } ], css_id: 'tinytable_css_y4ip96pk8nwl4pwk4h6j',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_dj3fh7fzel4p5ud34wmj',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_nu0xu4a6g7vawk2hhmve.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_nu0xu4a6g7vawk2hhmve td.tinytable_css_0g2ubd4azi8uddscm9o8, #tinytable_nu0xu4a6g7vawk2hhmve th.tinytable_css_0g2ubd4azi8uddscm9o8 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_nu0xu4a6g7vawk2hhmve td.tinytable_css_xa7yvrut4g3mb0dbewsx, #tinytable_nu0xu4a6g7vawk2hhmve th.tinytable_css_xa7yvrut4g3mb0dbewsx { text-align: center }
    #tinytable_nu0xu4a6g7vawk2hhmve td.tinytable_css_r4e1pxmpphu17op28bg1, #tinytable_nu0xu4a6g7vawk2hhmve th.tinytable_css_r4e1pxmpphu17op28bg1 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: center }
    #tinytable_nu0xu4a6g7vawk2hhmve td.tinytable_css_r5mjqyz0pm3k2xtn230o, #tinytable_nu0xu4a6g7vawk2hhmve th.tinytable_css_r5mjqyz0pm3k2xtn230o {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    #tinytable_nu0xu4a6g7vawk2hhmve td.tinytable_css_y4ip96pk8nwl4pwk4h6j, #tinytable_nu0xu4a6g7vawk2hhmve th.tinytable_css_y4ip96pk8nwl4pwk4h6j { text-align: left }
    #tinytable_nu0xu4a6g7vawk2hhmve td.tinytable_css_dj3fh7fzel4p5ud34wmj, #tinytable_nu0xu4a6g7vawk2hhmve th.tinytable_css_dj3fh7fzel4p5ud34wmj {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_nu0xu4a6g7vawk2hhmve" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1"> </th>
                <th scope="col" data-row="0" data-col="2">H1a: Sleep Quality (direct)</th>
                <th scope="col" data-row="0" data-col="3">H2a: Sleep Quality (chronotype mod.)</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='3'>+ p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001</td></tr>
<tr><td colspan='3'>LN = late-night. Confidence intervals shown in brackets.</td></tr>
<tr><td colspan='3'>Complete-case estimates — no imputation applied to the diary outcome.</td></tr>
<tr><td colspan='3'>Cumulative link mixed models (random intercept for participant) on 5-level ordinal sleep quality (positive coefficients = higher probability of worse sleep). Probit link. Late-night gaming per 10 minutes; chronotype in centered hours; age, BMI, SES scaled within the diary subsample. Region excluded: all diary participants are US-only.</td></tr>
<tr><td colspan='3'>Within = daily deviation from person mean; Between = person mean - grand mean.</td></tr></tfoot>
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">LN gaming within-person (per 10 min)</td>
                  <td data-row="1" data-col="2">-0.00 [-0.01, 0.00]</td>
                  <td data-row="1" data-col="3">0.00 [-0.01, 0.01]</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">LN gaming between-person (per 10 min)</td>
                  <td data-row="2" data-col="2">0.03 [0.01, 0.04]**</td>
                  <td data-row="2" data-col="3">0.02 [-0.01, 0.05]</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">Chronotype (h, centered)</td>
                  <td data-row="3" data-col="2"></td>
                  <td data-row="3" data-col="3">0.03 [0.01, 0.06]*</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">LN within × Chronotype</td>
                  <td data-row="4" data-col="2"></td>
                  <td data-row="4" data-col="3">-0.00 [-0.00, -0.00]*</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">LN between × Chronotype</td>
                  <td data-row="5" data-col="2"></td>
                  <td data-row="5" data-col="3">-0.00 [-0.01, 0.01]</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">Age (scaled)</td>
                  <td data-row="6" data-col="2">0.03 [-0.04, 0.09]</td>
                  <td data-row="6" data-col="3">-0.05 [-0.15, 0.05]</td>
                </tr>
                <tr>
                  <td data-row="7" data-col="1">BMI (scaled)</td>
                  <td data-row="7" data-col="2">0.07 [0.00, 0.13]*</td>
                  <td data-row="7" data-col="3">0.07 [-0.02, 0.17]</td>
                </tr>
                <tr>
                  <td data-row="8" data-col="1">SES (scaled)</td>
                  <td data-row="8" data-col="2">-0.12 [-0.19, -0.06]***</td>
                  <td data-row="8" data-col="3">-0.14 [-0.25, -0.04]**</td>
                </tr>
                <tr>
                  <td data-row="9" data-col="1">Day: Weekend</td>
                  <td data-row="9" data-col="2">-0.00 [-0.04, 0.04]</td>
                  <td data-row="9" data-col="3">-0.01 [-0.06, 0.05]</td>
                </tr>
                <tr>
                  <td data-row="10" data-col="1">N Obs</td>
                  <td data-row="10" data-col="2">14683</td>
                  <td data-row="10" data-col="3">8392</td>
                </tr>
                <tr>
                  <td data-row="11" data-col="1">N Participants</td>
                  <td data-row="11" data-col="2">1132</td>
                  <td data-row="11" data-col="3">509</td>
                </tr>
                <tr>
                  <td data-row="12" data-col="1">ICC</td>
                  <td data-row="12" data-col="2">0.48</td>
                  <td data-row="12" data-col="3">0.52</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::



```{=typst}
#pagebreak()
```

## Panel H1 Natural Spline Sensitivity

For each panel outcome (H1a–H1d), we replace the linear gaming predictor with a natural cubic spline (df = 2–6, selected by BIC) to check whether the pre-registered linearity assumption holds. BIC is the primary criterion because its stronger complexity penalty matches the question this check is asking — whether non-linearity is warranted, not whether it yields any in-sample gain; AIC is reported alongside but does not drive df selection. BIC differences are interpreted via @jones2001nagin's rule of thumb on 2|ΔBIC|: 0–2 not worth mentioning, 2–6 positive, 6–10 strong, >10 very strong. Both linear and spline models (@tbl-appendix-panel-h1-spline-bic, @fig-appendix-panel-h1-spline-bic) are fit on the complete-case sample.


::: {.cell}

:::



::: {.cell}

:::



::: {#tbl-appendix-panel-h1-spline-bic .cell tbl-cap='AIC and BIC comparison for panel H1 linear vs. natural spline models (complete-case data). Best df selected from 2–6 by BIC (primary criterion); AIC values are reported alongside for transparency. ΔAIC = Spline AIC − Linear AIC; ΔBIC = Spline BIC − Linear BIC; negative values indicate improvement over the linear specification. The Verdict column applies the @jones2001nagin rule of thumb to 2|ΔBIC|: ≈ = not worth mentioning (0–2); + = positive (2–6); ++ = strong (6–10); +++ = very strong (>10), with the favoured model marked L (linear) or S (spline).'}
::: {.cell-output-display}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_mynho3adhfgda9uf5msc = TinyTable.createTableFunctions("tinytable_mynho3adhfgda9uf5msc");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '4', j: 8 }, { i: '4', j: 9 }, { i: '4', j: 10 } ], css_id: 'tinytable_css_iyr490f62ldseaid6j3q',}, 
          { positions: [ { i: '1', j: 8 }, { i: '2', j: 8 }, { i: '3', j: 8 }, { i: '1', j: 9 }, { i: '2', j: 9 }, { i: '3', j: 9 }, { i: '1', j: 10 }, { i: '2', j: 10 }, { i: '3', j: 10 } ], css_id: 'tinytable_css_ckj9emhtucah3tmhv9c3',}, 
          { positions: [ { i: '0', j: 8 }, { i: '0', j: 9 }, { i: '0', j: 10 } ], css_id: 'tinytable_css_f3r2yn07x9bnqxr8qrk4',}, 
          { positions: [ { i: '4', j: 1 }, { i: '4', j: 2 }, { i: '4', j: 3 }, { i: '4', j: 4 }, { i: '4', j: 5 }, { i: '4', j: 6 }, { i: '4', j: 7 } ], css_id: 'tinytable_css_oy81sma5yu9i9dd63s87',}, 
          { positions: [ { i: '0', j: 1 }, { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 }, { i: '0', j: 5 }, { i: '0', j: 6 }, { i: '0', j: 7 } ], css_id: 'tinytable_css_wzm6zwgb7a03qw5ge6vu',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_mynho3adhfgda9uf5msc.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_mynho3adhfgda9uf5msc td.tinytable_css_iyr490f62ldseaid6j3q, #tinytable_mynho3adhfgda9uf5msc th.tinytable_css_iyr490f62ldseaid6j3q {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; font-weight: bold }
    #tinytable_mynho3adhfgda9uf5msc td.tinytable_css_ckj9emhtucah3tmhv9c3, #tinytable_mynho3adhfgda9uf5msc th.tinytable_css_ckj9emhtucah3tmhv9c3 { font-weight: bold }
    #tinytable_mynho3adhfgda9uf5msc td.tinytable_css_f3r2yn07x9bnqxr8qrk4, #tinytable_mynho3adhfgda9uf5msc th.tinytable_css_f3r2yn07x9bnqxr8qrk4 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; font-weight: bold }
    #tinytable_mynho3adhfgda9uf5msc td.tinytable_css_oy81sma5yu9i9dd63s87, #tinytable_mynho3adhfgda9uf5msc th.tinytable_css_oy81sma5yu9i9dd63s87 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%;  }
    #tinytable_mynho3adhfgda9uf5msc td.tinytable_css_wzm6zwgb7a03qw5ge6vu, #tinytable_mynho3adhfgda9uf5msc th.tinytable_css_wzm6zwgb7a03qw5ge6vu {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%;  }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_mynho3adhfgda9uf5msc" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1">Outcome</th>
                <th scope="col" data-row="0" data-col="2">Best df</th>
                <th scope="col" data-row="0" data-col="3">Linear AIC</th>
                <th scope="col" data-row="0" data-col="4">Spline AIC</th>
                <th scope="col" data-row="0" data-col="5">Linear BIC</th>
                <th scope="col" data-row="0" data-col="6">Spline BIC</th>
                <th scope="col" data-row="0" data-col="7">ΔAIC</th>
                <th scope="col" data-row="0" data-col="8">ΔBIC</th>
                <th scope="col" data-row="0" data-col="9">2|ΔBIC|</th>
                <th scope="col" data-row="0" data-col="10">Verdict</th>
              </tr>
        </thead>
        
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">H1a: Sleep Quality</td>
                  <td data-row="1" data-col="2">2</td>
                  <td data-row="1" data-col="3">4153.4</td>
                  <td data-row="1" data-col="4">4147.2</td>
                  <td data-row="1" data-col="5">4211.5</td>
                  <td data-row="1" data-col="6">4211.2</td>
                  <td data-row="1" data-col="7">-6.2</td>
                  <td data-row="1" data-col="8">-0.3</td>
                  <td data-row="1" data-col="9">0.6</td>
                  <td data-row="1" data-col="10">≈</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">H1b: Sleep Duration</td>
                  <td data-row="2" data-col="2">2</td>
                  <td data-row="2" data-col="3">6895.4</td>
                  <td data-row="2" data-col="4">6891.4</td>
                  <td data-row="2" data-col="5">6947.8</td>
                  <td data-row="2" data-col="6">6949.6</td>
                  <td data-row="2" data-col="7">-4.0</td>
                  <td data-row="2" data-col="8">1.8</td>
                  <td data-row="2" data-col="9">3.6</td>
                  <td data-row="2" data-col="10">+ L</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">H1c: Daytime Sleepiness</td>
                  <td data-row="3" data-col="2">2</td>
                  <td data-row="3" data-col="3">12762.5</td>
                  <td data-row="3" data-col="4">12755.3</td>
                  <td data-row="3" data-col="5">12820.7</td>
                  <td data-row="3" data-col="6">12819.3</td>
                  <td data-row="3" data-col="7">-7.2</td>
                  <td data-row="3" data-col="8">-1.4</td>
                  <td data-row="3" data-col="9">2.8</td>
                  <td data-row="3" data-col="10">+ S</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">H1d: Wellbeing</td>
                  <td data-row="4" data-col="2">2</td>
                  <td data-row="4" data-col="3">31144.8</td>
                  <td data-row="4" data-col="4">31138.4</td>
                  <td data-row="4" data-col="5">31211.2</td>
                  <td data-row="4" data-col="6">31211.5</td>
                  <td data-row="4" data-col="7">-6.4</td>
                  <td data-row="4" data-col="8">0.3</td>
                  <td data-row="4" data-col="9">0.6</td>
                  <td data-row="4" data-col="10">≈</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::



::: {.cell}
::: {.cell-output-display}
![Marginal predicted outcomes as a function of late-night gaming (natural cubic spline, complete-case data). Solid blue line: spline fit with 95% delta-method CI ribbon. Dashed orange line: corresponding linear model fit, with its own 95% delta-method CI ribbon for comparison. X-axis shows average late-night gaming minutes per day on a common 0–90 min scale across panels; curves are drawn over the observed support within that range. All other covariates held at reference values (scaled continuous predictors at zero; isWeekend = 0; reference region). H1a: P(Fairly bad or Very bad sleep quality), marginalised over the participant random intercept via the marginal probit formula. H1b–H1d: predicted means on the original scale. Each panel title shows the selected spline df; AIC and BIC values are reported in @tbl-appendix-panel-h1-spline-bic.](manuscript_files/figure-html/fig-appendix-panel-h1-spline-bic-1.png){#fig-appendix-panel-h1-spline-bic width=864}
:::
:::


```{=typst}
#pagebreak()
```

## Panel: Equivalence Testing for All Confirmatory Hypotheses {#sec-appendix-equivalence}


::: {.cell}

:::


We conducted frequentist equivalence tests (TOST; @lakens2017equivalence; @lakens2018equivalence) for every confirmatory hypothesis (H1a–H1d and H2a–H2d), including those that were statistically significant, by checking whether each pooled 90% CI falls inside a Region of Practical Equivalence (ROPE). The ROPE width itself was not preregistered; we adopt Kruschke's [@kruschke2018rejecting] rule-of-thumb ±0.1 × SD(*y*) as a conventional anchor commonly used in the equivalence-testing literature. Because the focal predictors are raw late-night play-time variables (per 10 min/day) rather than standardized effect sizes, Kruschke's default ROPE of ±0.1 × SD(*y*) is rescaled to the coefficient's native units as ±0.1 × SD(*y*) / SD(*x*); for the ordinal probit models (H1a, H2a) the latent residual SD is fixed at 1 by identification, so the ROPE simplifies to ±0.1 / SD(*x*) on the latent scale, which at a category threshold corresponds to a ≈ 3.99 percentage-point shift in cumulative category probability (from $\Phi(0.05) - \Phi(-0.05)$). The TOST rule accepts equivalence when the 90% CI lies entirely inside the ROPE, rejects when it lies entirely outside, and otherwise returns "Undecided". The natural-unit interpretation of the H1b–H1d response-scale half-widths (≈ ±7 min of sleep, ±0.38 ESS points, and ±0.53 SWEMWBS points respectively) is given in the Discussion alongside the relevant clinical anchors. The same ±0.1 × SD(*y*) thresholds apply to the H2 interaction coefficients but now describe the change in the late-night gaming slope induced by a one-SD shift in chronotype (MSFsc); this is a stringent benchmark because the product predictor's SD (≈ 16.53 for late-night minutes × MSFsc) is substantially larger than that of either factor alone.


::: {#tbl-appendix-equivalence}


::: {.cell}
::: {.cell-output-display}

```{=html}
<!-- preamble start -->

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_9142l7aueeorl5meq9aw = TinyTable.createTableFunctions("tinytable_9142l7aueeorl5meq9aw");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '8', j: 2 }, { i: '8', j: 3 }, { i: '8', j: 4 }, { i: '8', j: 5 } ], css_id: 'tinytable_css_gervx4hvi73qsv3l504i',}, 
          { positions: [ { i: '1', j: 2 }, { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '5', j: 2 }, { i: '6', j: 2 }, { i: '7', j: 2 }, { i: '1', j: 3 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '5', j: 3 }, { i: '6', j: 3 }, { i: '7', j: 3 }, { i: '1', j: 4 }, { i: '2', j: 4 }, { i: '3', j: 4 }, { i: '4', j: 4 }, { i: '5', j: 4 }, { i: '6', j: 4 }, { i: '7', j: 4 }, { i: '1', j: 5 }, { i: '2', j: 5 }, { i: '3', j: 5 }, { i: '4', j: 5 }, { i: '5', j: 5 }, { i: '6', j: 5 }, { i: '7', j: 5 } ], css_id: 'tinytable_css_cb7j7j89pxpn25s2xue9',}, 
          { positions: [ { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 }, { i: '0', j: 5 } ], css_id: 'tinytable_css_a77hf96yuymf9qaqflml',}, 
          { positions: [ { i: '8', j: 1 } ], css_id: 'tinytable_css_mzi7y3gi4catevb5suck',}, 
          { positions: [ { i: '1', j: 1 }, { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '4', j: 1 }, { i: '5', j: 1 }, { i: '6', j: 1 }, { i: '7', j: 1 } ], css_id: 'tinytable_css_hv7lkpnw2rodwxcfvfqf',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_dwdcrrkl6cofo3kq6dys',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_9142l7aueeorl5meq9aw.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_9142l7aueeorl5meq9aw td.tinytable_css_gervx4hvi73qsv3l504i, #tinytable_9142l7aueeorl5meq9aw th.tinytable_css_gervx4hvi73qsv3l504i {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: right; font-size: 0.85em }
    #tinytable_9142l7aueeorl5meq9aw td.tinytable_css_cb7j7j89pxpn25s2xue9, #tinytable_9142l7aueeorl5meq9aw th.tinytable_css_cb7j7j89pxpn25s2xue9 { text-align: right; font-size: 0.85em }
    #tinytable_9142l7aueeorl5meq9aw td.tinytable_css_a77hf96yuymf9qaqflml, #tinytable_9142l7aueeorl5meq9aw th.tinytable_css_a77hf96yuymf9qaqflml {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: right; font-size: 0.85em }
    #tinytable_9142l7aueeorl5meq9aw td.tinytable_css_mzi7y3gi4catevb5suck, #tinytable_9142l7aueeorl5meq9aw th.tinytable_css_mzi7y3gi4catevb5suck {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left; font-size: 0.85em }
    #tinytable_9142l7aueeorl5meq9aw td.tinytable_css_hv7lkpnw2rodwxcfvfqf, #tinytable_9142l7aueeorl5meq9aw th.tinytable_css_hv7lkpnw2rodwxcfvfqf { text-align: left; font-size: 0.85em }
    #tinytable_9142l7aueeorl5meq9aw td.tinytable_css_dwdcrrkl6cofo3kq6dys, #tinytable_9142l7aueeorl5meq9aw th.tinytable_css_dwdcrrkl6cofo3kq6dys {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: black; --line-color-left: black; --line-color-right: black; --line-color-top: black; --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left; font-size: 0.85em }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_9142l7aueeorl5meq9aw" style="table-layout: fixed; width: 100% !important; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1">Hypothesis</th>
                <th scope="col" data-row="0" data-col="2">ROPE</th>
                <th scope="col" data-row="0" data-col="3">90% CI</th>
                <th scope="col" data-row="0" data-col="4">Decision</th>
                <th scope="col" data-row="0" data-col="5">p</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='5'>ROPE = Region of Practical Equivalence, rescaled to the native units of the raw coefficient: ±0.1 × SD(y) / SD(x) for linear mixed models and ±0.1 / SD(x) for the ordinal probit models (H1a, H2a), where the latent residual SD is fixed at 1. SD(x) is the sample SD of the focal predictor (product predictor for interactions).</td></tr>
<tr><td colspan='5'>Decision via TOST rule (Lakens, 2017): Accepted = 90% CI entirely inside ROPE; Rejected = 90% CI entirely outside ROPE; Undecided = otherwise.</td></tr>
<tr><td colspan='5'>p = Wald p-value for the pooled coefficient (not the TOST equivalence p).</td></tr></tfoot>
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">H1a: Sleep Quality</td>
                  <td data-row="1" data-col="2">[-0.028, 0.028]</td>
                  <td data-row="1" data-col="3">[0.028, 0.075]</td>
                  <td data-row="1" data-col="4">Undecided</td>
                  <td data-row="1" data-col="5">< .001</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">H1b: Sleep Duration</td>
                  <td data-row="2" data-col="2">[-0.033, 0.033]</td>
                  <td data-row="2" data-col="3">[-0.016, 0.006]</td>
                  <td data-row="2" data-col="4">Accepted</td>
                  <td data-row="2" data-col="5">0.456</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">H1c: Daytime Sleepiness</td>
                  <td data-row="3" data-col="2">[-0.107, 0.107]</td>
                  <td data-row="3" data-col="3">[-0.026, 0.053]</td>
                  <td data-row="3" data-col="4">Accepted</td>
                  <td data-row="3" data-col="5">0.577</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">H1d: Wellbeing</td>
                  <td data-row="4" data-col="2">[-0.134, 0.134]</td>
                  <td data-row="4" data-col="3">[-0.033, 0.024]</td>
                  <td data-row="4" data-col="4">Accepted</td>
                  <td data-row="4" data-col="5">0.805</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">H2a: Sleep Quality × Chronotype</td>
                  <td data-row="5" data-col="2">[-0.006, 0.006]</td>
                  <td data-row="5" data-col="3">[-0.011, 0.005]</td>
                  <td data-row="5" data-col="4">Undecided</td>
                  <td data-row="5" data-col="5">0.487</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">H2b: Sleep Duration × Chronotype</td>
                  <td data-row="6" data-col="2">[-0.007, 0.007]</td>
                  <td data-row="6" data-col="3">[-0.001, 0.007]</td>
                  <td data-row="6" data-col="4">Accepted</td>
                  <td data-row="6" data-col="5">0.199</td>
                </tr>
                <tr>
                  <td data-row="7" data-col="1">H2c: Daytime Sleepiness × Chronotype</td>
                  <td data-row="7" data-col="2">[-0.022, 0.022]</td>
                  <td data-row="7" data-col="3">[-0.021, 0.003]</td>
                  <td data-row="7" data-col="4">Accepted</td>
                  <td data-row="7" data-col="5">0.225</td>
                </tr>
                <tr>
                  <td data-row="8" data-col="1">H2d: Wellbeing × Chronotype</td>
                  <td data-row="8" data-col="2">[-0.028, 0.028]</td>
                  <td data-row="8" data-col="3">[-0.018, 0.000]</td>
                  <td data-row="8" data-col="4">Accepted</td>
                  <td data-row="8" data-col="5">0.101</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::


Equivalence test results for the focal predictor in every confirmatory hypothesis. H1a–H1d test the pooled late-night gaming coefficient; H2a–H2d test the pooled chronotype × late-night gaming interaction. Statistically significant effects (e.g., H1a) are included for completeness — a significant effect paired with an "Undecided" or "Rejected" equivalence decision indicates that the estimate, although reliably non-zero, is not small enough to be declared practically equivalent to the null under the ROPE.

:::

