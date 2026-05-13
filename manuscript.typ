// Some definitions presupposed by pandoc's typst output.
#let horizontalrule = line(start: (25%, 0%), end: (75%, 0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it
    .children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
    ])
    .join()
}

// Some quarto-specific definitions.

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }
}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(kind: kind, supplement: supplement, caption: caption, {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(
          subrefnumbering,
          n-super,
          quartosubfloatcounter.get().first() + 1,
        ))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(
              2,
            ) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      })#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(old_title_block, block_with_new_content(
    old_title_block.body,
    old_title_block.body.body.children.at(0) + old_title_block.body.body.children.at(1) + new_title,
  ))

  block_with_new_content(
    old_callout,
    block(below: 0pt, new_title_block) + old_callout.body.children.at(1),
  )
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(
  body: [],
  title: "Callout",
  background_color: rgb("#dddddd"),
  icon: none,
  icon_color: black,
  body_background_color: white,
) = {
  block(
    breakable: false,
    fill: background_color,
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"),
    width: 100%,
    radius: 2pt,
    block(inset: 1pt, width: 100%, below: 0pt, block(
      fill: background_color,
      width: 100%,
      inset: 4pt,
    )[#text(icon_color, weight: 900)[#icon] #title])
      + if (body != []) {
        block(inset: 1pt, width: 100%, block(fill: body_background_color, width: 100%, inset: 8pt, [
          #set align(left)
          #set par(first-line-indent: 0em)
          #body
        ]))
      },
  )
}
// Standalone Typst preprint template

// Imports
#import "@preview/fontawesome:0.5.0": *
#import "@preview/wordometer:0.1.5": total-words, word-count

// Appendix function. To use, include in .typ before appendix header
// #show: appendix.with(prefix: "A")
#let appendix(prefix: "A", columns: 1, numbering: none, doc) = {
  set page(columns: columns)

  // Add pagebreak before each level 1 heading in appendices and reset counters
  show heading.where(level: 1): it => {
    pagebreak(weak: true)
    // Reset figure counters for Quarto-specific kinds
    counter(figure.where(kind: "quarto-float-fig")).update(0)
    counter(figure.where(kind: "quarto-float-tbl")).update(0)
    counter(figure.where(kind: "quarto-float-lst")).update(0)

    // Reset callout counters (for each callout type used)
    counter(figure.where(kind: "quarto-callout-Note")).update(0)
    counter(figure.where(kind: "quarto-callout-Warning")).update(0)
    counter(figure.where(kind: "quarto-callout-Tip")).update(0)
    counter(figure.where(kind: "quarto-callout-Important")).update(0)
    counter(figure.where(kind: "quarto-callout-Caution")).update(0)

    // Reset generic counters
    counter(figure.where(kind: image)).update(0)
    counter(figure.where(kind: table)).update(0)
    counter(math.equation).update(0)
    it
  }

  // Numberings
  set heading(supplement: [Appendix], numbering: (..nums) => {
    let levels = nums.pos()
    [#prefix#levels.map(str).join(".")]
  })
  // Hide level 2+ headings from TOC in appendices
  set heading(outlined: false)
  show heading.where(level: 1): set heading(outlined: true)

  set figure(numbering: it => {
    let h = context counter(heading).get().first()
    [#prefix#h.#it]
  })
  set math.equation(numbering: it => {
    let h = context counter(heading).get().first()
    [(#prefix#h.#it)]
  })

  // Reset heading counter
  counter(heading).update(0)

  doc
}

#let preprint(
  // Theme (sets defaults for layout)
  theme: "default",
  // Document metadata
  title: none,
  running-head: none,
  subtitle: none,
  authors: (),
  affiliations: none,
  abstract: none,
  categories: none,
  wordcount: none,
  authornote: none,
  citation: none, // Not used currently
  date: none, // Not used currently
  corresponding-text: "Send correspondence to:",
  // Layout settings (can override theme defaults)
  leading: 0.5em,
  spacing: 0.6em,
  first-line-indent: 1.8em,
  all: false,
  linkcolor: blue,
  fontcolor: black,
  backgroundcolor: white,
  monobackgroundcolor: none,
  headingcolor: none,
  strongcolor: none,
  margin: (x: 2.8cm, y: 2.6cm),
  paper: "a4",
  // Typography settings
  lang: "en",
  region: "US",
  font: "libertinus serif",
  monofont: "Dejavu Sans Mono",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  // Structure settings
  sectionnumbering: none,
  pagenumbering: "1",
  linenumbering: none,
  mathnumbering: "(1)",
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  cols: 1,
  col-gutter: 4.2%,
  // Bibliography settings (no effect if citeproc used)
  bibliography-title: "References",
  bibliographystyle: "apa",
  doc,
) = {
  // Theme configurations
  let themes = (
    jou: (margin: (x: 2cm, y: 2.6cm), fontsize: 10pt, cols: 2),
    dracula: (
      backgroundcolor: rgb("#282A36"),
      fontcolor: rgb("#F8F8F2"),
      linkcolor: rgb("#FF5555"),
      monobackgroundcolor: rgb("#44475A"),
      headingcolor: rgb("#BD93F9"),
      strongcolor: rgb("#50FA7B"),
    ),
  )

  // Apply theme if it exists
  if theme in themes {
    let config = themes.at(theme)
    margin = config.at("margin", default: margin)
    fontsize = config.at("fontsize", default: fontsize)
    cols = config.at("cols", default: cols)
    linkcolor = config.at("linkcolor", default: linkcolor)
    fontcolor = config.at("fontcolor", default: fontcolor)
    backgroundcolor = config.at("backgroundcolor", default: backgroundcolor)
    monobackgroundcolor = config.at("monobackgroundcolor", default: monobackgroundcolor)
    headingcolor = config.at("headingcolor", default: headingcolor)
    strongcolor = config.at("strongcolor", default: strongcolor)
  }

  /* Document settings */
  set document(
    title: title,
    author: if authors != none { authors.map(a => str(a.name.text)) } else { () },
    description: abstract,
    keywords: if categories != none { categories.text } else { "" },
  )
  // Link and cite colors
  show link: set text(fill: linkcolor)
  show cite: set text(fill: linkcolor) // No effect when `citeproc: true`

  // Customize Typst bibliography (no effect if using citeproc)
  set bibliography(title: bibliography-title, style: bibliographystyle)
  show bibliography: set par(spacing: spacing, leading: leading)

  // List spacing
  show list: it => {
    // Space between list items
    set par(leading: 0.48em)
    // Space around whole list
    set block(
      spacing: spacing * 1.2,
      inset: (left: first-line-indent, right: first-line-indent),
    )
    it
  }

  // Number equations
  set math.equation(numbering: mathnumbering)
  // Add space around math blocks
  show math.equation.where(block: true): set block(spacing: spacing * 1.6)

  // Define space around block quotes
  show quote.where(block: true): set block(spacing: spacing * 1.8)
  // Don't indent anything in block quotes
  show quote.where(block: true): set par(first-line-indent: 0em)

  /* Improved figure display */
  // Add space above and below
  show figure: f => { [#v(leading) #f #v(leading) ] }
  // Set block width to align caption to page/column
  // Target figure only as could otherwise mess with table formatting
  show figure.where(kind: "quarto-float-fig"): set block(width: 100%)
  // Left-align captions and italicize "Figure X."
  show figure.caption: it => [
    #set align(left)
    #set par(first-line-indent: 0em)
    #emph([#it.supplement #context it.counter.display(it.numbering).])
    #it.body
  ]

  /* Page layout settings */
  set page(
    paper: paper,
    margin: margin,
    numbering: none,
    columns: cols,
    header-ascent: 50%,
    header: context {
      if (counter(page).get().at(0) > 1) [
        #grid(
          columns: (1fr, 1fr),
          align(left)[#running-head], align(right)[#counter(page).display(pagenumbering)],
        )
      ]
    },
    footer-descent: 10%,
    fill: backgroundcolor,
  )
  set columns(gutter: col-gutter)

  /* Typography settings */

  // Paragraph settings
  set par(justify: true, leading: leading, spacing: spacing, first-line-indent: (amount: first-line-indent, all: all))
  set par.line(numbering: linenumbering)

  // Text settings
  set text(
    lang: lang,
    region: region,
    font: font,
    size: fontsize,
    fill: fontcolor,
  )
  // Strong/bold text
  show strong: it => {
    if strongcolor != none {
      text(fill: strongcolor, it)
    } else {
      it
    }
  }
  // Code font
  show raw: set text(font: monofont)
  show raw.where(block: true): it => {
    if monobackgroundcolor != none {
      block(fill: monobackgroundcolor, width: 100%, inset: 8pt, radius: 2pt, it)
    } else {
      block(fill: luma(230), width: 100%, inset: 8pt, radius: 2pt, it)
    }
  }

  // Headers
  set heading(numbering: sectionnumbering)
  if headingcolor != none {
    show heading: set text(fill: headingcolor)
  }
  show heading.where(level: 1): it => block(width: 100%, below: 0.8em, above: 1em)[
    #set align(center)
    #set text(size: fontsize * 1.1, weight: "bold")
    #it
  ]
  show heading.where(level: 2): it => block(width: 100%, below: 0.8em, above: 1em)[
    #set text(size: fontsize * 1.05)
    #it
  ]
  show heading.where(level: 3): it => block(width: 100%, below: 0.6em, above: 0.8em)[
    #set text(size: fontsize, style: "italic")
    #it
  ]
  // Level 4 & 5 headers are in paragraph
  show heading.where(level: 4): it => box(inset: (top: 0em, bottom: 0em, left: 0em, right: 0.1em), text(
    size: 1em,
    weight: "bold",
    it.body + [.],
  ))
  show heading.where(level: 5): it => box(inset: (top: 0em, bottom: 0em, left: 0em, right: 0.1em), text(
    size: 1em,
    weight: "bold",
    style: "italic",
    it.body + [.],
  ))

  // Helper for unnumbered footnotes
  let footnote_non_numbered(body) = {
    footnote(numbering: _ => [], body)
    counter(footnote).update(n => if n > 0 { n - 1 } else { 0 })
  }

  // Collect author metadata once
  let corresponding_authors = if authors != none {
    authors.filter(a => (a.keys().contains("corresponding") and a.at("corresponding") == true))
  } else { () }

  let equal_authors = if authors != none {
    authors.filter(a => (
      a.keys().contains("equal-contributor") and a.at("equal-contributor") == true
    ))
  } else { () }

  // Find first author indices for each footnote type
  let first_corresponding_idx = if corresponding_authors.len() > 0 {
    authors.position(a => corresponding_authors.contains(a))
  } else { none }

  let first_equal_idx = if equal_authors.len() > 1 {
    authors.position(a => equal_authors.contains(a))
  } else { none }

  // Construct author display with inline footnotes
  let author_display = if authors != none {
    let result = authors
      .enumerate()
      .map(((idx, a)) => {
        let parts = (a.name,)
        if authors.len() > 1 { parts.push(super(a.affiliation)) }

        // Add correspondence footnote to first corresponding author
        if corresponding_authors.contains(a) and idx == first_corresponding_idx {
          parts.push(footnote(numbering: _ => "*")[
            #corresponding-text #corresponding_authors.map(a => [#a.name, #a.email]).join(", ", last: " & ").
          ])
        } else if corresponding_authors.contains(a) {
          parts.push(super("*"))
        }

        // Add equal contributor footnote to first equal contributor
        if equal_authors.len() > 1 and equal_authors.contains(a) and idx == first_equal_idx {
          parts.push(footnote(numbering: _ => "†")[
            #equal_authors.map(a => a.name).join(", ", last: " & ") contributed equally to this work.
          ])
        } else if equal_authors.len() > 1 and equal_authors.contains(a) {
          parts.push(super("†"))
        }

        if a.keys().contains("orcid") {
          parts.push(link(a.orcid, fa-orcid(fill: rgb("a6ce39"), size: 0.8em)))
        }
        parts.join()
      })
      .join(", ", last: " & ")

    // Add author note as unnumbered footnote (if provided)
    if authornote != none {
      result + footnote_non_numbered(authornote)
    } else {
      result
    }
  } else { none }

  // Hack: Include authors outside of "scope: parent" to ensure footnotes show
  if author_display != none {
    hide(author_display)
    counter(footnote).update(n => if n > 0 { n - 1 } else { 0 })
    v(-2.4em)
  }

  // Place title, author, abstract always in one column
  place(top, scope: "parent", float: true, {
    if title != none {
      align(center)[
        #block(width: 100%, above: 0em, below: 0em)[
          #text(weight: "bold", size: title-size)[#title]
        ]
      ]
    }
    if subtitle != none {
      align(center)[
        #block(width: 100%, above: 1em, below: 0em)[
          #text(weight: "bold", size: subtitle-size)[#subtitle]
        ]
      ]
    }

    if author_display != none {
      align(center)[
        #block(width: 100%, above: 2em, below: 0em)[
          #text(weight: "regular", size: subtitle-size)[#author_display]
        ]
      ]
    }

    if affiliations != none {
      align(center)[
        #block(width: 100%, above: 1em, below: 2em)[
          #text(weight: "regular", size: 1.1em)[
            #for a in affiliations [
              #if authors.len() > 1 [#super[#a.id]]#a.name#if a.keys().contains("department") [, #a.department] \
            ]
          ]
        ]
      ]
    }

    /* Abstract and metadata section */
    block(inset: (bottom: if toc { 0em } else { 2em }, left: 2.4em, right: 2.4em))[
      #set text(size: 0.92em)
      #set par(first-line-indent: 0em)
      #if abstract != none {
        abstract
      }
      #if categories != none {
        block()[#v(0.4em)#text(style: "italic")[Keywords:] #categories]
      }
      #if wordcount == true {
        block()[#text(style: "italic")[Words:] #total-words]
      }
    ]

    // Reset footnote counter for the main document
    counter(footnote).update(0)

    // Table of contents
    if toc {
      block(inset: (top: 1em, bottom: 2em, left: 2.4em, right: 2.4em))[
        #outline(
          title: toc_title,
          depth: toc_depth,
          indent: toc_indent,
        )
      ]
    }
  })

  // Word count with wordometer package
  show: word-count.with(exclude: (<refs>))

  /* Document content */
  doc
}

#show: doc => preprint(
// Default Quarto template variables
  title: [Late-Night Gaming and Sleep in Adults: A Registered Report Using Multi-Platform Telemetry],
  subtitle: [Chronotype, Sleep, and Wellbeing in Adult Gamers],
  running-head: [Late-Night Gaming and Sleep],
  authors: (
        (
        name: [Tamás A. Földes],
        affiliation: [1],
        corresponding: true,
        
        orcid: "https://orcid.org/0000-0002-0623-9149",
        email: [contact\@tamasfoldes.mozmail.com]
      ),
        (
        name: [Second Author],
        affiliation: [2],
        
        
        
        
      ),
    ),
  affiliations: (
    (
      id: "1",
      name: "First University",
      department: "Department Name"
    ),
    (
      id: "2",
      name: "Second University",
      
    ),
    
  ),
  date: [2026-05-13],
  abstract: [Late-night gaming has been linked to disrupted sleep and poorer wellbeing, but most evidence relies on self-reported play time in adolescent samples and rarely tests chronotype as a moderator in adults. Using the Open Play dataset---a three-month longitudinal study of adult gamers in the UK and US that paired session-level digital-trace data from Nintendo, Xbox, and Steam with biweekly panel and daily-diary self-reports---this Registered Report tested whether late-night gaming (23:00--06:00) predicted (H1a) poorer sleep quality, (H1b) shorter sleep duration, (H1c) greater daytime sleepiness, and (H1d) lower mental wellbeing, and whether these associations were amplified among individuals with more evening-leaning chronotypes (H2). Outcomes were assessed with the PSQI, ESS, and SWEMWBS across six biweekly waves; chronotype was indexed by the MCTQ-derived MSF#sub[sc];. Only H1a was supported: each additional 10 minutes of daily late-night gaming was associated with an approximate 0.9 percentage-point increase in the probability of reporting poor sleep quality, consistent across imputed, complete-case, and exploratory diary specifications. TOST equivalence tests accepted the null for H1b--H1d, bounding any true effects below ±7 minutes of nightly sleep, ±0.38 Epworth points, and ±0.53 SWEMWBS points per one-SD (≈36 min/day) increase in late-night gaming. None of the H2 chronotype × late-night gaming interactions reached significance in the pooled imputation models; equivalence tests accepted the null for H2b--H2d, with H2a undecided. The findings do not support the sleep-displacement account, which is primarily concerned with reduced sleep duration as equivalence tests ruled out meaningful effects on sleep duration, daytime sleepiness, wellbeing, and chronotype-based vulnerability. The small but robust association with subjective sleep quality is more consistent with arousal or attribution mechanisms than with sleep displacement per se.

],
  pagenumbering: "1",
  linenumbering: none,
  toc_title: [Table of contents],
  toc_depth: 3,
// Additional Typst variables
  authornote: [Author notes go here.

],
// Use categories or keywords
  categories: [late-night gaming, sleep quality, sleep duration, daytime sleepiness, mental wellbeing, chronotype, digital trace data, registered report],
  wordcount: true,
// Theme system (unified for standalone and Quarto)
  theme: "jou",
// Explicit overrides (optional)
  doc,
)

= Introduction
<introduction>
Concerns have been raised about the potential negative impacts of video gaming on sleep and overall wellbeing, particularly for adolescents and young adults and especially when gaming occurs late at night @higuchi2005effects@king2013impact@peracchia2018exposure. Heavy or problematic gaming has been shown to disrupt sleep patterns, reduce sleep duration, lower sleep quality, and increase daytime sleepiness @exelmans2015sleep@han2024electronic@kristensen2021problematic. Crucially, however, #cite(<kristensen2021problematic>, form: "prose") noted that none of the studies in their review registered the time of day gaming took place, meaning that timing-specific effects --- particularly those concentrated in the pre-sleep window --- remain poorly characterised. This is especially concerning given the far-reaching effects of sleep disturbances on cognitive and emotional functioning @mccoy2011cognitive@simon2020sleep@vriend2013manipulating. For instance, habitual gaming between 10 p.m. and 6 a.m. has been associated with an increased risk of depressive symptoms, partially mediated by daytime sleepiness @lemola2011habitual. Understanding the consequences of late-night gaming is thus vital for both gamers and health professionals.

== Mechanisms Linking Late-Night Gaming to Sleep Disturbance
<mechanisms-linking-late-night-gaming-to-sleep-disturbance>
Two key mechanisms have been proposed to explain the impact of late-night digital engagement---including gaming---on sleep @cain2010electronic@lebourgeois2017digital. The first is the displacement hypothesis, which argues that late-night gaming is more harmful than daytime gaming because it cuts into sleep time @twenge2019more@lemola2011habitual@exelmans2015sleep. Gamers often feel compelled to continue playing and may struggle with self-regulation, which can lead to insufficient sleep @king2009understanding@pirrone2024why@spada2017metacognitions. For example, adolescents with high trait flow delayed bedtime by \~90 minutes when playing challenging games @smith2017mechanisms.

The second mechanism involves arousal-related disturbances in sleep architecture caused by late-night gaming. Empirical studies have shown that extended gaming, especially when involving violent content, significantly decreases REM sleep and total sleep time @king2013impact. #cite(<weaver2010effect>, form: "prose") found that pre-sleep gaming extended sleep latency by approximately 5--10 minutes, while #cite(<king2013impact>, form: "prose") demonstrated that such arousal-related disturbances can also alter the natural progression into sleep stages. This delay in sleep onset could be exacerbated by lower melatonin levels following an evening of gaming, compared to neutral activities like board games, which are crucial for regulating the sleep-wake cycle @hartmann2019effects.

== The Moderating Role of Chronotype
<the-moderating-role-of-chronotype>
Negative effects of late-night gaming may be compounded among individuals with an eveningness chronotype---a group naturally predisposed to staying up late and consistently shown to spend more time on screen-based media in the pre-sleep window @reardon2023adolescent@kortesoja2023latenight. Pre-sleep technology use, in turn, appears to affect this group more strongly: in a large adult cohort, daily screen use before bed was associated with later bedtimes and shorter sleep among both chronotypes, but the delay and sleep loss were markedly greater in evening types --- a pattern the authors interpret as evidence of compounded social jetlag, i.e.~the misalignment between endogenous circadian rhythms and socially imposed schedules @zhong2025electronic. Adolescent evidence is consistent: late-night digital media use mediates the link between evening chronotype and poorer sleep quality and daytime tiredness, and these effects are most pronounced for evening types @kortesoja2023latenight.

The downstream picture for psychosocial outcomes is more nuanced. #cite(<reardon2023adolescent>, form: "prose") found that shorter sleep on weekdays was associated with greater psychological distress, but technology medium and chronotype themselves were not direct predictors of distress. #cite(<gumport2021impact>, form: "prose") reported that, in evening-type adolescents, technology use was linked to #emph[better] emotional, social, cognitive, and physical health but #emph[worse] behavioral health (sensation-seeking, ADHD diagnosis, and alcohol/substance use); notably, electronic game use specifically was not associated with behavioral health in that study --- the behavioral-health association was driven by other technology uses. The adult evidence reviewed above @zhong2025electronic suggests that the chronotype-amplified effect of pre-sleep screen exposure on sleep timing and duration is not confined to adolescence, but adult evidence specifically isolating #emph[gaming] (rather than screen use broadly) within the pre-sleep window remains sparse.

== The Present Study
<the-present-study>
In sum, the literature indicates that video gaming, particularly when it occurs late at night, has significant implications for sleep quality, sleep duration, and overall wellbeing. This disruption can be attributed to both the displacement hypothesis @twenge2019more@lemola2011habitual@exelmans2015sleep and arousal-related disturbances in sleep architecture @king2013impact. Individual differences, such as chronotype, may moderate these effects, with eveningness chronotypes particularly vulnerable to the negative consequences of pre-sleep screen exposure @zhong2025electronic@kortesoja2023latenight. The present study aims to empirically test the following hypotheses regarding the relationship between late-night gaming and sleep outcomes:

H1: Late-night gaming is associated with:

- H1a: Poorer sleep quality
- H1b: Shorter sleep duration
- H1c: Higher daytime sleepiness
- H1d: Lower wellbeing

In addition to testing direct relationships between late-night gaming and various sleep-related outcomes are critical to understand, we further assess the potential moderating role of chronotype, which refers to a person's natural preference for activities during certain times of the day (morningness or eveningness). Individuals with an evening chronotype tend to stay up later and may be more inclined to engage in late-night gaming, potentially exacerbating the negative impacts on sleep and wellbeing. The combination of an evening chronotype and late-night gaming may even have a compounded effect on overall wellbeing, as both factors are independently associated with poorer mental health outcomes. Given this, we propose the following:

H2: Chronotype moderates the relationships between late-night gaming and all outcomes in H1 (sleep quality, sleep duration, daytime sleepiness, and wellbeing), such that these negative associations are stronger for individuals with more of an eveningness chronotype.

By examining chronotype on a continuous scale as a moderating factor, this study seeks to provide a more nuanced understanding of the potential risks associated with late-night gaming and to identify individuals who may be most vulnerable to its negative effects.

= Methods
<methods>
== Data Source and Measures
<data-source-and-measures>
The analyses reported here are part of a Stage 1 Registered Report @ballou2024psychological and utilize data from the Open Play dataset @ballou2025openplay, a longitudinal study that collected multi-platform video game digital trace data alongside psychological measures from adult gamers in the UK and US over a three-month period. The study combined objective behavioral telemetry from gaming platforms with repeated self-report surveys administered biweekly across six waves. Importantly, the present analyses use only a subset of the Open Play dataset, specifically data from Nintendo, Xbox, and Steam platforms, as these provide session-level (Nintendo, Xbox) or near session-level (Steam) temporal granularity necessary for hourly aggregation of playtime to operationalize late-night gaming (23:00--06:00). The following validated measures were administered via panel surveys at multiple timepoints: Wellbeing was assessed using the Short Warwick-Edinburgh Mental Well-being Scale \[SWEMWBS; #cite(<tennant2007warwick>, form: "prose");\], a 7-item measure of mental wellbeing covering psychological functioning and subjective well-being over the past 2 weeks, with responses on a 5-point Likert scale ranging from "None of the time" to "All of the time" (score range: 7--35).

Sleep quality and duration were assessed using the Pittsburgh Sleep Quality Index \[PSQI; #cite(<buysse1989pittsburgh>, form: "prose");\], a 19-item questionnaire evaluating sleep quality over the past month. The measure yields seven component scores (sleep quality, sleep latency, sleep duration, sleep efficiency, sleep disturbances, use of sleep medication, and daytime dysfunction) and a global score (range: 0--21), with scores above 5 indicating poor sleep quality. Excessive daytime sleepiness was measured using the Epworth Sleepiness Scale \[ESS; #cite(<johns1991new>, form: "prose");\], an 8-item scale assessing the likelihood of dozing off in various situations (score range: 0--24). Higher scores indicate greater propensity for daytime sleepiness, with scores above 10 typically indicating clinically significant excessive sleepiness. Chronotype was measured at baseline (Wave 1) using the Munich Chronotype Questionnaire \[MCTQ; #cite(<roenneberg2003life>, form: "prose");\]. The key metric used in this study is MSF#sub[sc] (Mid-Sleep on Free Days corrected for sleep debt on work days), which represents an individual's natural sleep-wake preference when not constrained by social obligations. Higher MSF#sub[sc] values indicate a preference for eveningness (later sleep-wake times).

== Handling Missing Data
<handling-missing-data>
Missingness in the longitudinal self-report outcomes (PSQI components, PSQI sleep duration, Epworth Sleepiness Scale, and SWEMWBS) was addressed via hierarchical two-level multiple imputation by chained equations \[MICE; `mice` v3.16.0 and `miceadds` v3.19 in R; #cite(<vanbuuren2011mice>, form: "prose");; #cite(<robitzsch2024miceadds>, form: "prose");\] using predictive mean matching for multilevel data (`2l.pmm`). This method models participants as clusters with repeated waves nested within, preserving between-person variation that single-level PMM would attenuate. We imputed under a Missing at Random assumption conditional on rich auxiliary information. Because sleep measures (PSQI, ESS) were only collected at waves 2, 4, and 6 by design, the imputation was run in two passes: biweekly variables (SWEMWBS, measured at all six waves) and monthly variables (PSQI components, sleep duration, ESS, measured at waves 2, 4, 6 only). This two-pass approach avoids exposing the mixed-effects model inside `2l.pmm` to the structural NAs that arise at non-measurement waves. The PSQI global score was derived via passive imputation (sum of seven imputed component scores). Following van Buuren's #cite(<vanbuuren2018flexible>, form: "year") multilevel imputation recipe, predictor matrices coded participant ID as the random-intercept cluster (code −2), level-2 (person-level) predictors as fixed effects only (code 1), and level-1 outcome cross-predictors with disaggregated cluster means (code 3). In the biweekly pass, biweekly gaming exposure averages also received code 3; in the monthly pass, wave, gaming, and lag/lead terms remained at code 1 to avoid inflating the predictor count. This coding ensures that both the raw within-person value and its person mean enter the imputation model where appropriate, preserving the between- vs.~within-person decomposition critical for contextual effects. Continuous outcomes were grand-mean centred before imputation and back-transformed afterwards to improve stability of the mixed-effects models inside `2l.pmm` @vanbuuren2018flexible.

Quality control diagnostics (convergence, density, strip, missingness, and between-imputation variability plots) were generated and inspected for both the panel and diary imputations and found to be acceptable. Level-2 (person-level) predictors included static demographics (age, BMI, SES, region, gender recoded as male/female/other) and chronotype (MSF#sub[sc];); level-1 time-varying predictors included wave number, biweekly gaming exposure averages (total and late-night minutes over the preceding 14 days), and ±1-wave temporal lag and lead terms for each outcome variable to capture within-person trajectories. To ensure each participant contributed a full six-wave panel, we first expanded the self-report data to the complete participant × wave grid and inferred survey timestamps for missing waves by aligning observed dates with wave-specific medians; those inferred dates were then used to compute the rolling gaming exposures. For the primary panel dataset we generated 20 imputed datasets with 20 iterations per pass---sufficient to stabilize estimates given wave-specific missingness rates up to \~55%. Diagnostic density and strip plots confirmed plausible imputations. All regressions were fit separately in each imputed dataset and combined using Rubin's rules.

Missingness in the adjustment covariates was limited in the original (pre-imputation) analytical dataset; exact per-covariate counts are reported in the Results section below. The imputed outcomes serve as the primary analytic dataset throughout the main text. Complete-case versions of every regression (using only non-imputed observations for each outcome) were re-estimated in parallel and are reported in the Appendix (#ref(<tbl-appendix-h1-completecase>, supplement: [Table]), #ref(<tbl-appendix-h2-completecase>, supplement: [Table]), #ref(<tbl-appendix-diary-completecase>, supplement: [Table])). Wave-level outcome missingness rates are reported in #ref(<tbl-appendix-wave-missingness>, supplement: [Table]).

= Results
<results>
== Data Quality Controls
<data-quality-controls>
Prior to hypothesis testing, we conducted three positive data quality controls (DQCs) to validate expected patterns in the data. First, self-reported playtime was significantly correlated with digital trace playtime (r = 0.49, 95% CI \[0.47, 0.51\], p \< .001), confirming convergent validity between subjective reports and objective telemetry. Second, social jetlag showed the expected positive association with daytime sleepiness (Spearman's ρ = 0.09, p \< .001, one-sided), replicating established findings that circadian misalignment predicts sleepiness @Fernandes2023jetlag@Wu2025socialjetlag. Third, sleep quality was negatively associated with wellbeing (Spearman's ρ = -0.26, p \< .001, one-sided), consistent with the well-documented relationship between sleep and mental health @Gadie2016how. All three DQCs passed, providing confidence in the integrity of our measures before proceeding with hypothesis testing.

== Sample Demographics
<sample-demographics>
Of 34,922 participants who signed up for the study, 1,948 completed at least one biweekly panel survey. The analytical sample was derived from these participants by applying a three-step filter. First, participants were required to have at least one valid outcome measure (SWEMWBS, PSQI, or ESS) at wave 1, which excluded 107 participants (#emph[n] = 1,841). Second, participants needed valid timezone data (either self-reported or imputed for UK participants) to accurately classify late-night gaming sessions, which excluded a further 120 participants (#emph[n] = 1,721). Third, participants were required to have contributed at least one valid gaming session during the study period, which excluded 143 additional participants, yielding a final analytical sample of #emph[N] = 1,578. #ref(<tbl-demographics>, supplement: [Table]) presents the demographic composition of both the total survey sample and the analytical sample.

#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_0": 0, "2_0": 0, "3_0": 0, "4_0": 0, "8_0": 0, "11_0": 0, "12_0": 0, "14_0": 0, "17_0": 0, "18_0": 0, "19_0": 0, "21_0": 0, "22_0": 0, "25_0": 0, "27_0": 0, "5_0": 1, "6_0": 1, "7_0": 1, "9_0": 1, "10_0": 1, "15_0": 1, "23_0": 1, "24_0": 1, "26_0": 1, "0_1": 2, "2_1": 2, "3_1": 2, "4_1": 2, "5_1": 2, "6_1": 2, "7_1": 2, "8_1": 2, "9_1": 2, "10_1": 2, "11_1": 2, "12_1": 2, "14_1": 2, "15_1": 2, "17_1": 2, "18_1": 2, "19_1": 2, "21_1": 2, "22_1": 2, "23_1": 2, "24_1": 2, "25_1": 2, "26_1": 2, "27_1": 2, "0_2": 2, "2_2": 2, "3_2": 2, "4_2": 2, "5_2": 2, "6_2": 2, "7_2": 2, "8_2": 2, "9_2": 2, "10_2": 2, "11_2": 2, "12_2": 2, "14_2": 2, "15_2": 2, "17_2": 2, "18_2": 2, "19_2": 2, "21_2": 2, "22_2": 2, "23_2": 2, "24_2": 2, "25_2": 2, "26_2": 2, "27_2": 2, "1_0": 3, "13_0": 3, "16_0": 3, "20_0": 3, "1_1": 4, "13_1": 4, "16_1": 4, "20_1": 4, "1_2": 4, "13_2": 4, "16_2": 4, "20_2": 4
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: left,),
    (align: left, indent: 1em,),
    (align: right,),
    (bold: true, align: left,),
    (bold: true, align: right,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 3, stroke: 0.05em + black),
 table.hline(y: 28, start: 0, end: 3, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 3, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Characteristic], [Total], [Analytical],
    ),
    // tinytable header end

    // tinytable cell content after
[A. Sociodemographics], [], [],
[N], [1948], [1578],
[Age], [26.8 (5.0)], [27.1 (5.1)],
[Gender], [], [],
[Woman], [518 (26.6%)], [444 (28.1%)],
[Man], [1211 (62.2%)], [1035 (65.6%)],
[Other], [111 (5.7%)], [99 (6.3%)],
[Region], [], [],
[UK], [719 (36.9%)], [672 (42.6%)],
[US], [1121 (57.5%)], [906 (57.4%)],
[BMI (kg/m²)], [22.0 (7.0)], [22.1 (7.0)],
[SES index], [2.27 (0.54)], [2.26 (0.54)],
[B. Chronotype], [], [],
[No alarm on free days], [1240 (74.4%)], [1141 (74.5%)],
[MCTQ-MSFsc (HH:MM)¹], [06:00 (03:23)], [05:52 (03:05)],
[C. Gaming], [], [],
[Gaming (min/day)¹], [59.6 (138.3)], [83.7 (137.2)],
[LN gaming (min/day)¹], [4.4 (24.1)], [9.3 (30.7)],
[% nights LN gaming], [13.3 (16.3)], [16.4 (16.7)],
[D. Outcomes], [], [],
[Sleep (h)], [7.2 (1.2)], [7.2 (1.1)],
[PSQI global], [6.7 (2.9)], [6.7 (2.8)],
[Sleep quality], [1.3 (0.6)], [1.3 (0.6)],
[Poor sleep (PSQI>5)], [793 (63.8%)], [754 (63.8%)],
[ESS], [5.6 (3.5)], [5.6 (3.5)],
[Excessive sleepiness (ESS>10)], [126 (10.2%)], [120 (10.2%)],
[SWEMWBS], [23.2 (5.0)], [23.2 (5.0)],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 3, text([Values are M (SD) unless noted. ¹ Mdn (IQR). LN = late-night.])),
    ),
    

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Sample Characteristics
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-demographics>


Self-reported sleep duration in the analytical sample was 7.2 hours (SD = 1.1), mean PSQI sleep-quality component scores were 1.3 (SD = 0.6), mean daytime sleepiness was 5.6 on the Epworth Sleepiness Scale (SD = 3.5), and mean wellbeing was 23.2 on the SWEMWBS (SD = 5.0). #ref(<fig-raincloud>, supplement: [Figure]) displays the distributions of gaming patterns and outcomes across the analytical sample.

#figure([
#box(image("manuscript_files/figure-typst/fig-raincloud-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Gaming patterns and outcome distributions. (A) Hourly playtime distribution shows average daily minutes played by hour of day, with grouped bars for weekday (solid) vs weekend (striped) and late-night hours (23:00-06:00) highlighted in red. (B) Sleep Quality shows percentage of responses across ordinal categories. (C-E) Continuous variables displayed as raincloud plots with boxplots (median and IQR) and density distributions.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-raincloud>


Missingness in the adjustment covariates was minimal in the original (pre-imputation) analytical dataset of 1182 participants: BMI was missing for 80 (6.8%), while age, SES index, region, and the weekend/weekday indicator were complete.

== Panel
<panel>
=== H1
<h1>
The preregistered analyses in the Stage 1 protocol @ballou2024psychological specified four multilevel models in which late-night gaming minutes, averaged over 28 days (monthly) or 14 days (biweekly), predicted sleep quality (H1a), sleep duration (H1b), daytime sleepiness (H1c), and wellbeing (H1d), with random intercepts and random slopes for the late-night gaming exposure by participant and an additional random intercept for gender in the linear models. When applying this specification to the Open Play data, the preregistered random-slope structures led to convergence problems and boundary estimates (near-zero variance components), particularly for the cumulative link mixed model. To obtain stable and interpretable estimates we simplified the random-effects structure to random intercepts for participants (and for gender where supported) and used the multiply imputed outcomes as our primary analytic dataset rather than the incomplete original outcomes. Each model was fit separately to each of the 20 imputed datasets, and parameter estimates were pooled using Rubin's rules @rubin1987multiple.

More concretely, H1a replaces the preregistered by-participant random intercept and slope on late-night gaming with a random intercept for participants only. For H1b, attempts to retain the preregistered random intercept for gender led to non-convergence and boundary estimates, so we used a participant random intercept only while keeping the same 28-day late-night average exposure. H1c and H1d follow the same logic of dropping the preregistered by-participant random slopes while preserving a random intercept for gender and the same fixed-effect adjustment set (excluding gender as a fixed effect). As a sensitivity check, we also fit models using the PSQI global score (sum of all seven components, range 0--21) as a continuous alternative to the ordinal item-6 outcome; results are reported in #ref(<tbl-appendix-psqi-global>, supplement: [Table]). A natural cubic spline sensitivity analysis (#ref(<tbl-appendix-h1-spline-aic>, supplement: [Table]); #ref(<fig-appendix-h1-spline>, supplement: [Figure])), evaluated by BIC under the rule of #cite(<jones2001nagin>, form: "prose") (|2ΔBIC| \< 2 = indifferent, 2--6 = positive evidence), returned outcome-specific verdicts rather than a blanket endorsement of linearity: evidence was indifferent between the linear and spline parameterizations for H1a (sleep quality, |2ΔBIC| = 0.6) and H1d (wellbeing, |2ΔBIC| = 0.6); positive but weak evidence favored the linear specification for H1b (sleep duration, |2ΔBIC| = 3.6); and positive but weak evidence favored a non-linear (spline) specification for H1c (daytime sleepiness, |2ΔBIC| = 2.8). We retained the linear parameterization in the pre-registered models for all four outcomes --- for H1a, H1b, and H1d on the basis of the spline check itself, and for H1c on the basis of parsimony and direct comparability with the other H1 models, while flagging the weak evidence for a non-linear shape as a caveat.

Across these models, the late-night gaming term in the sleep-quality model (H1a) is estimated as b = 0.051, 95% CI \[0.023, 0.079\], p \< .001 (probit coefficient per 10 min/day). On the probability scale, each additional 10 minutes of average daily late-night gaming is associated with an approximate 0.9 percentage-point increase in the marginal probability of reporting fairly bad or very bad sleep quality. #ref(<fig-latenight-sleepquality-exceedance>, supplement: [Figure]) translates this coefficient to the probability scale: marginal predicted probabilities of poor sleep quality (Fairly bad or Very bad) were computed for each of the 20 imputed datasets and pooled via Rubin's rules, with 95% confidence intervals derived from the combined within- and between-imputation variance on the probability scale using the delta method. The corresponding effects are b = -0.005, 95% CI \[-0.019, 0.008\], p = 0.456 for sleep duration (H1b), suggesting little systematic association between late-night gaming and self-reported sleep hours, b = 0.013, 95% CI \[-0.034, 0.061\], p = 0.577 for daytime sleepiness (H1c), suggesting negligible variation in Epworth scores as a function of late-night gaming, and b = -0.004, 95% CI \[-0.038, 0.030\], p = 0.805 for wellbeing (H1d), again indicating minimal change in SWEMWBS scores with greater late-night play. All imputed-data estimates are pooled across 20 multiply imputed datasets using Rubin's rules @rubin1987multiple. These models additionally adjust for age, BMI, SES index, region, and weekend versus weekday timing (with gender modelled as a random intercept in H1c and H1d), and they use rolling 14- and 28-day windows of late-night play anchored to each survey date. Full coefficient estimates, confidence intervals, and variance components are reported in the H1 regression summary table, to which we refer for all remaining parameters.

#place(top, scope: "parent", float: true)[
#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "10_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "10_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "7_3": 0, "8_3": 0, "9_3": 0, "10_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "7_4": 0, "8_4": 0, "9_4": 0, "10_4": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "10_0": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 5, stroke: 0.05em + black),
 table.hline(y: 11, start: 0, end: 5, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], [H1a: Sleep Quality], [H1b: Sleep Duration], [H1c: Daytime Sleepiness], [H1d: Wellbeing],
    ),
    // tinytable header end

    // tinytable cell content after
[Daily LN gaming (per 10 min\/day, monthly)], [0.05 \[0.02, 0.08\]\*\*\*], [\-0.01 \[\-0.02, 0.01\]], [0.01 \[\-0.03, 0.06\]], [],
[Daily LN gaming (per 10 min\/day, biweekly)], [], [], [], [\-0.00 \[\-0.04, 0.03\]],
[Age (scaled)], [0.16 \[\-0.17, 0.50\]], [\-0.45 \[\-0.62, \-0.27\]\*\*\*], [\-0.68 \[\-1.27, \-0.09\]\*], [0.07 \[\-0.59, 0.72\]],
[BMI (scaled)], [0.18 \[0.05, 0.31\]\*\*], [\-0.07 \[\-0.14, \-0.00\]\*], [0.18 \[\-0.05, 0.41\]], [\-0.21 \[\-0.48, 0.07\]],
[SES (scaled)], [\-0.24 \[\-0.39, \-0.09\]\*\*], [\-0.09 \[\-0.16, \-0.01\]\*], [0.09 \[\-0.17, 0.34\]], [0.98 \[0.70, 1.26\]\*\*\*],
[Region: US], [\-0.14 \[\-0.37, 0.09\]], [0.04 \[\-0.08, 0.17\]], [0.25 \[\-0.18, 0.69\]], [0.19 \[\-0.29, 0.68\]],
[Day: Weekend], [0.03 \[\-0.16, 0.23\]], [\-0.00 \[\-0.09, 0.09\]], [0.04 \[\-0.25, 0.33\]], [\-0.01 \[\-0.27, 0.25\]],
[N Obs], [3561], [3553], [3551], [7425],
[N Participants], [1294], [1293], [1290], [1469],
[ICC], [0.75], [0.69], [0.69], [0.71],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 5, text([\+ p \< 0.10, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001])),
    table.cell(align: left, colspan: 5, text([LN \= late\-night. Confidence intervals shown in brackets.])),
    table.cell(align: left, colspan: 5, text([ICC \= Intraclass Correlation Coefficient (adjusted).])),
    ),
    

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Summary of H1 Hypotheses: Effects of Late-Night Gaming on Sleep and Wellbeing
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-h1-combined>


]
#figure([
#box(image("manuscript_files/figure-typst/fig-latenight-sleepquality-exceedance-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Marginal predicted probability of poor sleep quality (Fairly bad or Very bad) as a function of late-night gaming. Predictions are derived from the H1a probit cumulative link mixed model fitted separately on each of 20 multiply imputed datasets; predicted probabilities and their within-imputation variances are then pooled via Rubin's rules. The solid line shows the pooled point estimate and the shaded ribbon the 95% confidence interval, both computed on the probability scale using the delta method applied to each imputation's threshold, gaming coefficient, and variance--covariance matrix, then combined with Rubin's within- and between-imputation variance components. Probabilities are population-average (marginalised over the participant random intercept) with other covariates held at their reference or mean values. The top panel shows the marginal density of late-night gaming in the sample; vertical reference lines mark the median, mean, 75th, and 90th percentiles.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-latenight-sleepquality-exceedance>


To complement these pooled coefficient tests with positive evidence of null effects where applicable, we conducted frequentist equivalence tests (TOST; #cite(<lakens2017equivalence>, form: "prose");) on the focal H1 predictors against a Region of Practical Equivalence (ROPE) --- an interval around zero within which a coefficient is treated as too small to matter substantively, derived from ±0.1 × SD(outcome) rescaled to the raw coefficient's native units (see #ref(<sec-appendix-equivalence>, supplement: [Section]) for full derivation and response-scale thresholds). The pooled 90% CIs for H1b, H1c, and H1d all fall entirely within their respective ROPEs, providing positive evidence that any direct effect of late-night gaming on sleep duration, daytime sleepiness, and wellbeing is practically negligible at this benchmark. We note that the ROPE width itself was not preregistered: we adopt Kruschke's #cite(<kruschke2018rejecting>, form: "year") rule-of-thumb ±0.1 × SD(#emph[y];) (rescaled to native units) as a conventional, non-preregistered anchor for what counts as a negligible effect, and the natural-unit translation in the Discussion provides the substantive interpretation. The H1a sleep-quality effect, although statistically non-zero, was Undecided under the stricter ordinal-probit ROPE: the 90% CI lies partly outside the interval, so the estimate cannot be declared either practically equivalent to or reliably larger than zero. Full per-hypothesis TOST decisions, ROPE widths, and pooled 90% CIs are reported in #ref(<tbl-appendix-equivalence>, supplement: [Table]).

=== H2
<h2>
The Stage 1 protocol @ballou2024psychological further specified four moderation models that added a late-night gaming × chronotype (MSF#sub[sc];) interaction to the H1 specifications, predicting the same four outcomes (H2a--H2d); chronotype had a median of 5.9 hours (IQR 3.1) in the analytical sample. In Open Play this structure proved even more demanding than H1: the combination of highly skewed late-night exposure, strong collinearity between chronotype and late-night play, and the ordinal CLMM produced non-convergence and nearly singular variance--covariance matrices, particularly for H2a.

We therefore mean-centred chronotype and applied the same random-effects simplification used in H1 (random intercepts for participants, and for gender where supported, dropping the by-participant random slopes on late-night minutes) while retaining the gaming × chronotype interaction for all four outcomes. H2d already specified a participant random-intercept-only structure in the protocol and so deviates only through the chronotype centring. The ordinal H2a model still exhibits known identifiability issues for CLMMs with interactions, so its interaction term is interpreted cautiously and our substantive conclusions about moderation rely primarily on the linear mixed-effects models (H2b--H2d). Full main, interaction, and random-effect estimates are reported in the H2 regression summary table.

From these models, the main effect of late-night gaming on sleep quality in H2a is b = 0.064, 95% CI \[0.024, 0.105\], p = 0.002 (probit coefficient per 10 min/day), corresponding to an approximate 1.0 percentage-point increase in the probability of reporting fairly bad or worse sleep per additional 10 minutes of daily late-night gaming --- closely mirroring the H1a estimate. None of the preregistered chronotype × late-night gaming interactions reached conventional significance after pooling across imputations, so H2 was not supported for any outcome (H2a sleep quality: b = -0.003, 95% CI \[-0.013, 0.006\], p = 0.487; H2b sleep duration: b = 0.003, 95% CI \[-0.002, 0.008\], p = 0.199; H2c daytime sleepiness: b = -0.009, 95% CI \[-0.024, 0.006\], p = 0.225; H2d wellbeing: b = -0.009, 95% CI \[-0.019, 0.002\], p = 0.101). The equivalence tests reported in #ref(<tbl-appendix-equivalence>, supplement: [Table]) go further and provide positive evidence that these interactions are practically negligible: applying the same TOST procedure described for H1 --- now to the chronotype × late-night gaming coefficients against their interaction-scale ROPEs --- the pooled 90% CIs for H2b, H2c, and H2d all lie entirely within the ROPE, while the H2a interaction is Undecided under the stricter ordinal-probit ROPE. Full model summaries are reported in #ref(<tbl-h2-combined>, supplement: [Table]).

#place(top, scope: "parent", float: true)[
#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "10_1": 0, "11_1": 0, "12_1": 0, "13_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "10_2": 0, "11_2": 0, "12_2": 0, "13_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "7_3": 0, "8_3": 0, "9_3": 0, "10_3": 0, "11_3": 0, "12_3": 0, "13_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "7_4": 0, "8_4": 0, "9_4": 0, "10_4": 0, "11_4": 0, "12_4": 0, "13_4": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "10_0": 1, "11_0": 1, "12_0": 1, "13_0": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 5, stroke: 0.05em + black),
 table.hline(y: 14, start: 0, end: 5, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], [H2a: Sleep Quality], [H2b: Sleep Duration], [H2c: Daytime Sleepiness], [H2d: Wellbeing],
    ),
    // tinytable header end

    // tinytable cell content after
[Daily LN gaming (per 10 min\/day, monthly)], [0.06 \[0.02, 0.11\]\*\*], [\-0.01 \[\-0.03, 0.01\]], [0.02 \[\-0.04, 0.09\]], [],
[Daily LN gaming (per 10 min\/day, biweekly)], [], [], [], [0.04 \[\-0.01, 0.08\]],
[Chronotype (h, centered)], [0.03 \[\-0.02, 0.08\]], [\-0.02 \[\-0.05, 0.01\]], [0.04 \[\-0.04, 0.12\]], [\-0.07 \[\-0.16, 0.03\]],
[LN gaming × Chronotype (h, monthly)], [\-0.00 \[\-0.01, 0.01\]], [0.00 \[\-0.00, 0.01\]], [\-0.01 \[\-0.02, 0.01\]], [],
[LN gaming × Chronotype (h, biweekly)], [], [], [], [\-0.01 \[\-0.02, 0.00\]],
[Age (scaled)], [\-0.06 \[\-0.53, 0.40\]], [\-0.45 \[\-0.69, \-0.21\]\*\*\*], [\-0.93 \[\-1.71, \-0.16\]\*], [0.14 \[\-0.74, 1.01\]],
[BMI (scaled)], [0.06 \[\-0.10, 0.23\]], [\-0.04 \[\-0.13, 0.05\]], [0.12 \[\-0.15, 0.39\]], [\-0.06 \[\-0.41, 0.28\]],
[SES (scaled)], [\-0.28 \[\-0.50, \-0.07\]\*\*], [\-0.09 \[\-0.19, 0.01\]\+], [0.13 \[\-0.20, 0.46\]], [1.08 \[0.71, 1.44\]\*\*\*],
[Region: US], [\-0.17 \[\-0.47, 0.14\]], [\-0.03 \[\-0.19, 0.13\]], [0.38 \[\-0.17, 0.93\]], [0.30 \[\-0.34, 0.93\]],
[Day: Weekend], [0.07 \[\-0.19, 0.32\]], [0.00 \[\-0.11, 0.12\]], [0.04 \[\-0.31, 0.39\]], [\-0.06 \[\-0.38, 0.27\]],
[N Obs], [2580], [2580], [2580], [5160],
[N Participants], [860], [860], [860], [860],
[ICC], [0.76], [0.70], [0.70], [0.70],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 5, text([\+ p \< 0.10, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001])),
    table.cell(align: left, colspan: 5, text([LN \= late\-night. Confidence intervals shown in brackets.])),
    table.cell(align: left, colspan: 5, text([ICC \= Intraclass Correlation Coefficient (adjusted).])),
    ),
    

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Summary of H2 Hypotheses: Chronotype Moderation of Late-Night Gaming Effects
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-h2-combined>


]
== Diary
<diary>
=== H1 and H2
<h1-and-h2>
As an exploratory complement to the panel analyses, we re-used the H1a/H2a probit CLMM specification (random intercept for participant, late-night gaming scaled per 10 min/day, same covariate set) and refit it to daily diary reports of sleep quality against late-night gaming in the preceding 24 hours, rather than the 14- or 28-day averages used in the panel. Sleep quality was recorded on a 5-level ordinal scale (Very poor, Poor, Fair, Good, Very good). To separate within- from between-person variation in late-night gaming, the trimmed exposure (capped at the 99th percentile) was decomposed into a within-person component (daily deviation from the participant's own mean) and a between-person component (participant mean centred at the grand mean). The analytical diary sample comprised 1271 participants contributing 15,842 diary entries with valid sleep quality ratings. Region was dropped because all diary participants are US-based, and a random intercept for gender failed to converge with only three levels, so gender was excluded as well. Continuous covariates (age, BMI, SES) were rescaled within the diary subsample. Missing SES values for participants with unrecognised employment categories (n = 23) were imputed via standard PMM (m = 5) using age, BMI, gender, and region as predictors. These diary analyses were not formally pre-registered.

To handle missing data in the diary outcomes and daily predictors, we applied the same hierarchical two-level `2l.pmm` approach and predictor-matrix coding as the panel imputation, with participants as clusters and diary days nested within (`miceadds`). Level-2 variables for the diary pass were age, BMI, SES, chronotype, gender, and person-mean late-night gaming; level-1 daily predictors with genuine missingness (gaming played, basic psychological needs, stress, day type, late-night gaming hours) were imputed via standard PMM within the same MICE run, and ±5-day lag and lead terms (rather than the ±1-wave terms used in the panel) provided temporal context for each outcome. We generated 60 imputed datasets with 20 iterations each --- more than for the panel because of higher per-day missingness --- inspected QC diagnostics and judged them acceptable, and combined diary regression estimates across imputations using Rubin's rules.

For the direct-effects model (H1), the within-person effect of late-night gaming on sleep quality was b = -0.003, 95% CI \[-0.007, 0.002\], p = 0.214 (probit coefficient per 10 additional minutes). On days when participants gamed more than their own average, the association with sleep quality was not statistically significant. The between-person effect was b = 0.029, 95% CI \[0.012, 0.047\], p = 0.001, indicating that participants who habitually engaged in more late-night gaming tended to report worse sleep quality on average; this between-person effect was statistically significant. On the probability scale, each additional 10 minutes of habitual daily late-night gaming is associated with an approximate 0.6 percentage-point increase in the probability of reporting poor or very poor sleep, broadly consistent with the panel-level H1a estimate (0.9 pp per 10 min; #ref(<fig-diary-sleepquality>, supplement: [Figure]), Panel A). We compared the linear specification against natural cubic spline alternatives (df = 2--6), using BIC as the primary selection criterion and applying the #cite(<jones2001nagin>, form: "prose") rule of thumb on 2|ΔBIC| (consistent with the panel H1 spline sensitivity check in #ref(<tbl-appendix-h1-spline-aic>, supplement: [Table])). The best-fitting spline (df = 2) yielded a 2|ΔBIC| of 15.4 relative to the linear model --- very strong evidence for the linear specification (ΔAIC = 0.1, reported for transparency). We retained the linear specification for direct comparability with the panel H1a/H2a estimates and treat it as a parsimonious summary of the average within- and between-person associations. Diary subsample characteristics are presented in #ref(<tbl-diary-demographics>, supplement: [Table]) in the Appendix.

For the chronotype moderation model (H2), the interaction between within-person late-night gaming and chronotype was b = -0.002, 95% CI \[-0.004, -0.000\], p = 0.024, and the interaction between between-person late-night gaming and chronotype was b = -0.001, 95% CI \[-0.009, 0.007\], p = 0.730. The within-person interaction was statistically significant, indicating that the day-to-day link between late-night gaming and sleep quality was somewhat weaker for evening types. The between-person interaction was not statistically significant, suggesting that the trait-level association between habitual late-night gaming and sleep quality did not meaningfully vary across chronotypes. The main effect of chronotype was b = 0.035, 95% CI \[0.005, 0.065\], p = 0.022; a later chronotype was associated with worse sleep quality (this effect was statistically significant). These diary-based findings are exploratory and should be interpreted with caution (#ref(<fig-diary-sleepquality>, supplement: [Figure]), Panel B).

#figure([
#box(image("manuscript_files/figure-typst/fig-diary-sleepquality-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Predicted probability of poor sleep quality from the diary CLMM. (A) Between-person effect (H1): marginal probability of reporting Poor or Very poor sleep quality as a function of participants' average daily late-night gaming, with the within-person component held at zero. The ribbon shows 95% CIs from the between-person coefficient SE. (B) Within-person x chronotype interaction (H2): marginal probability of poor sleep quality as a function of daily within-person gaming deviation at three chronotype levels (morning = -1 SD, mean, evening = +1 SD), with the between-person component held at zero. Ribbons show 95% CIs via the delta method pooled across 60 imputations using Rubin's rules. Vertical reference lines mark the median (dotted), mean (dashed), 75th percentile (dotted), and 90th percentile (dashed) of the respective predictor distributions. Density plots (top) show predictor distributions.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-diary-sleepquality>


#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "10_1": 0, "11_1": 0, "12_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "10_2": 0, "11_2": 0, "12_2": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "10_0": 1, "11_0": 1, "12_0": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 3, stroke: 0.05em + black),
 table.hline(y: 13, start: 0, end: 3, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 3, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], [H1: Sleep Quality], [H2: Chronotype Moderation],
    ),
    // tinytable header end

    // tinytable cell content after
[LN gaming within\-person (per 10 min)], [\-0.00 \ \[\-0.01, 0.00\]], [0.00 \ \[\-0.00, 0.01\]],
[LN gaming between\-person (per 10 min)], [0.03\*\* \ \[0.01, 0.05\]], [0.02 \ \[\-0.01, 0.05\]],
[Chronotype (h, centered)], [], [0.04\* \ \[0.00, 0.07\]],
[LN within × Chronotype], [], [\-0.00\* \ \[\-0.00, \-0.00\]],
[LN between × Chronotype], [], [\-0.00 \ \[\-0.01, 0.01\]],
[Age (scaled)], [0.01 \ \[\-0.06, 0.07\]], [\-0.06 \ \[\-0.16, 0.04\]],
[BMI (scaled)], [0.07\* \ \[0.01, 0.14\]], [0.08 \ \[\-0.02, 0.18\]],
[SES (scaled)], [\-0.13\*\*\* \ \[\-0.20, \-0.07\]], [\-0.17\*\* \ \[\-0.28, \-0.07\]],
[Day: Weekend], [\-0.29\*\*\* \ \[\-0.33, \-0.24\]], [\-0.28\*\*\* \ \[\-0.34, \-0.22\]],
[N Obs], [14690], [8399],
[N Participants], [1132], [509],
[ICC], [0.48], [0.53],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 3, text([\+ p \< 0.10, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001])),
    table.cell(align: left, colspan: 3, text([LN \= late\-night. Confidence intervals shown in brackets below each estimate.])),
    table.cell(align: left, colspan: 3, text([Estimates pooled across 60 multiply imputed datasets using Rubin's rules.])),
    table.cell(align: left, colspan: 3, text([Cumulative link mixed models (random intercept for participant) on 5\-level ordinal sleep quality (positive coefficients \= higher probability of worse sleep). Both H1 and H2 use probit link. Late\-night gaming expressed per 10 minutes; chronotype in centered hours; age, BMI, SES scaled within the diary subsample. Region excluded: all diary participants are US\-only.])),
    table.cell(align: left, colspan: 3, text([Within \= daily deviation from person mean; Between \= person mean \- grand mean.])),
    ),
    

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Diary CLMM Regression Results: H1 (Direct Effects) and H2 (Chronotype Moderation) for Sleep Quality
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-diary-h1h2>


= Discussion
<discussion>
This preregistered study examined whether late-night gaming is associated with poorer sleep quality, shorter sleep duration, greater daytime sleepiness, and lower wellbeing among adult gamers, and whether chronotype moderates these associations. Using objective telemetry linked to repeated self-report surveys, we found a consistent but small association between late-night gaming and sleep quality --- and little evidence that late-night gaming affects sleep duration, daytime sleepiness, or wellbeing directly. Under the same, pooled multiple-imputation analysis, chronotype did not meaningfully moderate any of the sleep- or wellbeing-related associations.

Of the four preregistered direct-effect hypotheses (H1a--H1d), only the association between late-night gaming and sleep quality was supported (H1a). Across the panel models (#ref(<tbl-h1-combined>, supplement: [Table])), each additional 10 minutes of average daily late-night gaming was associated with an approximate 0.9 percentage-point increase in the marginal probability of reporting fairly bad or very bad sleep quality. This estimate was consistent across analytic specifications. Expressed on a common probability scale, the complete-case panel model (#ref(<tbl-appendix-h1-completecase>, supplement: [Table])) produced a closely matching 1.0 percentage-point increase per 10 additional minutes, and the exploratory diary between-person component (#ref(<tbl-diary-h1h2>, supplement: [Table])) --- capturing stable individual differences in habitual late-night gaming --- gave a comparable 0.6 percentage-point increase in the probability of reporting poor or very poor daily sleep. A sensitivity analysis substituting the continuous PSQI global score (#ref(<tbl-appendix-psqi-global>, supplement: [Table])) yielded directionally consistent positive fixed effects of late-night gaming on total PSQI points (imputed: b = 0.089, 95% CI \[0.055, 0.123\], p \< .001; complete-case: b = 0.036, 95% CI \[0.002, 0.070\]), where the coefficient denotes the change in PSQI global score per additional 10 late-night minutes per day. Translated to the probability scale, these correspond to an approximate 1.0 (imputed) / 0.4 (complete-case) percentage-point increase in the probability of exceeding the PSQI poor-sleeper cutoff (\> 5) at the sample mean. Linearity checks for the primary H1a specification, comparing natural-spline alternatives to the linear form on the gaming-exposure term, produced similar slopes at representative exposure levels (#ref(<tbl-appendix-h1-spline-aic>, supplement: [Table]); #ref(<fig-appendix-h1-spline>, supplement: [Figure])), supporting the preregistered linear parameterisation. The convergence of these findings across different operationalisations of sleep quality, analytic strategies, and study designs strengthens our confidence that the association, though small, is genuine. Our observational design does not permit causal inference: although the association is robust across specifications, we cannot determine whether late-night gaming itself degrades sleep quality, whether poor sleepers are drawn to late-night gaming, or whether a third, uncontrolled factor drives both.

While remaining direct-effect hypotheses were not supported, these are not merely underpowered nulls: frequentist equivalence tests (TOST; #cite(<lakens2017equivalence>, form: "prose");) allow us to actively accept the null hypothesis of a negligible effect. For all three non-significant direct effects (H1b--H1d), the 90% confidence intervals fell entirely within the region of practical equivalence (±0.1 × SD(#emph[y];); see #ref(<tbl-appendix-equivalence>, supplement: [Table])), providing positive evidence that any true effects of late-night gaming in this sample are too small to be of practical consequence. In substantive terms, we can rule out effects larger than roughly ±7 minutes of nightly sleep per one-SD increase in late-night gaming (H1b) --- far below the amount that would plausibly affect next-day functioning; ±0.38 points on the 0--24 Epworth Sleepiness Scale (H1c) --- much smaller than the \~5-point gap separating normal from clinically excessive daytime sleepiness @johns1991new; and ±0.53 points on the SWEMWBS wellbeing scale (H1d) --- well under the \~1 to 3-point change regarded as clinically meaningful @maheswaran2012evaluating. In other words, even the largest effects compatible with our data would be undetectable against the everyday variability in sleep, mood, and alertness. These findings speak to the sleep displacement hypothesis, which posits that late-night gaming displaces sleep time, thereby shortening sleep duration and producing daytime sleepiness @twenge2019more@lemola2011habitual@exelmans2015sleep. The core prediction of that account --- reduced sleep duration --- is directly contradicted by our equivalence-confirmed null for H1b, and the absence of any detectable effect on daytime sleepiness (H1c) removes a key downstream consequence that would be expected if meaningful displacement were occurring. Taken together, the pattern of results argues against a displacement explanation: sleep duration and alertness were preserved, while subjective sleep quality alone was modestly worse. The quality effect is more plausibly attributed to physiological arousal or pre-sleep cognitive stimulation from gaming @king2013impact, or to an attribution mechanism whereby players who game late at night perceive their sleep as poorer regardless of whether its duration or architecture is objectively altered. We cannot exclude the possibility that gamers engage in compensatory behaviour --- shifting rather than shortening their sleep window --- which would preserve duration and alertness while leaving perceptions of sleep quality degraded.

The null finding for wellbeing (H1d) sits within a growing body of trace-data evidence that objectively logged gameplay is at most weakly related to mental wellbeing. A narrative review of 13 proposed mechanisms argues that gaming operates through offsetting pathways --- relaxation, need satisfaction, and social connection on one side; displacement of sleep, exercise, and social activity, plus pre-sleep arousal on the other --- making a small or null net association with global wellbeing the expected result @ballou2024mechanisms. The empirical record agrees: telemetry from \~40,000 players across seven games found only a trivial playtime--wellbeing link @johannes2021video; a six-week longitudinal extension reached an equivalence-style conclusion that within-person changes in playtime are unlikely to move wellbeing meaningfully @vuorre2022time; and a telemetry study of casual adult Nintendo players replicated the null across windows from a single hour to a full year, with perceived value of play --- not hours played --- predicting wellbeing @ballou2025perceived. The short-window result is particularly relevant here, since late-night gaming is itself a narrow time-of-day exposure whose hedonic and psychological benefits appear to offset any modest cost routed through perceived sleep quality.

== Chronotype Moderation
<chronotype-moderation>
The preregistered moderation hypotheses (H2) predicted that evening chronotype would amplify the negative associations between late-night gaming and all four outcomes. These predictions were grounded in theories of circadian misalignment and social jetlag, which hold that evening chronotypes are especially affected by late-night stimulation because it exacerbates the existing misalignment between their endogenous sleep--wake rhythm and socially imposed schedules @zhong2025electronic@kortesoja2023latenight. None of the preregistered moderation hypotheses were supported under the pooled multiple-imputation analysis. The chronotype × late-night gaming interaction was non-significant for sleep quality (H2a: b = -0.003, 95% CI \[-0.013, 0.006\], p = 0.487), sleep duration (H2b: b = 0.003, 95% CI \[-0.002, 0.008\], p = 0.199), daytime sleepiness (H2c: b = -0.009, 95% CI \[-0.024, 0.006\], p = 0.225), and wellbeing (H2d: b = -0.009, 95% CI \[-0.019, 0.002\], p = 0.101). Frequentist equivalence tests with a ROPE rescaled to the coefficient's native units (±0.1 × SD(#emph[y];) / SD(#emph[x];); see #ref(<tbl-appendix-equivalence>, supplement: [Table])) supported practical equivalence for the H2b, H2c, and H2d interactions. The H2a interaction was undecided: the 90% CI was narrow but extended marginally beyond the lower bound of the latent-scale ROPE. The exploratory diary data told a similar story at the trait level: the between-person chronotype × late-night gaming interaction was small and non-significant (b = -0.001, 95% CI \[-0.009, 0.007\], p = 0.730), indicating that the habitual association between late-night gaming and nightly sleep quality did not meaningfully vary across chronotypes. The within-person interaction did reach conventional significance (b = -0.002, 95% CI \[-0.004, -0.000\], p = 0.024), suggesting that on days when evening-type participants gamed more than usual, the day-to-day link to sleep quality was paradoxically somewhat #emph[weaker] rather than stronger --- a finding that runs against the preregistered prediction and that we treat as exploratory. Notably, the diary model did show a significant #emph[main effect] of chronotype on nightly sleep quality (b = 0.035, 95% CI \[0.005, 0.065\], p = 0.022), with later chronotypes reporting worse sleep on average. This pattern where there is trait-level chronotype penalty for sleep quality, but no amplification of the late-night-gaming effect by chronotype, is consistent with the broader circadian-misalignment literature without supporting the more specific claim that evening types are differentially harmed by late-night play.

The largely null moderation results have several possible explanations. Evening-type gamers may have developed coping mechanisms or adapted sleep routines that buffer them against the effects of late-night play --- for instance, by habitually sleeping in later or napping to compensate. More likely, the circadian misalignment pathway may simply be weaker than previously assumed in adult populations who have more autonomy over their schedules than the adolescent samples on which much of the chronotype--technology literature is based @bruni2015technology@reardon2023adolescent.

== Contextualising the Effect Sizes
<contextualising-the-effect-sizes>
=== At the individual night level
<at-the-individual-night-level>
Viewed at the per-person, per-night scale, the association is small in absolute terms. At the marginal rate of 0.9 percentage points per 10 minutes, even a gamer averaging an hour of play past 11pm would see only a roughly 5 percentage-point increase in the probability of reporting poor sleep quality. Against the backdrop of 64% of participants already qualifying as poor sleepers by PSQI criteria, late-night gaming appears to be a minor per-person contributor to an issue with broader origins --- including work schedules, general screen use, caffeine consumption, and other lifestyle factors not captured here.

The descriptive patterns in our sample reinforce this reading. While sleep duration remained relatively preserved (7.2 hours on average), 64% of participants were classified as poor sleepers on the PSQI. This dissociation between duration and quality further undermines a displacement account --- if gaming were simply pushing sleep later and shortening it, elevated poor-sleeper prevalence would be expected to co-occur with shorter mean duration, yet only the former is apparent. The pattern is more consistent with arousal or attentional mechanisms that degrade perceived sleep quality without substantially reducing its length @king2013impact@weaver2010effect. It is useful to anchor the implied per-hour magnitude against other modifiable lifestyle influences on adult sleep. At roughly 5 percentage points per hour, the sleep-quality cost of late-night gaming is in the same broad range as the perceived sleep-quality cost reported for a high (400 mg) dose of caffeine consumed within four hours of bedtime @gardiner2025dose. This comparison is most directly interpretable for our exploratory diary substudy, which used a structurally similar single-item nightly sleep-quality rating; there, the between-person estimate (0.6 percentage points per 10 min of habitual late-night gaming) aligned closely with the panel PSQI result. A comparable benchmark on the continuous PSQI scale comes from #cite(<yan2024chronobiological>, form: "prose");, who report a ≈0.39-point higher PSQI total score for adults in the latest vs earliest tertile of last-meal time (a ≈3-hour shift). For a contrast of similar practical magnitude in our data --- moving from \~10 min/day to \~3 h/day of late-night gaming --- the panel PSQI sensitivity model implies a ≈0.26-point increase, of comparable order but somewhat smaller, suggesting that the per-person sleep-quality penalty of habitual late-night gaming sits below that of habitually late evening meal timing.

That we found this effect specifically for sleep quality --- rather than duration, sleepiness, or wellbeing --- suggests the pathway may operate through subjective perception of sleep disturbance rather than through the displacement or arousal mechanisms traditionally emphasised in the literature. It is possible that gamers who play late at night notice poorer sleep quality because they attribute restlessness or difficulty falling asleep to their recent gaming session. Because our sleep measures are all self-reported, we cannot distinguish such an attributional process from a genuine change in sleep architecture; wearable-based ambulatory measurement would be needed to adjudicate.

=== At the population and cumulative level
<at-the-population-and-cumulative-level>
A small per-night effect can nonetheless carry weight when it is sustained over time and aggregated across a common behaviour. Restricting sleep by only about an hour can still disturb emotion regulation @tomaso2021effect and undermine cognitive and behavioural performance @belenky2003patterns; if that restriction is maintained for two weeks, losses in alertness and working memory can rival those seen after an entire night without sleep @vandongen2003cumulative. Repeated small shifts in nightly sleep quality from habitual late-night play may therefore matter for mood, vigilance, and cognition over weeks and months, even if any single night looks unremarkable. Benchmarks tailored to sleep outcomes support this interpretation: Panjeh and colleagues' recalibration from 72 effect sizes in 65 sleep-quality intervention trials @panjeh2023establishing maps "small", "medium", and "large" to Cohen's #emph[d] of roughly 0.18, 0.33, and 0.56 --- below the usual 0.2 / 0.5 / 0.8 anchors. Judged against those sleep-specific standards, the cumulative impact of 1--2 hours of late-night gaming falls in the small-to-medium band, whereas the same pattern might look trivial-to-small on the generic Cohen scale. Population burden adds another layer: De Rosa and colleagues #cite(<derosa2024videogaming>, form: "year") note that video gaming is now one of the most widespread adult leisure exposures and that sleep outcomes track how intensively and how often people play, so a shallow per-night gradient can still shift the upper tail of poor sleep when layered onto a common behaviour. Public-health importance depends on both per-person effect magnitude and how prevalent the behaviour is --- and on this combined view, our findings sit alongside a broader literature reporting statistically detectable but per-person modest associations between digital media use and psychological outcomes.

=== Why timing-specific measurement matters
<why-timing-specific-measurement-matters>
A key gap in the existing literature is the near-complete absence of timing-specific measurement: #cite(<kristensen2021problematic>, form: "prose") noted that none of the studies in their systematic review registered the time of day gaming took place. Most adult gaming-and-sleep studies consequently operationalise exposure as aggregate daily volume or as binary/categorical problematic-gaming contrasts, which are likely to dilute any signal concentrated in the pre-sleep window. Our telemetry-based, continuous, time-of-day-specific operationalisation of late-night play is deliberately designed to isolate exactly that window, and the fact that we find a small but robust association only for sleep quality --- and only under this timing-sensitive measure --- is consistent with reviews identifying pre-sleep timing, arousal, and session duration as key moderators of gaming's effect on sleep @derosa2024videogaming. That the effect is detectable at all under a stringent, preregistered, multiply-imputed longitudinal specification suggests that timing-specific exposures capture signal that aggregate-volume measures may miss.

== Measurement and Conceptual Considerations
<measurement-and-conceptual-considerations>
Our modest effect estimates sit within a broader literature marked by genuine measurement and conceptual heterogeneity, and several features of that literature help frame how our findings should --- and should not --- be read. First, much of the foundational work on problematic and late-night gaming has been conducted in youth and adolescent samples, where sleep physiology, circadian timing, and day-to-day autonomy differ substantially from the early-adult population we studied @king2013impact@hale2018youth. Diagnostic criteria and assessment instruments for gaming disorders in adults are still evolving, and consensus on how to operationalise problematic use in populations with greater scheduling autonomy has not yet been reached @costa2019current@mannikko2020problematic. This developmental gap complicates direct comparisons between our estimates and those drawn from adolescent-focused studies: the same nominal exposure may carry different implications when players control their own bedtimes, commutes, and work schedules.

Second, the literature consistently reports substantial heterogeneity across study designs, populations, and gaming contexts --- including game content, device, session duration, and time-of-day --- and these contextual factors appear to moderate the magnitude of sleep effects @smith2017mechanisms@hale2018youth. Proposed mechanisms span physiological arousal, evening light exposure, and disruption of sleep routines, with individual differences in baseline sleep quality and chronotype shaping susceptibility @hartmann2019effects. Our telemetry-based exposure isolates late-night console and PC play but cannot easily distinguish arousal-inducing content from more routine gameplay, nor can it index the ambient light or pre-sleep routines that likely co-vary with late-night play. The small average effect we observe for sleep quality is therefore best read as a population-level average across a heterogeneous mix of contexts, some of which may carry larger effects than others.

Third, the effect-size estimates reported across this literature are generally small to modest and variable across individuals, and are sometimes not clinically meaningful at the person level @hartmann2019effects@smith2017mechanisms. Several reviews also highlight that gaming is entangled with broader patterns of nocturnal screen engagement and sedentary behaviour, with late-night screen use associated with downstream sleep and health outcomes that are not specific to any single platform or content type @dresp2022digital@gale2024late@ramirez2021use. Our finding of a small but robust association for sleep quality is consistent with this picture: late-night gaming is plausibly one contributor to a wider bundle of pre-sleep screen behaviours, and isolating its unique contribution is inherently constrained by measurement instruments that capture only part of that bundle.

Additionally, four constraints bound the scope of our findings. First, our telemetry captures console and PC play but excludes mobile platforms, which account for a substantial share of late-night leisure --- particularly in-bed use that bypasses "household curfews" and serves as a pre-sleep emotional-regulation tool. Late-night minutes that shifted to phones therefore go unmeasured, likely attenuating observed associations, and we cannot separate gaming-specific effects from the broader bundle of nocturnal screen behaviours (short-form video, messaging, doomscrolling) that share the same devices. Second, even within the platforms we do observe, session-level logs cannot distinguish active engagement from idle time, an ambiguity that is especially acute at night when consoles may stay on while players drift off or step away. This can inflate exposure and produce spurious late-night "activity" that does not reflect cognitive arousal.

Third, the analytic sample is restricted to early adults recruited from UK- and US-based Prolific panels. Younger players --- the focus of most policy debate --- have different sleep physiology, school schedules, parental oversight, and motivational profiles, and our estimates should not be extrapolated to pediatric or older populations. Prolific panels themselves are self-selected online workers with reliable broadband, high digital literacy, and tolerance for repeated surveys; eligibility filters around residency, language, and compliance further exclude players in other linguistic and regulatory contexts, as well as shift workers, caregivers, lower-income players, and those engaged in more extreme or stigmatized gaming profiles. Taken together, these constraints mean our findings speak most directly to digitally engaged young adults in the UK and US who play on major console and PC ecosystems; extension to other regions, life stages, device ecologies should await complementary data sources.

== Future Directions
<future-directions>
Several lines of follow-up work are suggested by our findings. First, our results reinforce a methodological lesson that is increasingly clear in the adult gaming-and-sleep literature: timing and context matter more than aggregate volume. A recent systematic review of 26 adult studies concluded that excessive gaming impairs sleep quality and delays sleep timing whereas habitual or casual gaming does not, and specifically identified arousal level, session duration, and pre-sleep timing as the key moderators of gaming's effect on sleep @derosa2024videogaming. Our observation that late-night gaming shows a small but robust association with sleep quality --- while aggregate gaming does not feature as the exposure of interest at all --- is directly consistent with this emphasis. Future work would benefit from combining telemetry with finer-grained metadata on game content, arousal level, and session proximity to bedtime, and from extending logging to mobile platforms so that the full pre-sleep screen bundle can be measured rather than inferred. Linking telemetry to ambulatory physiological measurements (e.g., consumer-grade actigraphy, heart-rate variability, or ambient light sensing) would additionally allow researchers to separate subjective perception of sleep disturbance from objective architecture, which our self-report instruments could not disentangle.

Second, the observational design of this study limits the strength of causal claims that can be drawn from any single estimate. A natural next step is to embed telemetry-based exposures within a target trial emulation framework @hernan2016using@hernan2022target, which makes the hypothetical randomized experiment explicit --- defining eligibility, exposure contrasts (e.g., a late-night play session versus none on a given evening), assignment procedures, follow-up windows, and causal estimands before estimation. Target trial emulation is particularly well suited to device-use research because the relevant question is rarely "do gamers sleep worse than non-gamers" but rather "does the same person sleep worse on nights they play late than on nights they do not." High-frequency telemetry naturally supports such within-person contrasts, and pairing it with g-methods or sequential exchangeability assumptions would allow future work to move from associational gradients toward more interpretable, policy-relevant causal estimates while keeping each individual as their own control.

The telemetry--survey linkage protocol piloted in Open Play is, in principle, portable: with appropriate ethical safeguards, assent procedures, and clinical validation, an analogous continuous-telemetry approach could be used to study adolescents and problematic gamers, populations in which the timing-specific effects of late-night play are most hotly contested and where aggregate self-report measures are especially vulnerable to recall bias. Extending the present methodology to those populations --- rather than extrapolating our estimates to them --- is, in our view, the most productive next step.

= References
<references>
#block[
] <refs>
#show: appendix.with()

// Override the extension's "A<h>.<n>" figure numbering to a flat "A<n>"
// so appendix figures/tables read as Fig A1, Fig A2, Table A1, ...
// Reset counters to zero so numbering starts at A1 regardless of main-text count.
#counter(figure.where(kind: "quarto-float-fig")).update(0)
#counter(figure.where(kind: "quarto-float-tbl")).update(0)
#counter(figure.where(kind: image)).update(0)
#counter(figure.where(kind: table)).update(0)

#set figure(numbering: n => "A" + str(n))
= Appendix
<appendix>
== Panel H1 Sensitivity Analysis: Complete-Case
<panel-h1-sensitivity-analysis-complete-case>
#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "10_1": 0, "11_1": 0, "12_1": 0, "13_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "10_2": 0, "11_2": 0, "12_2": 0, "13_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "7_3": 0, "8_3": 0, "9_3": 0, "10_3": 0, "11_3": 0, "12_3": 0, "13_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "7_4": 0, "8_4": 0, "9_4": 0, "10_4": 0, "11_4": 0, "12_4": 0, "13_4": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "10_0": 1, "11_0": 1, "12_0": 1, "13_0": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 5, stroke: 0.05em + black),
 table.hline(y: 14, start: 0, end: 5, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], [H1a: Sleep Quality], [H1b: Sleep Duration], [H1c: Daytime Sleepiness], [H1d: Wellbeing],
    ),
    // tinytable header end

    // tinytable cell content after
[Daily LN gaming (per 10 min\/day, monthly)], [0.06 \[0.03, 0.08\]\*\*\*], [\-0.00 \[\-0.02, 0.01\]], [0.02 \[\-0.02, 0.06\]], [],
[Daily LN gaming (per 10 min\/day, biweekly)], [], [], [], [\-0.02 \[\-0.05, 0.01\]],
[Age (scaled)], [0.24 \[\-0.12, 0.60\]], [\-0.45 \[\-0.62, \-0.28\]\*\*\*], [\-0.80 \[\-1.36, \-0.24\]\*\*], [0.07 \[\-0.58, 0.71\]],
[BMI (scaled)], [0.23 \[0.08, 0.39\]\*\*], [\-0.09 \[\-0.16, \-0.01\]\*], [0.24 \[\-0.00, 0.48\]\+], [\-0.20 \[\-0.48, 0.07\]],
[SES (scaled)], [\-0.26 \[\-0.42, \-0.11\]\*\*\*], [\-0.10 \[\-0.17, \-0.03\]\*\*], [0.13 \[\-0.11, 0.38\]], [0.94 \[0.67, 1.21\]\*\*\*],
[Region: US], [\-0.23 \[\-0.50, 0.05\]], [0.07 \[\-0.06, 0.20\]], [0.29 \[\-0.13, 0.72\]], [0.17 \[\-0.31, 0.66\]],
[Day: Weekend], [0.04 \[\-0.16, 0.24\]], [0.02 \[\-0.08, 0.11\]], [0.00 \[\-0.30, 0.31\]], [\-0.01 \[\-0.27, 0.24\]],
[SD (Intercept | Participant)], [2.01], [0.96], [3.12], [4.23],
[SD (Residual)], [], [0.66], [2.16], [2.81],
[SD (Intercept | Gender)], [], [], [0.46], [1.20],
[N Obs], [2482], [2482], [2482], [5704],
[N Participants], [1102], [1102], [1102], [1469],
[ICC], [0.80], [0.68], [0.68], [0.71],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 5, text([\+ p \< 0.10, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001])),
    table.cell(align: left, colspan: 5, text([LN \= late\-night. Confidence intervals shown in brackets.])),
    table.cell(align: left, colspan: 5, text([ICC \= Intraclass Correlation Coefficient (adjusted).])),
    ),
    

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Panel H1 Effects of Late-Night Gaming --- Complete-Case (Non-Imputed Data)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-appendix-h1-completecase>


#pagebreak()
== Panel H2 Sensitivity Analysis: Complete-Case
<panel-h2-sensitivity-analysis-complete-case>
#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "10_1": 0, "11_1": 0, "12_1": 0, "13_1": 0, "14_1": 0, "15_1": 0, "16_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "10_2": 0, "11_2": 0, "12_2": 0, "13_2": 0, "14_2": 0, "15_2": 0, "16_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "7_3": 0, "8_3": 0, "9_3": 0, "10_3": 0, "11_3": 0, "12_3": 0, "13_3": 0, "14_3": 0, "15_3": 0, "16_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "7_4": 0, "8_4": 0, "9_4": 0, "10_4": 0, "11_4": 0, "12_4": 0, "13_4": 0, "14_4": 0, "15_4": 0, "16_4": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "10_0": 1, "11_0": 1, "12_0": 1, "13_0": 1, "14_0": 1, "15_0": 1, "16_0": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 5, stroke: 0.05em + black),
 table.hline(y: 17, start: 0, end: 5, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], [H2a: Sleep Quality], [H2b: Sleep Duration], [H2c: Daytime Sleepiness], [H2d: Wellbeing],
    ),
    // tinytable header end

    // tinytable cell content after
[Daily LN gaming (per 10 min\/day, monthly)], [0.07 \[0.02, 0.12\]\*\*], [\-0.02 \[\-0.04, 0.01\]], [0.04 \[\-0.02, 0.10\]], [],
[Daily LN gaming (per 10 min\/day, biweekly)], [], [], [], [0.03 \[\-0.01, 0.07\]],
[Chronotype (h, centered)], [0.02 \[\-0.04, 0.08\]], [\-0.02 \[\-0.05, 0.01\]], [0.02 \[\-0.07, 0.11\]], [\-0.05 \[\-0.15, 0.04\]],
[LN gaming × Chronotype (h, monthly)], [\-0.01 \[\-0.02, 0.00\]], [0.00 \[\-0.00, 0.01\]\+], [\-0.01 \[\-0.03, 0.00\]], [],
[LN gaming × Chronotype (h, biweekly)], [], [], [], [\-0.01 \[\-0.02, \-0.00\]\*],
[Age (scaled)], [0.01 \[\-0.44, 0.46\]], [\-0.43 \[\-0.67, \-0.19\]\*\*\*], [\-1.18 \[\-1.90, \-0.46\]\*\*], [0.16 \[\-0.69, 1.01\]],
[BMI (scaled)], [0.04 \[\-0.15, 0.24\]], [\-0.06 \[\-0.15, 0.04\]], [0.19 \[\-0.09, 0.48\]], [\-0.04 \[\-0.37, 0.30\]],
[SES (scaled)], [\-0.11 \[\-0.32, 0.09\]], [\-0.11 \[\-0.21, \-0.01\]\*], [0.22 \[\-0.08, 0.52\]], [1.05 \[0.71, 1.40\]\*\*\*],
[Region: US], [\-0.16 \[\-0.52, 0.19\]], [\-0.02 \[\-0.20, 0.16\]], [0.51 \[\-0.02, 1.05\]\+], [0.26 \[\-0.36, 0.88\]],
[Day: Weekend], [\-0.01 \[\-0.27, 0.25\]], [0.05 \[\-0.08, 0.17\]], [\-0.02 \[\-0.39, 0.35\]], [\-0.08 \[\-0.40, 0.24\]],
[SD (Intercept | Participant)], [4.51], [1.02], [3.03], [4.12],
[SD (Intercept | Gender)], [], [0.04], [0.33], [0.85],
[SD (Residual)], [], [0.68], [2.03], [2.79],
[N Obs], [1520], [1520], [1520], [3462],
[N Participants], [673], [673], [673], [860],
[ICC], [0.95], [0.69], [0.69], [0.69],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 5, text([\+ p \< 0.10, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001])),
    table.cell(align: left, colspan: 5, text([LN \= late\-night. Confidence intervals shown in brackets.])),
    table.cell(align: left, colspan: 5, text([ICC \= Intraclass Correlation Coefficient (adjusted).])),
    ),
    

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Panel H2 Chronotype Moderation --- Complete-Case (Non-Imputed Data)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-appendix-h2-completecase>


#pagebreak()
== Panel: Wave-Level Missingness
<panel-wave-level-missingness>
We summarised the extent of missingness for the key self-report outcomes across each survey wave in the raw data before imputation. The `Observations` column reports the number of participants in a wave who completed at least one of the listed measures; the percentages in each row are calculated relative to that wave-specific participant count.

#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
  )

  #let style-array = ( 
    // tinytable cell style after
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 6, stroke: 0.05em + black),
 table.hline(y: 7, start: 0, end: 6, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 6, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Wave], [Observations], [Sleep quality (PSQI item 6)], [Sleep duration (hours)], [Daytime sleepiness (ESS)], [Wellbeing (SWEMWBS)],
    ),
    // tinytable header end

    // tinytable cell content after
[1], [1578], [], [], [], [1 (0.1%)],
[2], [1578], [458 (29.0%)], [471 (29.8%)], [472 (29.9%)], [455 (28.8%)],
[3], [1578], [], [], [], [530 (33.6%)],
[4], [1578], [663 (42.0%)], [674 (42.7%)], [669 (42.4%)], [661 (41.9%)],
[5], [1578], [], [], [], [740 (46.9%)],
[6], [1578], [876 (55.5%)], [881 (55.8%)], [888 (56.3%)], [872 (55.3%)],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Wave-level missingness for key self-report measures (pre-imputation data)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-appendix-wave-missingness>


#pagebreak()
== Panel: PSQI Global Score Sensitivity Analysis
<panel-psqi-global-score-sensitivity-analysis>
This section presents a sensitivity analysis using the PSQI global score as an alternative sleep quality outcome. The PSQI global score is the sum of all 7 PSQI component scores (range 0-21, higher = worse sleep quality), providing a continuous measure compared to the ordinal PSQI item 6 outcome used in the pre-registered H1a hypothesis.

#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_0": 0, "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "10_1": 0, "11_1": 0, "12_1": 0, "13_1": 0, "14_1": 0, "15_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "10_2": 0, "11_2": 0, "12_2": 0, "13_2": 0, "14_2": 0, "15_2": 0, "0_3": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "7_3": 0, "8_3": 0, "9_3": 0, "10_3": 0, "11_3": 0, "12_3": 0, "13_3": 0, "14_3": 0, "15_3": 0, "0_4": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "7_4": 0, "8_4": 0, "9_4": 0, "10_4": 0, "11_4": 0, "12_4": 0, "13_4": 0, "14_4": 0, "15_4": 0, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "10_0": 1, "11_0": 1, "12_0": 1, "13_0": 1, "14_0": 1, "15_0": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    column-gutter: 5pt,
    columns: (auto, auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 1, end: 5, stroke: 0.05em + black),
 table.hline(y: 2, start: 0, end: 5, stroke: 0.05em + black),
 table.hline(y: 16, start: 0, end: 5, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], table.cell(colspan: 4, align: center)[PSQI Global],
[ ], [Playtime (Imputed)], [Chronotype × Playtime (Imputed)], [Playtime (Complete\-Case)], [Chronotype × Playtime (Complete\-Case)],
    ),
    // tinytable header end

    // tinytable cell content after
[Daily LN gaming (per 10 min\/day, monthly)], [0.09 \[0.05, 0.12\]\*\*\*], [0.09 \[0.04, 0.15\]\*\*\*], [0.04 \[0.00, 0.07\]\*], [0.04 \[\-0.00, 0.09\]\+],
[Age (scaled)], [1.44 \[1.04, 1.83\]\*\*\*], [1.16 \[0.63, 1.69\]\*\*\*], [0.51 \[0.07, 0.95\]\*], [0.15 \[\-0.44, 0.74\]],
[BMI (scaled)], [0.14 \[\-0.03, 0.31\]], [0.01 \[\-0.20, 0.22\]], [0.31 \[0.13, 0.50\]\*\*], [0.14 \[\-0.09, 0.37\]],
[SES (scaled)], [\-0.18 \[\-0.34, \-0.01\]\*], [\-0.24 \[\-0.46, \-0.02\]\*], [\-0.41 \[\-0.60, \-0.22\]\*\*\*], [\-0.39 \[\-0.64, \-0.15\]\*\*],
[Chronotype (h, centered)], [], [0.01 \[\-0.05, 0.07\]], [], [0.10 \[0.03, 0.18\]\*\*],
[LN gaming × Chronotype (h, monthly)], [], [0.00 \[\-0.01, 0.01\]], [], [\-0.01 \[\-0.02, 0.00\]],
[Region: US], [\-0.20 \[\-0.49, 0.10\]], [\-0.17 \[\-0.55, 0.22\]], [\-0.38 \[\-0.71, \-0.04\]\*], [\-0.42 \[\-0.86, 0.02\]\+],
[Day: Weekend], [0.23 \[\-0.03, 0.48\]\+], [0.24 \[\-0.09, 0.56\]], [\-0.23 \[\-0.46, 0.00\]\+], [\-0.19 \[\-0.48, 0.10\]],
[SD (Residual)], [], [], [1.60], [1.55],
[SD (Intercept | Participant)], [], [], [2.47], [2.52],
[SD (Intercept | Gender)], [], [], [0.71], [0.55],
[N Obs], [4410], [2580], [2482], [1520],
[N Participants], [1470], [860], [1102], [673],
[ICC], [0.51], [0.51], [0.72], [0.73],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 5, text([\+ p \< 0.10, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001])),
    table.cell(align: left, colspan: 5, text([PSQI global score range: 0\-21 (higher \= worse sleep quality)])),
    table.cell(align: left, colspan: 5, text([Confidence intervals shown in brackets.])),
    table.cell(align: left, colspan: 5, text([ICC \= Intraclass Correlation Coefficient (adjusted).])),
    ),
    

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Sensitivity Analysis: PSQI Global Score Models (Imputed vs.~Original)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-appendix-psqi-global>


#pagebreak()
== Diary: Daily Diary Subsample
<diary-daily-diary-subsample>
#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_0": 0, "2_0": 0, "3_0": 0, "5_0": 0, "6_0": 0, "10_0": 0, "11_0": 0, "13_0": 0, "16_0": 0, "17_0": 0, "4_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "14_0": 1, "19_0": 1, "20_0": 1, "21_0": 1, "22_0": 1, "23_0": 1, "0_1": 2, "2_1": 2, "3_1": 2, "4_1": 2, "5_1": 2, "6_1": 2, "7_1": 2, "8_1": 2, "9_1": 2, "10_1": 2, "11_1": 2, "13_1": 2, "14_1": 2, "16_1": 2, "17_1": 2, "19_1": 2, "20_1": 2, "21_1": 2, "22_1": 2, "23_1": 2, "0_2": 2, "2_2": 2, "3_2": 2, "4_2": 2, "5_2": 2, "6_2": 2, "7_2": 2, "8_2": 2, "9_2": 2, "10_2": 2, "11_2": 2, "13_2": 2, "14_2": 2, "16_2": 2, "17_2": 2, "19_2": 2, "20_2": 2, "21_2": 2, "22_2": 2, "23_2": 2, "1_0": 3, "12_0": 3, "15_0": 3, "18_0": 3, "1_1": 4, "12_1": 4, "15_1": 4, "18_1": 4, "1_2": 4, "12_2": 4, "15_2": 4, "18_2": 4
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: left,),
    (align: left, indent: 1em,),
    (align: right,),
    (bold: true, align: left,),
    (bold: true, align: right,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 3, stroke: 0.05em + black),
 table.hline(y: 24, start: 0, end: 3, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 3, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Characteristic], [Total], [Analytical],
    ),
    // tinytable header end

    // tinytable cell content after
[A. Sociodemographics], [], [],
[N participants], [1275], [1271],
[N diary entries], [16131], [15842],
[Diary entries per person (Mdn, IQR)], [8 (20)], [8 (21)],
[Age], [26.6 (4.9)], [26.6 (4.9)],
[Gender], [], [],
[Man], [746 (58.5%)], [744 (58.5%)],
[Non-binary or other gender identity], [78 (6.1%)], [78 (6.1%)],
[Woman], [384 (30.1%)], [382 (30.1%)],
[BMI (scaled)], [0.00 (1.00)], [0.00 (1.00)],
[SES (scaled)], [0.00 (1.00)], [0.00 (1.00)],
[B. Chronotype], [], [],
[No alarm on free days], [657 (77.8%)], [657 (77.8%)],
[MSF~sc~ (HH:MM)], [06:10 (03:22)], [06:10 (03:22)],
[C. Gaming], [], [],
[LN gaming (min/day, Mdn, IQR)], [0.0 (15.1)], [0.0 (15.0)],
[% entries with any LN gaming], [19.1 (29.3)], [19.2 (29.4)],
[D. Sleep Quality Distribution], [], [],
[Very poor], [836 (5.3%)], [836 (5.3%)],
[Poor], [2436 (15.4%)], [2436 (15.4%)],
[Fair], [5575 (35.2%)], [5575 (35.2%)],
[Good], [5494 (34.7%)], [5494 (34.7%)],
[Very good], [1501 (9.5%)], [1501 (9.5%)],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 3, text([Values are M (SD) unless noted. LN = late-night. Sleep quality distribution counts are at the diary-entry level.])),
    ),
    

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Daily Diary Subsample Characteristics
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-diary-demographics>


#pagebreak()
== Diary: Models --- Complete-Case
<diary-models-complete-case>
#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "10_1": 0, "11_1": 0, "12_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "10_2": 0, "11_2": 0, "12_2": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "10_0": 1, "11_0": 1, "12_0": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 3, stroke: 0.05em + black),
 table.hline(y: 13, start: 0, end: 3, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 3, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], [H1a: Sleep Quality (direct)], [H2a: Sleep Quality (chronotype mod.)],
    ),
    // tinytable header end

    // tinytable cell content after
[LN gaming within\-person (per 10 min)], [\-0.00 \[\-0.01, 0.00\]], [0.00 \[\-0.01, 0.01\]],
[LN gaming between\-person (per 10 min)], [0.03 \[0.01, 0.04\]\*\*], [0.02 \[\-0.01, 0.05\]],
[Chronotype (h, centered)], [], [0.03 \[0.01, 0.06\]\*],
[LN within × Chronotype], [], [\-0.00 \[\-0.00, \-0.00\]\*],
[LN between × Chronotype], [], [\-0.00 \[\-0.01, 0.01\]],
[Age (scaled)], [0.03 \[\-0.04, 0.09\]], [\-0.05 \[\-0.15, 0.05\]],
[BMI (scaled)], [0.07 \[0.00, 0.13\]\*], [0.07 \[\-0.02, 0.17\]],
[SES (scaled)], [\-0.12 \[\-0.19, \-0.06\]\*\*\*], [\-0.14 \[\-0.25, \-0.04\]\*\*],
[Day: Weekend], [\-0.00 \[\-0.04, 0.04\]], [\-0.01 \[\-0.06, 0.05\]],
[N Obs], [14683], [8392],
[N Participants], [1132], [509],
[ICC], [0.48], [0.52],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 3, text([\+ p \< 0.10, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001])),
    table.cell(align: left, colspan: 3, text([LN \= late\-night. Confidence intervals shown in brackets.])),
    table.cell(align: left, colspan: 3, text([Complete\-case estimates — no imputation applied to the diary outcome.])),
    table.cell(align: left, colspan: 3, text([Cumulative link mixed models (random intercept for participant) on 5\-level ordinal sleep quality (positive coefficients \= higher probability of worse sleep). Probit link. Late\-night gaming per 10 minutes; chronotype in centered hours; age, BMI, SES scaled within the diary subsample. Region excluded: all diary participants are US\-only.])),
    table.cell(align: left, colspan: 3, text([Within \= daily deviation from person mean; Between \= person mean \- grand mean.])),
    ),
    

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Diary H1 and H2 Probit CLMM --- Complete-Case (Non-Imputed Data)
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-appendix-diary-completecase>


#pagebreak()
== Panel H1 Natural Spline Sensitivity
<panel-h1-natural-spline-sensitivity>
This section examines whether the relationship between late-night gaming and each panel outcome (H1a--H1d) deviates meaningfully from the linearity assumed in the pre-registered models. For each outcome, the linear gaming predictor is replaced by a natural cubic spline (degrees of freedom selected by BIC across df = 2--4). We adopt BIC as the primary selection criterion because its stronger penalty on model complexity is better aligned with the goal of this sensitivity check: to ask whether the added flexibility of a non-linear specification is warranted, rather than whether it yields any marginal in-sample improvement. AIC values are reported alongside BIC for transparency, but do not drive the df choice. To interpret the resulting BIC differences we follow the rule of thumb of #cite(<jones2001nagin>, form: "prose");, which compares models on twice the raw BIC difference: |2ΔBIC| in 0--2 is "not worth mentioning," 2--6 is "positive" evidence, 6--10 is "strong," and \>10 is "very strong." Both the linear and spline models reported in #ref(<tbl-appendix-h1-spline-aic>, supplement: [Table]) and the predicted curves in #ref(<fig-appendix-h1-spline>, supplement: [Figure]) are fit on the complete-case sample.

#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_8": 0, "1_8": 0, "2_8": 0, "3_8": 0, "4_8": 0, "0_9": 0, "1_9": 0, "2_9": 0, "3_9": 0, "4_9": 0, "0_10": 0, "1_10": 0, "2_10": 0, "3_10": 0, "4_10": 0
  )

  #let style-array = ( 
    // tinytable cell style after
    (bold: true,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, left, left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto, auto, auto, auto, auto, auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 11, stroke: 0.05em + black),
 table.hline(y: 5, start: 0, end: 11, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 11, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Outcome], [Model], [Best df], [Linear AIC], [Spline AIC], [Linear BIC], [Spline BIC], [ΔAIC], [ΔBIC], [2|ΔBIC|], [Interpretation],
    ),
    // tinytable header end

    // tinytable cell content after
[H1a: Sleep Quality], [CLMM probit], [2], [4153.4], [4147.2], [4211.5], [4211.2], [-6.2], [-0.3], [0.6], [Not worth mentioning],
[H1b: Sleep Duration], [lmer], [2], [6895.4], [6891.4], [6947.8], [6949.6], [-4.0], [1.8], [3.6], [Positive (linear)],
[H1c: Daytime Sleepiness], [lmer], [2], [12762.5], [12755.3], [12820.7], [12819.3], [-7.2], [-1.4], [2.8], [Positive (spline)],
[H1d: Wellbeing], [lmer], [2], [31144.8], [31138.4], [31211.2], [31211.5], [-6.4], [0.3], [0.6], [Not worth mentioning],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
AIC and BIC comparison for panel H1 linear vs.~natural spline models (complete-case data). Best df selected from 2--4 by BIC (primary criterion); AIC values are reported alongside for transparency. ΔAIC = Spline AIC − Linear AIC; ΔBIC = Spline BIC − Linear BIC; negative values indicate improvement over the linear specification. The Interpretation column applies the #cite(<jones2001nagin>, form: "prose") rule of thumb to 2|ΔBIC|: 0--2 = not worth mentioning; 2--6 = positive; 6--10 = strong; \>10 = very strong evidence in favour of the model with the lower BIC.
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-appendix-h1-spline-aic>


#figure([
#box(image("manuscript_files/figure-typst/fig-appendix-h1-spline-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Marginal predicted outcomes as a function of late-night gaming (natural cubic spline, complete-case data). Shaded ribbon: 95% CI from the delta method. X-axis shows average late-night gaming minutes per day on a common 0--90 min scale across panels; curves are drawn over the observed support within that range. All other covariates held at reference values (scaled continuous predictors at zero; isWeekend = 0; reference region). H1a: P(Fairly bad or Very bad sleep quality), marginalised over the participant random intercept via the marginal probit formula. H1b--H1d: predicted means on the original scale. Each panel title shows the selected spline df; AIC and BIC values are reported in #ref(<tbl-appendix-h1-spline-aic>, supplement: [Table]). The dashed orange line shows the corresponding linear model prediction for comparison.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-appendix-h1-spline>


#pagebreak()
== Panel: Equivalence Testing for All Confirmatory Hypotheses
<sec-appendix-equivalence>
We conducted frequentist equivalence tests (TOST; #cite(<lakens2017equivalence>, form: "prose");; #cite(<lakens2018equivalence>, form: "prose");) for every confirmatory hypothesis (H1a--H1d and H2a--H2d), including those that were statistically significant, by checking whether each pooled 90% CI falls inside a Region of Practical Equivalence (ROPE). Because the focal predictors are raw late-night play-time variables (per 10 min/day) rather than standardized effect sizes, Kruschke's @kruschke2018rejecting default ROPE of ±0.1 × SD(#emph[y];) is rescaled to the coefficient's native units as ±0.1 × SD(#emph[y];) / SD(#emph[x];); for the ordinal probit models (H1a, H2a) the latent residual SD is fixed at 1 by identification, so the ROPE simplifies to ±0.1 / SD(#emph[x];) on the latent scale, which at a category threshold corresponds to a ≈ 3.99 percentage-point shift in cumulative category probability (from $Phi (0.05) - Phi (- 0.05)$). The TOST rule accepts equivalence when the 90% CI lies entirely inside the ROPE, rejects when it lies entirely outside, and otherwise returns "Undecided". The natural-unit interpretation of the H1b--H1d response-scale half-widths (≈ ±7 min of sleep, ±0.38 ESS points, and ±0.53 SWEMWBS points respectively) is given in the Discussion alongside the relevant clinical anchors. The same ±0.1 × SD(#emph[y];) thresholds apply to the H2 interaction coefficients but now describe the change in #emph[the late-night gaming slope] induced by a one-SD shift in chronotype (MSFsc); this is a stringent benchmark because the product predictor's SD (≈ 16.53 for late-night minutes × MSFsc) is substantially larger than that of either factor alone.

#figure([
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_0": 0, "1_0": 0, "2_0": 0, "3_0": 0, "4_0": 0, "5_0": 0, "6_0": 0, "7_0": 0, "8_0": 0, "0_1": 1, "1_1": 1, "2_1": 1, "3_1": 1, "4_1": 1, "5_1": 1, "6_1": 1, "7_1": 1, "8_1": 1, "0_2": 1, "1_2": 1, "2_2": 1, "3_2": 1, "4_2": 1, "5_2": 1, "6_2": 1, "7_2": 1, "8_2": 1, "0_3": 1, "1_3": 1, "2_3": 1, "3_3": 1, "4_3": 1, "5_3": 1, "6_3": 1, "7_3": 1, "8_3": 1, "0_4": 1, "1_4": 1, "2_4": 1, "3_4": 1, "4_4": 1, "5_4": 1, "6_4": 1, "7_4": 1, "8_4": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (fontsize: 0.85em, align: left,),
    (fontsize: 0.85em, align: right,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (20.00%, 20.00%, 20.00%, 20.00%, 20.00%),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 5, stroke: 0.05em + black),
 table.hline(y: 9, start: 0, end: 5, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 5, stroke: 0.1em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Hypothesis], [ROPE], [90% CI], [Decision], [p],
    ),
    // tinytable header end

    // tinytable cell content after
[H1a: Sleep Quality], [[-0.028, 0.028]], [[0.028, 0.075]], [Undecided], [< .001],
[H1b: Sleep Duration], [[-0.033, 0.033]], [[-0.016, 0.006]], [Accepted], [0.456],
[H1c: Daytime Sleepiness], [[-0.107, 0.107]], [[-0.026, 0.053]], [Accepted], [0.577],
[H1d: Wellbeing], [[-0.134, 0.134]], [[-0.033, 0.024]], [Accepted], [0.805],
[H2a: Sleep Quality × Chronotype], [[-0.006, 0.006]], [[-0.011, 0.005]], [Undecided], [0.487],
[H2b: Sleep Duration × Chronotype], [[-0.007, 0.007]], [[-0.001, 0.007]], [Accepted], [0.199],
[H2c: Daytime Sleepiness × Chronotype], [[-0.022, 0.022]], [[-0.021, 0.003]], [Accepted], [0.225],
[H2d: Wellbeing × Chronotype], [[-0.028, 0.028]], [[-0.018, 0.000]], [Accepted], [0.101],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 5, text([ROPE = Region of Practical Equivalence, rescaled to the native units of the raw coefficient: ±0.1 × SD(y) / SD(x) for linear mixed models and ±0.1 / SD(x) for the ordinal probit models (H1a, H2a), where the latent residual SD is fixed at 1. SD(x) is the sample SD of the focal predictor (product predictor for interactions).])),
    table.cell(align: left, colspan: 5, text([Decision via TOST rule (Lakens, 2017): Accepted = 90% CI entirely inside ROPE; Rejected = 90% CI entirely outside ROPE; Undecided = otherwise.])),
    table.cell(align: left, colspan: 5, text([p = Wald p-value for the pooled coefficient (not the TOST equivalence p).])),
    ),
    

  ) // end table

  ]) // end align

] // end block
], caption: figure.caption(
position: top, 
[
Equivalence test results for the focal predictor in every confirmatory hypothesis. H1a--H1d test the pooled late-night gaming coefficient; H2a--H2d test the pooled chronotype × late-night gaming interaction. Statistically significant effects (e.g., H1a) are included for completeness --- a significant effect paired with an "Undecided" or "Rejected" equivalence decision indicates that the estimate, although reliably non-zero, is not small enough to be declared practically equivalent to the null under the ROPE.
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-appendix-equivalence>






#bibliography("bibliography.bib")
