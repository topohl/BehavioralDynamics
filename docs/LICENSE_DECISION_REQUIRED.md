# License decision required

**Status: no `LICENSE` file exists in this repository.**

This was verified during the publication restructuring and left unresolved
deliberately. Choosing a license is a legal and institutional decision, not a
technical one, and it was not made autonomously.

---

## Why this matters before release

Without an explicit license, **default copyright applies**. In practice that
means:

- the code is "source-available" but **not** open source;
- nobody outside the copyright holder has permission to copy, modify,
  redistribute or build on it, even though the repository is publicly readable;
- a journal, reviewer or reader who wants to reuse the pipeline has no legal
  basis to do so;
- some journals and funders require an explicit open license as a condition of
  publishing code, and some archives (including Zenodo deposits intended for a
  DOI) expect a declared license.

The repository README states this plainly rather than implying openness that has
not been granted.

---

## Options, described neutrally

These are the common choices for academic research code. No recommendation is
made here.

### MIT License

Permissive. Anyone may use, modify and redistribute, including in proprietary
and commercial work, provided the copyright notice and license text are retained.
Very short and widely understood. Offers no patent grant.

### BSD 3-Clause

Permissive, practically similar to MIT, with an additional clause prohibiting use
of the authors' or institution's names to endorse derived products without
permission. Sometimes preferred where institutional name protection matters.

### Apache License 2.0

Permissive, like MIT, but longer and more explicit. Adds an express patent
license from contributors and a patent-retaliation clause, and requires derived
works to state significant changes. Often preferred where patent exposure is a
consideration.

### GNU GPL v3

Copyleft. Anyone may use and modify, but distributed derivative works must also
be released under GPL v3 with source. Keeps derivatives open; may deter reuse by
groups that cannot open-source their own code.

### GNU AGPL v3

Copyleft, extending GPL v3 to network use: a modified version offered as a
service must also make its source available. Rarely relevant to an offline
analysis pipeline.

### CC BY 4.0

A Creative Commons attribution license. Widely used for data, figures and
documentation. **Not recommended for software** — Creative Commons itself advises
against using CC licenses for code, since they do not address source
distribution, patents or warranty disclaimers in software terms.

### Deliberately no license

A valid choice if the code is intended to remain viewable but not reusable. It
should then be stated explicitly rather than left ambiguous, and it may conflict
with journal or funder policy.

---

## Additional considerations for this repository

1. **Institutional policy.** The work was done at the Max Delbrück Center for
   Molecular Medicine. MDC may have a policy or a preferred default for research
   software, and may hold or share copyright depending on the employment terms.
   This should be checked before choosing.

2. **Funder requirements.** Grants supporting the work may mandate an open
   license for research outputs.

3. **Journal requirements.** The target journal's code-availability policy may
   require a specific license class.

4. **Co-authors and contributors.** If anyone other than the maintainer holds
   copyright in any part of the code, they must agree to the license.

5. **Third-party code.** If any snippet was adapted from an externally licensed
   source, that license may constrain the choice. Nothing of the kind was
   identified during the restructuring audit, but this was a structural audit,
   not a provenance review of every function.

6. **The data are separate.** A code license does **not** license the
   experimental data. The repository contains no raw data
   (see `docs/DATA_AND_OUTPUTS.md`), and data sharing terms are a distinct
   decision.

---

## How to apply the decision

Once chosen:

1. Add the full license text as `LICENSE` in the repository root.
2. Add the matching SPDX identifier to `CITATION.cff`, e.g.
   `license: MIT`.
3. Replace the "License" section of `README.md`, which currently states that no
   license is present and that reuse is not yet permitted.
4. Delete this file, or replace it with a short record of the decision and its
   rationale.

Until then this file stands as the record that the question was raised, examined
and consciously left to the maintainer.
