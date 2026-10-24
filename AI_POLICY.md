# AI / LLM Usage Policy for Contributors

This policy applies to any use of AI tools (LLMs, coding assistants, autonomous agents) in
contributions to Audacity. It includes code, tests, issues, bug reports, pull request descriptions,
review comments, documentation, translations, and other project contributions.

Coding agents have become powerful enough to carry out complex tasks, promising large gains in
productivity. In practice, we have found that they can shift the bottleneck from writing a solution
to _reviewing_ it. As maintainers, we remain responsible for the product we deliver, which means
understanding its every single bit.

A good contribution carries its share of that responsibility. Contributors need to understand their
work, check that it solves the intended problem, and help reviewers assess it.

The goal is to make sure these tools help the project rather than create additional work for
maintainers.

## 1. AI-assisted contributions are allowed

You may use LLMs and AI coding tools to help write code, tests, documentation, translations, and
other contributions to Audacity.

The same standards for correctness, maintainability, licensing, testing, and review apply regardless
of how the contribution was produced.

## 2. You are accountable for what you submit

Contributors must read, review, and understand all LLM-generated code and text, including code
comments, before requesting review. They remain fully accountable for their contribution and must be
able to answer questions about it.

To ensure sufficient self review and understanding of the work, it is strongly recommended that
contributors write PR descriptions themselves (if needed, using tools for translation or
copy-editing).

Do not submit generated output that you have not personally reviewed and understood.

Do not mechanically paste maintainer feedback into an AI tool and submit its response. Reviewers
need to engage with your understanding and reasoning, not with an automated intermediary.

Exercise caution when allowing AI tools to act on your behalf. It is still expected for a human to
review, understand and approve any agent task to open pull requests, create issues, post review
comments, push changes, or take other actions in Audacity repositories, issue trackers, or community
spaces.

Maintainers may close, postpone, or decline to review contributions that do not meet the project's
requirements or that need disproportionate review effort, regardless of whether AI was involved.

## 3. Disclosure

Disclose AI assistance when an AI tool produced a substantial portion of the submitted content.

Disclosure is not required for routine autocomplete, boilerplate generation, spelling or grammar
correction, light copy-editing, or translation of text you wrote yourself.

When disclosure is required for a pull request containing code, tests, or documentation:

- check the AI-assistance field in the pull request template and name the tool
- add an `Assisted-by:` trailer to the relevant commit or commits:

`Assisted-by: <tool name> <version, if known>`

For issues or other contributions that do not involve commits, disclose the assistance in the
relevant issue, template field, or accompanying description.

Use `Assisted-by:`, not `Co-authored-by:`. An AI tool cannot take responsibility for a contribution
or sign Audacity's CLA.

Disclosure is intended to give reviewers useful context. It is not necessary to identify exactly
which individual lines were generated.

When in doubt, disclose.

## 4. Copyright, licensing, and the CLA

By submitting a contribution, you are responsible for making sure that you have the necessary rights
to contribute everything it contains.

AI assistance does not make copied, regenerated, or otherwise protected third-party material safe to
relicense.

If generated output appears to reproduce code, text, artwork, or other material from an identifiable
source, you must verify that you have the right to use it and that its license is compatible with
Audacity before including it.

Do not use AI tools to disguise, rewrite, or regenerate material in order to avoid the requirements
of its original license.

Where Audacity's Contributor License Agreement applies, you must sign it as usual.

AI assistance does not change the representations you make under the CLA or your responsibility for
having the necessary rights to submit the contribution.

## References

Our policy is adapted from the following references:

- [LLVM: AI Tool Use Policy](https://llvm.org/docs/AIToolPolicy.html)
- [Fedora Project: Policy on AI-Assisted
  Contributions](https://communityblog.fedoraproject.org/council-policy-proposal-policy-on-ai-assisted-contributions/)
- [Apache Software Foundation: Generative AI
  Tooling](https://www.apache.org/legal/generative-tooling.html)
