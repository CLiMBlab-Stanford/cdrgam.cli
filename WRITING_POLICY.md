# Writing policy

Write for researchers and engineers who may know GAMs but not this harness.
State observable behavior before implementation detail. Use plain, direct
language and the established terms *project*, *definition*, *dataset*,
*model*, *comparison*, *work item*, *artifact*, and *resolved identity*.

Document inputs, outputs, side effects, constraints, and failure modes when
they affect callers. Keep examples short and valid. Do not claim that a fit
converged merely because it produced finite values, and do not generalize
Gaussian-only core behavior to every family.

Avoid marketing language, filler, unnecessary headings, invented terminology,
and explanations that merely restate syntax. Check every technical claim
against the implementation. Keep AI disclosure in `AI_PROVENANCE.md` and
publication metadata rather than ordinary technical documentation.
