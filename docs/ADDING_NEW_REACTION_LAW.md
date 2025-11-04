## Adding a New Reaction Law

### Overview
- Implement a new rate-law function (returns string, pretty.string, latex, mj, mathml, content.ml).
- Add create/edit UI and previews.
- Store/update in `rv.REACTIONS` and parameters.
- Ensure ODE derivation, export, and load/save support the law.
- Show the law in the Summary tab.

### Files to Update
- Backend rate-law implementation:
  - `server/rate_laws_equations.R` (kinetics) or `server/rate_laws_IO.R` (transport/IO)
- Create/Edit UI:
  - `ui/modal_reaction_add.R`, `ui/modal_reaction_edit.R` (picker option + conditionalPanel)
  - `server/02_equations_renderUI.R` (add `output$equationBuilder_<law>` inputs)
- Live previews (create + edit):
  - `server/02_equations_text_mathjax.R` (text, MathJax, LaTeX branches)
- Create/Edit storage logic:
  - `server/02_equations.R` (collect inputs, validate, call rate-law, store to `rv.REACTIONS` and parameters)
  - `server/02_equations_edit.R` (load existing, rebuild on edit, save back)
- Differential equations:
  - `server/DeriveODEs.R` (apply rate string with stoichiometry)
  - `server/05_differential_equations.R` if needed
- Export and persistence:
  - Writers: `server/write_R.R`, `server/write_MATLAB.R`, `server/write_sbml.R`
  - Loaders: `server/load_rds.R`, `server/load_sbml.R`, `server/sbml_fxns.R` if parsing tweaks needed
- Summary rendering:
  - `server/41_summary.R`

### Minimal Workflow
1) Duplicate a close existing law (e.g., Michaelis–Menten) across the files above.
2) Rename input IDs, output IDs, and list keys consistently (`<law>` and `<law>_edit`).
3) In the rate-law function, return all representations (`string`, `pretty.string`, `latex`, `mj`, `mathml`, `content.ml`).
4) Use `CheckParametersForErrors` and helpers (`BuildParameters`/`StoreParameters`) to register parameters.
5) Store a reaction entry under `rv.REACTIONS$<Group>` and the master `rv.REACTIONS$reactions`.
6) Verify previews (create/edit), ODE contribution signs, and exports (R/MATLAB/SBML).

### Template: Rate-Law Function (R)
```r
My_New_Law <- function(arg1, arg2, volumeVar) {
  # Functional string used by ODE builder
  rate.law <- paste0(arg1, "*", arg2)  # example; build your expression

  # Pretty and display formats
  ps            <- paste0(arg1, "*[", arg2, "]")
  latex.rate    <- paste0(Var2Latex(arg1), "*", Var2Latex(arg2))
  mj            <- paste0(Var2MathJ(arg1),  "*", Var2MathJ(arg2))

  # Scale by compartment volume
  rate.law      <- paste0(volumeVar, "*(", rate.law, ")")
  mj            <- paste0(Var2Latex(volumeVar), "*(", latex.rate, ")")
  latex.rate    <- paste0(Var2Latex(volumeVar), "*(", latex.rate, ")")

  # MathML variants
  ml            <- katex_mathml(latex.rate)
  content.ml    <- paste0("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
                          string2mathml(rate.law),
                          "</math>")

  list(
    string      = rate.law,
    pretty.string = ps,
    latex       = latex.rate,
    mj          = mj,
    mathml      = ml,
    content.ml  = content.ml
  )
}
```

### UI Hook Checklist
- Add law option to the Reaction Law picker in `ui/modal_reaction_add.R` and `ui/modal_reaction_edit.R`.
- Add a `conditionalPanel` with any toggles (e.g., Vmax vs kcat).
- Implement `output$equationBuilder_<law>` in `server/02_equations_renderUI.R` (species pickers, parameter inputs, values).

### Previews (Create/Edit)
- In `server/02_equations_text_mathjax.R` add branches for:
  - Create builder text line, MathJax, and LaTeX
  - Edit builder text line, MathJax, and LaTeX

### Storage and Edits
- Create: in `server/02_equations.R` gather inputs, validate params, call the law function, store to `rv.REACTIONS`, append to `rv.REACTIONS$reactions`.
- Edit: in `server/02_equations_edit.R` load existing values, rebuild on change, update `rv.REACTIONS` entries.

### ODEs, Export, Summary
- ODEs: ensure `server/DeriveODEs.R` consumes the returned `string` and applies stoichiometry signs to species.
- Export: add the law to `write_R.R`, `write_MATLAB.R`, `write_sbml.R`.
- Load: support in `load_rds.R`, `load_sbml.R` (and `sbml_fxns.R` if needed).
- Summary: render MathJax for the new law in `server/41_summary.R`.

---
Tip: Start by cloning the Michaelis–Menten code paths; rename IDs and variables, then adapt the formula.


