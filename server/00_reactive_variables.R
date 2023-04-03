# This script holds the main storage variables for the important aspects of the 
# model, separated in meaningful ways.

# Curent varables include:
# rv.COMPARTMENTS 
# rv.SPECIES
# rv.REACTIONS
# IO
# params
# DE
# options
# results
# info
# logs
# id
# pe (parameter estimation)
# loop (plot loop mode vars)
# units
# TableOverrides

# Compartments in Model --------------------------------------------------------
rv.COMPARTMENTS <- reactiveValues(
  #   Name
  #   ID
  #   Value
  #   Volume (volume variable: V1, V2 etc)
  #   par.Id (id associated with volume)
  #   Unit
  #   UnitDescription
  #   BaseUnit  
  #   BaseValue
  #   Description
  compartments = list(),
  compartments.df = data.frame(),
  compartments.names = vector()
)

# rv.REACTIONS# Species in Model -----------------------------------------------------------
rv.SPECIES <- reactiveValues(
  #   Name
  #   ID
  #   Value
  #   Unit
  #   UnitDescription
  #   BaseUnit  
  #   BaseValue
  #   Description
  #   Compartment
  #   Compartment ID
  #   boundaryCondition (if true, differential eqn gen is ignored)
  
  species = list(),
  species.df = data.frame(),
  species.names = vector(),

  df.by.compartment = data.frame(),
  plotted.var.table = data.frame()
)

# Reactions in Model -----------------------------------------------------------
rv.REACTIONS <- reactiveValues(
  # Holds overall equation information for quick searching
  # There should be an overall (reactions) variable for reaction information 
  # and then each individual law should have its own variable
  # This means mass action, michaleis menton, deg, etc should all be their own
  
  reactions = list(),
  # "ID",             (1)  Specific equation ID
  # "Eqn.Type",       (2)  Type of equation (chem, enz)
  # "Law",            (3)  Law that the equation uses
  # "Species",        (4)  Species in equations
  # "Rate.Constants", (5)  Parameters in equation
  # "Compartment",    (6)  Compartment reaction occurs in
  # "Description",    (7)  Equation Description
  # "Species.Id",     (8)  IDs of species in model
  # "Parameters.Id",  (9)  IDs of parameters in model
  # "Compartment.Id"  (10) ID of compartment eqn is in
  # "Equation.Text"   (11) Text version of equation
  # "Equation.Latex"  (12) Latex text version of equation
  # "Equation.MathJax (13) Mathjax text version of equation
  # "Rate.Law"        (14) String text for rate law
  # "Rate.MathML      (15) MathMl for rate law
  # "Reversible       (16) Bool if the equation is reversible or not
  
  
  # Holds all information on chemical based reactions
  massAction = list(),
  # "ID",         # (1)  Specific equation ID
  # "Law",        # (2)  Chemical Law
  # "LHS.coef",   # (3)  LHS Coefs (3 in 3A --> 2B)
  # "LHS.var",    # (4)  LHS Vars (A in 3A --> 2B)
  # "RHS.coef",   # (5)  Coefficients on RHS of equation
  # "RHS.var",    # (6)  Variables on RHS of equation
  # "arrow",      # (7)  Reversible or forward only
  # "kf",         # (8)  Forward Reaction Coefficient
  # "kr",         # (9)  Reverse Reaction Coefficient
  # "FM.bool",    # (10) Boolean if forward regulator exists
  # "FMs",        # (11) Forward Regulators (Modifiers)
  # "FM.rateC",   # (12) Corresponding rate constants for FM
  # "RM.bool",    # (13) Boolean if reverse regulator exists
  # "RMs",        # (14) Reverse Regulators (Modifiers)
  # "RM.rateC",   # (15) Corresponding rate constants for RM
  
  # Holds all information on enzyme based reactions
  michaelisMenten = list(),
  # "ID",        # (1)  ID of enzyme reaction
  # "Law",       # (2)  Law that enzyme reaction follows
  # "Substrate", # (3)  Substrate that enzyme acts upon
  # "Product",   # (4)  Product of the enzyme reaction
  # "Enzyme",    # (5)  Enzyme in reaction
  # "kcat",      # (6)  Catalytic RC for enzyme reaction
  # "Km",        # (7)  Michelis Menton Constant
  # "Vmax"       # (8)  Maximum Velocity for enz reaction
  
  # Holds Synthesis Reaction Information
  synthesis = list(),
  # "ID",        # (1)  ID of enzyme reaction
  # "Law",       # (2)  Law that enzyme reaction follows
  # "VarSyn",    # (3)  Variable being synthesized
  # "RC",        # (4)  Rate Constant for synthesis reaction
  # "Factor"     # (5)  Factor causing synthesis of VarSyn
  
  # Holds Degradation Reaction Information
  degradation = list(),
  # "ID",        # (1)  ID of enzyme reaction
  # "Law",       # (2)  Law that enzyme reaction follows
  # "VarDeg",    # (3)  Variable being degraded
  # "ConcDep",   # (4)  Bool is rate is concentration dependent
  # "RC",        # (5)  Rate Constant for Degradation reaction
  # "Km",        # (6)  Michaelis Menton Constant
  # "Enz",       # (7)  Enzyme causing the degradation
  # "Vmax",      # (8)  Maximum Velocity of enzyme degradation
  # "Prods"      # (9)  Products made from degradation if made
  
  # Lists above get converted to dataframes below for various reasons
  reactions.df = data.frame(),
  massAction.df = data.frame(),
  michaelisMenten.df = data.frame(),
  synthesis.df = data.frame(),
  degradation.df = data.frame(),
  
  # This is used to keep track of how many eqns were made 
  # (specifically keeping strack of pregenerated rate constant naming)
  reaction.id.counter = 0,

)

# Input/ Ouput ----------------------------------------------------------------
rv.IO <- reactiveValues(
  InputOutput = list(),
  # ID
  # Direction - (Input, Output, Both)
  # Type - gets the type of the input (rate, diffusion, synthesis, etc)
  # Compartment.Out
  # Compartment.In
  # Species.Out
  # Species.In
  # Parameters - Parameters used in flow
  # Compartment.ids
  # Species.ids
  # Parameter.ids
  
  IO.df = data.frame(),
  IO.logs = vector(),
  
  Flow.In  = list(),
  # Compartment
  # Compartment.Id
  # Species
  # Flow.Parameter
  # Parameter.Id
  
  Flow.Out = list(),
  # Compartment
  # Compartment.Id
  # Species
  # Species.Id
  # Flow.Parameter
  # Parameter.Id
  
  Flow.Between = list(),
  # Compartment.In
  # Compartment.In.Id
  # Compartment.Out
  # Compartment.Out.Id
  # Species.In
  # Species.In.Id
  # Species.Out
  # Species.Out.Id
  # Flow.Parameter
  # Parameter.id
  
  Clearance = list(),
  # Compartment
  # Compartment.Id
  # Species
  # Species.Id
  # Flow.Parameter
  # Parameter.Id
  
  Simple.Diffusion = list(),
  # Compartment.In
  # Compartment.In.Id
  # Compartment.Out
  # Compartment.Out.Id
  # Species.In
  # Species.In.Id
  # Species.Out
  # Species.Out.Id
  # PS (Diffusivity Coeffficient)
  # Parameter.id
  
  Facillitated.Diffusion = list()
  # Compartment.In
  # Compartment.In.Id
  # Compartment.Out
  # Compartment.Out.Id
  # Species.In
  # Species.In.Id
  # Species.Out
  # Species.Out.Id
  # Vmax
  # Km
  # Parameter.id
)


# Parameters -------------------------------------------------------------------
rv.PARAMETERS <- reactiveValues(
  parameters = list(), 
  #   Name
  #   ID
  #   Value
  #   Unit
  #   UnitDescription
  #   BaseUnit  
  #   BaseValue
  #   Description
  #   Type
  #   Type.note
  parameters.df = data.frame(),
  parameters.names = vector(),
  # Parameters that are not constant and based off other variables
  non.constant.pars = list()

)

# Differential Equations -------------------------------------------------------
rv.DE <- reactiveValues(
  # Store differential equations for viewing (without volume term)
  de.eqns = vector(), 
  # Store differential equations as latex eqns to print
  de.eqns.in.latex  = vector(), 
  # Eqns to be used in solver (volume term included)
  de.eqns.for.solver = vector(),
  custom.diffeq.var = vector(), #keeps track of indices of custom differential eqns
  custom.diffeq = vector(), #keeps track of custom entered diffeq
  custom.diffeq.df = data.frame(matrix(ncol = 2, nrow = 0))
)

# Options ----------------------------------------------------------------------
options <- reactiveValues(time.start = 0,
                          time.end = 100,
                          time.step = 1,
                          time.scale.bool = FALSE,
                          time.scale.value = 0,
                          ode.solver.type = "lsoda"
)

# Result -----------------------------------------------------------------------
results <- reactiveValues(
  model = data.frame(),
  model.units.view = data.frame(), #model to view in results table with viewing units
  is.pp = FALSE, #lets system know if post processing has occured
  pp.eqns = vector(), # keeeps tack of equations in text print form.
  pp.eqns.col = vector(), # keeps track of equation in processing form
  pp.vars = vector(), #vars to add
  pp.model = data.frame(), #new model with post processing
  model.final = data.frame(), #final data frame
  model.has.been.solved = FALSE,
  time.units = "min",
  concentration.units = "mol"
)

# Info -------------------------------------------------------------------------
info <- reactiveValues(
  version.number = 1.2
)

# Logs -------------------------------------------------------------------------
logs <- reactiveValues(variable.debug.button = "",
                       variable.debug.table = data.frame()
)

# ID for variable Section ------------------------------------------------------

id <- reactiveValues(
  id.df = data.frame(matrix(ncol = 2
                            ,nrow = 0,
                            dimnames = list(NULL, c("id", "idName")))),

  id.var.seed = 1,
  id.eqn.seed = 1,
  id.param.seed = 1,
  id.comp.seed = 1,
  id.io.seed = 1
)

counts <- reactiveValues(loading.model = 0)

# Parameter Estimation ---------------------------------------------------------
pe <- reactiveValues(
  loaded.species = vector(),
  pars = vector(),
  initial.guess = vector(),
  lb = vector(),
  ub = vector(),
  calculated.values = vector(),
  solved.model = data.frame(),
  successful.run = FALSE,
  previous.values = vector(),
  log.of.run = "Parameter Estimation Iterations will appear here"
)


# Plot Loop Mode ---------------------------------------------------------------

loop <- reactiveValues(
  parameters = data.frame(matrix(ncol=3,
                                 nrow=0,
                                 dimnames = list(NULL, c("Parameter",
                                                         "Value",
                                                         "Description")))),
  ICs = data.frame(matrix(ncol = 4,
                          nrow = 0,
                          dimnames = list(NULL, c("Variable",
                                                  "Value",
                                                  "Units",
                                                  "Description")))),
  time.start = 0,
  time.end = 100, 
  time.step = 1,
  model.results = data.frame()
)


# Units ------------------------------------------------------------------------

units <- reactiveValues(
  types = c("Duration",
            "Energy",
            "Length",
            "Mass",
            "Volume",
            "Flow",
            "Count"),
  base.units = list("Duration" = "min",
                     "Energy" ="kJ",
                     "Length" = "m",
                     "Mass" = "g",
                     "Volume" = "l",
                     "Flow" = "l_per_min",
                     "Count" = "mol",
                     "For.Var" = "mol"),
  possible.units = list("Duration" = measurements::conv_unit_options$duration,
                        "Energy" = measurements::conv_unit_options$energy,
                        "Length" = measurements::conv_unit_options$length,
                        "Mass" = measurements::conv_unit_options$mass,
                        "Volume" = measurements::conv_unit_options$volume,
                        "Flow" = measurements::conv_unit_options$flow,
                        "Count" = measurements::conv_unit_options$count,
                        "For.Var" = measurements::conv_unit_options$count
                        ),
  selected.units = list("Duration" = "min",
                        "Energy" ="kJ",
                        "Length" = "m",
                        "Mass" = "g",
                        "Volume" = "l",
                        "Flow" = "l_per_min",
                        "Count" = "mol",
                        "For.Var" = "mol")
)

# Table Overrides --------------------------------------------------------------
TableOverrides <- reactiveValues(
  # tables don't rerender when the table value changes to value it can't be
  # and changes back (i.e changing volume unit from mol to joule) By updating
  # this value in that case, I can force a re render of the table
  compartment.table = 1,
  var.table = 1,
  param.table = 1,
  eqn.table = 1
)



