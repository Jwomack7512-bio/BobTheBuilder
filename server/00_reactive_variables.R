# This script holds the main storage variables for the important aspects of the 
# model, separated in meaningful ways.

# Curent varables include:
# rv.COMPARTMENTS 
# rv.SPECIES
# rv.REACTIONS
# rv.IO (Input/Output)
# rv.PARAMETERS
# rv.DE (Differential Equations)
# rv.SOLVER.OPTIONS
# rv.RESULTS
# rv.PROGRAM.INFO
# rv.LOGS
# rv.ID
# rv.COUNTS
# rv.PAR.ESTIMATION 
# rv.PLOT.LOOP
# rv.UNITS
# rv.REFRESH (Refresh table values when error checking)

# rv.COMPARTMENTS --------------------------------------------------------------
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

# rv.SPECIES -------------------------------------------------------------------
rv.SPECIES <- reactiveValues(
  species = list(),
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
  #   ReactionIds
  #   IO.Ids
  
  species.df = data.frame(),
  species.names = vector(),

  df.by.compartment = data.frame(),
  plotted.var.table = data.frame()
)

# rv.REACTIONS -----------------------------------------------------------------
rv.REACTIONS <- reactiveValues(
  # Holds overall equation information for quick searching
  # There should be an overall (reactions) variable for reaction information 
  # and then each individual law should have its own variable
  # This means mass action, michaleis menton, deg, etc should all be their own
  
  reactions = list(),
  # ID                || Specific equation ID
  # Eqn.Display.Type  || Display name shown on tables
  # Reaction.Law      || Law that the equation uses
  # Species           || Species in equations
  # Rate.Constants    || Parameters in equation
  # Compartment       || Compartment reaction occurs in
  # Description       || Equation Description
  # Species.Id        || IDs of species in model
  # Parameters.Id     || IDs of parameters in model
  # Compartment.Id    || ID of compartment eqn is in
  # Equation.Text     || Text version of equation
  # Equation.Latex    || Latex text version of equation
  # Equation.MathJax  || Mathjax text version of equation
  # Rate.Law          || String text for rate law
  # Rate.MathML       || MathMl for rate law
  # Reversible        || Bool if the equation is reversible or not
  
  
  # Holds all information on chemical based reactions
  massAction = list(),
  # ID                || Specific equation ID
  # Reaction.Law      || Chemical Law
  # r.stoichiometry   || LHS Coefs (3 in 3A --> 2B)
  # Reactants         || LHS Vars (A in 3A --> 2B)
  # Reactants.Id      || IDs of reactants (collapsed with ", ")
  # p.stoichiometry   || Coefficients on RHS of equation
  # Products          || Variables on RHS of equation
  # Products.Id       || IDs of products 
  # Reversible        || Bool, if true reversible reaction, else irreversible
  # kf                || Forward Reaction Coefficient
  # kr                || Reverse Reaction Coefficient
  # kf.Id             || ID of forward reaction coefficient
  # kr.Id             || ID of reverse reaction coefficient
  
  massActionwReg = list(),
  # ID           # Specific equation ID
  # Reaction.Law # Chemical Law
  # LHS.coef     # LHS Coefs (3 in 3A --> 2B)
  # LHS.var      # LHS Vars (A in 3A --> 2B)
  # RHS.coef     # Coefficients on RHS of equation
  # RHS.var      # Variables on RHS of equation
  # arrow        # Reversible or forward only
  # kf           # Forward Reaction Coefficient
  # kr           # Reverse Reaction Coefficient
  # FM.bool      # Boolean if forward regulator exists
  # FMs          #  Forward Regulators (Modifiers)
  # FM.rateC     # Corresponding rate constants for FM
  # RM.bool      # Boolean if reverse regulator exists
  # RMs          # Reverse Regulators (Modifiers)
  # RM.rateC     # Corresponding rate constants for RM
  
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
  # ID               || ID of synthesis reaction
  # Reaction.Law     || Syn law
  # VarSyn           || Variable being synthesized
  # VarSyn.Id        || Id of variable being synthesized
  # Rate.Constant    || Rate Constant for synthesis reaction
  # Rate.Constant.Id || Rate Constant ID
  # Factor           || Factor causing synthesis of VarSyn (NA if not used)
  # Factor.Id        || Id of Factor
  
  # Holds Degradation Reaction Information
  degradation.by.rate = list(),
  # ID               ||  ID of enzyme reaction
  # Reaction.Law     ||  Law that enzyme reaction follows
  # VarDeg           ||  Variable being degraded
  # VarDeg.Id        ||  Id of degraded variable
  # ConcDep          ||  Bool is rate is concentration dependent
  # Rate.Constant    ||  Rate Constant for Degradation reaction
  # Rate.Constant.Id ||  ID of rate constant
  # Products         ||  Products made from degradation if made
  # Products.Id      ||  IDs of products made from degradation
  
  degradation.by.enzyme = list(),
  # ID        ||  ID of enzyme reaction
  # Law       ||  Law that enzyme reaction follows
  # VarDeg    ||  Variable being degraded
  # ConcDep   ||  Bool is rate is concentration dependent
  # RC        ||  Rate Constant for Degradation reaction
  # Km        ||  Michaelis Menton Constant
  # Enz       ||  Enzyme causing the degradation
  # Vmax      ||  Maximum Velocity of enzyme degradation
  # Prods     ||  Products made from degradation if made
  
  # Lists above get converted to dataframes below for various reasons
  reactions.df = data.frame(),
  massAction.df = data.frame(),
  michaelisMenten.df = data.frame(),
  synthesis.df = data.frame(),
  degradation.by.rate.df = data.frame(),
  degradation.by.enzyme.df = data.frame(),
  
  # This is used to keep track of how many eqns were made 
  # (specifically keeping strack of pregenerated rate constant naming)
  reaction.id.counter = 0,

)

# rv.IO ------------------------------------------------------------------------
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
  # ID
  # Compartment
  # Compartment.Id
  # Species
  # Species.Id
  # Flow.Parameter
  # Parameter.Id

  Flow.Out = list(),
  # ID
  # Compartment
  # Compartment.Id
  # Species
  # Species.Id
  # Flow.Parameter
  # Parameter.Id
  
  Flow.Between = list(),
  # ID
  # n.Split
  # Compartment.In
  # Compartment.In.Id
  # Compartment.Out
  # Compartment.Out.Id
  # Species.In
  # Species.In.Id
  # Species.Out
  # Species.Out.Id
  # Flow.out
  # Flow.out.id
  # Flow.in
  # Flow.in.id
  
  Clearance = list(),
  # ID
  # Compartment
  # Compartment.Id
  # Species
  # Species.Id
  # Flow.Parameter
  # Parameter.Id
  
  Simple.Diffusion = list(),
  # ID
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
  
  Facilitated.Diffusion = list(),
  # ID
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
  
  IO.id.counter = 1
)


# rv.PARAMETERS ----------------------------------------------------------------
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
  #   Used.In           # Vector of IDs parameter is found in
  parameters.df = data.frame(),
  parameters.names = vector(),
  # Parameters that are not constant and based off other variables
  non.constant.pars = list()

)

# rv.DE ------------------------------------------------------------------------
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

# rv.SOLVER.OPTIONS ------------------------------------------------------------
rv.SOLVER.OPTIONS <- reactiveValues(
  time.start = 0,
  time.end = 100,
  time.step = 1,
  time.scale.bool = FALSE,
  time.scale.value = 0,
  time.unit = "min",
  ode.solver.type = "lsoda"
)

# rv.RESULTS -------------------------------------------------------------------
rv.RESULTS <- reactiveValues(
  results.model = data.frame(),
  results.model.units.view = data.frame(), #model to view in results table with viewing units
  results.is.pp = FALSE, #lets system know if post processing has occured
  results.pp.eqns = vector(), # keeeps tack of equations in text print form.
  results.pp.eqns.col = vector(), # keeps track of equation in processing form
  results.pp.vars = vector(), #vars to add
  results.pp.model = data.frame(), #new model with post processing
  results.model.final = data.frame(), #final data frame
  results.model.has.been.solved = FALSE,
  results.time.units = "min",
  results.concentration.units = "mol"
)

# rv.PROGRAM.INFO --------------------------------------------------------------
rv.PROGRAM.INFO <- reactiveValues(
  version.number = 1.2
)

# rv.LOGS ----------------------------------------------------------------------
rv.LOGS <- reactiveValues(
  variable.debug.button = "",
  variable.debug.table = data.frame()
)

# rv.ID ------------------------------------------------------------------------
rv.ID <- reactiveValues(
  # Dataframe with id values in column 1 and id names in col two
  #  |  id  |     idName      |
  #  |   a  |    species001   |
  #  |  kf1 |    parameter001 |
  
  id.df = data.frame(
  matrix(ncol = 2,
         nrow = 0,
         dimnames = list(NULL, c("id", "idName"))
         )
  ),
  id.var.seed = 1,
  id.eqn.seed = 1,
  id.param.seed = 1,
  id.comp.seed = 1,
  id.io.seed = 1
)

# rv.COUNTS --------------------------------------------------------------------
rv.COUNTS <- reactiveValues(
  loading.model = 0
  )

# rv.PAR.ESTIMATION ------------------------------------------------------------
rv.PAR.ESTIMATION <- reactiveValues(
  pe.loaded.species = vector(),
  pe.parameters = vector(),
  pe.initial.guess = vector(),
  pe.lb = vector(),
  pe.ub = vector(),
  pe.calculated.values = vector(),
  pe.solved.model = data.frame(),
  pe.successful.run = FALSE,
  pe.previous.values = vector(),
  pe.log.of.run = "Parameter Estimation Iterations will appear here"
)


# rv.PLOT.LOOP -----------------------------------------------------------------
rv.PLOT.LOOP <- reactiveValues(
  loop.parameters = data.frame(matrix(ncol=3,
                                 nrow=0,
                                 dimnames = list(NULL, c("Parameter",
                                                         "Value",
                                                         "Description")))),
  loop.ICs = data.frame(matrix(ncol = 4,
                          nrow = 0,
                          dimnames = list(NULL, c("Variable",
                                                  "Value",
                                                  "Units",
                                                  "Description")))),
  loop.time.start = 0,
  loop.time.end = 100, 
  loop.time.step = 1,
  loop.model.results = data.frame()
)


# rv.UNITS ---------------------------------------------------------------------
rv.UNITS <- reactiveValues(
  units.types = c("Duration",
                  "Energy",
                  "Length",
                  "Mass",
                  "Volume",
                  "Flow",
                  "Count"), 
  units.base = list("Duration" = "min",
                     "Energy" ="kJ",
                     "Length" = "m",
                     "Mass" = "g",
                     "Volume" = "l",
                     "Flow" = "l_per_min",
                     "Count" = "mol",
                     "For.Var" = "mol"),
  units.choices = list("Duration" = measurements::conv_unit_options$duration,
                        "Energy" =  measurements::conv_unit_options$energy,
                        "Length" =  measurements::conv_unit_options$length,
                        "Mass" =    measurements::conv_unit_options$mass,
                        "Volume" =  measurements::conv_unit_options$volume,
                        "Flow" =    measurements::conv_unit_options$flow,
                        "Count" =   measurements::conv_unit_options$count,
                        "For.Var" = measurements::conv_unit_options$count
                        ),
  units.selected = list("Duration" = "min",
                        "Energy" ="kJ",
                        "Length" = "m",
                        "Mass" = "g",
                        "Volume" = "l",
                        "Flow" = "l_per_min",
                        "Count" = "mol",
                        "For.Var" = "mol")
)

# rv.REFRESH -------------------------------------------------------------------
rv.REFRESH <- reactiveValues(
  # tables don't rerender when the table value changes to value it can't be
  # and changes back (i.e changing volume unit from mol to joule) By updating
  # this value in that case, I can force a re render of the table
  refresh.compartment.table = 1,
  refresh.species.table = 1,
  refresh.param.table = 1,
  refresh.eqn.table = 1
)

rv.REACTIONLAWS <- reactiveValues(
  # Want to store dataframe of reaction laws and types for use in app
  laws = data.frame(
    Name = c("Mass Action",
             "Mass Action (Regulated)",
             "Synthesis",
             "Degradation (Rate)",
             "Degradation (Enzyme)",
             
             "Michaelis Menten"),
    BackendName = c("mass_action",
                    "mass_action_w_reg",
                    "synthesis",
                    "degradation_rate",
                    "degradation_by_enzyme",
                    
                    "michaelis_menten"), 
    Type = c("chemical",
             "chemical",
             "chemical",
             "chemical",
             "chemical",
             
             "enzyme")
  )
)

# rv.DEBUG ---------------------------------------------------------------------
rv.DEBUG <- reactiveValues(
  variable.debug.button = "compartments"
)

