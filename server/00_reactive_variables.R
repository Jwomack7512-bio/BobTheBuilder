
#these are the main values that will be used to run and generate the model
vars <- reactiveValues(
  species = vector() #stores model species
  ,descriptions = vector() #stores descriptions of Model
  ,table = data.frame(matrix(ncol = 2
                            ,nrow = 0,
                            dimnames = list(NULL, c("Variable Name"
                                                    ,"Description"))))
)


eqns <- reactiveValues(
   first.run = TRUE #determine if first equation is added yet or not
  ,main = vector() #stores eqn type in model
  ,n.eqns = 0 #stores number of total equations in model (used to autofill names of some var)
  ,additional.eqns = vector() #stores all additional eqns -time, rate, etc...
  ,rate.eqns = vector() #stores all the elements of the rate equations to be added to the model
  ,time.dep.eqns = vector() #stores all time dependent eqns
  
  ,lr.eqns = vector() #stores all rate eqns
  ,eqn.descriptions = vector() #stores all eqn descriptions
  ,eqn.info = data.frame(
    matrix(
      ncol = 18, 
      nrow = 0, 
      dimnames = list(NULL, c("eqn_type", 
                              "LHS_coef", 
                              "LHS_var", 
                              "RHS_coef", 
                              "RHS_var",
                              "arrow_type", 
                              "kf", 
                              "kr",  
                              "kcat",
                              "Vmax", 
                              "Km", 
                              "Enzyme",
                              "FM_bool", 
                              "FMs", 
                              "FM_rateC",
                              "RM_bool",
                              "RMs", 
                              "RM_rateC"))))

    #(1)  eqn_type = type of equation (chem, diffusion, enzyme, etc)    
    #(2)  LHS_coef = Coefficients on LHS of equation (the 3 in 3A -> 2B)
    #(3)  LHS_var = variables on LHS of equation (the A in 3A -> 2B)
    #(4)  RHS_coef = Coefficients on RHS of equation (the 2 in 3A -> 2B) 
    #(5)  RHS_var = variables on RHS of equation (the B in 3A -> 2B)
    #(6)  arrow_type = tells if the reaction is reversible or not
    #(7)  kf = reaction forward coefficient
    #(8)  kr = reaction reverse coefficient
    #(9)  kcat = catalytic coefficient for enzyme reactions
    #(10) Vmax = maximum velocity for enzyme reactions
    #(11) Km = Michelis Menton coefficient for enzyme reactions
    #(12) Enzyme = The enzyme of the reaction
    #(13) FM_bool = boolean if modifiers are used on forward equation (chem rxn)
    #(14) FMs = variables that are used in the modification of reaction (chem rxn) (ex think Wee1)
    #(15) FM_rateC = rate constants associated with the modifying variables
    #(16) RM_bool = boolean if modifiers are used on forward equation (chem rxn)
    #(17) RMs = variables that are used in the modification of reaction (chem rxn) (ex think Wee1)
    #(18) RM_rateC = rate constants associated with the modifying variables
  )

IO <- reactiveValues(
  n.IO = 0 #stores the number of total Input and Outputs
  ,n.inputs = 0
  ,n.outputs = 0
  ,bool.IO.exists = TRUE #determines if In/out input has been given yet.  Avoids adding to df error
  ,bool.IO.added = FALSE
  ,bool.input.exists = TRUE
  ,bool.output.exists = TRUE
  ,bool.input.added = FALSE
  ,bool.output.added = FALSE
  ,input.info = data.frame(matrix(ncol = 7, nrow = 0,
                                  dimnames = list(NULL, c("Type", 
                                                          "Species", 
                                                          "RateConstant",
                                                          "RateBySpecies", 
                                                          "Vmax", 
                                                          "Kcat", 
                                                          "Enzyme"))
                                  ))
  ,output.info = data.frame(matrix(ncol = 7, nrow = 0,
                                  dimnames = list(NULL, c("Type", 
                                                          "Species", 
                                                          "RateConstant",
                                                          "RateBySpecies", 
                                                          "Vmax", 
                                                          "Kcat", 
                                                          "Enzyme"))
  ))
  ,IO.info = data.frame(matrix(ncol = 8, nrow = 0,
                                      dimnames = list(NULL, c("In_or_Out", 
                                                              "Type", 
                                                              "Species", 
                                                              "RateConstant",
                                                              "RateBySpecies", 
                                                              "Vmax", 
                                                              "Kcat", 
                                                              "Enzyme"))))
  #(1) in_or_out = value to tell if this column is an input or output: "input" or "output"
  #(2) Type = gets the type of the input (rate, diffusion, synthesis, etc)
  #(3) Species = actual name of the species going in or out
  #(4) RateConstant = if type rate, name of the rate constant 
  #(5) Vmax = if type enzyme, Vmax of enzyme reaction
  #(6) Kcat = f type enzyme and Vmax not used, kcat of reaction (note Vmax = kcat*enzyme)
  #(7) Enzyme = if type enzyme and Vmax not used, enzyme concentration of reaction
  #(8) RateBySpecies = if rate equation, boolean to tell user to multiply the rate by the concentration of the rate species 
)
ICs <- reactiveValues(
  vals = vector() #store initial condition value
  ,comments = vector() #store comments for ICs
  ,ICs.table = data.frame(matrix(ncol = 3
                                 ,nrow = 0,
                                 dimnames = list(NULL, c("Variable"
                                                         ,"Value"
                                                         ,"Description"))))
  ,first.IC.stored = FALSE #if IC stored, this parameter is used to render values
)

params <- reactiveValues(
   vars.all = vector() #store parameter variable
  ,vals.all = vector() #store parameter value
  ,comments.all = vector() #store comments of parameters
  ,param.table = data.frame(matrix(ncol = 3
                                   ,nrow = 0,
                                   dimnames = list(NULL, c("Parameter"
                                                           ,"Value"
                                                           ,"Description"))))
  #store parameters from equations
  ,eqns.vars = vector() #param variable
  ,eqns.vals = vector() #param variable values
  ,eqns.comments = vector() #param comments
  ,first.param.eqn.stored = FALSE #if parameter stored button hit then this will update parameter values based on those stored and not reset them all to zero
  #store parameters for input variables
  ,inputs.vars = vector()
  ,inputs.vals = vector()
  ,inputs.comments = vector()
  ,first.inputs.stored = FALSE
  #store parameters for output variables
  ,outputs.vars = vector()
  ,outputs.vals = vector()
  ,outputs.comments = vector()
  ,first.outputs.stored = FALSE
  #store parameters from rate variables
  ,rate.eqn.vars = vector()
  ,rate.eqn.vals = vector()
  ,rate.eqn.comments = vector()
  ,first.rate.eqn.stored = FALSE
  ,rate.params = vector()
  #store parameters from rate variables
  ,time.dep.vars = vector()
  ,time.dep.values = vector()
  ,time.dep.comments = vector()
  ,first.time.dep.stored = FALSE
  
  ,parameters.based.on.other.values = vector() #stores all vectors that are not based on other values and not given a hard value (ie k1 = 5*k2+k3 not simply k1 = 5)
  
)

DE <- reactiveValues(
  eqns = vector() #store differential equations
  ,eqns.in.latex  = vector() #store differential equations as latex eqns to print
)

options <- reactiveValues(time.start = 0 
                                ,time.end = 100
                                ,time.step = 1
                                ,time.scale.bool = FALSE
                                ,time.scale.value = 0
                                ,ode.solver.type = "lsoda"
)

results <- reactiveValues(model = data.frame()
                                ,is.pp = FALSE #lets system know if post processing has occured
                                ,pp.eqns = vector() # keeeps tack of equations in text print form.
                                ,pp.eqns.col = vector() # keeps track of equation in processing form
                                ,pp.vars = vector() #vars to add
                                ,pp.model = data.frame() #new model with post processing
)

info <- reactiveValues(
  version.number = 1.2
)

logs <- reactiveValues(IO.logs = vector() #record the log for which inputs are added or not
                       ,input.logs = vector()
                       ,output.logs = vector()
)
