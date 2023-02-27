============================
Degradation Reactions
============================

Degradation
------------------------------
These reactions are dependant on a rate constant. Here we have two specific 
options where the degradation can be concentration dependent or not. 
Often degradations are concentration dependent on themselves, meaning there is 
more degradation at higher concentrations and vice versa. The following is the 
derivation for a concentration dependent degradation: 


.. math:: 
    \begin{equation*}
        \frac{d[Species]}{dt} = -k_{deg}*[Species]
    \end{equation*}

where,

:|kdeg|: degradation rate constant
:Species: species being degraded

.. |kdeg| replace:: :math:`k_{deg}`

When the degradation is not concentration dependent, the resulting flux is:

.. math:: 
    \frac{d[Species]}{dt} = -k_{deg}

When products are made they are just the opposite sign of the above reactions. 
For example for concentration dependent reactions with one degradation to two 
products we would have the resulting differential equations:

.. math::
    \begin{align*}
        \frac{d[Species]}{dt} = -k_{deg}*[Species] \\
        \frac{d[Product 1]}{dt} = +k_{deg}*[Species] \\
        \frac{d[Product 2]}{dt} = +k_{deg}*[Species]
    \end{align*}


Degradation by Enzyme
-----------------------------------------
This degradation option is a basic enzyme reaction using Michaelis Menten 
kinetics where the substrate is the species to be degraded and their often 
is no product. The resulting mathematical flux for the degraded substrate 
would be:

.. math::
    \frac{d[S]}{dt} = -V_{max}\frac{[S]}{K_m+[S]} = 
    -(k_{cat}*[E])\frac{[S]}{K_m+[S]}

where,

:|Vmax|: Maximum Velocity
:E: Enzyme Concentration
:S: Substrate Concentration
:P: Product Concentration
:|KM|: Michaelis Menten Constant
:|kcat|: Catalytic Rate of Enzyme Reaction

.. |Vmax| replace:: :math:`V_{max}`
.. |KM| replace:: :math:`K_M`
.. |kcat| replace:: :math:`k_{cat}`

There is an option to add a product (P), which results in the mathematical 
flux equations to break down to pure Michaelis Menten kinetics for the product 
and substrate which would have the same equation above except with a positive 
sign:

.. math::
    \begin{align*}
        \frac{d[S]}{dt} &= -V_{max}\frac{[S]}{K_m+[S]} = 
        -(k_{cat}*[E])\frac{[S]}{K_m+[S]} \\
        \frac{d[P]}{dt} &= +V_{max}\frac{[S]}{K_m+[S]} = 
        +(k_{cat}*[E])\frac{[S]}{K_m+[S]}
    \end{align*}