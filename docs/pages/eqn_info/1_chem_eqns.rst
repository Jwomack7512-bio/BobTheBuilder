============================
Chemical Based Equations
============================

Law of Mass Action 
----------------------------
Chemical reactions are derived using the law of mass action. Given the following
chemical reaction scheme: 

.. math::
    \begin{equation*}
        aA + bB \longleftrightarrow[k_{-1}]{k_1} cC + dD
    \end{equation*}

where a, b, c, d are stoichiometic coefficients and A, B, C, D are the chemical
species. The law is as follows:

.. math:: 
     \begin{equation*}
        -\frac{1}{a}\frac{d[A]}{dt} = -\frac{1}{b}\frac{d[B]}{dt} = 
        \frac{1}{c}\frac{d[C]}{dt} = \frac{1}{d}\frac{d[D]}{dt} =
         k_1[A]^a[B]^b - k_{-1}[C]^c[D]^d 
    \end{equation*}

From the above law, each individual term can is derived below:

.. math:: 
    \begin{align*}
        \frac{d[A]}{dt} &= -a*k_1[A]^a[B]^b + a*k_2[C]^c[D]^d \\
        \frac{d[B]}{dt} &= -b*k_1[A]^a[B]^b + b*k_2[C]^c[D]^d \\
        \frac{d[C]}{dt} &= c*k_1[A]^a[B]^b - c*k_2[C]^c[D]^d \\
        \frac{d[D]}{dt} &= d*k_1[A]^a[B]^b - d*k_2[C]^c[D]^d
    \end{align*}

Each individual equation will be added to their overall flux differential
equation in the differential equation solver section.