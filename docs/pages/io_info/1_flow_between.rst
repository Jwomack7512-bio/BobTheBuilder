============================
Flow Between
============================

Flow between two or more compartments follow the mechanics described below. In 
the below figure, we have flow between two compartments. We have species 
:math:`A_1` flowing out of compartment 1 into compartment 2 as :math:`A_2`.

.. container:: bordergrey

    .. figure:: images/flow_between.png
        :width: 400
        :height: 300
        :align: center

The flows from a compartment are derived as the flow rate (F) multiplied by the 
concentration of the species leaving the comparment as a minus term. 
Flow to a compartment is the same derivation without the minus term. 
The flows in the above diagram would be derived as:

.. math::
    \begin{align*}
        V_1 * \frac{d[A_1]}{dt} &= -F * A_{1} \\
        V_2 * \frac{d[A_2]}{dt} &= F * A_{1}
    \end{align*}