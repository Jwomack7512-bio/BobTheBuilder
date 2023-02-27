Solve Model 
===============

Navigate to the "Execute Model" tab on the lefthand tabbar. This tab solves our 
model and provides us with the options to customize the solver. For this model,
we will just solve using the standard options:

#. Enter the following times: Starting = 0, End = 5, Step = 0.01, Unit = hr.  
   This generates the model to be solved and integrated at time points 0, 0.01,
   0.02.....5.98, 5.99, 6. 
#. Press the "Run Solver" button. 
#. An output table should be generated with the concentration of each species 
   along each time step of the solved model. 

   .. note:: BioModMe is set by default to show the results table rounded to 3
             digits.  Most terms in these results at the beginning will round
             to 0. To change this, check the **Viewing Options** box and turn 
             off the **Round** option or turn on the **Scientific Notation** 
             option. 


You can download this solved model data to a csv file in the 
download tab on this page.  Post processing is not used in this tutorial. 

.. figure:: images/execute_1.png
