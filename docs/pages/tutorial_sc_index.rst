============================
Tutorial
============================

This tutorial covers basics features of BioModME by building a simplified, 
single compartment model. In this example, we focus on a set of reactions 
occurring in a cell. Compound “A” is synthesized by “Prot” at a constant rate 
and reacts with a limited supply of compound “B” found in the cell to make an
intermediate “C1”. C1 undergoes an enzymatic conversion to “C2” and this new
intermediate undergoes self-cleavage to the desired end product, “P”, and an 
inhibitor, “I”, of Prot.  This inhibitor feedback binding to Prot stopping the
synthesis of A, and thereby the entire reaction scheme.

.. image:: tutorial_sc/images/cell_model.png
    :width: 50 %
    :align: center 

.. toctree::

    tutorial_sc/defineVariables
    tutorial_sc/createEquations

