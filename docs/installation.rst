Installation
=================

Requirements
----------------------------

* CDO
* NCO

needed for pre/post processing tools, but not the rest of the workflow
* python3
*  netCDF4
*  pygrib

Download and compile
----------------------------

The following commands will download the hybrid-GODAS repository as well as any other child repositories that it requires:

.. code-block:: bash
       
  git clone https://github.com/UMD-AOSC/hybrid-godas.git
  cd hybrid-godas
  git submodule update --init --recursive


For a new build, configure using an existing machine configuration file, or create your own configuration file

.. code-block::

  ./configure.sh config/gcc.env

   
Build the source code:

.. code-block::

   cd build/gcc
   make -j
