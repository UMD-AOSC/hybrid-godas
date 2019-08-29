!********+*********+*********+*********+*********+*********+*********+*
!*   This include file determines the compile-time memory settings    *
!*  for the Modular Ocean Model (MOM), versions 6 and later.          *
!********+*********+*********+*********+*********+*********+*********+*

!  Specify the numerical domain.
#ifndef NIGLOBAL_
#define NIGLOBAL_ 1440
#endif

#ifndef NJGLOBAL_
#define NJGLOBAL_ 1080
#endif
                               !    NIGLOBAL_ and NJGLOBAL_ are the number of thickness
                               !  grid points in the zonal and meridional
                               !  directions of the physical domain.
#define NK_ 75
                               !    The number of layers in the ocean.

#define STATIC_MEMORY_
                               !    If STATIC_MEMORY_ is defined, the principle
                               !  variables will have sizes that are statically
                               !  determined at compile time.  Otherwise the
                               !  sizes are not determined until run time. The
                               !  STATIC option is substantially faster, but
                               !  does not allow the PE count to be changed at
                               !  run time.

#ifndef NIPROC_
#define NIPROC_ 36
#endif
                               !    NIPROC_ is the number of processors in the
                               !  x-direction.
#ifndef NJPROC_
#define NJPROC_ 24
#endif
                               !    NJPROC_ is the number of processors in the
                               !  y-direction.

#define NIHALO_ 4
#define NJHALO_ 4
                               !   NIHALO_ and NJHALO_ are the sizes of the
                               ! memory halos on each side.

#define MAX_FIELDS_ 80
                               !    The maximum permitted number (each) of
                               !  restart variables, time derivatives, etc.
                               !  This is mostly used for the size of pointer
                               !  arrays, so it should be set generously.

#define BTHALO_ 0
                               !   BTHALO_ is the size of the memory halos in
                               ! the barotropic solver.

#undef SYMMETRIC_MEMORY_

#include <MOM_memory_macros.h>


