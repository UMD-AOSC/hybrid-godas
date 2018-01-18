!********+*********+*********+*********+*********+*********+*********+*
!*   This include file determines the compile-time memory settings    *
!*  for the Modular Ocean Model (MOM), versions 6 and later.          *
!********+*********+*********+*********+*********+*********+*********+*

#include <MOMSIS2_domain.h>

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
