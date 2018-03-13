!********+*********+*********+*********+*********+*********+*********+*
!*   This include file determines the compile-time memory settings    *
!*  for the Sea Ice Simulator (SIS), versions 2 and later.            *
!********+*********+*********+*********+*********+*********+*********+*

#include <MOMSIS2_domain.h>

#define NCAT_ICE_ 5
                               !    The number of sea-ice categories.  When
                               !  open water is considered, there are 0:NCAT_ICE
                               !  categories in total.
#define NK_ICE_  4
                               !    The number of vertical partitions within the
                               !  sea-ice.  (For SIS this is 2; for SIS5L it is 4.)
#define NK_SNOW_ 1
                               !    The number of vertical partitions within the
                               !  snow layer atop the sea-ice, usually 1.
#define MAX_FIELDS_ 50
                               !    The maximum permitted number (each) of
                               !  restart variables, time derivatives, etc.
                               !  This is mostly used for the size of pointer
                               !  arrays, so it should be set generously.

#undef SYMMETRIC_MEMORY_

#include <MOM_memory_macros.h>
#include <SIS_memory_macros.h>
