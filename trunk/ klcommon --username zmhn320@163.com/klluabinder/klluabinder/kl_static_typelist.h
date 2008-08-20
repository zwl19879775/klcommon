///
/// @file kl_static_typelist.h
/// @author Kevin Lynx
/// @date 8.20.2008
///
#ifndef ___KL_STATIC_TYPELIST_H_
#define ___KL_STATIC_TYPELIST_H_

#include "kl_typelist.h"

///
/// I have to write these dirty codes below, because i cannot use TYPE_LIST##n( DEF_ARG( n ) ).
///
#define STATIC_TYPE_LIST1 TYPE_LIST1( P1 )
#define STATIC_TYPE_LIST2 TYPE_LIST2( P1, P2 )
#define STATIC_TYPE_LIST3 TYPE_LIST3( P1, P2, P3 )
#define STATIC_TYPE_LIST4 TYPE_LIST4( P1, P2, P3, P4 )
#define STATIC_TYPE_LIST5 TYPE_LIST5( P1, P2, P3, P4, P5 )
#define STATIC_TYPE_LIST6 TYPE_LIST6( P1, P2, P3, P4, P5, P6 )
#define STATIC_TYPE_LIST7 TYPE_LIST7( P1, P2, P3, P4, P5, P6, P7 )
#define STATIC_TYPE_LIST8 TYPE_LIST8( P1, P2, P3, P4, P5, P6, P7, P8 )
#define STATIC_TYPE_LIST9 TYPE_LIST9( P1, P2, P3, P4, P5, P6, P7, P8, P9 )
#define STATIC_TYPE_LIST10 TYPE_LIST10( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10 )
#define STATIC_TYPE_LIST11 TYPE_LIST11( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11 )
#define STATIC_TYPE_LIST12 TYPE_LIST12( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12 )
#define STATIC_TYPE_LIST13 TYPE_LIST13( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13 )
#define STATIC_TYPE_LIST14 TYPE_LIST14( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14 )
#define STATIC_TYPE_LIST15 TYPE_LIST15( P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15 )

#endif // ___KL_STATIC_TYPELIST_H_