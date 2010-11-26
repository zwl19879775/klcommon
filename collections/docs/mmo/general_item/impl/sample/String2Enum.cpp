///
/// @file String2Enum.cpp
///
///
#include "String2Enum.h"
#include <string.h>
#include "GoodsPropertyType.h"
#include "ValGenFuncs.h"
#include "../GIProperty.h"

using namespace GI;
using namespace ValGen;

MAKE_PAIR_BEGIN( ParamType )
    MAKE_PAIR( DEFAULT )
    MAKE_PAIR( RANGE )
    MAKE_PAIR( SCRIPT )
    MAKE_PAIR( SPEC_GEN_GUID )
MAKE_PAIR_END()

MAKE_PAIR_BEGIN( DefaultType )
    MAKE_PAIR( DLONG )
    MAKE_PAIR( DDOUBLE )
    MAKE_PAIR( DGUID )
    MAKE_PAIR( DSTRING )
MAKE_PAIR_END()

MAKE_PAIR_BEGIN( PropertyType )
    MAKE_PAIR( PT_STATIC )
    MAKE_PAIR( PT_INDEX )
    MAKE_PAIR( PT_IDENTIFY )
    MAKE_PAIR( PT_DYNAMIC )
    MAKE_PAIR( PT_GENERAL )
MAKE_PAIR_END()

MAKE_PAIR_BEGIN( EnumGoodsProperty )
    MAKE_PAIR( PINDEX )
    MAKE_PAIR( PID )
    MAKE_PAIR( PORIGNAME )
    MAKE_PAIR( PNAME )
    MAKE_PAIR( PGOLD_PRICE )
    MAKE_PAIR( PSILVER_PRICE )
    MAKE_PAIR( PTYPE )
    MAKE_PAIR( PUI_PICID )
    MAKE_PAIR( PRGN_PICID )
    MAKE_PAIR( PEQUIP_PICID )
    MAKE_PAIR( PGET_SNDID )
    MAKE_PAIR( PNORMAL_SNDID )
    MAKE_PAIR( PSPECIAL_SNDID )
    MAKE_PAIR( PATTACK_FLAG )
    MAKE_PAIR( PEQUIP_ACTION )
    MAKE_PAIR( PDESC )
    MAKE_PAIR( PMAX_STACKCNT )
    MAKE_PAIR( PSTACKCNT )
    MAKE_PAIR( PMAKER_NAME )
    MAKE_PAIR( PRGNX )
    MAKE_PAIR( PRGNY )
    MAKE_PAIR( PCELL_POS )
    MAKE_PAIR( PINC_HP_MIN )
    MAKE_PAIR( PINC_HP_MAX )
    MAKE_PAIR( PINC_HP )
    MAKE_PAIR( PCOOLDOWN )
MAKE_PAIR_END()

long _String2Enum( const struct EnumDesc *descs, const char *s )
{
    int i = 0;
    for( ; descs[i].desc; ++ i )
        if( strcmp( descs[i].desc, s ) == 0 ) return descs[i].val;
    return 0;
}

