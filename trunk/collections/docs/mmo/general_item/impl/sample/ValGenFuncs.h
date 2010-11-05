///
/// @file ValGenFuncs.h
///
///
#ifndef ___VALGENFUNCS_H_
#define ___VALGENFUNCS_H_

#include "TypesDef.h"
#include "../GIForwardRefs.h"
#include "../GIProperty.h"

namespace ValGen
{
    enum ParamType { DEFAULT, RANGE, SCRIPT, SPEC_GEN_GUID };
    enum DefaultType { DLONG, DDOUBLE, DGUID, DSTRING };
    struct Param
    {
        enum { SCRIPT_SIZE = 64 };
        int type; ///< param type.
        union
        {
            int type; ///< 'DEFAULT' value type.
            struct Range ///< range value [...).
            {
                long minKey;
                long maxKey;
            }range;
            char file[SCRIPT_SIZE]; ///< script file name.
        } arg;
    }; 

    TypeSet::ValueType GenDefault( const GI::Object*, void *u );

    TypeSet::ValueType GenLongRange( const GI::Object *obj, void *u );

    TypeSet::ValueType GenFromScript( const GI::Object *obj, void *u );

    TypeSet::ValueType GenGUID( const GI::Object *obj, void *u );

    GI::PropertyType::GenValFunc GetFunc( int type );
}

#endif
