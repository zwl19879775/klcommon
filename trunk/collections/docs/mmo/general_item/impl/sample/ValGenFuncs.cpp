///
/// @file ValGenFuncs.cpp
///
///
#include "ValGenFuncs.h"
#include "../GIObject.h"
#include <assert.h>

#define CAST_PARAM( u ) (struct Param*)(u)

namespace ValGen
{
    TypeSet::ValueType GenDefault( const GI::Object*, void *u )
    {
        Param *p = CAST_PARAM( u );
        assert( p->type == DEFAULT );
        if( p->arg.type == DLONG ) 
        {
            return TypeSet::ValueType( 0L );
        }
        else if( p->arg.type == DDOUBLE )
        {
            return TypeSet::ValueType( 0.0 );
        }
        else if( p->arg.type == DGUID )
        {
            return TypeSet::ValueType( NULL_GUID );
        }
        return TypeSet::ValueType( "" );
    }

    TypeSet::ValueType GenLongRange( const GI::Object *obj, void *u )
    {
        Param *p = CAST_PARAM( u );
        assert( p->type == RANGE );
        TypeSet::KeyType minKey( p->arg.range.minKey );
        TypeSet::KeyType maxKey( p->arg.range.maxKey );
        long min = TypeSet::ValueType::ToLong( obj->GetValue( minKey ) );
        long max = TypeSet::ValueType::ToLong( obj->GetValue( maxKey ) );
        return TypeSet::ValueType( random( min, max ) );
    }

    TypeSet::ValueType GenFromScript( const GI::Object *obj, void *u )
    {
        // TODO:
        return TypeSet::ValueType( 0L );
    }

    TypeSet::ValueType GenGUID( const GI::Object *obj, void *u )
    {
        // TODO:
        return TypeSet::ValueType( NULL_GUID );
    }

    GI::PropertyType::GenValFunc GetFunc( int type )
    {
        struct FuncTable
        {
            int type;
            GI::PropertyType::GenValFunc func;
        } funcTable[] = {
            { DEFAULT, GenDefault },
            { RANGE, GenLongRange },
            { SCRIPT, GenFromScript },
            { SPEC_GEN_GUID, GenGUID },
            { 0, NULL }
        };
        for( int i = 0; funcTable[i].func != 0; ++ i )
        {
            if( funcTable[i].type == type ) return funcTable[i].func;
        }
        return NULL;
    }
}

