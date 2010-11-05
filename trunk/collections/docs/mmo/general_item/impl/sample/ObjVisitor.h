///
/// @file ObjVisitor.h
/// @brief Some helper functions to visit object properties.
///
#ifndef ___OBJVISITOR_H_
#define ___OBJVISITOR_H_

#include "TypesDef.h"
#include "../GIObject.h"

namespace ObjVisitor
{
    TypeSet::IDType ID( const GI::Object *obj );

    long Count( const GI::Object *obj );

    void SetCount( GI::Object *obj, long cnt );

    long Pos( const GI::Object *obj );

    void SetPos( GI::Object *obj, long pos );

    TypeSet::IndexType Index( const GI::Object *obj );

    bool CanStack( const GI::Object *obj1, const GI::Object *obj2 );

    bool IsSameIndex( const GI::Object *obj1, const GI::Object *obj2 );

    long MaxCount( const GI::Object *obj );

    bool IsConObject( const GI::Object *obj );

    long ConSize( const GI::Object *obj );

    long Type( const GI::Object *obj );

    void SetRgnPosX( GI::Object *obj, double x );

    void SetRgnPosY( GI::Object *obj, double y );
}

#endif
