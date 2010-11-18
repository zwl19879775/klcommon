///
/// @file RefObject.cpp
///
///
#include "RefObject.h"
#include "GoodsPropertyType.h"
#include "ObjVisitor.h"

void RefObject::RefTo( const GI::Object *obj )
{
    RefToNoPos( obj );
    pos = ObjVisitor::Pos( obj );
}

void RefObject::RefToNoPos( const GI::Object *obj )
{
    id = ObjVisitor::ID( obj );
    index = ObjVisitor::Index( obj );
    maxStackCnt = ObjVisitor::MaxCount( obj );
    stackCnt = ObjVisitor::Count( obj );
    pos = INVALID_POS;
}

