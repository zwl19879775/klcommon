///
/// @file RgnContainer.cpp
///
///
#include "RgnContainer.h"
#include "ObjVisitor.h"
#include "GoodsPropertyType.h"
#include "../GIObjectProto.h"

void RgnContainer::SetStackCntPos::operator() ( GI::Object *obj )
{
    ObjVisitor::SetRgnPosX( obj, m_pos.x );
    ObjVisitor::SetRgnPosY( obj, m_pos.y );
    ObjVisitor::SetCount( obj, m_cnt );
}

RgnContainer::RgnContainer()
{
}

RgnContainer::~RgnContainer()
{
}

bool RgnContainer::Move( GI::MergeContainer *srcCon, TypeSet::IDType objID, GoodsPos pos, long cnt )
{
    GI::Object *srcObj = AgentGet( srcCon, objID );
    if( !srcObj ) return false;
    long srcCnt = ObjVisitor::Count( srcObj );
    if( srcCnt < cnt ) return false;
    if( srcCnt == cnt ) // full move
    {
        ObjVisitor::SetRgnPosX( srcObj, pos.x );
        ObjVisitor::SetRgnPosY( srcObj, pos.y );
        AgentRemove( srcCon, srcObj );
        Add( srcObj );
    }
    else // partial move
    {
        GI::Object *newObj = new GI::Object( NULL );
        srcObj->Clone( newObj );
        srcCon->DecStack( objID, cnt );
        ObjVisitor::SetRgnPosX( srcObj, pos.x );
        ObjVisitor::SetRgnPosY( srcObj, pos.y );
        Add( newObj );
    }
    return true;
}

bool RgnContainer::Remove( GI::Object *obj )
{
    obj->RemoveProperty( TypeSet::KeyType( PRGNX ) );
    obj->RemoveProperty( TypeSet::KeyType( PRGNY ) );
    return GI::FactoryContainer::Remove( obj );
}

