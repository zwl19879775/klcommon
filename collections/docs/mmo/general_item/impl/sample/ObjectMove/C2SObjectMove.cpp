///
/// @file C2SObjectMove.cpp
/// @author Kevin Lynx
///
#include "C2SObjectMove.h"
#include "../BaseCellContainer.h"
#include "../ObjVisitor.h"

MoveOperator::MoveOperator()
{
    m_info = NULL;
}

int MoveOperator::GetCellOperType()
{
    if( m_info->src.ownerType != TYPE_PLAYER ||
        m_info->dest.ownerType != TYPE_PLAYER ) return NONE;
    BaseCellContainer *destCon = (BaseCellContainer*) GetDestCon();
    long destPos = m_info->dest.conPos;
    const BaseCellContainer::Cell &cell = destCon->GetCell( destPos );
    if( cell.status == BaseCellContainer::Cell::DISABLED ) return NONE;
    long operCnt = m_info->obj.cnt;
    BaseCellContainer *srcCon = (BaseCellContainer*) GetSrcCon();
    const GI::Object *srcObj = srcCon->GetObject( m_info->obj.id );
    long srcCnt = ObjVisitor::Count( srcObj );
    if( cell.status == BaseCellContainer::Cell::EMPTY ) 
    {
        if( operCnt < srcCnt ) return SPLIT;
        return MOVE;
    }
    const GI::Object *destObj = destCon->GetObject( cell.id );
    if( !srcObj || !destObj ) return NONE;
    if( !ObjVisitor::IsSameIndex( srcObj, destObj ) ) return SWAP;
    long maxCnt = ObjVisitor::MaxCount( destObj );
    long destCnt = ObjVisitor::Count( destObj );
    if( operCnt + destCnt > maxCnt ) return SWAP;
    if( srcCnt > operCnt ) return PARTIAL_MERGE;
    
    return MERGE;
}

bool MoveOperator::IsSameOwner()
{
    return m_info->src.ownerID == m_info->dest.ownerID;
}

bool MoveOperator::IsSameCon()
{
    return m_info->src.conID == m_info->dest.conID;
}

GI::BaseContainer *MoveOperator::GetSrcCon()
{
    // TODO:
    return NULL;
}

GI::BaseContainer *MoveOperator::GetDestCon()
{
    // TODO:
    return NULL;
}

///////////////////////////////////////////////////////////////////////////////
C2SObjectMoveDispatcher::C2SObjectMoveDispatcher()
{
}

C2SObjectMoveDispatcher::~C2SObjectMoveDispatcher()
{
}

void C2SObjectMoveDispatcher::Register( Identifier id, MoveOperator *op ) 
{
    m_funcs[id] = op;
}

bool C2SObjectMoveDispatcher::Run( const C2SObjectMoveInfo &info )
{
    Identifier id = ToId( info ); 
    HandlerTableT::const_iterator it = m_funcs.find( id );
    if( it == m_funcs.end() ) return false;
    MoveOperator *op = it->second;
    op->Setup( &info );
    if( !op->Check() ) return false;
    return op->Move();
}

C2SObjectMoveDispatcher::Identifier C2SObjectMoveDispatcher::ToId( long srcType, 
        long srcCon, long destType, long destCon )
{
    Identifier id = ( srcType << 24 ) | ( srcCon << 16 ) | 
        ( destType << 8 ) | destCon;
    return id;
}

C2SObjectMoveDispatcher::Identifier C2SObjectMoveDispatcher::ToId( 
        const C2SObjectMoveInfo &info )
{
    return ToId( info.src.ownerType, info.src.conID, 
            info.dest.ownerType, info.dest.conID );
}

