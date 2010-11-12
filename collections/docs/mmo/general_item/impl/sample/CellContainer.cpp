///
/// @file CellContainer.cpp
///
///
#include "CellContainer.h"
#include "RefContainer.h"
#include "ObjVisitor.h"
#include "../GIObjCreator.h"

CellContainer::CellContainer()
{
}

CellContainer::~CellContainer()
{
}

void CellContainer::BatchDestroy( const DelRetListT *rets )
{
    for( DelRetListT::const_iterator it = rets->begin();
            it != rets->end(); ++ it )
    {
        const DelRet &delRet = *it;
        const Cell &cell = GetCell( delRet.pos );
        if( cell.status != Cell::USED ) continue;
        if( delRet.op == DelRet::DEL )
        {
            Destroy( cell.id );
        }
        else
        {
            DecStack( cell.id, delRet.dec );
        }
    }
}

bool CellContainer::Move( BaseContainer *srcCon, TypeSet::IDType objID )
{
    GI::Object *obj = AgentGet( srcCon, objID );
    if( !obj ) return false;
    RefCellContainer refCellCon;
    refCellCon.RefTo( this );
    RefObject refObj;
    refObj.RefTo( obj );
    AddRet ret;
    if( !refCellCon.Add( refObj, &ret ) ) return false;
    if( !DoAdd( obj, ret ) ) return false;
    AgentRemove( srcCon, obj );
    NOTIFY_LISTENER( OnMoved( srcCon, this, obj ) );
    return true; 
}

bool CellContainer::MoveAll( BaseContainer *srcCon )
{
    RefCellContainer refCellCon;
    RefBaseContainer refBaseCon;
    AddRetListT addRets;
    refBaseCon.RefTo( srcCon );
    refCellCon.RefTo( this );
    if( !refBaseCon.TestMoveAll( &refCellCon, &addRets ) ) return false;
    for( AddRetListT::const_iterator it = addRets.begin();
            it != addRets.end(); ++ it )
    {
        const AddRet &ret = *it;
        GI::Object *obj = AgentGet( srcCon, ret.id );
        DoAdd( obj, ret );
        AgentRemove( srcCon, obj );
        NOTIFY_LISTENER( OnMoved( srcCon, this, obj ) );
    }
    return true;
}

bool CellContainer::Move( BaseCellContainer *srcCon, TypeSet::IDType objID, 
        TypeSet::StackCntType cnt, long pos )
{
    if( GetCellStatus( pos ) != Cell::EMPTY ) return false;
    GI::Object *obj = AgentGet( srcCon, objID );
    if( !obj ) return false;
    long maxCnt = (long) ObjVisitor::Count( obj );
    if( cnt > maxCnt ) return false;
    if( cnt == maxCnt ) // full move
    {
        AgentRemove( srcCon, obj );
        ObjVisitor::SetPos( obj, pos );
        Add( obj );
        NOTIFY_LISTENER( OnMoved( srcCon, this, obj ) );
    }
    else // partial move(split)
    {
        srcCon->DecStack( objID, cnt );
        GI::Object *newObj = SINGLETON( GI::ObjCreator ).Clone( obj );
        ObjVisitor::SetCount( newObj, cnt );
        ObjVisitor::SetPos( newObj, pos );
        Add( newObj );
    }
    return true;
}

bool CellContainer::Merge( BaseCellContainer *srcCon, TypeSet::IDType objID, 
        TypeSet::StackCntType cnt, long pos )
{
    const GI::Object *obj = srcCon->GetObject( objID );
    if( !obj ) return false;
    const Cell &cell = GetCell( pos );
    if( cell.status != Cell::USED ) return false;
    GI::Object *thisObj = Get( cell.id );
    if( !ObjVisitor::IsSameIndex( obj, thisObj ) ) return false;
    long srcCnt = ObjVisitor::Count( obj );
    long thisCnt = ObjVisitor::Count( thisObj );
    long maxCnt = ObjVisitor::MaxCount( thisObj );
    if( cnt > srcCnt || thisCnt + cnt > maxCnt ) return false;
    ObjVisitor::SetCount( thisObj, thisCnt + cnt );
    if( cnt == srcCnt ) // full merge
    {
        srcCon->Destroy( objID );
    }
    else // partial merge
    {
        srcCon->DecStack( objID, cnt );
    }
    return true;
}

bool CellContainer::Swap( BaseCellContainer *srcCon, TypeSet::IDType srcObjID, TypeSet::IDType thisObjID ) 
{
    GI::Object *srcObj = AgentGet( srcCon, srcObjID );
    GI::Object *thisObj = Get( thisObjID );
    if( !srcObj || !thisObj ) return false;
    AgentRemove( srcCon, srcObj );
    Remove( thisObj );
    long srcPos = ObjVisitor::Pos( srcObj );
    long thisPos = ObjVisitor::Pos( thisObj );
    ObjVisitor::SetPos( srcObj, thisPos );
    ObjVisitor::SetPos( thisObj, srcPos );
    AgentAdd( srcCon, thisObj );
    Add( srcObj );
    return true;
}

bool CellContainer::Move( TypeSet::IDType objID, long newPos )
{
    int status = GetCellStatus( newPos );
    if( status != Cell::EMPTY ) return false;  
    GI::Object *obj = Get( objID );
    if( !obj ) return false;
    long pos = ObjVisitor::Pos( obj ); 
    UnFillCell( pos );
    ObjVisitor::SetPos( obj, newPos );
    FillCell( newPos, obj );
    NOTIFY_LISTENER( OnMoved( this, this, obj ) );
    return true;
}

bool CellContainer::Swap( TypeSet::IDType objID1, TypeSet::IDType objID2 )
{
    GI::Object *obj1 = Get( objID1 );
    GI::Object *obj2 = Get( objID2 );
    if( !obj1 || !obj2 ) return false;
    long pos1 = ObjVisitor::Pos( obj1 );
    long pos2 = ObjVisitor::Pos( obj2 );
    ObjVisitor::SetPos( obj1, pos2 );
    ObjVisitor::SetPos( obj2, pos1 );
    FillCell( pos1, obj2 );
    FillCell( pos2, obj1 );
    return true;
}

bool CellContainer::Split( TypeSet::IDType objID, TypeSet::StackCntType splitCnt, long pos )
{
    if( GetCellStatus( pos ) != Cell::EMPTY ) return false;
    GI::Object *obj = Get( objID );
    if( !obj ) return false;
    TypeSet::StackCntType curCnt = GetStackCnt( obj );
    if( splitCnt >= curCnt ) return false;

    TypeSet::StackCntType retCnt = curCnt - splitCnt;
    obj->SetValue( KeySet::StackCntKey, TypeSet::ValueType( retCnt ) );
    GI::Object *newObj = SINGLETON( GI::ObjCreator ).Clone( obj );
    ObjVisitor::SetCount( newObj, splitCnt );
    ObjVisitor::SetPos( newObj, pos );
    Add( newObj );
    return true;
}

bool CellContainer::Merge( TypeSet::IDType obj1, TypeSet::IDType obj2 )
{
    return BaseCellContainer::Merge( obj1, obj2 );
}

bool CellContainer::Merge( TypeSet::IDType objID, TypeSet::StackCntType cnt, long pos )
{
    GI::Object *srcObj = Get( objID );
    if( !srcObj ) return false;
    const Cell &cell = GetCell( pos );
    if( cell.status != Cell::USED ) return false;
    GI::Object *destObj = Get( cell.id );
    if( !destObj ) return false;
    if( !ObjVisitor::IsSameIndex( srcObj, destObj ) ) return false;
    long srcCnt = ObjVisitor::Count( srcObj );
    long destCnt = ObjVisitor::Count( destObj );
    long maxCnt = ObjVisitor::Count( destObj );
    if( cnt > srcCnt || cnt + destCnt > maxCnt ) return false;
    ObjVisitor::SetCount( destObj, destCnt + cnt );
    if( cnt == srcCnt )
    {
        Destroy( objID );
    }
    else
    {
        ObjVisitor::SetCount( srcObj, srcCnt - cnt );
    }
    return true;
}

void CellContainer::DestroyAll()
{
    BaseCellContainer::DestroyAll();
}

bool CellContainer::DoAdd( GI::Object *obj, const AddRet &ret )
{
    ObjVisitor::SetPos( obj, ret.pos );
    if( ret.op == AddRet::NEW )
    {
        return Add( obj );
    }
    else
    {
        GI::MergeContainer::Add( obj ); 
        const Cell &cell = GetCell( ret.pos );
        Merge( cell.id, ret.id );
    }
    return true;
}

bool CellContainer::ReFill()
{
    if( ObjCount() > Size() ) return false;
    long pos = 0;
    for( ObjectMap::iterator it = m_objs.begin();
            it != m_objs.end(); ++ it, ++pos )
    {
        GI::Object *obj = it->second;
        ObjVisitor::SetPos( obj, pos );
        FillCell( pos, obj );
    }
    return true;
}


