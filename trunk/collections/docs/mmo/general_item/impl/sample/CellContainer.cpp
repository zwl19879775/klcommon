///
/// @file CellContainer.cpp
///
///
#include "CellContainer.h"
#include "RefContainer.h"
#include "GoodsPropertyType.h"
#include "ObjVisitor.h"

CellContainer::CellContainer( long maxCell )
{
    m_usedCellCnt = 0;
    SetSize( maxCell );
    ResetCells();
}

CellContainer::~CellContainer()
{
}

bool CellContainer::SetSize( long size )
{
    if( size < m_usedCellCnt ) return false;
    m_cells.resize( size );
    m_maxCellCnt = size;
    return true;
}

void CellContainer::ResetCells()
{
    for( CellListT::iterator it = m_cells.begin();
            it != m_cells.end(); ++ it )
    {
        Cell &cell = *it;
        cell.status = Cell::EMPTY;
    }
    m_usedCellCnt = 0;
}

void CellContainer::BatchDestroy( const DelRetListT *rets )
{
    for( DelRetListT::const_iterator it = rets->begin();
            it != rets->end(); ++ it )
    {
        const DelRet &delRet = *it;
        TypeSet::IDType id = m_cells[delRet.pos].id;
        if( delRet.op == DelRet::DEL )
        {
            Destroy( id );
        }
        else
        {
            DecStack( id, delRet.dec );
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
    return true; 
}

bool CellContainer::Move( MergeContainer *srcCon, TypeSet::IDType objID, 
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
    }
    else // partial move(split)
    {
        srcCon->DecStack( objID, cnt );
        GI::Object *newObj = new GI::Object( NULL );
        obj->Clone( newObj );
        NOTIFY_LISTENER( OnCreate( newObj ) );
        ObjVisitor::SetCount( newObj, cnt );
        ObjVisitor::SetPos( newObj, pos );
        Add( newObj );
    }
    return true;
}

bool CellContainer::Merge( MergeContainer *srcCon, TypeSet::IDType objID, 
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
    NOTIFY_LISTENER( OnModify( thisObj, KeySet::StackCntKey, TypeSet::ValueType( thisCnt + cnt ) ) );
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

bool CellContainer::Swap( GI::BaseContainer *srcCon, TypeSet::IDType srcObjID, TypeSet::IDType thisObjID ) 
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
    FillCell( newPos, objID );
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
    FillCell( pos1, objID2 );
    FillCell( pos2, objID1 );
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
    }
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
    NOTIFY_LISTENER( OnModify( obj, KeySet::StackCntKey, TypeSet::ValueType( retCnt ) ) );
    obj->SetValue( KeySet::StackCntKey, TypeSet::ValueType( retCnt ) );
    GI::Object *newObj = new GI::Object( NULL );
    obj->Clone( newObj );
    NOTIFY_LISTENER( OnCreate( newObj ) );
    newObj->SetValue( KeySet::StackCntKey, TypeSet::ValueType( splitCnt ) );
    ObjVisitor::SetPos( newObj, pos );
    Add( newObj );
    return true;
}

bool CellContainer::Merge( TypeSet::IDType obj1, TypeSet::IDType obj2 )
{
    return GI::MergeContainer::Merge( obj1, obj2 );
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
    NOTIFY_LISTENER( OnModify( destObj, KeySet::StackCntKey, TypeSet::ValueType( destCnt + cnt ) ) );
    ObjVisitor::SetCount( destObj, destCnt + cnt );
    if( cnt == srcCnt )
    {
        Destroy( objID );
    }
    else
    {
        NOTIFY_LISTENER( OnModify( srcObj, KeySet::StackCntKey, TypeSet::ValueType( srcCnt - cnt ) ) );
        ObjVisitor::SetCount( srcObj, srcCnt - cnt );
    }
    return true;
}

void CellContainer::DestroyAll()
{
    MergeContainer::DestroyAll();
    ResetCells();
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
        const Cell &cell = m_cells[ret.pos];
        Merge( cell.id, ret.id );
    }
    return true;
}

bool CellContainer::Add( GI::Object *obj )
{
    long pos = ObjVisitor::Pos( obj );
    if( pos < 0 ) return false;
    FillCell( pos, ObjVisitor::ID( obj ) );
    return GI::MergeContainer::Add( obj );
}

bool CellContainer::Remove( GI::Object *obj )
{
    long pos = ObjVisitor::Pos( obj );
    if( pos < 0 ) return false;
    UnFillCell( pos );
    return GI::MergeContainer::Remove( obj );
}

void CellContainer::FillCell( long pos, TypeSet::IDType id )
{
    Cell &cell = m_cells[pos];
    cell.status = Cell::USED;
    cell.id = id;
    ++ m_usedCellCnt;
}

void CellContainer::UnFillCell( long pos )
{
    m_cells[pos].status = Cell::EMPTY;
    -- m_usedCellCnt;
}

int CellContainer::GetCellStatus( long pos )
{
    return GetCell( pos ).status;
}

const CellContainer::Cell &CellContainer::GetCell( long pos ) const
{
    static Cell nullCell; nullCell.status = Cell::INVALID;
    if( pos < 0 || pos > (long) m_cells.size() ) return nullCell;
    return m_cells[pos];
}

bool CellContainer::ReFill()
{
    if( ObjCount() > GetMaxCellCnt() ) return false;
    ResetCells();
    long pos = 0;
    for( ObjectMap::iterator it = m_objs.begin();
            it != m_objs.end(); ++ it, ++pos )
    {
        GI::Object *obj = it->second;
        ObjVisitor::SetPos( obj, pos );
        FillCell( pos, it->first );
    }
    return true;
}

