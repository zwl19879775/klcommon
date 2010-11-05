///
/// @file CellContainer.cpp
///
///
#include "CellContainer.h"
#include "RefContainer.h"
#include "GoodsPropertyType.h"


CellContainer::CellContainer( long maxCell )
{
    m_maxCellCnt = maxCell;
    m_usedCellCnt = 0;
    m_cells.resize( maxCell );
}

CellContainer::~CellContainer()
{
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

bool CellContainer::Move( TypeSet::IDType objID, long newPos )
{
    int status = GetCellStatus( newPos );
    if( status != Cell::EMPTY ) return false;  
    GI::Object *obj = Get( objID );
    if( !obj ) return false;
    long pos = TypeSet::ValueType::ToLong( obj->GetValue( TypeSet::KeyType( PCELL_POS ) ) );
    UnFillCell( pos );
    SetObjectPos( obj, newPos ); 
    FillCell( newPos, objID );
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

bool CellContainer::Destroy( TypeSet::IDType objID )
{
    GI::Object *obj = Get( objID );
    long pos = TypeSet::ValueType::ToLong( obj->GetValue( TypeSet::KeyType( PCELL_POS ) ) );
    UnFillCell( pos );
    return Destroy( objID );
}

void CellContainer::DestroyAll()
{
    MergeContainer::DestroyAll();
    ResetCells();
}

bool CellContainer::DoAdd( GI::Object *obj, const AddRet &ret )
{
    SetObjectPos( obj, ret.pos );
    if( ret.op == AddRet::NEW )
    {
        FillCell( ret.pos, ret.id );
        return Add( obj );
    }
    else
    {
        Add( obj ); // will be destroyed soon.
        const Cell &cell = m_cells[ret.pos];
        Merge( cell.id, ret.id );
    }
    return true;
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
    if( pos < 0 || pos >= (long) m_cells.size() ) return Cell::INVALID;
    return m_cells[pos].status;
}

void CellContainer::SetObjectPos( GI::Object *obj, long pos )
{
    TypeSet::KeyType posKey( PCELL_POS );
    if( !obj->HasProperty( posKey ) )
    {
        obj->AddProperty( posKey, TypeSet::ValueType( pos ) );
    }
    else
    {
        obj->SetValue( posKey, TypeSet::ValueType( pos ) );
    }
}

