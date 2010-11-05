///
/// @file SubContainer.cpp
///
///
#include "SubContainer.h"
#include "CellContainer.h"
#include "ObjVisitor.h"

SubContainer::SubContainer()
{
}

SubContainer::~SubContainer()
{
    DisableAll();
}

void SubContainer::SetSize( long size )
{
    m_cells.resize( size );
}

long SubContainer::GetSize() const
{
    return (long) m_cells.size();
}

void SubContainer::Enable( long pos )
{
    if( pos < 0 || pos >= GetSize() ) return;
    if( m_cells[pos].status == Cell::INVALID )
    {
        m_cells[pos].status = Cell::EMPTY;
        m_cells[pos].con = new CellContainer( 0 );
    }
}

void SubContainer::EnableAll()
{
    for( size_t i = 0; i < m_cells.size(); ++ i )
    {
        Enable( (long) i );
    }
}

void SubContainer::Disable( long pos )
{
    if( pos < 0 || pos >= GetSize() ) return;
    if( m_cells[pos].status != Cell::INVALID )
    {
        m_cells[pos].status = Cell::INVALID;
        delete m_cells[pos].con;
        m_cells[pos].con = NULL;
    }
}

void SubContainer::DisableAll()
{
    for( size_t i = 0; i < m_cells.size(); ++ i )
    {
        Disable( (long) i );
    }
}

bool SubContainer::Move( BaseContainer *srcCon, TypeSet::IDType objID, long pos )
{
    if( GetCellStatus( pos ) != Cell::EMPTY ) return false;
    GI::Object *obj = AgentGet( srcCon, objID );
    if( !obj || !ObjVisitor::IsConObject( obj ) ) return false;
    AgentRemove( srcCon, obj );
    ObjVisitor::SetPos( obj, pos );
    if( !Add( obj ) ) return false;
    return true;
}

bool SubContainer::Swap( BaseContainer *srcCon, TypeSet::IDType srcObjID, 
        TypeSet::IDType destObjID )
{
    GI::Object *srcObj = AgentGet( srcCon, srcObjID );
    if( !srcObj || !ObjVisitor::IsConObject( srcObj ) ) return false;
    GI::Object *destObj = Get( destObjID );
    if( !destObj ) return false;
    long destPos = ObjVisitor::Pos( destObj );
    if( GetCellStatus( destPos ) != Cell::USED ) return false;
    Cell &cell = m_cells[destPos];
    long srcConSize = ObjVisitor::ConSize( srcObj );
    if( srcConSize < cell.con->ObjCount() ) return false;
    cell.con->SetSize( srcConSize );
    cell.con->ReFill();    
    AgentRemove( srcCon, srcObj );
    Remove( destObj );
    long srcPos = ObjVisitor::Pos( srcObj );
    ObjVisitor::SetPos( srcObj, destPos );
    ObjVisitor::SetPos( destObj, srcPos );
    AgentAdd( srcCon, destObj );
    Add( srcObj );
    return true;
}

bool SubContainer::Swap( TypeSet::IDType srcObjID, TypeSet::IDType destObjID )
{
    GI::Object *srcObj = Get( srcObjID );
    if( !srcObj ) return false;
    GI::Object *destObj = Get( destObjID );
    if( !destObj ) return false;
    if( !CanSwap( srcObj, destObj ) ) return false;
    long destPos = ObjVisitor::Pos( destObj );
    long srcPos = ObjVisitor::Pos( srcObj );
    const Cell &srcCell = GetCell( srcPos );
    if( srcCell.status != Cell::USED || srcCell.con->ObjCount() > 0 ) return false;
    ObjVisitor::SetPos( srcObj, destPos );
    ObjVisitor::SetPos( destObj, srcPos );
    FillCell( srcPos, destObj, ObjVisitor::ConSize( destObj ) );
    FillCell( destPos, srcObj, ObjVisitor::ConSize( srcObj ) );
    Cell &destCell = m_cells[destPos];
    destCell.con->ReFill();    
    return true;
}

bool SubContainer::Move( TypeSet::IDType objID, long pos )
{
    GI::Object *obj = Get( objID );
    if( !obj ) return false;
    if( GetCellStatus( pos ) != Cell::EMPTY ) return false;
    long srcPos = ObjVisitor::Pos( obj );
    UnFillCell( srcPos );
    ObjVisitor::SetPos( obj, pos );
    FillCell( pos, obj, ObjVisitor::ConSize( obj ) );
    return true;
}

bool SubContainer::CanSwap( const GI::Object *srcObj, const GI::Object *destObj )
{
    if( !ObjVisitor::IsConObject( srcObj ) ) return false;
    long srcConSize = ObjVisitor::ConSize( srcObj );
    long pos = ObjVisitor::Pos( destObj );
    const Cell &cell = GetCell( pos );
    if( cell.status != Cell::USED ) return false;
    if( srcConSize >= cell.con->ObjCount() ) return true;
    return false;
}

const SubContainer::Cell &SubContainer::GetCell( long pos ) const
{
    static Cell nullCell;
    if( pos < 0 || pos >= GetSize() ) return nullCell;
    return m_cells[pos];
}

bool SubContainer::Add( GI::Object *obj )
{
    long pos = ObjVisitor::Pos( obj );
    if( pos < 0 || pos >= GetSize() ) return false;
    if( !ObjVisitor::IsConObject( obj ) ) return false;
    m_cells[pos].status = Cell::USED;
    m_cells[pos].id = ObjVisitor::ID( obj );
    return GI::BaseContainer::Add( obj );
}

bool SubContainer::Remove( GI::Object *obj )
{
    long pos = ObjVisitor::Pos( obj );
    if( pos < 0 || pos >= GetSize() ) return false;
    m_cells[pos].status = Cell::EMPTY;
    return GI::BaseContainer::Remove( obj );
}

void SubContainer::FillCell( long pos, const GI::Object *obj, long conSize )
{
    if( pos < 0 || pos >= GetSize() ) return;
    Cell &cell = m_cells[pos];
    if( cell.status == Cell::INVALID ) return;
    cell.status = Cell::USED;
    cell.con->SetSize( conSize );
    cell.id = ObjVisitor::ID( obj );
}

void SubContainer::UnFillCell( long pos )
{
    if( pos < 0 || pos >= GetSize() ) return;
    Cell &cell = m_cells[pos];
    if( cell.status == Cell::INVALID ) return;
    cell.status = Cell::EMPTY;
    cell.con->SetSize( 0 );
}

