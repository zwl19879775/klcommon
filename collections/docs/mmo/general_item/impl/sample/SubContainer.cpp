///
/// @file SubContainer.cpp
///
///
#include "SubContainer.h"
#include "CellContainer.h"
#include "ObjVisitor.h"

#define CAST_CELL(u) ((CellContainer*)(u))

SubContainer::SubContainer()
{
}

SubContainer::~SubContainer()
{
    DisableAll();
}

bool SubContainer::Enable( long pos )
{
    if( !BaseCellContainer::Enable( pos ) ) return false;
    m_cells[pos].u = new CellContainer();
    return true;
}

bool SubContainer::Disable( long pos )
{
    if( !BaseCellContainer::Disable( pos ) ) return false;
    delete CAST_CELL( m_cells[pos].u );
    m_cells[pos].u = NULL;
    return true;
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
    if( srcConSize < CAST_CELL( cell.u )->ObjCount() ) return false;
    AgentRemove( srcCon, srcObj );
    Remove( destObj ); // Will not change cell.u status.
    long srcPos = ObjVisitor::Pos( srcObj );
    ObjVisitor::SetPos( srcObj, destPos );
    ObjVisitor::SetPos( destObj, srcPos );
    AgentAdd( srcCon, destObj );
    Add( srcObj ); // Will change cell.u size: ReSize( srcConSize ).
    // And now i refill the container.
    CAST_CELL( cell.u )->ReFill();    
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
    // Cann't move container which already contains some objects.
    if( srcCell.status != Cell::USED || CAST_CELL( srcCell.u )->ObjCount() > 0 ) return false;
    ObjVisitor::SetPos( srcObj, destPos );
    ObjVisitor::SetPos( destObj, srcPos );
    FillCell( srcPos, destObj );
    FillCell( destPos, srcObj );
    Cell &destCell = m_cells[destPos];
    CAST_CELL( destCell.u )->ReFill();    
    return true;
}

bool SubContainer::Move( TypeSet::IDType objID, long pos )
{
    GI::Object *obj = Get( objID );
    if( !obj ) return false;
    if( GetCellStatus( pos ) != Cell::EMPTY ) return false;
    long srcPos = ObjVisitor::Pos( obj );
    const Cell &srcCell = GetCell( srcPos );
    if( CAST_CELL( srcCell.u )->ObjCount() > 0 ) return false;
    UnFillCell( srcPos );
    ObjVisitor::SetPos( obj, pos );
    FillCell( pos, obj );
    return true;
}

bool SubContainer::CanSwap( const GI::Object *srcObj, const GI::Object *destObj )
{
    if( !ObjVisitor::IsConObject( srcObj ) ) return false;
    long srcConSize = ObjVisitor::ConSize( srcObj );
    long pos = ObjVisitor::Pos( destObj );
    const Cell &cell = GetCell( pos );
    if( cell.status != Cell::USED ) return false;
    if( srcConSize >= CAST_CELL( cell.u )->ObjCount() ) return true;
    return false;
}

long SubContainer::SubObjCount( long pos ) const
{
    const Cell &cell = GetCell( pos );
    if( cell.status != Cell::USED ) return 0;
    return (long) CAST_CELL( cell.u )->ObjCount();
}

bool SubContainer::FillCell( long pos, const GI::Object *obj )
{
    if( !BaseCellContainer::FillCell( pos, obj ) ) return false;
    CAST_CELL( m_cells[pos].u )->ReSize( ObjVisitor::ConSize( obj ) );
    return true;
}

bool SubContainer::UnFillCell( long pos )
{
    if( !BaseCellContainer::UnFillCell( pos ) ) return false;
    /*CAST_CELL( m_cells[pos].u )->ReSize( 0 );*/
    return true;
}

