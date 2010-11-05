///
/// @file BaseCellContainer.cpp
///
///
#include "BaseCellContainer.h"
#include "ObjVisitor.h"

BaseCellContainer::BaseCellContainer()
{
    m_usedSize = 0;
}

BaseCellContainer::~BaseCellContainer()
{
}

bool BaseCellContainer::Enable( long pos )
{
    if( pos < 0 || pos >= Size() ) return false;
    if( m_cells[pos].status == Cell::DISABLED )
    {
        m_cells[pos].status = Cell::EMPTY;
    }
    return true;
}

void BaseCellContainer::EnableAll()
{
    for( size_t i = 0; i < m_cells.size(); ++ i )
    {
        Enable( (long) i );
    }
}

bool BaseCellContainer::Disable( long pos )
{
    if( pos < 0 || pos >= Size() ) return false;
    if( m_cells[pos].status != Cell::DISABLED )
    {
        m_cells[pos].status = Cell::DISABLED;
    }
    return true;
}

void BaseCellContainer::DisableAll()
{
    for( size_t i = 0; i < m_cells.size(); ++ i )
    {
        Disable( (long) i );
    }
}

bool BaseCellContainer::ReSize( long size )
{
    if( size <= UsedSize() ) return false;
    m_cells.resize( size );
    return true;
}

const BaseCellContainer::Cell &BaseCellContainer::GetCell( long pos ) const
{
    static Cell nullCell;
    if( pos < 0 || pos >= Size() ) return nullCell;
    return m_cells[pos];
}

const GI::Object *BaseCellContainer::GetObjAtPos( long pos ) const
{
    const Cell &cell = GetCell( pos );
    if( cell.status != Cell::USED ) return NULL;
    return GetObject( cell.id );
}

bool BaseCellContainer::FillCell( long pos, const GI::Object *obj )
{
    if( pos < 0 || pos >= Size() ) return false;
    Cell &cell = m_cells[pos];
    if( cell.status == Cell::DISABLED ) return false;
    cell.status = Cell::USED;
    cell.id = ObjVisitor::ID( obj );
    ++ m_usedSize;
    return true;
}

bool BaseCellContainer::UnFillCell( long pos )
{
    if( pos < 0 || pos >= Size() ) return false;
    Cell &cell = m_cells[pos];
    if( cell.status != Cell::USED ) return false;
    cell.status = Cell::EMPTY;
    -- m_usedSize;
    return true;
}

bool BaseCellContainer::Add( GI::Object *obj )
{
    long pos = ObjVisitor::Pos( obj );
    if( pos < 0 || pos >= Size() ) return false;
    if( !FillCell( pos, obj ) ) return false;
    return GI::MergeContainer::Add( obj );
}

bool BaseCellContainer::Remove( GI::Object *obj )
{
    long pos = ObjVisitor::Pos( obj );
    if( pos < 0 || pos >= Size() ) return false;
    if( !UnFillCell( pos ) ) return false;
    return GI::MergeContainer::Remove( obj );
}

