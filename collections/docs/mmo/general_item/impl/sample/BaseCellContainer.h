///
/// @file BaseCellContainer.h
///
///
#ifndef ___BASECELLCONTAINER_H_
#define ___BASECELLCONTAINER_H_

#include "../GIContainer.h"
#include <vector>

class BaseCellContainer : public GI::MergeContainer, public GI::SerialData
{
public:
    struct Cell
    {
        enum Status { DISABLED, EMPTY, USED };
        int status;
        TypeSet::IDType id;
        void *u;
        Cell() : status( DISABLED ), id( TypeSet::IDType() ), u( 0 ) { }
    };
    typedef std::vector<Cell> CellListT;
public:
    BaseCellContainer();

    virtual ~BaseCellContainer();

    virtual bool Enable( long pos );

    void EnableAll();

    virtual bool Disable( long pos );

    void DisableAll();
    
    /// The new size('size') must greater than 'UsedSize'.
    bool ReSize( long size );

    long UsedSize() const { return m_usedSize; }

    /// Return the max size of this container.
    long Size() const { return (long) m_cells.size(); }

    /// Get the specified cell.
    const Cell &GetCell( long pos ) const;

    /// Get the specified object.
    const GI::Object *GetObjAtPos( long pos ) const;

    /// Get the whole cell list.
    const CellListT &GetCells() const { return m_cells; }

    /// Get the cell status.
    int GetCellStatus( long pos ) const { return GetCell( pos ).status; }

    /// Traverse all the cell. fn( const Cell& ).
    template <typename T>
    void TraverseCell( T fn );

protected:
    virtual bool FillCell( long pos, const GI::Object *obj );

    virtual bool UnFillCell( long pos );

    /// Be sure 'obj' has PCELL_POS property.
    virtual bool Add( GI::Object *obj );

    virtual bool Remove( GI::Object *obj );

protected:
    CellListT m_cells;
    long m_usedSize;
};

template <typename T>
void BaseCellContainer::TraverseCell( T fn )
{
    for( CellListT::const_iterator it = m_cells.begin();
            it != m_cells.end(); ++ it )
        fn( (const Cell&) *it );
}

#endif

