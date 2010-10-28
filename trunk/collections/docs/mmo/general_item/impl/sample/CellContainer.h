///
/// @file CellContainer.h
/// 
///
#ifndef ___CELL_CONTAINER_H_
#define ___CELL_CONTAINER_H_

#include "../GIContainer.h"
#include "RefObject.h"
#include <vector>

class CellContainer : public GI::MergeContainer, public GI::SerialData
{
public:
    struct Cell
    {
        enum Status { INVALID, EMPTY, USED } status;
        TypeSet::IDType id;
        Cell() : status( EMPTY ), id( TypeSet::IDType() ) { }
    };
    typedef std::vector<Cell> CellListT;

public:
    CellContainer( long maxCell );

    virtual ~CellContainer();

    bool SetSize( long size );

    /// Destroy objects by 'rets'.
    void BatchDestroy( const DelRetListT *rets );

    /// Move objects in different container. The position is determined by
    /// 'this'.
    virtual bool Move( BaseContainer *srcCon, TypeSet::IDType objID );

    /// Move all the objects in 'srcCon' to this.
    virtual bool MoveAll( BaseContainer *srcCon );

    /// Move some count objects from 'srcCon'. The 'pos' must be empty.
    bool Move( MergeContainer *srcCon, TypeSet::IDType objID, 
            TypeSet::StackCntType cnt, long pos );  
    
    /// Merge the object in 'srcCon' to 'this'.(full merge or partial merge)
    /// The 'pos' must have the same type object which can be merged.
    bool Merge( MergeContainer *srcCon, TypeSet::IDType objID, 
            TypeSet::StackCntType cnt, long pos );    

    /// Swap the object in 'srcCon' to 'this'.
    bool Swap( BaseContainer *srcCon, TypeSet::IDType srcObj, TypeSet::IDType thisObj );

    /// Move objects in this container. The 'newPos' must be empty.
    bool Move( TypeSet::IDType objID, long newPos );

    /// Swap the two objects position.
    bool Swap( TypeSet::IDType objID1, TypeSet::IDType objID2 );

    /// Split objects and put the new object on 'pos'. The 'pos' must be empty.
    bool Split( TypeSet::IDType objID, TypeSet::StackCntType splitCnt, long pos );

    /// Merge two objects in 'this' container.
    virtual bool Merge( TypeSet::IDType obj1, TypeSet::IDType obj2 );

    /// Merge two objects and put it on the position specified.
    /// The 'pos' must have the same type object.(full merge or partial merge)
    bool Merge( TypeSet::IDType objID, TypeSet::StackCntType cnt, long pos );

    /// Destroy all the objects and reset the cells.
    virtual void DestroyAll();

    /// Serialize the cell information and all the objects.
    virtual void Serialize( GI::ByteBuffer &buf ) const;

    virtual bool UnSerialize( GI::ByteBuffer &buf );

    bool ReFill();

    long GetMaxCellCnt() const { return m_maxCellCnt; }

    long GetUsedCellCnt() const { return m_usedCellCnt; }

    const CellListT &GetCells() const { return m_cells; }

    /// Get the specified cell.
    const Cell &GetCell( long pos ) const;
    
    template <typename T>
    void TraverseCell( T fn );
protected:
    void ResetCells();
    
    bool DoAdd( GI::Object *obj, const AddRet &ret );

    void FillCell( long pos, TypeSet::IDType id );

    void UnFillCell( long pos );

    int GetCellStatus( long pos );
    
    /// The 'obj' must has correct CELL_POS property.
    virtual bool Add( GI::Object *obj );

    /// Remove the object and unfill the cell.
    virtual bool Remove( GI::Object *obj );

protected:
    long m_maxCellCnt;
    long m_usedCellCnt;
    CellListT m_cells;
};

template <typename T>
void CellContainer::TraverseCell( T fn )
{
    for( CellListT::const_iterator it = m_cells.begin();
            it != m_cells.end(); ++ it )
    {
        fn( (const Cell&) it );
    }
}

#endif

