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

    void ResetCells();

    void BatchDestroy( const DelRetListT *rets );

    virtual bool Move( BaseContainer *srcCon, TypeSet::IDType objID );

    bool Move( TypeSet::IDType objID, long newPos );

    virtual bool MoveAll( BaseContainer *srcCon );

    virtual bool Destroy( TypeSet::IDType objID );

    virtual void DestroyAll();

    virtual void Serialize( GI::ByteBuffer &buf ) const;

    virtual bool UnSerialize( GI::ByteBuffer &buf );

    long GetMaxCellCnt() const { return m_maxCellCnt; }

    long GetUsedCellCnt() const { return m_usedCellCnt; }

    const CellListT &GetCells() const { return m_cells; }

protected:
    bool DoAdd( GI::Object *obj, const AddRet &ret );

    void FillCell( long pos, TypeSet::IDType id );

    void UnFillCell( long pos );

    int GetCellStatus( long pos );
    
    void SetObjectPos( GI::Object *obj, long pos );
protected:
    long m_maxCellCnt;
    long m_usedCellCnt;
    CellListT m_cells;
};

#endif

