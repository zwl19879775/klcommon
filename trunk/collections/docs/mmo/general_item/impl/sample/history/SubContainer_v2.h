///
/// @file SubContainer.h
///
///
#ifndef ___SUBCONTAINER_H_
#define ___SUBCONTAINER_H_

#include "../GIContainer.h"
#include <vector>

class CellContainer;
class SubContainer : public GI::BaseContainer, public GI::SerialData
{
public:
    struct Cell
    {
        enum { INVALID, EMPTY, USED };
        TypeSet::IDType id;
        int status;
        CellContainer *con;
        Cell() : status( INVALID ), con( 0 ) { }
    };
    typedef std::vector<Cell> CellListT;
public:
    SubContainer();

    virtual ~SubContainer();

    void SetSize( long size );

    long GetSize() const;

    void Enable( long pos );

    void EnableAll();

    void Disable( long pos );

    void DisableAll();

    bool Move( BaseContainer *srcCon, TypeSet::IDType objID, long pos );

    bool Swap( BaseContainer *srcCon, TypeSet::IDType srcObjID, TypeSet::IDType destObjID );

    bool Move( TypeSet::IDType objID, long pos );

    bool Swap( TypeSet::IDType srcObjID, TypeSet::IDType destObjID );

    const Cell &GetCell( long pos ) const;

    int GetCellStatus( long pos ) const { return GetCell( pos ).status; }

    bool CanSwap( const GI::Object *srcObj, const GI::Object *destObj );
protected:
    virtual bool Add( GI::Object *obj );

    virtual bool Remove( GI::Object *obj );

    void FillCell( long pos, const GI::Object *obj, long conSize );

    void UnFillCell( long pos );
protected:
    CellListT m_cells;
};

#endif

