///
/// @file SubContainer.h
///
///
#ifndef ___SUBCONTAINER_H_
#define ___SUBCONTAINER_H_

#include "BaseCellContainer.h"

#define CAST_CELL(u) ((CellContainer*)(u))

class SubContainer : public BaseCellContainer
{
public:
    SubContainer();

    virtual ~SubContainer();

    virtual bool Enable( long pos );

    virtual bool Disable( long pos );

    bool Move( BaseContainer *srcCon, TypeSet::IDType objID, long pos );

    bool Swap( BaseContainer *srcCon, TypeSet::IDType srcObjID, TypeSet::IDType destObjID );

    bool Move( TypeSet::IDType objID, long pos );

    bool Swap( TypeSet::IDType srcObjID, TypeSet::IDType destObjID );

    bool CanSwap( const GI::Object *srcObj, const GI::Object *destObj );

    long SubObjCount( long pos ) const;

    BaseCellContainer *GetSubCon( long pos ) const;

protected:
    virtual bool FillCell( long pos, const GI::Object *obj );

    virtual bool UnFillCell( long pos );
};

#endif
