///
/// @file CellContainer.h
///
///
#ifndef ___CELLCONTAINER_H_
#define ___CELLCONTAINER_H_

#include "BaseCellContainer.h"
#include "RefObject.h"

class CellContainer : public BaseCellContainer
{
public:
    CellContainer();

    virtual ~CellContainer();

    /// Destroy objects by 'rets'.
    void BatchDestroy( const DelRetListT *rets );

    /// Move objects in different container. The position is determined by
    /// 'this'.
    virtual bool Move( BaseContainer *srcCon, TypeSet::IDType objID );

    /// Move all the objects in 'srcCon' to this.
    virtual bool MoveAll( BaseContainer *srcCon );

    /// Move some count objects from 'srcCon'. The 'pos' must be empty.
    bool Move( BaseCellContainer *srcCon, TypeSet::IDType objID, 
            TypeSet::StackCntType cnt, long pos );  
    
    /// Merge the object in 'srcCon' to 'this'.(full merge or partial merge)
    /// The 'pos' must have the same type object which can be merged.
    bool Merge( BaseCellContainer *srcCon, TypeSet::IDType objID, 
            TypeSet::StackCntType cnt, long pos );    

    /// Swap the object in 'srcCon' to 'this'.
    bool Swap( BaseCellContainer *srcCon, TypeSet::IDType srcObj, TypeSet::IDType thisObj );

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

    /// Refill the cell information.
    bool ReFill();

protected:
    bool DoAdd( BaseContainer *con, const AddRet &ret );
};

#endif
