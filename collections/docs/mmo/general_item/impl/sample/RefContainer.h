///
/// @file RefContainer.h
/// 
///
#ifndef ___REF_CONTAINER_H_
#define ___REF_CONTAINER_H_

#include "CellContainer.h"
#include "RefObject.h"
#include <map>

class RefCellContainer;
class RefBaseContainer
{
public:
    typedef std::map<TypeSet::IDType, RefObject> RefObjectTableT;

public:

    void RefTo( const GI::BaseContainer *con );

    bool TestMoveAll( RefCellContainer *con, AddRetListT *rets );

    void Reset();

private:
    RefObjectTableT m_refObjs;
};

class RefCellContainer 
{
public:
    typedef std::multimap<TypeSet::IndexType, RefObject> RefObjectTableT;
    typedef RefObjectTableT::iterator RefIteratorT;

public:
    RefCellContainer();

    ~RefCellContainer();

    void RefTo( const CellContainer *con );

    bool Add( const RefObject &obj, AddRet *ret );

    void Reset();

    bool TestDel( TypeSet::IndexType index, long decCnt, DelRetListT *rets );
private:

    bool AddExist( const RefObject &obj, AddRet *ret );

    bool AddNew( const RefObject &obj, AddRet *ret );

    void AddNewObject( const RefObject &obj, long pos );
private:
    RefObjectTableT m_refObjs;
    long m_maxCellCnt;
    long m_usedCellCnt;
    CellContainer::CellListT m_cells;
};

#endif

