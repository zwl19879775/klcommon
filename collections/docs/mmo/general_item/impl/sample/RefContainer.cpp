///
/// @file RefContainer.cpp
///
///
#include "RefContainer.h"
#include "GoodsPropertyType.h"
#include <assert.h>

static RefObject &CollectRefObj( RefObject &refObj, TypeSet::IDType id, const GI::Object *obj )
{
    refObj.id = id;
    TypeSet::ValueType indexVal = obj->GetValue( KeySet::IndexKey );
    refObj.index = TypeSet::ValueType::ToLong( indexVal );
    TypeSet::ValueType maxStackVal = obj->GetValue( KeySet::MaxStackCntKey );
    refObj.maxStackCnt = TypeSet::ValueType::ToStackCnt( maxStackVal );
    TypeSet::ValueType stackCntVal = obj->GetValue( KeySet::StackCntKey );
    refObj.stackCnt = TypeSet::ValueType::ToStackCnt( stackCntVal );
    TypeSet::ValueType posVal = obj->GetValue( TypeSet::KeyType( PCELL_POS ) );
    assert( posVal.Valid() );
    refObj.pos = TypeSet::ValueType::ToLong( posVal );
    return refObj;
}

struct RefCellCollector
{
    RefCellCollector( RefCellContainer::RefObjectTableT &refs ) : m_refs( refs ) { }

    void operator() ( TypeSet::IDType id, const GI::Object *obj )
    {
        RefObject refObj;
        CollectRefObj( refObj, id, obj );
        m_refs.insert( std::make_pair( refObj.index, refObj ) );
    }

    RefCellContainer::RefObjectTableT &m_refs;
};

struct RefBaseCollector
{
    RefBaseCollector( RefBaseContainer::RefObjectTableT &refs ) : m_refs( refs ) { }

    void operator() ( TypeSet::IDType id, const GI::Object *obj )
    {
        RefObject refObj;
        CollectRefObj( refObj, id, obj );
        m_refs.insert( std::make_pair( id, refObj ) );
    }

    RefBaseContainer::RefObjectTableT &m_refs;
};

void RefBaseContainer::RefTo( const GI::BaseContainer *con )
{
    con->Traverse( RefBaseCollector( m_refObjs ) );
}

bool RefBaseContainer::TestMoveAll( RefCellContainer *con, AddRetListT *rets )
{
    AddRet addRet;
    for( RefObjectTableT::iterator it = m_refObjs.begin();
            it != m_refObjs.end(); ++ it )
    {
        if( !con->Add( it->second, &addRet ) ) return false;
        rets->push_back( addRet );
    }
    return true;
}

void RefBaseContainer::Reset()
{
    m_refObjs.clear();
}

///////////////////////////////////////////////////////////////////////////////////////
RefCellContainer::RefCellContainer()
{
    m_maxCellCnt = m_usedCellCnt = 0;
}

RefCellContainer::~RefCellContainer()
{
}

void RefCellContainer::RefTo( const CellContainer *con )
{
    con->Traverse( RefCellCollector( m_refObjs ) );
    m_maxCellCnt = con->GetMaxCellCnt();
    m_usedCellCnt = con->GetUsedCellCnt();
    m_cells = con->GetCells();
}

bool RefCellContainer::TestDel( TypeSet::IndexType index, long decCnt, DelRetListT *rets )
{
    std::pair<RefIteratorT, RefIteratorT> pos;
    pos = m_refObjs.equal_range( index );
    DelRet delRet;
    for( ; pos.first != pos.second && decCnt > 0; ++ pos.first )
    {
        RefObject &refObj = pos.first->second; 
        if( refObj.stackCnt > decCnt )
        {
            decCnt = 0;
            delRet.pos = refObj.pos;
            delRet.dec = decCnt;
            delRet.op = DelRet::CHG_STACKCNT;
            rets->push_back( delRet );
        }
        else
        {
            decCnt -= refObj.stackCnt;
            delRet.pos = refObj.pos;
            delRet.dec = refObj.stackCnt;
            delRet.op = DelRet::DEL;
            rets->push_back( delRet );
        }
    }
    return decCnt == 0;
}

void RefCellContainer::Reset()
{
    m_maxCellCnt = 0;
    m_usedCellCnt = 0;
    m_cells.clear();
    m_refObjs.clear();
}

bool RefCellContainer::Add( const RefObject &obj, AddRet *ret )
{
    if( AddExist( obj, ret ) )
    {
        return true;
    }
    return AddNew( obj, ret );
}

bool RefCellContainer::AddExist( const RefObject &obj, AddRet *ret )
{
    std::pair<RefIteratorT, RefIteratorT> pos;
    pos = m_refObjs.equal_range( obj.index );
    for( ; pos.first != pos.second; ++ pos.first )
    {
        RefObject &existObj = pos.first->second;
        if( existObj.stackCnt + obj.stackCnt <= existObj.maxStackCnt )
        {
            // merge them.
            existObj.stackCnt += obj.stackCnt;
            ret->id = obj.id;
            ret->op = AddRet::CHG_STACKCNT; 
            ret->pos = existObj.pos;
            return true;
        }
    }
    return false;
}

bool RefCellContainer::AddNew( const RefObject &obj, AddRet *ret )
{
    if( m_usedCellCnt >= m_maxCellCnt ) return false;
    for( size_t i = 0; i < m_cells.size(); ++ i )
    {
        CellContainer::Cell &cell = m_cells[i];
        if( cell.status == CellContainer::Cell::EMPTY )
        {
            // put it here.
            cell.status = CellContainer::Cell::USED;
            cell.id = obj.id;
            ret->id = cell.id;
            ret->pos = (long) i;
            ret->op = AddRet::NEW;
            AddNewObject( obj, (long) i );
            return true;
        }
    }
    return false;
}

void RefCellContainer::AddNewObject( const RefObject &obj, long pos )
{
    RefObject newObj;
    newObj.id = obj.id;
    newObj.index = obj.index;
    newObj.stackCnt = obj.stackCnt;
    newObj.maxStackCnt = obj.maxStackCnt;
    newObj.pos = pos;
    
    ++ m_usedCellCnt;
    CellContainer::Cell &cell = m_cells[pos];
    cell.id = obj.id;
    cell.status = CellContainer::Cell::USED;
    m_refObjs.insert( std::make_pair( newObj.index, newObj ) );
}

