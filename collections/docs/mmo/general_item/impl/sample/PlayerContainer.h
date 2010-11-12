///
/// @file PlayerContainer.h
///
///
#ifndef ___PLAYERCONTAINER_H_
#define ___PLAYERCONTAINER_H_

#include "SubContainer.h"
#include "CellContainer.h"
#include "ContainerDef.h"

class PlayerContainer
{
public:
    PlayerContainer();

    ~PlayerContainer();

    void SetOwner( CPlayer *owner ) { m_owner = owner; }

    /// bool fn( type, BaseCellContainer* );
    template <typename T>
    bool Traverse( T fn );

    BaseCellContainer *GetContainer( long type );

    /// Move all the objects in srcCon to player container.
    bool Move( GI::BaseContainer *srcCon );

    long GetType( GI::BaseContainer *con ) const;

    CPlayer *GetOwner() const { return m_owner; }
private:
    long ToCellPos( long t ) const;

public:
    SubContainer m_subCons;
    CellContainer m_mainCon;
    CPlayer *m_owner;
};

template <typename T>
bool PlayerContainer::Traverse( T fn )
{
    fn( ConDef::PEI_PACKET, &m_mainCon );
    for( long i = 0; i < m_subCons.Size(); ++ i )
    {
        const SubContainer::Cell &cell = m_subCons.GetCell( i );
        if( cell.status == SubContainer::Cell::USED )
        {
            if( fn( i + ConDef::PEI_PACK1, CAST_CELL( cell.u ) ) ) return true;
        }
    }
    return false;
}

#endif

