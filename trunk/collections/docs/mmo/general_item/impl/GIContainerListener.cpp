///
/// @file GIContainerListener.cpp
///
///
#include "GIContainerListener.h"
#include "GIContainer.h"

namespace GI
{
    void ContainerListener::Attach( BaseContainer *con )
    {
        con->SetListener( this );
    }

    void ContainerListener::Detach( BaseContainer *con )
    {
        con->SetListener( NULL );
    }
}

