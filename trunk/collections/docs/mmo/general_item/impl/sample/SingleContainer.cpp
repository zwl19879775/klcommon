///
/// @file SingleContainer.cpp
///
///
#include "SingleContainer.h"
#include "ObjVisitor.h"

SingleContainer::SingleContainer( long index )
{
    m_index = index;
    m_obj = NULL;
}

SingleContainer::~SingleContainer()
{
}

bool SingleContainer::SetCount( long cnt )
{
    if( cnt < 0 ) return false;
    if( cnt == 0 )
    {
        DestroyAll();
        m_obj = NULL;
        return true;
    }
    if( !m_obj )
    {
        if( !Create( m_index, NULL ) ) return false;
    }
    if( ObjVisitor::MaxCount( m_obj ) < cnt ) return false;
    ObjVisitor::SetCount( m_obj, cnt );
    NOTIFY_LISTENER( OnSetAmount( this, m_obj ) );
    return true;
}

long SingleContainer::GetCount() const
{
    if( !m_obj ) return 0;
    return ObjVisitor::Count( m_obj );
}

bool SingleContainer::ChangeCount( long cnt )
{
    long curCnt = GetCount();
    long retCnt = curCnt + cnt;
    return SetCount( retCnt );
}

bool SingleContainer::Add( GI::Object *obj )
{
    if( ObjVisitor::Index( obj ) != m_index ) return false;
    if( m_obj ) return false;
    m_obj = obj;
    return GI::FactoryContainer::Add( obj );
}

bool SingleContainer::Remove( GI::Object *obj )
{
    return GI::FactoryContainer::Remove( obj );
}

