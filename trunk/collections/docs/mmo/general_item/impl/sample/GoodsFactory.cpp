///
/// @file GoodsFactory.cpp
///
///
#include "GoodsFactory.h"
#include "../GIObjectProto.h"

void GoodsFactory::SetStackCnt::operator() ( GI::Object *obj )
{
    obj->SetValue( KeySet::StackCntKey, TypeSet::ValueType( m_cnt ) );
}

bool GoodsFactory::BatchCreate( TypeSet::IndexType index, GI::Object::PListenerType *listener,
     long cnt )
{
    const GI::ObjectProto *proto = SINGLETON( GI::ObjProtoFactory ).GetProto( index );
    if( !proto ) return false;
    TypeSet::ValueType maxStackCntVal = proto->GetValue( KeySet::MaxStackCntKey ); 
    long maxStackCnt = TypeSet::ValueType::ToStackCnt( maxStackCntVal );
    if( !maxStackCnt ) return false;
    long createCnt = cnt < maxStackCnt ? cnt : maxStackCnt;
    bool ret = true;
    while( cnt > 0 && ret )
    {
        ret = GI::FactoryContainer::Create( index, listener, SetStackCnt( createCnt ) );
        cnt -= createCnt;
        createCnt = cnt < maxStackCnt ? cnt : maxStackCnt;
    }
    return ret;
}

