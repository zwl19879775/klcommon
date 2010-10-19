///
/// @file GoodsFactory.h
///
///
#ifndef ___GOODS_FACTORY_H_
#define ___GOODS_FACTORY_H_

#include "../GIContainer.h"

class GoodsFactory : public GI::FactoryContainer
{
public:
    /// Functor to set object initial stack count.
    struct SetStackCnt
    {
        SetStackCnt( long cnt ) : m_cnt( cnt ) { }
        
        void operator() ( GI::Object *obj );

        int m_cnt;
    };
public:
    GoodsFactory() { }

    virtual ~GoodsFactory() { }

    bool BatchCreate( TypeSet::IndexType index, GI::Object::PListenerType *listener,
            long cnt );
};

#endif

