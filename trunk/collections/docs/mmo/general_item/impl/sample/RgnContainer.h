///
/// @file RgnContainer.h
///
///
#ifndef ___RGNCONTAINER_H_
#define ___RGNCONTAINER_H_

#include "../GIContainer.h"

class RgnContainer : public GI::FactoryContainer
{
public:
    struct GoodsPos
    {
        double x, y;
        GoodsPos( double _x = 0, double _y = 0 ) : x(_x), y(_y) { }
    };

    struct SetStackCntPos
    {
        SetStackCntPos( long cnt, const GoodsPos &pos ) : m_pos( pos ), m_cnt( cnt )
        {
        }
        void operator() ( GI::Object *obj );

        GoodsPos m_pos;
        long m_cnt;
    };
public:
    RgnContainer();

    virtual ~RgnContainer();

    bool Move( GI::MergeContainer *srcCon, TypeSet::IDType objID, GoodsPos pos, long cnt );

protected:
    virtual bool Remove( GI::Object *obj );
};

#endif

