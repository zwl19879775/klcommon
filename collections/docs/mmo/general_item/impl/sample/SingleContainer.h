///
/// @file SingleContainer.h
///
///
#ifndef ___SINGLECONTAINER_H_
#define ___SINGLECONTAINER_H_

#include "../GIContainer.h"

class SingleContainer : public GI::FactoryContainer
{
public:
    SingleContainer( long index );
    
    virtual ~SingleContainer();

    bool SetCount( long cnt );

    long GetCount() const;

    bool ChangeCount( long cnt );

    const GI::Object *GetSingleObj() const { return m_obj; }

private:
    virtual bool Add( GI::Object *obj );

    virtual bool Remove( GI::Object *obj );
private:
    long m_index;
    GI::Object *m_obj;
};

#endif

