///
/// @file PropertyTypeLoader.h
/// @brief Load 'PropertyType.xml'.
///
#ifndef ___PROPERTY_TYPE_LOADER_H_
#define ___PROPERTY_TYPE_LOADER_H_

#include "../GIBase.h"
#include "ValGenFuncs.h"
#include <vector>

class PropertyTypeLoader : public GI::SerialData 
    MULTI_DEF_SINGLETON( PropertyTypeLoader )
{
public:
    struct Item
    {
        int key;
        int type;
        ValGen::Param param;
    };
    typedef std::vector<Item> ItemListT;
public:
    PropertyTypeLoader() { }

    virtual ~PropertyTypeLoader() { Clear(); }

    bool Load();

    void Clear();

    virtual void Serialize( GI::ByteBuffer &buf ) const;

    virtual bool UnSerialize( GI::ByteBuffer &buf );

    void Build() const;
private:
    ItemListT m_items;
};

#endif

