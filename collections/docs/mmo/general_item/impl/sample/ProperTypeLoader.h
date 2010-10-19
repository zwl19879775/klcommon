///
/// @file PropertyTypeLoader.h
/// @brief Load 'PropertyType.xml'.
///
#ifndef ___PROPERTY_TYPE_LOADER_H_
#define ___PROPERTY_TYPE_LOADER_H_

#include "../GIBase.h"
#include <vector>

class PropertyTypeLoader : public GI::SerialData
{
public:
    struct Item
    {
        int key;
        int type;
        // more...
    };
    typedef std::vector<Item> ItemListT;
public:

    bool Load();

    virtual void Serialize( GI::ByteBuffer &buf ) const;

    /// UnSerialize these config and build Property Prototype.
    virtual bool UnSerialize( GI::ByteBuffer &buf );

private:
    ItemListT m_items;
};

#endif

