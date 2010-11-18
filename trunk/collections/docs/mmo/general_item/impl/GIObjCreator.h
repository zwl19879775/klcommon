///
/// @file GIObjCreator.h
/// @author Kevin Lynx
/// @date 11.9.2010
///
#ifndef ___GI_OBJ_CREATOR_H_
#define ___GI_OBJ_CREATOR_H_

#include "GIConfig.h"

namespace GI
{
    class Object;

    class ObjectListener
    {
    public:
        virtual ~ObjectListener() { }
        virtual void OnCreate( Object *obj ) { }
        virtual void OnClone( const Object *srcObj, Object *obj ) { }
        virtual void OnDestroy( Object *obj ) { }
    };

    class ObjCreator DEF_SINGLETON( ObjCreator )
    {
    public:
        ObjCreator( ObjectListener *listener ) : m_listener( listener )
        {
        }

        /// Create an object.
        Object *Create();

        /// Clone an object from 'srcObj'.
        Object *Clone( const GI::Object *srcObj );

        /// Destroy an object.
        void Destroy( Object *obj );

        /// The object must have no properties yet.
        bool FillProperty( TypeSet::IndexType index, Object *obj );

        /// THe object must already have index property.
        bool SetProto( Object *obj );
    private:
        ObjectListener *m_listener;
    };
}

#endif

