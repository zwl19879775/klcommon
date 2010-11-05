///
/// @file ObjVisitor.cpp
///
///
#include "ObjVisitor.h"
#include "GoodsPropertyType.h"

namespace ObjVisitor
{
    TypeSet::IDType ID( const GI::Object *obj )
    {
        TypeSet::ValueType val = obj->GetValue( KeySet::IDKey );
        if( !val.Valid() ) return TypeSet::IDType();
        return TypeSet::ValueType::ToID( val );
    }

    long Count( const GI::Object *obj )
    {
        TypeSet::ValueType val = obj->GetValue( KeySet::StackCntKey );
        if( !val.Valid() ) return 0;
        return TypeSet::ValueType::ToStackCnt( val );
    }

    long MaxCount( const GI::Object *obj )
    {
        TypeSet::ValueType val = obj->GetValue( KeySet::MaxStackCntKey );
        if( !val.Valid() ) return 0;
        return TypeSet::ValueType::ToStackCnt( val );
    }

    void SetCount( GI::Object *obj, long cnt )
    {
        obj->SetValue( KeySet::StackCntKey, TypeSet::ValueType( cnt ) );
    }

    long Pos( const GI::Object *obj )
    {
        TypeSet::ValueType val = obj->GetValue( TypeSet::KeyType( PCELL_POS ) );
        if( !val.Valid() ) return -1;
        return TypeSet::ValueType::ToLong( val );
    }

    void SetPos( GI::Object *obj, long pos )
    {
        TypeSet::KeyType posKey( PCELL_POS );
        if( !obj->HasProperty( posKey ) )
        {
            obj->AddProperty( posKey, TypeSet::ValueType( pos ) );
        }
        else
        {
            obj->SetValue( posKey, TypeSet::ValueType( pos ) );
        }
    }

    TypeSet::IndexType Index( const GI::Object *obj )
    {
        TypeSet::ValueType val = obj->GetValue( KeySet::IndexKey );
        if( !val.Valid() ) return TypeSet::IndexType();
        return TypeSet::ValueType::ToIndex( val );
    }

    bool IsSameIndex( const GI::Object *obj1, const GI::Object *obj2 )
    {
        return obj1->GetValue( KeySet::IndexKey ) == 
            obj2->GetValue( KeySet::IndexKey );
    }

    bool CanStack( const GI::Object *obj1, const GI::Object *obj2 )
    {
        if( !IsSameIndex( obj1, obj2 ) ) return false;
        long maxCnt = MaxCount( obj1 );
        long cnt1 = Count( obj1 );
        long cnt2 = Count( obj2 );
        return cnt1 + cnt2 <= maxCnt;
    }

    bool IsConObject( const GI::Object *obj )
    {
        return Type( obj ) == PACKET;
    }

    long Type( const GI::Object *obj )
    {
        TypeSet::KeyType typeKey( PTYPE );
        TypeSet::ValueType val = obj->GetValue( typeKey );
        return TypeSet::ValueType::ToLong( val );
    }

    long ConSize( const GI::Object *obj )
    {
        TypeSet::KeyType sizeKey( PCON_SIZE );
        TypeSet::ValueType val = obj->GetValue( sizeKey );
        return TypeSet::ValueType::ToLong( val );
    }

    void SetRgnPosX( GI::Object *obj, double x )
    {
        TypeSet::KeyType xKey( PRGNX );
        if( !obj->HasProperty( xKey ) )
        {
            obj->AddProperty( xKey, TypeSet::ValueType( x ) );
        }
        else
        {
            obj->SetValue( xKey, TypeSet::ValueType( x ) );
        }
    }

    void SetRgnPosY( GI::Object *obj, double y )
    {
        TypeSet::KeyType yKey( PRGNY );
        if( !obj->HasProperty( yKey ) )
        {
            obj->AddProperty( yKey, TypeSet::ValueType( y ) );
        }
        else
        {
            obj->SetValue( yKey, TypeSet::ValueType( y ) );
        }
    }
}

