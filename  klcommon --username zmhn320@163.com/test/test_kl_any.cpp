///
///
///
#include "kl_any.h"
#include <stdio.h>

struct ObjType
{
    int _v;
    ObjType( int v ) : _v( v )
    {
    }
};

int main()
{
    ObjType obj( 1 );

    kl_common::any a1( obj );
    ObjType *pObj = kl_common::any_cast<ObjType>( &a1 );
    printf( "%d\n", pObj->_v );
    
    const kl_common::any *pA1 = &a1;
    const ObjType *pcObj = kl_common::any_cast<ObjType>( pA1 );
    
    ObjType obj2 = kl_common::any_cast<ObjType>( a1 );
    printf( "%d\n", obj2._v );

    const kl_common::any &a2 = a1;
    obj2 = kl_common::any_cast<ObjType>( a2 );
    printf( "%d\n", obj2._v );

    return 0;
}
