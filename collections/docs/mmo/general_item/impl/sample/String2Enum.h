///
/// @file String2Enum.h
///
///
#ifndef ___STRING2ENUM_H_
#define ___STRING2ENUM_H_

struct EnumDesc
{
    const char *desc;
    long val;
};

#define MAKE_NAME( name ) __enumDesc_##name
#define DECLARE_PAIR( name ) \
    extern struct EnumDesc MAKE_NAME( name ) []
#define MAKE_PAIR( e ) { #e, e },
#define MAKE_PAIR_BEGIN( name )  \
    struct EnumDesc MAKE_NAME( name ) [] = { 
#define MAKE_PAIR_END() \
    { NULL, 0 } };

long _String2Enum( const struct EnumDesc *descs, const char *s );

#define String2Enum( name, s ) \
    _String2Enum( MAKE_NAME( name ), s )

/// pre-declaration.
DECLARE_PAIR( EnumGoodsProperty );
DECLARE_PAIR( PropertyType );
DECLARE_PAIR( ParamType );
DECLARE_PAIR( DefaultType );

#endif
