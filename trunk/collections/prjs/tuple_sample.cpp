///
/// A very simple tuple implemention.
/// Kevin Lynx
/// 12.8.2009
///
#include <string>
#include <stdio.h>

/// To combine sereral types in one type.
#define MAKE_TYPES1( T1 ) T1()
#define MAKE_TYPES2( T1, T2 ) T1( T2 )
#define MAKE_TYPES3( T1, T2, T3 ) T1( T2, T3 )

template <typename TypeList>
class tuple;

template <typename P1>
class tuple<MAKE_TYPES1( P1 )>
{
public:
	typedef P1 Type1;
public:
	tuple() : _v1( Type1() )
	{
	}

	tuple( const Type1 &v ) : _v1( v )
	{
	}

public:
	Type1 _v1;
};

template <typename P1, typename P2>
class tuple<MAKE_TYPES2( P1, P2 )>
{
public:
	typedef P1 Type1;
	typedef P2 Type2;
public:
	tuple() : _v1( Type1() ), _v2( Type2() )
	{
	}

	tuple( const Type1 &v1, const Type2 &v2 ) : _v1( v1 ),
		_v2( v2 )
	{
	}

public:
	Type1 _v1;
	Type2 _v2;
};

template <typename P1, typename P2, typename P3>
class tuple<MAKE_TYPES3( P1, P2, P3 )>
{
public:
	typedef P1 Type1;
	typedef P2 Type2;
	typedef P3 Type3;
public:
	tuple() : _v1( Type1() ), _v2( Type2() ), _v3( Type3() )
	{
	}

	tuple( const Type1 &v1, const Type2 &v2, const Type3 &v3 ) : 
		_v1( v1 ), _v2( v2 ), _v3( v3 )
	{
	}

public:
	Type1 _v1;
	Type2 _v2;
	Type3 _v3;
};

/// Convenient macro to access values from a tuple.
#define VAL( t, i ) (t._v##i)
#define TYPE( T, i ) T::Type##i

int main()
{
	tuple<MAKE_TYPES2( int, std::string )> t2( 2, "test" );
	tuple<MAKE_TYPES3( char, int, float )> t3( 'A', 3, 3.12f );

	VAL( t2, 2 ) = "sample";

	typedef tuple<MAKE_TYPES3( double, char, int)> TupleType;
	TupleType t4( 2.0, 'B', 4 );
	TYPE( TupleType, 2 ) c = VAL( t4, 2 );

	return 0;
}
