
#include <stdio.h>
#include "kl_factory.h"

struct ProductBase
{
	char name[32];
};

struct ProductA : public ProductBase
{
	static ProductBase *Create()
	{
		return new ProductA();
	}
	static void Destroy( ProductBase *p )
	{
		delete p;
	}
};

struct ProductB : public ProductBase
{
	static ProductBase *Create()
	{
		return new ProductB();
	}
	static void Destroy( ProductBase *p )
	{
		delete p;
	}
};

//Product *CreateProduct()
//{
//	return new Product();
//}
//
//void DestroyProduct( Product *p )
//{
//	delete p;
//}

int main()
{
	typedef ProductBase *(*CreatorT)();
	typedef void (*DestroyerT)( ProductBase* );

	kl_common::factory<long, ProductBase, CreatorT, DestroyerT> factory;
	factory.add( 1, ProductA::Create, ProductA::Destroy );
	ProductBase *p = factory.create( 1 );
	factory.destroy( 1, p );

	factory.add( 2, ProductB::Create, ProductB::Destroy );
	ProductBase *p2 = factory.create( 2 );
	factory.destroy( 2, p2 );

	factory.remove( 1 );
	return 0;
}
