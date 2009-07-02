///
///
///
#include <stdio.h>
#include <list>

class ref_op
{
public:
	virtual void be_null()  { }
};

typedef std::list<ref_op*> RefOpList;
class ref_base 
{
public:
	~ref_base()
	{
		clear_ops();
	}

	void add_ref( ref_op *op )
	{
		_oplist.push_back( op );
	}

	void clear_ops()
	{
		for( RefOpList::const_iterator it = _oplist.begin();
				it != _oplist.end(); ++ it )
		{
			(*it)->be_null();
		}
	}
	
private:
	RefOpList _oplist;
};

template <typename _Tp>
class auto_null : public ref_op
{
public:
	void fetch( _Tp *t )
	{
		_t = t;
		t->add_ref( this );
	}
	auto_null<_Tp> &operator = ( _Tp *t )
	{
		fetch( t );
		return *this;
	}
	void be_null()
	{
		_t = 0;
	}
	operator _Tp*()
	{
		return _t;
	}
private:
	_Tp *_t;
};

//////////////////////////////////////////////////////////////////////////////
class CMonster : public ref_base 
{
};

class CMonsterAI 
{
public:
	void SetOwner( CMonster *pOwner )
	{
		m_Owner = pOwner;
	}

	void Test()
	{
		if( (CMonster*)m_Owner == NULL )
		{
			printf( "The owner is null.\n" );
		}
		else
		{
			printf( "The owner is NOT null.\n" );
		}
	}
private:
	auto_null<CMonster> m_Owner;	
};

int main()
{
	CMonster *pMonster = new CMonster();
	CMonsterAI *pAI = new CMonsterAI();
	pAI->SetOwner( pMonster );
	pAI->Test();
	delete pMonster;
	pAI->Test();
	delete pAI;
	return 0;
}