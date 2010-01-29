///
/// @file VariableList.h
/// @author Kevin Lynx
/// @brief VariableList is a variables table, to store some variables.
///
#ifndef ___VARIABLE_LIST_H_
#define ___VARIABLE_LIST_H_

#include "tString.h"
#include <map>
#include <string>

#define VAR_TEST_ONLY

#ifdef VAR_TEST_ONLY
#define CGUID long
#define NULL_GUID 0
#endif

class VariableList
{
public:
	enum VarType
	{
		VAR_NUM,
		VAR_STRING,
		VAR_GUID,
	};

	typedef double Number;

	struct Var
	{
		long type;
		long count;
		union
		{
			tString *strs;
			Number *nums;
			CGUID *guids;
			void *p;
		};
	};

	/// <varName, var>
	typedef std::map<std::string, Var*> VarTableT;

public:
	VariableList();

	/// Rmove all the variables.
	~VariableList();

	///
	/// Add a variable.
	///  
	/// @param name the variable name
	/// @param initValue the initial value, if the variable is an array, it will set
	/// all the elements to this value.
	/// @param count the array's element count, 0 means the variable is not an array.
	///
	template <typename VarT>
	bool AddVar( const char *name, const VarT &initValue, long count = 0 );

	/// Remove a variable.
	void RemoveVar( const char *name );

	/// Remove all variables.
	void RemoveAll();

	///
	/// Get a variable value, must specify the variable type, 
	/// i.e:
	///		const tString &str = varList.GetVarValue<tString>( "name" );
	/// If not found the variable, this function will return an invalid value which 
	/// can be checked by using IsValueValid function.
	///
	template <typename VarT>
	const VarT &GetVarValue( const char *name, long index = 0 );

	/// Check whether it has a variable with name.
	bool HasVar( const char *name ) const;

	///
	/// Check whether a variable value is valid.
	/// i.e:
	///		const tString &str = varList.GetVarValue<tString>( "name" );	
	///		bool valid = VariableList::IsValudValid( str );
	///
	template <typename VarT>
	static bool IsValueValid( const VarT &value );

private:
	Var *GetVar( const char *name );

	void AddVar( const char *name, Var *var );
private:
	VarTableT m_VarTable;
};

/// Some template functions are defined the file included below.
#include "VariableList.tcc"

#endif
