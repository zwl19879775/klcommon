/**
 * @file klsymtab.h
 * @author Kevin Lynx
 * @brief Symbol table implemention for kl.
 */
#ifndef ___KL_SYMTAB_H_
#define ___KL_SYMTAB_H_

/**
 * symbol type
 */
typedef enum
{
	SB_FUNC,
	SB_VAR_NUM,
	SB_VAR_STRING
} SymType;

/**
 * generic value 
 */
struct Value
{
	union
	{	
		double dval;
		char *sval;
		/* used for functions */
		void *address;
	};
	SymType type;
};


/**
 * symbol
 */
struct Symbol 
{
	char *name;
	struct Value val;
	struct Symbol *next;
};

#define SYM_SIZE 211

/**
 * symbol table
 */
struct symTable
{
	struct Symbol *table[SYM_SIZE];
};

/**
 * create a new symbol table
 */
struct symTable *sym_new();

/**
 * free a symbol table
 */
void sym_free( struct symTable *st );

/**
 * insert or update a symbol into the symbol table, if the symbol exists,
 * update its value, otherwise create a new symbol in the table.
 */
int sym_insert( struct symTable *st, const char *name, struct Value val );

/**
 * lookup whether a symbol is in the symbol table
 */
struct Symbol *sym_lookup( struct symTable *st, const char *name );

#endif 
