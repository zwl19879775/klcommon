/**
 * @file klsymtab.h
 * @author Kevin Lynx
 * @brief Symbol table implemention for kl.
 */
#ifndef ___KL_SYMTAB_H_
#define ___KL_SYMTAB_H_

/**
 * generic value 
 */
union Value
{
	double dval;
	char *sval;
	/* used for functions */
	void *address;
};

/**
 * symbol type
 */
typedef enum
{
	SB_FUNC,
	SB_VAR,
	SB_STRING
} SymType;

/**
 * symbol
 */
struct Symbol 
{
	char *name;
	union Value val;
	SymType type;
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
int sym_insert( struct symTable *st, const char *name, union Value val, SymType type );

/**
 * lookup whether a symbol is in the symbol table
 */
struct Symbol *sym_lookup( struct symTable *st, const char *name );

#endif 
