///
///
///
#ifndef ___XML_UTILS_H_
#define ___XML_UTILS_H_

#define QUERY_NUM( name, var, elem, type ) \
	{ \
		double v = 0; \
		elem->Attribute( name, &v ); \
		var = static_cast<type>( v ); \
	}
#define QUERY_STR( name, var, elem ) \
	{ \
		var = elem->Attribute( name ); \
	}
#define QUERY_NUM_DEFAULT( name, var, elem, type, def_v ) \
	{ \
		double v = 0; \
		elem->Attribute( name, &v ); \
		var = static_cast<type>( v ); \
		var = var == type() ? def_v : var; \
	}
#define QUERY_STR_DEFAULT( name, var, elem, def_v ) \
	{ \
		const char *t = elem->Attribute( name ); \
		var = t == 0 ? def_v : t; \
	}

#endif
