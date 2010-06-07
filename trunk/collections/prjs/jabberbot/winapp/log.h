
#ifndef ___LOG_H_
#define ___LOG_H_

#include "strophe.h"

xmpp_log_t *get_logger() ;

void set_loglvl (int lvl);

void open_logfile();

#endif
