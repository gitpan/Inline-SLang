#ifndef SL2PL_H
#define SL2PL_H

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "slang.h"

#ifdef I_SL_DEBUG
#  define Printf(x)	printf x
#else
#  define Printf(x)	/* empty */
#endif

/* is this correct ? */
#ifdef I_SL_FIXME
#  define fixme(x) \
         printf("FIXME at line %d\n", __LINE__); \
         printf( (x) );
#else
#  define fixme(x)     /* empty */
#endif

void pl2sl( SV *item );
SV * sl2pl( void );

#endif /* SL2PL_H */

