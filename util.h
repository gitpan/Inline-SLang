#ifndef SL2PL_UTIL_H
#define SL2PL_UTIL_H

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

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


#include "slang.h"

/*
 * BAD BAD BAD code
 * I want to create a SLang_Ref_Type object and
 * push it onto the S-Lang stack but there does
 * not appear to be any public routines for this
 *
 * sltypes.c/_SLang_push_ref() would be good since
 *   it is not statically linked, but we would
 *   need to access the elements of the ref structure
 *   (which is not publibally defined)
 *
 * so, for now, we hack out the structure definition
 * from _slang.h and include it here. This is NOT
 * good code. 
 *
 */
typedef struct
{
  int is_global;
  union
  {
    /* assuming can use 'void *' instead of the actual pointer types */
    VOID_STAR nt;
    VOID_STAR local_obj;
  }
  v;
} _Inline_SLang_Ref_Type;

/* functions that are visible outside of util.c */
void pl2sl( SV *item );
SV * sl2pl( void );

/* macro definitions intended for util.h only but placed here for convenience */

/*
 * utility routines for calling Perl object methods from C
 *
 *   char *_get_obj_type( SV *obj )
 *     returns a string giving the object name (or "<none>")
 *     the string does not have to be freed after use (I believe)
 *
 * extra_par_code is a set of XPUSHs(...) statements used to push
 * method parameters onto the stack. If there are none then use ""
 *
 *   CALL_METHOD_VOID( SV *obj, char *method, extra_par_code )
 *     calls the method on the given object which is expected
 *     to return nothing
 *
 *   CALL_METHOD_SCALAR_DOUBLE( SV *obj, char *method, extra_par_code, double result  )
 *     calls the method on the given object which is expected
 *     to return a double.
 *
 *   CALL_METHOD_SCALAR_SV( SV *obj, char *method, extra_par_code, SV * result )
 *     calls the method on the given object which is expected
 *     to return a SV *
 *
 * see 'perldoc perlcall' for information on what's happening here
 *
 */

#define C2PL_MARG(x)   XPUSHs( sv_2mortal( x ) )
#define C2PL_MARG_D(x) C2PL_MARG( newSVnv( x ) )

#define CALL_METHOD_VOID(object,method,parstring) \
 { \
  dSP; ENTER; SAVETMPS; PUSHMARK(SP); \
  XPUSHs(object); \
  parstring; \
  PUTBACK; \
  Printf( ("Calling <some object>->%s(...)\n",method) ); \
  (void) call_method( method, G_VOID ); \
  SPAGAIN; PUTBACK; FREETMPS; LEAVE; \
 }

#define CALL_METHOD_SCALAR_DOUBLE(object,method,parstring,result) \
 { \
  int count; \
  dSP; ENTER; SAVETMPS; PUSHMARK(SP); \
  XPUSHs(object); \
  parstring; \
  PUTBACK; \
  Printf( ("Calling <some object>->%s(...)\n",method) ); \
  count = call_method( method, G_SCALAR ); \
  SPAGAIN; \
  if ( count != 1 ) { \
    char emsg[256]; /* if it over-runs, it over-runs */ \
    snprintf( emsg, 256, "%s->%s() did not return a value (expected double)\n", \
      _get_object_type(object), method ); \
    croak ( emsg ); \
  } \
  result = (double) POPn; \
  PUTBACK; FREETMPS; LEAVE; \
 }
   
#define CALL_METHOD_SCALAR_SV(object,method,parstring,result) \
 { \
  int count; \
  dSP; ENTER; SAVETMPS; PUSHMARK(SP); \
  XPUSHs(object); \
  parstring; \
  PUTBACK; \
  Printf( ("Calling <some object>->%s(...)\n",method) ); \
  count = call_method( method, G_SCALAR ); \
  SPAGAIN; \
  if ( count != 1 ) { \
    char emsg[256]; /* if it over-runs, it over-runs */ \
    snprintf( emsg, 256, "%s->%s() did not return a value (expected SV *)\n", \
      _get_object_type(object), method ); \
    croak ( emsg ); \
  } \
  result = SvREFCNT_inc( POPs ); /* is this correct ? */ \
  PUTBACK; FREETMPS; LEAVE; \
 }

/*
 * SL2PL_ARRAY1D_ITYPE( INT, int ) 
 * will create code to handle 1D array values of integer types
 *
 *   SLANG_INT_TYPE and SLANG_UINT_TYPE
 *
 * SL2PL_ARRAY1D_FTYPE( FLOAT, float ) 
 * will create code to handle 1D array values of float types
 *
 *   SLANG_FLOAT_TYPE 
 *
 * it assumes that i is defined as an int
 * and the array is in at and that the perl array
 * is in parray
 */

#define SL2PL_ARRAY1D_ITYPE(stypeu,ctype) \
  case SLANG_##stypeu##_TYPE: \
    { \
      ctype ival; \
      for ( i = 0; i < at->num_elements; i++ ) { \
	(void) SLang_get_array_element( at, &i, &ival ); \
	Printf( ("  ctype array1D[%d] = %d\n", i, ival ) ); \
	av_store( parray, i, newSViv( ival ) ); \
      } \
      break; \
    } \
  \
  case SLANG_U##stypeu##_TYPE: \
    { \
      unsigned ctype ival; \
      for ( i = 0; i < at->num_elements; i++ ) { \
	(void) SLang_get_array_element( at, &i, &ival ); \
	Printf( ("  unsigned ctype array1D[%d] = %d\n", i, ival ) ); \
	av_store( parray, i, newSVuv( ival ) ); \
      } \
      break; \
    }

#define SL2PL_ARRAY1D_FTYPE(stypeu,ctype) \
  case SLANG_##stypeu##_TYPE: \
    { \
      ctype fval; \
      for ( i = 0; i < at->num_elements; i++ ) { \
	(void) SLang_get_array_element( at, &i, &fval ); \
	Printf( ("  ctype array1D[%d] = %g\n", i, fval ) ); \
	av_store( parray, i, newSVnv( fval ) ); \
      } \
      break; \
    }

/*
 * SL2PL_ARRAY2D_ITYPE( INT, int ) 
 * will create code to handle 2D array values of integer types
 *
 *   SLANG_INT_TYPE and SLANG_UINT_TYPE
 *
 * SL2PL_ARRAY2D_FTYPE( FLOAT, float ) 
 * will create code to handle 2D array values of float types
 *
 *   SLANG_FLOAT_TYPE 
 *
 * it assumes that i, j is defined as an int
 * and the array is in at and that the perl arrays
 * are in xarray & yarray
 * also nx,ny must be the axes sizes
 * and long dims[2]
 */

#define SL2PL_ARRAY2D_ITYPE(stypeu,ctype) \
  case SLANG_##stypeu##_TYPE: \
    { \
      ctype ival; \
      for ( i = 0; i < nx; i++ ) { \
	yarray = (AV *) sv_2mortal( (SV *) newAV() ); \
	av_extend( yarray, (I32) ny ); \
 \
	dims[0] = i; \
	for ( j = 0; j < ny; j++ ) { \
	  dims[1] = j; \
 \
	  (void) SLang_get_array_element( at, dims, &ival ); \
	  Printf( ("  ctype array2D[%d,%d] = %i\n", i, j, ival ) ); \
	  av_store( yarray, j, newSViv( ival ) ); \
 \
	} /* for: j */ \
 \
	av_store( xarray, i, newRV_inc( (SV *) yarray ) ); \
 \
      } /* for: i */ \
      break; \
    } \
 \
  case SLANG_U##stypeu##_TYPE: \
    { \
      unsigned ctype ival; \
      for ( i = 0; i < nx; i++ ) { \
	yarray = (AV *) sv_2mortal( (SV *) newAV() ); \
	av_extend( yarray, (I32) ny ); \
 \
	dims[0] = i; \
	for ( j = 0; j < ny; j++ ) { \
	  dims[1] = j; \
 \
	  (void) SLang_get_array_element( at, dims, &ival ); \
	  Printf( ("  unsigned ctype array2D[%d,%d] = %i\n", i, j, ival ) ); \
	  av_store( yarray, j, newSVuv( ival ) ); \
 \
	} /* for: j */ \
 \
	av_store( xarray, i, newRV_inc( (SV *) yarray ) ); \
 \
      } /* for: i */ \
      break; \
    } 

#define SL2PL_ARRAY2D_FTYPE(stypeu,ctype) \
  case SLANG_##stypeu##_TYPE: \
    { \
      ctype fval; \
      for ( i = 0; i < nx; i++ ) { \
	yarray = (AV *) sv_2mortal( (SV *) newAV() ); \
	av_extend( yarray, (I32) ny ); \
 \
	dims[0] = i; \
	for ( j = 0; j < ny; j++ ) { \
	  dims[1] = j; \
 \
	  (void) SLang_get_array_element( at, dims, &fval ); \
	  Printf( ("  ctype array2D[%d,%d] = %g\n", i, j, fval ) ); \
	  av_store( yarray, j, newSVnv( fval ) ); \
 \
	} /* for: j */ \
 \
	av_store( xarray, i, newRV_inc( (SV *) yarray ) ); \
 \
      } /* for: i */ \
      break; \
    }

/*
 * SL2PL_ITYPE( INT, integer, int ) 
 * will create code to handle scalar values of integer type
 *   SLANG_INT_TYPE and SLANG_UINT_TYPE
 * the second argument is the name of the SLang_pop_xxx
 * routine, and the third the c type (it's only for int/integer
 * that the second and third args are different)
 */

#define SL2PL_ITYPE(stypeu,stypel,ctype) \
  case SLANG_##stypeu##_TYPE: \
    { \
      ctype ival; \
      if ( -1 == SLang_pop_##stypel ( &ival ) ) \
	croak( "Error: unable to read stypel value from the stack\n" ); \
      Printf( ("  stack contains: ctype = %i\n", ival ) ); \
      return newSViv(ival); \
    } \
  \
  case SLANG_U##stypeu##_TYPE: \
    { \
      unsigned ctype ival; \
      if ( -1 == SLang_pop_u##stypel ( &ival ) ) \
	croak( "Error: unable to read stypel value from the stack\n" ); \
      Printf( ("  stack contains: unsigned ctype = %i\n", ival ) ); \
      return newSVuv(ival); \
    }

#endif /* SL2PL_UTIL_H */

