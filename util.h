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

/* functions that are visible outside of util.c */
void pl2sl( SV *item );
SLtype pltype( SV *plval, int *flag );

SV * sl2pl( void );

char *_get_object_type( SV *item );
SV * _create_empty_array( int ndim, int dims[] );

/*
 * we can convert a S-Lang array into
 *
 * numeric types:
 *   perl array reference
 *   Array_Type object
 *   piddle
 *
 * non-numeric types:
 *   perl array reference
 *   Array_Type object
 */
#define I_SL_ARRAY2AREF  0
#define I_SL_ARRAY2ATYPE 1 
#define I_SL_ARRAY2PDL   (I_SL_HAVE_PDL<<1)

extern int _slang_array_format; /* declaration in SLang.xs */

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
 * Note:
 *   these routines assume that the return value - if there is
 *   one - are going to be placed onto the Perl stack, hence they
 *   explicitly increase the reference count of the returned variable.
 *   This may turn out to be silly.
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
#define C2PL_MARG_S(x) C2PL_MARG( newSVpv( x, 0 ) )

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

/* 
 * need to pop item off S-Lang's internal stack and push
 * it onto S-Lang's main stack (or I've confused myself)
 */
#define SL_PUSH_ELEM1_ONTO_STACK(nelem) \
  (void) SLang_load_string( \
    "$2=struct {value};set_struct_field($2,\"value\",$1);__push_args($2);" \
  ); \
  _clean_slang_vars(nelem);

/* macros only used in SLang.xs but placed here for convenience */

/*
 * a macro to convert the S-Lang stack to a perl one
 * - should have made it a function but since it messes
 *   around with perl stack commands (eg EXTEND()) I
 *   couldn't be bothered working out how to do that
 *
 * note the minor complication in that we have to reverse
 * the order of the stack when moving from S-Lang to perl
 *
 * The macro requires the following in the PREINIT: section
 *
 *   SV **slist = NULL;
 *   int i, sdepth;
 *
 * and calls the function 'SV * sl2pl()'
 *
 * unlike Inline::Python/Ruby I always check the context
 */

#define CONVERT_SLANG2PERL_STACK \
    sdepth = _SLstack_depth(); \
    Printf( ("    *** stack depth = %d\n", sdepth) ); \
 \
    Printf( ("  checking context:\n") ); \
    Printf( ("    GIMME_V=%i\n", GIMME_V) ); \
    Printf( ("    G_VOID=%i\n", G_VOID) ); \
    Printf( ("    G_ARRAY=%i\n", G_ARRAY) ); \
    Printf( ("    G_SCALAR=%i\n", G_SCALAR) ); \
 \
    /* We can save a little time by checking our context */ \
    switch( GIMME_V ) { \
      case G_VOID: \
        /* let's clear the S-Lang stack */ \
        if ( sdepth ) { \
          Printf( ("clearing the S-Lang stack (%d items) since run in void context\n", sdepth) ); \
          if ( -1 == SLdo_pop_n( sdepth ) ) \
            croak( "Error: unable to clear the S-Lang stack\n" ); \
        } \
        XSRETURN_EMPTY; \
        break; \
 \
      case G_SCALAR: \
        if ( sdepth ) { \
          /* dump everything but the 'first' item */ \
          Printf( ("removing %d items from the stack since run in scalar context\n", \
	    sdepth-1 ) ); \
          if ( sdepth > 1 ) \
            if ( -1 == SLdo_pop_n( sdepth-1 ) ) \
              croak( "Error: unable to clear the S-Lang stack\n" ); \
 \
          Printf( ("trying to set perl stack item 0\n" ) ); \
          PUSHs( sv_2mortal( sl2pl() ) ); \
        } /* if: sdepth */ \
        break; \
 \
      case G_ARRAY: \
        /*  \
         * convert the S-Lang objects on the S-Lang stack into perl objects on  \
         * the perl stack \
         * \
         * note: the order of the S-Lang stack has to be reversed \
         */ \
        if ( sdepth ) { \
          Newz( 0, slist, sdepth, SV * ); \
          if ( slist == NULL ) \
            croak("Error: unable to allocate memory\n" ); /* ott ? */ \
          for ( i = sdepth-1; i >= 0; i-- ) { \
            Printf( ("reading from S-Lang stack item #%d\n", i ) ); \
            slist[i] = sl2pl(); \
          } \
 \
          /* now can stick the objects onto the perl stack */ \
          EXTEND( SP, sdepth ); \
          for ( i = 0; i < sdepth; i++ ) { \
            Printf( ("trying to set perl stack #%d\n", i ) ); \
            PUSHs( sv_2mortal( slist[i] ) ); \
          } \
 \
          Printf( ("freeing up stack-related memory\n") ); \
          Safefree( slist ); \
        } /* if: sdepth */ \
        break; \
 \
      default: \
        /* shouldn't happen with perl <= 5.8.0 */ \
        croak( "Internal error: GIMME_V is set to a value I don't understand\n" ); \
 \
    } /* switch(GIMME_V) */


#endif /* SL2PL_UTIL_H */

