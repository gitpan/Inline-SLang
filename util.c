/****************************************************************************
 * util.c
 * Conversion routines between S-Lang and Perl data types.
 ****************************************************************************/

/* Supported types (taken from slang.h):

2p - does conversion from S-Lang to Perl work?
     s - scalars
     1 - only 1D arrays
     2 - <= 2D arrays
     n - nD arrays
2s - does conversion from Perl to S-Lang work?


2p 2s S-Lang                   Perl
---------------------------------------------------

s  s  SLANG_NULL_TYPE          undef

s2 s  SLANG_CHAR_TYPE          integer (IV)
s2 s  SLANG_SHORT_TYPE
s2 s  SLANG_INT_TYPE
s2 s  SLANG_LONG_TYPE

s2 s  SLANG_UCHAR_TYPE         unsigned integer (UV)
s2 s  SLANG_USHORT_TYPE        [note: when converting perl to
s2 s  SLANG_UINT_TYPE           S-Lang all integers are
s2 s  SLANG_ULONG_TYPE          converted as signed integers only]

s2 s  SLANG_FLOAT_TYPE         double
s2 s  SLANG_DOUBLE_TYPE

s2 s  SLANG_STRING_TYPE        string

s2 s  SLANG_COMPLEX_TYPE       Math::Complex

2-    SLANG_ARRAY_TYPE         array reference

?     SLANG_ASSOC_TYPE         hash reference

s  s  SLANG_DATATYPE_TYPE      Inline::SLang::datatype

?     SLANG_FILE_PTR_TYPE      should be able to convert to a perl IO variable

 */

/* Are we going to support these?

SLANG_ANY_TYPE - should be easy to support since can just convert
  to the correct perl type BUT would we want to keep the
  fact that it's an Any_Type (eg converting perl back to S-Lang
  knows that it's an Any_Type)?

SLANG_UNDEFINED_TYPE - convert to undef ?

SLANG_VOID_TYPE
SLANG_REF_TYPE

SLANG_STRUCT_TYPE - handle as an object?
SLANG_ISTRUCT_TYPE - what is this - an intrinsic structure>


SLANG_BSTRING_TYPE - what is this?
SLANG_FILE_FD_TYPE - what is this?

 */

/*
 * Unsupported types (taken from slang.h):

SLANG_INTP_TYPE - as apparently I shouldn't see one of these

 *
 */

#include "util.h"

/*
 * utility routines for pl2sl()
 *
 * from_perl_complex() - convert Math::Complex object to Real,Imag numbers
 *
 * the array  should handle the integers as does the scalar
 * code (ie respect the types)
 */

static double
call_double_complex_method( SV *item, const char *method ) {

  dSP; /* this defines variables; it's not a bit of code! */
  double result;
  int count;

  /*
   * - see 'perldoc perlcall' for information on what's happening
   */

  ENTER;
  SAVETMPS;
  PUSHMARK(SP);

  /* tried caching "Math::Complex", but it got over-written somewhere */
  XPUSHs( item );
  PUTBACK;

  Printf( ("Calling Math::Complex->%s()\n",method) );
  count = call_method( method, G_SCALAR );
  
  SPAGAIN;
  if ( count != 1 )
    croak("Math::Complex method did not return a value (unable to convert Complex_Type)\n");

  result = (double) POPn;

  PUTBACK;
  FREETMPS;
  LEAVE;

  return result;

} /* call_double_complex_method() */

/*
 * convert perl variables to S-Lang variables
 *
 * note: we automatically push each variable onto the S-Lang stack
 * - this will probably turn out to be a bad idea; for instance it
 *   means it can't be called recursively when converting
 *   array/associative arrays.
 */

void
pl2sl( SV *item ) {

  /* undef */
  if ( !SvOK(item) ) {
    Printf( ("item=undef\n") );
    if ( -1 == SLang_push_null() )
      croak( "Unable to push a NULL onto the stack" );
    return;
  }

  /* integer */
  if ( SvIOK(item) ) {
    Printf( ("item=integer %d\n", SvIV(item)) );
    if ( -1 == SLang_push_integer( SvIV(item) ) )
      croak( "Unable to push an integer onto the stack" );
    return;
  }

  /* floating-point number */
  if ( SvNOK(item) ) {
    Printf( ("item=float %f\n", SvNV(item)) );
    if ( -1 == SLang_push_double( SvNV(item) ) )
      croak( "Unable to push a double onto the stack" );
    return;
  }

  /* a string */
  if ( SvPOK(item) ) {
    STRLEN len;
    char *ptr = SvPV(item, len);
    Printf(("string: %s\n", ptr));
    if ( -1 == SLang_push_string( ptr ) )
      croak( "Unable to push a string onto the stack" );
    return;
  } /* string 2 S-Lang */

  /* a complex number (Math::Complex object) */
  if ( sv_isobject(item) && sv_derived_from(item, "Math::Complex" ) ) {
    double real, imag;

    Printf( ("*** converting Perl's Math::Complex to S-Lang Complex_Type\n") );

    /* call the Re and Im methods */
    real = call_double_complex_method( item, "Re" );
    imag = call_double_complex_method( item, "Im" );

    /* push the complex number onto the S-Lang stack */
    if ( -1 == SLang_push_complex( real, imag ) )
      croak( "Unable to push a complex number onto the stack" );
    return;
  } /* complex 2 S-Lang */

  /* an Inline::SLang::datatype object */
  if ( sv_isobject(item) && sv_derived_from(item, "Inline::SLang::datatype" ) ) {
    char *name;

    Printf( ("*** converting Inline::SLang::datatype to S-Lang Datatype_Type\n") );

    /* de-reference the object (we can do this since it's our class) */
    name = SvPV_nolen( SvRV(item) );

    /*
     * now, we have the "printed" name of the datatype which we need to
     * convert to the S-Lang datatype. All we do is push the name
     * onto the stack and let S-Lang do the conversion to an
     * actual DataType_Type
     */
    if ( -1 == SLang_load_string( name ) ) /* ignore the trailing ';' that we should have */
      croak( "Unable to create a DataType_Type on the stack" );
    return;
  } /* datatype 2 S-Lang */

  /* an array: always set to SLANG_ANY_TYPE */
  if ( SvROK(item) && SvTYPE(SvRV(item)) == SVt_PVAV ) {
    AV *av = (AV*) SvRV(item);
    SV *elem;
    SLang_Array_Type *at;
    int i;
    int len = av_len(av) + 1;

    fprintf( stderr, "Errr: assuming the perl array reference is 1D!!!\n" );

    Printf( ("array: size=%i\n", len) );
    if ( len == 0 ) {
      /* is this true ? */
      croak("Empty arrays are unsupported in S-Lang (I think)\n");
    }

    if ( NULL ==
	 (at = SLang_create_array(SLANG_ANY_TYPE, 0, NULL, &len, 1)) ) {
	croak("Unable to allocate a S-Lang array of type Any_Type.\n");
    }

    for( i=0; i<len; i++ ) {
      SV *tmp = *av_fetch(av, i, 0);
      /* want this to be recursive here */
      /***
      SLang_set_array_element( at, &i, pl2sl( tmp ) );
      ***/
    }
      
    croak( "Internal Error: currently unable to convert perl array references\n to S-Lang arrays" );

    /***
    o = rb_ary_new2(len);
	
    for (i=0; i<len; i++) {
      SV *tmp = *av_fetch(av, i, 0);
      rb_ary_store(o, i, pl2rb(tmp));
    }
    ***/

    return;
  }

  /*
   * hash ref
   *
   * it doesn't appear that there's much of an API to access S-Lang's
   * associative arrays from C. We could cheat and do it in S-Lang,
   * but that means creating a temporary variable (or having one
   * that we can use [with down-stream issues regarding threading])
   *
   */
  if ( SvROK(item) && SvTYPE(SvRV(item)) == SVt_PVHV ) {
    HV *hv = (HV*) SvRV(item);
    int len = hv_iterinit(hv);
    int i;

    Printf(("perl hash: size=%i\n", len));

    croak( "Internal Error: currently unable to convert perl hash references\n to S-Lang associative arrays" );

    /***
	o = rb_hash_new();

	for (i=0; i<len; i++) {
	    HE *next = hv_iternext(hv);
	    I32 len;
	    char *key = hv_iterkey(next, &len);
	    VALUE key_rb = rb_str_new(key, len);
	    VALUE val_rb = pl2rb(hv_iterval(hv, next));
	    rb_hash_aset(o, key_rb, val_rb);
	}
    ***/

  }

  /*
   * should this be before all the checks so we can catch
   * 'perl scalar blessed into an object' situations?
   *
   * and shouldn't we have check it's a reference first?
   */
  if ( sv_isobject(item) ) {
    croak( "Sorry, I am unable to convert a perl object into a S-Lang datatype\n" );
  }

  croak( "Internal Error: trying to convert a 'strange' perl type to S-Lang\n" );

} /* pl2sl() */

/*
 * utility routines for sl2pl()
 *
 * make_perl_complex() - convert Real,Imag to a Math::Complex object
 * from_perl_complex() - convert Math::Complex object to Real,Imag numbers
 *
 * sl2pl1D() - convert a 1D array into a perl array reference
 * sl2pl2D() - convert a 2D array into a perl array reference
 * sl2plND() - convert a nD array into a perl array reference
 *
 * the array  should handle the integers as does the scalar
 * code (ie respect the types)
 */

static SV *
make_perl_complex( double real, double imag ) {

  dSP; /* this defines variables; it's not a bit of code! */
  SV *object;
  int count;

  /*
   * create the Math::Complex object
   * - see 'perldoc perlcall' for information on what's happening
   */

  ENTER;
  SAVETMPS;
  PUSHMARK(SP);

  /* tried caching "Math::Complex", but it got over-written somewhere */
  XPUSHs(sv_2mortal(newSVpv("Math::Complex",0)));
  XPUSHs(sv_2mortal(newSVnv(real)));
  XPUSHs(sv_2mortal(newSVnv(imag)));
  PUTBACK;

  Printf( ("Calling Math::Complex->make()\n") );
  count = call_method( "make", G_SCALAR );
  
  SPAGAIN;
  if ( count != 1 )
    croak("Math::Complex did not return an object (unable to convert Complex_Type)\n") ;

  /*
   * this is unlikely to be the right thing to do, but we need
   * to do something to stop the object from disappearing
   * once the stack is restored
   */
  object = SvREFCNT_inc( POPs );

  PUTBACK;
  FREETMPS;
  LEAVE;

  return object;

} /* make_perl_complex */

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

static AV *
sl2pl1D( SLang_Array_Type *at ) {
  AV *parray;
  int i;

  Printf( ("*** in sl2pl1D\n*" ) );

  /* create the perl array */
  parray = (AV *) sv_2mortal( (SV *) newAV() );
  av_extend( parray, (I32) at->num_elements );

  /*
   * fill in the data from the array
   * Q: should we be checking the return value of SLang_get_array_element() ?
   */
  switch( at->data_type ) {

    SL2PL_ARRAY1D_ITYPE( CHAR,  char )
    SL2PL_ARRAY1D_ITYPE( SHORT, short )
    SL2PL_ARRAY1D_ITYPE( INT,   int )
    SL2PL_ARRAY1D_ITYPE( LONG,  long )

    SL2PL_ARRAY1D_FTYPE( FLOAT,  float )
    SL2PL_ARRAY1D_FTYPE( DOUBLE, double )

  case SLANG_COMPLEX_TYPE:
    {
      double *dptr;
      double real, imag;

      /* not good */
      fixme( "hacking the internals of Complex_Type arrays [1D]." );

      dptr = (double *) at->data;
      for ( i = 0; i < at->num_elements; i++ ) {
	real = *dptr++;
	imag = *dptr++;
	Printf( ("  complex array1D[%d] = %g + %g i\n", i, real, imag ) );

	/* do we need to bother with memory management here? */
	av_store( parray, i, make_perl_complex(real,imag) );
      }
      break;
    } /* complex */

  case SLANG_STRING_TYPE:
    {
      Printf( ("* array of strings\n") );

      /*
       * thanks to John Davis for pointing out my incorrect pointer
       * use in get_array_element()
       */
      for ( i = 0; i < at->num_elements; i++ ) {
	char *string;
        (void) SLang_get_array_element( at, &i, &string );
	Printf( ( "array string %d = [%s]\n", i, string ) );
	av_store( parray, i, newSVpv( string, 0 ) );
	SLang_free_slstring( string );
      }

      break;
    } /* string */

  case SLANG_DATATYPE_TYPE:
    {
      Printf( ("* array of datatypes\n") );

      croak("Do we require >=v1.4.7 to handle datatype arrays\n");

      //      for ( i = 0; i < at->num_elements; i++ ) {
      //	unsigned char dtype;
      //	(void) SLang_get_array_element( at, &i, &dtype );
      //	Printf( ("array datatype %d = [%d]\n", i, dtype) );
      //
      //	/* assume that the load_string will catch any errors from the push_datatype */
      //	(void) SLang_push_datatype( dtype );
      //	if ( 0 != SLang_load_string( "string();") )
      //	  croak( "Error: unable to execute 'string()' while messing with the stack\n" );
      //	av_store( parray, i,
      //		  sv_bless( newRV_inc( sl2pl() ), gv_stashpv("Inline::SLang::datatype",1) )
      //		  );
      //      } /* for: i */

      break;
    } /* datatype */

  case SLANG_ANY_TYPE:
    {
      /* this is going to be painful w/out re-writing the way we do all this */
      Printf( ("* an array of any types\n") );

      croak("*** Any_Type is not supported yet\n");
      break;
    }
    
  default:
    croak( "Error: 1D arrays of type %d are currently not supported\n", 
	   at->data_type );

  } /* switch: at->data_type */

  /* return the array */
  return parray;

} /* sl2pl1D */

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

static AV *
sl2pl2D( SLang_Array_Type *at ) { 

  AV *xarray, *yarray;
  int dims[2], nx, ny, i, j;

  Printf( ("*** in sl2pl2D\n" ) );
  nx = at->dims[0];
  ny = at->dims[1];

  /* create the perl array */
  xarray = (AV *) sv_2mortal( (SV *) newAV() );
  av_extend( xarray, (I32) nx );

  /*
   * fill in the data from the array
   * Q: should we be checking the return value of SLang_get_array_element() ?
   */
  switch( at->data_type ) {

    SL2PL_ARRAY2D_ITYPE( CHAR,  char )
    SL2PL_ARRAY2D_ITYPE( SHORT, short )
    SL2PL_ARRAY2D_ITYPE( INT,   int )
    SL2PL_ARRAY2D_ITYPE( LONG,  long )

    SL2PL_ARRAY2D_FTYPE( FLOAT,  float )
    SL2PL_ARRAY2D_FTYPE( DOUBLE, double )

  case SLANG_STRING_TYPE:
    {
      char *string;
      Printf( ("* 2D array of strings\n") );

      for ( i = 0; i < nx; i++ ) {
	yarray = (AV *) sv_2mortal( (SV *) newAV() );
	av_extend( yarray, (I32) ny );

	dims[0] = i;
	for ( j = 0; j < ny; j++ ) {
	  dims[1] = j;

	  (void) SLang_get_array_element( at, dims, &string );
	  Printf( ("  string array2D[%d,%d] = %s\n", i, j, string ) );
	  av_store( yarray, j, newSVpv( string, 0 ) );
	  SLang_free_slstring( string );

	} /* for: j */

	av_store( xarray, i, newRV_inc( (SV *) yarray ) );

      } /* for: i */

      break;
    } /* STRINGS-2D */

  case SLANG_COMPLEX_TYPE:
    {
      double *dptr;
      double real, imag;

      /* not good */
      fixme( "hacking the internals of Complex_Type arrays [2D]." );

      dptr = (double *) at->data;
      for ( i = 0; i < nx; i++ ) {
	yarray = (AV *) sv_2mortal( (SV *) newAV() );
	av_extend( yarray, (I32) ny );

	dims[0] = i;
	for ( j = 0; j < ny; j++ ) {
	  dims[1] = j;
	  
	  /* assume we're looping the correct way */
	  real = *dptr++;
	  imag = *dptr++;

	  Printf( ("  complex array2D[%d,%d] = %g + %g i\n", i, j, real, imag ) );

	  /* do we need to bother with memory management here? */
	  av_store( yarray, j, make_perl_complex(real,imag) );
	} /* for: j */

	av_store( xarray, i, newRV_inc( (SV *) yarray ) );

      } /* for: i */

      break;
    } /* COMPLEX-2D */

  default:
    croak( "Error: 2D arrays of type %d are currently not supported\n", 
	   at->data_type );

  } /* switch: at->data_type */

  /* return the array */
  return xarray;

} /* sl2pl2D() */

static AV *
sl2plND( SLang_Array_Type *at ) { 
  croak( "internal error: add support for ND arrays\n" );
  return (AV *) NULL;

} /* sl2plND() */

/*
 * convert S-Lang variables to perl variables
 *
 * note: we pop the current element off the S-Lang stack
 * - this will probably turn out to be a bad idea
 */

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
 * convert the object on the S-Lang stack to
 * a perl object.
 *
 * I would like to not read from the stack, since
 * this makes recursion hard. However, it's not obvious
 * how to handle SLang_Any_Type pointers from C.
 * Perhaps we should still read from the stack and
 * recurse by pushing objects onto the stack?
 * (note: doing this when converting associative arrays)
 */

SV *
sl2pl( void ) {
  int type = SLang_peek_at_stack();

  /*
   * handle the various types
   * - having separate items for all the "integer" types is
   *   probably OTT
   */
  switch( type ) {

  case SLANG_NULL_TYPE:
    {
      /* return an undef */
      Printf( ("  stack contains: NULL\n") );
      /* clear out the stack, assume it works */
      (void) SLdo_pop_n(1);
      return &PL_sv_undef;
      break;
    } /* NULL */

    /* integers */
    SL2PL_ITYPE( CHAR,  char,    char )
    SL2PL_ITYPE( SHORT, short,   short )
    SL2PL_ITYPE( INT,   integer, int )
    SL2PL_ITYPE( LONG,  long,    long )

  case SLANG_FLOAT_TYPE:
    {
      float fval;
      if ( -1 == SLang_pop_float( &fval ) )
	croak( "Error: unable to read float value from the stack\n" );
      Printf( ("  stack contains: float = %g\n", fval ) );
      return newSVnv(fval);
    }

  case SLANG_DOUBLE_TYPE:
    {
      double dval;
      if ( -1 == SLang_pop_double( &dval, NULL, NULL ) )
	croak( "Error: unable to read double value from the stack\n" );
      Printf( ("  stack contains: double = %g\n", dval ) );
      return newSVnv(dval);
    }

  case SLANG_STRING_TYPE:
    {
      SV *out;
      char *sval;
      if ( -1 == SLang_pop_slstring(&sval) )
	croak( "Error: unable to read a string from the stack\n" );
      Printf( ("  stack contains: string = %s\n", sval ) );
      out = newSVpv( sval, 0 );
      SLang_free_slstring( sval );
      return out;
    }

  case SLANG_COMPLEX_TYPE:
    {
      /*
       * store as a Math::Complex object
       */
      double real, imag;

      if ( -1 == SLang_pop_complex( &real, &imag ) )
	croak( "Error: unable to read complex value from the stack\n" );
      Printf( ("  stack contains: complex %g + %g i\n", real, imag ) );

      return make_perl_complex( real, imag );

    } /* COMPLEX */

  case SLANG_ARRAY_TYPE:
    {
      SLang_Array_Type *at;
      AV *parray;

      if ( -1 == SLang_pop_array( &at, 0 ) )
        croak( "Error: unable to pop an array off the stack\n" );
      Printf( ("  stack contains: array type=%d ndims=%d size=%d\n",
	       at->data_type, at->num_dims, at->num_elements ) );

      /* we know that the datatype is constant for all elements */
      switch ( at->num_dims ) {
      case 1: 
	parray = sl2pl1D( at );
	break;

      case 2: 
	parray = sl2pl2D( at );
	break;

      default: 
	parray = sl2plND( at );
	break;
      } /* switch: at->num_dims */

      SLang_free_array( at );
      return newRV_inc( (SV *) parray ); /* this is made mortal by calling function */
      break;
    }

  case SLANG_ASSOC_TYPE:
    {
      HV *harray;
      AV *parray;
      SLang_Array_Type *keys = NULL;

      int i;
      unsigned char type;

      Printf( ("  stack contains an associative array\n") );

      /*
       * use S-Lang to parse the Associative array
       * (approach suggested by John Davis) since there isn't
       * a public C API for them (internals liable to change)
       */
      if ( 0 != SLang_load_string(
		"$1=();assoc_get_values($1);assoc_get_keys($1);"
                ) )
        croak("Error: unable to parse an associative array\n");

      /* we leave the values array on the stack */
      if ( -1 == SLang_pop_array( &keys, 0 ) )
        croak("Error: unable to pop keys array off the stack\n");

      /* more a safety check in case the assoc array interface changes (unlikely) */
      if ( keys->data_type != SLANG_STRING_TYPE )
	croak( "Error: keys of assoc. array not returned as strings\n" );

      /*
       * convert the value S-Lang array to a Perl array reference.
       * We could save *some* time by calling sl2pl1D() directly,
       * but call sl2pl() [which means array must be left on the stack]
       * in case the array-handling code changes.
       */
      parray = (AV *) SvRV( sl2pl() ); /* sl2pl returns an array reference */

      /* create the perl hash array */
      harray = (HV *) sv_2mortal( (SV *) newHV() );

      /* loop through each element, converting the values to Perl types */
      for ( i = 0; i < keys->num_elements; i++ ) {
	SV **value;
	char *keyname;

	/* get the key */
        (void) SLang_get_array_element( keys, &i, &keyname );
	Printf( ( "assoc array key = [%s]\n", keyname ) );

	/* get the value (all the processing has been done by sl2pl()) */
	value = av_fetch( parray, (I32) i, 0 );

	/* store it all */
	hv_store( harray, keyname, strlen(keyname), *value, 0 );

	SLang_free_slstring( keyname );
      }

      /* do I need to free up parray ??? */
      SLang_free_array( keys );
      Printf( ("freed up keys array (S-Lang)\n") );

      return newRV_inc( (SV *) harray ); /* this is made mortal by calling function */
      break;

    } /* ASSOC */

  case SLANG_DATATYPE_TYPE:
    {
      /*
       * store the datatype value as a string of the name,
       * into an Inline::SLang::datatype object
       * we let S-Lang do the conversion to a string
       */
      Printf( ("  stack contains: a S-Lang datatype object\n") );

      /* convert the object on the stack to a string */
      if ( 0 != SLang_load_string( "string();") )
	croak( "Error: unable to execute 'string()' while messing with the stack\n" );
      return
	sv_bless(
		 newRV_inc( sl2pl() ),
		 gv_stashpv("Inline::SLang::datatype",1)
		 );
      break;

    } /* DATATYPE */

  case SLANG_FILE_PTR_TYPE:
    {
      FILE *fp;
      SLang_MMT_Type *mmt;
      if ( -1 == SLang_pop_fileptr( &mmt, &fp ) )
	croak( "Error: unable to read a file pointer from the stack\n" );
      Printf( ("  stack contains: file pointer\n") );
      SLang_free_mmt( mmt );
      croak( "Error: how do I convert a s-lang file pointer into a perl one?\n" );
      break;
    } /* FILE_PTR */

  default:
    croak( "Error: unable to handle a variable with S-Lang type of %d\n", type );
  }

} /* sl2pl() */
