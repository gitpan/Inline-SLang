/****************************************************************************
 * util.c
 * Conversion routines between S-Lang and Perl data types.
 *
 * needs better organisation/planning!
 * - should we have a table of types and functions that
 *   support them, since this would make changes easier/code
 *   more modular, at a possible speed expense?
 *
 * Need to change to return an "opaque" reference for un-defined
 * types (sub-types of Inline::SLang::type say?) so that we can
 * pass around items we don't know about (such as Region_Type
 * from CIAO's region library)
 *
 ****************************************************************************/

/* Supported types (taken from slang.h):

2p - does conversion from S-Lang to Perl work?
2s - does conversion from Perl to S-Lang work?

     s - scalars
     1 - only 1D arrays
     2 - <= 2D arrays
     n - nD arrays


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

s     SLANG_STRUCT_TYPE        Inline::SLang::Struct_Type

  NOTE: Should typedef-fed structures be subclasses of Inline::SLang::struct
   so 'typedef struct { foo, bar } FooBar_Struct;' would create an
   object of class Inline::SLang::struct::FooBar_Struct ?
   Prefer if could be Inline::SLang::FooBar_Struct [but still a sub-class
     of Struct_Type]

s2 s  SLANG_DATATYPE_TYPE      Inline::SLang::DataType_Type

s     SLANG_REF_TYPE           Inline::SLang::Ref_Type

?     SLANG_FILE_PTR_TYPE      should be able to convert to a perl IO variable

?  ?  memory-managed types     Inline::SLang::mmt ???
                               or can we find out their name and use that?

Are we going to support these?
------------------------------

SLANG_ANY_TYPE - should be easy to support since can just convert
  to the correct perl type BUT would we want to keep the
  fact that it's an Any_Type (eg converting perl back to S-Lang
  knows that it's an Any_Type)?

SLANG_UNDEFINED_TYPE - convert to undef ?

SLANG_VOID_TYPE

SLANG_ISTRUCT_TYPE - what is this - an intrinsic structure?

SLANG_BSTRING_TYPE - what is this?
SLANG_FILE_FD_TYPE - what is this?

Unsupported types (taken from slang.h):
---------------------------------------
SLANG_INTP_TYPE - as apparently I shouldn't see one of these

 *
 */

#include "util.h"

/* used by the CALL_xxx macros */
static char *
_get_object_type( SV *obj ) {
  HV *stash = SvSTASH( obj ); /* assume obj really is an object */
  return ( stash ? HvNAME(stash) : "<none>" );
} /* _get_object_type() */

/*
 * since we use $1..$n to create/manipulate variables
 * it can lead to mis-leading results (ie what you think a routine
 * is doing is only happening because $1 happens to be set
 * correctly from a previous call [can happen if test set/get
 * routines next to each other].
 *
 * so, we try and clean things out (setting them to null)
 */
static void
_clean_slang_vars( int n ) {
  char stxt[12]; /* assuming n <= 99 */
  int i;
  for ( i = 1; i <= n; i++ ) {
    (void) sprintf( stxt, "$%d = NULL;", i );
    if ( -1 == SLang_load_string( stxt ) )
      croak( "Internal error: unable to clean up S-Lang $n vars\n" );
  }
} /* _clean_slang_vars() */

/*
 * convert perl variables to S-Lang variables
 *
 * note: we automatically push each variable onto the S-Lang stack
 * - this will probably turn out to be a bad idea; for instance it
 *   means it can't be called recursively when converting
 *   array/associative arrays.
 *
 * - push NULL onto the stack for those variables we do not understand
 *   also - as an aid to debugging - print messages to STDERR
 *   NEED to work out how to inform the calling program that this
 *   transformation has taken place (or do we?)
 */

void
pl2sl( SV *item ) {

  /* undef */
  if ( !SvOK(item) ) {
    Printf( ("item=undef\n") );
    if ( -1 == SLang_push_null() )
      croak( "Unable to push a NULL onto the stack" );
    return;
  } /* undef */

  /* integer */
  if ( SvIOK(item) ) {
    Printf( ("item=integer %d\n", SvIV(item)) );
    if ( -1 == SLang_push_integer( SvIV(item) ) )
      croak( "Unable to push an integer onto the stack" );
    return;
  } /* integer */

  /* floating-point */
  if ( SvNOK(item) ) {
    Printf( ("item=float %f\n", SvNV(item)) );
    if ( -1 == SLang_push_double( SvNV(item) ) )
      croak( "Unable to push a double onto the stack" );
    return;
  } /* floating-point */

  /* string */
  if ( SvPOK(item) ) {
    STRLEN len;
    char *ptr = SvPV(item, len);
    Printf(("string: %s\n", ptr));
    if ( -1 == SLang_push_string( ptr ) )
      croak( "Unable to push a string onto the stack" );
    return;
  } /* string */

  /* Math::Complex */
  if ( sv_isobject(item) && sv_derived_from(item, "Math::Complex" ) ) {
    double real, imag;

    Printf( ("*** converting Perl's Math::Complex to S-Lang Complex_Type\n") );

    /* call the Re and Im methods */
    CALL_METHOD_SCALAR_DOUBLE( item, "Re", , real );
    CALL_METHOD_SCALAR_DOUBLE( item, "Im", , imag );

    /* push the complex number onto the S-Lang stack */
    if ( -1 == SLang_push_complex( real, imag ) )
      croak( "Unable to push a complex number onto the stack" );
    return;
  } /* Math::Complex */

  /* Inline::SLang::DataType_Type */
  if ( sv_isobject(item) &&
       sv_derived_from(item, "Inline::SLang::DataType_Type" ) ) {
    char *name;

    Printf( ("*** converting Inline::SLang::DataType_Type to S-Lang Datatype_Type\n") );

    /* de-reference the object (we can do this since it's our class) */
    name = SvPV_nolen( SvRV(item) );

    /*
     * now, we have the "printed" name of the datatype which we need to
     * convert to the S-Lang datatype. All we do is push the name
     * onto the stack and let S-Lang do the conversion to an
     * actual DataType_Type
     * - not the most efficient implementation but saves messing
     *   with the internals of S-Lang
     */
    if ( -1 == SLang_load_string( name ) ) /* ignores the trailing ';' that we should have */
      croak( "Unable to create a DataType_Type on the stack" );
    return;
  } /* Inline::SLang::DataType_Type */

  /* Inline::SLang::Ref_Type */
  if ( sv_isobject(item) &&
       sv_derived_from(item, "Inline::SLang::Ref_Type" ) ) {

    /*
     * This is not an ideal way to do it since it relies on
     * peaking at library internals - see comments in util.h
     */
    extern int _SLang_push_ref( int is_global, VOID_STAR ptr );
    _Inline_SLang_Ref_Type *sl_ref;
    /*** SLang_Ref_Type *sl_ref; ***/

    SLang_Ref_Type *pl_ref;

    Printf( ("*** converting Inline::SLang::Ref_Type to S-Lang Ref_Type\n") );
    fixme( "stop using library internal routines/structures to access ref types" );

    /*
     * de-reference the object (we can do this since it's our class)
     * - do we need the SvIV() ?
     */
    pl_ref = INT2PTR( SLang_Ref_Type *, SvIV(SvRV(item)) ); 

    /*
     * push the reference onto the stack
     */
    sl_ref = (_Inline_SLang_Ref_Type *) pl_ref;
    if ( -1 ==
         _SLang_push_ref( sl_ref->is_global, (VOID_STAR) sl_ref->v.nt ) )
      croak( "Error: unable to push a Ref_Type object onto the stack" );
    return;
  } /* Inline::SLang::Ref_Type */

  /*
   * Inline::SLang::Struct_Type
   */
  if ( sv_isobject(item) &&
       sv_derived_from(item, "Inline::SLang::Struct_Type" ) ) {

    SV *dstruct;
    SV *aref;
    AV *fields;
    I32 nfields, i;

    Printf( ("*** converting Perl's Inline::SLang::Struct_Type to S-Lang Struct_Type\n") );
    fixme( "memleak" );

    /*
     * should we use the API of the perl object or just access the hash array
     * directly?
     *
     * we actually use a bit of both - not the cleanest/most ideal way of
     * doing this, but it's a bit painful as we want to convert the
     * fields from their perl representation to their S-Lang one
     *
     * one possible improveement (if it works) is to only call
     * Inline::SLang::Struct_Type's get_field() method once - ie stick all
     * the items onto the Perl stack in one go
     */

    /* create a structure in $1 with the correct fields */
    CALL_METHOD_SCALAR_SV( item, "_define_struct", , dstruct );
    Printf( ("struct definition =\n[%s]\n", SvPV_nolen(dstruct)) );
    if ( -1 == SLang_load_string( SvPV_nolen(dstruct) ) )
      croak("Internal Error: unable to create a struct in $1\n");

    /* return the field names (as a string array) */
    CALL_METHOD_SCALAR_SV( item, "get_field_names", , aref );
    //    aref = sv_2mortal( aref ); // randomly trying things
    fields = (AV *) SvRV( aref );
    nfields = 1 + av_len( fields );

    /* MEM:
       have tried 'aref = sv_2mortal( aref );'
       and 'SvREFCNT_inc( *name );'
       - umm, need to look at what call_method_scalar_sv returns
         (is there an increased ref count?)
    */

    /* loop through each field and set its value */
    for ( i = 0; i < nfields; i++ ) {
      SV **name;
      SV *value;

      name = av_fetch( fields, i, 0 );
      // SvREFCNT_inc( *name ); // randomly trying things
      if ( -1 == SLang_push_string( SvPV_nolen(*name) ) )
	croak( "Unable to push a string onto the stack" );

      CALL_METHOD_SCALAR_SV( item, "get_field", C2PL_MARG( *name );, value );
      // pl2sl( sv_2mortal(value) );  // randomly trying things
      pl2sl( value );
      
      if ( -1 ==
	   SLang_load_string( "$2=(); $3=(); set_struct_field( $1, $3, $2 );" ) )
	croak( "Internal Error:\n"
               " Unable to fill in a field (in $3) of a structure (in $1)"
               " with the value in $2\n" );
    }
    
    /* 
     * need to pop item off S-Lang's internal stack and push
     * it onto S-Lang's main stack (or I've messed up above)
     */
    if ( -1 == SLang_load_string("$2=struct {value};set_struct_field($2,\"value\",$1);__push_args($2);") )
      croak("Error: unable to hack a structure together\n");

    _clean_slang_vars(3);
    return;

  } /* Inline::SLang::Struct_Type */

  /* an array reference: always set to SLANG_ANY_TYPE */
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
  } /* array reference */

  /*
   * hash ref
   *
   * there isn't an API to access S-Lang's associative arrays from C. 
   * So it looks like we'll have to let S-Lang do it for us.
   */
  if ( SvROK(item) && SvTYPE(SvRV(item)) == SVt_PVHV ) {
    HV *hv = (HV*) SvRV(item);
    int len = hv_iterinit(hv);
    int i;

    Printf(("perl hash: size=%i\n", len));

    croak( "Internal Error: currently unable to convert perl hash references\n to S-Lang associative arrays" );

    /* create an assoc type array containing Any_Type variables */

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

  } /* hash reference */

    /*
     * if we've got this far then we return NULL
     * . we could croak()
     * . we could call an exception
     * . we could set an error flag
     *
     * ACTUALLY, I think we should probably return a reference to the
     * type - perhaps Inline::SLang::unknown, say,
     * so that we can pass around - if not handle - unknown types
     *
     * the screen messages are temporary
     */
    if ( sv_isobject(item) ) {
      fprintf( stderr, "Internal error: unable to handle objects of type %s\n",
         _get_object_type(item) );
    } else
      fprintf( stderr, "Internal Error: unable to handle this type of perl variable\n" );
    if ( -1 == SLang_push_null() )
      croak( "Unable to push a NULL onto the stack" );

} /* pl2sl() */

/*
 * utility routines for sl2pl()
 *
 * sl2pl1D() - convert a 1D array into a perl array reference
 * sl2pl2D() - convert a 2D array into a perl array reference
 * sl2plND() - convert a nD array into a perl array reference
 *
 * the array  should handle the integers as does the scalar
 * code (ie respect the types)
 */

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
      SV *elem;
      double data[2];

      /* need to check this */
      fixme( "assuming Complex_Type elements of 1D arrays returned as foo[2] arrays." );

      for ( i = 0; i < at->num_elements; i++ ) {

        (void) SLang_get_array_element( at, &i, data );
	Printf( ("  complex array1D[%d] = %g + %g i\n", i, data[0], data[1] ) );

	CALL_METHOD_SCALAR_SV(
	     sv_2mortal(newSVpv("Math::Complex",0)),
             "make",
	     C2PL_MARG_D( data[0] ); C2PL_MARG_D( data[1] );,
             elem );

	/* do we need to bother with memory management here? */
	av_store( parray, i, elem );
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
      int dtype; /* datatypes stored as integers not as SLtype */

      Printf( ("* array of datatypes\n") );

      for ( i = 0; i < at->num_elements; i++ ) {

      	(void) SLang_get_array_element( at, &i, &dtype );

      	Printf( ("array datatype %d = [%d] [name=%s]\n",
		 i, dtype, SLclass_get_datatype_name(dtype)) );

	/* leaks if use newRV [==newRV_inc] */
      	av_store( parray, i,
      		  sv_bless(
			   newRV_noinc( newSVpv( SLclass_get_datatype_name(dtype), 0 ) ),
			   gv_stashpv("Inline::SLang::DataType_Type",1) )
      		  );

      } /* for: i */

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
      double data[2];
      SV *elem;

      /* need to check this */
      fixme( "assuming Complex_Type elements of 2D arrays returned as foo[2] arrays." );

      for ( i = 0; i < nx; i++ ) {
	yarray = (AV *) sv_2mortal( (SV *) newAV() );
	av_extend( yarray, (I32) ny );

	dims[0] = i;
	for ( j = 0; j < ny; j++ ) {
	  dims[1] = j;
	  
	  (void) SLang_get_array_element( at, dims, data );
	  Printf( ("  complex array2D[%d,%d] = %g + %g i\n", i, j, data[0], data[1] ) );

	  /* do we need to bother with memory management here? */
          CALL_METHOD_SCALAR_SV(
	     sv_2mortal(newSVpv("Math::Complex",0)),
             "make",
	     C2PL_MARG_D( data[0] ); C2PL_MARG_D( data[1] );,
             elem );
	  av_store( yarray, j, elem );
	} /* for: j */

	av_store( xarray, i, newRV_inc( (SV *) yarray ) );

      } /* for: i */

      break;
    } /* COMPLEX-2D */

  case SLANG_DATATYPE_TYPE:
    {
      int dtype; /* datatypes stored as integers not as SLtype */

      Printf( ("* 2D array of datatypes\n") );

      for ( i = 0; i < nx; i++ ) {
	yarray = (AV *) sv_2mortal( (SV *) newAV() );
	av_extend( yarray, (I32) ny );

	dims[0] = i;
	for ( j = 0; j < ny; j++ ) {
	  dims[1] = j;

	  (void) SLang_get_array_element( at, dims, &dtype );

	  Printf( ("  datatype array2D[%d,%d] = %d [%s]\n",
		   i, j, dtype, SLclass_get_datatype_name(dtype) ) );

	  av_store( yarray, j,
		    sv_bless(
			     newRV_inc( newSVpv( SLclass_get_datatype_name(dtype), 0 ) ),
			     gv_stashpv("Inline::SLang::DataType_Type",1) )
		    );

	} /* for: j */

	av_store( xarray, i, newRV_inc( (SV *) yarray ) );

      } /* for: i */

      break;
    } /* DATATYPE-2D */

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
 * convert the object on the S-Lang stack to
 * a perl object.
 *
 * I would like to not read from the stack, since
 * this makes recursion hard. However, it's not obvious
 * how to handle SLang_Any_Type pointers from C.
 * Perhaps we should still read from the stack and
 * recurse by pushing objects onto the stack?
 * (note: actually doing this when converting associative arrays)
 */

SV *
sl2pl( void ) {

  /* should we really be using SLtype instead of int? */
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

  case SLANG_ANY_TYPE:
    {
      SLang_Any_Type *any;

      /*
       * can the stack contain one of these?
       * (may be needed to handle recursive parse)
       */
      Printf( ("  stack contains: Any_Type\n") );

      if ( -1 == SLang_pop_anytype( &any ) )
	croak( "Error: unable to pop an Any_Type object from the stack\n" );

      /*** UMMM, I don't know what I'm doing ***/
      //      printf( "debug: Any_Type %d\n", any->data_type );
      //      printf( "debug: class name %s\n", _SLclass_get_class(any->data_type)->name );

      croak( "Error: can not handle an Any_Type object\n" );

      SLang_free_anytype( any );
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
      SV *object;
      double real, imag;

      if ( -1 == SLang_pop_complex( &real, &imag ) )
	croak( "Error: unable to read complex value from the stack\n" );
      Printf( ("  stack contains: complex %g + %g i\n", real, imag ) );

      CALL_METHOD_SCALAR_SV(
       	 sv_2mortal(newSVpv("Math::Complex",0)),
         "make",
	 C2PL_MARG_D( real ); C2PL_MARG_D( imag );,
         object );

      return object;

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
      SV *arrayref;
      SLang_Array_Type *keys = NULL;

      int i;

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
      _clean_slang_vars(1);

      /* we leave the values array on the stack */
      if ( -1 == SLang_pop_array( &keys, 0 ) )
        croak("Error: unable to pop keys array off the stack\n");

      /* something to check in case of errors/changes to internals of S-Lang
       *
       * if ( keys->data_type != SLANG_STRING_TYPE )
       *   croak( "Error: keys of assoc. array not returned as strings\n" );
       */

      /*
       * convert the value S-Lang array to a Perl array reference.
       * We could save *some* time by calling sl2pl1D() directly,
       * but call sl2pl() [which means array must be left on the stack]
       * in case the array-handling code changes.
       */
      arrayref = sv_2mortal( sl2pl() );
      parray = (AV *) SvRV( arrayref );

      /* create the perl hash array (have tried making this non-mortal) */
      harray = (HV *) sv_2mortal( (SV *) newHV() );

      /* loop through each element, converting the values to Perl types */
      for ( i = 0; i < keys->num_elements; i++ ) {
	SV **value;
	char *keyname;

	/* get the key */
        (void) SLang_get_array_element( keys, &i, &keyname );
	Printf( ( "assoc array key = [%s]\n", keyname ) );

	/*
         * get the value (note av_fetch returns a SV **),
         * increase the reference count, and then store it
         * in the perl hash array
         */
	value = av_fetch( parray, (I32) i, 0 );
	SvREFCNT_inc( *value );
	hv_store( harray, keyname, strlen(keyname), *value, 0 );

	SLang_free_slstring( keyname ); // is this necessary?
      }

      /* free up memory */
      SLang_free_array( keys );
      Printf( ("freed up keys array (S-Lang)\n") );

      return newRV_inc( (SV *) harray );
      break;

    } /* ASSOC */

  case SLANG_STRUCT_TYPE:
    {
      SV *object;
      SV *fieldsref;
      AV *fields;
      int i, nfields;

      Printf( ("  stack contains: structure\n") );

      /*
       * return an Inline::SLang::Struct_Type object
       * - handle similarly to associative arrays, in that
       *   we take advantage of the S-Lang stack
       */
      if ( -1 == SLang_load_string( "$1=();get_struct_field_names($1);" ) )
        croak("Error: unable to get the fields of a structure\n");

      /* convert the item on the stack (ie the field names) to a perl array */
      fieldsref = sv_2mortal( sl2pl() );
      fields = (AV *) SvRV( fieldsref );
      nfields = 1 + av_len( fields );
      Printf( ("Number of fields in the structure = %d\n", nfields ) );

      /*
       * create the Inline::SLang::Struct_Type object 
       */
      CALL_METHOD_SCALAR_SV(
        sv_2mortal(newSVpv("Inline::SLang::Struct_Type",0)), 
        "new", XPUSHs(fieldsref);, object );

      /*
       * push all the values onto the S-Lang stack, convert them to
       * the corresponding Perl types, and then set the corresponding
       * field in the Perl object. Note that _push_struct_field_values()
       * returns items in reverse order
       *
       * we're relying on $1 still being defined
       */
      if ( -1 == SLang_load_string(
		"$2 = _push_struct_field_values($1);"
                ) )
        croak("Error: unable to get the values of a structure\n");

      for ( i = 0; i<nfields; i++ ) {
	SV **name;

	/* get the field name */
	name = av_fetch( fields, (I32) i, 0 );
	Printf( ( "struct field name %d = [%s]\n", i, SvPV_nolen(*name) ) );

	/* set the field: the value is read from the S-Lang stack */
        CALL_METHOD_VOID( object,
                          "set_field",
                          XPUSHs(*name); XPUSHs( sv_2mortal(sl2pl()) ); );

      } /* for: i */

      _clean_slang_vars(2);
      return object;
      break;

    } /* STRUCT */

  case SLANG_DATATYPE_TYPE:
    {
      /*
       * store the datatype value as a string of the name,
       * into an Inline::SLang::DataType_Type object
       * we let S-Lang do the conversion to a string
       *
       * ideally would pop the datatype off the stack and then
       * call SLclass_get_datatype_name() on it, but
       * the pop datatype routine is only present in >= v1.4.5
       * and can't be bothered to have 2 code pathways depending
       * on the library version
       */
      Printf( ("  stack contains: a S-Lang datatype object\n") );

      /* convert the object on the stack to a string */
      if ( 0 != SLang_load_string( "string();") )
	croak( "Error: unable to execute 'string()' while messing with the stack\n" );

      /* if use newRV [== newRV_inc] then this leaks memory */
      return
	sv_bless(
		 newRV_noinc( sl2pl() ),
		 gv_stashpv("Inline::SLang::DataType_Type",1)
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

  case SLANG_REF_TYPE:
    {
      /*
       * store as a Inline::SLang::Ref_Type object
       * - relies on S-Lang to work out if the referenced object
       *   remains in scope
       */
      SLang_Ref_Type *sl_ref;
      SV *pl_val;
      SV *pl_ref;

      if ( -1 == SLang_pop_ref( &sl_ref ) )
	croak( "Error: unable to pop a reference from the stack\n" );
      
      Printf( ("DBG: S-Lang ref pointer = %p\n", sl_ref) );

      pl_val = sv_2mortal( newSViv( PTR2IV((void *) sl_ref) ) );
      pl_ref = newRV_inc( pl_val );

      return
	sv_bless( pl_ref, gv_stashpv("Inline::SLang::Ref_Type",1) );
      break;
    } /* REF */

  default:
    {
#ifdef UGLY_HACKS

      /* 
       * pop the object off the stack and then see if we can work
       * with it
       *
       * note:
       *   not good - using a 'private' function to access the class 
       *   information
       * 
       *   also, assuming that SLang_Object_Type == SLang_Any_Type
       *   (which it does now, but can we guarantee that?)
       */

      SLang_Any_Type *obj;

      SLang_Class_Type *_SLclass_get_class( SLtype );
      SLang_Class_Type *cl = _SLclass_get_class( (SLtype) type );
      int class_id = SLclass_get_class_id( cl );

      fixme( "handling of a 'general' S-Lang variable" );

      Printf( ("Trying to handle an unknown type") );
      if ( -1 == SLang_pop_anytype(&obj) )
	croak("Error: unable to pop the next item off the stack");

      printf( "popped off an anytype object\n" );
      printf( "  [stack]   type=%d and class id=%d\n", type, class_id );
      printf( "  [anytype] type=%d\n", obj->data_type );

      croak( "Error: handliong of unknown types is not yet finished.\n" );

      /* currently can't get here */
      SLang_free_anytype( obj );
      return;

#endif /* UGLY_HACKS */

      croak( "Error: unable to handle a variable with S-Lang type of %d\n", type );

    } /* default */
  }

} /* sl2pl() */
