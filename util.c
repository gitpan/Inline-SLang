/****************************************************************************
 * util.c
 * Conversion routines between S-Lang and Perl data types.
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
      "named" structs          Inline::SLang::<<Name>>

s2 s  SLANG_DATATYPE_TYPE      Inline::SLang::DataType_Type

Other types, such as SLANG_REF_TYPE:

s  s  SLANG_UNDEFINED_TYPE     Inline::SLang::Undefined_Type
                               Perhaps should use undef for this and make
                               Null_Type convert to Inline::SLang::Null_Type
                               [although I'm not sure whether a user will
                                see an Undefined_Type object]

s  s  SLANG_REF_TYPE           Inline::SLang::Ref_Type
s  s  SLANG_ANY_TYPE           Inline::SLang::Any_Type
s  s  SLANG_BSTRING_TYPE       Inline::SLang::BString_Type
                               May want to allow access to this data?

s  s  SLANG_FILE_PTR_TYPE      I think it will be hard to convert these into
s  s  SLANG_FILE_FD_TYPE       a PerlIO * object, since 'man perlapio' seems
                               to suggest that PerlIO_importFILE() can be
                               used to convert a FILE * to a PerlIO * object
                               BUT this then can only be closed via the
                               PerlIO object. Which is impossible (I think)
                               to enforce (ie can't add a hook to the
                               S-Lang destructor?)

                               So, they are stored as "opaque" types
                               Inline::SLang::FD_Type/File_Type

s  s  SLANG_INTP_TYPE          Inline::SLang::_IntegerP_Type
                               [apparently shouldn't see one of these]

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
 * could also free up some memory quicker
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
 * - we croak for those types we do not recognise
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

  /*
   * Inline::SLang::Struct_Type and derived types
   */
  if ( sv_isobject(item) &&
       sv_derived_from(item, "Inline::SLang::Struct_Type" ) ) {

    SV *dstruct;
    SV *aref;
    AV *fields;
    I32 nfields, i;

    Printf( ("*** converting Perl struct to S-Lang\n") );
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

  /*
   * an array reference
   * - need to think about what we are going to convert this to
   */
  if ( SvROK(item) && SvTYPE(SvRV(item)) == SVt_PVAV ) {
    AV *av = (AV*) SvRV(item);
    SV *elem;
    SLang_Array_Type *at;
    int i;
    int len = av_len(av) + 1;

    fprintf( stderr, "Errr: assuming the perl array reference is 1D!!!\n" );

    croak( "Internal Error: currently unable to convert perl array references\n to S-Lang arrays" );

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
   * if we are derived from Inline::SLang::_Type and we've got this
   * far then assume we're a type that Perl can't handle directly
   * (ie the S-Lang data is actually stored in_inline->_store[])
   *
   * Perhaps we need to add a routine to the _Type class to indicate
   * this condition?
   */
  if ( sv_isobject(item) &&
       sv_derived_from(item, "Inline::SLang::_Type" ) ) {

    SV *perlkey;
    
    Printf( ("*** converting Perl _Type object to S-Lang\n") );

    /*
     * de-reference the object to get at the string it contains
     * - let's hope you can't get this far with a _Type object that
     *   doesn't store data this way...
     */
    pl2sl( SvRV(item) );
    if ( -1 == SLang_load_string( "$1 = (); _inline->_push_data( $1 );" ) )
      croak( "Error: unable to convert a variable to S-Lang" );
    _clean_slang_vars(1);

    return;

  } /* Inline::SLang::_Type */

    /*
     * if we've got this then croak
     * Would be nice to be more informative
     */
  croak( "Sent a perl type that can not be converted to S-Lang" );

} /* pl2sl() */

/*
 * utility routines for sl2pl()
 *
 * sl2pl1D() - convert a 1D array into a perl array reference
 * sl2pl2D() - convert a 2D array into a perl array reference
 * sl2plND() - convert a nD array into a perl array reference
 *
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

  default:
    croak( "Error: 1D arrays of type %s are currently not supported\n", 
	   SLclass_get_datatype_name(at->data_type) );

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
    croak( "Error: 2D arrays of type %s are currently not supported\n", 
	   SLclass_get_datatype_name(at->data_type) );

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
 * Convert S-Lang structs - including type-deffed ones
 * to Perl Inline::SLang::<> objects
 *
 * If we were just bothered about S-lang structs - ie not the
 * type-deffed ones - then we could just have this code directly
 * in sl2pl() within a "case SLANG_STRUCT_TYPE: {}".
 * However, as I don't know how we can easily tell whether an
 * item on the S_Lang stack is a type-deffed structure we go
 * with this method
 *
 * We are called with a S-Lang structure in $1
 */
SV *
sl2pl_struct(void) {
  char *stype;
  SV *object;
  SV *fieldsref;
  AV *fields;
  int i, nfields;

  Printf( ("  stack contains: structure\n") );

  /*
   * get the Perl class name for this structure
   * (let S-Lang bother with the string handling)
   */
  if ( -1 == SLang_load_string( "\"Inline::SLang::\" + string(typeof($1));") ||
       -1 == SLang_pop_slstring(&stype) )
    croak( "Error: unable to get datatype of a structure\n" );
  Printf( ("  it's type is %s\n",stype) );

  /*
   * - handle similarly to associative arrays, in that
   *   we take advantage of the S-Lang stack
   */
  if ( -1 == SLang_load_string( "get_struct_field_names($1);" ) )
    croak("Error: unable to get the fields of a structure\n");

  /* convert the item on the stack (ie the field names) to a perl array */
  fieldsref = sv_2mortal( sl2pl() );
  fields = (AV *) SvRV( fieldsref );
  nfields = 1 + av_len( fields );
  Printf( ("Number of fields in the structure = %d\n", nfields ) );

  /*
   * create the Inline::SLang::<XXX> object 
   */
  CALL_METHOD_SCALAR_SV(
			sv_2mortal(newSVpv(stype,0)), 
			"new", XPUSHs(fieldsref);, object );

  /* free up the type string */
  SLang_free_slstring( stype );

  /*
   * push all the values onto the S-Lang stack, convert them to
   * the corresponding Perl types, and then set the corresponding
   * field in the Perl object. Note that _push_struct_field_values()
   * returns items in reverse order
   *
   * we're relying on $1 still containing the S-Lang structure
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

} /* sl2pl_struct() */

/*
 * Handle S-Lang variables for which we 
 * consider the type to be "opaque" in Perl scope - ie
 * you can assign it to a variable and send it back to S-Lang
 * but there's not a lot else you can do with it.
 * To do this we store the variable in the _inline namespace
 * and return the index string for that variable. This
 * variable gets converted to a Perl object of class
 * Inline::SLang::<typeof S-Lang variable>, which inherits
 * from Inline::SLang::_Type.
 * See the definition of the _inline namespace in SLang.pm
 * (created during the load phase of processing)
 *
 * We are called with a S-Lang variable in $1
 */
SV *
sl2pl_opaque(void) {
  char *sltype;
  char *slkey;
  SV *perlobj;

  if ( -1 == SLang_load_string( "_inline->_store_data( $1 );" ) ||
       -1 == SLang_pop_slstring(&slkey) ||
       -1 == SLang_pop_slstring(&sltype) )
    croak( "Error: unable to store S-Lang data" );
  _clean_slang_vars(1);
  Printf( ("Storing S-Lang type %s using key %s\n", sltype, slkey) );

  /*
   * Now create an object of the right type
   */
  CALL_METHOD_SCALAR_SV(
			sv_2mortal(newSVpv(sltype,0)),
			"new",
			C2PL_MARG_S( slkey ),
			perlobj );
  return perlobj;

} /* sl2pl_opaque() */

/*
 * convert S-Lang variables to perl variables
 */

/*
 * convert the object on the S-Lang stack to
 * a perl object.
 *
 * The use of the S-Lang stack may limit recursion,
 * but it's easy to stick values back onto the S-Lang
 * stack. In fact, we make use of this when processing
 * certain types.
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
      if ( -1 == SLang_load_string( "string();") )
	croak( "Error: unable to execute 'string()' while messing with the stack\n" );

      /* if use newRV [== newRV_inc] then this leaks memory */
      return
	sv_bless(
		 newRV_noinc( sl2pl() ),
		 gv_stashpv("Inline::SLang::DataType_Type",1)
		 );
      break;

    } /* DATATYPE */

  default:
    {
      /*
       * There are 2 cases:
       *  - a struct, including type-deffed ones
       *  - everything else
       */
      int is_struct;
      if ( -1 == SLang_load_string( "$1 = (); is_struct_type($1);" ) ||
	   -1 == SLang_pop_integer( &is_struct ) )
	croak( "Error: unable to pop an item from the S-Lang stack" );

      if ( is_struct )
	return sl2pl_struct();
      else
	return sl2pl_opaque();

    } /* default */
  }

} /* sl2pl() */
