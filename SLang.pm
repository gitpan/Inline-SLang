
# Inline package for S-Lang (http://www.s-lang.org/)
# - the name has been changed to Inline::SLang since hyphens
#   seem to confuse ExtUtils
#
# Similarities to Inline::Python and Ruby are to be expected
# since I used these modules as a base rather than bother to
# think about things. However, all errors are likely to be
# mine
#

package Inline::SLang;

use strict;

use Carp;
use IO::File;
use Math::Complex;

require Inline;
require DynaLoader;
require Exporter;

require Inline::denter;

use vars qw(@ISA $VERSION @EXPORT_OK);

$VERSION = '0.06';
@ISA = qw(Inline DynaLoader Exporter);
@EXPORT_OK =
    qw(
       sl_eval
       );

# should read ExtUtils::MakeMaker to find out about these
#sub import { Inline::SLag->export_to_level(1,@_); }
#sub dl_load_flags { 0x01 }
Inline::SLang->bootstrap($VERSION);

#==============================================================================
# Register S-Lang.pm as a valid Inline language
#==============================================================================
sub register {
    return {
            language => 'SLang',
            aliases => ['sl', 'slang'], # not sure hyphens are allowed
            type => 'interpreted',
            suffix => 'sldat', # contains source code AND namespace info
           };
}

#==============================================================================
# Validate the S-Lang config options
#==============================================================================
sub usage_validate ($) {
  "'$_[0]' is not a valid configuration option\n";
}

sub usage_config_bind_ns {
  "Invalid value for Inline::SLang option 'BIND_NS';\n" .
    "It must be a string (either \"Global\" or \"All\") or an array reference";
}

sub usage_config_bind_slfuncs {
  "The Inline::SLang option 'BIND_SLFUNCS' must be given an array reference";
}

sub validate {
  my $o = shift;
    
  # default ILSM values
  $o->{ILSM} ||= {};
  # do I need to add support for the FILTERS key in the loop below?
  $o->{ILSM}{FILTERS} ||= [];
  $o->{ILSM}{bind_ns} = [ "Global" ];
  $o->{ILSM}{bind_slfuncs} = [];

  # loop through the options    
  my $flag = 0;
  while ( @_ ) {
    my ( $key, $value ) = ( shift, shift );

    # note: if the user supplies options and they still want the
    # Global namespace bound then they need to include it in the
    # list (ie we over-write the defaults, not append to it)
    #
    if ( $key eq "BIND_NS" ) {
      my $type = ref($value);
      # note: we could make a better stab of ensuring the package name
      # in the 'Global' regexp is correct Perl
      #
      croak usage_config_bind_ns()
	unless ($type eq "" and
		($value =~ m/^Global(=[A-Za-z_0-9]+)?$/ or
		 $value eq "All"))
	or $type eq "ARRAY";
      # we let build() worry about the actual contents
      $o->{ILSM}{bind_ns} = $value;
      next;
    } # BIND_NS

    if ( $key eq "BIND_SLFUNCS" ) {
      my $type = ref($value);
      croak usage_config_bind_slfuncs()
	unless $type eq "ARRAY";
      $o->{ILSM}{bind_slfuncs} = $value;
      next;
    } # BIND_SLFUNCS

    print usage_validate $key;
    $flag = 1;
  }
  die if $flag;

  # set up other useful values 
  # - not the best place to define these
  #   since this is only run when the code has been changed?
  $o->{ILSM}{built}     ||= 0;
  $o->{ILSM}{loaded}    ||= 0;

} # sub: validate()

#==========================================================================
# Pass the code off to S-Lang, let it interpret it, and then
# parse the namespaces to find the functions
#
# Have considered allowing a compile-time option to use a
# byte-compiled version of the code, but decided it was too
# much effort.
#
#==========================================================================
sub build {
    my $o = shift;
    return if $o->{ILSM}{built};

    # Filter the code
    $o->{ILSM}{code} = $o->filter(@{$o->{ILSM}{FILTERS}});

    # bind_ns = [ $ns1, ..., $nsN ]
    # where $ns1 is either the name of the S-Lang
    # namespace (eg "Global") or "Global=foo", 
    # which means to bind S-Lang namespace Global
    # to Perl package foo
    # (not sure if this is really necessary, but it's easy
    #  to implement ;)
    #
    # the keys of %ns_map are the S-Lang namespace names,
    # and the value the Perl package name (they're going to
    # be the same for virtually all cases)
    #
    # It's complicated by allowing bind_ns = "All", which says
    # to bind all known namespaces. We only allow this for
    # S-Lang librarise >= 1.4.7 (since we use the _get_namespaces()
    # function). Use with an earlier S-Lang library causes the
    # code to die (could try and reset to ["Global"] in this
    # case but I think that's going to cause confusion/errors.
    #
    # It's also complicated by allowing the user to specify
    # S-Lang intrinsic functions that are to be bound
    # (bind_slfuncs)
    #
    # First off we need to check for bind_ns eq "All" or "Global"
    my $bind_ns = $o->{ILSM}{bind_ns};
    my $bind_all_ns = 0;
    if ( ref($bind_ns) eq "" ) {
      if ( $bind_ns =~ "^Global" ) { $bind_ns = [ $bind_ns ]; }
      else {
	# if "All" then we have to list all the namespaces,
	# but this is only avalable in >= 1.4.7
	#
	my $ver = sl_eval("_slang_version");
	die "You need at least v1.4.7 of the S-Lang library to use the BIND_NS = \"All\" option." 
	  if $ver < 10407;

	# we will need to append to this after running sl_eval()
	$bind_ns = sl_eval( "_get_namespaces();" );
	$bind_all_ns = 1;
      }			  
    }
    my %ns_map = map {
      my ( $slns, $plns ) = split(/=/,$_,2);
      $plns ||= $slns;
      ( $slns, $plns );
    } @{ $bind_ns };

    # parse the bind_slfuncs information
    my %intrin_funs = map {
      my ( $slfn, $plfn ) = split(/=/,$_,2);
      $plfn ||= $slfn;
      ( $slfn, $plfn );
    } @{ $o->{ILSM}{bind_slfuncs} };

    # What does the current namespace look like before evaluating
    # the user-supplied code?
    # - we only need to worry about those namespaces listed
    #   in the bind_ns array
    #
    # Perhaps we should hack the Perl namespace of Global to main
    # (if it hasn't been explicitly specified)
    #
    my %ns_orig = ();
    foreach my $ns ( keys %ns_map ) {
      # we do not exclude any values in %intrin_funs since
      # they are processed slightly differently from other
      # functions (they can be renamed, but not placed into
      # a different namespace)
      #
      $ns_orig{$ns} = 
      {
	map { ($_,1); } @{ sl_eval( '_apropos("' . $ns . '","",3);' ) || [] }
      };
    }

    # Run the code: sl_eval falls over on error
    # we ignore any output from the eval'd code
    sl_eval( $o->{ILSM}{code} );

    # update the list of namespaces if BIND_NS was set to "All"
    #
    if ( $bind_all_ns ) {
      foreach my $ns ( @{ sl_eval( "_get_namespaces();" ) || [] } ) {
	unless ( exists $ns_map{$ns} ) {
	  $ns_map{$ns} = $ns;
	  $ns_orig{$ns} = {};
	}
      }
    }

    # now find out what we've got available
    # - we use the bind_ns array to tell us what namespaces
    #   to bind to
    #
    # - we bind all functions that are NOT S-Lang intrinsics:
    #   more specifically, we only add those functions that
    #   were added to the S-Lang namespace by the eval call
    #   above
    #
    my %namespaces = ();
    foreach my $ns ( keys %ns_map ) {
      my $funclist = sl_eval( '_apropos("' . $ns . '","",3);' );

      # remove those we already know about
      my $orig = $ns_orig{$ns};
      my @bind = ();
      foreach my $fname ( @$funclist ) {
	push @bind, $fname unless exists $$orig{$fname};
      }

      warn "No functions found in $ns namespace!"
	if $#bind == -1;
      $namespaces{$ns} = \@bind;
    }

    # now bind any S-Lang intrinsics
    # note that they get bound into whatever package the
    # Global namespace is mapped to
    #
    my $href = $ns_orig{Global};
    my $aref = $namespaces{Global};
    while ( my ( $slfn, $plfn ) = each %intrin_funs ) {
      if ( exists $$href{$slfn} ) {
	push @{$aref}, [$slfn,$plfn];
      } else {
	warn "Requested S-Lang intrinsic function $slfn is not found in the Global namespace";
      }
    }

    # now find the defined data types
    # and create the perl classes
    #
    # From slang v1.4.8, the S-Lang defined types that we
    # want to handle are:
    #   Any_Type
    #   BString_Type
    #   FD_Type
    #   File_Type
    #   Ref_Type
    #
    # [would like to handle FD/File handles via PerlIO]
    #
    # The list below is the remaining types - ie those we plan
    # to handle separately - either by using native Perl
    # types or hand-crafter classes
    # (ignoring the fact that 12/14 are both UInteger_Type)
    #
    my %ignore = map { ($_,1); }
      (
       'Undefined_Type', 
       'Integer_Type', 
       'Double_Type', 
       'Char_Type', 
       '_IntegerP_Type', 
       'Complex_Type', 
       'Null_Type', 
       'UChar_Type', 
       'Short_Type', 
       'UShort_Type', 
       'UInteger_Type', 
       'Integer_Type', 
       'String_Type', 
       'Float_Type', 
       'Struct_Type', 
       'Array_Type', 
       'DataType_Type', 
       'Assoc_Type', 
       );

    my $dtypes = Inline::SLang::_sl_defined_types();
    my $classes = "";
    while ( my ( $dname, $dref ) = each %$dtypes ) {
      next if exists $ignore{$dname};

      # create the Perl class code
      if ( $$dref[1] ) {
	# a sub-class of Inline::SLang::Struct_Type
	$classes .= qq{
package Inline::SLang::$dname;
no strict;
\@ISA = ( "Inline::SLang::Struct_Type" );
use strict;
};

	# find out the field names and create the constructor
	my $fields = Inline::SLang::sl_eval(
	     "get_struct_field_names(@" . $dname . ");"
	);

	$classes .=
'sub new {
  my $this  = shift;
  my $class = ref($this) || $this;

  # we have no inputs, since we know what the field names are
  my @fields = ( ' . join(', ',map { "\"$_\""; } @$fields) . ' );
  my $self = {
    fields => [ @fields ],
    data   => { map { ($_,undef) } @fields },
  };
  bless $self, $class;
  return $self;

} # new()
';

      } else {
	# a sub-class of Inline::SLang::_Type
	$classes .= qq{
package Inline::SLang::$dname;
no strict;
\@ISA = ( "Inline::SLang::_Type" );
use strict;
sub new {
  my \$this  = shift;
  my \$class = ref(\$this) || \$this;
  my \$key   = shift;
  return bless \\\$key, \$class;
}
sub DESTROY {
  my \$self = shift;
  Inline::SLang::sl_eval( "_inline->_delete_data(\\"\$\$self\\");" );
}
};

      }
    } # while: each %$dtypes

    # Cache the results
    #
    my $odir = "$o->{API}{install_lib}/auto/$o->{API}{modpname}";
    $o->mkpath($odir) unless -d $odir;

    my $parse_info = Inline::denter->new->indent(
	*namespaces => \%namespaces,
        *sl_types   => $dtypes,
        *pl_classes => $classes,
        *ns_map     => \%ns_map,
	*code       => $o->{ILSM}{code},
    );

    my $odat = $o->{API}{location};
    my $fh = IO::File->new( "> $odat" )
	or croak "Inline::SLang couldn't write parse information!";
    $fh->print( $parse_info );
    $fh->close();

    $o->{ILSM}{namespaces} = \%namespaces;
    $o->{ILSM}{sl_types}   = $dtypes;
    $o->{ILSM}{pl_classes} = $classes;
    $o->{ILSM}{ns_map}     = \%ns_map;
    $o->{ILSM}{built}++;

} # sub: build()

#==============================================================================
# Load the code, run it, and bind everything to Perl
# -- could we store the S-Lang pointers for each function 
#    - ie that returned by SLang_get_function() ?
#      but there may be issues if the function is re-defined
#
# -- is it even worth loading the data from the file, since
#    we can just evaluate it from the data statement (or
#    wherever it is stored within the file). I guess it depends
#    on what the overheads are (especially if we allow filtering)
#    versus file I/O
#
# -- at some point we also create the Perl classes used to represent
#    many of the S-Lang types
#
# Finish by creating the _inline namespace and it's constituents
#   ( type, key ) = _store_data( value );
#   _remove_data( key );
#   _store = Assoc_Type [String_Type]
# 
#==============================================================================
sub load {
    my $o = shift;
    return if $o->{ILSM}{loaded};

    # Load the code
    # - only necessary if we've not already evaluated the code
    #   (part of the build routine)
    #
    unless ( $o->{ILSM}{built} ) {
      my $fh = IO::File->new( "< $o->{API}{location}" )
	or croak "Inline::SLang couldn't open parse information!";
      my $sldat = join '', <$fh>;
      $fh->close();
      
      my %sldat = Inline::denter->new->undent($sldat);
      $o->{ILSM}{namespaces} = $sldat{namespaces};
      $o->{ILSM}{sl_types}   = $sldat{sl_types};
      $o->{ILSM}{pl_classes} = $sldat{pl_classes};
      $o->{ILSM}{ns_map}     = $sldat{ns_map};
      $o->{ILSM}{code}       = $sldat{code};

      # Run it
      sl_eval( $o->{ILSM}{code} );
    }

    # Bind the functions
    # The functions in S-Lang namespace foo
    # are placed into the Perl package bar
    # where foo = $o->{ILSM}{ns_map}{foo}
    #
    # In most cases foo == bar
    # We hack Global so that it appears in
    # main ***UNLESS** the user has specified
    # a name for the Perl package (ie they
    # had BIND_NS => [ ..., "Global=foo", ... ]
    # 
    while ( my ( $slns, $plns ) = each %{ $o->{ILSM}{ns_map} } ) { 
      my $qualname = "$o->{API}{pkg}::";
      $qualname .= "${plns}::" unless 
	$slns eq "Global" && $slns eq $plns;
      foreach my $fn ( @{ $o->{ILSM}{namespaces}{$slns} || [] } ) {
	# if it's an array reference then we have
	# [ $slang_name, $perl_name ]
	# This is currently only for S-Lang intrinsic functions
	#
	my ( $slfn, $plfn );
	if ( ref($fn) eq "ARRAY" ) { $slfn = $$fn[0]; $plfn = $$fn[1]; }
	else                       { $slfn = $fn;     $plfn = $fn; }
	sl_bind_function( "$qualname$plfn", $slns, $slfn );
      }
    }

    # Set up the Perl classes to handle the registered types
    #
    eval $o->{ILSM}{pl_classes};
    die "INTERNAL ERROR: Unable to create Perl classes for S-Lang types\n" .
      "$@\n" if $@;
      
    # bind the _inline namespace
    # v1.4.9 allows eval() to specify the namespace for the code
    # - do not use apostrohpes (') in the S-Lang comments!!!
    sl_eval( 
'
implements("_inline");
private variable _store = Assoc_Type [];
static define _store_data( invar ) {
  % need a unique key for _store: for now go with this
  % simple, but slow algorithm
  %
  variable key = 0;
  while ( assoc_key_exists(_store,string(key)) ) { key++; }
  key = string(key);
  if ( assoc_key_exists(_store,key) ) {
    % want to use exit(), but that is not part of S-Lang; slsh provides it
    error( "Internal error: unable to find a unique key when storing data" );
%    message("Internal error: unable to find a unique key when storing data");
%    exit(1);
  }
  _store[key] = invar;
  return ( "Inline::SLang::" + string(typeof(invar)), key );
} % _store_data

% note: assoc_delete_key() does nothing if the key
% does not exist in the array
%
static define _delete_data( key ) { assoc_delete_key(_store,key); }

% for speed we avoid error checking; if there is an error
% this should cause a S-Lang error
%
static define _push_data( key ) { return _store[key]; }

% useful for debugging
%
static define _dump_data () {
  variable fp;
  switch ( _NARGS )
  { case 0: fp = stdout; }
  { case 1: fp = (); }
  { error( "Internal error: called _inline->dump_data incorrectly" ); }

  () = fprintf( fp, "# Dump of stored S-Lang variables\n" );
  foreach ( _store ) using ( "keys", "values" ) {
    variable k, v;
    ( k, v ) = ();
    () = fprintf( fp, "  %s = \t%s\n", k, string(typeof(v)) );
  }
} % _dump_data
'
	     );
    # do I need to end with an 'implements("Global");' ??

    $o->{ILSM}{loaded}++;

} # sub: load()

#==============================================================================
# Evaluate a string as a piece of S-Lang code
#==============================================================================
sub sl_eval ($) {
    my $str = shift;
    # too lazy to do a possibly-quicker check than this regexp
    $str .= ";" unless $str =~ /;\s*$/;
    return _sl_eval($str);
}

#==============================================================================
# Wrap a S-Lang function with a Perl sub which calls it.
#==============================================================================
sub sl_bind_function {
    my $perlfunc = shift;	# The fully-qualified Perl sub name to create
    my $slangns  = shift;       # The namespace for the S-Lang sub
    my $slangfn  = shift;	# The S-Lang sub name to wrap

    my $qualname;
    if ( $slangns eq "Global" ) {
      $qualname = $slangfn;
    } else {
      $qualname = "${slangns}->${slangfn}";
    }
    
    my $bind = <<END;
sub $perlfunc {
    unshift \@_, "$qualname";
    return &Inline::SLang::sl_call_function;
}
END

    eval $bind;
    croak $@ if $@;
}

#==============================================================================
# Return a small report about the S-Lang code
#==============================================================================

sub info {
    my $o = shift;

    $o->build unless $o->{ILSM}{built};

    my $info = "Configuration details\n---------------------\n\n";

    # get the version of the S-Lang library: if we bind variables then
    # we won't need to do this
    #
    my $ver = sl_eval("_slang_version_string");
    $info .= "Version of S-Lang:\n";
    $info .= "\tcompiled against " . _sl_version() . "\n";
    $info .= "\tusing            $ver\n";
    $info .= "\tand Perl module  $VERSION\n\n";

    $info .= "The following S-Lang types are recognised:\n";
    my $str = "";
    while ( my ( $dname, $dref ) = each %{ $o->{ILSM}{sl_types} } ) {
      $str .= " $dname";
      $str .= " [Struct_Type]" if $$dref[1];
      if ( length($str) > 70 ) {
	$info .= "$str\n";
	$str = "";
      }
    }
    $info .= "$str\n" unless $str eq "";
    $info .= "\n";

    $info .= "The following S-Lang namespaces have been bound to Perl:\n\n";
    while ( my ( $slns, $plns ) = each %{ $o->{ILSM}{ns_map} } ) {

      $plns = "main" if $slns eq "Global" and $slns eq $plns;
      my $aref = $o->{ILSM}{namespaces}{$slns} || [];
      my $nfn  = 1 + $#$aref;
      if ( $nfn == 1 ) {
	$info .= sprintf( "  1 function from namespace %s is bound to package %s\n",
			  $slns, $plns );
      } else {
	$info .= sprintf( "  %d functions from namespace %s are bound to package %s\n",
			  1+$#$aref, $slns, $plns );
      }
      foreach my $fn ( @$aref ) {
	if ( ref($fn) eq "ARRAY" ) {
	  $info .= "\t$$fn[0]() -> $$fn[1]()\n";
	} else {
	  $info .= "\t$fn()\n";
	}
      }
      $info .= "\n";
    }
    return $info;

} # sub: info()

#==============================================================================
# S-Lang datatypes as perl objects, all based on the Inline::SLang::_Type 
# class.
#
# Inline::SLang::_Type
#
# - base class of all the S-Lang types that aren't convertable to a 
#   common Perl type/object
# - essentially all this does (at the moment) is ensure that every class 
#   has 3 methods:
#     an overloaded "print/stringify" function
#     typeof()
#     is_struct_type() [only useful when we support type-deffed structs]
#
#   Might want to add new() to this list (and have it croak)?
#
# To Do:
#    remove Inline::SLang:: from the class names
#
#==============================================================================

package Inline::SLang::_Type;

use strict;
use Carp;

# returns the name of the object w/out the leading 'Inline::SLang' text
sub typeof {
  my $self  = shift;
  my $class = ref($self) || $self;
  return substr($class,15);
}

# pretty printer, which just calls typeof
# [would be quicker to include the typeof code directly]
#
use overload ( "\"\"" => \&Inline::SLang::_Type::stringify );
sub stringify { return $_[0]->typeof(); }

sub is_struct_type { 0; }

#==============================================================================
# Inline::SLang::Struct_Type
#
#  Handle structs.
#  type-deffed structs - e.g. 'typedef { foo, bar } Baz_Type;' -
#  are handled by sub-classing this type
#
# Methods - based on those provided by S-Lang for structures:
#   new()
#   is_struct_type() - actually redefines the base class method
#   get_field_names()
#   get_field() - extended to allow multiple fields
#   set_field() - extended to allow multiple fields
#
# Note:
#   to avoid having to add the same functions into the sub-classes
#   of Inline::SLang::Struct_Type, we make all the functions
#   aware of whether they are being run in the "base" [ie this] class
#   or a derived class (the use of base is incorrect since _Type is
#   really the base class ;)
#   - which probably isn't a clever thing to do
#
# To do:
#   set_fields() - need to think how interacts with set_field() extension
#   _push_field_values()
#
#   either copy() or dup() -- including Mike Nobles's "field-slicing"
#     idea, ie $self->copy("-foo"); removes foo
#
#==============================================================================

package Inline::SLang::Struct_Type;

no strict; # stupid way to get package-scoped global
@ISA = ( "Inline::SLang::_Type" );
use strict;

use Carp;

sub is_struct_type { 1; }

# Struct_Type
# - field names stored as an array
# - data stored as an associative array
# - this method is over-ridden for "named" structs
#
sub new {
    my $this  = shift;
    my $class = ref($this) || $this;

    # input can either be an array reference (deprecate this,
    # or is it easier from C ?) or a list of arguments
    my @names;
    if ( $#_ > 0 ) {
	# all scalars, we hope
	@names = @_;
    } elsif ( $#_ == 0 ) {
	# can be a scalar or array reference
	my $val = shift;
	if ( ref($val) eq "ARRAY" ) {
	    @names = @$val;
	} elsif ( ref($val) ) {
	    die "Error: I don't know how to handle a " . ref($val) .
		" reference";
	} else {
	    push @names, $val;
	}
    }

    # ensure that the field names are all valid:
    # - die if name contains a space, begins with a number
    # - check for multiple versions of the same name
    # - anything else?
    #
    my ( %fields, @fields );
    foreach my $field ( @names ) {
        # should check up on S-Lang's allowable names
	die "Error: field name ($field) is invalid."
	    if $field =~ m/(\s|^\d)/;
	die "Error: attempted to use the same field name ($field) twice creating an $class object"
	    if exists $fields{$field};
	push @fields, $field;
    }

    # make the object
    my $self = {
	fields => [ @fields ], # a copy, not a reference
	data   => { map { ($_,undef) } @fields },
    };
    bless $self, $class;
    return $self;

} # sub: new()

# return an array reference of the field names
# (matches the S-Lang name w/out the 'struct_')
#
# - note: we return a reference to a copy of the array
#         rather than to the array itself
#   well, that's what I want, but I'm not sure I'm actually doing it...
#
sub get_field_names {
    my $self = shift;
    return [ @{ $$self{fields} } ];
}

# read the field data - the functionality is an extension of that
# of S-Lang's get_struct_field() since we allow multiple
# values to be queried at once:
#
#   $val  = $obj->get_field("foo");
#   @vals = $obj->get_field("foo","bar");
#
# if the given field name doesn't exist then we die
#
sub get_field {
    my $self = shift;
    my @ret;
    foreach my $field ( @_ ) {
	if ( exists $$self{data}{$field} ) {
	    push @ret, $$self{data}{$field};
	} else {
	    croak( "The " . ref($self) . " object does not contain the field \"$field\"\n" );
	}
    }
    return wantarray ? @ret : $ret[0];
} # sub: get_field()

# set the field data - the functionality is an extension of that
# of S-Lang's set_struct_field() since we allow multiple
# values to be set at once:
#
#   $obj->set_field( $field1, $val1, $field2, $val2, ... );
#
# if the field doesn't exist then we die
#
# NOTE:
#   may be too confusing with set_fields() [which isn't
#   implemented yet]
#
sub set_field {
    my $self = shift;
    my %hash = @_;
    while ( my ( $field, $value ) = each %hash ) {
	if ( exists $$self{data}{$field} ) {
	    $$self{data}{$field} = $value;
	} else {
	    croak( "The " . ref($self) . " object does not contain the field \"$field\"\n" );
	}
    }
} # sub: set_field()

# over-ride the default object "stringification"
# - include structure type for "named" structs
#
use overload ( "\"\"" => \&Inline::SLang::Struct_Type::stringify );
sub stringify {
  my $self = shift;
  my $string = "Structure Type: " . $self->typeof() . "\n";
  foreach my $field ( @{ $$self{fields} } ) {
    $string .= "\t$field\t= $$self{data}{$field}\n";
  }
  return $string;
}

## private methods for this object (no guarantee they will
## remain - or behave the same - between releases)

# returns the S-Lang code necessary to create a struct
# with the correct fields in $1, but doesn't actually execute it
# (since this would convert it back into Perl which we don't want)
#
# we make this code also handle the case when called from a sub-class
# of Inline::SLang::Struct_Type
#
sub _define_struct {
  my $self  = shift;
  my $class = ref($self) ||
    die "Error: Inline::SLang::Struct_Type::_define_struct() can not be called as a class method";
  $class = substr( $class, 15 );
  if ( $class eq "Struct_Type" ) {
    return "\$1 = struct { " . join( ', ', @{ $$self{fields} } ) . " };";
  } else {
    return "\$1 = \@$class;"; # a lot easier ;)
  }

} # sub: _define_struct()

#==============================================================================
# Inline::SLang::DataType_Type
#
# - the type is returned as a string (which is the output of
#   'typeof(foo);' for the S-Lang variable foo)
# - the string is blessed into the Inline::SLang::DataType_Type object
# - we use S-Lang to create a DataType_Type variable so that we can
#     a) check we have a datatype
#     b) handle type synonyms correctly
#
# - if given an incorrect type name you WILL get ugly error messages printed
#   to STDERR from S-Lang
#
#==============================================================================

package Inline::SLang::DataType_Type;

no strict; # stupid way to get package-scoped global
@ISA = ( "Inline::SLang::_Type" );
use strict;

# delegate all the checking to S-Lang itself, so that
# we can handle class synonyms
#
# cheat and say an empty constructor creates a datatype_type
#
sub new {
    my $this  = shift;
    my $class = ref($this) || $this;
    my $self  = shift || "DataType_Type";

    # this will convert class synonyms to their "base" class
    #
    # I guess with clever coding then you could get S-Lang
    # to evaluate something here, but I am not going to worry about
    # that (since you can call sl_eval() directly...)
    #
    my ( $flag, $val );
    eval qq{
      ( \$flag, \$val ) =
        Inline::SLang::sl_eval(
          "typeof($self)==DataType_Type;string($self);"
        );
    };

    # return undef on failure
    return undef unless defined $flag and $flag;

    return bless \$val, $class;
} # sub: new()

# over-ride the base 'stringify' method
# since we actually want to print out the actual datatype,
# and not that this is a DataType_Type object
#
use overload ( "\"\"" => \&Inline::SLang::DataType_Type::stringify );
sub stringify { return ${$_[0]}; }

#==============================================================================
# Inline::SLang::Assoc_Type
#
# Let's try and handle Assoc_Type arrays as tied hashes so that we can retain
# knowledge of the S-Lang type of the fields.
#
#==============================================================================

=begin FORTHEFUTURE

package Inline::SLang::Assoc_Type;

require Tie::Hash;

no strict; # stupid way to get package-scoped global
@ISA = qw( "Tie::ExtraHash" );
use strict;

# Usage:
#   tie %foo, Inline::SLang::Assoc_Type;
#   tie %foo, Inline::SLang::Assoc_Type, "Any_Type";
#   tie %foo, Inline::SLang::Assoc_Type, "String_Type";
#   tie %foo, Inline::SLang::Assoc_Type, "Array_Type"; ???
#
# these correspond to S-Lang's
#
#   variable foo = Assoc_Type [];
#   variable foo = Assoc_Type [Any_Type];
#   variable foo = Assoc_Type [String_Type];
#   variable foo = Assoc_Type [Array_Type];
#
# At the moment we do not allow the default value, although that should
# be easy to implement
#
# the object is a 2-element array: the first element is the hash
# reference that Tie::ExtraHash needs; the second is a string representing
# the type of the data stored in the array
#

# or at least that's the plan

=end FORTHEFUTURE

=cut

#==============================================================================

# End
1;
