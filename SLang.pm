
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

$VERSION = '0.04';
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
  "Invalid value for Inline::SLang option 'BIND_NS'; must be string or array reference";
}

sub validate {
  my $o = shift;
    
  # default ILSM values
  $o->{ILSM} ||= {};
  # do I need to add support for the FILTERS key in the loop below?
  $o->{ILSM}{FILTERS} ||= [];
  $o->{ILSM}{bind_ns} = [ "Global" ];

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
      croak usage_config_bind_funcs()
	unless $type eq "" or $type eq "ARRAY";

      $value = [ $value ] if $type eq "";
      $o->{ILSM}{bind_ns} = $value;
      next;
    }

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

    my @ns = @{ $o->{ILSM}{bind_ns} };

    # What does the current namespace look like before evaluating
    # the user-supplied code?
    # - we only need to worry about those namespaces listed
    #   in the bind_ns array
    #
    my %ns_orig = ();
    foreach my $ns ( @ns ) {
      $ns_orig{$ns} = 
      {
	map { ($_,1); } @{ sl_eval( '_apropos("' . $ns . '","",3);' ) || [] }
      };
    }

    # Run the code: sl_eval falls over on error
    # we ignore any output from the eval'd code
    sl_eval( $o->{ILSM}{code} );

    # now find out what we've got available
    # - we use the bind_ns array to tell us what namespaces
    #   to bind to ("" means the 'Global' namespace)
    #
    # - we bind all functions that are NOT S-Lang intrinsics:
    #   more specifically, we only add those functions that
    #   were added to the S-Lang namespace by the eval call
    #   above
    #
    my %namespaces = ();
    foreach my $ns ( @ns ) {
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

    # Cache the results
    #
    my $odir = "$o->{API}{install_lib}/auto/$o->{API}{modpname}";
    $o->mkpath($odir) unless -d $odir;

    my $parse_info = Inline::denter->new->indent(
	*namespaces => \%namespaces,
	*code       => $o->{ILSM}{code},
    );

    my $odat = $o->{API}{location};
    my $fh = IO::File->new( "> $odat" )
	or croak "Inline::SLang couldn't write parse information!";
    $fh->print( $parse_info );
    $fh->close();

    $o->{ILSM}{namespaces} = \%namespaces;
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
      $o->{ILSM}{code}       = $sldat{code};

      # Run it
      sl_eval( $o->{ILSM}{code} );
    }

    # Bind the functions
    # ns=Global goes into the package namespace,
    # otherwise goes into package::ns namespace
    # - this may not be a good idea and perhaps should
    #   be configurable -- eg
    #     BIND_NS => [ "foo=>bar", "Global" ],
    #   or
    #     BIND_NS => [ ["foo","bar"], "Global" ] ],
    #   to say stick S-Lang ns foo into perl's bar
    #
    foreach my $ns ( keys %{ $o->{ILSM}{namespaces} } ) {
      my $qualname = "$o->{API}{pkg}::";
      $qualname .= "${ns}::" unless $ns eq "Global";
      foreach my $fn ( @{ $o->{ILSM}{namespaces}{$ns} || [] } ) {
	sl_bind_function( "$qualname$fn", $ns, $fn );
      }
    }

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
    $info .= "Version of S-Lang library:\n";
    $info .= "\tcompiled against " . _sl_version() . "\n";
    $info .= "\tusing            $ver\n\n";

    # always print this header, whether we've bound anything or not
    $info .= "The following S-Lang functions have been bound to Perl:\n\n";

    foreach my $ns ( keys %{ $o->{ILSM}{namespaces} } ) {
      my $aref = $o->{ILSM}{namespaces}{$ns} || [];
      $info .= sprintf( "Namespace $ns contains %d bound function(s).\n",
			1+$#$aref );
      foreach my $fn ( @$aref ) { $info .= "\t$fn()\n"; }
      $info .= "\n";
    }

    return $info;

} # sub: info()

#==============================================================================
# S-Lang datatypes as perl objects
#
# The objects are:
#    Inline::SLang::datatype
#    Inline::SLang::struct
#
#==============================================================================

package Inline::SLang::datatype;

# Datatype_Type
# - the type is returned as a string (which is the output of
#   'typeof(foo);' for the S-Lang variable foo)
# - the string is blessed into the Inline::SLang::datatype object
#

# currently we just take the string and bless it into this
# class. So, there's no error checking.
#
# - perhaps we should call S-Lang to do this
#
sub new () {
    my $this  = shift;
    my $class = ref($this) || $this;

    # "make" the object
    my $name = shift || "";
    my $self = \$name;
    bless $self, $class;
    return $self;
} # sub: new()

# pretty printer
use overload ( "\"\"" => \&Inline::SLang::datatype::stringify );
sub stringify { return ${$_[0]}; }

#==============================================================================
# Inline::SLang::struct
#==============================================================================

package Inline::SLang::struct;

# note:
#   tThere are private methods which are only meant to be used by
#   this module when converting between Perl and S-Lang datatypes.
#   These begin with a '_' character.
#   There's no guarantee that they will remain the same/exist in
#   other versions of the module, so don't use ;)
#

use Carp;

# Struct_Type
# - let's see how this works
# - field names stored as an array
# - data stored as an associative array
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
	die "Error: attempted to use the same field name ($field) twice creating an Inline::SLang::struct object"
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
# - note: we return a reference to a copy of the array
#         rather than to the array itself
#   well, that's what I want, but I'm not sure I'm actually doing it...
#
# perhaps this should match the name of the corresponding S-Lang function?
#
sub fields {
    my $self = shift;
    return [ @{ $$self{fields} } ];
} # sub: fields()

# access the field data
#   $val  = $obj->get("foo");
#   @vals = $obj->get("foo","bar");
#
# if the given field name doesn't exist then we die
#
sub get {
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
} # sub: get()

# set the field data
#   $obj->set( $field1, $val1, $field2, $val2, ... );
#
# sets the given field(s) to the supplied value
# if the field doesn't exist then we die
#
sub set {
    my $self = shift;
    my %hash = @_;
    while ( my ( $field, $value ) = each %hash ) {
	if ( exists $$self{data}{$field} ) {
	    $$self{data}{$field} = $value;
	} else {
	    croak( "The " . ref($self) . " object does not contain the field \"$field\"\n" );
	}
    }
} # sub: set()

# pretty printer - act a bit like print
use overload ( "\"\"" => \&Inline::SLang::struct::stringify );
sub stringify {
    my $self = shift;
    my $string = "";
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
sub _define_struct {
    my $self = shift;
    return "\$1 = struct { " . join( ', ', @{ $$self{fields} } ) . " };";
} # sub: _define_struct()

1;




