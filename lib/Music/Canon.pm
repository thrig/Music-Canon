# -*- Perl -*-

package Music::Canon;

use 5.010000;
use strict;
use warnings;

use Carp qw(croak);

use List::Util qw/sum/;
use Music::Scales qw/get_scale_nums is_scale/;
use Scalar::Util qw/looks_like_number/;

our $VERSION = '0.01';

my $DEG_IN_SCALE = 12;

########################################################################
#
# SUBROUTINES

sub new {
  my ( $class, %param ) = @_;
  my $self = {};

  # maybe
  # $self->{_keep_state}        = $param{keep_state}        // 1;
  # $self->{_sticky_state}      = $param{sticky_state}      // 0;

  # maybe
  # if ( exists $param{TODO_hook} ) {
  #   croak "TODO_hook must be code ref"
  #     unless ref $param{TODO_hook} eq 'CODE';
  #   $self->{_TODO_hook} = $param{TODO_hook};
  # }

  $self->{_non_octave_scales} = $param{non_octave_scales} // 0;

  bless $self, $class;

  eval {
    if ( exists $param{input_scale} ) {
      $self->set_scale_intervals( 'input', $param{input_scale} );
    }
    if ( exists $param{output_scale} ) {
      $self->set_scale_intervals( 'output', $param{output_scale} );
    }
  };
  croak $@ if $@;

  return $self;
}

sub modal_transmogrify {
  my ( $self, $pset ) = @_;

}

sub get_scale_intervals {
  my ( $self, $layer ) = @_;
  unless ( $layer eq 'input' or $layer eq 'output' ) {
    croak "unsupported layer (must be input or output)\n";
  }
  if ( !exists $self->{$layer}->{_asc_ints}
    or !exists $self->{$layer}->{_dsc_ints} ) {
    croak "scale intervals for $layer not previously set\n";
  }
  return $self->{$layer}->{_asc_ints}, $self->{$layer}->{_dsc_ints};
}

sub set_scale_intervals {
  my ( $self, $layer, $scale, $dsc ) = @_;

  unless ( $layer eq 'input' or $layer eq 'output' ) {
    croak "unsupported layer (must be input or output)\n";
  }

  my ( @asc_nums, @dsc_nums );
  if ( ref $scale eq 'ARRAY' ) {
    # raw intervals passed as array ref
    @asc_nums = @$scale;
    croak "asc scales need intervals\n" if @asc_nums < 2;
    if ( defined $dsc ) {
      croak "descending intervals must be array reference\n"
        unless ref $dsc eq 'ARRAY';
      @dsc_nums = @$dsc;
      croak "dsc scales need intervals\n" if @dsc_nums < 2;
    } else {
      # assume dsc is asc unless otherwise stated
      @dsc_nums = @asc_nums;
    }

  } else {
    # derive intervals via scale name via third-party module
    croak "scale unknown to Music::Scales\n" unless is_scale($scale);
    @asc_nums = get_scale_nums($scale);
    @dsc_nums = get_scale_nums( $scale, 1 );
  }

  for my $n ( @asc_nums, @dsc_nums ) {
    croak "intervals must be integers\n"
      unless looks_like_number $n and $n =~ m/^[+-]?\d+$/;
  }

  for my $i ( 1 .. $#asc_nums ) {
    push @{ $self->{$layer}->{_asc_ints} },
      $asc_nums[$i] - $asc_nums[ $i - 1 ];
  }
  for my $i ( reverse 1 .. $#dsc_nums ) {
    push @{ $self->{$layer}->{_dsc_ints} },
      $dsc_nums[ $i - 1 ] - $dsc_nums[$i];
  }

  unless ( $self->{non_octave_scales} ) {
    for
      my $ref ( $self->{$layer}->{_asc_ints}, $self->{$layer}->{_dsc_ints} ) {
      my $interval_sum = sum @$ref;
      if ( $interval_sum < $DEG_IN_SCALE ) {
        push @$ref, $DEG_IN_SCALE - $interval_sum;
      } elsif ( $interval_sum > $DEG_IN_SCALE ) {
        croak "scales >${DEG_IN_SCALE} require non_octave_scales param\n";
      }
    }
  }
  $self->{$layer}->{_asc_sum} = sum @{ $self->{$layer}->{_asc_ints} };
  $self->{$layer}->{_dsc_sum} = sum @{ $self->{$layer}->{_dsc_ints} };

  return $self;
}

1;
__END__

=head1 NAME

Music::Canon - utility routines for musical canon construction

=head1 SYNOPSIS

  use Music::Canon;
  TODO

=head1 DESCRIPTION

Utility routines for musical canon construction.

=head1 SEE ALSO

"The Technique of Canon" by Hugo Norden

=head1 AUTHOR

Jeremy Mates, E<lt>jmates@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2013 by Jeremy Mates

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself, either Perl version 5.16 or, at
your option, any later version of Perl 5 you may have available.

=cut
