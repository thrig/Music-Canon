# -*- Perl -*-
#
# utility routines for musical canon construction

package Music::Canon;

use 5.010000;
use strict;
use warnings;

use Carp qw(croak);
use List::Util qw/sum/;

use Music::LilyPondUtil ();    # transpose convenience
use Music::Scales qw/get_scale_nums is_scale/;
use Scalar::Util qw/blessed looks_like_number/;

our $VERSION = '0.10';

# NOTE a new() param, below, but I have not thought about what changing
# it would actually do. Use the $self entry in all subsequent code.
my $DEG_IN_SCALE = 12;

########################################################################
#
# SUBROUTINES

sub contrary {
  my ( $self, $contrary ) = @_;
  $self->{_contrary} = $contrary ? 1 : 0;
  return $self;
}

# 1:1 interval mapping, though with the contrary, retrograde, and
#   transpose parameters as possible influences on the results.
sub exact_map {
  my $self = shift;

  my @new_phrase;
  for my $e (ref $_[0] eq 'ARRAY' ? @{ $_[0] } : @_ ) {
    my $pitch;
    if ( !defined $e ) {
      # presumably rests/silent bits
      push @new_phrase, undef;
      next;
    } elsif ( blessed $e and $e->can('pitch') ) {
      $pitch = $e->pitch;
    } elsif ( looks_like_number $e) {
      $pitch = $e;
    } else {
      # pass through unknowns
      push @new_phrase, $e;
      next;
    }

    my $new_pitch;
    if ( !defined $self->{_exact}->{prev_output} ) {
      $new_pitch = $pitch + $self->{_transpose};
    } else {
      my $delta = $pitch - $self->{_exact}->{prev_input};
      $delta *= -1 if $self->{_contrary};
      $new_pitch = $self->{_exact}->{prev_output} + $delta;
    }
    push @new_phrase, $new_pitch;
    $self->{_exact}->{prev_input}  = $pitch;
    $self->{_exact}->{prev_output} = $new_pitch;
  }

  @new_phrase = reverse @new_phrase if $self->{_retrograde};

  if ( !$self->{_keep_state} ) {
    undef $self->{_exact}->{prev_input};
    undef $self->{_exact}->{prev_output};
  }

  return @new_phrase;
}

sub exact_map_reset {
  my ($self) = @_;
  undef $self->{_exact}->{prev_input};
  undef $self->{_exact}->{prev_output};
}

sub get_contrary   { $_[0]->{_contrary} }
sub get_retrograde { $_[0]->{_retrograde} }

sub get_scale_intervals {
  my ( $self, $layer ) = @_;
  if ( !defined $layer or ( $layer ne 'input' and $layer ne 'output' ) ) {
    croak "unsupported layer (must be 'input' or 'output')\n";
  }
  if ( !exists $self->{$layer}->{_asc_ints}
    or !exists $self->{$layer}->{_dsc_ints} ) {
    croak "scale intervals for $layer not previously set\n";
  }
  return $self->{$layer}->{_asc_ints}, $self->{$layer}->{_dsc_ints};
}

sub get_transpose {
  my ($self) = @_;
  if ( !exists $self->{_transpose}
    or !defined $self->{_transpose} ) {
    $self->{_transpose} = 0;
  }
  return $self->{_transpose};
}

# Modal interval mapping, where steps taken will vary depending on the
# input and output modes (lists of intervals), where in those modes the
# notes lie, the starting notes, and also the various contrary,
# retrograde, and transpose parameters.
sub modal_map {
  my $self = shift;

  # default to major/major conversion
  if ( !exists $self->{input} ) {
    $self->scale_intervals( 'input', 'major' );
  }
  if ( !exists $self->{output} ) {
    $self->scale_intervals( 'output', 'major' );
  }

  my @new_phrase;
  for my $e (ref $_[0] eq 'ARRAY' ? @{ $_[0] } : @_ ) {
    my $pitch;
    if ( !defined $e ) {
      # presumably rests/silent bits
      push @new_phrase, undef;
      next;
    } elsif ( blessed $e and $e->can('pitch') ) {
      $pitch = $e->pitch;
    } elsif ( looks_like_number $e) {
      $pitch = $e;
    } else {
      # pass through unknowns
      push @new_phrase, $e;
      next;
    }

    $self->{_modal}->{input_start_pitch} = $pitch
      unless defined $self->{_modal}->{input_start_pitch};

    my $new_pitch;
    if ( !defined $self->{_modal}->{output_start_pitch} ) {
      # copy at transpose offset if nothing prior, set things up for
      # subsequent modal foo
      $new_pitch = $pitch + $self->{_transpose};
      $self->{_modal}->{output_start_pitch} = $new_pitch;
    } else {
      # TODO
    }

    push @new_phrase, $new_pitch;
  }

  @new_phrase = reverse @new_phrase if $self->{_retrograde};

  if ( !$self->{_keep_state} ) {
    undef $self->{_modal}->{input_start_pitch};
    undef $self->{_modal}->{output_start_pitch};
  }

  return @new_phrase;
}

# another way to influence modal_map (will be set automatically from the
# phrase and transpose if unset, XXX need to think about persisting them
# across calls, e.g. via sticky_state)
#sub modal_pitch {
#  my ( $self, $layer, $pitch ) = @_;
#
#  if ( !defined $layer or ( $layer ne 'input' and $layer ne 'output' ) ) {
#    croak "unsupported layer (must be 'input' or 'output')\n";
#  }
#  eval {
#    $self->{$layer}->{start_pitch} = $self->{_lyu}->notes2pitches($pitch);
#  };
#  croak $@ if $@;
#
#  return $self;
#}

sub new {
  my ( $class, %param ) = @_;
  my $self = {};

  $self->{_contrary} = exists $param{contrary} ? $param{contrary} ? 1 : 0 : 1;

  $self->{_DEG_IN_SCALE} = int( $param{DEG_IN_SCALE} // $DEG_IN_SCALE );
  if ( $self->{_DEG_IN_SCALE} < 2 ) {
    croak("degrees in scale must be greater than one");
  }

  $self->{_lyu} =
    exists $param{lyu}
    ? $param{lyu}
    : Music::LilyPondUtil->new( mode => 'absolute' );

  $self->{_keep_state} =
    exists $param{keep_state} ? $param{keep_state} ? 1 : 0 : 1;

  $self->{_retrograde} =
    exists $param{retrograde} ? $param{retrograde} ? 1 : 0 : 1;

  if ( exists $param{transpose} ) {
    eval {
      $self->{_transpose} = $self->{_lyu}->notes2pitches( $param{transpose} );
    };
    croak $@ if $@;
  } else {
    $self->{_transpose} = 0;
  }

  # for exceptions in modal mapping (undefined note conversions)
  if ( exists $param{modal_hook} ) {
    croak "modal_hook must be code ref"
      unless ref $param{modal_hook} eq 'CODE';
    $self->{_modal_hook} = $param{modal_hook};
  }

  $self->{_non_octave_scales} = $param{non_octave_scales} // 0;

  bless $self, $class;

  eval {
    if ( exists $param{input} ) {
      $self->set_scale_intervals( 'input', $param{input} );
    }
    if ( exists $param{output} ) {
      $self->set_scale_intervals( 'output', $param{output} );
    }
  };
  croak $@ if $@;

  return $self;
}

sub retrograde {
  my ( $self, $retrograde ) = @_;
  $self->{_retrograde} = $retrograde ? 1 : 0;
  return $self;
}

sub scale_intervals {
  my ( $self, $layer, $scale ) = @_;

  if ( !defined $layer or ( $layer ne 'input' and $layer ne 'output' ) ) {
    croak "unsupported layer (must be 'input' or 'output')\n";
  }

  if ( ref $scale eq 'ARRAY' ) {
    # raw intervals passed as array ref
    croak "intervals must be array of array refs"
      unless ref $scale->[0] eq 'ARRAY';
    croak "asc scales need intervals\n" if @{ $scale->[0] } < 2;
    for my $n ( @{ $scale->[0] } ) {
      croak "asc intervals must be integers\n"
        unless looks_like_number $n and $n =~ m/^[+-]?\d+$/;
    }
    $self->{$layer}->{_asc_ints} = $scale->[0];

    if ( @$scale > 1 ) {
      croak "descending intervals must be array reference\n"
        unless ref $scale->[1] eq 'ARRAY';
      croak "dsc scales need intervals\n" if @{ $scale->[1] } < 2;
      for my $n ( @{ $scale->[1] } ) {
        croak "asc intervals must be integers\n"
          unless looks_like_number $n and $n =~ m/^[+-]?\d+$/;
      }

      $self->{$layer}->{_dsc_ints} = $scale->[1];

    } else {
      # assume dsc is asc (true, excepting melodic minor and whatnot)
      $self->{$layer}->{_dsc_ints} = $self->{$layer}->{_asc_ints};
    }

  } else {
    # derive intervals via scale name via third-party module
    croak "scale unknown to Music::Scales\n" unless is_scale($scale);
    my @asc_nums = get_scale_nums($scale);
    my @dsc_nums = get_scale_nums( $scale, 1 );

    # do not expect this to be a problem, but if it is...
    for my $n ( @asc_nums, @dsc_nums ) {
      die "intervals from Music::Scales not integers"
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
  }

  if ( !$self->{non_octave_scales} ) {
    for
      my $ref ( $self->{$layer}->{_asc_ints}, $self->{$layer}->{_dsc_ints} ) {
      my $interval_sum = sum @$ref;
      if ( $interval_sum < $self->{_DEG_IN_SCALE} ) {
        push @$ref, $self->{_DEG_IN_SCALE} - $interval_sum;
      } elsif ( $interval_sum > $self->{_DEG_IN_SCALE} ) {
        croak "non-octave scales require non_octave_scales param\n";
      }
    }
  }
  $self->{$layer}->{_asc_sum} = sum @{ $self->{$layer}->{_asc_ints} };
  $self->{$layer}->{_dsc_sum} = sum @{ $self->{$layer}->{_dsc_ints} };

  return $self;
}

sub transpose {
  my ( $self, $transpose ) = @_;
  $transpose //= 0;
  eval { $self->{_transpose} = $self->{_lyu}->notes2pitches($transpose); };
  croak $@ if $@;
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
