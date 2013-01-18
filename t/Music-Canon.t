#!perl

use strict;
use warnings;

use Test::More;    # plan is down at bottom
use Test::Exception;

eval 'use Test::Differences';    # display convenience
my $deeply = $@ ? \&is_deeply : \&eq_or_diff;

# for modal_map tests, see perldocs for chart showing where these occur
my @major_to_major_undefined = qw/-11 -4 1 8 13 20/;

########################################################################
#
# Defaults and Initial Mode Setup

BEGIN { use_ok('Music::Canon') }

my $mc = Music::Canon->new;
isa_ok( $mc, 'Music::Canon' );

# defaults
is( $mc->get_transpose,  0, 'default transpose' );
is( $mc->get_contrary,   1, 'default contrary' );
is( $mc->get_retrograde, 1, 'default retrograde' );

# major/major the default
$deeply->(
  [ $mc->get_scale_intervals('input') ],
  [ [qw(2 2 1 2 2 2 1)], [qw(1 2 2 2 1 2 2)] ],
  'major intervals check input'
);
$deeply->(
  [ $mc->get_scale_intervals('output') ],
  [ [qw(2 2 1 2 2 2 1)], [qw(1 2 2 2 1 2 2)] ],
  'major intervals check output'
);

# set intervals by scale name (via Music::Scales)
my $resp = $mc->set_scale_intervals( 'input', 'aeolian' );
isa_ok( $resp, 'Music::Canon' );

# or by interval (aeolian again)
$resp = $mc->set_scale_intervals( 'output', [qw/2 1 2 2 1 2 2/] );
isa_ok( $resp, 'Music::Canon' );

$deeply->(
  [ $mc->get_scale_intervals('input') ],
  [ [qw(2 1 2 2 1 2 2)], [qw(2 2 1 2 2 1 2)] ],
  'minor intervals check input'
);
$deeply->(
  [ $mc->get_scale_intervals('output') ],
  [ [qw(2 1 2 2 1 2 2)], [qw(2 2 1 2 2 1 2)] ],
  'minor intervals check output'
);

########################################################################
#
# Mappings

$mc = Music::Canon->new;

$deeply->( [ $mc->exact_map(qw/0 1 2/) ], [qw/-2 -1 0/], 'exact map' );
isa_ok( $mc->exact_map_reset, 'Music::Canon' );

# vs. individual calls for each note
{
  my @input = qw/0 1 2/;
  my @output;
  for my $p (@input) {
    push @output, $mc->exact_map($p);
  }

  # Retrograde is meaningless if doing note-by-note calls, as there is
  # never anything that can be reversed...
  @output = reverse @output if $mc->get_retrograde;

  $deeply->( \@output, [qw/-2 -1 0/], 'exact map multiple calls' );
  $mc->exact_map_reset;
}

isa_ok( $mc->set_transpose(60), 'Music::Canon' );
$deeply->(
  [ $mc->exact_map(qw/2 9 5 2 1 2 4 5/) ],
  [qw/59 60 62 63 62 59 55 62/]
);

$mc->set_transpose(0);

$deeply->(
  [ $mc->modal_map(qw/0 2 4 5 7 9 11 12 14 16 17 19/) ],
  [qw/-19 -17 -15 -13 -12 -10 -8 -7 -5 -3 -1 0/],
  'modal map diatonics up'
);
isa_ok( $mc->modal_map_reset, 'Music::Canon' );

$deeply->(
  [ $mc->modal_map(qw/0 -1 -3 -5 -7 -8 -10 -12 -13 -15/) ],
  [qw/16 14 12 11 9 7 5 4 2 0/],
  'modal map diatonics down'
);
$mc->modal_map_reset;

# TODO also chromatics up and down

# TODO iterate over list of ones that should all fail, some in 2nd
# octave as well
$deeply->( [ $mc->modal_map(qw/0 3/) ], [qw/-2 0/], 'modal chromatic' );
$mc->modal_map_reset;

for my $i (@major_to_major_undefined) {
  dies_ok(
    sub {
      my @surprise = $mc->modal_map( 0, $i );
      diag "error: instead of an exception got: @surprise\n";
    },
    'undefined chromatic conversion'
  );
  # no per-loop-item reset as all start with 0 and then something that
  # should cause an exception
}
$mc->modal_map_reset;

# TODO test other modal maps, like pentatonic to other things, Forte
# Numbers, melodic minor asc and dsc

########################################################################
#
# getters/setters

$mc = Music::Canon->new;

isa_ok( $mc->set_contrary(0), 'Music::Canon' );
is( $mc->get_contrary, 0, 'set contrary false' );
$mc->set_contrary(1);
is( $mc->get_contrary, 1, 'set contrary true' );

isa_ok( $mc->set_retrograde(0), 'Music::Canon' );
is( $mc->get_retrograde, 0, 'set retrograde false' );
$mc->set_retrograde(1);
is( $mc->get_retrograde, 1, 'set retrograde true' );

# transpose to a note defers the conversion to a pitch until have the
# starting pitch of the input phrase so can convert from that pitch to
# the desired lilypond note
$mc->set_transpose(q{c'});
is( $mc->get_transpose, q{c'}, 'transpose to lilypond note' );

# some value that should not be set by default
my $rand_transpose = 200 + int rand 100;
$mc->set_transpose($rand_transpose);
is( $mc->get_transpose, $rand_transpose, 'get rand transpose' );

$mc->set_transpose;
is( $mc->get_transpose, 0, 'reset transpose' );

isa_ok( $mc->set_modal_pitches( 99, 100 ), 'Music::Canon' );
$deeply->( [ $mc->get_modal_pitches ], [ 99, 100 ], 'lookup modal pitches' );

########################################################################
#
# some more set_scale_intervals tests - plenty to go wrong

# melodic minor from Music::Scales and then manually
$mc->set_scale_intervals( 'output', 'mm' );
my @mm_via_scales = $mc->get_scale_intervals('output');

$mc->set_scale_intervals(
  'output',
  [ 2, 1, 2, 2, 2, 2 ],
  [ 2, 1, 2, 2, 1, 2 ]
);
my @mm_via_intervals = $mc->get_scale_intervals('output');
$deeply->(
  \@mm_via_intervals, \@mm_via_scales, 'Music::Scales vs. raw intervals'
);

# Forte Numbers!
$mc->set_scale_intervals( 'input', '5-35', '5-25' );
$deeply->(
  [ $mc->get_scale_intervals('input') ],
  [ [qw/2 2 3 2 3/], [qw/4 3 2 1 2/] ],
  'scale intervals by Forte'
);

$mc = Music::Canon->new( non_octave_scales => 1 );
my @run_up   = 59 .. 86;
my @run_down = 32 .. 59;
$deeply->( [ $mc->exact_map(@run_up) ], \@run_down, 'exact run up' );
$mc->exact_map_reset;
$deeply->(
  [ $mc->exact_map( reverse @run_down ) ],
  [ reverse @run_up ],
  'exact run down'
);

# non_octave_scales tests - whole tone non-octave bounded modal_map is
# identical to exact_map (only more expensive to compute), due to the
# even interval spacing. However, it did uncover an edge case at the
# interval sum boundary of modal mapping, among other bugs.
$mc->set_scale_intervals( 'input',  '6-35' );
$mc->set_scale_intervals( 'output', '6-35' );
$deeply->(
  [ $mc->modal_map(@run_up) ],
  \@run_down, 'whole tone modal run up'
);
$mc->modal_map_reset;
$deeply->(
  [ $mc->modal_map( reverse @run_down ) ],
  [ reverse @run_up ],
  'whole tone modal run down'
);

# next would be 5-25, which should *not* line up on the 12-pitch octave
# with non-octave bounding (not that that octave has much to do with the
# algo, perhaps mostly to see what results are produced)

plan tests => 37 + @major_to_major_undefined;
