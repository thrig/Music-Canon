#!perl

use strict;
use warnings;

use Test::More tests => 26;
use Test::Exception;

eval 'use Test::Differences';    # display convenience
my $deeply = $@ ? \&is_deeply : \&eq_or_diff;

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

# set intervals by scale name (via Music::Scales)
my $resp = $mc->scale_intervals( 'input', 'major' );
isa_ok( $resp, 'Music::Canon' );

# set intervals manually (also major) - XXX think about whether reverse
# should be top down, as descending is how musician would think about it
$resp = $mc->scale_intervals( 'output',
  [ [qw/2 2 1 2 2 2 1/], [qw/2 2 1 2 2 2 1/] ] );
isa_ok( $resp, 'Music::Canon' );

# XXX not sure about order of things, how reverse is layed out, must
# compare against 'major' vs. passing qw(2 2 1 2 2 2 1) vs. passing qw(2
# 2 1 2 2 2) in vs. differing asc vs dsc intervals sets vs. what
# whatever the modal transmogrifier sub needs to do its thing.
$deeply->(
  [ $mc->get_scale_intervals('input') ],
  [ [qw(2 2 1 2 2 2 1)], [qw(2 2 1 2 2 2 1)] ],
  'major intervals check input'
);
$deeply->(
  [ $mc->get_scale_intervals('output') ],
  [ [qw(2 2 1 2 2 2 1)], [qw(2 2 1 2 2 2 1)] ],
  'major intervals check output'
);

########################################################################
#
# Mappings

$deeply->( [ $mc->exact_map(qw/0 1 2/) ], [qw/-2 -1 0/], 'exact map' );

isa_ok( $mc->exact_map_reset,   'Music::Canon' );
isa_ok( $mc->set_transpose(60), 'Music::Canon' );
$deeply->(
  [ $mc->exact_map(qw/2 9 5 2 1 2 4 5/) ],
  [qw/59 60 62 63 62 59 55 62/]
);

$mc->set_transpose(0);
$deeply->( [ $mc->modal_map(qw/0 2 4 5/) ], [qw/-5 -3 -1 0/], 'modal map' );

isa_ok( $mc->modal_map_reset, 'Music::Canon' );

$deeply->( [ $mc->modal_map(qw/0 3/) ], [qw/-2 0/], 'modal chromatic' );
dies_ok( sub { $mc->modal_map(qw/0 1/) }, 'undefined chromatic' );

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

$mc->set_transpose(q{c'});
is( $mc->get_transpose, 60, 'transpose to lilypond note' );

# some value that should not be set by default
my $rand_transpose = 200 + int rand 100;
$mc->set_transpose($rand_transpose);
is( $mc->get_transpose, $rand_transpose, 'get rand transpose' );

$mc->set_transpose;
is( $mc->get_transpose, 0, 'reset transpose' );
