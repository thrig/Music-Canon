#!perl

use strict;
use warnings;

use Test::More tests => 14;
BEGIN { use_ok('Music::Canon') }

eval 'use Test::Differences';    # display convenience
my $deeply = $@ ? \&is_deeply : \&eq_or_diff;

########################################################################
#
# Defaults and Initial Mode Setup

my $mc = Music::Canon->new;
isa_ok( $mc, 'Music::Canon' );

# defaults
is( $mc->get_transpose, 0, 'default transpose' );
is( $mc->get_contrary,  1, 'default contrary' );
is( $mc->get_retrograde,  1, 'default retrograde' );

# set intervals by scale name (via Music::Scales)
my $resp = $mc->scale_intervals( 'input', 'major' );
isa_ok( $resp, 'Music::Canon' );

# set intervals manually (also major) - XXX think about whether reverse
# should be top down, as descending is how musician would think about it
$mc->scale_intervals( 'output',
  [ [qw/2 2 1 2 2 2 1/], [qw/2 2 1 2 2 2 1/] ] );

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

########################################################################
#
# getters/setters

$mc = Music::Canon->new;

$mc->contrary(0);
is( $mc->get_contrary, 0, 'set contrary false' );
$mc->contrary(1);
is( $mc->get_contrary, 1, 'set contrary true' );

$mc->transpose(q{c'});
is( $mc->get_transpose, 60, 'transpose to lilypond note' );

# some value that should not be set by default
my $rand_transpose = 200 + int rand 100;
$mc->transpose($rand_transpose);
is( $mc->get_transpose, $rand_transpose, 'get rand transpose' );

$mc->transpose;
is( $mc->get_transpose, 0, 'reset transpose' );
