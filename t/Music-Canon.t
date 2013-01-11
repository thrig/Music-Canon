#!perl

use strict;
use warnings;

use Test::More tests => 4;
BEGIN { use_ok('Music::Canon') }

my $mc = Music::Canon->new;
isa_ok( $mc, 'Music::Canon' );

my $resp = $mc->set_scale_intervals('input', 'major');
isa_ok( $resp, 'Music::Canon' );

# XXX not sure about order of things, how reverse is layed out, must
# compare against 'major' vs. passing qw(2 2 1 2 2 2 1) vs. passing qw(2
# 2 1 2 2 2) in vs. differing asc vs dsc intervals sets vs. what
# whatever the modal transmogrifier sub needs to do its thing.
is_deeply(
  [ $mc->get_scale_intervals('input') ],
  [ [qw(2 2 1 2 2 2 1)], [qw(2 2 1 2 2 2 1)] ],
  'major intervals check'
);
