#!/usr/bin/env perl
use strict;
use warnings;
use utf8;

use FindBin qw($Bin);
use lib $Bin;

use ParserV7;

binmode(STDIN,  ":encoding(UTF-8)");
binmode(STDOUT, ":encoding(UTF-8)");
binmode(STDERR, ":encoding(UTF-8)");

my $grammar = "$Bin/resources/grammar.json";
my $lexicon = "$Bin/resources/lexicon.json";
my $beam    = 8;

# args: --grammar path --lexicon path --beam N
for (my $i=0; $i<@ARGV; $i++) {
    if ($ARGV[$i] eq '--grammar' && defined $ARGV[$i+1]) { $grammar = $ARGV[++$i]; next; }
    if ($ARGV[$i] eq '--lexicon' && defined $ARGV[$i+1]) { $lexicon = $ARGV[++$i]; next; }
    if ($ARGV[$i] eq '--beam'    && defined $ARGV[$i+1]) { $beam = 0 + $ARGV[++$i]; next; }
}

my $p = ParserV7->new(
    grammar_path => $grammar,
    lexicon_path => $lexicon,
    beam         => $beam,
);

print "Ready. Pegá 1 oración por línea (EOF para salir).\n";

while (defined(my $line = <STDIN>)) {
    chomp $line;
    next if $line =~ /^\s*$/;

    my $tree = $p->parse_sentence($line);
    if (!$tree) {
        print "(NO-PARSE)\n";
    } else {
        print $tree, "\n";
    }
}
