package ParserV7;

use strict;
use warnings;
use utf8;
use JSON::PP ();
use Scalar::Util qw(looks_like_number);

# ----------------------------
# Constructor / Resource load
# ----------------------------
sub new {
    my ($class, %opts) = @_;
    my $self = bless {
        beam        => $opts{beam} // 8,

        sym2id      => {},   # string -> int
        id2sym      => [undef],  # 1-based

        start_sym   => 0,

        lex_by_word => {},   # word -> [lex_entry...]
        rules       => [],

        unary_index => {},   # rhs1 -> [rule_idx...]
        binary_index=> {},   # "rhs1,rhs2" -> [rule_idx...]

        arena       => [undef],  # 1-based nodes
    }, $class;

    if ($opts{grammar_path} && $opts{lexicon_path}) {
        $self->load_resources($opts{grammar_path}, $opts{lexicon_path});
    }
    return $self;
}

sub load_resources {
    my ($self, $grammar_path, $lexicon_path) = @_;

    my $g = _read_json($grammar_path);
    my $l = _read_json($lexicon_path);

    my $start = $g->{start} // $g->{root} // $g->{start_symbol} // 'S';
    $self->{start_sym} = $self->_intern($start);

    $self->_load_lexicon($l);
    $self->_load_grammar($g);
    $self->_build_rule_index();

    return 1;
}

sub _read_json {
    my ($path) = @_;
    open my $fh, '<:encoding(UTF-8)', $path or die "Cannot open $path: $!";
    local $/;
    my $txt = <$fh>;
    close $fh;

    my $json = JSON::PP->new->utf8->relaxed->decode($txt);
    return $json;
}

# ----------------------------
# Symbol table
# ----------------------------
sub _intern {
    my ($self, $s) = @_;
    $s //= '';
    $s =~ s/^\s+|\s+$//g;
    return 0 if $s eq '';

    my $key = uc($s);
    if (exists $self->{sym2id}{$key}) {
        return $self->{sym2id}{$key};
    }
    my $id = scalar(@{$self->{id2sym}});
    $self->{sym2id}{$key} = $id;
    $self->{id2sym}[$id] = $key;
    return $id;
}

sub _sym_str {
    my ($self, $id) = @_;
    return '<?>' if !$id || $id >= @{$self->{id2sym}};
    return $self->{id2sym}[$id];
}

# ----------------------------
# Lexicon loader
# ----------------------------
sub _load_lexicon {
    my ($self, $l) = @_;
    $self->{lex_by_word} = {};

    my $entries = $l->{entries} // $l->{lexicon} // [];
    $entries = [] unless ref($entries) eq 'ARRAY';

    for my $e (@$entries) {
        next unless ref($e) eq 'HASH';

        my $word = $e->{word} // $e->{form} // '';
        $word = lc($word);

        my $pos  = $e->{pos} // $e->{tag} // 'X';
        my $w    = defined($e->{weight}) ? $e->{weight} : (defined($e->{prob}) ? $e->{prob} : 1.0);
        $w = 1e-12 if !looks_like_number($w) || $w <= 0;

        my ($fk, $fv) = _read_feats($self, $e);
        my ($sig, $feat_hash) = _feats_signature($fk, $fv);

        my $entry = {
            word => $word,
            pos  => $self->_intern($pos),
            logw => log($w),
            feats=> $feat_hash,  # key_id -> val_id
            sig  => $sig,
        };

        push @{$self->{lex_by_word}{$word}}, $entry;
    }
}

sub _read_feats {
    my ($self, $e) = @_;
    my $feats = $e->{feats} // $e->{features};
    return ([], []) if !defined $feats;

    my (@fk, @fv);

    if (ref($feats) eq 'HASH') {
        for my $k (keys %$feats) {
            my $v = $feats->{$k};
            push @fk, $self->_intern($k);
            push @fv, $self->_intern("$v");
        }
    }
    elsif (ref($feats) eq 'ARRAY') {
        for my $p (@$feats) {
            next unless ref($p) eq 'HASH';
            next unless exists $p->{k} && exists $p->{v};
            push @fk, $self->_intern($p->{k});
            push @fv, $self->_intern($p->{v});
        }
    }

    return (\@fk, \@fv);
}

sub _feats_signature {
    my ($fk, $fv) = @_;
    my %h;
    for (my $i=0; $i<@$fk; $i++) {
        my $k = $fk->[$i] // 0;
        my $v = $fv->[$i] // 0;
        next if !$k || !$v;
        $h{$k} = $v; # last wins
    }
    my @k = sort { $a <=> $b } keys %h;
    my @pairs = map { $_ . ':' . $h{$_} } @k;
    my $sig = join(',', @pairs);
    return ($sig, \%h);
}

# ----------------------------
# Grammar loader
# ----------------------------
sub _load_grammar {
    my ($self, $g) = @_;
    $self->{rules} = [];

    my $rules = $g->{rules} // $g->{productions} // [];
    $rules = [] unless ref($rules) eq 'ARRAY';

    for my $r (@$rules) {
        next unless ref($r) eq 'HASH';

        my $lhs = $self->_intern($r->{lhs} // '<?>');

        my $rhs = $r->{rhs} // $r->{rhs_symbols};
        my @rhs;
        if (ref($rhs) eq 'ARRAY') {
            @rhs = map { "$_" } @$rhs;
        } else {
            # tolerate rhs1/rhs2
            my $r1 = $r->{rhs1};
            my $r2 = $r->{rhs2};
            @rhs = grep { defined($_) && "$_" ne '' } ($r1, $r2);
        }
        @rhs = map { $self->_intern($_) } @rhs;

        next if @rhs < 1 || @rhs > 2;

        my $w = defined($r->{weight}) ? $r->{weight} : (defined($r->{prob}) ? $r->{prob} : 1.0);
        $w = 1e-12 if !looks_like_number($w) || $w <= 0;

        my $prop = uc($r->{propagate} // 'MERGE');
        $prop = 'MERGE' if $prop eq '';

        my $cs_arr = $r->{constraints} // $r->{conds} // [];
        $cs_arr = [] unless ref($cs_arr) eq 'ARRAY';

        my @cs;
        for my $c (@$cs_arr) {
            next unless ref($c) eq 'HASH';
            push @cs, {
                type    => uc($c->{type}   // 'REQUIRE'),
                target  => uc($c->{target} // 'LEFT'),
                target2 => uc($c->{target2} // ($c->{other} // 'RIGHT')),
                key     => $self->_intern($c->{key}   // ''),
                val     => $self->_intern($c->{value} // ''),
                key2    => $self->_intern($c->{key2}  // ''),
            };
        }

        push @{$self->{rules}}, {
            lhs     => $lhs,
            rhs_len => scalar(@rhs),
            rhs1    => $rhs[0],
            rhs2    => ($rhs[1] // 0),
            logw    => log($w),
            prop    => $prop,
            cs      => \@cs,
        };
    }
}

sub _build_rule_index {
    my ($self) = @_;
    $self->{unary_index}  = {};
    $self->{binary_index} = {};

    for (my $i=0; $i<@{$self->{rules}}; $i++) {
        my $r = $self->{rules}[$i];
        if ($r->{rhs_len} == 1) {
            push @{$self->{unary_index}{$r->{rhs1}}}, $i;
        } else {
            my $k = $r->{rhs1} . ',' . $r->{rhs2};
            push @{$self->{binary_index}{$k}}, $i;
        }
    }
}

# ----------------------------
# Public: parse
# ----------------------------
sub parse_sentence {
    my ($self, $sentence) = @_;

    my @t0 = $self->_tokenize($sentence);
    my @t  = $self->_split_morphology(@t0);
    my $n  = scalar(@t);
    return '' if $n == 0;

    $self->{arena} = [undef];

    # chart[i][j] for 1<=i<=n, i<j<=n+1
    my @chart;
    for my $i (1..$n) {
        $chart[$i] ||= [];
        $chart[$i][$i+1] = _init_cell();
        $self->_seed_lexical($chart[$i][$i+1], $t[$i-1]);
        $self->_unary_closure($chart[$i][$i+1]);
    }

    for my $span (2..$n) {
        for my $i (1..($n-$span+1)) {
            my $j = $i + $span;
            my $cell = _init_cell();

            for my $k ($i+1..$j-1) {
                my $left  = $chart[$i][$k];
                my $right = $chart[$k][$j];
                next if !$left || !$right;
                $self->_combine_cells($cell, $left, $right);
            }

            $self->_unary_closure($cell);
            $chart[$i][$j] = $cell;
        }
    }

    my ($best_node, $best_score) = $self->_pick_best_root($chart[1][$n+1]);
    return '' if !$best_node;
    return $self->_render_tree($best_node);
}

# ----------------------------
# Tokenization + morphology
# ----------------------------
sub _tokenize {
    my ($self, $s) = @_;
    $s //= '';
    my @t = ($s =~ /([\p{L}\p{N}_]+)/ug);
    @t = map { lc($_) } @t;
    return @t;
}

sub _split_morphology {
    my ($self, @toks) = @_;
    my @out;

    my @encl = qw(me te se lo la los las le les nos os);

    for my $t (@toks) {
        if ($t eq 'al') {
            push @out, 'a', 'el';
            next;
        }
        if ($t eq 'del') {
            push @out, 'de', 'el';
            next;
        }

        my ($base, $suf) = _maybe_split_enclitic($t, \@encl);
        if (defined($suf) && $suf ne '') {
            push @out, $base, $suf;
        } else {
            push @out, $t;
        }
    }
    return @out;
}

sub _maybe_split_enclitic {
    my ($tok, $encl) = @_;
    # Heurística: <verbo>(r|d|n) + pronombre átono
    for my $s (@$encl) {
        next if length($tok) <= length($s) + 2;
        next unless $tok =~ /\Q$s\E$/;
        my $cut = length($tok) - length($s);
        my $base = substr($tok, 0, $cut);
        my $last = substr($base, -1, 1);
        if ($last eq 'r' || $last eq 'd' || $last eq 'n') {
            my $suf = substr($tok, $cut);
            return ($base, $suf);
        }
    }
    return ($tok, '');
}

# ----------------------------
# CKY cell + beam insert
# ----------------------------
sub _init_cell {
    return { items => [], bykey => {} }; # bykey: "cat|sig" -> index
}

sub _cell_insert {
    my ($self, $cell, $it) = @_;
    my $key = $it->{cat} . '|' . $it->{sig};

    # dedup
    if (exists $cell->{bykey}{$key}) {
        my $idx = $cell->{bykey}{$key};
        my $cur = $cell->{items}[$idx];
        if ($it->{score} > $cur->{score}) {
            $cell->{items}[$idx] = $it;
        }
        _sort_and_trim($cell->{items}, $self->{beam});
        _rebuild_bykey($cell);
        return;
    }

    # add
    push @{$cell->{items}}, $it;
    _sort_and_trim($cell->{items}, $self->{beam});
    _rebuild_bykey($cell);
}

sub _sort_and_trim {
    my ($items, $beam) = @_;
    @$items = sort { $b->{score} <=> $a->{score} } @$items;
    if (@$items > $beam) {
        splice(@$items, $beam);
    }
}

sub _rebuild_bykey {
    my ($cell) = @_;
    my %h;
    for (my $i=0; $i<@{$cell->{items}}; $i++) {
        my $it = $cell->{items}[$i];
        $h{$it->{cat}.'|'.$it->{sig}} = $i;
    }
    $cell->{bykey} = \%h;
}

# ----------------------------
# Arena nodes + items
# ----------------------------
sub _add_leaf_node {
    my ($self, $label, $leaf) = @_;
    push @{$self->{arena}}, {
        label  => $label,
        left   => 0,
        right  => 0,
        isLeaf => 1,
        leaf   => $leaf,
    };
    return $#{$self->{arena}};
}

sub _add_unary_node {
    my ($self, $label, $child) = @_;
    push @{$self->{arena}}, {
        label  => $label,
        left   => $child,
        right  => 0,
        isLeaf => 0,
        leaf   => '',
    };
    return $#{$self->{arena}};
}

sub _add_binary_node {
    my ($self, $label, $l, $r) = @_;
    push @{$self->{arena}}, {
        label  => $label,
        left   => $l,
        right  => $r,
        isLeaf => 0,
        leaf   => '',
    };
    return $#{$self->{arena}};
}

sub _make_item {
    my ($self, $cat, $score, $node, $feats) = @_;
    my ($sig, $fh) = _hash_to_signature($feats);
    return {
        cat   => $cat,
        score => $score,
        node  => $node,
        feats => $fh,
        sig   => $sig,
    };
}

sub _hash_to_signature {
    my ($feats) = @_;
    $feats ||= {};
    my @k = sort { $a <=> $b } keys %$feats;
    my @pairs = map { $_ . ':' . $feats->{$_} } @k;
    my $sig = join(',', @pairs);
    my %h = %$feats;
    return ($sig, \%h);
}

# ----------------------------
# Lexical seeding
# ----------------------------
sub _seed_lexical {
    my ($self, $cell, $word) = @_;
    my $wl = lc($word // '');

    my $entries = $self->{lex_by_word}{$wl};
    if ($entries && @$entries) {
        for my $e (@$entries) {
            my $node = $self->_add_leaf_node($e->{pos}, $word);
            my $it = $self->_make_item($e->{pos}, $e->{logw}, $node, $e->{feats});
            $self->_cell_insert($cell, $it);
        }
        return;
    }

    # OOV fallback
    my $cat = ($word =~ /^\p{Lu}/u) ? $self->_intern('PROPN') : $self->_intern('NOUN');
    my $node = $self->_add_leaf_node($cat, $word);
    my $it = $self->_make_item($cat, log(1e-6), $node, {});
    $self->_cell_insert($cell, $it);
}

# ----------------------------
# Unary closure
# ----------------------------
sub _unary_closure {
    my ($self, $cell) = @_;
    my $changed = 1;
    my $iter = 0;

    while ($changed && $iter < 64) {
        $iter++;
        $changed = 0;

        my @snapshot = @{$cell->{items}};
        for my $src (@snapshot) {
            my $rhs1 = $src->{cat};
            my $idxs = $self->{unary_index}{$rhs1} // [];
            next unless @$idxs;

            for my $ri (@$idxs) {
                my $rule = $self->{rules}[$ri];

                my ($ok, $out_feats) = $self->_apply_unary_constraints($rule, $src->{feats});
                next unless $ok;

                my $node = $self->_add_unary_node($rule->{lhs}, $src->{node});
                my $it = $self->_make_item(
                    $rule->{lhs},
                    $src->{score} + $rule->{logw},
                    $node,
                    $out_feats
                );

                my $before = scalar(@{$cell->{items}});
                $self->_cell_insert($cell, $it);
                my $after  = scalar(@{$cell->{items}});
                $changed ||= ($after > $before); # conservative
            }
        }
    }
}

sub _apply_unary_constraints {
    my ($self, $rule, $child_feats) = @_;
    my %out = %{$child_feats || {}};

    for my $c (@{$rule->{cs}}) {
        my $typ = $c->{type};
        if ($typ eq 'REQUIRE') {
            my $v = $out{$c->{key}} // 0;
            return (0, undef) if $v != $c->{val};
        } elsif ($typ eq 'ASSIGN') {
            $out{$c->{key}} = $c->{val} if $c->{key} && $c->{val};
        }
    }
    return (1, \%out);
}

# ----------------------------
# Combine (binary rules)
# ----------------------------
sub _combine_cells {
    my ($self, $out, $left, $right) = @_;

    for my $L (@{$left->{items}}) {
        for my $R (@{$right->{items}}) {

            my $key = $L->{cat} . ',' . $R->{cat};
            my $idxs = $self->{binary_index}{$key} // [];
            next unless @$idxs;

            for my $ri (@$idxs) {
                my $rule = $self->{rules}[$ri];

                my ($ok, $out_feats) = $self->_apply_binary_constraints($rule, $L->{feats}, $R->{feats});
                next unless $ok;

                my $node = $self->_add_binary_node($rule->{lhs}, $L->{node}, $R->{node});
                my $it = $self->_make_item(
                    $rule->{lhs},
                    $L->{score} + $R->{score} + $rule->{logw},
                    $node,
                    $out_feats
                );
                $self->_cell_insert($out, $it);
            }
        }
    }
}

sub _apply_binary_constraints {
    my ($self, $rule, $lf, $rf) = @_;
    $lf ||= {}; $rf ||= {};

    # propagate
    my %out;
    my $prop = $rule->{prop} // 'MERGE';
    if ($prop eq 'LEFT') {
        %out = %$lf;
    } elsif ($prop eq 'RIGHT') {
        %out = %$rf;
    } else {
        %out = %$lf;
        for my $k (keys %$rf) {
            $out{$k} = $rf->{$k} unless exists $out{$k};
        }
    }

    for my $c (@{$rule->{cs}}) {
        my $typ = $c->{type};

        if ($typ eq 'REQUIRE') {
            my $src = ($c->{target} && $c->{target} eq 'RIGHT') ? $rf : $lf;
            my $v = $src->{$c->{key}} // 0;
            return (0, undef) if $v != $c->{val};

        } elsif ($typ eq 'UNIFY') {
            my $k = $c->{key};
            my $v1 = $lf->{$k} // 0;
            my $v2 = $rf->{$k} // 0;
            return (0, undef) if ($v1 && $v2 && $v1 != $v2);
            $out{$k} = $v1 if $v1;
            $out{$k} = $v2 if !$v1 && $v2;

        } elsif ($typ eq 'AGREE') {
            my $k = $c->{key};
            my $v1 = $lf->{$k} // 0;
            my $v2 = $rf->{$k} // 0;
            return (0, undef) if (!$v1 || !$v2 || $v1 != $v2);
            $out{$k} = $v1;

        } elsif ($typ eq 'ASSIGN') {
            $out{$c->{key}} = $c->{val} if $c->{key} && $c->{val};
        }
    }

    return (1, \%out);
}

# ----------------------------
# Root pick + render
# ----------------------------
sub _pick_best_root {
    my ($self, $cell) = @_;
    return (0, -9e99) if !$cell;

    my $best_node = 0;
    my $best_score = -9e99;

    for my $it (@{$cell->{items}}) {
        next unless $it->{cat} == $self->{start_sym};
        if ($it->{score} > $best_score) {
            $best_score = $it->{score};
            $best_node  = $it->{node};
        }
    }
    return ($best_node, $best_score);
}

sub _render_tree {
    my ($self, $node_id) = @_;
    return '' if !$node_id || $node_id >= @{$self->{arena}};

    my $n = $self->{arena}[$node_id];
    my $lab = $self->_sym_str($n->{label});

    if ($n->{isLeaf}) {
        return '(' . $lab . ' ' . $n->{leaf} . ')';
    }
    if (!$n->{right}) {
        my $a = $self->_render_tree($n->{left});
        return '(' . $lab . ' ' . $a . ')';
    } else {
        my $a = $self->_render_tree($n->{left});
        my $b = $self->_render_tree($n->{right});
        return '(' . $lab . ' ' . $a . ' ' . $b . ')';
    }
}

1;
