#!/usr/bin/env perl
use strict;
use warnings;


sub parse_coordinates {
    my (@lines) = @_;
    my @coords = ();
    for my $line (@lines) {
        $line =~ s/^\s+|\s+$//g;
        next unless $line;
        my @parts = split /,/, $line;
        push @coords, [int($parts[0]), int($parts[1]), int($parts[2])];
    }
    return @coords;
}

sub squared_distance {
    my ($p1, $p2) = @_;
    my $dx = $p2->[0] - $p1->[0];
    my $dy = $p2->[1] - $p1->[1];
    my $dz = $p2->[2] - $p1->[2];
    return $dx*$dx + $dy*$dy + $dz*$dz;
}

# Union-Find data structure
package UnionFind {
    sub new {
        my ($class, $n) = @_;
        my $self = {
            parent => [0..$n-1],
            size => [(1) x $n],
            component_count => $n
        };
        bless $self, $class;
        return $self;
    }
    
    sub find {
        my ($self, $x) = @_;
        if ($self->{parent}[$x] != $x) {
            $self->{parent}[$x] = $self->find($self->{parent}[$x]);
        }
        return $self->{parent}[$x];
    }
    
    sub union {
        my ($self, $x, $y) = @_;
        my $root_x = $self->find($x);
        my $root_y = $self->find($y);
        return 0 if $root_x == $root_y;
        
        # Union by size
        if ($self->{size}[$root_x] < $self->{size}[$root_y]) {
            ($root_x, $root_y) = ($root_y, $root_x);
        }
        
        $self->{parent}[$root_y] = $root_x;
        $self->{size}[$root_x] += $self->{size}[$root_y];
        $self->{component_count}--;
        return 1;
    }
}

sub solve {
    my ($input_data) = @_;
    my @lines = split /\n/, $input_data;
    my @coords = parse_coordinates(@lines);
    
    my $n = scalar @coords;
    return ("0", "0") if $n == 0;
    
    # Generate all pairs with squared distances
    my @pairs = ();
    for my $i (0..$n-1) {
        for my $j ($i+1..$n-1) {
            my $dist_sq = squared_distance($coords[$i], $coords[$j]);
            push @pairs, [$i, $j, $dist_sq];
        }
    }
    
    # Sort by distance
    @pairs = sort { $a->[2] <=> $b->[2] } @pairs;
    
    # Part 1: Connect first 1000 pairs
    my $uf1 = UnionFind->new($n);
    my $connections_made = 0;
    for my $pair_ref (@pairs) {
        last if $connections_made >= 1000;
        my ($i, $j) = @$pair_ref;
        $uf1->union($i, $j);  # Counts even if already connected
        $connections_made++;
    }
    
    # Get component sizes
    my %component_sizes = ();
    for my $i (0..$n-1) {
        my $root = $uf1->find($i);
        $component_sizes{$root} = $uf1->{size}[$root];
    }
    
    my @sizes = sort { $b <=> $a } values %component_sizes;
    my $part1 = @sizes >= 3 ? $sizes[0] * $sizes[1] * $sizes[2] : 0;
    
    # Part 2: Connect until all in one circuit
    my $uf2 = UnionFind->new($n);
    my $final_pair = undef;
    for my $pair_ref (@pairs) {
        last if $uf2->{component_count} == 1;
        my ($i, $j) = @$pair_ref;
        if ($uf2->union($i, $j)) {
            if ($uf2->{component_count} == 1) {
                $final_pair = [$i, $j];
                last;
            }
        }
    }
    
    my $part2 = 0;
    if (defined $final_pair) {
        my ($i, $j) = @$final_pair;
        $part2 = $coords[$i][0] * $coords[$j][0];
    }
    
    return ($part1, $part2);
}

open my $fh, '<', "../data/input.txt" or die "Cannot open file: $!";
my $data = do { local $/; <$fh> };
close $fh;
my ($part1, $part2) = solve($data);
print "Part 1: $part1\n";
print "Part 2: $part2\n";
