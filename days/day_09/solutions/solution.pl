#!/usr/bin/env perl
use strict;
use warnings;

sub solve {
    my ($input_data) = @_;
    my @lines = split /\n/, $input_data;
    @lines = grep { $_ =~ /\S/ } map { s/^\s+|\s+$//g; $_ } @lines;
    
    # Parse coordinates
    my @red_tiles = ();
    for my $line (@lines) {
        if ($line =~ /,/) {
            my @parts = split /,/, $line;
            my $x = int($parts[0]);
            my $y = int($parts[1]);
            push @red_tiles, [$x, $y];
        }
    }
    
    return ("0", "0") if @red_tiles < 2;
    
    # Part 1: Find largest rectangle area using any two red tiles as corners
    my $max_area_part1 = 0;
    for my $i (0..$#red_tiles) {
        for my $j ($i+1..$#red_tiles) {
            my ($x1, $y1) = @{$red_tiles[$i]};
            my ($x2, $y2) = @{$red_tiles[$j]};
            my $width = abs($x1 - $x2) + 1;
            my $height = abs($y1 - $y2) + 1;
            my $area = $width * $height;
            $max_area_part1 = $area if $area > $max_area_part1;
        }
    }
    
    # Part 2: Coordinate compression + flood-fill + prefix sums
    # Collect all x and y coordinates for compression
    my %all_x_set = ();
    my %all_y_set = ();
    for my $tile (@red_tiles) {
        my ($x, $y) = @$tile;
        $all_x_set{$x} = 1;
        $all_x_set{$x + 1} = 1;
        $all_y_set{$y} = 1;
        $all_y_set{$y + 1} = 1;
    }
    
    my @all_x = sort { $a <=> $b } keys %all_x_set;
    my @all_y = sort { $a <=> $b } keys %all_y_set;
    
    # Create compression maps
    my %x_to_cx = ();
    for my $i (0..$#all_x) {
        $x_to_cx{$all_x[$i]} = $i;
    }
    my %y_to_cy = ();
    for my $i (0..$#all_y) {
        $y_to_cy{$all_y[$i]} = $i;
    }
    
    my $width = scalar @all_x;
    my $height = scalar @all_y;
    
    # Build grid: 0 = outside/invalid, 1 = inside/valid
    my @grid = map { [(0) x $height] } 0..$width-1;
    
    # Mark boundary (red + green tiles)
    for my $tile (@red_tiles) {
        my ($x, $y) = @$tile;
        if (exists $x_to_cx{$x} && exists $y_to_cy{$y}) {
            $grid[$x_to_cx{$x}][$y_to_cy{$y}] = 1;
        }
    }
    
    # Connect consecutive red tiles with green tiles
    for my $i (0..$#red_tiles) {
        my ($x1, $y1) = @{$red_tiles[$i]};
        my ($x2, $y2) = @{$red_tiles[($i + 1) % @red_tiles]};
        
        if ($x1 == $x2) {
            my $start_y = $y1 < $y2 ? $y1 : $y2;
            my $end_y = $y1 > $y2 ? $y1 : $y2;
            for my $y ($start_y..$end_y) {
                if (exists $x_to_cx{$x1} && exists $y_to_cy{$y}) {
                    $grid[$x_to_cx{$x1}][$y_to_cy{$y}] = 1;
                }
            }
        } elsif ($y1 == $y2) {
            my $start_x = $x1 < $x2 ? $x1 : $x2;
            my $end_x = $x1 > $x2 ? $x1 : $x2;
            for my $x ($start_x..$end_x) {
                if (exists $x_to_cx{$x} && exists $y_to_cy{$y1}) {
                    $grid[$x_to_cx{$x}][$y_to_cy{$y1}] = 1;
                }
            }
        }
    }
    
    # Point-in-polygon check
    sub point_in_polygon {
        my ($px, $py, $red_tiles_ref) = @_;
        my $inside = 0;
        for my $i (0..$#$red_tiles_ref) {
            my ($x1, $y1) = @{$red_tiles_ref->[$i]};
            my ($x2, $y2) = @{$red_tiles_ref->[($i + 1) % @$red_tiles_ref]};
            if (($y1 > $py) != ($y2 > $py)) {
                my $intersect_x;
                if ($y2 != $y1) {
                    $intersect_x = ($py - $y1) * ($x2 - $x1) / ($y2 - $y1) + $x1;
                } else {
                    $intersect_x = $px;
                }
                if ($px < $intersect_x) {
                    $inside = !$inside;
                }
            }
        }
        return $inside;
    }
    
    # Flood fill interior
    my $found_interior = 0;
    for my $cx (0..$width-1) {
        for my $cy (0..$height-1) {
            if (!$grid[$cx][$cy]) {
                my $orig_x = $all_x[$cx];
                my $orig_y = $all_y[$cy];
                if (point_in_polygon($orig_x, $orig_y, \@red_tiles)) {
                    # Flood fill from this point
                    my @stack = ([$cx, $cy]);
                    while (@stack) {
                        my $pos = pop @stack;
                        my ($x, $y) = @$pos;
                        if ($x < 0 || $y < 0 || $x >= $width || $y >= $height || $grid[$x][$y]) {
                            next;
                        }
                        my $orig_x_val = $all_x[$x];
                        my $orig_y_val = $all_y[$y];
                        if (point_in_polygon($orig_x_val, $orig_y_val, \@red_tiles)) {
                            $grid[$x][$y] = 1;
                            push @stack, [$x - 1, $y];
                            push @stack, [$x + 1, $y];
                            push @stack, [$x, $y - 1];
                            push @stack, [$x, $y + 1];
                        }
                    }
                    $found_interior = 1;
                    last;
                }
            }
        }
        last if $found_interior;
    }
    
    # Build 2D prefix sum for O(1) rectangle queries
    my @prefix = map { [(0) x ($height + 1)] } 0..$width;
    for my $cx (0..$width-1) {
        for my $cy (0..$height-1) {
            $prefix[$cx + 1][$cy + 1] = $prefix[$cx][$cy + 1] + $prefix[$cx + 1][$cy]
                                       - $prefix[$cx][$cy] + ($grid[$cx][$cy] ? 1 : 0);
        }
    }
    
    # Helper: get sum in rectangle [cx1, cx2] x [cy1, cy2] (inclusive)
    sub rect_sum {
        my ($cx1, $cx2, $cy1, $cy2, $prefix_ref) = @_;
        return $prefix_ref->[$cx2 + 1][$cy2 + 1] - $prefix_ref->[$cx1][$cy2 + 1]
             - $prefix_ref->[$cx2 + 1][$cy1] + $prefix_ref->[$cx1][$cy1];
    }
    
    # Generate candidates sorted by area descending
    my @candidates = ();
    for my $i (0..$#red_tiles) {
        for my $j ($i+1..$#red_tiles) {
            my ($x1, $y1) = @{$red_tiles[$i]};
            my ($x2, $y2) = @{$red_tiles[$j]};
            my $min_x = $x1 < $x2 ? $x1 : $x2;
            my $max_x = $x1 > $x2 ? $x1 : $x2;
            my $min_y = $y1 < $y2 ? $y1 : $y2;
            my $max_y = $y1 > $y2 ? $y1 : $y2;
            my $area = ($max_x - $min_x + 1) * ($max_y - $min_y + 1);
            
            if (exists $x_to_cx{$min_x} && exists $x_to_cx{$max_x} &&
                exists $y_to_cy{$min_y} && exists $y_to_cy{$max_y}) {
                my $cx1 = $x_to_cx{$min_x};
                my $cx2 = $x_to_cx{$max_x};
                my $cy1 = $y_to_cy{$min_y};
                my $cy2 = $y_to_cy{$max_y};
                push @candidates, [$min_x, $max_x, $min_y, $max_y, $area, $cx1, $cx2, $cy1, $cy2];
            }
        }
    }
    
    @candidates = sort { $b->[4] <=> $a->[4] } @candidates;
    
    # Check candidates in descending area order
    my $max_area_part2 = 0;
    for my $cand (@candidates) {
        my ($min_x, $max_x, $min_y, $max_y, $area, $cx1, $cx2, $cy1, $cy2) = @$cand;
        last if $area <= $max_area_part2;
        
        # Check if rectangle is fully contained using prefix sum
        my $valid_count = rect_sum($cx1, $cx2, $cy1, $cy2, \@prefix);
        my $expected_cells = ($cx2 - $cx1 + 1) * ($cy2 - $cy1 + 1);
        
        if ($valid_count == $expected_cells) {
            # Check corners to ensure rectangle doesn't extend beyond valid regions
            my $all_valid = 1;
            for my $corner ([$min_x, $min_y], [$min_x, $max_y], [$max_x, $min_y], [$max_x, $max_y]) {
                my ($x, $y) = @$corner;
                if (exists $x_to_cx{$x} && exists $y_to_cy{$y}) {
                    my $cx = $x_to_cx{$x};
                    my $cy = $y_to_cy{$y};
                    if (!$grid[$cx][$cy]) {
                        $all_valid = 0;
                        last;
                    }
                } else {
                    if (!point_in_polygon($x, $y, \@red_tiles)) {
                        $all_valid = 0;
                        last;
                    }
                }
            }
            
            if ($all_valid) {
                $max_area_part2 = $area;
                last;
            }
        }
    }
    
    return ($max_area_part1, $max_area_part2);
}

open my $fh, '<', "../data/input.txt" or die "Cannot open file: $!";
my $data = do { local $/; <$fh> };
close $fh;

my ($part1, $part2) = solve($data);
print "Part 1: $part1\n";
print "Part 2: $part2\n";
