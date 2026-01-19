#!/usr/bin/env perl
use strict;
use warnings;

sub read_input_raw {
    my ($file_path) = @_;
    open my $fh, '<', $file_path or die "Cannot open file: $!";
    my $content = do { local $/; <$fh> };
    close $fh;
    return $content;
}

sub solve {
    my ($input_data) = @_;
    my @lines = split /\n/, $input_data;
    return ("0", "0") unless @lines;
    
    my @grid = map { [split //, $_] } @lines;
    my $rows = scalar @grid;
    my $cols = $rows > 0 ? scalar @{$grid[0]} : 0;
    
    # Find starting position S
    my $start_row = -1;
    my $start_col = -1;
    for my $r (0..$rows-1) {
        for my $c (0..$cols-1) {
            if ($grid[$r][$c] eq 'S') {
                $start_row = $r;
                $start_col = $c;
                last;
            }
        }
        last if $start_row != -1;
    }
    
    return ("0", "0") if $start_row == -1;
    
    # Part 1: Count total splits
    my $split_count = 0;
    # Track active beam columns per row (using a set for each row)
    my %active_beams = ();
    $active_beams{$start_col} = 1;  # Start with beam at S column
    
    # Process each row starting from the row after S
    for my $r ($start_row+1..$rows-1) {
        my %next_beams = ();
        for my $col (keys %active_beams) {
            if ($grid[$r][$col] eq '.') {
                # Beam continues down
                $next_beams{$col} = 1;
            } elsif ($grid[$r][$col] eq '^') {
                # Beam splits
                $split_count++;
                # Add beams to left and right
                if ($col - 1 >= 0) {
                    $next_beams{$col - 1} = 1;
                }
                if ($col + 1 < $cols) {
                    $next_beams{$col + 1} = 1;
                }
            }
        }
        %active_beams = %next_beams;
    }
    
    # Part 2: Count beams reaching bottom row
    # Use a 2D count matrix to track beam counts per cell
    my @beam_counts = map { [(0) x $cols] } 0..$rows-1;
    $beam_counts[$start_row][$start_col] = 1;  # Start with 1 beam at S
    
    # Process each row starting from the row after S
    for my $r ($start_row+1..$rows-1) {
        for my $c (0..$cols-1) {
            my $prev_count = $beam_counts[$r-1][$c];
            if ($prev_count > 0) {
                if ($grid[$r][$c] eq '.') {
                    # Beam continues down
                    $beam_counts[$r][$c] += $prev_count;
                } elsif ($grid[$r][$c] eq '^') {
                    # Beam splits into left and right
                    if ($c - 1 >= 0) {
                        $beam_counts[$r][$c-1] += $prev_count;
                    }
                    if ($c + 1 < $cols) {
                        $beam_counts[$r][$c+1] += $prev_count;
                    }
                }
            }
        }
    }
    
    # Sum all beams in bottom row
    my $bottom_beam_count = 0;
    for my $c (0..$cols-1) {
        $bottom_beam_count += $beam_counts[$rows-1][$c];
    }
    
    return ($split_count, $bottom_beam_count);
}

my $data = read_input_raw("../data/input.txt");
my ($part1, $part2) = solve($data);
print "Part 1: $part1\n";
print "Part 2: $part2\n";
