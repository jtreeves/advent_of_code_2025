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

sub count_neighbors {
    my ($grid, $i, $j, $rows, $cols) = @_;
    my $neighbor_count = 0;
    for my $di (-1..1) {
        for my $dj (-1..1) {
            next if $di == 0 && $dj == 0;
            my $ni = $i + $di;
            my $nj = $j + $dj;
            if ($ni >= 0 && $ni < $rows && $nj >= 0 && $nj < $cols) {
                if (substr($grid->[$ni], $nj, 1) eq '@') {
                    $neighbor_count++;
                }
            }
        }
    }
    return $neighbor_count;
}

sub solve {
    my ($input_data) = @_;
    my @lines = grep { $_ =~ /\S/ } split /\n/, $input_data;
    my $rows = scalar @lines;
    my $cols = $rows > 0 ? length($lines[0]) : 0;
    
    # Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
    my $part1_count = 0;
    for my $i (0..$rows-1) {
        for my $j (0..$cols-1) {
            if (substr($lines[$i], $j, 1) eq '@') {
                my $neighbors = count_neighbors(\@lines, $i, $j, $rows, $cols);
                if ($neighbors < 4) {
                    $part1_count++;
                }
            }
        }
    }
    
    # Part 2: Iteratively remove accessible rolls until none can be removed
    my @grid = map { [split //, $_] } @lines;
    my $part2_count = 0;
    
    while (1) {
        my @to_remove = ();
        my @grid_str = map { join('', @$_) } @grid;
        for my $i (0..$rows-1) {
            for my $j (0..$cols-1) {
                if ($grid[$i][$j] eq '@') {
                    my $neighbors = count_neighbors(\@grid_str, $i, $j, $rows, $cols);
                    if ($neighbors < 4) {
                        push @to_remove, [$i, $j];
                    }
                }
            }
        }
        
        last unless @to_remove;
        
        # Remove all marked positions
        for my $pos (@to_remove) {
            my ($i, $j) = @$pos;
            $grid[$i][$j] = '.';
        }
        $part2_count += scalar @to_remove;
    }
    
    return ($part1_count, $part2_count);
}

my $data = read_input_raw("../data/input.txt");
my ($part1, $part2) = solve($data);
print "Part 1: $part1\n";
print "Part 2: $part2\n";
