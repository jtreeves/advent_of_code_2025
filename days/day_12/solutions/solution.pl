#!/usr/bin/env perl
use strict;
use warnings;

sub solve {
    my ($input_data) = @_;
    my @lines = split /\n/, $input_data;
    return ("0", "N/A") unless @lines;
    
    # Parse shapes (first 6 shapes, numbered 0-5)
    my @shape_areas = (0) x 6;
    my $i = 0;
    my $shape_idx = 0;
    
    while ($i < @lines && $shape_idx < 6) {
        my $line = $lines[$i];
        $line =~ s/^\s+|\s+$//g;
        # Check if this is a shape header (format: "number:")
        if ($line && $line =~ /^(\d+):$/) {
            my $shape_num = int($1);
            if ($shape_num == $shape_idx) {
                # Read the next 3 lines for the shape grid
                my @shape_grid = ();
                for my $j (0..2) {
                    if ($i + 1 + $j < @lines) {
                        my $grid_line = $lines[$i + 1 + $j];
                        $grid_line =~ s/^\s+|\s+$//g;
                        push @shape_grid, $grid_line;
                    } else {
                        push @shape_grid, "";
                    }
                }
                
                # Count '#' characters in the shape
                my $area = 0;
                for my $row (@shape_grid) {
                    $area += ($row =~ tr/#//);
                }
                $shape_areas[$shape_idx] = $area;
                $shape_idx++;
                # Skip shape header + 3 grid lines + empty line (if present)
                $i += 4;
                next;
            }
        }
        $i++;
    }
    
    # Find where queries start (skip empty lines after shapes)
    my $query_start = $i;
    while ($query_start < @lines) {
        $lines[$query_start] =~ s/^\s+|\s+$//g;
        last if $lines[$query_start];
        $query_start++;
    }
    
    # Parse queries
    my $possible_count = 0;
    for my $line_idx ($query_start..$#lines) {
        my $line = $lines[$line_idx];
        $line =~ s/^\s+|\s+$//g;
        next unless $line;
        
        # Parse query: "widthxheight: count0 count1 count2 count3 count4 count5"
        next unless $line =~ /:/;
        
        my @parts = split /:/, $line, 2;
        next unless @parts == 2;
        
        # Parse dimensions
        my $dims = $parts[0];
        $dims =~ s/^\s+|\s+$//g;
        next unless $dims =~ /x/;
        
        my @dim_parts = split /x/, $dims;
        next unless @dim_parts == 2;
        
        my $width = int($dim_parts[0]);
        my $height = int($dim_parts[1]);
        
        # Parse counts
        my $count_str = $parts[1];
        $count_str =~ s/^\s+|\s+$//g;
        my @count_parts = split /\s+/, $count_str;
        next unless @count_parts == 6;
        
        my @counts = map { int($_) } @count_parts;
        
        # Calculate area check
        my $region_area = $width * $height;
        my $required_area = 0;
        for my $j (0..5) {
            $required_area += $shape_areas[$j] * $counts[$j];
        }
        
        if ($required_area <= $region_area) {
            $possible_count++;
        }
    }
    
    # Part 2: Final star (no computation needed)
    my $part2 = "Final star";
    
    return ($possible_count, $part2);
}

open my $fh, '<', "../data/input.txt" or die "Cannot open file: $!";
my $data = do { local $/; <$fh> };
close $fh;

my ($part1, $part2) = solve($data);
print "Part 1: $part1\n";
print "Part 2: $part2\n";
