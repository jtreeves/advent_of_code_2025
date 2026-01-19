#!/usr/bin/env perl
use strict;
use warnings;

sub solve {
    my ($input_data) = @_;
    my @lines = split /\n/, $input_data;
    
    # Find blank line separator
    my $blank_idx = -1;
    for my $i (0..$#lines) {
        $lines[$i] =~ s/^\s+|\s+$//g;
        if ($lines[$i] eq '') {
            $blank_idx = $i;
            last;
        }
    }
    
    # Parse ranges (first section)
    my @ranges = ();
    for my $i (0..$blank_idx-1) {
        $lines[$i] =~ s/^\s+|\s+$//g;
        next unless $lines[$i];
        if ($lines[$i] =~ /(\d+)-(\d+)/) {
            push @ranges, [$1, $2];
        }
    }
    
    # Parse IDs to check (second section)
    my @ids = ();
    for my $i ($blank_idx+1..$#lines) {
        $lines[$i] =~ s/^\s+|\s+$//g;
        next unless $lines[$i];
        push @ids, int($lines[$i]);
    }
    
    # Part 1: Count how many IDs fall into any range
    my $part1_count = 0;
    for my $id_val (@ids) {
        for my $range_ref (@ranges) {
            my ($start, $end) = @$range_ref;
            if ($id_val >= $start && $id_val <= $end) {
                $part1_count++;
                last;
            }
        }
    }
    
    # Part 2: Merge ranges and count total unique IDs covered
    # Sort ranges by start value
    @ranges = sort { $a->[0] <=> $b->[0] } @ranges;
    
    # Merge overlapping/adjacent ranges
    my @merged = ();
    if (@ranges) {
        push @merged, [@{$ranges[0]}];
        for my $i (1..$#ranges) {
            my ($start, $end) = @{$ranges[$i]};
            my ($_, $last_end) = @{$merged[-1]};
            # Check if overlaps or is adjacent (start <= last_end + 1)
            if ($start <= $last_end + 1) {
                # Merge: update end to max of both ends
                $merged[-1][1] = $last_end > $end ? $last_end : $end;
            } else {
                # No overlap, add as new range
                push @merged, [$start, $end];
            }
        }
    }
    
    # Calculate total unique IDs covered
    my $part2_total = 0;
    for my $range_ref (@merged) {
        my ($start, $end) = @$range_ref;
        $part2_total += $end - $start + 1;
    }
    
    return ($part1_count, $part2_total);
}

open my $fh, '<', "../data/input.txt" or die "Cannot open file: $!";
my $data = do { local $/; <$fh> };
close $fh;
my ($part1, $part2) = solve($data);
print "Part 1: $part1\n";
print "Part 2: $part2\n";
