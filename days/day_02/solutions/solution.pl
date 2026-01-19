#!/usr/bin/env perl
use strict;
use warnings;

sub is_invalid_part1 {
    my ($id_str) = @_;
    my $n = length($id_str);
    # Must be even length to have exactly two sequences
    return 0 if $n % 2 != 0;
    # Split in half and check if halves are identical
    my $half = $n / 2;
    my $first_half = substr($id_str, 0, $half);
    my $second_half = substr($id_str, $half);
    return $first_half eq $second_half ? 1 : 0;
}

sub is_invalid_part2 {
    my ($id_str) = @_;
    my $n = length($id_str);
    # Check all possible divisors k >= 2
    for my $k (2..$n) {
        if ($n % $k == 0) {
            my $seq_len = $n / $k;
            my $pattern = substr($id_str, 0, $seq_len);
            # Check if the pattern repeats k times
            if ($id_str eq $pattern x $k) {
                return 1;
            }
        }
    }
    return 0;
}

sub parse_ranges {
    my ($line) = @_;
    my @ranges = ();
    for my $range_str (split /,/, $line) {
        $range_str =~ s/^\s+|\s+$//g;
        if ($range_str =~ /(\d+)-(\d+)/) {
            push @ranges, [$1, $2];
        }
    }
    return @ranges;
}

sub solve {
    my ($input_data) = @_;
    my @lines = split /\n/, $input_data;
    
    my $part1_sum = 0;
    my $part2_sum = 0;
    
    for my $line (@lines) {
        $line =~ s/^\s+|\s+$//g;
        next unless $line;
        my @ranges_list = parse_ranges($line);
        
        for my $range_ref (@ranges_list) {
            my ($start, $end) = @$range_ref;
            # Iterate through all IDs in the range (inclusive)
            for my $num ($start..$end) {
                my $id_str = "$num";
                
                # Part 1: exactly two identical sequences
                if (is_invalid_part1($id_str)) {
                    $part1_sum += $num;
                }
                
                # Part 2: sequence repeated 2+ times (includes Part 1 cases)
                if (is_invalid_part2($id_str)) {
                    $part2_sum += $num;
                }
            }
        }
    }
    
    return ($part1_sum, $part2_sum);
}

open my $fh, '<', "../data/input.txt" or die "Cannot open file: $!";
my $data = do { local $/; <$fh> };
close $fh;

my ($part1, $part2) = solve($data);
print "Part 1: $part1\n";
print "Part 2: $part2\n";
