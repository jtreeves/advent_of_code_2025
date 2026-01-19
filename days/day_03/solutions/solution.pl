#!/usr/bin/env perl
use strict;
use warnings;

sub find_largest_subsequence {
    my ($bank, $n) = @_;
    return 0 if length($bank) < $n;
    
    my @result = ();
    my $start = 0;
    my $bank_len = length($bank);
    
    for my $i (0..$n-1) {
        # How many digits we still need after this one
        my $remaining_needed = $n - $i - 1;
        # The last index we can choose from
        my $end = $bank_len - $remaining_needed;
        
        # Find the maximum digit in the range [start, end)
        my $max_digit = substr($bank, $start, 1);
        my $max_pos = $start;
        for my $j ($start+1..$end-1) {
            my $digit = substr($bank, $j, 1);
            if ($digit gt $max_digit) {
                $max_digit = $digit;
                $max_pos = $j;
            }
        }
        
        push @result, $max_digit;
        $start = $max_pos + 1;
    }
    
    return int(join('', @result));
}

sub solve {
    my ($input_data) = @_;
    my @lines = split /\n/, $input_data;
    
    my $part1_sum = 0;
    my $part2_sum = 0;
    
    for my $line (@lines) {
        $line =~ s/^\s+|\s+$//g;
        next unless $line;
        my $bank = $line;
        
        # Part 1: largest 2-digit number
        $part1_sum += find_largest_subsequence($bank, 2);
        
        # Part 2: largest 12-digit number
        if (length($bank) >= 12) {
            $part2_sum += find_largest_subsequence($bank, 12);
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
