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
    my @lines = grep { $_ =~ /\S/ } split /\n/, $input_data;
    
    # Part 1: Count times dial ends at 0 after a rotation
    my $position = 50;
    my $count_part1 = 0;
    
    for my $line (@lines) {
        my $direction = substr($line, 0, 1);
        my $distance = int(substr($line, 1));
        
        # Apply rotation
        if ($direction eq 'L') {
            $position = (($position - $distance) % 100 + 100) % 100;
        } else {  # direction == 'R'
            $position = ($position + $distance) % 100;
        }
        
        # Check if ended at 0
        if ($position == 0) {
            $count_part1++;
        }
    }
    
    # Part 2: Count times dial is at 0 during entire process
    $position = 50;
    my $count_part2 = 0;
    
    for my $line (@lines) {
        my $direction = substr($line, 0, 1);
        my $distance = int(substr($line, 1));
        
        my $start_pos = $position;
        
        # Check each click position during rotation
        for my $click (1..$distance) {
            my $click_pos;
            if ($direction eq 'L') {
                $click_pos = (($start_pos - $click) % 100 + 100) % 100;
            } else {  # direction == 'R'
                $click_pos = ($start_pos + $click) % 100;
            }
            
            if ($click_pos == 0) {
                $count_part2++;
            }
        }
        
        # Update position after rotation
        if ($direction eq 'L') {
            $position = (($position - $distance) % 100 + 100) % 100;
        } else {
            $position = ($position + $distance) % 100;
        }
    }
    
    return ($count_part1, $count_part2);
}

my $data = read_input_raw("../data/input.txt");
my ($part1, $part2) = solve($data);
print "Part 1: $part1\n";
print "Part 2: $part2\n";
