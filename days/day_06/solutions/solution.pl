#!/usr/bin/env perl
use strict;
use warnings;


sub solve {
    my ($input_data) = @_;
    my @lines = split /\n/, $input_data;
    return ("0", "0") unless @lines;
    
    # Find maximum line length and pad all lines
    my $max_len = 0;
    for my $line (@lines) {
        my $len = length($line);
        $max_len = $len if $len > $max_len;
    }
    my @padded_lines = map { $_ . (' ' x ($max_len - length($_))) } @lines;
    
    # Operator row is the last row
    my $op_row_idx = $#padded_lines;
    my $op_row = $padded_lines[$op_row_idx];
    my @num_rows = @padded_lines[0..$op_row_idx-1];
    
    # Part 1: Parse horizontally
    my $part1_total = 0;
    
    # Find problem boundaries (columns that are all spaces)
    my @is_space_col = ();
    for my $col (0..$max_len-1) {
        my $all_spaces = 1;
        for my $line (@padded_lines) {
            if (substr($line, $col, 1) ne ' ') {
                $all_spaces = 0;
                last;
            }
        }
        push @is_space_col, $all_spaces;
    }
    
    # Group columns into problems
    my @problems = ();
    my $i = 0;
    while ($i < $max_len) {
        if (!$is_space_col[$i]) {
            # Start of a problem
            my $start_col = $i;
            while ($i < $max_len && !$is_space_col[$i]) {
                $i++;
            }
            my $end_col = $i;
            # Extract operator for this problem
            my $op = undef;
            for my $j ($start_col..$end_col-1) {
                my $char = substr($op_row, $j, 1);
                if ($char eq '+' || $char eq '*') {
                    $op = $char;
                    last;
                }
            }
            if (defined $op) {
                push @problems, [$start_col, $end_col, $op];
            }
        } else {
            $i++;
        }
    }
    
    # Solve Part 1: Extract numbers horizontally
    for my $problem_ref (@problems) {
        my ($start_col, $end_col, $op) = @$problem_ref;
        my @numbers = ();
        # Extract numbers from each row in this problem area
        for my $row (@num_rows) {
            # Extract the substring for this problem
            my $problem_str = substr($row, $start_col, $end_col - $start_col);
            $problem_str =~ s/^\s+|\s+$//g;
            # Split by spaces and parse numbers
            for my $part (split /\s+/, $problem_str) {
                if ($part =~ /^\d+$/) {
                    push @numbers, int($part);
                }
            }
        }
        
        # Apply operator
        if (@numbers) {
            my $result;
            if ($op eq '+') {
                $result = 0;
                $result += $_ for @numbers;
            } else {  # op eq '*'
                $result = 1;
                $result *= $_ for @numbers;
            }
            $part1_total += $result;
        }
    }
    
    # Part 2: Parse vertically (columns, right-to-left)
    my $part2_total = 0;
    
    for my $problem_ref (@problems) {
        my ($start_col, $end_col, $op) = @$problem_ref;
        # Extract column strings (transpose)
        my @col_strings = ();
        for my $col ($start_col..$end_col-1) {
            next if $is_space_col[$col];
            my $col_str = '';
            for my $row (@num_rows) {
                my $char = length($row) > $col ? substr($row, $col, 1) : ' ';
                $col_str .= $char;
            }
            $col_str =~ s/^\s+|\s+$//g;
            push @col_strings, $col_str if $col_str;
        }
        
        # Reverse for right-to-left reading
        @col_strings = reverse @col_strings;
        
        # Parse numbers by removing spaces and converting
        my @numbers_vert = ();
        for my $col_str (@col_strings) {
            my $digits_only = join('', grep { /\d/ } split //, $col_str);
            if ($digits_only =~ /^\d+$/) {
                push @numbers_vert, int($digits_only);
            }
        }
        
        # Apply operator
        if (@numbers_vert) {
            my $result;
            if ($op eq '+') {
                $result = 0;
                $result += $_ for @numbers_vert;
            } else {  # op eq '*'
                $result = 1;
                $result *= $_ for @numbers_vert;
            }
            $part2_total += $result;
        }
    }
    
    return ($part1_total, $part2_total);
}

open my $fh, '<', "../data/input.txt" or die "Cannot open file: $!";
my $data = do { local $/; <$fh> };
close $fh;
my ($part1, $part2) = solve($data);
print "Part 1: $part1\n";
print "Part 2: $part2\n";
