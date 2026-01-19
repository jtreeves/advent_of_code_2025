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

# Part 1: Count paths from "you" to "out"
sub count_paths_part1 {
    my ($node, $graph_ref, $memo_ref) = @_;
    return 1 if $node eq "out";
    return $memo_ref->{$node} if exists $memo_ref->{$node};
    
    my $count = 0;
    my @neighbors = @{$graph_ref->{$node} || []};
    for my $neighbor (@neighbors) {
        $count += count_paths_part1($neighbor, $graph_ref, $memo_ref);
    }
    
    $memo_ref->{$node} = $count;
    return $count;
}

# Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
sub count_paths_part2 {
    my ($node, $visited_fft, $visited_dac, $graph_ref, $memo_ref) = @_;
    if ($node eq "out") {
        return ($visited_fft && $visited_dac) ? 1 : 0;
    }
    
    my $key = "$node|$visited_fft|$visited_dac";
    return $memo_ref->{$key} if exists $memo_ref->{$key};
    
    # Update flags when visiting fft or dac
    my $new_visited_fft = $visited_fft || ($node eq "fft");
    my $new_visited_dac = $visited_dac || ($node eq "dac");
    
    my $count = 0;
    my @neighbors = @{$graph_ref->{$node} || []};
    for my $neighbor (@neighbors) {
        $count += count_paths_part2($neighbor, $new_visited_fft, $new_visited_dac, $graph_ref, $memo_ref);
    }
    
    $memo_ref->{$key} = $count;
    return $count;
}

sub solve {
    my ($input_data) = @_;
    my @lines = split /\n/, $input_data;
    
    # Build graph: device -> list of outputs
    my %graph = ();
    for my $line (@lines) {
        $line =~ s/^\s+|\s+$//g;
        next unless $line;
        my @parts = split /:/, $line, 2;
        next unless @parts == 2;
        my $device = $parts[0];
        $device =~ s/^\s+|\s+$//g;
        my $outputs_str = $parts[1];
        $outputs_str =~ s/^\s+|\s+$//g;
        my @outputs = $outputs_str ? split /\s+/, $outputs_str : ();
        $graph{$device} = \@outputs;
    }
    
    # Part 1: Count paths from "you" to "out"
    my %part1_memo = ();
    my $part1_count = 0;
    if (exists $graph{"you"}) {
        $part1_count = count_paths_part1("you", \%graph, \%part1_memo);
    }
    
    # Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
    my %part2_memo = ();
    my $part2_count = 0;
    if (exists $graph{"svr"}) {
        $part2_count = count_paths_part2("svr", 0, 0, \%graph, \%part2_memo);
    }
    
    return ($part1_count, $part2_count);
}

my $data = read_input_raw("../data/input.txt");
my ($part1, $part2) = solve($data);
print "Part 1: $part1\n";
print "Part 2: $part2\n";
