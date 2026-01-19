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

sub parse_line {
    my ($line) = @_;
    # Extract pattern: [.##.]
    my @target_pattern = ();
    my @buttons = ();
    my @joltages = ();
    
    if ($line =~ /\[([.#]+)\]/) {
        my $pattern_str = $1;
        @target_pattern = map { $_ eq '#' ? 1 : 0 } split //, $pattern_str;
    } else {
        return ([], [], []);
    }
    
    # Extract buttons: (1,3) (2) etc.
    while ($line =~ /\(([^)]*)\)/g) {
        my $btn_str = $1;
        if ($btn_str =~ /\S/) {
            my @lights = map { int($_) } split /,/, $btn_str;
            push @buttons, \@lights;
        } else {
            push @buttons, [];
        }
    }
    
    # Extract joltages: {3,5,4,7}
    if ($line =~ /\{([^}]+)\}/) {
        my $joltage_str = $1;
        @joltages = map { int($_) } split /,/, $joltage_str;
    }
    
    return (\@target_pattern, \@buttons, \@joltages);
}

# Generate all combinations of k elements from range [0, n-1]
sub combinations {
    my ($n, $k) = @_;
    return [] if $k == 0;
    return [map { [$_] } 0..$n-1] if $k == 1;
    
    my @result = ();
    for my $first (0..$n-$k) {
        my @rest_combos = combinations($n - $first - 1, $k - 1);
        for my $combo (@rest_combos) {
            my @new_combo = ($first, map { $_ + $first + 1 } @$combo);
            push @result, \@new_combo;
        }
    }
    return \@result;
}

sub gaussian_elimination_gf2 {
    my ($matrix_ref, $target_ref) = @_;
    my @matrix = @$matrix_ref;
    my @target = @$target_ref;
    
    my $num_buttons = scalar @matrix;
    my $num_lights = scalar @target;
    
    # Brute force: try all combinations in order of increasing weight
    for my $k (0..$num_buttons) {
        my $combos_ref = combinations($num_buttons, $k);
        for my $button_combo (@$combos_ref) {
            # Simulate pressing these buttons
            my @lights = (0) x $num_lights;  # All start OFF (0)
            for my $btn_idx (@$button_combo) {
                for my $light_idx (0..$num_lights-1) {
                    if ($matrix[$btn_idx][$light_idx]) {
                        $lights[$light_idx] ^= 1;  # Toggle
                    }
                }
            }
            
            # Check if matches target
            my $matches = 1;
            for my $i (0..$num_lights-1) {
                if ($lights[$i] != $target[$i]) {
                    $matches = 0;
                    last;
                }
            }
            
            if ($matches) {
                return $k;  # k buttons pressed = minimum weight
            }
        }
    }
    
    return undef;  # No solution found
}

sub gcd {
    my ($a, $b) = @_;
    while ($b) {
        ($a, $b) = ($b, $a % $b);
    }
    return abs($a);
}

sub integer_gaussian_elimination {
    my ($matrix_ref, $target_ref) = @_;
    my @matrix = map { [@$_] } @$matrix_ref;
    my @target = @$target_ref;
    
    my $num_rows = scalar @matrix;
    my $num_cols = $num_rows > 0 ? scalar @{$matrix[0]} : 0;
    
    my @pivot_cols = ();
    my @free_cols = ();
    my $pivot_row = 0;
    
    # Forward elimination
    for my $col (0..$num_cols-1) {
        # Find a row with non-zero entry in this column, starting from pivot_row
        my $pivot_idx = undef;
        for my $row ($pivot_row..$num_rows-1) {
            if ($matrix[$row][$col] != 0) {
                $pivot_idx = $row;
                last;
            }
        }
        
        if (!defined $pivot_idx) {
            # No pivot in this column - it's a free variable
            push @free_cols, $col;
            next;
        }
        
        # Swap rows
        if ($pivot_idx != $pivot_row) {
            @{$matrix[$pivot_row]}, @{$matrix[$pivot_idx]} = @{$matrix[$pivot_idx]}, @{$matrix[$pivot_row]};
            ($target[$pivot_row], $target[$pivot_idx]) = ($target[$pivot_idx], $target[$pivot_row]);
        }
        
        push @pivot_cols, $col;
        my $pivot_val = $matrix[$pivot_row][$col];
        
        # Eliminate this column in other rows using GCD-based elimination
        for my $row (0..$num_rows-1) {
            next if $row == $pivot_row;
            if ($matrix[$row][$col] != 0) {
                if ($matrix[$row][$col] % $pivot_val == 0) {
                    my $factor = $matrix[$row][$col] / $pivot_val;
                    for my $c (0..$num_cols-1) {
                        $matrix[$row][$c] -= $factor * $matrix[$pivot_row][$c];
                    }
                    $target[$row] -= $factor * $target[$pivot_row];
                } else {
                    # Use GCD approach: combine rows to eliminate
                    my $g = gcd($pivot_val, $matrix[$row][$col]);
                    if ($g > 0) {
                        my $factor1 = $matrix[$row][$col] / $g;
                        my $factor2 = $pivot_val / $g;
                        my @new_row = ();
                        for my $c (0..$num_cols-1) {
                            push @new_row, $factor1 * $matrix[$pivot_row][$c] - $factor2 * $matrix[$row][$c];
                        }
                        my $new_b = $factor1 * $target[$pivot_row] - $factor2 * $target[$row];
                        @{$matrix[$row]} = @new_row;
                        $target[$row] = $new_b;
                    }
                }
            }
        }
        
        $pivot_row++;
        last if $pivot_row >= $num_rows;
    }
    
    # Mark remaining columns as free
    for my $col (0..$num_cols-1) {
        push @free_cols, $col unless grep { $_ == $col } @pivot_cols;
    }
    
    return (\@matrix, \@target, \@pivot_cols, \@free_cols);
}

sub solve_part2_ilp_fallback {
    my ($buttons_ref, $joltages_ref, $target_pattern_ref) = @_;
    my @buttons = @$buttons_ref;
    my @joltages = @$joltages_ref;
    my @target_pattern = @$target_pattern_ref;
    
    my $num_buttons = scalar @buttons;
    my $num_lights = scalar @joltages;
    
    # Divide-by-2 optimization: if all joltages are even, halve and recurse
    my $all_even = 1;
    my $any_positive = 0;
    for my $j (@joltages) {
        $all_even = 0 if $j % 2 != 0;
        $any_positive = 1 if $j > 0;
    }
    if ($all_even && $any_positive) {
        my @halved_joltages = map { $_ / 2 } @joltages;
        my $result = solve_part2_ilp_fallback(\@buttons, \@halved_joltages, $target_pattern_ref);
        return defined $result ? $result * 2 : undef;
    }
    
    # Build incidence matrix: A[i][j] = 1 if button j affects light i, else 0
    my @matrix = ();
    for my $i (0..$num_lights-1) {
        push @matrix, [(0) x $num_buttons];
    }
    for my $btn_idx (0..$#buttons) {
        for my $light (@{$buttons[$btn_idx]}) {
            if ($light >= 0 && $light < $num_lights) {
                $matrix[$light][$btn_idx] = 1;
            }
        }
    }
    
    # Perform integer Gaussian elimination
    my ($reduced_A_ref, $reduced_b_ref, $pivot_cols_ref, $free_cols_ref) = 
        integer_gaussian_elimination(\@matrix, \@joltages);
    my @reduced_A = @$reduced_A_ref;
    my @reduced_b = @$reduced_b_ref;
    my @pivot_cols = @$pivot_cols_ref;
    my @free_cols = @$free_cols_ref;
    
    # If no free variables, solve directly by back-substitution
    if (@free_cols == 0) {
        my @solution = (0) x $num_buttons;
        for my $i (reverse 0..$#pivot_cols) {
            next if $i >= @reduced_A;
            my $col = $pivot_cols[$i];
            my $pivot_val = ($i < @reduced_A && $col < @{$reduced_A[$i]}) ? $reduced_A[$i][$col] : 0;
            next if $pivot_val == 0;
            
            my $rhs = $reduced_b[$i];
            for my $j ($i+1..$#pivot_cols) {
                if ($j < @{$reduced_A[$i]}) {
                    $rhs -= $reduced_A[$i][$pivot_cols[$j]] * $solution[$pivot_cols[$j]];
                }
            }
            
            return undef if $rhs % $pivot_val != 0;
            $solution[$col] = $rhs / $pivot_val;
            return undef if $solution[$col] < 0;
        }
        
        # Verify solution
        my @test_joltages = (0) x $num_lights;
        for my $btn_idx (0..$#buttons) {
            for my $light (@{$buttons[$btn_idx]}) {
                if ($light >= 0 && $light < $num_lights) {
                    $test_joltages[$light] += $solution[$btn_idx];
                }
            }
        }
        
        my $matches = 1;
        for my $i (0..$num_lights-1) {
            if ($test_joltages[$i] != $joltages[$i]) {
                $matches = 0;
                last;
            }
        }
        
        return $matches ? (sum(@solution)) : undef;
    }
    
    # There are free variables - search over them
    my $max_joltage = @joltages > 0 ? (sort { $b <=> $a } @joltages)[0] : 0;
    my $best = undef;
    
    sub solve_with_free_vars {
        my ($free_assignments_ref, $reduced_A_ref, $reduced_b_ref, $pivot_cols_ref, $buttons_ref, $joltages_ref) = @_;
        my @free_assignments = @$free_assignments_ref;
        my %free_dict = map { @$_ } @free_assignments;
        my @reduced_A = @$reduced_A_ref;
        my @reduced_b = @$reduced_b_ref;
        my @pivot_cols = @$pivot_cols_ref;
        my @buttons = @$buttons_ref;
        my @joltages = @$joltages_ref;
        my $num_buttons = scalar @buttons;
        my $num_lights = scalar @joltages;
        
        my @solution = (0) x $num_buttons;
        
        for my $col (keys %free_dict) {
            $solution[$col] = $free_dict{$col};
        }
        
        for my $i (reverse 0..$#pivot_cols) {
            next if $i >= @reduced_A;
            my $col = $pivot_cols[$i];
            my $pivot_val = ($i < @reduced_A && $col < @{$reduced_A[$i]}) ? $reduced_A[$i][$col] : 0;
            next if $pivot_val == 0;
            
            my $rhs = $reduced_b[$i];
            for my $j (0..$num_buttons-1) {
                if ($j != $col && $j < @{$reduced_A[$i]}) {
                    $rhs -= $reduced_A[$i][$j] * $solution[$j];
                }
            }
            
            return undef if $rhs % $pivot_val != 0;
            $solution[$col] = $rhs / $pivot_val;
            return undef if $solution[$col] < 0;
        }
        
        my @test_joltages = (0) x $num_lights;
        for my $btn_idx (0..$#buttons) {
            for my $light (@{$buttons[$btn_idx]}) {
                if ($light >= 0 && $light < $num_lights) {
                    $test_joltages[$light] += $solution[$btn_idx];
                }
            }
        }
        
        my $matches = 1;
        for my $i (0..$num_lights-1) {
            if ($test_joltages[$i] != $joltages[$i]) {
                $matches = 0;
                last;
            }
        }
        
        return $matches ? sum(@solution) : undef;
    }
    
    my $solve_with_free_vars_wrapper = sub {
        my ($free_assignments_ref) = @_;
        return solve_with_free_vars($free_assignments_ref, \@reduced_A, \@reduced_b, \@pivot_cols, \@buttons, \@joltages);
    };
    
    sub search_free_vars {
        my ($free_idx, $assignments_ref, $current_sum, $best_ref, $max_joltage, $free_cols_ref, $wrapper) = @_;
        if ($free_idx >= @$free_cols_ref) {
            my $result = $wrapper->($assignments_ref);
            if (defined $result) {
                $$best_ref = $result if (!defined $$best_ref || $result < $$best_ref);
            }
            return;
        }
        
        if (defined $$best_ref && $current_sum >= $$best_ref) {
            return;
        }
        
        my $col = $free_cols_ref->[$free_idx];
        my $max_val = $max_joltage;
        if (defined $$best_ref) {
            $max_val = $max_val < ($$best_ref - $current_sum) ? $max_val : ($$best_ref - $current_sum);
        }
        
        for my $val (0..$max_val) {
            my @new_assignments = (@$assignments_ref, [$col, $val]);
            search_free_vars($free_idx + 1, \@new_assignments, $current_sum + $val, $best_ref, $max_joltage, $free_cols_ref, $wrapper);
            last if defined $$best_ref && $current_sum + $val >= $$best_ref;
        }
    }
    
    search_free_vars(0, [], 0, \$best, $max_joltage, \@free_cols, $solve_with_free_vars_wrapper);
    return $best;
}

sub sum {
    my $total = 0;
    $total += $_ for @_;
    return $total;
}

sub solve_part2_ilp {
    my ($buttons_ref, $joltages_ref, $target_pattern_ref) = @_;
    my @buttons = @$buttons_ref;
    my @joltages = @$joltages_ref;
    my @target_pattern = @$target_pattern_ref;
    
    my $num_buttons = scalar @buttons;
    my $num_lights = scalar @joltages;
    
    # Base case: all joltages are 0
    my $all_zero = 1;
    for my $j (@joltages) {
        $all_zero = 0 if $j != 0;
    }
    return 0 if $all_zero;
    
    # Bifurcate approach: try all parity solutions (buttons pressed 0 or 1 time)
    # Build button matrix for GF(2) operations
    my @button_matrix = ();
    for my $btn_idx (0..$#buttons) {
        push @button_matrix, [(0) x $num_lights];
        for my $light (@{$buttons[$btn_idx]}) {
            if ($light >= 0 && $light < $num_lights) {
                $button_matrix[$btn_idx][$light] = 1;
            }
        }
    }
    
    # Required parity: pattern ON = odd, pattern OFF = even
    my @required_parity = @target_pattern;
    
    # Find minimum parity presses needed (Part 1 style)
    my $min_parity_presses = gaussian_elimination_gf2(\@button_matrix, \@required_parity);
    
    if (defined $min_parity_presses) {
        my $best_result = undef;
        my $max_parity_to_try = $num_buttons;
        
        for my $k ($min_parity_presses..$max_parity_to_try) {
            # Pruning: if we already have a solution and k is getting large, skip
            last if defined $best_result && $k > $best_result;
            
            my $combos_ref = combinations($num_buttons, $k);
            for my $button_combo (@$combos_ref) {
                # Simulate pressing these buttons once
                my @resulting_joltages = @joltages;
                for my $btn_idx (@$button_combo) {
                    for my $light (@{$buttons[$btn_idx]}) {
                        if ($light >= 0 && $light < $num_lights) {
                            $resulting_joltages[$light]++;
                        }
                    }
                }
                
                # Check if parity matches
                my @resulting_parity = map { $_ % 2 == 1 ? 1 : 0 } @resulting_joltages;
                my $parity_matches = 1;
                for my $i (0..$num_lights-1) {
                    if ($resulting_parity[$i] != $required_parity[$i]) {
                        $parity_matches = 0;
                        last;
                    }
                }
                
                if ($parity_matches) {
                    # Check if all remaining are even (can divide by 2)
                    my $all_even = 1;
                    for my $j (@resulting_joltages) {
                        $all_even = 0 if $j % 2 != 0;
                    }
                    
                    if ($all_even) {
                        # Divide by 2 and recurse
                        my @halved_joltages = map { $_ / 2 } @resulting_joltages;
                        my $remaining_result = solve_part2_ilp(\@buttons, \@halved_joltages, $target_pattern_ref);
                        if (defined $remaining_result) {
                            my $total = $k + $remaining_result * 2;
                            $best_result = $total if (!defined $best_result || $total < $best_result);
                        }
                    }
                }
            }
        }
        
        return $best_result if defined $best_result;
    }
    
    # If no bifurcate solution found, fall back to fallback method
    return solve_part2_ilp_fallback($buttons_ref, $joltages_ref, $target_pattern_ref);
}

sub solve {
    my ($input_data) = @_;
    my @lines = split /\n/, $input_data;
    return ("0", "0") unless @lines;
    
    my $part1_total = 0;
    my $part2_total = 0;
    
    for my $line (@lines) {
        $line =~ s/^\s+|\s+$//g;
        next unless $line;
        
        my ($target_pattern_ref, $buttons_ref, $joltages_ref) = parse_line($line);
        my @target_pattern = @$target_pattern_ref;
        next unless @target_pattern;
        
        my $num_lights = scalar @target_pattern;
        
        # Part 1: GF(2) linear system
        # Build incidence matrix: matrix[i][j] = True if button i toggles light j
        my @button_matrix = ();
        my @buttons = @$buttons_ref;
        for my $i (0..$#buttons) {
            push @button_matrix, [(0) x $num_lights];
            for my $light (@{$buttons[$i]}) {
                if ($light >= 0 && $light < $num_lights) {
                    $button_matrix[$i][$light] = 1;
                }
            }
        }
        
        # Target: all start OFF, need to toggle to match pattern
        # Required toggles: target_pattern (1 = needs toggle, 0 = no toggle)
        my @required_toggles = @target_pattern;
        
        my $result = gaussian_elimination_gf2(\@button_matrix, \@required_toggles);
        if (defined $result) {
            $part1_total += $result;
        }
        
        # Part 2: Integer Linear Programming
        my @joltages = @$joltages_ref;
        if (@joltages && scalar @joltages == $num_lights) {
            my $result2 = solve_part2_ilp($buttons_ref, $joltages_ref, $target_pattern_ref);
            if (defined $result2) {
                $part2_total += $result2;
            }
        }
    }
    
    return ($part1_total, $part2_total);
}

my $data = read_input_raw("../data/input.txt");
my ($part1, $part2) = solve($data);
print "Part 1: $part1\n";
print "Part 2: $part2\n";
