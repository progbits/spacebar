#!/usr/bin/env bats

root_dir=$(git rev-parse --show-toplevel)

spacebar_bin="${root_dir}/_build/default/spacebar.exe"

@test "basic_main" {
    cat test/basic_main.c | "${spacebar_bin}" | wspace /dev/stdin
}

@test "basic_stack_allocation" {
    output="$(cat test/basic_stack_allocation.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "41 102 426 Done." ]
}

@test "basic_scope" {
    output="$(cat test/basic_scope.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "41 42 43 44 45 44 43 42 41 Done." ]
}

@test "basic_input" {
    tmp_file=$(tempfile)
    cat test/basic_input.c | "${spacebar_bin}" >> ${tmp_file}
    output="$(printf '43\n' | wspace ${tmp_file} | head -n 1)"
    echo $output
    [ "$output" == "+Done." ]
}

@test "basic_output" {
    output="$(cat test/basic_output.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "*Done." ]
}

@test "basic_function" {
    output="$(cat test/basic_function.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "43Done." ]
}

@test "basic_return" {
    output="$(cat test/basic_return.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "43Done." ]
}

@test "basic_if" {
    output="$(cat test/basic_if.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "42 84 42   Done." ]
}

@test "basic_logical_or" {
    output="$(cat test/basic_logical_or.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "2 3 4 5 Done." ]
}

@test "basic_while" {
    output="$(cat test/basic_while.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "*+,-./0123Done." ]
}

@test "basic_do_while" {
    output="$(cat test/basic_do_while.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "1 Done." ]
}

@test "basic_while_break" {
    output="$(cat test/basic_while_break.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "1 2 3 4 5 6 7 8 9 10 11 Done." ]
}

@test "basic_while_continue" {
    output="$(cat test/basic_while_continue.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "1 2 4 6 7 8 9 10 Done." ]
}

@test "basic_for" {
    output="$(cat test/basic_for.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "0 1 2 3 4 5 6 7 8 9 Done." ]
}

@test "basic_for_init" {
    output="$(cat test/basic_for_init.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "5 6 7 8 9 Done." ]
}

@test "basic_nested_for" {
    output="$(cat test/basic_nested_for.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "0 0 0 1 0 2 0 3 1 0 1 1 1 2 1 3 2 0 2 1 2 2 2 3 3 0 3 1 3 2 3 3 Done." ]
}

@test "basic_arith" {
    output="$(cat test/basic_arith.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "89Done." ]
}

@test "basic_prefix_addition" {
    output="$(cat test/basic_prefix_addition.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "1 2 3 4 5 6 7 8 9 10 Done." ]
}

@test "basic_prefix_subtraction" {
    output="$(cat test/basic_prefix_subtraction.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "9 8 7 6 5 4 3 2 1 0 Done." ]
}

@test "basic_postfix_addition" {
    output="$(cat test/basic_postfix_addition.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "0 1 2 3 4 5 6 7 8 9 Done." ]
}

@test "basic_postfix_subtraction" {
    output="$(cat test/basic_postfix_subtraction.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "10 9 8 7 6 5 4 3 2 1 Done." ]
}

@test "basic_pointer" {
    output="$(cat test/basic_pointer.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "42Done." ]
}

@test "basic_array" {
    output="$(cat test/basic_array.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "0 10 20 30 40 50 60 70 Done." ]
}

@test "basic_multiple_array" {
    output="$(cat test/basic_multiple_array.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "1 12 23 34 45 56 67 78 Done." ]
}

@test "fizz_buzz" {
    output="$(cat test/fizz_buzz.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "0 0 0 3 5 6 9 10 12 15 15 15 Done." ]
}

@test "fibonacci" {
    output="$(cat test/fib.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "233 Done." ]
}

