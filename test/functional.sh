#!/usr/bin/env bats

root_dir=$(git rev-parse --show-toplevel)

spacebar_bin="${root_dir}/_build/default/spacebar.exe"

@test "basic_main" {
    cat test/basic_main.c | "${spacebar_bin}" | wspace /dev/stdin
}

@test "basic_output" {
    output="$(cat test/basic_output.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "*Done." ]
}

@test "basic_function" {
    output="$(cat test/basic_function.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "+Done." ]
}

@test "basic_while" {
    output="$(cat test/basic_while.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "*+,-./01234Done." ]
}

@test "basic_arith" {
    output="$(cat test/basic_arith.c | "${spacebar_bin}" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "XDone." ]
}

