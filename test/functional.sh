#!/usr/bin/env bats

root_dir=$(git rev-parse --show-toplevel)

@test "basic"  {
    cat test/basic.c | "${root_dir}/_build/default/spacebar.exe" | wspace /dev/stdin 
}

@test "basic_output"  {
    output="$(cat test/basic_output.c | "${root_dir}/_build/default/spacebar.exe" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "*Done." ]
}

@test "basic_function"  {
    output="$(cat test/basic_function.c | "${root_dir}/_build/default/spacebar.exe" | wspace /dev/stdin | head -n 1)"
    echo $output
    [ "$output" == "+Done." ]
}
