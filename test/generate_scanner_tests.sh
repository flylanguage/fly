#!/bin/bash

# Input and output directories
input_dir="sample_programs"
output_dir="scanner"

# Ensure the output directory exists
mkdir -p "$output_dir"

escape_quotes() {
    echo "$1" | sed 's/"/\\"/g'
}
# Loop over each .fly file in the input directory
for file in "$input_dir"/*.fly; do
    fname=$(basename "$file") #get filename
    fname_no_ext="${fname%.*}" #remove extension
    IFS='_' read -r part1 part2 <<< "$fname_no_ext" #split on first underscore

    feature="$part2" #store the feature we are testing
    
    test_cases=()
    current_test_case=""

    while IFS= read -r line; do
        if [[ "$line" =~ ^// ]]; then
            continue
        fi
        line=$(escape_quotes "$line")

        if [[ -n "$line" ]]; then
            current_test_case+="$line\n"
        else
            test_cases+=("$current_test_case")
            current_test_case=""
        fi
    done < "$file"

    if [[ -n "$current_test_case" ]]; then
        test_cases+=("$current_test_case")
    fi

    index=0
    test_case_code=""
    for test_case in "${test_cases[@]}"; do
        test_case_code+="
        \"test$((index+1))\" >:: (fun _ ->
        let lexbuf = Lexing.from_string \"$test_case\" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat \" \" in
        let expected = \"\" in
        assert_equal 
        expected actual
        ~printer:(fun s -> \"\\\"\" ^ s ^ \"\\\"\"));"$'\n'
        ((index++))
    done

    ocaml_test_file="open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf = 
    let tk = Scanner.tokenize lexbuf in
    match tk with
    | Fly_lib.Parser.EOF -> []
    | t -> t :: to_list lexbuf

let tests = \"testing_$feature\" >::: [
    $test_case_code
]

let _ = run_test_tt_main tests
"

    echo "$ocaml_test_file" > "$output_dir/$fname_no_ext.ml"
done


