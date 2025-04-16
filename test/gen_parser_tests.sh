#!/bin/bash

scanner_tests_dir="scanner/*"

for input_file in $scanner_tests_dir
do
    filename=$(basename "$input_file")
    if [[ "$filename" == "dune" ]]; then
        continue
    fi

    output_file="parser/$filename"

    final_output=""
    current_block=""
    in_test_block=false

    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ "$line" =~ test[0-9]+ ]]; then
            in_test_block=true
            current_block="$line"
        elif $in_test_block; then
            current_block+=$'\n'"$line"

            if [[ "$line" == *"))"* ]]; then
                in_test_block=false

                if [[ "$current_block" =~ Lexing\.from_string\ \"([^\"]+)\" ]]; then
                    input_string="${BASH_REMATCH[1]}"
                else
                    input_string=""
                fi

                replacement=$(printf 'let actual = Parser.program Scanner.tokenize lexbuf in print_endline (string_of_program program)')

                current_block=$(echo "$current_block" | sed -E "s/let actual = .*/$(echo "$replacement" | sed 's/[&/\]/\\&/g')/")

                if [ -n "$input_string" ]; then
                    escaped=$(echo "$input_string" | sed 's/"/\\\"/g')
                    echo $escaped
                    current_block=$(echo "$current_block" | sed -E "s#let expected = .*#let expected = \"$escaped\"#")
                fi

                final_output+="$current_block"$'\n\n'
                current_block=""
            fi
        else
            final_output+="$line"$'\n'
        fi
    done < "$input_file"

    echo "$final_output" > "$output_file"
done
