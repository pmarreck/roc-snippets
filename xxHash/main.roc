app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br"
}

import pf.Stdout exposing [line]
import pf.Stdin exposing [readAllBytes]
import pf.Args exposing [all]
import pf.Exit exposing [Exit]
import Num exposing [shiftLeftBy, shiftRightBy, bitwiseAnd, bitwiseOr, bitwiseXor]
import Num.U64 exposing [fromU8, fromU32, fromNat, toHex]
import Num.U32 exposing [fromU8]
import List exposing [len]
import String exposing [toUtf8]
import Task

# Constants for xxHash64 algorithm
PRIME64_1 : U64
PRIME64_1 = 0x9E3779B185EBCA87

PRIME64_2 : U64
PRIME64_2 = 0xC2B2AE3D27D4EB4F

PRIME64_3 : U64
PRIME64_3 = 0x165667B19E3779F9

PRIME64_4 : U64
PRIME64_4 = 0x85EBCA77C2B2AE63

PRIME64_5 : U64
PRIME64_5 = 0x27D4EB2F165667C5

# Rotate left operation for U64
rotateLeft : U64, U32 -> U64
rotateLeft = \value, count ->
    left = shiftLeftBy value count
    right = shiftRightBy value (64 - count)
    bitwiseOr left right

# Round function used in the main loop
round : U64, U64 -> U64
round = \acc, input ->
    acc1 = acc + input * PRIME64_2
    acc2 = rotateLeft acc1 31
    acc2 * PRIME64_1

# Merge function used in the finalization
merge : U64, U64 -> U64
merge = \hash, acc ->
    acc1 = round 0 acc
    hash1 = bitwiseXor hash acc1
    hash1 * PRIME64_1 + PRIME64_4

# Function to read 64-bit unsigned integer in little-endian
readU64LE : List U8, Nat -> U64
readU64LE = \data, index ->
    part0 = fromU8 (data[index + 0])
    part1 = shiftLeftBy (fromU8 (data[index + 1])) 8
    part2 = shiftLeftBy (fromU8 (data[index + 2])) 16
    part3 = shiftLeftBy (fromU8 (data[index + 3])) 24
    part4 = shiftLeftBy (fromU8 (data[index + 4])) 32
    part5 = shiftLeftBy (fromU8 (data[index + 5])) 40
    part6 = shiftLeftBy (fromU8 (data[index + 6])) 48
    part7 = shiftLeftBy (fromU8 (data[index + 7])) 56

    part0
        |> bitwiseOr part1
        |> bitwiseOr part2
        |> bitwiseOr part3
        |> bitwiseOr part4
        |> bitwiseOr part5
        |> bitwiseOr part6
        |> bitwiseOr part7

# Function to read 32-bit unsigned integer in little-endian
readU32LE : List U8, Nat -> U32
readU32LE = \data, index ->
    part0 = fromU8 (data[index + 0])
    part1 = shiftLeftBy (fromU8 (data[index + 1])) 8
    part2 = shiftLeftBy (fromU8 (data[index + 2])) 16
    part3 = shiftLeftBy (fromU8 (data[index + 3])) 24

    part0
        |> bitwiseOr part1
        |> bitwiseOr part2
        |> bitwiseOr part3

# Main xxHash64 function
xxHash64 : List U8, U64 -> U64
xxHash64 = \data, seed ->
    lenData = len data

    # Initialize variables
    (hash, index) =
        if lenData >= 32 then
            # Initialize accumulators
            acc1 = seed + PRIME64_1 + PRIME64_2
            acc2 = seed + PRIME64_2
            acc3 = seed + 0
            acc4 = seed - PRIME64_1

            # Process chunks of 32 bytes
            (acc1Final, acc2Final, acc3Final, acc4Final, indexFinal) =
                processChunks data lenData 0 acc1 acc2 acc3 acc4

            hashCombined =
                rotateLeft acc1Final 1
                    + rotateLeft acc2Final 7
                    + rotateLeft acc3Final 12
                    + rotateLeft acc4Final 18

            hashMerged =
                hashCombined
                    |> merge acc1Final
                    |> merge acc2Final
                    |> merge acc3Final
                    |> merge acc4Final

            (hashMerged + fromNat lenData, indexFinal)
        else
            (seed + PRIME64_5 + fromNat lenData, 0)

    # Process remaining data
    hashAfter8Bytes = processRemainingChunks8 data lenData index hash
    hashAfter4Bytes = processRemainingChunks4 data lenData index hashAfter8Bytes
    finalHash = processRemainingBytes data lenData index hashAfter4Bytes

    # Finalization mix
    finalizeHash finalHash

# Process chunks of 32 bytes
processChunks : List U8, Nat, Nat, U64, U64, U64, U64 -> (U64, U64, U64, U64, Nat)
processChunks = \data, lenData, index, acc1, acc2, acc3, acc4 ->
    if index <= (lenData - 32) then
        acc1New = round acc1 (readU64LE data index)
        acc2New = round acc2 (readU64LE data (index + 8))
        acc3New = round acc3 (readU64LE data (index + 16))
        acc4New = round acc4 (readU64LE data (index + 24))
        processChunks data lenData (index + 32) acc1New acc2New acc3New acc4New
    else
        (acc1, acc2, acc3, acc4, index)

# Process remaining chunks of 8 bytes
processRemainingChunks8 : List U8, Nat, Nat, U64 -> U64
processRemainingChunks8 = \data, lenData, index, hash ->
    if index <= (lenData - 8) then
        k1 = readU64LE data index
            |> (\v -> v * PRIME64_2)
            |> (\v -> rotateLeft v 31)
            |> (\v -> v * PRIME64_1)
        hashNew = hash
            |> bitwiseXor k1
            |> (\v -> rotateLeft v 27)
            |> (\v -> v * PRIME64_1 + PRIME64_4)
        processRemainingChunks8 data lenData (index + 8) hashNew
    else
        hash

# Process remaining chunks of 4 bytes
processRemainingChunks4 : List U8, Nat, Nat, U64 -> U64
processRemainingChunks4 = \data, lenData, index, hash ->
    if index <= (lenData - 4) then
        k1 = fromU32 (readU32LE data index)
            |> (\v -> v * PRIME64_1)
        hashNew = hash
            |> bitwiseXor k1
            |> (\v -> rotateLeft v 23)
            |> (\v -> v * PRIME64_2 + PRIME64_3)
        processRemainingChunks4 data lenData (index + 4) hashNew
    else
        hash

# Process remaining bytes
processRemainingBytes : List U8, Nat, Nat, U64 -> U64
processRemainingBytes = \data, lenData, index, hash ->
    if index < lenData then
        k1 = fromU8 data[index]
            |> (\v -> v * PRIME64_5)
        hashNew = hash
            |> bitwiseXor k1
            |> (\v -> rotateLeft v 11)
            |> (\v -> v * PRIME64_1)
        processRemainingBytes data lenData (index + 1) hashNew
    else
        hash

# Finalization mix
finalizeHash : U64 -> U64
finalizeHash = \hash ->
    hash
        |> (\h -> bitwiseXor h (shiftRightBy h 33))
        |> (\h -> h * PRIME64_2)
        |> (\h -> bitwiseXor h (shiftRightBy h 29))
        |> (\h -> h * PRIME64_3)
        |> (\h -> bitwiseXor h (shiftRightBy h 32))

# Function to process input bytes and output the hash
processInputBytes : List U8 -> Task {} [Exit I32 Str]
processInputBytes = \inputBytes ->
    hash = xxHash64 inputBytes 0
    hashHex = toHex hash
    line hashHex
        |> Task.map \_ -> {}

# Main function
main : Task {} [Exit I32 Str]
main =
    readAllBytes
        |> Task.andThen \inputBytes ->
            if len inputBytes > 0 then
                # There is piped-in input
                processInputBytes inputBytes
            else
                # No piped-in input, check command-line arguments
                args = all

                when args is
                    [] ->
                        line "Error: No input provided."
                            |> Task.andThen \_ ->
                                Task.err (Exit 1 "No input provided.")
                    [input] ->
                        inputBytes = toUtf8 input
                        processInputBytes inputBytes
                    _ ->
                        line "Error: Too many arguments."
                            |> Task.andThen \_ ->
                                Task.err (Exit 1 "Too many arguments.")
