#!/usr/bin/env bash

find data/realNotes/ -name "*_out.enex" -delete
find data/realNotes -name "*.enex" -exec ./script.hs {}  \;
# ./script.hs "./data/realNotes/Programming/Programming.enex"
# ./script.hs "./data/realNotes/Programming/ProgrammingConcepts.enex"
# ./script.hs "./data/realNotes/Programming/ProgrammingReadings.enex"
# ./script.hs "./data/realNotes/Programming/Haskell.enex"
# ./script.hs "./data/realNotes/Programming/Vim.enex"
# ./script.hs "./data/realNotes/Programming/Unix based systems.enex"
# ./script.hs "./data/realNotes/Programming/Tools.enex"
# ./script.hs "./data/realNotes/Silent/Shorts.enex"
# ./script.hs "./data/realNotes/Silent/Studies.enex"
find data/realNotes/ -name "*_out.enex"
