#!/bin/bash

# Add a single reading to readings.csv

AUTHOR="Francis Windram"
DATE="18/03/19"
VERSION="1.0.0"

set -o errexit -o pipefail -o noclobber -o nounset

function show_usage {
    echo -e "\nUsage: $0 [-h/--help] [--coloff] [measurement] [reading]\n"

    echo -e "Add a single reading to readings.csv\n"

    echo -e "    -h/--help              Show this help text."
    echo -e "    -d/--dryrun            Disable writing to readings file."
    echo -e "    --coloff               Disable coloured output."
    echo -e "    measurement            a meter measurement"
    echo -e "    date                   date of reading in yyyy/mm/dd format (current date if blank)\n\n"
exit -1
}

! getopt --test > /dev/null
if [[ ${PIPESTATUS[0]} -ne 4 ]]; then
    echo "I’m sorry, `getopt --test` failed in this environment."
    exit 1
fi

OPTIONS=hd
LONGOPTS=help,dryrun,coloff

# -use ! and PIPESTATUS to get exit code with errexit set
# -temporarily store output to be able to check for errors
# -activate quoting/enhanced mode (e.g. by writing out “--options”)
# -pass arguments only via   -- "$@"   to separate them correctly
! PARSED=$(getopt --options=$OPTIONS --longoptions=$LONGOPTS --name "$0" -- "$@")
if [[ ${PIPESTATUS[0]} -ne 0 ]]; then
    # e.g. return value is 1
    #  then getopt has complained about wrong arguments to stdout
    exit 2
fi
# read getopt’s output this way to handle the quoting right:
eval set -- "$PARSED"

coloff=n
dryrun=n
# now enjoy the options in order and nicely split until we see --
while true; do
    case "$1" in
        -h|--help)
            show_usage
            ;;
        -d|--dryrun)
            dryrun=y
            shift
            ;;
        --coloff)
            coloff=y
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            echo -e "${ERR}ERROR: Programming error${NC}"
            exit 3
            ;;
    esac
done

# Enable colours asap if needed
if [ $coloff == "y" ] ; then
    NC="\033[0m"
    ERR=${NC}
    BERR=${NC}
    WARN=${NC}
    OK=${NC}
    LOK=${NC}
    BINPUT=${NC}
    INPUT=${NC}
    INFO=${NC}
else
    NC="\033[0m"
    ERR="\033[0;31m"
    BERR="\033[1;31m"
    WARN="\033[1;33m"
    OK="\033[1;32m"
    LOK="\033[0;32m"
    BINPUT="\033[1;34m"
    INPUT="\033[0;34m"
    INFO="\033[0;33m"
fi

# handle non-option arguments
if [[ $# < 1 ]]; then
    echo -e "${ERR}ERROR: $0: A measurement is required.${NC}"
    show_usage
    exit 4
fi


#####
# Main script

DATE=`date +%d/%m/%Y`
FILEPATH='/home/francis/Coding/BarnMeter/Data/readings.csv'

if [[ $1 =~ ^[0-9]+$ ]]
then
    MEASUREMENT=$1
else
    echo -e "${ERR}ERROR: $0: Arg 1 must be a positive integer${NC}"
    exit 1
fi

# Assign date var to a fake value if needed
if [[ $# -eq 1 ]]; then
    MEASUREDATE="NULL"
else
    MEASUREDATE=$2
fi


# Check date format
if [[ $MEASUREDATE =~ ^[0-9]{4}/[0-9]{2}/[0-9]{2}$ ]] && date -d "$MEASUREDATE" >/dev/null 2>&1
then
    MEASUREDATE=`date -d $MEASUREDATE +%d/%m/%Y`
else
    echo -e "${WARN}WARNING: Provided date not in the form yyyy/mm/dd\nUsing current date...${NC}"
    MEASUREDATE=$DATE
fi

OUTPUT="$MEASUREDATE,$MEASUREMENT,"


if [ $dryrun == "y" ] ; then
    echo -e "${OK}$OUTPUT${NC}"
else
    echo -e "$OUTPUT" >> $FILEPATH
    echo -e "${OK}'$OUTPUT' written to $FILEPATH${NC}"
fi
