#!/bin/bash

# This script assumes Linux (Android ?) and is for testing ddpt_sgl which
# is a helper for ddpt, a dd clone.
#
# Environment variables that may be overridden by caller:
#    DDPT_SGL        default: `which ddpt`
#    DDPT_SGL_OPTS   default: "";
#
#
# dpg 20180715

VERBOSE="0"
VERSION="1.00 20180715 [r361]"
VB_ARG=""
DDPT_SGL_OPTS=""
SGL_OUT="/tmp/ddpt"

echoerr() { printf "%s\n" "$*" >&2; }
echoerr_n() { printf "%s" "$*" >&2; }
pr_exit_stat() {
    echoerr "${1} failed; exit status: ${2}"
    # --verbose causes output --> stderr
    # needs sg3_utils-1.43 rev 777 or later
    sg_decode_sense --verbose --err="${2}"
}

getopt --test > /dev/null
if [[ $? -ne 4 ]]; then
    echoerr "I’m sorry, `getopt --test` failed in this environment."
    exit 1
fi

# Command line parser from:
#   stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash

OPTIONS=a:hqvV
LONGOPTIONS=arg:,help,quiet,verbose,version

# -temporarily store output to be able to check for errors
# -e.g. use “--options” parameter by name to activate quoting/enhanced mode
# -pass arguments only via   -- "$@"   to separate them correctly
PARSED=$(getopt --options=$OPTIONS --longoptions=$LONGOPTIONS --name "$0" -- "$@")
if [[ $? -ne 0 ]]; then
    # e.g. $? == 1
    #  then getopt has complained about wrong arguments to stdout
    exit 2
fi
# read getopt’s output this way to handle the quoting right:
eval set -- "$PARSED"

# now enjoy the options in order and nicely split until we see --
while true; do
    case "$1" in
        -a|--arg)
            DDPT_SGL_OPTS="$2"
            shift 2
            ;;
        -h|--help)
            HELP=y
            shift
            ;;
        -q|--quiet)
            QUIET=y
            DDPT_SGL_OPTS="${DDPT_SGL_OPTS} --quiet"
            shift
            ;;
        -v|--verbose)
            if [ ${VERBOSE} -eq 0 ] ; then
                VB_ARG="-v"
            else
                VB_ARG="${VB_ARG}v"
            fi
            VERBOSE=$((VERBOSE + 1))
            shift
            ;;
        -V|--version)
	    echo "${VERSION}"
	    exit
	    shift
	    ;;
        --)
            shift
            break
            ;;
        *)
            echoerr "Syntax error in script command line options"
            exit 3
            ;;
    esac
done

if [ ${HELP} ] ; then
    echo -n "test_sgl.sh  [--arg=DA] [--help] [--quiet] [--verbose] "
    echo "[--version]"
    echo "where:"
    echo "  --arg=DA|-a DA    DA arbitrary argument passed to ddpt calls"
    echo "  --help|-h       outputs this usage message then exits"
    echo "  --quiet|-q      suppress output from ddpt calls"
    echo -n "  --verbose|-v    increase verbosity in this scripts and ddpt "
    echo "calls"
    echo "  --version|-V    print out version number then exit"
    echo ""
    echo "Script for testing scatter gather list (sgl) manipulations using"
    echo "the the ddpt_sgl helper. Uses '/tmp/ddpt*.sgl' for temporary storage."
    exit 0
fi

##  # handle non-option arguments
##  if [[ $# -ne 1 ]]; then
##      echo "$0: A single input file is required."
##      exit 4
##  fi

DDPT_SGL_OPTS="${DDPT_SGL_OPTS} ${VB_ARG}"

# Just testing ....
if [ ${VERBOSE} -gt 0 ] ; then
    echoerr "DDPT_SGL_OPTS=${DDPT_SGL_OPTS}"
    echoerr ""
fi

if [[ $EUID -eq 0 ]]; then
   echoerr ">>> This script need not be run as root"
   echoerr ""
fi

if [ ${DDPT_SGL} ] ; then
    echoerr "Instead of ddpt using DDPT_SGL=${DDPT_SGL}"
else
    DDPT_SGL=`which ddpt_sgl`
fi

# >>> RUN section start here

# Copy command line sgl {{1,100k},} to /tmp/ddpt00.sgl
echoerr "${DDPT_SGL} ${DDPT_SGL_OPTS} -A 1,100k --out=${SGL_OUT}00 --extension=sgl"
${DDPT_SGL} ${DDPT_SGL_OPTS} -A 1,100k --out=${SGL_OUT}00 --extension=sgl
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT_SGL} ${RES}
    exit ${RES}
fi
echoerr ""

# Split sgl in /tmp/ddpt00.sgl into 4 sgls with 3k interleave, output to /tmp/ddpt[1-4].sgl
echoerr "${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}00.sgl -i 3k --out=${SGL_OUT} --extension=sgl --action=split_4"
${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}00.sgl -i 3k --out=${SGL_OUT} --extension=sgl --action=split_4
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT_SGL} ${RES}
    exit ${RES}
fi
echoerr ""

# Use cat on first three split pieces:
echoerr "cat ${SGL_OUT}1.sgl ${SGL_OUT}2.sgl ${SGL_OUT}3.sgl > ${SGL_OUT}1_2_3.sgl"
cat ${SGL_OUT}1.sgl ${SGL_OUT}2.sgl ${SGL_OUT}3.sgl > ${SGL_OUT}1_2_3.sgl
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT_SGL} ${RES}
    exit ${RES}
fi
echoerr ""

# Use --action=append to append last split file
echoerr "${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}1_2_3.sgl -B @${SGL_OUT}4.sgl --out=${SGL_OUT}1234 --extension=sgl --action=append"
${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}1_2_3.sgl -B @${SGL_OUT}4.sgl --out=${SGL_OUT}1234 --extension=sgl --action=append
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT_SGL} ${RES}
    exit ${RES}
fi
echoerr ""

# Now check if reconstructed file is non-overlapping (it should be)
echoerr "${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}1234.sgl --non-overlap"
${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}1234.sgl --non-overlap
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT_SGL} ${RES}
    exit ${RES}
fi
echoerr ""

# Now check if original sgl is equal to reconstructed sgl (it shouldn't be)
echoerr "${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}00.sgl -B @${SGL_OUT}1234.sgl --action=equal"
${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}00.sgl -B @${SGL_OUT}1234.sgl --action=equal
RES=$?
if [ ${RES} -eq 36 ] ; then
    echoerr "Expected false exit status (36) and got it"
else
    echoerr "Expected false exit status (36) but did _not_ get it"
    pr_exit_stat ${DDPT_SGL} ${RES}
fi
echoerr ""

# Now check if original sgl is the same as reconstructed sgl (it should be)
echoerr "${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}00.sgl -B @${SGL_OUT}1234.sgl --action=same"
${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}00.sgl -B @${SGL_OUT}1234.sgl --action=same
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT_SGL} ${RES}
    exit ${RES}
fi
echoerr ""

# Now sort reconstructed sgl
echoerr "${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}1234.sgl --out=${SGL_OUT}_sort --extension=sgl --action=sort"
${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}1234.sgl --out=${SGL_OUT}_sort --extension=sgl --action=sort
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT_SGL} ${RES}
    exit ${RES}
fi
echoerr ""

# Now check if original sgl is equal to sorted sgl (it should be)
echoerr "${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}00.sgl -B @${SGL_OUT}_sort.sgl --action=equal"
${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}00.sgl -B @${SGL_OUT}_sort.sgl --action=equal
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT_SGL} ${RES}
    exit ${RES}
fi
echoerr ""

# Now try out to-chs (to cylinders/heads/sectors)
echoerr "${DDPT_SGL} ${DDPT_SGL_OPTS} -A 0,10k --out=${SGL_OUT}_chs -e sgl --chs=768,16,255 -a to-chs"
${DDPT_SGL} ${DDPT_SGL_OPTS} -A 0,10k --out=${SGL_OUT}_chs -e sgl --chs=768,16,255 -a to-chs
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT_SGL} ${RES}
    exit ${RES}
fi
echoerr ""

# Since to-chs mapping doesn't maaintain ascending order, do a twin sort
echoerr "${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}_chs.sgl -B 0,10k -o ${SGL_OUT}_chs_sort -e sgl -a tsort"
${DDPT_SGL} ${DDPT_SGL_OPTS} -A @${SGL_OUT}_chs.sgl -B 0,10k -o ${SGL_OUT}_chs_sort -e sgl -a tsort
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT_SGL} ${RES}
    exit ${RES}
fi
echoerr ""

#
# throw away stdout and stderr: > /dev/null 2>&1


