#!/bin/bash

# This script assumes Linux (Android ?) and is for testing ddpt which is a
# dd clone. It also assumes the sg3_utils package is loaded (for
# sg_decode_sense) It uses ramdisks (/dev/ram0 and /dev/ram1) so users may
# need to check if something else is using them. It also uses two scsi_debug
# virtual disks (suggesting a scsi_debug modprobe instruction if they are
# not already present. It also uses some /tmp/ddpt*.bin scratch files which
# may need to be cleaned up by the user. It uses lsscsi (version 0.30
# revision 149 or later) to identify the scratch scsi_debug generic and block
# devices (lest it accidentally overwritesthe users real disks or SSDs). Use:
#   ./prepare.sh --help      or     ./prepare.sh -h
# to see the command run options. By default (i.e. without the --run) option
# it will do the preparation only. It is recommended that the user do
#   ./prepare.sh --verbose    or    ./prepare.sh -vv
# before using the --run option. Check that the devices that ./prepare.sh
# selects are actually the correct ones ...
#
# Environment variables that may be overridden by caller:
#    DDPT         default: `which ddpt`
#    DDPT_OPTS    default: ""; command line options -d and -q appended
#    DDPT_ARG     default: ""; command line option -a argument appended
#
# and these Environment variables are consumed by the ddpt utility:
#    DDPT_DEF_BS        the default block size is 512 bytes, this overrides
#    ODX_RTF_LEN        add 8 bytes to ROD length (512) that contain file
#                       size in bytes (see ddpt manpage)
#
# dpg 20180714

VERBOSE="0"
VERSION="1.02 20180714 [r361]"
VB_ARG=""
DDPT_OPTS=""

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

OPTIONS=a:dfhqrvV
LONGOPTIONS=arg:,dry-run,force,help,quiet,run,verbose,version

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
            DDPT_ARG="$2"
            shift 2
            ;;
        -d|--dry-run)
            DRY_RUN=y
            DDPT_OPTS="${DDPT_OPTS} --dry-run"
            shift
            ;;
        -f|--force)
            FORCE=y
            shift
            ;;
        -h|--help)
            HELP=y
            shift
            ;;
        -q|--quiet)
            QUIET=y
            DDPT_OPTS="${DDPT_OPTS} --quiet"
            shift
            ;;
        -r|--run)
            RUN=y
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
    echo -n "prepare.sh  [--arg=DA] [--dry-run] [--force] [--help] "
    echo "[--quiet]"
    echo "               [--run] [--verbose] [--version]"
    echo "where:"
    echo "  --arg=DA|-a DA    DA arbitrary argument passed to ddpt calls"
    echo "  --dry-run|-d    bypasses copy (or read) with ddpt"
    echo "  --force|-f      override some checks"
    echo "  --help|-h       outputs this usage message then exits"
    echo "  --quiet|-q      suppress output from ddpt calls"
    echo -n "  --run|-r        run ddpt calls (default: prepare but don't "
    echo "run)"
    echo -n "  --verbose|-v    increase verbosity in this scripts and ddpt "
    echo "calls"
    echo "  --version|-V    print out version number then exit"
    echo ""
    echo "Script for collecting 'safe' block and SCSI generic devices and"
    echo "then optionally run various ddpt calls using those devices. Also"
    echo "uses '/tmp/ddpt*.bin' for temporary storage."
    exit 0
fi

##  # handle non-option arguments
##  if [[ $# -ne 1 ]]; then
##      echo "$0: A single input file is required."
##      exit 4
##  fi

DDPT_OPTS="${DDPT_OPTS} ${VB_ARG}"

# Just testing ....
if [ ${VERBOSE} -gt 0 ] ; then
    echoerr "DDPT_OPTS=${DDPT_OPTS}"
    echoerr ""
fi

if [[ $EUID -ne 0 ]]; then
   echoerr ">>> This script should be run as root, will try anyway"
   echoerr ""
fi

LSSCSI=`which lsscsi`
PREP_TMP="/tmp/ddpt_prep.tmp"
PREP_BIN="/tmp/ddpt_prep.bin"
PREP2_BIN="/tmp/ddpt_prep2.bin"

if [ ! -x ${LSSCSI} ] ; then
    echoerr "Can't find lsscsi utility, is the package loaded?"
    echoerr "Recent version at http://sg.danny.cz/scsi/lsscsi.html "
    exit 1
else
    LSSCSI_VSTR=`lsscsi -VV`
    if [ -z "${LSSCSI_VSTR}" ] ; then
        echoerr "lsscsi version is too old, not before 0.30 [rev 149]"
        echoerr "Recent version at http://sg.danny.cz/scsi/lsscsi.html "
        exit 1
    fi
fi

${LSSCSI} --hosts --brief > ${PREP_TMP}

if grep "scsi_debug" ${PREP_TMP} > /dev/null 2>&1 ; then
    echoerr "found scsi_debug host, good, now checking it ..."
    SCSI_DEBUG_HOST=`grep "scsi_debug" ${PREP_TMP}`
else
    echoerr "Did not find scsi_debug host, need to load scsi_debug module."
    echoerr "Try this (as root):"
    echoerr_n "    modprobe scsi_debug max_luns=2 sector_size=4096 "
    echoerr "dev_size_mb=400 ndelay=5000"
    exit 1
fi

unset a
a=( ${SCSI_DEBUG_HOST} )
# Remove leading [ and trailing ]
HOST_NUM=${a[0]:1}
HOST_NUM=${HOST_NUM::1}

if [ "$HOST_NUM" -eq "$HOST_NUM" ] 2>/dev/null
then
    if [ $VERBOSE -gt 0 ] ; then
        echoerr "scsi_debug driver is host number $HOST_NUM"
    fi
else
    echoerr "Didn't find scsi_debug host (with lsscsi), strange"
    exit 1
fi

${LSSCSI} --generic --brief ${HOST_NUM} > ${PREP_TMP}

unset a
k=0
while read -r line
do
    a=( ${line} )

    SG_DEV[${k}]=${a[2]}
    SG_BLK_DEV[${k}]=${a[1]}
    if [ $VERBOSE -gt 0 ] ; then
        echoerr "sg dev name ${k}: " ${SG_DEV[${k}]}
        echoerr "  corresponding block dev name ${k}: " ${SG_BLK_DEV[${k}]}
    fi
    k=$(( k + 1 ))
done < "${PREP_TMP}"

# Remove a temporary, symlinks still left in /tmp
rm -f ${PREP_TMP}

if [ ${k} -eq 0 ] ; then
    echoerr "Strange, no sg devices found (by lsscsi)"
    exit
fi

if [ ${#SG_DEV[@]} -lt 2 ] ; then
    echoerr "Need at least 2 sg devices, looks like only one. Try removing "
    echoerr "the scsi_debug module (with rmmod) and re-attach it (with "
    echoerr "modprobe) and add 'max_luns=2' argument"
    exit 1
fi

if [ ${#SG_BLK_DEV[@]} -lt 2 ] ; then
    echoerr "Something is wrong, should have at least 2 blocks devices "
    echoerr "corresponding to the sg devices just found."
    exit 1
fi

if df | grep "ram[01]"
then
    echoerr "Looks like /dev/ram0 or 1 in use (according to df)"
    echoerr "this script want to write to them, so exit unless --force given"
    if [ ${FORCE} ] ; then
	echoerr "continuing ..."
    else
        exit 1
    fi
fi

if [ -b /dev/ram0 -a -b /dev/ram1 ] ; then
    if [ $VERBOSE -gt 0 ] ; then
        echoerr "Detected /dev/ram0 and /dev/ram1 block devices"
    fi
else
    echoerr "Need 2 scratch block devices: /dev/ram0 and /dev/ram1 but not"
    echoerr "found. As root try 'mknod -m 660 /dev/ram b 1 1' or if that "
    echoerr "doesn't create them try:"
    echoerr "    mknod -m 660 /dev/ram0 b 1 0 ; mknod -m 660 /dev/ram1 b 1 1"
    exit 1
fi

if [ ${DDPT} ] ; then
    echoerr "Instead of ddpt using DDPT=${DDPT}"
else
    DDPT=`which ddpt`
fi

if [ ! ${RUN} ] ; then
    echoerr ""
    echoerr "Since --run option not given will exit now with preparation done"
    exit 0
fi
echoerr ""

rm -f /tmp/sg_a_dev
ln -s ${SG_DEV[0]} /tmp/sg_a_dev
rm -f /tmp/sg_b_dev
ln -s ${SG_DEV[1]} /tmp/sg_b_dev
rm -f /tmp/sd_sg_a_dev
ln -s ${SG_BLK_DEV[0]} /tmp/sd_sg_a_dev
rm -f /tmp/sd_sg_b_dev
ln -s ${SG_BLK_DEV[1]} /tmp/sd_sg_b_dev

# >>> RUN section start here

echoerr "${DDPT} ${DDPT_OPTS} if=/dev/ram0  bs=512 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/dev/ram0  bs=512 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} if=/dev/ram1  bs=512 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/dev/ram1  bs=512 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} if=/tmp/sg_a_dev bs=4096 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/tmp/sg_a_dev bs=4096 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} if=/tmp/sg_b_dev  bs=4096 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/tmp/sg_b_dev  bs=4096 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} if=/tmp/sd_sg_a_dev bs=4096 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/tmp/sd_sg_a_dev bs=4096 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} if=/tmp/sd_sg_b_dev  bs=4096 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/tmp/sd_sg_b_dev  bs=4096 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

# Pick 16,999,999 since its a prime and noticeable
echoerr "${DDPT} ${DDPT_OPTS} if=/dev/urandom bs=1 of=${PREP_BIN} count=16999999 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/dev/urandom bs=1 of=${PREP_BIN} count=16999999 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} if=${PREP_BIN} bs=512 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=${PREP_BIN} bs=512 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} if=${PREP_BIN} bs=512 of=${PREP2_BIN} ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=${PREP_BIN} of=${PREP2_BIN} bs=512 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} if=${PREP_BIN} bs=1 of=${PREP2_BIN} ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=${PREP_BIN} of=${PREP2_BIN} bs=1 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} urand_ram0.jf ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} urand_ram0.jf ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} ram0_2_ram1.jf ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} ram0_2_ram1.jf ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} ram1_2_sg_a.jf ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} ram1_2_sg_a.jf ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} sg_a_2_sg_b.jf ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} sg_a_2_sg_b.jf ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

echoerr "${DDPT} ${DDPT_OPTS} sd_b_2_sd_a.jf ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} sd_b_2_sd_a.jf ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

rm ${PREP2_BIN}
echoerr "${DDPT} ${DDPT_OPTS} sgl_1.jf of=${PREP2_BIN} ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} sgl_1.jf of=${PREP2_BIN} ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

# generate ascending pattern, 512 bytes long, wraps every 256 bytes
rm ${PREP2_BIN}
echoerr "${DDPT} ${DDPT_OPTS} iflag=00,ff of=${PREP2_BIN} bs=512 count=1 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} iflag=00,ff of=${PREP2_BIN} bs=512 count=1 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""

rm ${PREP_BIN}
echoerr "${DDPT} ${DDPT_OPTS} if=${PREP2_BIN} of=- bs=1 bpt=7 seek=64 ${DDPT_ARG} > ${PREP_BIN}"
${DDPT} ${DDPT_OPTS} if=${PREP2_BIN} of=- bs=1 seek=64 ${DDPT_ARG} > ${PREP_BIN}
RES=$?
if [ ${RES} -ne 0 ] ; then
    pr_exit_stat ${DDPT} ${RES}
    exit ${RES}
fi
echoerr ""


#
# throw away stdout and stderr: > /dev/null 2>&1
