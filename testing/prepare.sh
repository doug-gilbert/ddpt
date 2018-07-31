K 14
svn:executable
V 1
*
PROPS-END
#!/bin/bash

VERBOSE="0"
VB_ARG=""
DDPT_OPTS=""

# Command line parser from:
#   stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash


getopt --test > /dev/null
if [[ $? -ne 4 ]]; then
    echo "I’m sorry, `getopt --test` failed in this environment."
    exit 1
fi

OPTIONS=a:dho:qrv
LONGOPTIONS=arg:,dry-run,help,output:,quiet,run,verbose

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
        -h|--help)
            HELP=y
            shift
            ;;
        -o|--output)
            outFile="$2"
            shift 2
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
        --)
            shift
            break
            ;;
        *)
            echo "Programming error"
            exit 3
            ;;
    esac
done

if [ ${HELP} ] ; then
    echo -n "prepare.sh  [--arg=DA] [--dry-run] [--help] [--output=OF] "
    echo "[--quiet]"
    echo "               [--run] [--verbose]"
    echo "where:"
    echo "  --arg=DA|-a DA    DA arbitrary argument passed to ddpt calls"
    echo "  --dry-run|-d    bypasses copy (or read) with ddpt"
    echo "  --help|-h       outputs this usage message then exits"
    echo "  --output=OF|-o OF    not implemented yet, placeholder"
    echo "  --quiet|-q      suppress output from ddpt calls"
    echo -n "  --run|-r        run ddpt calls (default: prepare but don't "
    echo "run)"
    echo -n "  --verbose|-v    increase verbosity in this scripts and ddpt "
    echo "calls"
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
    echo "DDPT_OPTS=${DDPT_OPTS}"
    echo ""
fi

if [[ $EUID -ne 0 ]]; then
   echo ">>> This script should be run as root, will try anyway"
   echo ""
fi

LSSCSI=`which lsscsi`
PREP_TMP="/tmp/ddpt_prep.tmp"
PREP2_TMP="/tmp/ddpt_prep2.tmp"

if [ ! -x ${LSSCSI} ] ; then
    echo "Can't find lsscsi utility, is the package loaded"
    exit 1
else
    LSSCSI_VSTR=`lsscsi -VV`
    if [ -z "${LSSCSI_VSTR}" ] ; then
        echo "lsscsi version is to old, not before 0.30 [rev 149]"
        exit 1
    fi
fi

${LSSCSI} --hosts --brief > ${PREP_TMP}

if grep "scsi_debug" ${PREP_TMP} > /dev/null 2>&1 ; then
    echo "found scsi_debug host, good, now checking it ..."
    SCSI_DEBUG_HOST=`grep "scsi_debug" ${PREP_TMP}`
else
    echo "Did not find scsi_debug host, need to load scsi_debug module."
    echo "Try this (as root):"
    echo -n "    modprobe scsi_debug max_luns=2 sector_size=4096 "
    echo "dev_size_mb=400 ndelay=5000"
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
        echo "scsi_debug driver is host number $HOST_NUM"
    fi
else
    echo "Didn't find scsi_debug host (with lsscsi), strange"
    exit 1
fi

## echo ${a[1]} ;  echo ${a[2]}

${LSSCSI} --generic --brief ${HOST_NUM} > ${PREP_TMP}

unset a
k=0
while read -r line
do
    a=( ${line} )

    SG_DEV[${k}]=${a[2]}
    SG_BLK_DEV[${k}]=${a[1]}
    if [ $VERBOSE -gt 0 ] ; then
        echo "sg dev name ${k}: " ${SG_DEV[${k}]}
        echo "  corresponding block dev name ${k}: " ${SG_BLK_DEV[${k}]}
    fi
    k=$(( k + 1 ))
done < "${PREP_TMP}"

if [ ${k} -eq 0 ] ; then
    echo "Strange, no sg devices found (in lsscsi)"
    exit
fi

if [ ${#SG_DEV[@]} -lt 2 ] ; then
    echo "Need at least 2 sg devices, looks like only one. Try removing "
    echo "the scsi_debug module (with rmmod) and re-attach it (with "
    echo "modprobe) and add 'max_luns=2' argument"
    exit 1
fi

if [ ${#SG_BLK_DEV[@]} -lt 2 ] ; then
    echo "Something is wrong, should have at least 2 blocks devices "
    echo "corresponding to the sg devices just found."
    exit 1
fi

if [ -b /dev/ram0 -a -b /dev/ram1 ] ; then
    if [ $VERBOSE -gt 0 ] ; then
        echo "Detected /dev/ram0 and /dev/ram1 block devices"
    fi
else
    echo "Need 2 scratch block devices: /dev/ram0 and /dev/ram1 but not found"
    echo "As root try 'mknod -m 660 /dev/ram b 1 1' or if that doesn't "
    echo "create them try:"
    echo "    mknod -m 660 /dev/ram0 b 1 0 ; mknod -m 660 /dev/ram1 b 1 1"
    exit 1
fi

if [ ! ${RUN} ] ; then
    echo ""
    echo "Since --run option not given will exit now with preparation done"
    exit 0
fi
echo ""

rm -f /tmp/sg_a_dev
ln -s ${SG_DEV[0]} /tmp/sg_a_dev
rm -f /tmp/sg_b_dev
ln -s ${SG_DEV[1]} /tmp/sg_b_dev
rm -f /tmp/sd_sg_a_dev
ln -s ${SG_BLK_DEV[0]} /tmp/sd_sg_a_dev
rm -f /tmp/sd_sg_b_dev
ln -s ${SG_BLK_DEV[1]} /tmp/sd_sg_b_dev


DDPT=`which ddpt`

echo "${DDPT} ${DDPT_OPTS} if=/dev/ram0  bs=512 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/dev/ram0  bs=512 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    echo "ddpt failed; exit status: ${RES}"
    exit ${RES}
fi
echo ""

echo "${DDPT} ${DDPT_OPTS} if=/dev/ram1  bs=512 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/dev/ram1  bs=512 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    echo "ddpt failed; exit status: ${RES}"
    exit ${RES}
fi
echo ""

echo "${DDPT} ${DDPT_OPTS} if=/tmp/sg_a_dev bs=4096 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/tmp/sg_a_dev bs=4096 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    echo "ddpt failed; exit status: ${RES}"
    exit ${RES}
fi
echo ""

echo "${DDPT} ${DDPT_OPTS} if=/tmp/sg_b_dev  bs=4096 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/tmp/sg_b_dev  bs=4096 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    echo "ddpt failed; exit status: ${RES}"
    exit ${RES}
fi
echo ""

echo "${DDPT} ${DDPT_OPTS} if=/tmp/sd_sg_a_dev bs=4096 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/tmp/sd_sg_a_dev bs=4096 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    echo "ddpt failed; exit status: ${RES}"
    exit ${RES}
fi
echo ""

echo "${DDPT} ${DDPT_OPTS} if=/tmp/sd_sg_b_dev  bs=4096 ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} if=/tmp/sd_sg_b_dev  bs=4096 ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    echo "ddpt failed; exit status: ${RES}"
    exit ${RES}
fi
echo ""

echo "${DDPT} ${DDPT_OPTS} urand_ram0.jf ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} urand_ram0.jf ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    echo "ddpt failed; exit status: ${RES}"
    exit ${RES}
fi
echo ""

echo "${DDPT} ${DDPT_OPTS} ram0_2_ram1.jf ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} ram0_2_ram1.jf ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    echo "ddpt failed; exit status: ${RES}"
    exit ${RES}
fi
echo ""

echo "${DDPT} ${DDPT_OPTS} ram1_2_sg_a.jf ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} ram1_2_sg_a.jf ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    echo "ddpt failed; exit status: ${RES}"
    exit ${RES}
fi
echo ""

echo "${DDPT} ${DDPT_OPTS} sg_a_2_sg_b.jf ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} sg_a_2_sg_b.jf ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    echo "ddpt failed; exit status: ${RES}"
    exit ${RES}
fi
echo ""

echo "${DDPT} ${DDPT_OPTS} sd_b_2_sd_a.jf ${DDPT_ARG}"
${DDPT} ${DDPT_OPTS} sd_b_2_sd_a.jf ${DDPT_ARG}
RES=$?
if [ ${RES} -ne 0 ] ; then
    echo "ddpt failed; exit status: ${RES}"
    exit ${RES}
fi
echo ""


#
# throw away stdout and stderr: > /dev/null 2>&1


