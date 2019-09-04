#!/bin/bash
set -e

cat <<EOF


#================================================================================
#================================================================================
# Hybrid-GODAS  -  cycle.scrub.sh
#================================================================================
EOF

envar=()
envar+=("SCRATCH_DIR")
envar+=("SAVE_DIR")
envar+=("KEEP_CYCLES")
envar+=("KEEP_CYCLES_REGEX")

# make sure required env vars exist
for v in ${envar[@]}; do
    if [[ -z "${!v}" ]]; then
	echo "ERROR: env var $v is not set."; exit 1
    fi
    echo " $v = ${!v}"
done
set -u
echo ""


dtz(){ echo ${1:0:8}Z${1:8:10}; }



keep_hrs=$(($CYCLE_LEN * $KEEP_CYCLES))
keep_date=$(date "+%Y%m%d%H" -d "$(dtz $CYCLE) - $keep_hrs hours")
echo "Deleting scratch directories for cycles older than $keep_date"
echo ""


# delete old cycles no longer needed in the scratch directory
for d in $SCRATCH_DIR/cycle_??????????/; do
    d2=${d%/}
    d2=${d2##*/}
    d2=${d2:6:10}
    if [[ "$d2" -lt "$keep_date" ]]; then
	echo "deleting $d"
	rm -rf $d
    fi
done



# delete old cycles no longer needed in the save directory
for d in $SAVE_DIR/??????????/; do
    d2=${d%/}
    d2=${d2##*/}
    keep=1

    if [[ "$d2" -lt "$keep_date" ]]; then
	keep=0
    fi

    # check to see if it matches the regular expression of special dates to keep
    if [[ "$KEEP_CYCLES_REGEX" != 0 ]]; then
	( echo $d2 | grep -qE "$KEEP_CYCLES_REGEX" ) && keep=1
    fi

    if [[ "$keep" -eq 0 ]]; then
	echo "deleting $d"
	rm -rf $d
    fi
done
